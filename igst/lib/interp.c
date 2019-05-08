/***********************************************************************
 *
 *	Byte Code Interpreter Module.
 *
 *	Interprets the compiled bytecodes of a method.
 *
 *	$Revision: 1.6.2$
 *	$Date: 1999/08/31 11:23:18$
 *	$Author: pb$
 *
 ***********************************************************************/

/***********************************************************************
 *
 * Copyright 1990, 91, 92, 94, 95, 99 Free Software Foundation, Inc.
 * Written by Steve Byrne.
 *
 * This file is part of GNU Smalltalk.
 *
 * GNU Smalltalk is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2, or (at your option) any later 
 * version.
 * 
 * GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * GNU Smalltalk; see the file COPYING.  If not, write to the Free Software
 * Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
 *
 ***********************************************************************/


#include "gst.h"
#include "alloc.h"
#include "interp.h"
#include "dict.h"
#include "sym.h"
#include "oop.h"
#include "save.h"
#include "sym.h"
#include "comp.h"
#include "callin.h"
#include "cint.h"
#include "sysdep.h"
#include "lex.h"
#include "lib.h"
#include "byte.h"
#include <math.h>
#include <stdio.h>

#include "igstlib.h"

#ifdef sgi
# define _BSD_SIGNALS
#endif
#include <signal.h>

#include <sys/types.h>
#include <setjmp.h>
#ifdef STDC_HEADERS
#include <string.h>
#include <stdlib.h>
#endif /* STDC_HEADERS */

#ifdef HAVE_IO_H
#include <io.h>
#endif


/* Used to handle the case when the user types a ^C while executing callout
 * code.  If STACK_JMPBUFS is defined, the C callout primitive saves the
 * old jmp_buf on the stacks and uses a new one; if it is not defined, a ^C
 * will immediately jump outside ALL the callouts.  The former behavior is
 * usually cleaner, so I define it. */
#define STACK_JMPBUFS

/* The local regs concept hopes, by caching the values of IP and SP in
 * local register variables, to increase performance.  You only need to
 * export the variables when calling out to routines that might change them
 * and that create objects.  This is because a GC may start at any time, and
 * the values of IP and SP can change on a GC flip, since they're in the root
 * set).  It's easy to deal with that, however, it's just a matter of
 * importing/exporting the registers at the correct places: for example,
 * message sends (including calls to #basicNew and #basicNew:) which can
 * result in a GC always export the registers, while push/pops are innocuous.
 * I'm leaving the code to deal with them as local registers conditionally
 * compiled in so that you can disable it easily if necessary. */

#define LOCAL_REGS

/* New-style dispatching almost obtains the speed of threaded code without its
 * building complications.  Alas it relies on two GNU CC extensions (the &&
 * address-of-label operator, and the `goto void *', so we have to #define
 * USE_OLD_DISPATCH if GNU CC is not being used.  Uncomment this to use
 * old-style dispatching with GCC too. */
/* #define USE_OLD_DISPATCH */

#ifndef HAVE_GOTO_VOID_P
#define USE_OLD_DISPATCH
#endif

/* By "hard wiring" the definitions of the special math operators (bytecodes
 * 176-191), we get a performance boost of more than 50%.  Yes, it means that
 * we cannot redefine + et al for Integer and Float, but I think the trade is
 * worth it.  Besides, the Blue Book does it. */
#define OPEN_CODE_MATH

/* Jump lookahead uses special machinery after open-coded boolean selectors
 * (<, =, >, <=, >=, ~= for Integers and Floats; ==, isNil and notNil for all
 * objects) that executes conditional jump bytecodes without pushing and
 * popping the result of the comparison.  This catches the common
 * "a < b ifTrue: [ ... ]" and "[ ... a < b ] whileTrue: [ ... ]" patterns.
 * Jump lookahead only works with new-style dispatching. */

#define JUMP_LOOKAHEAD

#ifdef USE_OLD_DISPATCH
#undef JUMP_LOOKAHEAD
#endif

/* The method cache is a hash table used to cache the most commonly used
 * methods.  Its size is determined by this preprocessor constant.  It is
 * currently 2048, a mostly random choice; you can modify it, but be sure
 * it is a power of two. */
#define	METHOD_CACHE_SIZE		(1 << 11)


#ifdef atarist
#define ASYNC_QUEUE_SIZE		16 /* still way too much */
#else
#define ASYNC_QUEUE_SIZE		100 /* way too much */
#endif

/* Max number of C-style signals on a machine */
#define NUM_SIGNALS	32



/* An unrolled loop, efficient if numNils is small. */
#define pushNNils(numNils) \
  if (numNils) { \
    unsigned int tempsRemaining; \
      switch ((numNils) & 7) { \
        case 7:	pushOOP(nilOOP); \
        case 6:	pushOOP(nilOOP); \
        case 5:	pushOOP(nilOOP); \
        case 4:	pushOOP(nilOOP); \
        case 3:	pushOOP(nilOOP); \
        case 2:	pushOOP(nilOOP); \
        case 1:	pushOOP(nilOOP); \
        default: break; \
      } \
      tempsRemaining = (numNils) >> 3; \
      while(tempsRemaining--) { \
        pushOOP(nilOOP); \
        pushOOP(nilOOP); \
        pushOOP(nilOOP); \
        pushOOP(nilOOP); \
        pushOOP(nilOOP); \
        pushOOP(nilOOP); \
        pushOOP(nilOOP); \
        pushOOP(nilOOP); \
      } \
  }

/* Ordering of file operations must match that used in FileSegment.st */
typedef enum {
  openFilePrim,
  closeFilePrim,
  getCharPrim,
  putCharPrim,
  seekPrim,
  tellPrim,
  eofPrim,
  popenFilePrim,
  sizePrim,
  putCharsPrim,
  getCharsPrim,
  flushPrim,
  getBytePrim
} filePrimitiveTypes;

typedef struct FileStreamStruct {
  OBJ_HEADER;
  OOP		collection;
  OOP		ptr;
  OOP		endPtr;
  OOP		access;
  OOP		file;
  OOP		name;
  OOP		buffer;
  OOP		isPipe;
} *FileStream;


long methodCount = 0, totalMethods = 0;


unsigned long		byteCodeCounter;
unsigned long		cacheHits = 0;
unsigned long		cacheMisses = 0;

/* !!! */
mst_Boolean			gcDebug = false;

/* If this is true, for each byte code that is executed, the byte index
 * within the current CompiledMethod and a decoded interpretation of
 * the byte code is printed on standard output. */
mst_Boolean			executionTracing;

/* When this is true, and an interrupt occurs (such as SIGSEGV), Smalltalk
 * will terminate itself by making a core dump (normally it does not
 * terminate in this manner). */
mst_Boolean			makeCoreFile = false;

/* When true, this indicates that there is no top level loop for control
 * to return to, so it causes the system to exit. */
mst_Boolean			nonInteractive = true;

/* Number of samples, i.e. cache lookups - either hits or misses */
unsigned long			sampleCounter;

/* Queue for async (outside the interpreter) semaphore signals */
static OOP		queuedAsyncSignals[ASYNC_QUEUE_SIZE]; 
static int		asyncQueueIndex;

/* brd Sun Jun 25 14:16:31 GMT-0800 1995 */
/* When true, enable GC of Smalltalk objects referenced in callins/callouts. */
mst_Boolean enableGC = false;

OOP			thisMethod;
Byte			*outer__IP;
OOP			*outer__SP;

#ifdef PROFBLOCK
struct profStruct ps;
static long		byteCodes[256];
static long		primitives[1024];
#endif

typedef struct MethodCacheEntryStruct {
  OOP selectorOOP;
  OOP classOOP;
  OOP methodOOP;
  OOP methodClassOOP;
#ifdef COLLISION_CHECKING
  int collide;
#endif
} MethodCacheEntry;



static MethodCacheEntry	methodCache[METHOD_CACHE_SIZE];

static OOP		switchToProcess; /* non-nil when proc switch wanted */

/* points into method or context to start of literals, arguments and temporaries */
static OOP		*temporaries, *literals;	
static OOP		self;
static OOP		thisContextOOP;
static mst_Boolean	inInterpreter = false;
mst_Boolean		exceptFlag;

/* Holds the semaphore to signal when the processor interval timesout */
static OOP		timeoutSem;

static OOP		semIntVec[NUM_SIGNALS];
static mst_Boolean	semIntHappened[NUM_SIGNALS];


/* When true, this causes the byte code interpeter to immediately send
 * #error: to the current stack top, passing the string.
 */
static char			*abortExecution = nil;

/* When this is true, it means that the system is executing external C code,
 * which can be used by the ^C handler to know whether it longjmp to the
 * end of the C callout primitive in executePrimitiveOperation. */
static mst_Boolean		inCCode = false;

#ifdef STACK_JMPBUFS

/* This type hides the implementation of the jmp_buf type.  If jmp_buf is
 * implemented as an array, taking is address for storing in cCalloutJmpBuf
 * causes the compiler to warn about taking the address of an array, but
 * there's no way to tell at compile time whether this is going to be a
 * problem.  We could put a test in configure, but that only works on
 * Unix boxes (or other places where configure gets run), and the non-Unix
 * cases have to be explicitly hard coded.  This seems much simpler, and much
 * more guaranteed to work in a variety of environments.
 */
typedef struct InterpJmpBufStruct {
  jmp_buf	jmpBuf;
} InterpJmpBuf;
static InterpJmpBuf 		*cCalloutJmpBuf;

#else
static jmp_buf cCalloutJmpBuf;
#endif

/* when this flag is on and execution tracing is in effect, the top
 * of the stack is printed as well as the byte code */
static mst_Boolean		verboseExecTracing = false;


#ifdef ACCESSOR_DEBUGGING
static OOP		methodTemporary(), receiverVariable(),
  			methodVariable(), getMethodClass(),
			getStackReceiver(), methodLiteral(),
			getMethodContext();
static void		storeMethodTemporary(), storeReceiverVariable(),
  			storeMethodVariable(), storeMethodLiteral();
static mst_Boolean	noParentContext(), inBounds(), isBlockContext();
static Byte		*getMethodByteCodes();
static MethodHeader	getMethodHeader();
static int		relativeByteIndex();
#endif /* ACCESSOR_DEBUGGING */
static OOP		/* createMethodContext(), getProcessLists(), */
			highestPriorityProcess(), nextScheduledProcess(),
			removeFirstLink(), semaphoreNew();
static void		showBacktrace(), stopExecuting(),
			sleepProcess(), activateProcess(),
			changeProcessContext(), addLastLink(),
			markSemaphoreOOPs(), syncSignal();
			/* markFakeContextObjects(), */

static mst_Boolean	sendBlockValue(), resumeProcess(), isProcessReady(),
			isProcessTerminating();

static inline void		returnWithValue();

static inline mst_Boolean	isEmpty(), *boolAddrIndex(),
				executePrimitiveOperation();

static inline OOP		getActiveProcess(), allocFakeContext();

static inline void		deallocFakeContext();

static RETSIGTYPE	interruptHandler(SIG_ARG_TYPE), 
			timeoutHandler(SIG_ARG_TYPE), semIntHandler(SIG_ARG_TYPE);

#define allocMethodContext(size) allocFakeContext(methodContextClass, size)
#define allocBlockContext(size)  allocFakeContext(blockContextClass, size)

#define activeProcessYield() \
  activateProcess(nextScheduledProcess());

#define getProcessLists() \
  (((ProcessorScheduler)oopToObj(processorOOP))->processLists)


/*
 *
 * This is basically how the interpreter works:
 *  1) The interpreter expects to be called in an environment where there
 *     already exists a well-defined method context.  The instruction pointer,
 *     stored in the global variable "ip", and the stack pointer, stored in the
 *     global variable "sp", should be set up to point into the current
 *     method and MethodContext.  Other global variables, such as "thisMethod",
 *     "self", "temporaries", etc. should also be setup.  See the routine
 *     prepareExecutionEnvironment for details.
 *  2) The interpreter checks to see if any change in its state is required,
 *     such as switching to a new process, dealing with an asynchronous signal
 *     and printing out the byte codes that are being executed, if that was
 *     requested by the user.
 *  3) After that, the byte code that ip points to is fetched and decoded.
 *     Some byte codes perform jumps, which are performed by merely adjusting
 *     the value of ip.  Some are message sends, which are described in
 *     more detail below.  Some instructions require more than one byte code
 *     to perform their work; ip is advanced as needed and the extension
 *     byte codes are fetched.  Some byte codes, which push booleans,
 *     are usually followed by jump byte codes; in this case, and if
 *     JUMP_LOOKAHEAD is defined, the two byte codes are merged for speed.
 *  4) After dispatching the byte code, the interpreter loops around to
 *     execute another byte code.  If ip has changed to point to nil, it is
 *     a signal that the execution of the method is over, and the interpreter
 *     returns to its caller.
 *
 * Note that the interpreter is not called recursively to implement message
 * sends.  Rather the state of the interpreter is saved away in the currently
 * executing context, and a new context is created and the global variables
 * such as ip, sp, and temporaries are initialized accordingly.
 *
 * When a message send occurs, the sendMessage routine is invoked.  It 
 * determines the class of the receiver, and checks to see if it already has
 * cached the method definition for the given selector and receiver class.
 * If so, that method is used, and if not, the receiver's method dictionary
 * is searched for a method with the proper selector.  If it's not found in
 * that method dictionary, the method dictionary of the classes parent is
 * examined, and on up the hierarchy, until a matching selector is found.
 *
 * If no selector is found, the receiver is sent a #doesNotUnderstand: message
 * to indicate that a matching method could not be found.
 *
 * If a method is found, it is examined for some special cases.  The special
 * cases are primitive return of self, return of an instance variable, or
 * execution of a primitive method definition.  This latter operation is
 * performed by the executePrimitiveOperation routine.  If the execution
 * of this primitive interpreter fails, the normal message send operation
 * is performed.
 *
 * If the found method is not one of the special cases, or if it is a 
 * primitive that failed to execute, a "normal" message send is performed.
 * This basically entails saving away what state the interpreter has, such
 * as the values of ip, and sp, being careful to save their relative locations
 * and not their physical addresses, because one or more garbage collections
 * could occur before the method context is returned to, and the absolute
 * pointers would be invalid.
 *
 * The sendMessage routine then creates a new MethodContext object, makes
 * its parent be the currently executing MethodContext, and sets up
 * the interpreters global variables to reference the new method and
 * new MethodContext.  Once those variables are set, sendMessage returns
 * to the interpreter, which cheerfully begins executing the new method,
 * totally unaware that the method that it was executing has changed.
 *
 * When a method returns, the method that called it is used to restore the
 * interpreter's global variables to the state that they were in before
 * the called method was called.  The values of ip and sp are restored to
 * their absolute address values, and the other global state variables
 * are restored accordingly.  When after the state has been restored, the
 * interpreter continues execution, again totally oblivious to the fact
 * that it's not running the same method it was on its previous byte code.
 *
 * Global state
 * The following variables constitute the interpreter's state:
 * ip -- the real memory address of the next byte code to be executed.
 * sp -- the real memory address of the stack that's stored in the currently
 *       executing block or method context.
 * thisMethod -- a CompiledMethod that is the currently executing method.
 * thisContextOOP -- a BlockContext or MethodContext that indicates the
 *                   context that the interpreter is currently running in.
 * temporaries -- physical address of the base of the method temporary
 *                variables.  Typically a small number of bytes (multiple of 4
 *                since it points to OOPs) lower than sp.
 * literals -- physical address of the base of the method literals
 * self -- an OOP that is the current receiver of the current message.
 *
 * Structure of the stack:
 *
 *    +-----------------------------------+	HERE END THE CONTEXT'S
 *    | receiver (self)		    	  |	FIXED INSTANCE VARIABLES
 *    +-----------------------------------+-------------------------------
 *    | args				  |	HERE BEGIN THE CONTEXT'S
 *    +-----------------------------------+	INDEXED INSTANCE VARIABLES
 *    | ...				  |
 *    +-----------------------------------+
 *    | temps				  |
 *    +-----------------------------------+
 *    | ...				  |
 *    +-----------------------------------+
 *    
 */



#ifdef USE_OLD_DISPATCH
  #define setExceptFlag(x)		(exceptFlag = (x))
#else
  static void		**globalMonitoredByteCodes, **globalNormalByteCodes;
  static void		**dispatchVec;

  #define setExceptFlag(x) {					\
    exceptFlag = (x);						\
    dispatchVec = exceptFlag ?					\
      globalMonitoredByteCodes : globalNormalByteCodes;		\
  }

#endif

void
interpret()
{
#ifdef LOCAL_REGS
  #undef  sp
  #undef  ip
  #define exportRegs()	{ outer__SP = sp; outer__IP = ip; }
  #define importRegs()	{ sp = outer__SP; ip = outer__IP; }
  register OOP	*sp;
  register Byte	*ip;
#else
  #define exportRegs()
  #define importRegs()
#endif /* LOCAL_REGS */
  register OOP  tempOOP;
  OOP		returnedValue, methodContextOOP;
  BlockContext	blockContext;
  Byte		ival2, ival3;

#ifdef USE_OLD_DISPATCH

/* Most of the difference in speed between switch-based dispatch and
   label-based dispatch comes from the fact that all bytecodes pass
   through an if in the former case, while when using labels we can
   use dispatchVec instead of an if statement. */

#define NEXT_BYTECODE			     break
#define DISPATCH_BYTECODE dispatchBytecodes: switch(*ip) {
#define ALSO(num, lab) 				case num:
#define DEFAULT_LABEL				default: ip++;
#define END_DISPATCH 			     }

#define BRANCH_LABEL(x)

#else /* !defined(USE_OLD_DISPATCH) */


  /* Used for threaded bytecode interpretation when using GCC.  Normal bytecodes
   * is indexed by bytecode and holds onto the address of the label to jump to
   * to execute that byte code.  MonitoredByteCodes has all byte codes jump
   * to a common place to check for exceptional conditions, and then jump thru
   * normalByteCodes.  DispatchVec points normally at normalByteCodes, but when
   * there is an exceptional condition, it points at monitoredByteCodes.
   * TrueByteCodes and falseByteCodes are used (if JUMP_LOOKAHEAD is defined)
   * to dispatch conditional jumps immediately following comparisons (see above)
   * without pushing and popping the result of the conditional.
   */
  static void *normalByteCodes[] = {
    /* 1-byte Push bytecodes */
    &&pushRecVar0,    &&pushRecVar1,    &&pushRecVar2,    &&pushRecVar3,	/* 0 */
    &&pushRecVar4,    &&pushRecVar5,    &&pushRecVar6,    &&pushRecVar7,	/* 4 */
    &&pushRecVar8,    &&pushRecVar9,    &&pushRecVar10,   &&pushRecVar11,	/* 8 */
    &&pushRecVar12,   &&pushRecVar13,   &&pushRecVar14,   &&pushRecVar15,	/* 12 */
    &&pushTemp0,      &&pushTemp1,      &&pushTemp2,      &&pushTemp3,		/* 16 */
    &&pushTemp4,      &&pushTemp5,      &&pushTemp6,      &&pushTemp7,		/* 20 */
    &&pushTemp8,      &&pushTemp9,      &&pushTemp10,     &&pushTemp11,		/* 24 */
    &&pushTemp12,     &&pushTemp13,     &&pushTemp14,     &&pushTemp15,		/* 28 */
    &&pushLit0,	      &&pushLit1,       &&pushLit2,	  &&pushLit3,		/* 32 */
    &&pushLit4,	      &&pushLit5,	&&pushLit6,	  &&pushLit7,		/* 36 */
    &&pushLit8,       &&pushLit9,	&&pushLit10,      &&pushLit11,		/* 40 */
    &&pushLit12,      &&pushLit13,	&&pushLit14,      &&pushLit15,		/* 44 */
    &&pushLit16,      &&pushLit17,	&&pushLit18,      &&pushLit19,		/* 48 */
    &&pushLit20,      &&pushLit21,	&&pushLit22,      &&pushLit23,		/* 52 */
    &&pushLit24,      &&pushLit25,	&&pushLit26,      &&pushLit27,		/* 56 */
    &&pushLit28,      &&pushLit29,	&&pushLit30,      &&pushLit31,		/* 60 */
    &&pushVar0,	      &&pushVar1,	&&pushVar2,	  &&pushVar3,		/* 64 */
    &&pushVar4,	      &&pushVar5,	&&pushVar6,	  &&pushVar7,		/* 68 */
    &&pushVar8,	      &&pushVar9,	&&pushVar10,      &&pushVar11,		/* 72 */
    &&pushVar12,      &&pushVar13,	&&pushVar14,      &&pushVar15,		/* 76 */
    &&pushVar16,      &&pushVar17,	&&pushVar18,      &&pushVar19,		/* 80 */
    &&pushVar20,      &&pushVar21,	&&pushVar22,      &&pushVar23,		/* 84 */
    &&pushVar24,      &&pushVar25,	&&pushVar26,      &&pushVar27,		/* 88 */
    &&pushVar28,      &&pushVar29,	&&pushVar30,      &&pushVar31,		/* 92 */

    /* 1-byte Pop/Store bytecodes */
    &&stRecVar0,      &&stRecVar1,	&&stRecVar2,      &&stRecVar3,		/* 96 */
    &&stRecVar4,      &&stRecVar5,	&&stRecVar6,      &&stRecVar7,		/* 100 */
    &&stTemp0,	      &&stTemp1,	&&stTemp2,	  &&stTemp3,		/* 104 */
    &&stTemp4,	      &&stTemp5,	&&stTemp6,	  &&stTemp7,		/* 108 */

    /* Special push bytecodes */
    &&pushSelf,	      &&pushTrue,	&&pushFalse,      &&pushNil,		/* 112 */
    &&pushNeg1,	      &&push0,		&&push1,	  &&push2,		/* 116 */

    /* Return bytecodes */
    &&retSelf,	      &&retTrue,	&&retFalse,	  &&retNil,		/* 120 */
    &&explicitRet,    &&retStackTop,						/* 124 */

    /* All sorts of bytecodes */
    &&bigLiteralOp,   &&breakpoint,     				  	/* 126 */

    /* Long bytecodes */
    &&pushIndexedVal, &&storeStackTop,	&&popAndStoreTop,			/* 128 */
    &&sendShort,      &&sendLong,	&&supSendShort,   &&supSendLong,	/* 131 */

    /* More stack bytecodes */
    &&popStack,	      &&dupStack,	&&pushThisContext,			/* 135 */
    &&outerTempOp,    &&nop,		&&topSelf,				/* 138 */
    &&topOne,	      &&topIndexedVal,  					/* 141 */

    /* Unused bytecode */
    &&unused143,								/* 143 */

    /* Jump bytecodes */
    &&shJmp1,         &&shJmp2,		&&shJmp3,         &&shJmp4,		/* 144 */
    &&shJmp5,         &&shJmp6,		&&shJmp7,         &&shJmp8,		/* 148 */
    &&shJmp1False,    &&shJmp2False,	&&shJmp3False,    &&shJmp4False,	/* 152 */
    &&shJmp5False,    &&shJmp6False,	&&shJmp7False,    &&shJmp8False,	/* 156 */
    &&longJmpNeg4,    &&longJmpNeg3,	&&longJmpNeg2,    &&longJmpNeg1,	/* 160 */
    &&longJmp0,	      &&longJmp1,	&&longJmp2,	  &&longJmp3,		/* 164 */
    &&popJmpTrue0,    &&popJmpTrue1,	&&popJmpTrue2,    &&popJmpTrue3,	/* 168 */
    &&popJmpFalse0,   &&popJmpFalse1,	&&popJmpFalse2,   &&popJmpFalse3,	/* 172 */

    /* Special 1-byte send bytecodes */
    &&sendPlus,       &&sendMinus,	&&sendLess,	  &&sendGreater,	/* 176 */
    &&sendLessEq,     &&sendGreaterEq,	&&sendEqual,      &&sendNotEqual,	/* 180 */
    &&sendTimes,      &&sendDivide,	&&sendRemainder,  &&sendAtSign,		/* 184 */
    &&sendBitShift,   &&sendIntDivide,	&&sendBitAnd,     &&sendBitOr,		/* 188 */
    &&sendAt,	      &&sendAtPut,	&&sendSize,       &&sendNext,		/* 192 */
    &&sendNextPut,    &&sendAtEnd,	&&sendSameObject, &&sendClass,		/* 196 */
    &&sendBlockCopy,  &&sendValue,	&&sendValueColon, &&sendDoColon,	/* 200 */
    &&sendNew,	      &&sendNewColon,	&&sendIsNil,      &&sendNotNil,       	/* 204 */

    /* General 1-byte send bytecodes */
    &&sendNoArg0,     &&sendNoArg1,	&&sendNoArg2,     &&sendNoArg3,		/* 208 */
    &&sendNoArg4,     &&sendNoArg5,	&&sendNoArg6,     &&sendNoArg7,		/* 212 */
    &&sendNoArg8,     &&sendNoArg9,	&&sendNoArg10,    &&sendNoArg11,	/* 216 */
    &&sendNoArg12,    &&sendNoArg13,	&&sendNoArg14,    &&sendNoArg15,	/* 220 */
    &&sendOneArg0,    &&sendOneArg1,	&&sendOneArg2,    &&sendOneArg3,	/* 224 */
    &&sendOneArg4,    &&sendOneArg5,	&&sendOneArg6,    &&sendOneArg7,	/* 228 */
    &&sendOneArg8,    &&sendOneArg9,	&&sendOneArg10,   &&sendOneArg11,	/* 232 */
    &&sendOneArg12,   &&sendOneArg13,	&&sendOneArg14,   &&sendOneArg15,	/* 236 */
    &&sendTwoArg0,    &&sendTwoArg1,	&&sendTwoArg2,    &&sendTwoArg3,	/* 240 */
    &&sendTwoArg4,    &&sendTwoArg5,	&&sendTwoArg6,    &&sendTwoArg7,	/* 244 */
    &&sendTwoArg8,    &&sendTwoArg9,	&&sendTwoArg10,   &&sendTwoArg11,	/* 248 */
    &&sendTwoArg12,   &&sendTwoArg13,	&&sendTwoArg14,   &&sendTwoArg15	/* 252 */
  };

  static void *monitoredByteCodes[] = {
    [0 ... 255] = &&monitorByteCodes
  };

#ifdef JUMP_LOOKAHEAD
  static void *trueByteCodes[] = {
    [  0 ... 151] = &&lookaheadFailedTrue,		/* pot-pourri    */
    [152 ... 159] = &&nop,				/* jump if false */
    [160 ... 167] = &&lookaheadFailedTrue,		/* unconditional */

    &&longJmp0,	      &&longJmp1,	&&longJmp2,	  &&longJmp3,

    [172 ... 175] = &&lookaheadLongJumpNotTaken,	/* jump if false */
    [176 ... 255] = &&lookaheadFailedTrue		/* pot-pourri */
  };

  static void *falseByteCodes[] = {
    [  0 ... 151] = &&lookaheadFailedFalse,		/* pot-pourri */

    &&shJmp1,         &&shJmp2,		&&shJmp3,         &&shJmp4,
    &&shJmp5,         &&shJmp6,		&&shJmp7,         &&shJmp8,
    
    [160 ... 167] = &&lookaheadFailedFalse,		/* unconditional */
    [168 ... 171] = &&lookaheadLongJumpNotTaken,	/* jump if true */

    &&longJmp0,	      &&longJmp1,	&&longJmp2,	  &&longJmp3,

    [176 ... 255] = &&lookaheadFailedFalse		/* pot-pourri */
  };
#endif

#define NEXT_BYTECODE goto *dispatchVec[*ip];
#define ALSO(num, lab) lab:
#define DISPATCH_BYTECODE  goto *normalByteCodes[*ip];
#define BRANCH_LABEL(x) x:
#define DEFAULT_LABEL  ip++; /* no need for default when using gotos */
#define END_DISPATCH

#endif /* !defined(USE_OLD_DISPATCH) */

#define BYTECODE(num, lab) ALSO(num, lab)  ip++; byteCodeCounter++; 

  if(isNil(thisContextOOP)) {
    return;
  }
  importRegs();

  if(!ip) {
    return;
  }
  inInterpreter = true;

  /* this is all we need to do -- since we only pay attention to the 
   * execution tracing flag on entry to this routine, and since the first time 
   * through we do what setting dispatchVec to monitoredByteCodes would do,
   * we can get away with just exceptFlag here.
   */
  exceptFlag = executionTracing;
#ifndef USE_OLD_DISPATCH
  globalNormalByteCodes = normalByteCodes;
  globalMonitoredByteCodes = monitoredByteCodes;
  dispatchVec = normalByteCodes;
#endif

  for (; ; ) {
  BRANCH_LABEL(monitorByteCodes)
    if (exceptFlag) {
      exportRegs();
      if (asyncQueueIndex) {	/* deal with any async signals  */
	register int	i;
	IntState	oldSigMask;
	oldSigMask = disableInterrupts(); /* block out everything! */
	for (i = 0; i < asyncQueueIndex; i++) {
	  syncSignal(queuedAsyncSignals[i]);
	}
	asyncQueueIndex = 0;
	enableInterrupts(oldSigMask);
      }
      if (!isNil(switchToProcess)) {
	/*exportRegs(); */
	changeProcessContext(switchToProcess);
	importRegs();
	/* make sure to validate the IP again */
	if (!ip) {
	  break;
	}
      }
      if (abortExecution) {
        OOP selectorOOP;
#ifdef old_code
/**/	goto abortMethod;	/* ugh! */
#endif
	selectorOOP = internString(abortExecution);
        abortExecution = nil;
        sendMessage(selectorOOP, 0, false);
      }
      if (executionTracing) {
	if (verboseExecTracing) {
	  printf("\t  --> ");
	  printObject(stackTop());
	  printf("\n");
	}
	printf(" %5d:\t", relativeByteIndex(ip, thisMethod));
	printByteCodeName(ip, relativeByteIndex(ip, thisMethod),
			  literals);
	printf("\n");
      }
      setExceptFlag(executionTracing);
      importRegs();
    }

#ifdef PROFBLOCK
    byteCodes[*ip]++;
#endif /* PROFBLOCK */



    /* Note: some of the case arms are expanded out to literal cases,
       instead of case0: case1: ... pushOOP(receiverVariable(self, ival&15))
       this is an experiment to try to improve performance of the byte code
       interpreter throughout the system. */
    DISPATCH_BYTECODE
    BYTECODE(0, pushRecVar0) pushOOP(receiverVariable(self, 0)); NEXT_BYTECODE;
    BYTECODE(1, pushRecVar1) pushOOP(receiverVariable(self, 1)); NEXT_BYTECODE;
    BYTECODE(2, pushRecVar2) pushOOP(receiverVariable(self, 2)); NEXT_BYTECODE;
    BYTECODE(3, pushRecVar3) pushOOP(receiverVariable(self, 3)); NEXT_BYTECODE;
    BYTECODE(4, pushRecVar4) pushOOP(receiverVariable(self, 4)); NEXT_BYTECODE;
    BYTECODE(5, pushRecVar5) pushOOP(receiverVariable(self, 5)); NEXT_BYTECODE;
    BYTECODE(6, pushRecVar6) pushOOP(receiverVariable(self, 6)); NEXT_BYTECODE;
    BYTECODE(7, pushRecVar7) pushOOP(receiverVariable(self, 7)); NEXT_BYTECODE;
    BYTECODE(8, pushRecVar8) pushOOP(receiverVariable(self, 8)); NEXT_BYTECODE;
    BYTECODE(9, pushRecVar9) pushOOP(receiverVariable(self, 9)); NEXT_BYTECODE;
    BYTECODE(10, pushRecVar10)pushOOP(receiverVariable(self, 10)); NEXT_BYTECODE;
    BYTECODE(11, pushRecVar11)pushOOP(receiverVariable(self, 11)); NEXT_BYTECODE;
    BYTECODE(12, pushRecVar12)pushOOP(receiverVariable(self, 12)); NEXT_BYTECODE;
    BYTECODE(13, pushRecVar13)pushOOP(receiverVariable(self, 13)); NEXT_BYTECODE;
    BYTECODE(14, pushRecVar14)pushOOP(receiverVariable(self, 14)); NEXT_BYTECODE;
    BYTECODE(15, pushRecVar15)pushOOP(receiverVariable(self, 15)); NEXT_BYTECODE;

    BYTECODE(16, pushTemp0) pushOOP(methodTemporary(0));   NEXT_BYTECODE;
    BYTECODE(17, pushTemp1) pushOOP(methodTemporary(1));   NEXT_BYTECODE;
    BYTECODE(18, pushTemp2) pushOOP(methodTemporary(2));   NEXT_BYTECODE;
    BYTECODE(19, pushTemp3) pushOOP(methodTemporary(3));   NEXT_BYTECODE;
    BYTECODE(20, pushTemp4) pushOOP(methodTemporary(4));   NEXT_BYTECODE;
    BYTECODE(21, pushTemp5) pushOOP(methodTemporary(5));   NEXT_BYTECODE;
    BYTECODE(22, pushTemp6) pushOOP(methodTemporary(6));   NEXT_BYTECODE;
    BYTECODE(23, pushTemp7) pushOOP(methodTemporary(7));   NEXT_BYTECODE;
    BYTECODE(24, pushTemp8) pushOOP(methodTemporary(8));   NEXT_BYTECODE;
    BYTECODE(25, pushTemp9) pushOOP(methodTemporary(9));   NEXT_BYTECODE;
    BYTECODE(26, pushTemp10) pushOOP(methodTemporary(10)); NEXT_BYTECODE;
    BYTECODE(27, pushTemp11) pushOOP(methodTemporary(11)); NEXT_BYTECODE;
    BYTECODE(28, pushTemp12) pushOOP(methodTemporary(12)); NEXT_BYTECODE;
    BYTECODE(29, pushTemp13) pushOOP(methodTemporary(13)); NEXT_BYTECODE;
    BYTECODE(30, pushTemp14) pushOOP(methodTemporary(14)); NEXT_BYTECODE;
    BYTECODE(31, pushTemp15) pushOOP(methodTemporary(15)); NEXT_BYTECODE;

    BYTECODE(32, pushLit0) pushOOP(methodLiteral(0));  NEXT_BYTECODE;
    BYTECODE(33, pushLit1) pushOOP(methodLiteral(1));  NEXT_BYTECODE;
    BYTECODE(34, pushLit2) pushOOP(methodLiteral(2));  NEXT_BYTECODE;
    BYTECODE(35, pushLit3) pushOOP(methodLiteral(3));  NEXT_BYTECODE;
    BYTECODE(36, pushLit4) pushOOP(methodLiteral(4));  NEXT_BYTECODE;
    BYTECODE(37, pushLit5) pushOOP(methodLiteral(5));  NEXT_BYTECODE;
    BYTECODE(38, pushLit6) pushOOP(methodLiteral(6));  NEXT_BYTECODE;
    BYTECODE(39, pushLit7) pushOOP(methodLiteral(7));  NEXT_BYTECODE;
    BYTECODE(40, pushLit8) pushOOP(methodLiteral(8));  NEXT_BYTECODE;
    BYTECODE(41, pushLit9) pushOOP(methodLiteral(9));  NEXT_BYTECODE;
    BYTECODE(42, pushLit10)pushOOP(methodLiteral(10)); NEXT_BYTECODE;
    BYTECODE(43, pushLit11)pushOOP(methodLiteral(11)); NEXT_BYTECODE;
    BYTECODE(44, pushLit12)pushOOP(methodLiteral(12)); NEXT_BYTECODE;
    BYTECODE(45, pushLit13)pushOOP(methodLiteral(13)); NEXT_BYTECODE;
    BYTECODE(46, pushLit14)pushOOP(methodLiteral(14)); NEXT_BYTECODE;
    BYTECODE(47, pushLit15)pushOOP(methodLiteral(15)); NEXT_BYTECODE;
    BYTECODE(48, pushLit16)pushOOP(methodLiteral(16)); NEXT_BYTECODE;
    BYTECODE(49, pushLit17)pushOOP(methodLiteral(17)); NEXT_BYTECODE;
    BYTECODE(50, pushLit18)pushOOP(methodLiteral(18)); NEXT_BYTECODE;
    BYTECODE(51, pushLit19)pushOOP(methodLiteral(19)); NEXT_BYTECODE;
    BYTECODE(52, pushLit20)pushOOP(methodLiteral(20)); NEXT_BYTECODE;
    BYTECODE(53, pushLit21)pushOOP(methodLiteral(21)); NEXT_BYTECODE;
    BYTECODE(54, pushLit22)pushOOP(methodLiteral(22)); NEXT_BYTECODE;
    BYTECODE(55, pushLit23)pushOOP(methodLiteral(23)); NEXT_BYTECODE;
    BYTECODE(56, pushLit24)pushOOP(methodLiteral(24)); NEXT_BYTECODE;
    BYTECODE(57, pushLit25)pushOOP(methodLiteral(25)); NEXT_BYTECODE;
    BYTECODE(58, pushLit26)pushOOP(methodLiteral(26)); NEXT_BYTECODE;
    BYTECODE(59, pushLit27)pushOOP(methodLiteral(27)); NEXT_BYTECODE;
    BYTECODE(60, pushLit28)pushOOP(methodLiteral(28)); NEXT_BYTECODE;
    BYTECODE(61, pushLit29)pushOOP(methodLiteral(29)); NEXT_BYTECODE;
    BYTECODE(62, pushLit30)pushOOP(methodLiteral(30)); NEXT_BYTECODE;
    BYTECODE(63, pushLit31)pushOOP(methodLiteral(31)); NEXT_BYTECODE;

    BYTECODE(64, pushVar0) pushOOP(methodVariable(0)); NEXT_BYTECODE;
    BYTECODE(65, pushVar1) pushOOP(methodVariable(1)); NEXT_BYTECODE;
    BYTECODE(66, pushVar2) pushOOP(methodVariable(2)); NEXT_BYTECODE;
    BYTECODE(67, pushVar3) pushOOP(methodVariable(3)); NEXT_BYTECODE;
    BYTECODE(68, pushVar4) pushOOP(methodVariable(4)); NEXT_BYTECODE;
    BYTECODE(69, pushVar5) pushOOP(methodVariable(5)); NEXT_BYTECODE;
    BYTECODE(70, pushVar6) pushOOP(methodVariable(6)); NEXT_BYTECODE;
    BYTECODE(71, pushVar7) pushOOP(methodVariable(7)); NEXT_BYTECODE;
    BYTECODE(72, pushVar8) pushOOP(methodVariable(8)); NEXT_BYTECODE;
    BYTECODE(73, pushVar9) pushOOP(methodVariable(9)); NEXT_BYTECODE;
    BYTECODE(74, pushVar10)pushOOP(methodVariable(10));NEXT_BYTECODE;
    BYTECODE(75, pushVar11)pushOOP(methodVariable(11));NEXT_BYTECODE;
    BYTECODE(76, pushVar12)pushOOP(methodVariable(12));NEXT_BYTECODE;
    BYTECODE(77, pushVar13)pushOOP(methodVariable(13));NEXT_BYTECODE;
    BYTECODE(78, pushVar14)pushOOP(methodVariable(14));NEXT_BYTECODE;
    BYTECODE(79, pushVar15)pushOOP(methodVariable(15));NEXT_BYTECODE;
    BYTECODE(80, pushVar16)pushOOP(methodVariable(16));NEXT_BYTECODE;
    BYTECODE(81, pushVar17)pushOOP(methodVariable(17));NEXT_BYTECODE;
    BYTECODE(82, pushVar18)pushOOP(methodVariable(18));NEXT_BYTECODE;
    BYTECODE(83, pushVar19)pushOOP(methodVariable(19));NEXT_BYTECODE;
    BYTECODE(84, pushVar20)pushOOP(methodVariable(20));NEXT_BYTECODE;
    BYTECODE(85, pushVar21)pushOOP(methodVariable(21));NEXT_BYTECODE;
    BYTECODE(86, pushVar22)pushOOP(methodVariable(22));NEXT_BYTECODE;
    BYTECODE(87, pushVar23)pushOOP(methodVariable(23));NEXT_BYTECODE;
    BYTECODE(88, pushVar24)pushOOP(methodVariable(24));NEXT_BYTECODE;
    BYTECODE(89, pushVar25)pushOOP(methodVariable(25));NEXT_BYTECODE;
    BYTECODE(90, pushVar26)pushOOP(methodVariable(26));NEXT_BYTECODE;
    BYTECODE(91, pushVar27)pushOOP(methodVariable(27));NEXT_BYTECODE;
    BYTECODE(92, pushVar28)pushOOP(methodVariable(28));NEXT_BYTECODE;
    BYTECODE(93, pushVar29)pushOOP(methodVariable(29));NEXT_BYTECODE;
    BYTECODE(94, pushVar30)pushOOP(methodVariable(30));NEXT_BYTECODE;
    BYTECODE(95, pushVar31)pushOOP(methodVariable(31));NEXT_BYTECODE;

    BYTECODE(96, stRecVar0) storeReceiverVariable(self, 0, popOOP()); NEXT_BYTECODE;
    BYTECODE(97, stRecVar1) storeReceiverVariable(self, 1, popOOP()); NEXT_BYTECODE;
    BYTECODE(98, stRecVar2) storeReceiverVariable(self, 2, popOOP()); NEXT_BYTECODE;
    BYTECODE(99, stRecVar3) storeReceiverVariable(self, 3, popOOP()); NEXT_BYTECODE;
    BYTECODE(100,stRecVar4) storeReceiverVariable(self, 4, popOOP()); NEXT_BYTECODE;
    BYTECODE(101,stRecVar5) storeReceiverVariable(self, 5, popOOP()); NEXT_BYTECODE;
    BYTECODE(102,stRecVar6) storeReceiverVariable(self, 6, popOOP()); NEXT_BYTECODE;
    BYTECODE(103,stRecVar7) storeReceiverVariable(self, 7, popOOP()); NEXT_BYTECODE;

    BYTECODE(104, stTemp0) storeMethodTemporary(0, popOOP()); NEXT_BYTECODE;
    BYTECODE(105, stTemp1) storeMethodTemporary(1, popOOP()); NEXT_BYTECODE;
    BYTECODE(106, stTemp2) storeMethodTemporary(2, popOOP()); NEXT_BYTECODE;
    BYTECODE(107, stTemp3) storeMethodTemporary(3, popOOP()); NEXT_BYTECODE;
    BYTECODE(108, stTemp4) storeMethodTemporary(4, popOOP()); NEXT_BYTECODE;
    BYTECODE(109, stTemp5) storeMethodTemporary(5, popOOP()); NEXT_BYTECODE;
    BYTECODE(110, stTemp6) storeMethodTemporary(6, popOOP()); NEXT_BYTECODE;
    BYTECODE(111, stTemp7) storeMethodTemporary(7, popOOP()); NEXT_BYTECODE;

    BYTECODE(112, pushSelf) uncheckedPushOOP(self);      NEXT_BYTECODE;
    BYTECODE(113, pushTrue) uncheckedPushOOP(trueOOP);	 NEXT_BYTECODE;
    BYTECODE(114, pushFalse) uncheckedPushOOP(falseOOP); NEXT_BYTECODE;
    BYTECODE(115, pushNil) uncheckedPushOOP(nilOOP); 	 NEXT_BYTECODE;
    BYTECODE(116, pushNeg1) pushInt(-1);		 NEXT_BYTECODE;
    BYTECODE(117, push0) pushInt(0);			 NEXT_BYTECODE;
    BYTECODE(118, push1) pushInt(1);			 NEXT_BYTECODE;
    BYTECODE(119, push2) pushInt(2);			 NEXT_BYTECODE;


    ALSO(120, retSelf)
    ALSO(121, retTrue)
    ALSO(122, retFalse)
    ALSO(123, retNil)
      switch (*ip & 3) {
      case 0: uncheckedPushOOP(self);   	break;
      case 1: uncheckedPushOOP(trueOOP);	break;
      case 2: uncheckedPushOOP(falseOOP); 	break;
      case 3: uncheckedPushOOP(nilOOP); 	break;
      }

      /* fall through */

    BYTECODE(124, explicitRet)		/* return stack top from method */
#ifdef old_code
/**/abortMethod:			/* here if ^C is seen to abort things */
#endif
      returnedValue = popOOP();

      if (isBlockContext(thisContextOOP)) {
	/*
	 * We're executing in a block context and an explicit return is
	 * encountered.  This means that we are to return from the caller of
	 * the method that created the block context, no matter how many
	 * levels of message sending are between where we currently are and
	 * our parent method context.
	 */
	blockContext = (BlockContext)oopToObj(thisContextOOP);
	methodContextOOP = blockContext->home;
	if (noParentContext(methodContextOOP)) {
	  OOP prevContextOOP;
	  prevContextOOP = thisContextOOP;

	  /* First walk down the contexts until we reach a valid one. */
	  while (noParentContext(methodContextOOP)) {
	    thisContextOOP = blockContext->caller;
	    if (isBlockContext(thisContextOOP)) {
	      blockContext = (BlockContext)oopToObj(thisContextOOP);
	      methodContextOOP = blockContext->home;
	    } else {
	      methodContextOOP = thisContextOOP;
              break;
            }
	  }

	  /* If aborting execution, doesn't matter.  Else send a message to the
	     context */
#ifdef old_code
/**/	  if (!abortExecution) {
#endif
          exportRegs();
          returnWithValue(returnedValue, methodContextOOP);
          sendMessage(internString("badReturnError"), 0, nil);
          importRegs();
          NEXT_BYTECODE;
#ifdef old_code
/**/	  }
#endif
	} /* test for block context in a dead method */
      } else {
	methodContextOOP = thisContextOOP;
      }
      if (methodCount) {
	methodCount--;
	totalMethods++;
      }

      exportRegs();
      returnWithValue(returnedValue, methodContextOOP);
      importRegs();		/* don't need to export these */
      if (!ip) {
        inInterpreter = false;
        return;
      }
      literals = getMethodLiterals(thisMethod);
      NEXT_BYTECODE;

    BYTECODE(125, retStackTop)
      returnedValue = popOOP();
      exportRegs();
      returnWithValue(returnedValue, thisContextOOP);
      importRegs();
      if (!ip) {
        inInterpreter = false;
        return;
      }
      literals = getMethodLiterals(thisMethod);
      NEXT_BYTECODE;

    /* 126, not in the blue book, is used for big literals access as follows:
       01111110 yyxxxxxx xxxxxxxx
       on method literal xxx do
            yy = 00 -> push constant 
            yy = 01 -> push variable
            yy = 10 -> store variable
            yy = 11 -> pop/store variable
    */

    BYTECODE(126, bigLiteralOp);
      {
        unsigned int num;
        num = *ip++;
        num <<= 8;
        num |= *ip++;

        switch(num >> 14) {
          case 0:
            pushOOP(methodLiteral(num & 16383)); 
            break;
          case 1:
            pushOOP(methodVariable(num & 16383));
            break;
	  case 2:
	    storeMethodVariable(num & 16383, popOOP());
	    break;
	  case 3:
	    storeMethodVariable(num & 16383, stackTop());
	    break;
        }
        NEXT_BYTECODE;
      }

    ALSO(127, breakpoint)
      /* we reenter execution in the same bytecode.  That's why we use
         ALSO */
      {
        OOP returned;
        MethodContext thisContext = (MethodContext)oopToObj(thisContextOOP);
        thisContext->hasBlock = trueOOP;

        returned = popOOP();
        pushOOP(self);
        pushOOP(thisContextOOP);
        pushOOP(returned);
        exportRegs();
        sendMessage(internString("breakpoint:return:"), 2, false);
        importRegs();
        NEXT_BYTECODE;
      }

    BYTECODE(128, pushIndexedVal)
      ival2 = *ip++;
      switch (ival2 >> 6) {
      case 0:
	pushOOP(receiverVariable(self, (signed char)ival2 & 63));
	break;
      case 1:
	pushOOP(methodTemporary(ival2 & 63));
	break;
      case 2:
	pushOOP(methodLiteral(ival2 & 63));
	break;
      case 3:
	pushOOP(methodVariable(ival2 & 63));
	break;
      }
      NEXT_BYTECODE;

    BYTECODE(129, storeStackTop)
      ival2 = *ip++;
      switch (ival2 >> 6) {
      case 0:
	storeReceiverVariable(self, (signed char)ival2 & 63, stackTop());
	break;
      case 1:
	storeMethodTemporary(ival2 & 63, stackTop());
	break;
      case 2:
	errorf("Attempt to store into a method constant");
	break;
      case 3:
	storeMethodVariable(ival2 & 63, stackTop());
      }
      NEXT_BYTECODE;

    BYTECODE(130, popAndStoreTop)
      ival2 = *ip++;
      switch (ival2 >> 6) {
      case 0:
	storeReceiverVariable(self, (signed char)ival2 & 63, popOOP());
	break;
      case 1:
	storeMethodTemporary(ival2 & 63, popOOP());
	break;
      case 2:
	errorf("Attempt to store into a method constant");
	break;
      case 3:
	storeMethodVariable(ival2 & 63, popOOP());
      }
      NEXT_BYTECODE;

    BYTECODE(131, sendShort)	/* send selector y (xxxyyyyy), x args */
      ival2 = *ip++;
      /* ### Send message knows the number of arguments that are being
	 passed.  We could easily adjust the stack pointer here by doing
	 some kind of popNOOPs.  The only trouble is what happens when
	 the number of args doesn't agree with what the method is expecting,
	 and we have to generate an error.  Also, if we don't export the sp
	 here, we'll have to pass this as a parameter and sendMessage will
	 have to export it anyway.  The cost of an export or import is
	 about 1 or 2 instructions, so it may be cheap enough to just do
	 in the places that we need to to it */
      exportRegs();		/* ### can this be removed? */
      sendMessage(methodLiteral(ival2 & 31), ival2 >> 5, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(132, sendLong)	/* send selector y (xxxxxxxx,yyyyyyyy) x args*/
      ival2 = *ip++;		/* the number of args */
      ival3 = *ip++;		/* the selector */
      exportRegs();
      sendMessage(methodLiteral(ival3), ival2, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(133, supSendShort)	/* send super selector y (xxxyyyyy), x args*/
      ival2 = *ip++;
      exportRegs();
      sendToSuper(methodLiteral(ival2 & 31), ival2 >> 5, true);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(134, supSendLong)	/* send super y (xxxxxxxx,yyyyyyyy) x args */
      ival2 = *ip++;		/* the number of args */
      ival3 = *ip++;		/* the selector */
      exportRegs();
      sendToSuper(methodLiteral(ival3), ival2, true);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(135, popStack)
      popNOOPs(1);
      NEXT_BYTECODE;

    BYTECODE(136, dupStack)
      tempOOP = stackTop();
      pushOOP(tempOOP);
      NEXT_BYTECODE;

    BYTECODE(137, pushThisContext) /* push active context */
      {
        MethodContext thisContext = (MethodContext)oopToObj(thisContextOOP);

        thisContext->hasBlock = trueOOP;
        pushOOP(thisContextOOP);
	numThisContexts++;
#ifdef PROFBLOCK
        ps.numThisContexts++;
#endif
      }
      NEXT_BYTECODE;

    BYTECODE(138, outerTempOp)
      {
	unsigned int numScopes;
	unsigned int varIndex;
	OOP	contextOOP;
	BlockContext context;
	
	varIndex = *ip++;
	numScopes = *ip++;

	context = (BlockContext)oopToObj(thisContextOOP);
	do {
	  contextOOP = context->outerContext;
	  context = (BlockContext)oopToObj(contextOOP);
	} while (--numScopes);

#ifndef OPTIMIZE
	if (context != nil) {	/* we're on the right one */
#endif
	  switch(varIndex >> 6) {
	    case 0:
	      errorf("Invalid bytecode 138/0");
	      break;
	    case 1:
	      pushOOP(context->contextStack[varIndex & 63]);
	      break;
	    case 2:
	      context->contextStack[varIndex & 63] = popOOP();
	      break;
	    case 3:
	      context->contextStack[varIndex & 63] = stackTop();
	      break;
#ifndef OPTIMIZE
	  }
#endif
	}
      }
      NEXT_BYTECODE;

    /* :-( Sigh ... have to sacrifice 139 for nop... )-: */
    BYTECODE(139, nop); 	    NEXT_BYTECODE;

    BYTECODE(140, topSelf); uncheckedSetTop(self); NEXT_BYTECODE;
    BYTECODE(141, topOne); uncheckedSetTop(fromInt(1)); NEXT_BYTECODE;

    BYTECODE(142, topIndexedVal);
      ival2 = *ip++;
      switch (ival2 >> 6) {
      case 0:
	setStackTop(receiverVariable(self, (signed char)ival2 & 63));
	break;
      case 1:
	setStackTop(methodTemporary(ival2 & 63));
	break;
      case 2:
	setStackTop(methodLiteral(ival2 & 63));
	break;
      case 3:
	setStackTop(methodVariable(ival2 & 63));
	break;
      }
      NEXT_BYTECODE;

    /* :-) THE LAST EXTRA BYTECODES!!!  143 UNUSED!!!!! (-: */


    /* Forward jump by 1 to 8 */
    BYTECODE(144, shJmp1) ip += 1;  NEXT_BYTECODE;
    BYTECODE(145, shJmp2) ip += 2;  NEXT_BYTECODE;
    BYTECODE(146, shJmp3) ip += 3;  NEXT_BYTECODE;
    BYTECODE(147, shJmp4) ip += 4;  NEXT_BYTECODE;
    BYTECODE(148, shJmp5) ip += 5;  NEXT_BYTECODE;
    BYTECODE(149, shJmp6) ip += 6;  NEXT_BYTECODE;
    BYTECODE(150, shJmp7) ip += 7;  NEXT_BYTECODE;
    BYTECODE(151, shJmp8) ip += 8;  NEXT_BYTECODE;


#define jmpFalse(bytes) \
  if ((tempOOP = popOOP()) == falseOOP) { /* jump forward if false */ \
    ip += (bytes); \
  } else if (tempOOP != trueOOP) { \
    ip += (bytes); \
    unPop(1); \
    exportRegs(); \
    sendMessage(internString("mustBeBoolean"), 0, false); \
    importRegs(); \
    NEXT_BYTECODE;				\
  }

    BYTECODE(152, shJmp1False) jmpFalse(1); NEXT_BYTECODE;
    BYTECODE(153, shJmp2False) jmpFalse(2); NEXT_BYTECODE;
    BYTECODE(154, shJmp3False) jmpFalse(3); NEXT_BYTECODE;
    BYTECODE(155, shJmp4False) jmpFalse(4); NEXT_BYTECODE;
    BYTECODE(156, shJmp5False) jmpFalse(5); NEXT_BYTECODE;
    BYTECODE(157, shJmp6False) jmpFalse(6); NEXT_BYTECODE;
    BYTECODE(158, shJmp7False) jmpFalse(7); NEXT_BYTECODE;
    BYTECODE(159, shJmp8False) jmpFalse(8); NEXT_BYTECODE;

#undef jmpFalse

#define longJmpSigned(highDelta) \
  ival2 = *ip++;		/* jump forward or back */ \
  ip += (long)((highDelta) << 8) + ival2
    
  /*
   * only do the NEXT_BYTECODE on those that branch and can
   * backward and create an infinite loop.
   */

    BYTECODE(160, longJmpNeg4) longJmpSigned(-4); NEXT_BYTECODE;
    BYTECODE(161, longJmpNeg3) longJmpSigned(-3); NEXT_BYTECODE;
    BYTECODE(162, longJmpNeg2) longJmpSigned(-2); NEXT_BYTECODE;
    BYTECODE(163, longJmpNeg1) longJmpSigned(-1); NEXT_BYTECODE;
    BYTECODE(164, longJmp0)    longJmpSigned(0);  NEXT_BYTECODE;
    BYTECODE(165, longJmp1)    longJmpSigned(1);  NEXT_BYTECODE;
    BYTECODE(166, longJmp2)    longJmpSigned(2);  NEXT_BYTECODE;
    BYTECODE(167, longJmp3)    longJmpSigned(3);  NEXT_BYTECODE;

#undef longJmpSigned



#define boolJmp(highDelta, jmpCaseOOP, nonJmpCaseOOP)	\
  ival2 = *ip++; 					\
  if ((tempOOP = popOOP()) == (jmpCaseOOP)) { 		\
    ip += (highDelta << 8) + ival2; 			\
  } else if (tempOOP != (nonJmpCaseOOP)) { 		\
    ip += (highDelta << 8) + ival2; 			\
    unPop(1); \
    exportRegs(); \
    sendMessage(internString("booleanRequired"), 0, false); \
    importRegs(); \
    NEXT_BYTECODE;				\
  } 

#define trueJmp(highDelta) \
  boolJmp(highDelta, trueOOP, falseOOP)
    
#define falseJmp(highDelta) \
  boolJmp(highDelta, falseOOP, trueOOP)
  


    BYTECODE(168, popJmpTrue0)  trueJmp(0);  NEXT_BYTECODE;
    BYTECODE(169, popJmpTrue1)  trueJmp(1);  NEXT_BYTECODE;
    BYTECODE(170, popJmpTrue2)  trueJmp(2);  NEXT_BYTECODE;
    BYTECODE(171, popJmpTrue3)  trueJmp(3);  NEXT_BYTECODE;

    BYTECODE(172, popJmpFalse0) falseJmp(0); NEXT_BYTECODE;
    BYTECODE(173, popJmpFalse1) falseJmp(1); NEXT_BYTECODE;
    BYTECODE(174, popJmpFalse2) falseJmp(2); NEXT_BYTECODE;
    BYTECODE(175, popJmpFalse3) falseJmp(3); NEXT_BYTECODE;

#undef falseJmp
#undef trueJmp
#undef boolJmp



#ifndef JUMP_LOOKAHEAD
#define DO_JUMP_LOOKAHEAD(x) {		\
  setStackTopBoolean(x);		\
  NEXT_BYTECODE;			\
}
#else

#define DO_JUMP_LOOKAHEAD(x) {     	\
  if (x) {                        	\
    popNOOPs(1);                       	\
    goto *trueByteCodes[*ip]; 		\
  } else {                        	\
    popNOOPs(1);                       	\
    goto *falseByteCodes[*ip];		\
  }					\
}

    BRANCH_LABEL(lookaheadFailedTrue)
      uncheckedPushOOP(trueOOP);
      NEXT_BYTECODE;

    BRANCH_LABEL(lookaheadFailedFalse)
      uncheckedPushOOP(falseOOP);
      NEXT_BYTECODE;

    BRANCH_LABEL(lookaheadLongJumpNotTaken)
      byteCodeCounter++;
      ip += 2;
      NEXT_BYTECODE;
#endif


       
#ifndef OPEN_CODE_MATH

#define RAW_INT_OP(op, noOverflow)
#define RAW_FLOAT_OP(op)
#define INTERP_BASIC_OP(op, fop, overflow)
#define RAW_BOOL_OP(operator)						\
#define RAW_BOOL_FLOAT_OP(operator)
#define INTERP_BASIC_BOOL(operator)

#else

#define RAW_INT_OP(op, noOverflow)      \
  if (isInt(stackTop())) {		\
    register long iarg1, iarg2;		\
    iarg1 = toInt(tempOOP);		\
    iarg2 = toInt(popOOP());		\
    op;                        		\
    if(noOverflow || !INT_OVERFLOW(iarg1)) { \
      setStackTopInt(iarg1);	   	\
      NEXT_BYTECODE;			\
    } else {                            \
      unPop(1);				\
    }					\
  }

#define RAW_FLOAT_OP(op)		\
{					\
  OOP	oop2,oopResult;			\
  double farg1, farg2;			\
  oop2 = popOOP();			\
  if (isClass(oop2, floatClass)) {	\
    farg1 = floatOOPValue(tempOOP);	\
    farg2 = floatOOPValue(oop2);	\
    exportRegs();			\
    oopResult = floatNew(op);           \
    importRegs();			\
    setStackTop(oopResult);		\
    NEXT_BYTECODE;			\
  }					\
  unPop(1);				\
}


#define INTERP_BASIC_OP(op, fop, overflow)	\
  tempOOP = stackAt(1);			\
  if (isInt(tempOOP)) {			\
    primitivesExecuted++;		\
    RAW_INT_OP(op, overflow);	       	\
  } else if (oopClass(tempOOP) == floatClass) {	\
    primitivesExecuted++;		\
    RAW_FLOAT_OP(fop);	        	\
  }


#define RAW_BOOL_OP(operator)						\
  if (isInt(stackTop())) {						\
    DO_JUMP_LOOKAHEAD(	((long)tempOOP) operator ((long)popOOP()) );	\
    NEXT_BYTECODE;							\
  }

#define RAW_BOOL_FLOAT_OP(operator)	\
{					\
  OOP	oop2;				\
  oop2 = popOOP();			\
  if (isClass(oop2, floatClass)) {	\
    double farg1, farg2;		\
    farg1 = floatOOPValue(tempOOP);	\
    farg2 = floatOOPValue(oop2);	\
    DO_JUMP_LOOKAHEAD(farg1 operator farg2); \
    NEXT_BYTECODE;			\
  }					\
  unPop(1);				\
}

#define INTERP_BASIC_BOOL(operator)	\
  tempOOP = stackAt(1);			\
  if (isInt(tempOOP)) {			\
    primitivesExecuted++;		\
    RAW_BOOL_OP(operator);		\
  } else if (oopClass(tempOOP) == floatClass) {	\
    primitivesExecuted++;		\
    RAW_BOOL_FLOAT_OP(operator);	\
  }

#endif

    BYTECODE(176, sendPlus)
      INTERP_BASIC_OP(iarg1 += iarg2, farg1 + farg2, false);
      exportRegs();
      sendMessage(plusSymbol, 1, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(177, sendMinus)
      INTERP_BASIC_OP(iarg1 -= iarg2, farg1 - farg2, false);
      exportRegs();
      sendMessage(minusSymbol, 1, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(178, sendLess)
      INTERP_BASIC_BOOL(<);
      exportRegs();
      sendMessage(lessThanSymbol, 1, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(179, sendGreater)
      INTERP_BASIC_BOOL(>);
      exportRegs();
      sendMessage(greaterThanSymbol, 1, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(180, sendLessEq)
      INTERP_BASIC_BOOL(<=);
      exportRegs();
      sendMessage(lessEqualSymbol, 1, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(181, sendGreaterEq)
      INTERP_BASIC_BOOL(>=);
      exportRegs();
      sendMessage(greaterEqualSymbol, 1, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(182, sendEqual)
      INTERP_BASIC_BOOL(==);
      exportRegs();
      sendMessage(equalSymbol, 1, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(183, sendNotEqual)
      INTERP_BASIC_BOOL(!=);
      exportRegs();
      sendMessage(notEqualSymbol, 1, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(184, sendTimes)
      INTERP_BASIC_OP(iarg1 = mulWithCheck(iarg1, iarg2), farg1 * farg2, false);
      exportRegs();
      sendMessage(timesSymbol, 1, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(185, sendDivide)
      exportRegs();
      sendMessage(divideSymbol, 1, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(186, sendRemainder)
      exportRegs();
      sendMessage(remainderSymbol, 1, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(187, sendAtSign)
      exportRegs();
      /* The compiler won't even generate this bytecode */
      sendMessage(internString("@"), 1, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(188, sendBitShift)
      tempOOP = stackAt(1);
      if (isInt(tempOOP)) {
	register long	iarg1, iarg2;
	if (isInt(stackTop())) {
	  iarg1 = toInt(tempOOP);
	  iarg2 = toInt(popOOP());
          if (iarg2 > 0 && iarg2 <= ST_INT_SIZE) {
            long guard = iarg1 >> (ST_INT_SIZE - iarg2); 
            if(guard == (guard >> 1)) {
	      setStackTopInt(iarg1 << iarg2);
	      NEXT_BYTECODE;
	    }
          } else if (iarg2 <= 0) {
	    setStackTopInt(iarg1 >> -iarg2);
            NEXT_BYTECODE;
          }
	}
	unPop(1);
      }
      exportRegs();
      sendMessage(bitShiftColonSymbol, 1, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(189, sendIntDivide)
      exportRegs();
      sendMessage(integerDivideSymbol, 1, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(190, sendBitAnd)
      tempOOP = stackAt(1);
      if (isInt(tempOOP)) {
	RAW_INT_OP(iarg1 &= iarg2, true);
      }
      exportRegs();
      sendMessage(bitAndColonSymbol, 1, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(191, sendBitOr)
      tempOOP = stackAt(1);
      if (isInt(tempOOP)) {
	RAW_INT_OP(iarg1 |= iarg2, true);
      }
      exportRegs();
      sendMessage(bitOrColonSymbol, 1, false);
      importRegs();
      NEXT_BYTECODE;



    BYTECODE(192, sendAt)
      exportRegs();
      sendMessage(atColonSymbol, 1, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(193, sendAtPut)
      exportRegs();
      sendMessage(atColonPutColonSymbol, 2, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(194, sendSize)
      exportRegs();
      sendMessage(sizeSymbol, 0, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(195, sendNext)
      exportRegs();
      sendMessage(nextSymbol, 0, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(196, sendNextPut)
      exportRegs();
      sendMessage(nextPutColonSymbol, 1, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(197, sendAtEnd)
      exportRegs();
      sendMessage(atEndSymbol, 0, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(198, sendSameObject)
      {
        OOP oop1;
        tempOOP = stackAt(1);
        oop1 = popOOP();
        DO_JUMP_LOOKAHEAD(oop1 == tempOOP);	
      }

    BYTECODE(199, sendClass)
      exportRegs();
      sendMessage(classSymbol, 0, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(200, sendBlockCopy)
      exportRegs();
      sendMessage(blockCopyColonTemporariesColonSymbol, 2, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(201, sendValue)
      exportRegs();
      sendMessage(valueSymbol, 0, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(202, sendValueColon)
      exportRegs();
      sendMessage(valueColonSymbol, 1, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(203, sendDoColon)
      exportRegs();
      sendMessage(doColonSymbol, 1, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(204, sendNew)
      exportRegs();
      sendMessage(newSymbol, 0, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(205, sendNewColon)
      exportRegs();
      sendMessage(newColonSymbol, 1, false);
      importRegs();
      NEXT_BYTECODE;

    BYTECODE(206, sendIsNil)
      DO_JUMP_LOOKAHEAD(stackTop() == nilOOP);	
      NEXT_BYTECODE;

    BYTECODE(207, sendNotNil)
      DO_JUMP_LOOKAHEAD(stackTop() != nilOOP);	
      NEXT_BYTECODE;


#define sendLit(index, numArgs) \
  exportRegs();			\
  sendMessage(methodLiteral((index)), (numArgs), false); \
  importRegs()


    BYTECODE(208, sendNoArg0)  sendLit(0, 0);   NEXT_BYTECODE;
    BYTECODE(209, sendNoArg1)  sendLit(1, 0);   NEXT_BYTECODE;
    BYTECODE(210, sendNoArg2)  sendLit(2, 0);   NEXT_BYTECODE;
    BYTECODE(211, sendNoArg3)  sendLit(3, 0);   NEXT_BYTECODE;
    BYTECODE(212, sendNoArg4)  sendLit(4, 0);   NEXT_BYTECODE;
    BYTECODE(213, sendNoArg5)  sendLit(5, 0);   NEXT_BYTECODE;
    BYTECODE(214, sendNoArg6)  sendLit(6, 0);   NEXT_BYTECODE;
    BYTECODE(215, sendNoArg7)  sendLit(7, 0);   NEXT_BYTECODE;
    BYTECODE(216, sendNoArg8)  sendLit(8, 0);   NEXT_BYTECODE;
    BYTECODE(217, sendNoArg9)  sendLit(9, 0);   NEXT_BYTECODE;
    BYTECODE(218, sendNoArg10) sendLit(10, 0);  NEXT_BYTECODE;
    BYTECODE(219, sendNoArg11) sendLit(11, 0);  NEXT_BYTECODE;
    BYTECODE(220, sendNoArg12) sendLit(12, 0);  NEXT_BYTECODE;
    BYTECODE(221, sendNoArg13) sendLit(13, 0);  NEXT_BYTECODE;
    BYTECODE(222, sendNoArg14) sendLit(14, 0);  NEXT_BYTECODE;
    BYTECODE(223, sendNoArg15) sendLit(15, 0);  NEXT_BYTECODE;

    BYTECODE(224, sendOneArg0)  sendLit(0, 1);  NEXT_BYTECODE;
    BYTECODE(225, sendOneArg1)  sendLit(1, 1);  NEXT_BYTECODE;
    BYTECODE(226, sendOneArg2)  sendLit(2, 1);  NEXT_BYTECODE;
    BYTECODE(227, sendOneArg3)  sendLit(3, 1);  NEXT_BYTECODE;
    BYTECODE(228, sendOneArg4)  sendLit(4, 1);  NEXT_BYTECODE;
    BYTECODE(229, sendOneArg5)  sendLit(5, 1);  NEXT_BYTECODE;
    BYTECODE(230, sendOneArg6)  sendLit(6, 1);  NEXT_BYTECODE;
    BYTECODE(231, sendOneArg7)  sendLit(7, 1);  NEXT_BYTECODE;
    BYTECODE(232, sendOneArg8)  sendLit(8, 1);  NEXT_BYTECODE;
    BYTECODE(233, sendOneArg9)  sendLit(9, 1);  NEXT_BYTECODE;
    BYTECODE(234, sendOneArg10) sendLit(10, 1); NEXT_BYTECODE;
    BYTECODE(235, sendOneArg11) sendLit(11, 1); NEXT_BYTECODE;
    BYTECODE(236, sendOneArg12) sendLit(12, 1); NEXT_BYTECODE;
    BYTECODE(237, sendOneArg13) sendLit(13, 1); NEXT_BYTECODE;
    BYTECODE(238, sendOneArg14) sendLit(14, 1); NEXT_BYTECODE;
    BYTECODE(239, sendOneArg15) sendLit(15, 1); NEXT_BYTECODE;

    BYTECODE(240, sendTwoArg0)  sendLit(0, 2);  NEXT_BYTECODE;
    BYTECODE(241, sendTwoArg1)  sendLit(1, 2);  NEXT_BYTECODE;
    BYTECODE(242, sendTwoArg2)  sendLit(2, 2);  NEXT_BYTECODE;
    BYTECODE(243, sendTwoArg3)  sendLit(3, 2);  NEXT_BYTECODE;
    BYTECODE(244, sendTwoArg4)  sendLit(4, 2);  NEXT_BYTECODE;
    BYTECODE(245, sendTwoArg5)  sendLit(5, 2);  NEXT_BYTECODE;
    BYTECODE(246, sendTwoArg6)  sendLit(6, 2);  NEXT_BYTECODE;
    BYTECODE(247, sendTwoArg7)  sendLit(7, 2);  NEXT_BYTECODE;
    BYTECODE(248, sendTwoArg8)  sendLit(8, 2);  NEXT_BYTECODE;
    BYTECODE(249, sendTwoArg9)  sendLit(9, 2);  NEXT_BYTECODE;
    BYTECODE(250, sendTwoArg10) sendLit(10, 2); NEXT_BYTECODE;
    BYTECODE(251, sendTwoArg11) sendLit(11, 2); NEXT_BYTECODE;
    BYTECODE(252, sendTwoArg12) sendLit(12, 2); NEXT_BYTECODE;
    BYTECODE(253, sendTwoArg13) sendLit(13, 2); NEXT_BYTECODE;
    BYTECODE(254, sendTwoArg14) sendLit(14, 2); NEXT_BYTECODE;
    BYTECODE(255, sendTwoArg15) sendLit(15, 2); NEXT_BYTECODE;

#undef sendLit

/* !!! SBB NOTE: default seems pointless when it's not a switch */
    ALSO(143, unused143)
    DEFAULT_LABEL
      errorf("Invalid byte code %d executed\n", ip[-1]);
      NEXT_BYTECODE;
    END_DISPATCH
  }
}

/* Always use outer ip/sp outside interpret */
#ifdef LOCAL_REGS
  #define ip		outer__IP
  #define sp		outer__SP
#endif



static void 
changeProcessContext(newProcess)
     OOP	newProcess;
{
  MethodContext thisContext, methodContext;
  OOP		processOOP, methodContextOOP;
  Process	process;
  ProcessorScheduler processor;
  
  switchToProcess = nilOOP;

  if (!isNil(thisContextOOP)) {
    thisContext = (MethodContext)oopToObj(thisContextOOP);
    /* save old context information */
    thisContext->ipOffset = fromInt(relativeByteIndex(ip, thisMethod));
    /* leave sp pointing to receiver, which is replaced on return with value*/
    thisContext->spOffset = fromInt(sp - thisContext->contextStack);
  }

  processor = (ProcessorScheduler)oopToObj(processorOOP);
  processOOP = processor->activeProcess;
  if (processOOP != newProcess && !isProcessTerminating(processOOP)) {
    process = (Process)oopToObj(processOOP);
    process->suspendedContext = thisContextOOP;
  }

  processor->activeProcess = newProcess;
  process = (Process)oopToObj(newProcess);
  thisContextOOP = process->suspendedContext;

  /* ### should this be block context? */
  thisContext = (MethodContext)oopToObj(thisContextOOP);

  methodContextOOP = getMethodContext(thisContextOOP);
  methodContext = (MethodContext)oopToObj(methodContextOOP);
  thisMethod = methodContext->method;
  ip = toInt(thisContext->ipOffset) + getMethodByteCodes(thisMethod);
  sp = thisContext->contextStack + toInt(thisContext->spOffset);

#ifdef test
/**/  printf("thisMethod %d, isNil %d \nipOffset %d \nip %d\n", 
/**/    thisMethod, isNil(thisMethod), toInt(thisContext->ipOffset), ip);
#endif

  temporaries = thisContext->contextStack;
  literals = getMethodLiterals(thisMethod);

  /* self lives in the method, not in the block */
  self = methodContext->receiver;
}

#ifdef ACCESSOR_DEBUGGING

/*
 *	static mst_Boolean noParentContext(methodContextOOP)
 *
 * Description
 *
 *	Returns true if there is no parent context for "methodContextOOP".
 *	This occurs when the method context has been returned from, but it had
 *	created a block context during its execution and so it was not
 *	deallocated when it returned.  Now some block context is trying to
 *	return from that method context, but where to return to is undefined.
 *
 * Inputs
 *
 *	methodContextOOP: 
 *		An OOP that is the method context to be examined.
 *
 * Outputs
 *
 *	True if the current method has no parent, false otherwise.
 */
static
mst_Boolean noParentContext(methodContextOOP)
     OOP methodContextOOP;
{
  return (noParentContextInternal(contextOOP));
}

/*
 *	static OOP getMethodContext(contextOOP)
 *
 * Description
 *
 *	Returns the method context for either a block context or a method
 *	context. 
 *
 * Inputs
 *
 *	contextOOP: Block or Method context OOP
 *		
 *
 * Outputs
 *
 *	Method context for CONTEXTOOP.
 */
static OOP
getMethodContext(contextOOP)
     OOP	contextOOP;
{
  return (getMethodContextInternal(contextOOP));
}
#endif /* ACCESSOR_DEBUGGING */



/* I made CONTEXTS_PER_CHUNK a nice power of two.  Allocate 64KB at a time,
   never use more than 3 MB;  anyway these are here so behavior can be fine
   tuned */

#define CHUNK_SIZE			65536
#define MAX_CHUNKS_IN_MEMORY		48

long numMethods, numBlocks;
long totalRealized = 0;

MethodContext		fakeList[MAX_CTX_SIZE];
static char		*curChunkBegin = nil, *curChunkEnd = nil;
static char		*chunks[MAX_CHUNKS_IN_MEMORY];
static char		**chunk = chunks - 1;

#define	lastChunk 	(chunks + MAX_CHUNKS_IN_MEMORY)

void
resetFakeContexts()
{
  register unsigned int i;
  curChunkBegin = curChunkEnd = nil;
  chunk = chunks - 1;
  for (i = 0; i < MAX_CTX_SIZE; i++) {
    fakeList[i] = nil;
  }
}

static inline OOP
allocFakeContext(classOOP, size)
     OOP classOOP;
     register int size;
{
  register OOP			oop;
  register MethodContext	methodContext;

  size = CTX_SIZE(size);

#ifndef OPTIMIZE
  numBlocks += (classOOP == blockContextClass);
  numMethods += (classOOP == methodContextClass);
#endif

  if (!fakeList[size]) {
    curChunkBegin += size2Bytes(size);
    if (curChunkBegin > curChunkEnd) {
      /* Not enough room in the current chunk */
      if (++chunk >= lastChunk) {
        /* No more chunks available - GC */
        minorGCFlip();
        ++chunk;
      }
      if (!*chunk) {
        *chunk = xmalloc(CHUNK_SIZE);
      }

      curChunkBegin = *chunk + size2Bytes(size);
      curChunkEnd = *chunk + CHUNK_SIZE;
    }

#ifdef PROFBLOCK
    ps.numMethodAllocs++;
#endif
    methodContext = (MethodContext) (curChunkBegin - size2Bytes(size));
    /* printf("[[[[ Allocing new fake %d %x\n", size, methodContext); */
    /* fakeList[size] = nil; */

  } else {
    methodContext = fakeList[size];
    fakeList[size] = (MethodContext)methodContext->sender;
    /* printf("[[[[ Reusing fake %d %d\n", size, methodContext->objSize); */
#ifdef PROFBLOCK
    ps.numMethodReclaims++;
#endif
  }

#ifdef pointless
/**/ nilFill(&methodContext->sender,
/**/   size - (sizeof(ObjectHeader) / sizeof(OOP)));
#endif

  methodContext->objSize = size;
  methodContext->objClass = classOOP;
  oop = allocOOP(methodContext);
  oop->flags |= F_FAKE | F_CONTEXT;
  return (oop);
}

static inline void
deallocFakeContext(context)
     register MethodContext	context;
{
#ifdef PROFBLOCK
  ps.numMethodFrees++;
#endif

  context->sender = (OOP) fakeList[context->objSize];
  fakeList[context->objSize] = context;
}



#ifdef ACCESSOR_DEBUGGING
/*
 *	static mst_Boolean isBlockContext(contextOOP)
 *
 * Description
 *
 *	Returns true if "contextOOP" is a block context.
 *
 * Inputs
 *
 *	contextOOP: 
 *		an OOP for a context that is to be checked.
 *
 * Outputs
 *
 *	True if it's a block context, false otherwise.
 */
static mst_Boolean
isBlockContext(contextOOP)
     OOP	contextOOP;
{
  return (oopClass(contextOOP) == blockContextClass);
}
#endif /* ACCESSOR_DEBUGGING */


#include "prims.inl"


/*
 * on entry to this routine, the stack should have the receiver and the
 * arguments pushed on the stack.  We need to get a new context,
 * setup things like the IP, SP, and Temporary pointers, and then
 * return.   Note that this routine DOES NOT invoke the interpreter; it merely
 * sets up a new context so that calling (or, more typically, returning to) the
 * interpreter will operate properly.  This kind of sending is for normal
 * messages only.  Things like sending a "value" message to a block context are
 * handled by primitives which do similar things, but they use information from
 * the block and method contexts that we don't have available (or need) here.
 */

void
sendMessageInternal(sendSelector, sendArgs, receiver, methodClass)
     OOP	sendSelector;
     int	sendArgs;
     OOP	receiver;
     OOP	methodClass;	/* the class in which to start the search */
{
  volatile OOP	 		methodOOP;
  OOP			 	newContextOOP;
  MethodHeader		 	header;
  int			 	numTemps;
  long			 	hashIndex;
  register MethodContext 	newContext;
  register MethodCacheEntry	*methodData;

  /* hash the selector and the class of the receiver together using XOR.
   * Since both are pointers to long word aligned quantities, shift over
   * by 2 bits to remove the useless low order zeros.  Actually, since
   * they are addresses in the oopTable, and since oopTable entries are
   * 8 bytes long, we can profitably shift over 3 bits */

  sampleCounter++;
  hashIndex = ((long)sendSelector ^ (long)methodClass) >> (LONG_SHIFT+1);
  hashIndex &= (METHOD_CACHE_SIZE - 1);
  methodData = &methodCache[hashIndex];

  if (methodData->selectorOOP != sendSelector
      || methodData->classOOP != methodClass) {
    /* :-( cache miss )-: */
    IncPtr	inc;
    long	i;
    OOP		argsArray;

    methodData->classOOP = methodClass;
    methodData->selectorOOP = sendSelector;
    for (;;) {
      if (!isNil(methodClass)) {
        methodOOP = findClassMethod(methodClass, sendSelector);
        if (!isNil(methodOOP)) {
          break;
        }
        if (isNil(superClass(methodClass))) {
          debug();
          methodOOP = findClassMethod(methodClass, sendSelector);
        }
        methodClass = superClass(methodClass);
      } else {
        inc = incSavePointer();
        argsArray = arrayNew(sendArgs);
        incAddOOP(argsArray);
        for (i = 0; i < sendArgs; i++) {
	  arrayAtPut(argsArray, i+1, stackAt(sendArgs-i-1));
        }
        popNOOPs(sendArgs);
        pushOOP(messageNewArgs(sendSelector, argsArray));
        incRestorePointer(inc);
        sendMessage(doesNotUnderstandColonSymbol, 1, false);
        return;
      }
    }

    methodData->methodOOP = methodOOP;
    methodData->methodClassOOP = methodClass;
#ifdef COLLISION_CHECKING
    methodData->collide++;
#endif
    cacheHits--;				/* see below */
    cacheMisses++;
  }
  /* This is redundant when a cache miss occurs, but I want the code
     to be as streamlined as possible when cache hits occur (which is
     the vast majority of cases) */
  cacheHits++;
  methodOOP = methodData->methodOOP;

  header = getMethodHeader(methodOOP);
#ifndef OPTIMIZE  
  if (header.numArgs != (unsigned)sendArgs) {
    OOP receiverClass;
    errorf("invalid number of arguments %d, expecting %d", sendArgs,
	   header.numArgs);

    if (isInt(receiver)) {
      receiverClass = integerClass;
    } else {
      receiverClass = oopClass(receiver);
    }
    printObject(receiverClass);
    if (receiverClass != methodData->methodClassOOP) {
      printf("(");
      printObject(methodData->methodClassOOP);
      printf(")");
    }
    printf(">>");
    printObject(methodData->selectorOOP);
    printf("\n");
    return;
  }
#endif

  switch (header.headerFlag) {
  case 1:			/* return self */
    selfReturns++;
    /* self is already on the stack...so we leave it */
    return;

  case 2:			/* return instance variable */
    instVarReturns++;
    /* replace receiver with the returned instance variable */
    setStackTop(receiverVariable(receiver, (long)header.numTemps));
    return;

  case 3:			/* return literal constant */
    literalReturns++;
    /* replace receiver with the returned literal constant */
    setStackTop( oopToObj(((Method)oopToObj(methodOOP))->literals)->data[0] );
    return;

  case 0:
    break;
  }

  if (header.primitiveIndex) {
    primitivesExecuted++;
    if (!executePrimitiveOperation(header.primitiveIndex, sendArgs, methodOOP)) {
      /* primitive succeeded.  Invoke the normal method */
      return;
    }
    /* primitive failed.  Invoke the normal method.  methodData may be
       clobbered by a setjmp in executePrimitiveOperation */
    methodData = &methodCache[hashIndex];
  }

#ifdef PROFBLOCK
  ps.stackDepth++;
  if (ps.stackDepth > ps.maxStackDepth) {
    ps.maxStackDepth = ps.stackDepth;
  }
#endif

  /* prepare the new state */
  numTemps = header.numTemps;
  newContextOOP = allocMethodContext(getMethodStackDepth(methodOOP));
  newContext = (MethodContext)oopToObj(newContextOOP);
  newContext->sender = thisContextOOP;
  newContext->method = methodOOP;
  newContext->methodClass = methodData->methodClassOOP;
  newContext->hasBlock = falseOOP;	/* becomes true when a block is created */
  newContext->ipOffset = fromInt(0);
  newContext->spOffset = fromInt(sendArgs + numTemps - 1);
  newContext->selector = sendSelector;
  newContext->receiver = receiver;

  /* save old context information */
  if (!isNil(thisContextOOP)) {
    register MethodContext	thisContext;
    thisContext = (MethodContext)oopToObj(thisContextOOP);
    /* leave sp pointing to receiver, which is replaced on return with value */
    sp -= sendArgs;
    thisContext->spOffset = fromInt(sp - thisContext->contextStack);
    thisContext->ipOffset = fromInt(relativeByteIndex(ip, thisMethod));
  }

  /* copy sendArgs arguments into new context */
  temporaries = newContext->contextStack;
  memcpy(temporaries, &sp[1], (sendArgs) * sizeof(OOP));

  /* initialize new VM state */
  thisMethod = methodOOP;
  literals = getMethodLiterals(thisMethod);
  thisContextOOP = newContextOOP;
  self = newContext->receiver;
  sp = &newContext->contextStack[sendArgs-1]; /* start of stack-1 */
  ip = getMethodByteCodes(thisMethod);

  /* args already on the stack - now that sp is ok push the temps too */
  pushNNils(numTemps);

  messagesSent++;
  /* ### fix getmethodbytecodes to check for actual byte codes in method */
}


/*
 *	static mst_Boolean sendBlockValue(numArgs)
 *
 * Description
 *
 *	This is the equivalent of sendMessage, but is for blocks.  The block
 *	context that is to the the receiver of the "value" message should be
 *	"numArgs" into the stack.  Temporaries come from the block's method
 *	context, as does self.  IP is set to the proper
 *	place within the block's method's byte codes, and SP is set to the top
 *	of the arguments in the block context, which have been copied out of
 *	the caller's context.
 *
 * Inputs
 *
 *	numArgs: 
 *		The number of arguments sent to the block.
 *
 * Outputs
 *
 *      true if failed, false if numArgs matches the argument of blockCopy:
 */
static mst_Boolean
sendBlockValue(numArgs)
     int	numArgs;
{
  OOP			closureOOP, blockContextOOP, methodContextOOP;
  register BlockContext	blockContext;
  register BlockClosure closure;
  MethodContext 	thisContext, methodContext;
  int           	numTemps;

#ifdef PROFBLOCK
  ps.numValues++;
  ps.stackDepth++;
  if (ps.stackDepth > ps.maxStackDepth) {
    ps.maxStackDepth = ps.stackDepth;
  }
#endif

  closureOOP = stackAt(numArgs);
  closure = (BlockClosure)oopToObj(closureOOP);
  if(numArgs != closure->numArgs) {
    /* check numArgs before realizing */
    return (true);
  }

  methodContextOOP = getMethodContext(closure->outerContext);
  methodContext = (MethodContext)oopToObj(methodContextOOP);
#ifndef OPTIMIZE
  if (oopClass(methodContextOOP) != methodContextClass) {
    errorf("Block's home is not a MethodContext!!!\n");
  }
#endif

  /* a gc flip might happen - so reload closure too.  Note we don't use the
    incubator because we ensure GC happens only before the used OOP is created.
    This might change in a future version. */
  blockContextOOP = allocBlockContext(getMethodStackDepth(methodContext->method));
  closure = (BlockClosure)oopToObj(closureOOP);

  /* prepare the new state, loading data from the closure */
  blockContext = (BlockContext)oopToObj(blockContextOOP);
  blockContext->numArgs = fromInt(closure->numArgs);
  blockContext->numTemps = fromInt(closure->numTemps);
  blockContext->hasBlock = falseOOP;
  blockContext->outerContext = closure->outerContext;
  blockContext->home = methodContextOOP;
  blockContext->caller = thisContextOOP;

  /* save old context information */
  if (!isNil(thisContextOOP)) {
    thisContext = (MethodContext)oopToObj(thisContextOOP);
    /* leave sp pointing to receiver, which is replaced on return with value*/
    sp -= numArgs;
    thisContext->spOffset = fromInt(sp - thisContext->contextStack);
    thisContext->ipOffset = fromInt(relativeByteIndex(ip, thisMethod));
  }

  /* copy numArgs arguments into new context */
  temporaries = blockContext->contextStack;
  memcpy(temporaries, &sp[1], numArgs * sizeof(OOP));

  thisMethod = methodContext->method;
  literals = getMethodLiterals(thisMethod);
  thisContextOOP = blockContextOOP;
  sp = &blockContext->contextStack[numArgs-1]; /* start of stack-1 */
  ip = closure->initialIP + getMethodByteCodes(thisMethod);
  self = methodContext->receiver;

  /* args already on the stack - now that sp is ok push the temps too */
  numTemps = closure->numTemps;
  pushNNils(numTemps);

  return(false);
}

/*
 *	static void returnWithValue(returnedValue, returnContext)
 *
 * Description
 *
 *	Return from context "returnContext" with value "returnedValue".  Note
 *	that that context may not be the current context.  If returnContext
 *	is not a block context, then we need to carefully unwind the
 *	"method call stack".  Here carefully means that we examine each
 *	context.  If during its execution it did not create a block context,
 *	then we can deallocate it.  Otherwise, we need to mark it as returned
 *	and continue up the call chain until we reach returnContext.
 *
 * Inputs
 *
 *	returnedValue: 
 *		Value to be put on the stack in the sender's context.
 *	returnContext: 
 *		The context to return from, an OOP.  This may not be the
 *		current context.
 *
 */
static inline void
returnWithValue(returnedValue, returnContext)
     OOP	returnedValue, returnContext;
{
  BlockContext 	oldContext;
  MethodContext methodContext;
  OOP		oldContextOOP, methodContextOOP;

  do {
#ifdef PROFBLOCK
    ps.stackDepth--;
#endif
    oldContextOOP = thisContextOOP;
    oldContext = (BlockContext)oopToObj(oldContextOOP);
    thisContextOOP = oldContext->caller; /* or sender, it is the same */
    if (oldContext->hasBlock == falseOOP) {
      if (isFake(oldContextOOP)) {
        deallocFakeContext((MethodContext)oldContext);
        freeOOP(oldContextOOP);
      }
    } else {
      /* This context created a block.  If it is a block, no problem: the
         method context in which it lived is probably still alive.  If it
         is a method, however, we must keep the method context around so
         that the blocks it created can reference arguments and temporaries
	 of this method, but mark it as non-returnable so that attempts to
	 return from it to an undefined place will lose.
	 We also mark the parent context so that it won't be GCed.
	 Since we don't know who is holding the block, we must presume that
	 it is global, so we always mark it */
      ((BlockContext)oopToObj(thisContextOOP))->hasBlock = trueOOP;
      if (isBlockContext(oldContextOOP)) {
        ((BlockContext)oopToObj(oldContext->outerContext))->hasBlock = trueOOP;
      } else {
        oldContext->caller = nilOOP;
      }
    }
    /* When oldContextOOP == returnContext, thisContextOOP is returnContext's
       parent context. */
  } while (oldContextOOP != returnContext);

  oldContext = (BlockContext)oopToObj(thisContextOOP);

  methodContextOOP = getMethodContext(thisContextOOP);
  methodContext = (MethodContext)oopToObj(methodContextOOP);
  thisMethod = methodContext->method;

  ip = toInt(oldContext->ipOffset) + getMethodByteCodes(thisMethod);
  sp = oldContext->contextStack + toInt(oldContext->spOffset);
  temporaries = oldContext->contextStack;

  /* self lives in the method, not in the block */
  self = methodContext->receiver;

  setStackTop(returnedValue);
}



/***********************************************************************
 *
 *	Simple Method Object Accessors
 *
 ***********************************************************************/

#ifdef ACCESSOR_DEBUGGING

static OOP
receiverVariable(receiver, index)
     OOP	receiver;
     int	index;
{
  if (!inBounds(receiver, index)) {
    errorf("Index out of bounds %d", index);
    debug();
  }
  return (oopToObj(receiver)->data[index]);
}

static OOP
getStackReceiver(numArgs)
     int	numArgs;
{
  /* this is correct: numArgs == 0 means that there's just the receiver
     on the stack, at 0.  numArgs = 1 means that at location 0 is the arg,
     location 1 is the receiver. */
  return (stackAt(numArgs));
}

static OOP
methodTemporary(index)
     int	index;
{
  return (temporaries[index]);
}

static OOP
methodLiteral(methodOOP, index)
     OOP	methodOOP;
     int	index;
{
  Method	method = (Method)oopToObj(methodOOP);

  /* ### check for in bounds with index */
  return (literals[index]);
}

static OOP
methodVariable(methodOOP, index)
     OOP	methodOOP;
     int	index;
{
  Method	method = (Method)oopToObj(methodOOP);

  return (associationValue(literals[index]));
}

static Byte *
getMethodByteCodes(methodOOP)
     OOP	methodOOP;
{
  Method	method;

  if (isNil(methodOOP)) {
    return (nil);
  }

  method = (Method)oopToObj(methodOOP);

  /* skip the header and the number of literals to find the start of the
     byte codes */
  return (method->bytecodes);
}

static MethodHeader
getMethodHeader(methodOOP)
     OOP	methodOOP;
{
  Method	method;

  method = (Method)oopToObj(methodOOP);
  return (method->header);
}

/*
 *	static OOP getMethodClass(contextOOP)
 *
 * Description
 *
 *	This is called when a method contains a send to "super".  The context
 *	object for method calls contains the class in which the method was
 *	found, and this routine returns it by looking in the method context.
 *
 * Inputs
 *
 *	method: An OOP that represents a method.
 *
 * Outputs
 *
 *	An OOP for the class of the method.
 */
static OOP
getMethodClass(contextOOP)
     OOP	contextOOP;
{
  MethodContext	methodContext;
  OOP		associationOOP;

  methodContext = (MethodContext)oopToObj(getMethodContext(contextOOP));
  return (methodContext->methodClass);
}

/***********************************************************************
 *
 *	Simple Method Object Storing routines.
 *
 ***********************************************************************/


static void
storeReceiverVariable(receiver, index, oop)
     OOP	receiver, oop;
     int	index;
{
  if (!inBounds(receiver, index)) {
    errorf("Index out of bounds %d", index);
    debug();
  }
  oopToObj(receiver)->data[index] = oop;
}

static void
storeMethodTemporary(index, oop)
     int	index;
     OOP	oop;
{
  temporaries[index] = oop;
}

static void
storeMethodVariable(methodOOP, index, oop)
     OOP	methodOOP, oop;
     int	index;
{
  Method	method = (Method)oopToObj(methodOOP);

  setAssociationValue(literals[index], oop);
}

static void
storeMethodLiteral(methodOOP, index, oop)
     OOP	methodOOP, oop;
     int	index;
{
  Method	method = (Method)oopToObj(methodOOP);

  literals[index] = oop;
}

static
mst_Boolean inBounds(oop, index)
     OOP	oop;
     int	index;
{
  mst_Object	obj = oopToObj(oop);

  return (index >= 0 && index < numOOPs(obj));
}
#endif /* ACCESSOR_DEBUGGING */



static inline OOP
getActiveProcess()
{
  ProcessorScheduler processor;

  if (!isNil(switchToProcess)) {
    return (switchToProcess);
  } else {
    processor = (ProcessorScheduler)oopToObj(processorOOP);
    return (processor->activeProcess);
  }
}

static void
addFirstLink(semaphoreOOP, processOOP)
     OOP	semaphoreOOP, processOOP;
{
  Semaphore	sem;
  Process	process, lastProcess;
  OOP		lastProcessOOP;

  process = (Process)oopToObj(processOOP);
  if (!isNil(process->myList)) {
    sem = (Semaphore)oopToObj(process->myList);
    if (sem->firstLink == processOOP) {
      sem->firstLink = process->nextLink;
      if (sem->lastLink == processOOP) {
        /* It was the only process in the list */
        sem->lastLink = nilOOP;
      }
    } else if (sem->lastLink == processOOP) {
      /* Find the new last link */
      lastProcessOOP = sem->firstLink;
      lastProcess = (Process) oopToObj(lastProcessOOP);
      while(lastProcess->nextLink != processOOP) {
	lastProcessOOP = lastProcess->nextLink;
	lastProcess = (Process) oopToObj(lastProcessOOP);
      }
      sem->lastLink = lastProcessOOP;
      lastProcess->nextLink = nilOOP;
    }
  }

  sem = (Semaphore)oopToObj(semaphoreOOP);
  process->myList = semaphoreOOP;
  process->nextLink = sem->firstLink;

  sem->firstLink = processOOP;
  if (isNil(sem->lastLink)) {
    sem->lastLink = processOOP;
  }
}

static void
addLastLink(semaphoreOOP, processOOP)
     OOP	semaphoreOOP, processOOP;
{
  Semaphore	sem;
  Process	process, lastProcess;
  OOP		lastProcessOOP;

  process = (Process)oopToObj(processOOP);
  if (!isNil(process->myList)) {
    sem = (Semaphore)oopToObj(process->myList);
    if (sem->firstLink == processOOP) {
      sem->firstLink = process->nextLink;
      if (sem->lastLink == processOOP) {
        /* It was the only process in the list */
        sem->lastLink = nilOOP;
      }
    } else if (sem->lastLink == processOOP) {
      /* Find the new last link */
      lastProcessOOP = sem->firstLink;
      lastProcess = (Process) oopToObj(lastProcessOOP);
      while(lastProcess->nextLink != processOOP) {
	lastProcessOOP = lastProcess->nextLink;
	lastProcess = (Process) oopToObj(lastProcessOOP);
      }
      sem->lastLink = lastProcessOOP;
      lastProcess->nextLink = nilOOP;
    }
  }

  sem = (Semaphore)oopToObj(semaphoreOOP);
  process->myList = semaphoreOOP;
  process->nextLink = nilOOP;

  if (isNil(sem->lastLink)) {
    sem->firstLink = sem->lastLink = processOOP;
  } else {
    lastProcessOOP = sem->lastLink;
    lastProcess = (Process)oopToObj(lastProcessOOP);
    lastProcess->nextLink = processOOP;
    sem->lastLink = processOOP;
  }
}

static inline mst_Boolean
isEmpty(processListOOP)
     OOP	processListOOP;
{
  Semaphore	processList;

  processList = (Semaphore)oopToObj(processListOOP);
  return (isNil(processList->firstLink));
}


static void
syncSignal(semaphoreOOP)
     OOP	semaphoreOOP;
{
  Semaphore sem;
  OOP	    freedOOP;

  sem = (Semaphore)oopToObj(semaphoreOOP);
  do {
    if (isEmpty(semaphoreOOP)) {
      sem->signals = incrInt(sem->signals);
      break;
    }
    freedOOP = removeFirstLink(semaphoreOOP);

    /* If they terminated this process, well, try another */
  } while (!resumeProcess(freedOOP));
}

void
asyncSignal(semaphoreOOP)
     OOP semaphoreOOP;
{
  IntState oldSigMask;
  
  oldSigMask = disableInterrupts(); /* block out everything! */
  queuedAsyncSignals[asyncQueueIndex++] = semaphoreOOP;
  setExceptFlag(true);
  enableInterrupts(oldSigMask);
}


static OOP
removeFirstLink(semaphoreOOP)
     OOP	semaphoreOOP;
{
  Semaphore	sem;
  Process	process;
  OOP		processOOP;

  sem = (Semaphore)oopToObj(semaphoreOOP);
  processOOP = sem->firstLink;
  process = (Process)oopToObj(processOOP);

  sem = (Semaphore)oopToObj(semaphoreOOP);
  sem->firstLink = process->nextLink;
  if (isNil(sem->firstLink)) {
    sem->lastLink = nilOOP;
  }

  /* Unlink the process from any list it was in! */
  process->myList = nilOOP;
  process->nextLink = nilOOP;
  return (processOOP);
}

static
mst_Boolean resumeProcess(processOOP)
     OOP	processOOP;
{
  int		priority;
  OOP		activeOOP;
  OOP		processLists;
  OOP		processList;
  Process	process, active;

  activeOOP = getActiveProcess();
  active = (Process)oopToObj(activeOOP);
  process = (Process)oopToObj(processOOP);

  if (process == active) {
    return(true);
  }
  if (isProcessTerminating(processOOP)) {
    /* The process was terminated - nothing to resume, fail */
    return(false);
  }

  priority = toInt(process->priority);
  processLists = getProcessLists();
  processList = arrayAt(processLists, priority);

  if (priority >= toInt(active->priority)) {
    /*
     * we're resuming a process with a *equal or higher* priority, so sleep
     * the current one and activate the new one
     */
    sleepProcess(activeOOP);
    activateProcess(processOOP);
  } else {
    /* this process isn't higher than the active one, so the policy is that
     * it doesn't preempt the currently running one. Anyway, it will be the
     * first in its priority queue - so don't put it to sleep. */
    addFirstLink(processList, processOOP);
  }
  return(true);
}

static void
activateProcess(processOOP)
     OOP	processOOP;
{
  Process	process;
  int		priority;
  OOP		processLists;
  OOP		processList;

  if (processOOP == nilOOP) {
    return;
  }

  if (processOOP != getActiveProcess()) {
    process = (Process)oopToObj(processOOP);
    priority = toInt(process->priority);
    processLists = getProcessLists();
    processList = arrayAt(processLists, priority);
    addFirstLink(processList, processOOP);
  }

  setExceptFlag(true);
  switchToProcess = processOOP;
}

static mst_Boolean 
isProcessTerminating(processOOP)
     OOP	processOOP;
{
  Process	process;

  process = (Process)oopToObj(processOOP);
  return (isNil(process->suspendedContext));
}

static mst_Boolean
isProcessReady(processOOP)
     OOP	processOOP;
{
  Process	process;
  int		priority;
  OOP		processLists;
  OOP		processList;

  process = (Process)oopToObj(processOOP);
  priority = toInt(process->priority);
  processLists = getProcessLists();
  processList = arrayAt(processLists, priority);

  /* add process to end of priority queue */
  return (process->myList == processList);
}

static void
sleepProcess(processOOP)
     OOP	processOOP;
{
  Process	process;
  int		priority;
  OOP		processLists;
  OOP		processList;

  process = (Process)oopToObj(processOOP);
  priority = toInt(process->priority);
  processLists = getProcessLists();
  processList = arrayAt(processLists, priority);

  /* add process to end of priority queue */
  addLastLink(processList, processOOP);
}


/*
 *	static OOP highestPriorityProcess()
 *
 * Description
 *
 *	Locates and returns the highest priority process from the ??runlist??.
 *	(except the currently active one).  Removes it from the list.
 *
 * Outputs
 *
 *	The highest priority process, or nilOOP (after stopExecuting has been
 *	called).
 */
static OOP
highestPriorityProcess()
{
  OOP		processLists, processList;
  int		priority;
  OOP           processOOP;

  processLists = getProcessLists();
  priority = numOOPs(oopToObj(processLists));
  for (; priority > 0 ; priority--) {
    processList = arrayAt(processLists, priority);
    if (!isEmpty(processList)) {
      processOOP = removeFirstLink(processList);
      if (processOOP == getActiveProcess()) {
        /* The current process has yielded control, i.e. it has been
           moved to the end of its list - but if there's only one element
           it is still looks like the highest one, and we must discard it */
        /*printf("Current process discarded");*/
        addLastLink(processList, processOOP);
      } else {
        return(processOOP);
      }
    }
  }
  return (nilOOP);
}

static OOP
nextScheduledProcess()
{
  OOP			processLists, processList;
  OOP           	processOOP;
  ProcessorScheduler	processor;
  Process		process;
  MethodContext		dummyContext;
  OOP			dummyContextOOP;
  int			priority;

  processOOP = highestPriorityProcess();

  if(!isNil(processOOP)) {
    return(processOOP);
  }

  if(isProcessReady(getActiveProcess())) {
    return (nilOOP);
  }

  /* instead of returning nilOOP, let's return a newly created
     initial process and see what happens 10-Oct-93 14:17:48
     -- didn't work -- system hung

     pb -- Let's instead return the current process, modifying it so that
     it stops the Smalltalk interpreter. */

  /* printProcessState(); */
  /* initProcessSystem(); */		/* reset things */

  /* now make a dummy context to run with */
  dummyContextOOP = allocMethodContext(4);
  dummyContext = (MethodContext)oopToObj(dummyContextOOP);

  dummyContext->sender = nilOOP;
  dummyContext->method = nilOOP;
  dummyContext->methodClass = mst_objectClass; /* no real class */
  dummyContext->hasBlock = falseOOP;
  dummyContext->selector = nilOOP; /* no real selector invoked us */
  dummyContext->receiver = nilOOP; /* make self be real (well, nil) */
  dummyContext->ipOffset = fromInt(0);
  dummyContext->spOffset = fromInt(-1);

  processor = (ProcessorScheduler)oopToObj(processorOOP);
  process = (Process) oopToObj(processor->activeProcess);
  priority = toInt(process->priority);
  processLists = getProcessLists();
  processList = arrayAt(processLists, priority);

  process->suspendedContext = dummyContextOOP;
  process->myList           = processList;
  /* stopExecuting("noRunnableProcess"); */

  return (processor->activeProcess);
}

/* Mainly for being invoked from the the debugger */
void
printProcessState()
{
  OOP		processLists, processListOOP, processOOP;
  int		priority;
  Semaphore	processList;
  Process	process;

  processLists = getProcessLists();
  priority = numOOPs(oopToObj(processLists));
  for (; priority > 0 ; priority--) {
    printf("Priority %d: ", priority);
    processListOOP = arrayAt(processLists, priority);
    processList = (Semaphore)oopToObj(processListOOP);

    printf("First %p last %p ", processList->firstLink,
	   processList->lastLink);

    for (processOOP = processList->firstLink; !isNil(processOOP);
	 processOOP = process->nextLink) {
      process = (Process)oopToObj(processOOP);
      printf("<Proc %p prio: %ld next %p context %p> ",
	     processOOP, toInt(process->priority), process->nextLink,
	     process->suspendedContext);
    }


    printf("\n");
  }
}

static inline OOP
semaphoreNew()
{
  Semaphore	sem;

  sem = (Semaphore)instantiate(semaphoreClass);
  sem->signals = fromInt(0);

  return (allocOOP(sem));
}

/* runs before every evaluation (executeStatements) and before GC turned on.
   Note that we don't use the incubator because processorOOP is a global. */
void 
initProcessSystem()
{
  OOP		processLists;
  int		i;
  ProcessorScheduler processor;
  Process	initialProcess;
  OOP		initialProcessOOP, initialProcessListOOP;

  if (!processorOOP) {
    processor = (ProcessorScheduler)instantiate(processorSchedulerClass);
    processorOOP = allocOOP(processor);
    /* This is a global one - no need to incubate*/

    processLists = arrayNew(NUM_PRIORITIES);
    processor = (ProcessorScheduler)oopToObj(processorOOP);
    processor->processLists = processLists;
    /* processLists now is in the root set */

    for (i = 1; i <= NUM_PRIORITIES; i++) {
      arrayAtPut(processLists, i, semaphoreNew()); /* ### should be linked list */
    }
  } else {
    processor = (ProcessorScheduler)oopToObj(processorOOP);
    processLists = processor->processLists;
  }

  /* No process is active -- so highestPriorityProcess() need not worry
     about discarding an active process. */
  processor->activeProcess = nilOOP;

  initialProcessOOP = highestPriorityProcess();
  if (isNil(initialProcessOOP)) {
    initialProcess = (Process)instantiate(processClass);
    initialProcess->priority = fromInt(4); /* userSchedulingPriority */
    initialProcessOOP = allocOOP(initialProcess);
    /* initialProcessOOP now is in the root set */

    initialProcessListOOP = arrayAt(processLists, 4);
    addLastLink(initialProcessListOOP, initialProcessOOP);

    /* This is quite a problem. The initialProcess has undoubtedly a suspended
       context -- the #executeStatements context -- but it hasn't been created
       yet. But suspendedContext must not be nil, otherwise changeProcessContext
       will think that it belongs to a terminated process. No problem, we just
       set it to a bogus value.
       I chose this Integer because it is likely to cause a SIGSEGV if
       changeProcessContext behaves differently from what we think -- i.e.
       if it does something more interesting than comparing it to nil with
       the suspendedContext read from the suspended process. */

    initialProcess->suspendedContext = fromInt(0);
  }

  processor->activeProcess = initialProcessOOP;
  switchToProcess = nilOOP;
  timeoutSem = nilOOP;
}



/*
 *	static mst_Boolean *boolAddrIndex(index)
 *
 * Description
 *
 *	Used to help minimize the number of primitives used to control the
 *	various debugging flags, this routine maps an index to the address
 *	of a boolean debug flag, which it returns.
 *
 * Inputs
 *
 *	index : An integer (0 based) index to the set of debug variables
 *
 * Outputs
 *
 *	Address of the C debug variable, or NULL on failure.
 */
static inline mst_Boolean *
boolAddrIndex(index)
int	index;
{
  switch (index) {
  case 0: return (&declareTracing);
  case 1: return (&executionTracing);
  case 2: return (&verboseExecTracing);
  case 3: return (&gcMessage);
  case -1: return (&gcDebug);
  default: return (NULL);	/* index out of range, signal the error */
  }
}

void
setFileStreamFile(fileStreamOOP, fileOOP, fileNameOOP, isPipe)
OOP	fileStreamOOP, fileOOP, fileNameOOP;
mst_Boolean	isPipe;
{
  FileStream	fileStream;

  fileStream = (FileStream)oopToObj(fileStreamOOP);
  fileStream->file = fileOOP;
  fileStream->name = fileNameOOP;
  fileStream->isPipe = isPipe ? trueOOP : falseOOP;
}


void
initInterpreter()
{
  int		i;

  thisContextOOP = nilOOP;
  asyncQueueIndex = 0;

  for (i = 0; i < NUM_SIGNALS; i++) {
    semIntHappened[i] = false;
    semIntVec[i] = nilOOP;
  }
  initProcessSystem();
}

void
prepareExecutionEnvironment()
{
  MethodContext thisContext, newContext;
  OOP		newContextOOP;

#ifdef old_code
/**/  abortExecution = false;
#endif

  if (!isNil(thisContextOOP)) {
    thisContext = (MethodContext)oopToObj(thisContextOOP);
    /* save old context information */
    thisContext->ipOffset = fromInt(relativeByteIndex(ip, thisMethod));
    /* leave sp pointing to receiver, which is replaced on return with value*/
    thisContext->spOffset = fromInt(sp - thisContext->contextStack);
  }

  /* now make a dummy context to run with */
  /* the first +1 accounts for the receiver (which must be pushed on this
     context too); the second +1 accounts for any needed extra space, just to be
     sure */
  newContextOOP = allocMethodContext(((MAX_NUM_ARGS + 1) >> DEPTH_SCALE) + 1);
  newContext = (MethodContext)oopToObj(newContextOOP);

  newContext->sender = thisContextOOP;
  thisMethod = newContext->method = nilOOP;
  newContext->methodClass = mst_objectClass; /* no real class */
  newContext->hasBlock = falseOOP;
  newContext->selector = nilOOP; /* no real selector invoked us */
  newContext->receiver = nilOOP; /* make self be real (well, nil) */
  newContext->ipOffset = fromInt(0);
  newContext->spOffset = fromInt(-1);
  sp = newContext->contextStack - 1;
  ip = nil;

  temporaries = newContext->contextStack;
  literals = nil;
  self = nilOOP;

  thisContextOOP = newContextOOP;

  invalidateMethodCache();
}

OOP
finishExecutionEnvironment()
{
  MethodContext oldContext, thisContext;
  OOP		oldContextOOP, returnedValue;
  
  returnedValue = stackTop();
  oldContextOOP = thisContextOOP;
  oldContext = (MethodContext)oopToObj(oldContextOOP);
  thisContextOOP = oldContext->sender;

  if (oldContext->hasBlock == falseOOP) {
    if (isFake(oldContextOOP)) {
      deallocFakeContext(oldContext);
      freeOOP(oldContextOOP);
    }
  }

  if (!isNil(thisContextOOP)) {
    OOP methodContextOOP;
    thisContext = (MethodContext)oopToObj(thisContextOOP);
    methodContextOOP = getMethodContext(thisContextOOP);
    /* restore old context information */
    thisMethod = ((MethodContext)oopToObj(methodContextOOP))->method;
    temporaries = thisContext->contextStack;
    literals = getMethodLiterals(thisMethod);
    self = thisContext->receiver;
    ip = toInt(thisContext->ipOffset) + getMethodByteCodes(thisMethod);
    sp = thisContext->contextStack + toInt(thisContext->spOffset);
  }
  return (returnedValue);
}

void
invalidateMethodCache()
{
  int	i;

  cacheHits = cacheMisses = 0;

  for (i = 0; i < METHOD_CACHE_SIZE; i++) {
    methodCache[i].selectorOOP = nilOOP;
    methodCache[i].classOOP = nilOOP;
    methodCache[i].methodOOP = nilOOP;
    methodCache[i].methodClassOOP = nilOOP;
#ifdef COLLISION_CHECKING
    methodCache[i].collide = 0;
#endif
  }
}

#ifdef PROFBLOCK
initByteCodeCounter()
{
  int i;

  for (i = 0; i < 256; i++) {
    primitives[i] = byteCodes[i] = 0;
  }
  for (i = 0; i < 1024; i++) {
    primitives[i] = 0;
  }
}

printByteCodeCounts()
{
  int i;

  for (i = 0; i < 256; i++) {
    if (byteCodes[i]) {
      printf("Byte code %d = %d\n", i, byteCodes[i]);
    }
  }

  printf("\n---> primitives:\n");
  for (i = 0; i < 1024; i++) {
    if (primitives[i]) {
      printf("Primitive %d = %d\n", i, primitives[i]);
    }
  }

}
#endif


#ifdef ACCESSOR_DEBUGGING
static int
relativeByteIndex(bp, methodOOP)
Byte	*bp;
OOP	methodOOP;
{
  return (relativeByteIndexInternal(bp, methodOOP));
}
#endif /* ACCESSOR_DEBUGGING */



void
markProcessorRegisters()
{
  if (isNil(thisContextOOP)) {
    /* no processor registers to mark! */
    return;
  }

  markSemaphoreOOPs();
  maybeMarkOOP(thisContextOOP);

  /* marking getMethodContext(thisContextOOP) should not be necessary, */
  /* like marking self and thisMethod. thisContextOOP points at them */
  /* already, either directly or indirectly. */
  /* And processorOOP has already been marked by the global oop scan. */
#ifdef pointless
/**/    methodContextOOP = getMethodContext(thisContextOOP);
/**/    maybeMarkOOP(methodContextOOP);
/**/    maybeMarkOOP(self);
/**/    maybeMarkOOP(thisMethod);
/**/    maybeMarkOOP(processorOOP);
#endif

}

void
fixupObjectPointers()
{
  MethodContext	thisContext;

  if (!isNil(thisContextOOP)) {
    thisContext = (MethodContext)oopToObj(thisContextOOP);
#ifdef DEBUG_FIXUP
    fflush(stderr);
    printf("\nF sp %x %d    ip %x %d    thisMethod %x  thisContext %x",
	sp, sp - thisContext->contextStack,
	ip, relativeByteIndex(ip, thisMethod),
	thisMethod->object,
	thisContext);
    fflush(stdout);
#endif
    thisContext->spOffset = fromInt(sp - thisContext->contextStack);
    thisContext->ipOffset = fromInt(relativeByteIndex(ip, thisMethod));
  }
}

void
restoreObjectPointers()
{
  MethodContext thisContext;	/* may be block context, but doesn't matter */
  MethodContext	methodContext;
  OOP		methodContextOOP;

  /* !!! The objects can move after the growing or compact phase. But, all
     this information is re-computable, so we pick up thisMethod to adjust the
     ip and literals accordingly, and we also pick up the context to adjust sp
     and the temps accordingly. */

  if (!isNil(thisContextOOP)) {
    thisContext = (MethodContext)oopToObj(thisContextOOP);
    methodContextOOP = getMethodContext(thisContextOOP);
    methodContext = (MethodContext)oopToObj(methodContextOOP);

    temporaries = thisContext->contextStack;
    literals = getMethodLiterals(thisMethod);

#ifndef OPTIMIZE /* Mon Jul  3 01:21:06 1995 */
    /* these should not be necessary */
    if (thisMethod != methodContext->method) {
      printf("$$$$$$$$$$$$$$$$$$$ GOT ONE!!!!\n");
      printf("this method "); printObject(thisMethod); printf("\n");
      printf("method context "); printObject(methodContext->method); printf("\n");
      debug();
      thisMethod = methodContext->method;
    }
    if (self != methodContext->receiver) {
      printf("$$$$$$$$$$$$$$$$$$$ GOT ONE!!!!\n");
      printf("self "); printObject(self); printf("\n");
      printf("method context "); printObject(methodContext->receiver); printf("\n");
      debug();
      self = methodContext->receiver;
    }
#endif /* OPTIMIZE Mon Jul  3 01:21:06 1995 */

    ip = toInt(thisContext->ipOffset) + getMethodByteCodes(thisMethod);
    sp = toInt(thisContext->spOffset) + thisContext->contextStack;

#ifdef DEBUG_FIXUP
    fflush(stderr);
    printf("\nR sp %x %d    ip %x %d    thisMethod %x  thisContext %x\n",
	sp, sp - thisContext->contextStack,
	ip, relativeByteIndex(ip, thisMethod),
	thisMethod->object,
	thisContext);
    fflush(stdout);
#endif
  }
  setExceptFlag(true);            /* force to import registers */
}


/*
 *	static void markSemaphoreOOPs()
 *
 * Description
 *
 *	This routine is to be called during the root set copying part of a GC
 *	flip to copy any Smalltalk Semaphore related to asynchronous signals.
 *
 */
static void
markSemaphoreOOPs()
{
  int		i;
  IntState	oldSigMask;

  oldSigMask = disableInterrupts(); /* block out everything! */

  for (i = 0; i < asyncQueueIndex; i++) {
    maybeMarkOOP(queuedAsyncSignals[i]);
  }
  maybeMarkOOP(timeoutSem);

  /* there does seem to be a window where this is not valid */
  maybeMarkOOP(switchToProcess);

  for (i = 0; i < NUM_SIGNALS; i++) {
    maybeMarkOOP(semIntVec[i]);
  }
  enableInterrupts(oldSigMask);
}



/*
 *	void initSignals()
 *
 * Description
 *
 *	Trap the signals that we care about, basically SIGBUS and SIGSEGV.
 *	These are sent to the back trace routine so we can at least have some
 *	idea of where we were when we died.
 *
 */
void
initSignals()
{
  if (!makeCoreFile) {
#ifdef SIGBUS
    setSignalHandler(SIGBUS, interruptHandler);
#endif
    setSignalHandler(SIGSEGV, interruptHandler);
  }
  setSignalHandler(SIGINT, interruptHandler);
}


/*
 *	static void stopExecuting(msg)
 *
 * Description
 *
 *	Sets flags so that the interpreter starts returning immediately from
 *	whatever byte codes it's executing.  It returns via a normal message
 *	send, so that the world is in a consistent state when it's done.
 *
 * Inputs
 *
 *	msg   : error message to be passed to Smalltalk.
 *
 */

static void
stopExecuting(msg)
char *msg;
{
  abortExecution = msg;
  setExceptFlag(true);
  if (inCCode) {
#ifdef STACK_JMPBUFS
    longjmp(cCalloutJmpBuf->jmpBuf, 1);	/* throw out from C code */
#else
    longjmp(cCalloutJmpBuf, 1);	/* throw out from C code */
#endif
  }
}

static RETSIGTYPE
timeoutHandler(sig)
int sig;
{
  asyncSignal(timeoutSem);
  timeoutSem = nilOOP;
}

static RETSIGTYPE 
semIntHandler(sig)
int sig;
{
  if (!isNil(semIntVec[sig])) {
    if (isClass(semIntVec[sig], semaphoreClass)) {
      semIntHappened[sig] = true;
      asyncSignal(semIntVec[sig]);
    } else {
      errorf("C signal trapped, but no semaphore was waiting");
    }
  }
}


/*
 *	static RETSIGTYPE interruptHandler(sig)
 *
 * Description
 *
 *	Called to handle signals, such as interrupts or segmentation violation.
 *	In the latter case, try to show a method invocation backtrace if possibly,
 *	otherwise try to show where the system was in the file it was procesing when
 *	the error occurred.
 *
 * Inputs
 *
 *	sig   : Signal number, an integer
 *
 * Outputs
 *
 *	not used.
 */
static RETSIGTYPE
interruptHandler(sig)
int	sig;
{
  switch (sig) {
  case SIGINT:
    if (nonInteractive) {
      printf("Signal %d, exiting...\n", sig);
      showBacktrace();
      exit(1);
    } else {
      stopExecuting("userInterrupt");
      setSignalHandler(SIGINT, interruptHandler);
      return;
    }

#ifdef SIGBUS
  case SIGBUS:
    errorf("Bus Error");
    break;
#endif

  case SIGSEGV:
    errorf("Segmentation violation");
    break;
    
  default:
    errorf("Unknown signal caught: %d", sig);
  }

  if (inInterpreter) {
    /* Avoid recursive signals */
    inInterpreter = false;
    showBacktrace();
  } else {
    errorf("Error occurred while not in byte code interpreter!!");
  }

  debug();
  exit(1);
}

static void
showBacktrace()
{
  OOP		context, receiver, receiverClass;
  MethodContext methodContext, nextContext;
  BlockContext	blockContext;

/* printf("In showbacktrace\n"); */
  for (context = thisContextOOP; !isNil(context);
       context = nextContext->sender) {
    if (isBlockContext(context)) {
      /* a block context */
      blockContext = (BlockContext)oopToObj(context);
      methodContext = (MethodContext)oopToObj(blockContext->home);
      printf("[] in ");
      printObject(methodContext->methodClass);
      nextContext = (MethodContext)blockContext;
    } else {
      /* a method context */
      methodContext = (MethodContext)oopToObj(context);

      receiver = methodContext->receiver;
      if (isInt(receiver)) {
	receiverClass = integerClass;
      } else {
	receiverClass = oopClass(receiver);
      }
      printObject(receiverClass);
      if (receiverClass != methodContext->methodClass) {
	printf("(");
	printObject(methodContext->methodClass);
	printf(")");
      }
      nextContext = methodContext;
    }
    printf(">>");
    printObject(methodContext->selector);
    printf("\n");
  }

}
