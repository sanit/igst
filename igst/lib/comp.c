/***********************************************************************
 *
 *	Byte code compiler.
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
#include "gstpub.h"
#include "alloc.h"
#include "sym.h"
#include "comp.h"
#include "tree.h"
#include "byte.h"
#include "dict.h"
#include "oop.h"
#include "interp.h"
#include "lex.h"
#include "lib.h"
#include "sysdep.h"
#include <setjmp.h>
#ifdef sun
#include <sys/time.h>
#include <sys/resource.h>
#endif
#include <stdio.h>		/* for printf */
#if STDC_HEADERS
#include <string.h>
#include <stdlib.h>
#endif /* STDC_HEADERS */

/* Define this if you want declaration tracing to print the bytecodes
 * both *before* and *after* the optimizer is ran.  Default behavior
 * is to print the bytecodes only after the optimization pass.
 */
#undef PRINT_BEFORE_OPTIMIZATION


#define LITERAL_VEC_CHUNK_SIZE		32

unsigned long		numThisContexts, primitivesExecuted, messagesSent;
unsigned long		literalReturns, instVarReturns, selfReturns;



typedef enum {
  falseJump,
  trueJump,
  unconditionalJump
} JumpType;

typedef enum {
  methodContext,
  blockContext
} ContextType;

typedef struct MethodInfoStruct {
  OBJ_HEADER;
  OOP		sourceCode;
  OOP		category;
} *MethodInfo;

typedef struct FileSegmentStruct {
  OBJ_HEADER;
  OOP		fileName;
  OOP		startPos;
  OOP		length;
} *FileSegment;


/* These hold the compiler's notions of the current class for compilations,
 * and the current category that compiled methods are to be placed into */
OOP			thisClass, thisCategory;

/* This holds the CompiledMethod oop for the most recently compiled method.
 * It is only really valid after a compile: has been done, but this is 
 * the only place that its used. */
OOP			latestCompiledMethod;

/* These flags control whether byte codes are printed after compilation,
 * and whether regression testing is in effect (which causes any messages
 * that the system prints out to become constant messages, i.e. no timing
 * information is printed) */
mst_Boolean		declareTracing;

/* If true, the normal execution information is supressed, and the prompt
 * is emitted with a special marker character ahead of it to let the process
 * filter know that the execution has completed. */
mst_Boolean	 	emacsProcess = false;

/* If true, the compilation of a set of methods will be skipped completely;
 * only syntax will be checked.  Set by primitive, cleared by grammar.
 */
mst_Boolean		skipCompilation = false;

/* This is the value most recently returned by executeStatements.  It is
 * used to communicate the returned value past a parseStream call, without
 * pushing something on the called context stack in the case of nested
 * invocations of prepareExecutionEnvironment/finishExecutionEnvironment.
 * Most often, the caller does not care about the returned value, since it 
 * often is called from a radically different context.
 */
OOP			lastReturnedValue;



static mst_Boolean	isSuper(), equalConstant(), compileTimesRepeat(),
			compileWhileLoop(), compileIfStatement(),
			compileToByDo(), compileAndOrStatement(),
			compileIfTrueFalseStatement();
static OOP		computeSelector(), makeConstantOOP(),
			getLiteralsArray(), methodNew(),
			methodInfoNew(), fileSegmentNew();
static ByteCodes	compileSubExpression(), 
			compileSubExpressionWithGoto(),
			compileDefaultValue();
static int		isSpecialVariable(),
			addConstant(), addSelector(), whichBuiltinSelector(),
			addForcedSelector(), listLength(), addLiteral();
static Method		simpleMethodNew();
static void 		compileStatement(), compileExpression(),
			compileSimpleExpression(), compileVariable(),
			compileConstant(), compileBlock(),
			compileStatements(), compileUnaryExpr(),
			compileBinaryExpr(), compileKeywordExpr(),
			compileSend(), compileCascadedMessage(),
			compileAssignments(), /* addMethodClassVariable(), */
			compileJump(), compileKeywordList(),
			reallocLiteralVec(),
			installMethod(), initCompiler(),
			compilePushIntConstant(), compileLongJump();

/* Used to abort really losing compiles, jumps back to the top level of the
 * compiler */
static jmp_buf		badMethod;

/* The vector of literals that the compiler uses to accumulate literal
 * constants into */
static OOP		*literalVec = nil;

/* These indicate the current number of literals in the method being compiled,
 * and the current maximum allocated size of the literal vector */
static int		numLiterals, literalVecMax;

/* These indicate the current stack depth in the method/block being compiled,
 * and the maximum reached stackDepth */
static int		stackDepth, maxStackDepth;


/* Helps ensure that storage doesn't get left behind by the GC */
static OOP		methodDictionaryOOP = nil;

/* HACK ALERT!! HACK ALERT!!  This variable is used for cascading.  The
 * tree structure is all wrong for the code in cascade processing to find
 * the receiver of the initial message.  What this does is when it's true,
 * compileUnaryExpr, compileBinaryExpr, and compileKeywordExpr record
 * its value, and clear the global (to prevent propagation to compilation
 * of subnodes).  After compiling their receiver, if the saved value of
 * the flag is true, they emit a dupStackTop, and continue compilation.
 * Since cascaded sends are relatively rare, I figured that this was a better
 * alternative than passing useless parameters around all the time.
 */
static mst_Boolean		dupMessageReceiver = false;



#define incrStackDepth()	{					\
	if (++stackDepth > maxStackDepth) maxStackDepth++;		\
}
#define addStackDepth(n)	{					\
	stackDepth += (n);						\
	if (stackDepth > maxStackDepth) maxStackDepth = stackDepth;	\
}
#define subStackDepth(n)	stackDepth -= (n)

#define exitCompilation()						\
	longjmp(badMethod, 1);

#define computeLocationIndex(variable) \
  ((variable->scope == receiverScope) ? receiverLocation : \
    ((variable->scope == temporaryScope) ? temporaryLocation : litVarLocation))

#define hasParamsOrTemps(blockNode) \
  (blockNode->vBlock.temporaries || blockNode->vBlock.arguments)

/* for to:do: */
#define hasNotOneParamOrTemps(blockNode)		\
  (blockNode->vBlock.temporaries			\
   || !blockNode->vBlock.arguments			\
   || blockNode->vBlock.arguments->vList.next)



/*
 *	void installInitialMethods()
 *
 * Description
 *
 *	This routine does a very interesting thing.  It installs the inital
 *	method, which is the primitive for "methodsFor:".  It does this by
 *	creating a string that contains the method definition and then passing
 *	this to the parser as an expression to be parsed and compiled.  Once
 *	this has been installed, we can go ahead and begin loading the rest of
 *	the Smalltalk method definitions, but until the "methodsFor:" method is
 *	defined, we cannot begin to deal with
 *	"!Object methodsFor: 'primitives'!".
 *
 */
void
installInitialMethods ()
{
  char		*methodsForString;

  initDefaultCompilationEnvironment();

  methodsForString = "\
methodsFor: aCategoryString \
    <primitive: 150> \
";
  compileCode = true;		/* tell the lexer we do internal compiles */

  pushSmalltalkString(stringNew(methodsForString));
  parseStream();
  popStream(false);		/* can't close a string! */
}

/*
 *	void initDefaultCompilationEnvironment()
 *
 * Description
 *
 *	Does what it says.
 *
 */
void
initDefaultCompilationEnvironment ()
{
  setCompilationClass(behaviorClass);
  setCompilationCategory(nilOOP);
}

/*
 *	void invokeInitBlocks()
 *
 * Description
 *
 *	This function will send a message to Smalltalk (the system dictionary)
 *	asking it to invoke a set of initialization blocks.  There are methods
 *	in Smalltalk that allow for the recording of blocks to be invoked after
 *	image load, and this function sets that process in motion.
 *
 */
void invokeInitBlocks ()
{
  strMsgSend(smalltalkDictionary, "doInits", nil);
}

/*
 *	static void initCompiler()
 *
 * Description
 *
 *	Prepares the compiler for execution.
 *
 */
void
initCompiler ()
{
  /*
   * Prepare the literal vector for use.  The literal vector is where the
   * compiler will store any literals that are used by the method being
   * compiled.
   */
  literalVecMax = LITERAL_VEC_CHUNK_SIZE;
  literalVec = (OOP *)xmalloc(LITERAL_VEC_CHUNK_SIZE*sizeof(OOP));
}

/*
 *	void setCompilationClass(classOOP)
 *
 * Description
 *
 *	Sets the compiler's notion of the class to compile methods into.
 *
 * Inputs
 *
 *	classOOP: 
 *		An OOP for a Class object to compile method definitions into.
 *
 */
void
setCompilationClass (classOOP)
     OOP	classOOP;
{
  thisClass = classOOP;
}

/*
 *	void setCompilationCategory(categoryOOP)
 *
 * Description
 *
 *	Sets the compiler's notion of the current method category
 *
 * Inputs
 *
 *	categoryOOP: 
 *		An OOP that indicates the category to be used.  Typically a
 *		string.
 *
 */
void
setCompilationCategory (categoryOOP)
     OOP	categoryOOP;
{
  thisCategory = categoryOOP;
}



/*
 *	void displayCompilationTrace(string)
 *
 * Description
 *
 *	This function will print a message describing the method category
 *	and class being compiled.
 *
 * Inputs
 *
 *	string:
 *		a char * with the first part of the message
 */
void
displayCompilationTrace (string)
     char *string;
{
  if (declareTracing) {
    printf("%s category ", string);
    printObject(thisCategory);
    printf(" for ");
    printObject(thisClass);
    printf("\n");
  }
}


/*
 *	void markCompileContext()
 *
 * Description
 *
 *	Called only during a GC flip, this routine copies the current
 *	compilation context variables to new space, since theyre part of the
 *	"root set".
 *
 */
void
markCompileContext ()
{
  if (thisClass && !isNil(thisClass)) {
    maybeMarkOOP(thisClass);
  }

  if (thisCategory && !isNil(thisCategory)) {
    maybeMarkOOP(thisCategory);
  }

  if (literalVec) {		/* if non-NIL, we have literals needing 
				 * copying */
    markOOPRange(literalVec, literalVec + numLiterals);
  }

  if (latestCompiledMethod && !isNil(latestCompiledMethod)) {
    maybeMarkOOP(latestCompiledMethod);
  }

  if (methodDictionaryOOP && !isNil(methodDictionaryOOP)) {
    maybeMarkOOP(methodDictionaryOOP);
  }
}



/*
 *	void executeStatements(temporaries, statements, quiet)
 *
 * Description
 *
 *	Called to compile and execute an "immediate expression"; i.e. a set of
 *	Smalltalk statements that are not part of a method definition.
 *
 * Inputs
 *
 *	temporaries: 
 *		Syntax tree node that represents the temporary variables
 *		associated with the expression.
 *	statements: 
 *		The statements of the expression.  A syntax tree node.
 *	quiet : Flag to indicate either messages are to be output indicating
 *		the commencement of execution and some timing results at the
 *		end of execution.
 *
 */
void
executeStatements (temporaries, statements, quiet)
     TreeNode temporaries, statements;
mst_Boolean	quiet;
{
  TreeNode	messagePattern;
  unsigned long startTime, endTime, deltaTime;
#ifdef GET_RUSAGE_STATS
  struct rusage startRusage, endRusage;
#endif
  OOP		method;

  setCompilationClass(mst_objectClass);

  messagePattern = makeUnaryExpr(nil, "executeStatements");
  displayCompilationTrace("Compiling doit code in");

  method = compileMethod(makeMethod(messagePattern, temporaries, 0, statements), false);
  if (hadError) {		/* don't execute on error */
    return;
  }

  /* send a message to NIL, which will find this synthetic method definition
     in Object and execute it */
  prepareExecutionEnvironment();

  /* send message to NIL */
  pushOOP(nilOOP);
  quiet = true ; // sanit modified
  if (!quiet) {
    printf("\nExecution begins...\n");
  }
  sendMessage(internString("executeStatements"), 0, false);

  numThisContexts = byteCodeCounter = primitivesExecuted = messagesSent = 
  selfReturns = instVarReturns = literalReturns = 0;

  sampleCounter = 0;

  startTime = getMilliTime();
#ifdef GET_RUSAGE_STATS
  getrusage(RUSAGE_SELF, &startRusage);
#endif
  if (getMethodHeader(method).headerFlag == 0) {
    initProcessSystem();
    interpret();
  }
#ifdef GET_RUSAGE_STATS
  getrusage(RUSAGE_SELF, &endRusage);
#endif
  endTime = getMilliTime();
  lastReturnedValue = finishExecutionEnvironment();
  if (!quiet) {
    if (!regressionTesting) {
      printf("%lu byte codes executed\n", byteCodeCounter);
      deltaTime = endTime - startTime;
      deltaTime += (deltaTime == 0);	/* it could be zero which would core dump */
      printf("which took %.3f seconds, giving %lu bytecodes/sec\n",
	     deltaTime/1000.0, (unsigned long) (byteCodeCounter/ (deltaTime / 1000.0)));
#ifdef GET_RUSAGE_STATS
      deltaTime = ((endRusage.ru_utime.tv_sec * 1000) +
                  (endRusage.ru_utime.tv_usec / 1000)) -
		  ((startRusage.ru_utime.tv_sec * 1000) +
		  (startRusage.ru_utime.tv_usec / 1000));
      deltaTime += (deltaTime == 0);	/* it could be zero which would core dump */
      printf("(%.3f seconds user time, giving %lu bytecodes/sec, ",
	     deltaTime/1000.0, (int) (byteCodeCounter/ (deltaTime / 1000.0)));
      deltaTime = ((endRusage.ru_stime.tv_sec * 1000) +
                  (endRusage.ru_stime.tv_usec / 1000)) -
		  ((startRusage.ru_stime.tv_sec * 1000) +
		  (startRusage.ru_stime.tv_usec / 1000));
      printf("%.3f seconds system time)\n", deltaTime/1000.0);
      printf("(%d swaps, %d minor page faults, %d major page faults)\n",
             endRusage.ru_nswap - startRusage.ru_nswap,
             endRusage.ru_minflt - startRusage.ru_minflt,
             endRusage.ru_majflt - startRusage.ru_majflt);
      printf("(%d voluntary context switches, %d involuntary context switches)\n",
          endRusage.ru_nvcsw - startRusage.ru_nvcsw,
          endRusage.ru_nivcsw - startRusage.ru_nivcsw);
#endif
    }
    if (byteCodeCounter && !regressionTesting) {
      printf("%lu samples, percent %.2f\n", sampleCounter, 100.0*sampleCounter / byteCodeCounter);

      /* Sending executeStatements was surely a cache miss because the
         we had just invalidated the cache - but that doesn't count */
      cacheMisses--;
      printf("%lu cache hits, %lu misses", cacheHits, cacheMisses);
      if (cacheHits || cacheMisses) {
	printf(", %.2f percent hits\n", (100.0 * cacheHits) / (cacheHits + cacheMisses));
      } else {
        printf("\n");
      }

      printf("number of this contexts %lu\n", numThisContexts);
      printf("self returns %lu, inst var returns %lu, literal returns %lu\n", selfReturns, instVarReturns, literalReturns);
      printf("number of primitives %lu messages %lu", primitivesExecuted, messagesSent);
      if (messagesSent) {
	/* avoid division by zero */
	printf(", ratio %.3f\n", ((float)primitivesExecuted) / messagesSent);
      } else {
        printf("\n");
      }
    }

    printf("returned value is ");
    printObject(lastReturnedValue);
    printf("\n");
  }
}



/*
 *	OOP compileMethod(method)
 *
 * Description
 *
 *	Compile the code for a complete method definition.  Special cases for
 *	methods that don't return a value explicitly by returning "self".
 *	Actually creates the CompiledMethod object and installs it in the
 *	current method dictionary with the selector derived from the method
 *	expression.
 *
 * Inputs
 *
 *	method: A syntax tree node for a method definition.
 *
 * Outputs
 *
 *	the compiled method's OOP, or nil if the compilation failed.
 */
OOP
compileMethod (method)
     TreeNode method;
{
  TreeNode	statement;
  OOP		selector;
  ByteCodes	byteCodes;
  IncPtr	incPtr;

  dupMessageReceiver = false;
  latestCompiledMethod = nilOOP;

  incPtr = incSavePointer();

  initCompiler();

  pushNewScope();
  stackDepth = declareArguments(method->vMethod.selectorExpr);
  stackDepth += declareTemporaries(method->vMethod.temporaries);
  maxStackDepth = stackDepth;

  if (setjmp(badMethod) == 0) {
    for (statement = method->vMethod.statements; statement;
	 statement = statement->vExpr.expression) {
      compileStatement(statement->vExpr.receiver);
      if (statement->vExpr.receiver->nodeType != returnExprType) {
	/* ignore the result of the last statement if it's not used */
	subStackDepth(1);
	compileByte(popStackTop);

	if (statement->vExpr.expression == nil) {
	  /* compile a return of self.  Note that in theory the pop above is
	   * not necessary in this case (and in fact previous versions did
	   * not put it), but having it simplifies the optimizer's task because
	   * it reduces the number of patterns it has to look for.  If
	   * necessary, the optimizer itself will remove the pop. */
	  compileByte(returnIndexed | receiverIndex);
	}
      }
    }

    if (method->vMethod.statements == nil) {
      /* special case an empty statement body to return self */
      compileByte(returnIndexed | receiverIndex);
    }

    selector = computeSelector(method->vMethod.selectorExpr);
    incAddOOP(selector);
    byteCodes = getByteCodes();

    installMethod(selector, method->vMethod.primitiveIndex,
		  getArgCount(), getTempCount(), maxStackDepth,
		  byteCodes);
  } else {
    hadError = true;
    numLiterals = 0;
    byteCodes = getByteCodes();
    freeByteCodes(byteCodes);
  }

  popOldScope();
  freeTree(method);

  incRestorePointer(incPtr);
  return (latestCompiledMethod);
}

/*
 *	static void compileStatement(stmt)
 *
 * Description
 *
 *	Compiles a statement expression, including return expressions.
 *
 * Inputs
 *
 *	stmt  : A stmt tree node.
 *
 */
void
compileStatement (stmt)
     TreeNode stmt;
{
  int		index;

  switch (stmt->nodeType) {
  case constExprType:
  case blockNodeType:
    index = -1;
    break;

  default:
    index = isSpecialVariable(stmt->vExpr.receiver);
  }

  if (index < 0) {
    if (stmt->nodeType == returnExprType) {
      compileExpression(stmt->vExpr.receiver);
      compileByte(returnMethodStackTop);
    } else {
      compileExpression(stmt);
    }
  } else {
    if (stmt->nodeType == returnExprType) {
      /* return one of {self, true, false, nil} */
      compileByte(returnIndexed | index);
    } else {
      compileExpression(stmt);
    }
  }
}

/*
 *	static void compileExpression(expr)
 *
 * Description
 *
 *	Compile an arbitrary expression, including an assignment expression.
 *
 * Inputs
 *
 *	expr  : A syntax tree node for an expression, including assignments.
 *
 */
void
compileExpression (expr)
     TreeNode expr;
{
  if (expr->nodeType == assignExprType) {
    compileSimpleExpression(expr->vExpr.expression);
    compileAssignments(expr->vExpr.receiver);
  } else {
    compileSimpleExpression(expr);
  }
}

/*
 *	static void compileSimpleExpression(expr)
 *
 * Description
 *
 *	The basic expression compiler.  Can be called recursively.  Dispatches
 *	based on the type of the expression to different routines that
 *	specialize in compilations for that expression.
 *
 * Inputs
 *
 *	expr  : A syntax tree node for some kind of expression.
 *
 */
void
compileSimpleExpression (expr)
     TreeNode expr;
{
  switch (expr->nodeType) {
  case variableNodeType:
    compileVariable(expr);
    break;
  case constExprType:
    compileConstant(expr);
    break;
  case blockNodeType:
    compileBlock(expr);
    break;
  case unaryExprType:
    compileUnaryExpr(expr);
    break;
  case binaryExprType:
    compileBinaryExpr(expr);
    break;
  case keywordExprType:
    compileKeywordExpr(expr);
    break;
  case cascadedMessageNodeType:
    compileCascadedMessage(expr);
    break;
  default:
    compileExpression(expr);
  }
}

/*
 *	static void compileVariable(varName)
 *
 * Description
 *
 *	Compile code to push the value of a variable onto the stack.  The
 *	special variables, self, true, false, super, and thisContext, are
 *	handled specially.  For other variables, different code is emitted
 *	depending on where the variable lives, such as in a global variable or
 *	in a method temporary.
 *
 * Inputs
 *
 *	varName: 
 *		A syntax tree node that indicates a variable name.
 *
 */
void
compileVariable (varName)
     TreeNode varName;
{
  SymbolEntry	variable;
  int		index, location;

  incrStackDepth();
  index = isSpecialVariable(varName);
  if (index >= 0) {
    compileByte(pushSpecial | index);
    return;
  }

  /* interning just to compare with thisContext seems wrong -- we should just
   * compare the string, except that the rest of the symbol table already interns
   * the variables for "quicker checks" (yah, I guess). */
  if (internString(varName->vList.name) == thisContextSymbol) {
    compileByte(pushActiveContext);
    return;
  }

  variable = findVariable(varName->vList.name, false);
  if (variable == nil) {
    errorf("Undefined variable %s referenced", varName->vList.name);
    exitCompilation();
  }
  
  if (variable->scopeDistance != 0) {
    /* must be a temporary from an outer scope */
    compileByte(outerTempBytecode);
    compileByte(variable->varIndex | pushVariable);
    compileByte(variable->scopeDistance);
  } else if (variable->scope == temporaryScope
	     || variable->scope == receiverScope) {
    if (variable->varIndex <= 15) {
      if (variable->scope == temporaryScope) {
	compileByte(pushTemporaryVariable | variable->varIndex);
      } else {
	compileByte(pushReceiverVariable | variable->varIndex);
      }
    } else {
      compileByte(pushIndexed);
      location = (variable->scope == temporaryScope)
	? temporaryLocation : receiverLocation;
      compileByte(location | variable->varIndex);
    }
  } else {
    if (variable->varIndex <= 31) {
      compileByte(pushLitVariable | variable->varIndex);
    } else if (variable->varIndex <= 63) {
      compileByte(pushIndexed);
      compileByte(litVarLocation | variable->varIndex);
    } else {
      compileByte(bigLiteralsBytecode);
      compileByte(pushVariable | (variable->varIndex >> 8));
      compileByte(variable->varIndex & 255);
    }
  }
  freeSymbolEntry(variable);
}

/*
 *	static void compileConstant(constExpr)
 *
 * Description
 *
 *	Compile an expression that pushes a constant expression onto the stack.
 *	Special cases out the constants that the byte code interpreter knows
 *	about, which are the integers in the range -1 to 2.  Tries to emit the
 *	shortest possible byte sequence.
 *
 * Inputs
 *
 *	constExpr: 
 *		A syntax tree node that represents a literal constant.
 *
 */
void
compileConstant (constExpr)
     TreeNode constExpr;
{
  int		index;

  incrStackDepth();
  index = addConstant(constExpr);
  if (index < 0) {
    compileByte(pushSpecial | (index + 3 + 5));
  } else if (index <= 31) {
    compileByte(pushLitConstant | index);
  } else if (index <= 63) {
    compileByte(pushIndexed);
    compileByte(litConstLocation | index);
  } else {
      compileByte(bigLiteralsBytecode);
      compileByte(pushLiteral | (index >> 8));
      compileByte(index & 255);
  }
}

/*
 *	static void compileBlock(blockExpr)
 *
 * Description
 *
 *	Compile the expressions for a block.  Also, emits code to create the
 *	block, and then to skip around it.  The block will have its initial
 *	byte pointer pointing to two bytes past the long jump instruction, so
 *	that when the block is invoked it will start off at the first byte of
 *	the block.
 *	
 *
 * Inputs
 *
 *	blockExpr: 
 *		A syntax tree node for a block expression.
 *
 */
void
compileBlock (blockExpr)
     TreeNode blockExpr;
{
  ByteCodes	currentByteCodes, blockByteCodes;
  int		oldStackDepth;

  incrStackDepth();
  currentByteCodes = saveByteCodeArray(); /* ??? don't like this name */

  pushNewScope();
  oldStackDepth = stackDepth;
  stackDepth = declareBlockArguments(blockExpr->vBlock.arguments);
  stackDepth += declareTemporaries(blockExpr->vBlock.temporaries);
  addStackDepth(0);	/* update maxStackDepth */

  compileStatements(blockExpr->vBlock.statements, true);

  popOldScope();

  blockByteCodes = getByteCodes();
  restoreByteCodeArray(currentByteCodes);

  /* emit standard byte sequence to invoke a block:
   *   push current context
   *   push number of block args
   *   blockCopy: send
   *   long jump around block bytecodes
   *   <block byte codes>...
   */
  compileByte(pushActiveContext);
  compilePushIntConstant(listLength(blockExpr->vBlock.arguments));
  compilePushIntConstant(listLength(blockExpr->vBlock.temporaries));
  compileByte(blockCopyColonSpecial);
  compileByte(jumpLong | (byteCodeLength(blockByteCodes)/256 +4));
  compileByte(byteCodeLength(blockByteCodes) & 255);
  compileAndFreeByteCodes(blockByteCodes);
  stackDepth = oldStackDepth;
}


/*
 *	static void compileStatements(statementList, isBlock)
 *
 * Description
 *
 *	Compiles all of the statements in statement list.  Makes the final
 *	instruction of the block be a return top of stack, if the final
 *	statement isn't a return (^).
 *
 * Inputs
 *
 *	statementList: 
 *		A TreeNode of type ExprNode that is the list of statements in
 *		the block.  If it is nil, the block's return value is nil.
 *	isBlock:A boolean.  If true, these statements are from a block
 *		context.  If false, they are just an ordinary statement list.
 */
void
compileStatements (statementList, isBlock)
     TreeNode statementList;
     mst_Boolean	isBlock;
{
  TreeNode	stmt;

  if (statementList == nil) {
    if (isBlock) {
      compileByte(pushSpecial | nilIndex);
      compileByte(returnBlockStackTop);
    } else {
      incrStackDepth();
      compileByte(pushSpecial | nilIndex);
    }
    return;
  }
  
  for(stmt = statementList ; stmt; stmt = stmt->vExpr.expression) {
    compileStatement(stmt->vExpr.receiver);
    if (stmt->vExpr.expression == nil) {
      if (stmt->vExpr.receiver->nodeType != returnExprType) {
	/* if last statement isn't a return, then return the value on the
	   stack as the result.  For non-block contexts, returning the top
	   of the stack is the default, so it's ok.*/
	if (isBlock) {
	  compileByte(returnBlockStackTop);
	}
      }
    } else {
      /* throw away the value on the top of the stack...we don't need it
	 for all but the last one. */
      subStackDepth(1);
      compileByte(popStackTop);
    }
  }
}


/*
 *	static void compileUnaryExpr(expr)
 *
 * Description
 *
 *	Compile code to evaluate a unary expression.  Special cases sends to
 *	"super" and duplicates the receiver's value if this is part of a
 *	cascaded message send.
 *
 * Inputs
 *
 *	expr  : A syntax tree node for a unary expression.
 *
 */
void
compileUnaryExpr (expr)
     TreeNode expr;
{
  OOP		selector;
  int		selectorIndex;
  mst_Boolean	savedDupFlag;

  savedDupFlag = dupMessageReceiver;
  dupMessageReceiver = false;

  selector = expr->vExpr.selector;
  selectorIndex = addSelector(selector);

  if (expr->vExpr.receiver != nil) {
    compileExpression(expr->vExpr.receiver);
    if (savedDupFlag) {
      compileByte(dupStackTop);
    }
    if (isSuper(expr->vExpr.receiver)) {
      if (selectorIndex < 0) {
	selectorIndex = addForcedSelector(selector);
      }
      if (selectorIndex <= 31) {
	compileByte(sendSuper1ExtByte);
	compileByte(selectorIndex);
      } else {
	compileByte(sendSuper2ExtByte);
	compileByte(0);
	compileByte(selectorIndex);
      }
      return;
    }
  }
  
  if (isNil(selector)) {
    errorf("Nil selector in unary expression");
    exitCompilation();
  }

  if (selectorIndex < 0) {
    compileByte(-selectorIndex);
  } else {
    compileSend(selectorIndex, 0);
  }
}

/*
 *	static void compileBinaryExpr(expr)
 *
 * Description
 *
 *	Compiles code for a binary message.  Special cases sends to super, as
 *	they get different byte codes.  Also, checks to see if it's the first
 *	part of a cascaded message send and if so emits code to duplicate the
 *	stack top after the evaluation of the receiver for use by the
 *	subsequent cascaded expressions.
 *
 * Inputs
 *
 *	expr  : A syntax tree node for a binary expression.
 *
 */
void
compileBinaryExpr (expr)
     TreeNode expr;
{
  OOP		selector;
  int		selectorIndex;
  mst_Boolean	savedDupFlag;

  savedDupFlag = dupMessageReceiver;
  dupMessageReceiver = false;

  selector = expr->vExpr.selector;
  selectorIndex = addSelector(selector);

  if (expr->vExpr.receiver) {
    compileExpression(expr->vExpr.receiver);
    if (savedDupFlag) {
      compileByte(dupStackTop);
    }
  }
  if (expr->vExpr.expression) {
    compileExpression(expr->vExpr.expression);
  }
  subStackDepth(1);

  if (expr->vExpr.receiver) {
    if (isSuper(expr->vExpr.receiver)) {
      if (selectorIndex < 0) {
	selectorIndex = addForcedSelector(selector);
      }
      if (selectorIndex <= 31) {
	compileByte(sendSuper1ExtByte);
	compileByte((1 << 5) | selectorIndex);
      } else {
	compileByte(sendSuper2ExtByte);
	compileByte(1);
	compileByte(selectorIndex);
      }
      return;
    }
  }

  if (isNil(selector)) {
    errorf("nil selector in binary expression");
    exitCompilation();
  }

  if (selectorIndex < 0) {
    compileByte(-selectorIndex);
  } else {
    compileSend(selectorIndex, 1);
  }
}

/*
 *	static void compileKeywordExpr(expr)
 *
 * Description
 *
 *	Compile an keyword message send.  Special cases out while loops, the 4
 *	kinds of if tests, and the conditional "and" and conditional "or"
 *	messages.  If the expression isn't one of these, the expression is
 *	evaluated normally.  Has special hacks to support the duplication of
 *	the receiver's value in the case of the first part of a cascaded
 *	message send.
 *
 * Inputs
 *
 *	expr  : A syntax tree node that represents a keyword message send
 *		expression.
 *
 */
void
compileKeywordExpr (expr)
     TreeNode expr;
{
  OOP		selector;
  int		selectorIndex, numArgs;
  mst_Boolean	savedDupFlag;
  IncPtr	incPtr;

  savedDupFlag = dupMessageReceiver;
  dupMessageReceiver = false;

  incPtr = incSavePointer();
  selector = computeSelector(expr);
  incAddOOP(selector);

  /* check for optimized cases of messages to booleans and handle them
     specially */
  if (selector == whileTrueColonSymbol || selector == whileFalseColonSymbol) {
    if (compileWhileLoop(selector, expr)) {
      incRestorePointer(incPtr);
      return;
    }
  }

  if (expr->vExpr.receiver) {
    compileExpression(expr->vExpr.receiver);
    if (savedDupFlag) {
      compileByte(dupStackTop);
    }
  }

  if (selector == ifTrueColonSymbol || selector == ifFalseColonSymbol) {
    if (compileIfStatement(selector, expr->vExpr.expression)) {
      incRestorePointer(incPtr);
      return;
    }
  } else if (selector == ifTrueColonIfFalseColonSymbol
	     || selector == ifFalseColonIfTrueColonSymbol) {
    if (compileIfTrueFalseStatement(selector, expr->vExpr.expression)) {
      incRestorePointer(incPtr);
      return;
    }
  } else if (selector == andColonSymbol || selector == orColonSymbol) {
    if (compileAndOrStatement(selector, expr->vExpr.expression)) {
      incRestorePointer(incPtr);
      return;
    }
  } else if (selector == timesRepeatColonSymbol) {
    if (compileTimesRepeat(expr->vExpr.expression)) {
      incRestorePointer(incPtr);
      return;
    }
  } else if (selector == toColonDoColonSymbol) {
    if (compileToByDo(
      expr->vExpr.expression->vList.value, nil,			/* to:by: */
      expr->vExpr.expression->vList.next->vList.value)) {	/* do: */
      incRestorePointer(incPtr);
      return;
    }
  } else if (selector == toColonByColonDoColonSymbol) {
    if (compileToByDo(
      expr->vExpr.expression->vList.value,			      /* to: */
      expr->vExpr.expression->vList.next->vList.value,		      /* by: */
      expr->vExpr.expression->vList.next->vList.next->vList.value)) { /* do: */
      incRestorePointer(incPtr);
      return;
    }
  }

  selectorIndex = addSelector(selector);
  incRestorePointer(incPtr);

  numArgs = listLength(expr->vExpr.expression);

  compileKeywordList(expr->vExpr.expression);
  subStackDepth(numArgs);

  if (expr->vExpr.receiver) {
    if (isSuper(expr->vExpr.receiver)) {
      if (selectorIndex < 0) {
	selectorIndex = addForcedSelector(selector);
      }
      if (selectorIndex <= 31 && numArgs <= 7) {
	compileByte(sendSuper1ExtByte);
	compileByte((numArgs << 5) | selectorIndex);
      } else {
	compileByte(sendSuper2ExtByte);
	compileByte(numArgs);
	compileByte(selectorIndex);
      }
      return;
    }
  }

  if (selectorIndex < 0) {
    compileByte(-selectorIndex);
  } else {
    compileSend(selectorIndex, numArgs);
  }
}

/*
 *	static void compileKeywordList(list)
 *
 * Description
 *
 *	Emit code to evaluate each argument to a keyword message send.
 *
 * Inputs
 *
 *	list  : A list of expressions that represents the arguments to be
 *		evaluated.  A syntax tree node.
 *
 */
void
compileKeywordList (list)
     TreeNode list;
{
  for (; list; list = list->vList.next) {
    compileExpression(list->vList.value);
  }
}



/*
 *	static mst_Boolean compileWhileLoop(selector, expr)
 *
 * Description
 *
 *	Special case compilation of a #whileTrue: or #whileFalse: loop.
 *
 * Inputs
 *
 *	selector: 
 *		Symbol, one of #whileTrue: or #whileFalse:.
 *	expr  : An expression that represents the entire while loop.
 *
 * Outputs
 *
 *	True if byte codes were emitted, false if not.  If either the receiver
 *	and the argument to the while message are not block expressions, this
 *	routine cannot do it's job, and so returns false to indicate as much.
 */
mst_Boolean
compileWhileLoop (selector, expr)
     OOP	selector;
     TreeNode expr;
{
  int		whileLoopLen, startLoopLen;
  ByteCodes	receiverExprCodes, whileExprCodes;

  if (expr->vExpr.receiver->nodeType != blockNodeType
      || expr->vExpr.expression->vList.value->nodeType != blockNodeType) {
    return (false);
  }
  if (hasParamsOrTemps(expr->vExpr.receiver)
      || hasParamsOrTemps(expr->vExpr.expression->vList.value)) {
    return (false);
  }

  startLoopLen = currentByteCodeLength();

  receiverExprCodes = compileSubExpression(expr->vExpr.receiver);
  whileExprCodes = compileSubExpression(expr->vExpr.expression->vList.value);
  compileAndFreeByteCodes(receiverExprCodes);

  /* skip to the while loop if the receiver block yields the proper value */
  compileJump(2, (selector == whileTrueColonSymbol) ? trueJump : falseJump);

  /* otherwise, skip to the end, past the pop stack top and 2 byte goto
   * and exit the loop */
  subStackDepth(1);
  compileLongJump(byteCodeLength(whileExprCodes)+3);

  compileAndFreeByteCodes(whileExprCodes);
  subStackDepth(1);
  compileByte(popStackTop);	/* we don't care about while expr's value */

  /* +2 since we're using a 2 byte jump instruction here, so we have to
     skip back over it in addition to the other instructions */
  whileLoopLen = currentByteCodeLength() - startLoopLen +2;

  compileLongJump(-whileLoopLen);
  
  /* while loops always return nil (ain't expression languages grand?) -- 
     inefficient, but anyway the optimizer deletes this. */
  incrStackDepth();
  compileByte(pushSpecial | nilIndex);
  return (true);
}

/*
 *	static mst_Boolean compileTimesRepeat(expr)
 *
 * Description
 *
 *	Special case compilation of a #timesRepeat: loop.
 *
 * Inputs
 *
 *	selector: 
 *		Symbol, only #timesRepeat: for now.
 *	expr  : An expression that represents the entire loop code.
 *
 * Outputs
 *
 *	True if byte codes were emitted, false if not.  If the last argument
 *	to the message is not block expressions, this routine cannot do
 *	its job, and so returns false to indicate as much.
 */
mst_Boolean
compileTimesRepeat (expr)
     TreeNode expr;
{
  int		startLoopLen, wholeLoopLen;
  ByteCodes	loopExprCodes;

  if (expr->vList.value->nodeType != blockNodeType) {
    return (false);
  }
  if (hasParamsOrTemps(expr->vList.value)) {
    return (false);
  }

  /* save the receiver for the return value */
  compileByte(dupStackTop);				   incrStackDepth();

  startLoopLen  = currentByteCodeLength();
  loopExprCodes = compileSubExpression(expr->vList.value);

  compileByte(dupStackTop);				   incrStackDepth();
  compileByte(pushSpecial | litOneIndex);		   incrStackDepth();
  compileByte(greaterEqualSpecial);			   subStackDepth(1);

  /* skip the loop if there are no more occurrences */
  compileJump(byteCodeLength(loopExprCodes)+5, falseJump); subStackDepth(1);
  compileByte(pushSpecial | litOneIndex);		   incrStackDepth();
  compileByte(minusSpecial);				   subStackDepth(1);

  /* we don't care about while expr's value */
  compileAndFreeByteCodes(loopExprCodes);
  compileByte(popStackTop);				subStackDepth(1);

  /* +2 since we're using a 2 byte jump instruction here, so we have to
     skip back over it in addition to the other instructions */
  wholeLoopLen = currentByteCodeLength() - startLoopLen +2;

  compileLongJump(-wholeLoopLen);

  /* delete the 0 that remains on the stack */
  compileByte(popStackTop);				subStackDepth(1);
  return (true);
}

/*
 *	static mst_Boolean compileToByDo(to, block)
 *
 * Description
 *
 *	Special case compilation of a #to:do: and #to:by:do: loop.
 *	Note: the compilation of this structure is not all intuitive --
 *	sorry, but it's the best I could do...
 *
 * Inputs
 *
 *	to     :  Final value of the iteration
 *	by     :  Step value of the iteration, nil for #to:do:
 *	block  :  An expression that represents the entire loop code.
 *
 * Outputs
 *
 *	True if byte codes were emitted, false if not.  If the last argument
 *	to the message is not a block expression, this routine cannot do
 *	its job, and so returns false to indicate as much.
 */
mst_Boolean
compileToByDo (to, by, block)
     TreeNode to, by, block;
{
  int		loopLen, index, startLoopLen, wholeLoopLen;
  ByteCodes	loopExprCodes, stepCodes = nil;  /* initialize to please gcc */

  if (block->nodeType != blockNodeType) {
    return (false);
  }
  if (hasNotOneParamOrTemps(block)) {
    return (false);
  }
  if (by) {
    if (by->nodeType != constExprType || by->vConst.constType != intConst) {
      return (false);
    }
  }

  index = declareName(block->vBlock.arguments->vList.name, false);
  compileByte(storeIndexed);
  compileByte(index | temporaryLocation);

  compileExpression(to);
  compileByte(dupStackTop);				   incrStackDepth();
  if (index < 15) {
    compileByte(pushTemporaryVariable | index);		   incrStackDepth();
  } else {
    compileByte(pushIndexed);
    compileByte(index | temporaryLocation);		   incrStackDepth();
  }

  loopLen = index > 15 ? 9 : 8;

  if (by) {
    ByteCodes currentByteCodes;
    currentByteCodes = saveByteCodeArray(); /* ??? don't like this name */
    compileExpression(by);
    stepCodes = getByteCodes();
    restoreByteCodeArray(currentByteCodes);
    loopLen += byteCodeLength(stepCodes);
  } else {
    loopLen++;
  }

  startLoopLen  = currentByteCodeLength();
  loopExprCodes = compileSubExpression(block);
  loopLen	+= byteCodeLength(loopExprCodes);

  /* skip the loop if there are no more occurrences */
  compileByte((!by || by->vConst.val.iVal > 0)
      ? greaterEqualSpecial
      : lessEqualSpecial);			   subStackDepth(1);
  compileJump(loopLen, falseJump);      	   subStackDepth(1);

  /* we don't care about loop expr's value */
  compileAndFreeByteCodes(loopExprCodes);
  compileByte(popStackTop);				   subStackDepth(1);
  compileByte(dupStackTop);				   incrStackDepth();
  if (index > 15) {
    compileByte(pushIndexed);
    compileByte(index | temporaryLocation);		   incrStackDepth();
  } else {
    compileByte(pushTemporaryVariable | index);		   incrStackDepth();
  }

  if (by) {
    compileAndFreeByteCodes(stepCodes);
  } else {
    compileByte(pushSpecial | litOneIndex);		   incrStackDepth();
  }
  compileByte(plusSpecial);				   subStackDepth(1);
  compileByte(storeIndexed);
  compileByte(index | temporaryLocation);

  /* +2 since we're using a 2 byte jump instruction here, so we have to
     skip back over it in addition to the other instructions */
  wholeLoopLen = currentByteCodeLength() - startLoopLen +2;

  compileLongJump(-wholeLoopLen);

  /* delete the end from the stack */
  compileByte(popStackTop); 			subStackDepth(1);
  undeclareName();
  return (true);
}

/*
 *	static mst_Boolean compileIfTrueFalseStatement(selector, expr)
 *
 * Description
 *
 *	Special case compile of the code for #ifTrue:false: and #ifFalse:true:
 *	messages.
 *
 * Inputs
 *
 *	selector: 
 *		Symbol, one of #ifTrue:false: or #ifFalse:true:
 *	expr  : An tree node that represents the expressions for the first and
 *		second arguments to the message.  If either is not a block type
 *		expression, no byte codes are emitted, and this routine
 *		returns false.
 *
 * Outputs
 *
 *	True if the byte codes to perform the test were successfully emitted,
 *	false if not.
 */
mst_Boolean
compileIfTrueFalseStatement (selector, expr)
     OOP	selector;
     TreeNode expr;
{
  ByteCodes	trueByteCodes, falseByteCodes;

  if (expr->vList.value->nodeType != blockNodeType
      || expr->vList.next->vList.value->nodeType != blockNodeType) {
    return (false);
  }
  if (hasParamsOrTemps(expr->vList.value)
      || hasParamsOrTemps(expr->vList.next->vList.value)) {
    return (false);
  }

  if (selector == ifTrueColonIfFalseColonSymbol) {
    falseByteCodes = compileSubExpression(expr->vList.next->vList.value);
    trueByteCodes =
      compileSubExpressionWithGoto(expr->vList.value,
				   byteCodeLength(falseByteCodes));
  } else {
    falseByteCodes = compileSubExpression(expr->vList.value);
    trueByteCodes =
      compileSubExpressionWithGoto(expr->vList.next->vList.value,
				   byteCodeLength(falseByteCodes));
  }

  compileJump(byteCodeLength(trueByteCodes), falseJump);
  compileAndFreeByteCodes(trueByteCodes);
  compileAndFreeByteCodes(falseByteCodes);
  return (true);
}

/*
 *	static mst_Boolean compileIfStatement(selector, expr)
 *
 * Description
 *
 *	Special case compile of code for an #ifTrue: or #ifFalse: message.  The
 *	default value of an "if" type message is nil.
 *
 * Inputs
 *
 *	selector: 
 *		Symbol, one of #ifTrue: or #ifFalse:
 *	expr  : An expression to be evaluated if the given condition holds.
 *
 * Outputs
 *
 *	True if byte codes were emitted, false if not (as is the case if the
 *	expression following the selector is not a block).
 */
mst_Boolean
compileIfStatement (selector, expr)
     OOP	selector;
     TreeNode expr;
{
  ByteCodes	thenByteCodes;

  if (expr->vList.value->nodeType != blockNodeType
      || hasParamsOrTemps(expr->vList.value)) {
    return (false);
  }

  thenByteCodes = compileSubExpression(expr->vList.value);
  compileJump(byteCodeLength(thenByteCodes) + 1,
	      (selector == ifTrueColonSymbol) ? falseJump : trueJump);
  compileAndFreeByteCodes(thenByteCodes);

  /* Compare the code produced here with that produced in #and:/#or:
   * This produces less efficient bytecodes if the condition is true (there
   * are two jumps instead of one).  Actually, the push will 99% of the times
   * be followed by a pop stack top, and the optimizer changes
   *    0: jump to 2
   *    1: push nil
   *    2: pop stack top
   * to a single pop -- so the code ends up being is quite efficent. Note that
   * instead the result of #and:/#or: will be used (no pop) so we use the other
   * possible encoding. */
  compileByte(jumpShort);
  compileByte(pushSpecial | nilIndex);
  return (true);
}



/*
 *	static mst_Boolean compileAndOrStatement(selector, expr)
 *
 * Description
 *
 *	Special casing for and: an or: messages.  Emits code that jumps on the
 *	condition (true for and:, false for or:) to the actual expression for
 *	the block that's the argument to the message.  Then emits code that
 *	pushes the default value onto the stack, which will be only executed in
 *	the failure cases.  Then emits the code for the evaluation of the block
 *	that's the argument to the message.
 *
 * Inputs
 *
 *	selector: 
 *		A Symbol, either #and: or #or:.
 *	expr  : A syntax tree piece that represents the expressions contained
 *		in the block that's passed as an argument of the message.
 *
 * Outputs
 *
 *	Returns true if the code was successfully emitted, and false if the
 *	code was not (such as a non-block following the selector).
 */
mst_Boolean
compileAndOrStatement (selector, expr)
     OOP	selector;
     TreeNode expr;
{
  ByteCodes	blockByteCodes, defaultByteCodes;
  int		blockLen;
  
  if (expr->vList.value->nodeType != blockNodeType
      || hasParamsOrTemps(expr->vList.value)) {
    return (false);
  }

  blockByteCodes = compileSubExpression(expr->vList.value);
  blockLen = byteCodeLength(blockByteCodes);
  defaultByteCodes = compileDefaultValue((selector == andColonSymbol)
					 ? falseIndex : trueIndex, blockLen);
  compileJump(byteCodeLength(defaultByteCodes),
	      (selector == andColonSymbol) ? trueJump : falseJump);
  compileAndFreeByteCodes(defaultByteCodes);
  compileAndFreeByteCodes(blockByteCodes);
  return (true);
}



/*
 *	static ByteCodes compileDefaultValue(litIndex, realExprLen)
 *
 * Description
 *
 *	Compiles and returns a byte code sequence that represents the default
 *	value of a conditional expression, such as IfTrue: when the receiver is
 *	false.  The sequence causes the default value to be pushed on the
 *	stack, and then the byte codes for the non-default value to be jumped
 *	around.
 *
 * Inputs
 *
 *	litIndex: 
 *		The index for the value to push, typcially one of true, false,
 *		or nil.  Used as part of a pushSpecial byte code.
 *	realExprLen: 
 *		A C int that represents the length of the byte codes that are
 *		evaluated in the non-default case.
 *
 * Outputs
 *
 *	A sequence of byte codes pushes the default value and skips around the
 *	computation of the real value.
 */
ByteCodes
compileDefaultValue (litIndex, realExprLen)
     int	litIndex, realExprLen;
{
  ByteCodes	currentByteCodes, defaultByteCodes;

  currentByteCodes = saveByteCodeArray(); /* ??? don't like this name */

  compileByte(pushSpecial | litIndex);
  compileJump(realExprLen, unconditionalJump);

  defaultByteCodes = getByteCodes();
  restoreByteCodeArray(currentByteCodes);

  return (defaultByteCodes);
}

/*
 *	static ByteCodes compileSubExpression(expr)
 *
 * Description
 *
 *	Compile a "block" in a separate context and return the resulting
 *	bytecodes.  The block will not have argument declarations as it's only
 *	the code for things like ifTrue:, and:, whileTrue:, etc.  It is
 *	compiled as a list of statements such that the last statement leaves
 *	the value that is produced on the stack, as the value of the "block".
 *
 * Inputs
 *
 *	expr  : A "block" TreeNode that has no arguments. 
 *
 * Outputs
 *
 *	A ByteCodes vector of byte codes that represent the execution of the
 *	statements in the "block".
 */
ByteCodes
compileSubExpression (expr)
     TreeNode expr;
{
  return (compileSubExpressionWithGoto(expr, 0));
}


/*
 *	static ByteCodes compileSubExpressionWithGoto(expr, branchLen)
 *
 * Description
 *
 *	Like compileSubExpression, except that this sub expression always ends
 *	with an unconditional branch past "branchLen" bytecodes.
 *
 * Inputs
 *
 *	expr  : a TreeNode that looks like a "block".
 *	branchLen: 
 *		number of bytes to skip over, possibly zero (in which case no
 *		goto is generated).
 *
 * Outputs
 *
 *	ByteCodes that represent the execution of the statements in "expr",
 *	with a goto after them (if "branchLen" is nonzero).
 */
ByteCodes
compileSubExpressionWithGoto (expr, branchLen)
     TreeNode expr;
     int	branchLen;
{
  ByteCodes	currentByteCodes, subExprByteCodes;

  currentByteCodes = saveByteCodeArray(); /* ??? don't like this name */

  compileStatements(expr->vBlock.statements, false);
  if (branchLen) {
    compileJump(branchLen, unconditionalJump);
  }

  subExprByteCodes = getByteCodes();
  restoreByteCodeArray(currentByteCodes);

  return (subExprByteCodes);
}

/*
 *	static void compileJump(len, jumpType)
 *
 * Description
 *
 *	Compiles a jump instruction, using the smallest possible number of byte
 *	codes.  Special cases for the unconditional jump and the short false
 *	jump that the byte code interpreter handles.
 *
 * Inputs
 *
 *	len   : Number of byte codes to jump forward (only forward jumps are
 *		handled.  A C int > 0.
 *	jumpType: 
 *		An enumerated value that indicates whether the jump is
 *		unconditional, or a true or false jump.
 *
 */
void
compileJump (len, jumpType)
     int	len;
     JumpType jumpType;
{
  if (len <= 0) {
    errorf("Invalid length jump %d\n", len);
    exitCompilation();
  }

  switch (jumpType) {
  case unconditionalJump:
    if (len <= 8) {
      compileByte(jumpShort | (len - 1));
    } else {
      compileByte(jumpLong | (4 + len/256));
      compileByte(len & 255);
    }
    break;

  case falseJump:
    subStackDepth(1);
    if (len <= 8) {
      compileByte(popJumpFalseShort | (len - 1));
    } else {
      compileByte(popJumpFalse | (len/256));
      compileByte(len & 255);
    }
    break;

  case trueJump:
    subStackDepth(1);
    compileByte(popJumpTrue | (len/256));
    compileByte(len & 255);
    break;
  }
}

/*
 *	static void compileLongJump(len)
 *
 * Description
 *
 *	Compiles an unconditional long jump, forward or backward.
 *
 * Inputs
 *
 *	len   : Number of bytes to jump.  Can be negative.
 *
 */
void
compileLongJump (len)
     long	len;
{
  compileByte(jumpLong | (((len >> 8) + 4) & 7) );
  compileByte(len & 255);
}

/*
 *	compilePushIntConstant(intConst)
 *
 * Description
 *
 *	Compiles an instruction to push an Integer constant on the stack.
 *	Special cases out the literals -1..2, and tries to emit the shortest
 *	possible byte sequence to get the job done.
 *
 * Inputs
 *
 *	intConst: 
 *		The constant to be pushed; a C int.
 *
 */
void
compilePushIntConstant (intConst)
     int	intConst;
{
  TreeNode	constExpr;
  int		constIndex;

  if (intConst >= -1 && intConst <= 2) {
    compileByte(pushSpecial | (intConst + 5));
    return;
  }

  /* a hack to make use of the functionality provided by addConstant */
  constExpr = makeIntConstant((long)intConst);
  constIndex = addConstant(constExpr);
  freeTree(constExpr);
  
  if (constIndex <= 31) {
    compileByte(pushLitConstant | constIndex);
  } else {
    compileByte(pushIndexed | litConstLocation);
    compileByte(constIndex);
  }
}


/*
 *	static void compileSend(selectorIndex, numArgs)
 *
 * Description
 *
 *	Compile a message send byte code.  Tries to use the minimal length byte
 *	code sequence; does not know about the special messages that the
 *	interpreter has "wired in"; those should be handled specially and this
 *	routine should not be called with them (it's ok if it is, just not
 *	quite as efficient).
 *
 * Inputs
 *
 *	selectorIndex: 
 *		The index in the literal vector of the selector for the send
 *	numArgs: 
 *		The number of arguments that the selector takes.  A C integer.
 *
 */
void
compileSend (selectorIndex, numArgs)
     int	selectorIndex, numArgs;
{
  if (numArgs <= 2 && selectorIndex <= 15) {
    switch (numArgs) {
    case 0:
      compileByte(sendSelectorNoArg | selectorIndex);
      break;
    case 1:
      compileByte(sendSelector1Arg | selectorIndex);
      break;
    case 2:
      compileByte(sendSelector2Arg | selectorIndex);
      break;
    }
  } else if (selectorIndex <= 31 && numArgs <= 7) {
    compileByte(sendSelector1ExtByte);
    compileByte((numArgs << 5) | selectorIndex);
  } else if (selectorIndex <= 255) {
    compileByte(sendSelector2ExtByte);
    compileByte(numArgs);
    compileByte(selectorIndex);
  } else {
    errorf("Message send cannot be represented (yet)");
    exitCompilation();
  }    
}

/*
 *	static void compileCascadedMessage(cascadedExpr)
 *
 * Description
 *
 *	Compiles the code for a cascaded message send.  Due to the fact that
 *	cascaded sends go to the receiver of the last message before the first
 *	cascade "operator" (the ";"), the system to perform cascaded message
 *	sends is a bit kludgy.  We basically turn on a flag to the compiler
 *	that indicates that the value of the receiver of the last message
 *	before the cascaded sends is to be duplicated; and then compile code
 *	for each cascaded expression, throwing away the result, and duplicating
 *	the original receiver so that it can be used by the current message
 *	send, and following ones.
 *
 * Inputs
 *
 *	cascadedExpr: 
 *		A tree node that represents a cascaded expression.  Both the
 *		initial receiver and all the subsequent cascaded sends can be
 *		derived from this node.
 *
 */
void
compileCascadedMessage (cascadedExpr)
     TreeNode cascadedExpr;
{
  TreeNode message;

  dupMessageReceiver = true;
  compileExpression(cascadedExpr->vExpr.receiver);

  for(message = cascadedExpr->vExpr.expression; message;
      message = message->vList.next) {
    compileByte(popStackTop);
    if (message->vList.next) {
      compileByte(dupStackTop);
    } else {
      subStackDepth(1);
    }
    compileExpression(message->vList.value);
    /* !!! remember that unary, binary and keywordexpr should ignore the
       receiver field if it is nil; that is the case for these functions
       and things work out fine if that's the case. */
  }
}


/*
 *	static void compileAssignments(varList)
 *
 * Description
 *
 *	Compiles all the assignments in "varList", which is a TreeNode of type
 *	listNode.  The generated code assumes that the value on the top of the
 *	stack is what's to be used for the assignment.  Since this routine has
 *	no notion of now the value on top of the stack will be used by the
 *	calling environment, it makes sure that when the assignments are
 *	through, that the value on top of the stack after the assignment is the
 *	same as the value on top of the stack before the assignment.  The
 *	optimizer should fix this in the unnecessary cases. 
 *
 * Inputs
 *
 *	varList: 
 *	        TreeNode of type listNode that contains the names of the
 *		variables to be assigned into.
 *
 */
void
compileAssignments (varList)
     TreeNode varList;
{
  SymbolEntry	variable;
  int		locationIndex;

  for (; varList; varList = varList->vList.next) {
    variable = findVariable(varList->vList.name, true);
    if (!variable) {
      /* Might be an argument - if so, findVariable returned nil because
         its second argument was true.  Try with mustStore = false, just to
         discern between 'undeclared variable' and 'assignment to argument' */
      variable = findVariable(varList->vList.name, false);
      if (!variable) {
        errorf("assignment to undeclared variable %s", varList->vList.name);
      } else {
        errorf("invalid assignment to argument %s", varList->vList.name);
      }
      exitCompilation();
    }

    /* Here we have several kinds of things to store: receiver variable,
       temporary variable, "literal" variable (reference by association). */
       
    if (variable->scope == temporaryScope && variable->scopeDistance > 0) {
      /* a store into an outer temporary */
      compileByte(outerTempBytecode);
      compileByte(variable->varIndex | storeVariable);
      compileByte(variable->scopeDistance);
    } else if (variable->varIndex <= 63) {
      locationIndex = computeLocationIndex(variable);
      compileByte(storeIndexed);
      compileByte(locationIndex | variable->varIndex);
    } else {
      /* must be "literal" variable if > 63 */
      compileByte(bigLiteralsBytecode);
      compileByte(storeVariable | (variable->varIndex >> 8));
      compileByte(variable->varIndex & 255);
    }
    freeSymbolEntry(variable);
  }
}



/*
 *	static int isSpecialVariable(expr)
 *
 * Description
 *
 *	Examines the expression "expr" to see if it is a variable in the set
 *	"self", "false", "true", "nil".  Returns the "index" (number in 0..3)
 *	if true, -1 if false.  For this use, "super" is defined to be the
 *	equivalent of self.
 *
 * Inputs
 *
 *	expr  : TreeNode expression to be examined
 *
 * Outputs
 *
 *	-1 if expr is not a special variable
 *	0..3 if expr is "self", "true", "false", "nil", respectively.
 *	"super" is the same as self.
 */
int
isSpecialVariable (expr)
     TreeNode expr;
{
  OOP		variable;

  if (expr->nodeType != variableNodeType) {
    return (-1);
  }

  variable = internString(expr->vList.name);
  if (variable == selfSymbol || variable == superSymbol) {
    return (receiverIndex);
  } else if (variable == trueSymbol) {
    return (trueIndex);
  } else if (variable == falseSymbol) {
    return (falseIndex);
  } else if (variable == nilSymbol) {
    return (nilIndex);
  } else {
    return (-1);
  }
}


/*
 *	static mst_Boolean isSuper(expr)
 *
 * Description
 *
 *	Returns true if the expression passed to it represents the symbol
 *	"super"; false if not.
 *
 * Inputs
 *
 *	expr  : TreeNode that is an expression of some kind
 *
 * Outputs
 *
 *	true if "super" variable, false otherwise.
 */
mst_Boolean
isSuper (expr)
     TreeNode expr;
{
  if (expr->nodeType != variableNodeType) {
    return (false);
  }

  return (internString(expr->vList.name) == superSymbol);
}


/*
 *	static int addConstant(constExpr)
 *
 * Description
 *
 *	Scans the constants that are referenced by the current method.  If one
 *	is found that is equal to constExpr, the index of that constant is
 *	returned.  Otherwise, the constant is turned into a constant object,
 *	added to the constants of the method, and the new index is returned.
 *
 * Inputs
 *
 *	constExpr: 
 *		TreeNode of type constExprType, containing a literal constant
 *		of some kind.  Special cases -1, 0, 1, and 2 since they are
 *		available directly via instructions.
 *
 * Outputs
 *
 *	Index in the method's table of where this constant can be found,
 *	whether or not it was already there.  Returns a negative number if
 *	the constant is one of the above mentioned integers; in fact, the value
 *	returned is such that 3+returnValue is the numerical value.
 */
int
addConstant (constExpr)
     TreeNode constExpr;
{
  int		i;
  long		intVal;
  OOP		constantOOP;

  for (i = 0; i < numLiterals; i++) {
    if (equalConstant(literalVec[i], constExpr)) {
      return (i);
    }
  }

  constantOOP = makeConstantOOP(constExpr);
  if (isInt(constantOOP)) {
    intVal = toInt(constantOOP);
    if (intVal >= -1 && intVal <= 2) {
      return ((int)(intVal - 3));
    }
  } else {
    makeOOPReadOnly(constantOOP, true);
  }

  return (addLiteral(constantOOP));
}

/*
 *	static mst_Boolean equalConstant(oop, constExpr)
 *
 * Description
 *
 *	Returns true if "oop" and "constExpr" represent the same literal value.
 *	Primarily used by the compiler to store a single copy of duplicated
 *	literals in a method.  Can call itself in the case array literals.
 *
 * Inputs
 *
 *	oop   : An OOP that represents a constant value
 *	constExpr: 
 *		A piece of the syntax tree that represents a literal value.
 *
 * Outputs
 *
 *	True if "oop" and "constExpr" represent the same value; false
 *	otherwise. 
 */
mst_Boolean
equalConstant (oop, constExpr)
     OOP	oop;
     TreeNode constExpr;
{
  TreeNode	arrayElt;
  long		len, i;

  /* ??? this kind of special casing of the elements of arrays bothers
     me...it should all be in one neat place. */
  if (constExpr->nodeType == symbolNodeType) { /* symbol in array constant */
    return (oop == constExpr->vExpr.selector);
  } else if (constExpr->nodeType == arrayEltListType) {
    if (isOOP(oop) && oopToObj(oop)->objClass == arrayClass) {
      for(len = 0, arrayElt = constExpr; arrayElt;
	  len++, arrayElt = arrayElt->vList.next);

      if (len == numOOPs(oopToObj(oop))) {
	for (i = 1, arrayElt = constExpr; i <= len;
	     i++, arrayElt = arrayElt->vList.next) {
	  if (!equalConstant(arrayAt(oop, i), arrayElt->vList.value)) {
	    return (false);
	  }
	}
	return (true);
      }
    }
    return (false);
  }



  switch (constExpr->vConst.constType) {
  case intConst:
    if (oop == fromInt(constExpr->vConst.val.iVal)) {
      return (true);
    }
    break;

  case floatConst:
    if (isOOP(oop) && oopToObj(oop)->objClass == floatClass) {
      if (constExpr->vConst.val.fVal == floatOOPValue(oop)) {
	return (true);
      }
    }
    break;

  case charConst:
    if (oop == charOOPAt(constExpr->vConst.val.cVal)) {
      return (true);
    }
    break;

  case stringConst:
    if (isOOP(oop) && oopToObj(oop)->objClass == stringClass) {
      len = strlen(constExpr->vConst.val.sVal);
      if (len == stringOOPLen(oop)) {
	if (strncmp((char *)oopToObj(oop)->data, constExpr->vConst.val.sVal,
		    len) == 0) {
	  return (true);
	}
      }
    }
    break;

  case symbolConst:
    if (oop == constExpr->vConst.val.symVal) {
      return (true);
    }
    break;
    
  case arrayConst:
    if (isOOP(oop) && oopToObj(oop)->objClass == arrayClass) {
      /* ??? could keep the length in a counter */
      for(len = 0, arrayElt = constExpr->vConst.val.aVal; arrayElt;
	  len++, arrayElt = arrayElt->vList.next);
      if (len == numOOPs(oopToObj(oop))) {
	for (i = 1, arrayElt = constExpr->vConst.val.aVal; i <= len;
	     i++, arrayElt = arrayElt->vList.next) {
	  if (!equalConstant(arrayAt(oop, i), arrayElt->vList.value)) {
	    return (false);
	  }
#ifdef bogus /* Sat Feb 16 14:30:37 1991 */
/**/	  if (!equalConstant(arrayElt->vList.value, arrayAt(oop, i))) {
/**/	    return (false);
/**/	  }
#endif /* bogus Sat Feb 16 14:30:37 1991 */
	}
	return (true);
      }
    }
    break;
  }

  return (false);
}

/*
 *	static OOP makeConstantOOP(constExpr)
 *
 * Description
 *
 *	Given a section of the syntax tree that represents a Smalltalk
 *	constant, this routine creates an OOP to be stored as a method literal
 *	in the method that's currently being compiled.
 *
 * Inputs
 *
 *	constExpr: 
 *		A portion of the tree that contains a constant value, including
 *		array constants.
 *
 * Outputs
 *
 *	An OOP that represents the constant's value.
 */
OOP
makeConstantOOP (constExpr)
     TreeNode constExpr;
{
  TreeNode      arrayElt;
  int           len, i;
  OOP           resultOOP, elementOOP;
  IncPtr	incPtr;

  if (constExpr == nil) {
    return (nilOOP);            /* special case empty array literals */
  }

  if (constExpr->nodeType == symbolNodeType) { /* symbol in array constant */
    return (constExpr->vExpr.selector);
  } else if (constExpr->nodeType == arrayEltListType) {
    for(len = 0, arrayElt = constExpr; arrayElt;
	len++, arrayElt = arrayElt->vList.next);
    
    incPtr = incSavePointer();
    /* {this might be an uninitialized form of array creation for speed}
     * not now -- with the array temporarily part of the root set it must
     * be completely initialized (sigh).
     */
    resultOOP = arrayNew(len);
    incAddOOP(resultOOP);

    for (i = 1, arrayElt = constExpr; i <= len;
	 i++, arrayElt = arrayElt->vList.next) {
      elementOOP = makeConstantOOP(arrayElt->vList.value);
      arrayAtPut(resultOOP, i, elementOOP);
      if (isOOP(elementOOP)) {
        makeOOPReadOnly(elementOOP, true);
      }
    }
    incRestorePointer(incPtr);
    return (resultOOP);
  }

  switch (constExpr->vConst.constType) {
  case intConst:
    return (fromInt(constExpr->vConst.val.iVal));

  case floatConst:
    return (floatNew(constExpr->vConst.val.fVal));

  case charConst:
    return (charOOPAt(constExpr->vConst.val.cVal));

  case stringConst:
    return (stringNew(constExpr->vConst.val.sVal));

  case symbolConst:
    return (constExpr->vConst.val.symVal);
    
  case arrayConst:
    for(len = 0, arrayElt = constExpr->vConst.val.aVal; arrayElt;
	len++, arrayElt = arrayElt->vList.next);
    
    incPtr = incSavePointer();
    /* ??? this might be an uninitialized form of array creation for speed */
    resultOOP = arrayNew(len);
    incAddOOP(resultOOP);

    for (i = 1, arrayElt = constExpr->vConst.val.aVal; i <= len;
	 i++, arrayElt = arrayElt->vList.next) {
      arrayAtPut(resultOOP, i, makeConstantOOP(arrayElt->vList.value));
    }

    incRestorePointer(incPtr);
    return (resultOOP);
  }

  return (nilOOP);
}

/*
 *	static int addSelector(selector)
 *
 * Description
 *
 *	Like addConstant, this routine adds "selector" to the set of selectors
 *	for the current method, and returns the index of that selector.  If the
 *	selector already existed, its index is returned.  If the selector is
 *	a special selector, then the negative of the bytecode that's associated
 *	with that special selector is returned.
 *
 * Inputs
 *
 *	selector: 
 *		A symbol that is the selector to be added to the selectors of
 *		the current method.
 *
 * Outputs
 *
 *	Index of the selector in the current method, or number < 0 which is
 *	the negative of the bytecode for a send special.
 */
int
addSelector (selector)
     OOP	selector;
{
  int		builtin;

  if ((builtin = whichBuiltinSelector(selector)) != 0) {
    return (-builtin);
  } else {
    return (addForcedSelector(selector));
  }
}

/*
 *	static int whichBuiltinSelector(selector)
 *
 * Description
 *
 *	Looks for special-cased selectors, and returns a special number to
 *	indicate which selector was chosen.  If the selector isn't one of the
 *	special-cased ones, 0 is returned.
 *
 * Inputs
 *
 *	selector: 
 *		An instance of Symbol, to be special-cased.
 *
 * Outputs
 *
 *	0 if the selector isn't special-cased, otherwise an index to be used
 *	by the compiler for the selector.
 */
int
whichBuiltinSelector (selector)
     OOP	selector;
{
  if (selector == atColonSymbol) {
    return (atColonSpecial);
  } else if (selector == atColonPutColonSymbol) {
    return (atColonPutColonSpecial);
  } else if (selector == sizeSymbol) {
    return (sizeSpecial);
  } else if (selector == nextSymbol) {
    return (nextSpecial);
  } else if (selector == nextPutColonSymbol) {
    return (nextPutColonSpecial);
  } else if (selector == atEndSymbol) {
    return (atEndSpecial);
  } else if (selector == classSymbol) {
    return (classSpecial);
  } else if (selector == blockCopyColonTemporariesColonSymbol) {
    return (blockCopyColonSpecial);
  } else if (selector == valueSymbol) {
    return (valueSpecial);
  } else if (selector == valueColonSymbol) {
    return (valueColonSpecial);
  } else if (selector == doColonSymbol) {
    return (doColonSpecial);
  } else if (selector == newSymbol) {
    return (newSpecial);
  } else if (selector == newColonSymbol) {
    return (newColonSpecial);
  } else if (selector == plusSymbol) {		/* all these are open coded */
    return (plusSpecial);			/* for Integers and Floats */
  } else if (selector == minusSymbol) {
    return (minusSpecial);
  } else if (selector == lessThanSymbol) {
    return (lessThanSpecial);
  } else if (selector == greaterThanSymbol) {
    return (greaterThanSpecial);
  } else if (selector == lessEqualSymbol) {
    return (lessEqualSpecial);
  } else if (selector == greaterEqualSymbol) {
    return (greaterEqualSpecial);
  } else if (selector == equalSymbol) {
    return (equalSpecial);
  } else if (selector == notEqualSymbol) {
    return (notEqualSpecial);
  } else if (selector == timesSymbol) {
    return (timesSpecial);
  } else if (selector == divideSymbol) {
    return (divideSpecial);
  } else if (selector == remainderSymbol) {
    return (remainderSpecial);
  } else if (selector == bitShiftColonSymbol) {
    return (bitShiftColonSpecial);
  } else if (selector == integerDivideSymbol) {
    return (integerDivideSpecial);
  } else if (selector == bitAndColonSymbol) {
    return (bitAndColonSpecial);
  } else if (selector == bitOrColonSymbol) {
    return (bitOrColonSpecial);
  } else if (selector == sameObjectSymbol) {	/* these four are open coded */
    return (sameObjectSpecial);
  } else if (selector == isNilSymbol) {
    return (isNilSpecial);
  } else if (selector == notNilSymbol) {
    return (notNilSpecial);
  } else if (selector == yourselfSymbol) {	/* am I asking too much? */
    return (nopBytecode);
  } else {
    return (0);
  }
}

/*
 *	static int addForcedSelector(selector)
 *
 * Description
 *
 *	Adds the given selector to the method literals, returning the index
 *	that the selector was stored under.
 *
 * Inputs
 *
 *	selector: 
 *		An instance of Symbol to be added.
 *
 * Outputs
 *
 *	Index of where in the literal vector the Symbol was stored.
 */
int
addForcedSelector (selector)     
     OOP	selector;
{

  return (addForcedObject(selector));
}

/*
 *	int addForcedObject (oop)
 *
 * Description
 *
 *	Adds "oop" to the literal vector that's being created, unless it's
 *	already there.  "Already there" is defined as the exact same object is
 *	present in the literal vector.
 *
 * Inputs
 *
 *	oop   : An OOP to be added to the literal vector.
 *
 * Outputs
 *
 *	Index into the literal vector where the object was stored.  Seems like
 *	it's zero based, but I believe in practice that it's 1 based.
 */
int
addForcedObject (oop)
     OOP	oop;
{
  int		i;

  for (i = 0; i < numLiterals; i++) {
    if (literalVec[i] == oop) {
      return (i);
    }
  }

  return (addLiteral(oop));
}

/*
 *	static OOP computeSelector(selectorExpr)
 *
 * Description
 *
 *	Given a TreeNode of type keywordExprNode, this routine picks out the
 *	names selector "keywords", concatenates them, turns them into a symbol
 *	and returns that symbol.  This routine may also be called with a
 *	unaryExprType or binaryExprType tree node, in which case the selector
 *	is already known, so it is just returned.
 *
 * Inputs
 *
 *	selectorExpr:
 *		TreeNode node of type keywordExprNode, unaryExprType, or
 *		binaryExprType.
 *
 * Outputs
 *
 *	Symbol OOP that is the selector that "selectorExpr" represents.
 */
OOP
computeSelector (selectorExpr)
     TreeNode selectorExpr;
{
  TreeNode	keyword;
  int		len;
  char		*nameBuf, *p;

  if (selectorExpr->nodeType == unaryExprType
      || selectorExpr->nodeType == binaryExprType) {
    return (selectorExpr->vExpr.selector);
  }

  len = 0;
  for (keyword = selectorExpr->vExpr.expression; keyword != nil;
       keyword = keyword->vList.next) {
    len += strlen(keyword->vList.name);
  }

  p = nameBuf = (char *)alloca(len+1);
  for (keyword = selectorExpr->vExpr.expression; keyword != nil;
       keyword = keyword->vList.next) {
    len = strlen(keyword->vList.name);
    strcpy(p, keyword->vList.name);
    p += len;
  }

  *p = '\0';

  return (internString(nameBuf));
}


/*
 *	static void addMethodClassVariable()
 *
 * Description
 *
 *	Called when a method contains at least one reference to super as a
 *	receiver, this routine makes sure that the last literal variable of the
 *	method is the class that the method is a part of.
 *
 */
#ifdef not_needed
/**/void
/**/addMethodClassVariable()
/**/{
/**/  addLiteral(associationNew(getClassSymbol(thisClass), thisClass));
/**/}
#endif

/*
 *	static int listLength(listExpr)
 *
 * Description
 *
 *	Computes and returns the length of a parse tree list.
 *
 * Inputs
 *
 *	listExpr: 
 *		A list from the tree builder.
 *
 * Outputs
 *
 *	The length of the list, as an integer.
 */
int
listLength (listExpr)
     TreeNode listExpr;
{
  TreeNode 	l;
  long		len;

  for(len = 0, l = listExpr; l; l = l->vList.next, len++);

  if (sizeof(int) != 4) {
    if (len > (1L << (sizeof(int)*8 - 1))) {
      errorf("List too long, %ld", len);
      len = 1L << (sizeof(int)*8 - 1);
    }
  }

  return ((int)len);
}

mst_Boolean
isIdentityLiteral (literalIndex)
     int literalIndex;
{
  if (literalIndex == -1) {	/* you can pass in -1 for true, false, nil */
    return (true);
  } else {
    OOP literal;
    literal = literalVec[literalIndex];
    return (isInt(literal) || oopClass(literal) == charClass ||
    	oopClass(literal) == symbolClass);
  }
}



/***********************************************************************
 *
 *	Literal Vector manipulation routines.
 *
 ***********************************************************************/


/*
 *	static int addLiteral(OOP)
 *
 * Description
 *
 *	Adds "OOP" to the literals associated with the method being compiled
 *	and returns the index of the literal slot that was used (1 based).
 *
 * Inputs
 *
 *	oop:	OOP to add to the literal vector.
 *
 * Outputs
 *
 *	Index (1 based) in the literal vector of where the OOP was added.
 */
int
addLiteral (oop)
     OOP	oop;
{
  if (numLiterals >= literalVecMax) {
    reallocLiteralVec();
  }

  literalVec[numLiterals] = oop;
  return (numLiterals++);
}

/*
 *	static void reallocLiteralVec()
 *
 * Description
 *
 *	Called to grow the literal vector that the compiler is using.  Modifies
 *	the global variables "literalVec" and "literalVecMax" to reflect the
 *	growth. 
 *
 */
void
reallocLiteralVec()
{
  literalVecMax += LITERAL_VEC_CHUNK_SIZE;
  literalVec = (OOP *)xrealloc(literalVec, literalVecMax * sizeof(OOP));
}


/*
 *	OOP getLiteralsArray()
 *
 * Description
 *
 *	Creates a new array object that contains the literals for the method
 *	that's being compiled and returns it.  As a side effect, the currently
 *	allocated working literal vector is freed.  If there were no literals
 *	for the current method, nilOOP is returned.
 *
 * Outputs
 *
 *	The newly created array object, or nilOOP.
 */
OOP
getLiteralsArray()
{
  OOP		methodLiterals;
  int		i;

  if (numLiterals == 0) {
    return (nilOOP);
  }

  methodLiterals = arrayNew(numLiterals);
  for (i = 0; i < numLiterals; i++) {
    arrayAtPut(methodLiterals, i+1, literalVec[i]);
  }

  numLiterals = 0;

  return (methodLiterals);
}



/*
 *	static void installMethod(selector, primitiveIndex, numArgs, numTemps,
 *		maximumStackDepth, byteCodes)
 *
 * Description
 *
 *	Creates a new CompileMethod and installs it in the method dictionary
 *	for the current class.  If the current class does not contain a valid
 *	method dictionary, one is allocated for it.
 *
 * Inputs
 *
 *	selector: 
 *		A symbol that the compiled method should be stored under.  This
 *		selector is what will be matched against the message selector
 *		when a message is sent to the class that this method is a part
 *		of.
 *	primitiveIndex: 
 *		A C integer for the primitive operation associated with this
 *		method.  0 if no primitive is associated with this method.
 *	numArgs: 
 *		A C integer for the number of arguments that this method has.
 *	numTemps: 
 *		A C integer for the number of temporaries that this method has.
 *	maximumStackDepth:
 *		A C integer containing the number of stack slots to allocate.
 *	byteCodes: 
 *		Vector of the bytecodes for the CompiledMethod.
 *
 */
void
installMethod (selector, primitiveIndex, numArgs, numTemps,
		  maximumStackDepth, byteCodes)
     OOP	selector;
     int	primitiveIndex, numArgs, numTemps, maximumStackDepth;
     ByteCodes  byteCodes;
{
  OOP		method;
  IncPtr	incPtr;

  if (declareTracing) {
    printf("  Class "); printObject(thisClass);
    printf(", selector "); printSymbol(selector); printf("\n");
  }

  incPtr = incSavePointer();
  /* methodDictionaryOOP is held onto by the class, which is
   * presumably already reachable by the root set so we don't need to
   * hold onto it here.
   */
  methodDictionaryOOP = validClassMethodDictionary(thisClass);

  method = makeNewMethod(primitiveIndex, numArgs, numTemps,
			  maximumStackDepth, nil, byteCodes);
  incAddOOP(method);

  latestCompiledMethod = method; /* reachable by the root set */

  identityDictionaryAtPut(methodDictionaryOOP, selector, method); 
  methodDictionaryOOP = nilOOP;
  invalidateMethodCache();

  incRestorePointer(incPtr);
}

/*
 *	OOP makeNewMethod(primitiveIndex, numArgs, numTemps,
 *			  maximumStackDepth, literals, byteCodes)
 *
 * Description
 *
 *	Constructs and returns a new CompiledMethod instance.  It computes the
 *	method header based on its arguments, and on the contents of the
 *	method's byte codes.
 *
 * Inputs
 *
 *	primitiveIndex: 
 *		Integer, non-zero indicates that the method has a primitive
 *		method associated with it.  The primitive will be invoked first
 *		when the method is invoked, and only if the primitive signals
 *		failure will the remaining byte codes of the method be
 *		executed.
 *	numArgs: 
 *		Number of arguments that the method has.  A C integer.
 *	numTemps: 
 *		Number of temporaries that the method has.  A C integer.
 *	maximumStackDepth:
 *		A C integer containing the number of stack slots to allocate.
 *	byteCodes: 
 *		A vector of byte codes that make up the executable part of the
 *		method. 
 *
 * Outputs
 *
 *	A newly allocated CompiledMethod, fully initialized and ready to go.
 */
OOP
makeNewMethod (primitiveIndex, numArgs, numTemps, maximumStackDepth,
		literals, byteCodes)
     int	primitiveIndex, numArgs, numTemps, maximumStackDepth;
     OOP	literals;
ByteCodes byteCodes;
{
  MethodHeader	header;
  int		newFlags;
  OOP		method;
  IncPtr	incPtr;

  header.intMark = 1;
  header.headerFlag = 0;
  if (primitiveIndex) {
    if (declareTracing) {
      printf("  Primitive Index %d\n", primitiveIndex);
    }
  }

  if (primitiveIndex == 0 && numArgs == 0 && numTemps == 0) {
    if ((newFlags = isSimpleReturn(byteCodes)) != 0) {
      header.headerFlag = newFlags & 0xFF;
      /* if returning an instance variable, its index is indicated in the
         number of temporary variables */
      numTemps = newFlags >> 8;
      literals = nil;

      freeByteCodes(byteCodes);
      byteCodes = nil;
    }
  }

  incPtr = incSavePointer();
  if (!literals) {
    literals = getLiteralsArray();
    incAddOOP(literals);
  }

  if (byteCodes) {
#ifdef PRINT_BEFORE_OPTIMIZATION
    if (declareTracing) {
      printByteCodes(byteCodes, oopToObj(literals)->data);
    }
#endif
    byteCodes = optimizeByteCodes(byteCodes);
  }

  if (declareTracing) {
    printByteCodes(byteCodes, oopToObj(literals)->data);
  }

  maximumStackDepth += (1 << DEPTH_SCALE) - 1;	/* round */
  maximumStackDepth >>= DEPTH_SCALE;
  maximumStackDepth++;				/* for blockCopys, etc */
 
  header.stackDepth = maximumStackDepth;
  header.primitiveIndex = primitiveIndex;
  header.numArgs = numArgs;
  header.numTemps = numTemps;

  method = methodNew(header, literals, byteCodes);
  incRestorePointer(incPtr);
  return(method);
}

/*
 *	static OOP methodNew(header, literals, byteCodes)
 *
 * Description
 *
 *	Creates and returns a CompiledMethod.  The method is completely filled
 *	in, including the descriptor, the method literals, and the byte codes
 *	for the method.
 *
 * Inputs
 *
 *	header: Header of the method, a Smalltalk Integer.
 *	literals: 
 *		A Smalltalk Array of literals that the method should contain.
 *	byteCodes: 
 *		The byte code vector for the method.
 *
 * Outputs
 *
 *	A newly created CompiledMethod instance that's completely initialized.
 */
OOP
methodNew (header, literals, byteCodes)
     MethodHeader header;
     OOP	  literals;
     ByteCodes    byteCodes;
{
  int		numByteCodes;
  Method	method;
  OOP		methodOOP, methodDesc;

  if (byteCodes != nil) {
    numByteCodes = byteCodeLength(byteCodes);
  } else {
    numByteCodes = 0;
  }

  methodDesc = methodInfoNew();
  incAddOOP(methodDesc);

  method = simpleMethodNew(numByteCodes, header);
  method->descriptor = methodDesc;

  method->literals = literals;

  if (byteCodes != nil) {
    copyByteCodes(method->bytecodes, byteCodes);
    freeByteCodes(byteCodes);
  }

  /* do this last so low bits in flag are correct, and method gets moved into
   * "toSpace" after any intervening GC's */
  methodOOP = allocOOP(method);
  initEmptyBytes(methodOOP, numByteCodes);
  
  return (methodOOP);
}

/*
 *	OOP methodNewOOP(numByteCodes, header)
 *
 * Description
 *
 *	Used to implement the primitive that creates new compiled methods.
 *	Allocates and returns an OOP for a CompiledMethod.
 *
 * Inputs
 *
 *	numByteCodes: 
 *		Number of byte codes that the CompiledMethod will contain.
 *	header: The header for the CompiledMethod.  A Smalltalk integer.
 *
 * Outputs
 *
 *	Newly allocated CompiledMethod OOP.
 */
OOP
methodNewOOP (numByteCodes, header)
     long	numByteCodes;
     MethodHeader header;
{
  Method	method;
  OOP		oop;

  method = simpleMethodNew(numByteCodes, header);
  oop = allocOOP(method);
  /* ### Perhaps this could be a little better abstracted */
  initEmptyBytes(oop, numByteCodes);

  return (oop);
}

/*
 *	static Method simpleMethodNew(numByteCodes, header)
 *
 * Description
 *
 *	Creates and returns a compiled method object.  It sets the header of
 *	the compiled method from "header", and allocates the object so that it
 *	can hold "numByteCodes" byte codes.
 *
 * Inputs
 *
 *	numByteCodes: 
 *		An integer that is the number of byte codes the compiled method
 *		will contain.
 *	header: The method header to be used.  Used for placing into the
 *		compiled method and for figuring out how many literals the
 *		method has.
 *
 * Outputs
 *
 *	A compiled method object (NOT an OOP!).
 */
Method
simpleMethodNew (numByteCodes, header)
     long	numByteCodes;
     MethodHeader header;
{
  Method	method;
  int		numBytes;

  numBytes = sizeof(struct CompiledMethodStruct) - sizeof(Byte) + numByteCodes;

  method = (Method)allocObj(ROUNDED_WORDS(numBytes) << LONG_SHIFT);
  method->objSize = ROUNDED_WORDS(numBytes);
  method->objClass = compiledMethodClass;
  method->header = header;

  return (method);
}

/*
 *	mst_Boolean validMethodIndex(methodOOP, index)
 *
 * Description
 *
 *	Returns true if "index" is in the range of valid indices into the
 *	instance variables of CompiledMethod "methodOOP".
 *
 * Inputs
 *
 *	methodOOP: 
 *		An instance of CompiledMethod.
 *	index : 1 based integer index into the instance variables of the
 *		method.
 *
 * Outputs
 *
 *	True if "index" is within legal range of indices to the instance
 *	variables of the method.  False otherwise.
 */
mst_Boolean
validMethodIndex (methodOOP, index)
     OOP	methodOOP;
     long	index;
{
  Method	method;

  method = (Method)oopToObj(methodOOP);
  /* Written this way to allow a debugging person to see the return value */
  if (index >= 1 && index <= (oopToObj(method->literals)->objSize - 2)) {
    return (true);
  } else {
    return (false);
  }
}

/*
 *	OOP compiledMethodAt(methodOOP, index)
 *
 * Description
 *
 *	Return the object at "index" in CompiledMethod "methodOOP".  Method
 *	literals currently start at index 1.
 *
 * Inputs
 *
 *	methodOOP: 
 *		An instance of CompiledMethod.
 *	index : A 1 based integer index into the literals of the
 *		method. 
 *
 * Outputs
 *
 *	The object at the given index.
 */
OOP
compiledMethodAt (methodOOP, index)
     OOP	methodOOP;
     long	index;
{
  Method	method;
  OOP		oop;

  method = (Method)oopToObj(methodOOP);
  oop = oopToObj(method->literals)->data[index-1];
  return (oop);
}

/*
 *	void compiledMethodAtPut(methodOOP, index, valueOOP)
 *
 * Description
 *
 *	Store "valueOOP" into the instance variable of CompiledMethod
 *	"methodOOP" at "index".
 *
 * Inputs
 *
 *	methodOOP: 
 *		A CompiledMethod instance.
 *	index : A 1 based index into the method's literals.
 *	valueOOP: 
 *		The object to be stored at the given index.
 *
 */
void
compiledMethodAtPut (methodOOP, index, valueOOP)
     OOP	methodOOP, valueOOP;
     long	index;
{
  Method method;

  method = (Method)oopToObj(methodOOP);
  oopToObj(method->literals)->data[index-1] = valueOOP;
}

/*
 *	OOP getMethodDescriptor(methodOOP)
 *
 * Description
 *
 *	Returns the descriptor for the given CompiledMethod.  The descriptor
 *	contains information about which category the method was stored under,
 *	and information that can be used to reconstruct the source code for the
 *	method.
 *
 * Inputs
 *
 *	methodOOP: 
 *		OOP of a CompiledMethod instance.
 *
 * Outputs
 *
 *	Descriptor that was stored in the method, normally a MethodInfo
 *	instance.
 */
OOP
getMethodDescriptor (methodOOP)
     OOP	methodOOP;
{
  Method method;

  method = (Method)oopToObj(methodOOP);
  return (method->descriptor);
}

/*
 *	void setMethodDescriptor(methodOOP, descriptorOOP)
 *
 * Description
 *
 *	Sets the method descriptor of "methodOOP" to be "descriptorOOP".
 *
 * Inputs
 *
 *	methodOOP: 
 *		OOP of a CompiledMethod.
 *	descriptorOOP: 
 *		OOP of a MethodInfo instance which contains the descriptor for
 *		the CompiledMethod. 
 *
 */
void
setMethodDescriptor (methodOOP, descriptorOOP)
     OOP	methodOOP, descriptorOOP;
{
  Method method;

  method = (Method)oopToObj(methodOOP);
  method->descriptor = descriptorOOP;
}

/*
 *	static OOP methodInfoNew()
 *
 * Description
 *
 *	Returns an instance of MethodInfo.  This instance is used in the
 *	reconstruction of the source code for the method, and holds the
 *	category that the method belongs to.
 *
 * Outputs
 *
 *	Instance of MethodInfo used to hold category and source string
 *	information.
 */
OOP
methodInfoNew ()
{
  MethodInfo	methodInfo;
  OOP		sourceCode;
  IncPtr	incPtr;

  incPtr = incSavePointer();
  sourceCode = fileSegmentNew();
  incAddOOP(sourceCode);

  methodInfo = (MethodInfo)newInstance(methodInfoClass);
  methodInfo->sourceCode = sourceCode;
  methodInfo->category = thisCategory;

  incRestorePointer(incPtr);
  return (allocOOP(methodInfo));
}

/*
 *	static OOP fileSegmentNew()
 *
 * Description
 *
 *	Returns a FileSegment instance for the currently open compilation
 *	stream.   FileSegment instances are used record information useful in
 *	obtaining the source code for a method that's been compiled.  Depending
 *	on whether the input stream is a string or a FileStream, the instance
 *	variables are different; for a string, the entire contents of the
 *	string is preserved as the source code for the method; for a disk file,
 *	the file name, byte offset and length are preserved.
 *
 *  brd Sat Apr 23 12:39:55 PDT 1994
 *  Store in memory the string contents of source code derived from .st files
 *  loaded outside the kernel source directory.  There are too many
 *  data integrity problems in referencing the source code indirectly
 *  by its file origin.
 *
 * Outputs
 *
 *	A FileSegment instance that can be used to recover the current method's
 *	source code.
 */
OOP
fileSegmentNew ()
{
  OOP		fileName, stringContents;
  FileSegment	fileSegment;
  int		startPos;
  char		*s;
  IncPtr	incPtr;

  switch (getCurStreamType()) {
  case unknownStreamType:
  default:
    return (nilOOP);

  case fileStreamType: 
  if (isKernelFile()) {
    incPtr = incSavePointer();
    fileName = getCurFileName();
    incAddOOP(fileName);

    fileSegment = (FileSegment)newInstance(fileSegmentClass);
    fileSegment->fileName = fileName;
    startPos = getMethodStartPos();
    fileSegment->startPos = fromInt(startPos);
    /* The second -1 removes the terminating '!' */
    fileSegment->length = fromInt(getCurFilePos() - startPos - 1 - 1);

    incRestorePointer(incPtr);
    return (allocOOP(fileSegment));
  } else {
    /* Return a string instance containing method source if the source was
     * derived from a file loaded outside the kernel directory- brd*/ 
    stringContents = stringNew(s = getMethodSourceFromCurFile());
    xfree(s);
    return (stringContents);
  }

  case stringStreamType:
    stringContents = getCurString();
    return (stringContents);

#ifdef HAVE_READLINE
  case readlineStreamType:
    stringContents = getCurReadline();
    return (stringContents);
#endif /* HAVE_READLINE */
  }
}
