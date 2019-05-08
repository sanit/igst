/***********************************************************************
 *
 *	Declarations for the byte code compiler.
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


#ifndef __GSTCOMP__
#define __GSTCOMP__

/* These next three defines are the number of bits in a method header for
   the number of stack bits, the number of temporaries, and the number of
   arguments that the method takes.  If the representation is changed, these
   definitions need to be altered too */
#define NUM_DEPTH_BITS		6
#define NUM_TEMPS_BITS		6
#define NUM_ARGS_BITS		5
#define NUM_PRIM_BITS		10
#define DEPTH_SCALE		2

#define MAX_DEPTH		(((1 << NUM_DEPTH_BITS) - 1) << DEPTH_SCALE)
#define MAX_NUM_TEMPS		((1 << NUM_TEMPS_BITS) - 1)
#define MAX_NUM_ARGS		((1 << NUM_ARGS_BITS) - 1)

/*
This is the organization of a method header.  The 1 bit in the high end of
the word indicates that this is an integer, so that the GC won't be tempted
to try to scan the contents of this field, and so we can do bitwise operations
on this value to extract component pieces.

   3                   2                   1 
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|.|.| prim index        | #temps    |   unused  |flg| #args   |1|  new
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

temporarycount 5 6..10
args 5 11..15
context size (i.e. size of new Context object / 16) in unused bits?
primitiveIndex 8 16..23
flags 2 24-25
flags 0 -- use arguments as they are, ignore prim index
flags 1 -- return self
flags 2 -- return instance variable
flags 3 -- call the primitive indexed by primIndex
*/

typedef struct MethodHeaderStruct {
#ifdef WORDS_BIGENDIAN
#ifdef LONG_PAD_BIT_FIELDS
  unsigned	dummy			: 32; /* unused */
#endif
  unsigned				: 2; /* unused */
  unsigned	primitiveIndex		: NUM_PRIM_BITS; /* index of primitve, or 0 */
  unsigned	numTemps		: NUM_TEMPS_BITS;
  unsigned	stackDepth		: NUM_DEPTH_BITS;
  unsigned	headerFlag		: 2; /* numargs, prim self, etc. */
  unsigned	numArgs			: NUM_ARGS_BITS;
  unsigned	intMark			: 1; /* flag this as an Int */
#else
  unsigned	intMark			: 1; /* flag this as an Int */
  unsigned	numArgs			: NUM_ARGS_BITS;
  unsigned	headerFlag		: 2; /* numargs, prim self, etc. */
  unsigned	stackDepth		: NUM_DEPTH_BITS;
  unsigned	numTemps		: NUM_TEMPS_BITS;
  unsigned	primitiveIndex		: NUM_PRIM_BITS; /* index of primitve, or 0 */
  unsigned				: 2; /* unused */
#ifdef LONG_PAD_BIT_FIELDS
  unsigned	dummy			: 32; /* unused */
#endif
#endif /* WORDS_BIGENDIAN */
} MethodHeader;

struct CompiledMethodStruct {
  OBJ_HEADER;
  OOP		descriptor;
  MethodHeader	header;
  OOP		literals;
  Byte		  bytecodes[1];
};

typedef struct CompiledMethodStruct *Method;

extern OOP		thisClass, latestCompiledMethod, lastReturnedValue;
extern mst_Boolean	declareTracing;
extern mst_Boolean	emacsProcess;
extern mst_Boolean	skipCompilation;

extern void		executeStatements(), displayCompilationTrace(),
			installInitialMethods(), setCompilationClass(),
			invokeInitBlocks(), compiledMethodAtPut(),
			setMethodDescriptor(), /* copyCompileContext(), */
			setCompilationCategory(), markCompileContext(),
			initDefaultCompilationEnvironment();
extern OOP		compileMethod(), compiledMethodAt(),
			getMethodDescriptor(), methodNewOOP(),
			makeNewMethod();
extern int		addForcedObject();
extern mst_Boolean	validMethodIndex(), isIdentityLiteral();

/* temps */
extern unsigned long	numThisContexts, primitivesExecuted, messagesSent;
extern unsigned long	literalReturns, instVarReturns, selfReturns;

#endif /* __GSTCOMP__ */
