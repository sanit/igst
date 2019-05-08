/***********************************************************************
 *
 *	Byte Code interpreter declarations.
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


#ifndef __GSTINTERP__
#define __GSTINTERP__

#define NUM_PRIORITIES			9

typedef struct MethodContextStruct {
  OBJ_HEADER;
  OOP		sender;
  OOP		ipOffset;	/* an integer byte index into method */
  OOP		spOffset;	/* an integer index into cur context stack */
  OOP		hasBlock;	/* nil for dead ctx, else true or false */
  OOP		method;		/* the method that we're executing */
  OOP		methodClass;	/* the class of the method that's executing */
  OOP		selector;	/* the selector that invoked this method */
  OOP		receiver;	/* the Receiver OOP */
  OOP		contextStack[1];
} *MethodContext;

#define FIXED_CTX_SIZE	(sizeof(struct MethodContextStruct) / sizeof(long) - 1)
#define MAX_CTX_SIZE	(MAX_DEPTH + FIXED_CTX_SIZE)
#define CTX_SIZE(depth)	(((depth) << DEPTH_SCALE) + FIXED_CTX_SIZE)

typedef struct BlockContextStruct {
  OBJ_HEADER;
  OOP           caller;
  OOP		ipOffset;	/* an integer byte index into method */
  OOP		spOffset;	/* an integer index into cur context stack */
  OOP		hasBlock;	/* nil or not nil */
  OOP		numArgs;	/* number of arguments we have */
  OOP		numTemps;	/* number of temporaries we have */
  OOP		outerContext;   /* the parent BlockContext or MethodContext */
  OOP		home;		/* the home context */
  OOP		contextStack[1];
} *BlockContext;

#define NUM_BLOCK_CLOSURE_EXTRA_BYTES (2 * sizeof(char) + sizeof(short))
typedef struct BlockClosureStruct {
  OBJ_HEADER;
  OOP           outerContext;   /* the parent BlockContext or MethodContext */
  char		numArgs;	/* number of arguments we have */
  char		numTemps;	/* number of temporaries we have */
  short		initialIP;	/* initial value of IP (an offset) */
} *BlockClosure;

typedef struct SemaphoreStruct {
  OBJ_HEADER;
  OOP		firstLink;
  OOP		lastLink;
  OOP		signals;
} *Semaphore;

typedef struct ProcessStruct {
  OBJ_HEADER;
  OOP		nextLink;
  OOP		suspendedContext;
  OOP		priority;
  OOP		myList;
  OOP		name;
  OOP		exceptionHandlers;
} *Process;

typedef struct ProcessorSchedulerStruct {
  OBJ_HEADER;
  OOP		processLists;
  OOP		activeProcess;
} *ProcessorScheduler;


extern unsigned long		byteCodeCounter, sampleCounter;
extern unsigned long		cacheHits, cacheMisses;
extern mst_Boolean		executionTracing;
extern mst_Boolean		makeCoreFile;
extern mst_Boolean		nonInteractive;


extern OOP			finishExecutionEnvironment();
extern void			interpret(), sendMessageInternal(),
				initInterpreter(), resetFakeContexts(),
				prepareExecutionEnvironment(), asyncSignal(),
				setFileStreamFile(), invalidateMethodCache(),
				initSignals(), /* deallocFakeContext(), */
  				fixupObjectPointers(), restoreObjectPointers(),
  				initProcessSystem(), markProcessorRegisters(),
  				printProcessState();

#include "interp.inl"

#endif /* __GSTINTERP__ */
