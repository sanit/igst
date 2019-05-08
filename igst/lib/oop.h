/***********************************************************************
 *
 *	Object Table declarations.
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



#ifndef __GSTOOP__
#define __GSTOOP__

/* The number of OOPs in the system.  This is exclusive of Character, True,
   False, and UndefinedObject (nil) oops, which are built-ins. */
#define OOP_TABLE_SIZE		(10240 * 16) /* for the nonce, then back to 4 */

#define NUM_CHAR_OBJECTS	256
#define CHAR_OBJECT_BASE	OOP_TABLE_SIZE
#define NUM_BUILTIN_OBJECTS	3
#define BUILTIN_OBJECT_BASE	(CHAR_OBJECT_BASE + NUM_CHAR_OBJECTS)

#define nilOOPIndex		(BUILTIN_OBJECT_BASE + 0)
#define trueOOPIndex		(BUILTIN_OBJECT_BASE + 1)
#define falseOOPIndex		(BUILTIN_OBJECT_BASE + 2)

#define TOTAL_OOP_TABLE_SLOTS \
  ( OOP_TABLE_SIZE + NUM_CHAR_OBJECTS + NUM_BUILTIN_OBJECTS )

/*
 * Given a number of bytes "x", return the number of 32 bit words
 * needed to represent that object, rounded up to the nearest 32 bit
 * word boundary.
 */
#define ROUNDED_WORDS(x) \
  (((x) + sizeof(long) - 1) / sizeof(long))



typedef struct CharObjectStruct {
  OBJ_HEADER;
#if defined(WORDS_BIGENDIAN)
  Byte		dummy[3];	/* filler */
  Byte		charVal;
#else
  Byte		charVal;
  Byte		dummy[3];	/* filler */
#endif
} CharObject;

struct NilObjectStruct {
  OBJ_HEADER;
};

struct BooleanObjectStruct {
  OBJ_HEADER;
  OOP		booleanValue;
};

struct memorySpaceStruct {
  char		*space;		/* base of allocated storage */
  char		*allocPtr;	/* new space ptr, starts low, goes up */
  char		*maxPtr;	/* points to highest addr in heap  */
  char		*newAllocPtr;	/* new space ptr, starts low, goes up */
  unsigned long	totalSize;	/* current allocated size */
};

extern CharObject		charObjectTable[];
extern struct NilObjectStruct 	nilObject;
extern struct BooleanObjectStruct booleanObjects[];
extern int			numFreeOOPs;
extern unsigned long		maxSpaceSize;
extern mst_Boolean		gcState, gcMessage;

extern int			growThresholdPercent, spaceGrowRate;

extern OOP			/* allocOOP(), */ charOOPAt(), findAnInstance();
extern void			initOOP(), setOOPAt(), swapObjects(), 
				fixupMetaclassObjects(), moveOOP(), gcFlip(),
				minorGCFlip(), setSpaceInfo(), growBothSpaces(),
  				allocOOPTable(), initMem(), initOOPTable(),
				markAnOOPInternal(), printObject(),
				/* initCharTable(), initNil(), initBooleans(), */
				initBuiltinObjectsClasses(),
				debug(), refreshOOPFreeList(),
				incInitRegistry(), incGrowRegistry();
extern Byte			charOOPValue();
extern mst_Object		allocObj(), curSpaceAddr();
extern mst_Boolean		oopIndexValid(), gcOn(), gcOff(), setGCState(),
				growTo(), growMemoryTo();

extern struct OOPStruct		*oopTable;
extern OOP			lastUsedOOP;

/* This variable represents information about the memory space.  memSpace
   holds the required information: basically the pointer to the base and
   top of the space, and the pointers into it for allocation and copying.
*/
extern struct memorySpaceStruct memSpace;

#include "oop.inl"

#endif /* __GSTOOP__ */
