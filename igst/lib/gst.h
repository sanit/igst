/***********************************************************************
 *
 *	GNU Smalltalk generic inclusions.
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
#define ios_device

#ifndef __GST__

#ifndef __GST__PUBLIC__
#define __GST__PUBLIC__

#include "gstconf.h"

/* AIX is so broken that requires this to be the first thing in the file.  */
#if defined(_AIX)
#pragma alloca
#else
# if !defined(alloca)    /* predefined by HP cc +Olibcalls */
#  ifdef __GNUC__
#   define alloca __builtin_alloca
#  else
#   if HAVE_ALLOCA_H
#    include <alloca.h>
#   else
#    if defined(__hpux)
      void *alloca ();
#    else
#     if !defined(__OS2__) && !defined(_WIN32)
       char *alloca ();
#     else
#      include <malloc.h> /* OS/2 defines alloca in here */
#     endif
#    endif
#   endif
#  endif
# endif
#endif

#if SIZEOF_CHAR_P != SIZEOF_LONG
#error "Cannot compile: longs and pointer are different sizes"
#endif


/*#if !defined(__STDC__) && (defined(AIX) | defined(mips) | defined(ibm032) || (defined(sun) && !defined(SUNOS40))) */
#if !defined(__STDC__) && defined(OLDCC)
/* for older compilers that don't understand void * */
typedef char *voidPtr;
#else 
typedef void *voidPtr;
#endif

//typedef int booleanType ;

#define mst_Boolean int
#define true 1
#define false 0
//typedef enum booleanType {
//  false,
//  true
//} mst_Boolean;


#define F_FREE		((unsigned long) 0x80000000)
#define F_FAKE		((unsigned long) 0x40000000)
#define F_WEAK		((unsigned long) 0x20000000)
#define F_REACHABLE	((unsigned long) 0x10000000)
#define F_FINALIZE	((unsigned long) 0x08000000)
#define F_FINALIZING	((unsigned long) 0x04000000)
#define F_READONLY 	((unsigned long) 0x02000000)
#define F_CONTEXT 	((unsigned long) 0x01000000)
#define F_BYTE	 	((unsigned long) 0x00800000)
#define EMPTY_BYTES	((unsigned long) (SIZEOF_LONG - 1))


typedef struct OOPStruct	*OOP;
typedef struct ObjectStruct	*mst_Object;

struct OOPStruct {
  mst_Object	object;
  unsigned long flags;
};

/* some useful constants */
extern	OOP		nilOOP, trueOOP, falseOOP;

/* The header of all objects in the system.
 * Note how structural inheritance is achieved without adding extra levels of 
 * nested structures. */
#define OBJ_HEADER \
  long		objSize; /* for now, this is object size in 32bit words*/ \
  OOP		objClass


/* just for symbolic use in sizeof's */
typedef struct ObjectHeaderStruct {
  OBJ_HEADER;
} ObjectHeader;

#define OBJ_HEADER_SIZE_WORDS	(sizeof(ObjectHeader) / sizeof(long))

/* A bare-knuckles accessor for real objects */
struct ObjectStruct {
  OBJ_HEADER;
  OOP		data[1];	/* variable length, may not be objects, but
				   will always be at least this big. */
};

#define oopToObj(oop) \
  ((oop)->object)

#define oopIsReadOnly(oop) \
  (((oop)->flags & F_READONLY) != 0)

#define makeOOPReadOnly(oop, ro) \
  if (ro) { (oop)->flags |= F_READONLY; } \
  else { (oop)->flags &= ~F_READONLY; }

#define markOOPToFinalize(oop, fin) \
  if (fin) { (oop)->flags |= F_FINALIZE; } \
  else { (oop)->flags &= ~F_FINALIZE; }

#define makeOOPWeak(oop) \
  (oop)->flags |= F_WEAK;

#define oopClass(oop) \
  (oopToObj(oop)->objClass)

#define isClass(oop, class) \
  (class == (isInt(oop) ? integerClass : oopClass(oop)))


/* general functions */

#define isInt(oop) \
  ((long)(oop) & 1)

#define isOOP(oop) \
  (! isInt(oop) )

#define isNil(oop) \
  ((OOP)(oop) == nilOOP)


/************ incubator support **************/  
  
extern OOP			*incOOPBasePtr, *incOOPPtr, *incOOPEndPtr;

#define incAddOOP(oop) \
  { if (incOOPPtr >= incOOPEndPtr) \
      incGrowRegistry(); \
    *incOOPPtr++ = (oop); \
  }

#define incSavePointer() \
  (incOOPPtr - incOOPBasePtr)

#define incRestorePointer(ptr) \
  incOOPPtr = (ptr) + incOOPBasePtr;

typedef unsigned long	IncPtr;

#endif /* __GST__PUBLIC__ */



/**************************** BEGINNING OF PRIVATE STUFF **********/
/* This is skipped when gstpub.h includes this file               */

#ifndef __GST_FROM_GSTPUB_H__

/* Just to have something meaningful - there's no reason why
 * the makefile should not define IMAGE_PATH and KERNEL_PATH */
#ifndef IMAGE_PATH
#define IMAGE_PATH "/usr/local/share/smalltalk"
#endif
#ifndef KERNEL_PATH
//#define KERNEL_PATH "/usr/local/share/smalltalk/kernel"
#define KERNEL_PATH "/Users/Sanit/Library/Containers/th.ac.kmutt.sit.gst/Data/kernel"
#endif

#define nil			0
#define ENUM_INT(x)		((int)(x))

typedef unsigned char Byte;

typedef struct StreamStruct	*Stream;

#ifdef NO_INLINES
  #define inline
#else
#  if defined (__GNUC__)
#    undef inline
#    define inline __inline__	/* let's not lose when --ansi is specified */
#  endif
#endif

/* If they have no const, they're likely to have no volatile, either. */
#ifdef const
#define volatile
#endif

#if defined(_WIN32) && !defined(WIN32)
#define WIN32 _WIN32
#endif

#ifndef HAVE_STRDUP
extern char	*strdup();		/* else it is in string.h */
#endif

#ifdef _MSC_VER
/* Under Visual C++, the forward declaration of semIntHandler() & friends is
 * interpreted as semIntHandler(void), not semIntHandler(I'm K&R C compatible)
 * so we use this trick (hack) to shut the retentive compiler up 
 */
#define SIG_ARG_TYPE	int

/* Visual C++ does not define STDC */
#define __STDC__ 1

#else
#define SIG_ARG_TYPE	/* empty */
#endif


#ifndef HAVE_MEMCPY
#define memcpy(s1, s2, n) bcopy(s2, s1, n)

/* memset is only used for zero here, but let's be paranoid.  */
#define	memset(s, better_be_zero, n) \
  ((void) ((better_be_zero) == 0 ? (bzero((s), (n)), 0) : (abort(), 0)))
#endif /* HAVE_MEMCPY */

/* Implementation note: on 64 bit architectures, bit-encoded 32 bit words
 * are made to live in the low order 32 bits -- the high 32 bits are ignored,
 * and are padded if necessary to achieve the useful stuff in the lower 32.
 * This simplifies the current implementation, as most bit fields, both in C
 * and in Smalltalk do not have to be modified when on a 64 bit architecture.
 * Smalltalk Integers will be able to represent up to 62 bits + 1 sign bit.
 */

#if SIZEOF_LONG == 8
#define LONG_PAD_BIT_FIELDS	/* pad bit fields to be 64 bits long */
#define LONG_SHIFT 3		/* bits to shift for a long or pointer */
#define BIG_LONGS 1
#else
#define LONG_SHIFT 2		/* bits to shift for a long or pointer */
#endif


/*
   3                   2                   1 
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|    # fixed fields             |      unused         |p|w|F|0|1|
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

pb 1/11/1999: since the new non-bitfield approach makes code easier
and faster if there is nothing to the left of the "fixed fields"
field, I'm moving it to bits 16-30 (it used to stay at bits 5-30),
allocating space for more flags in case they're needed.
If you change ISP_NUMFIXEDFIELDS you should modify Behavior too.
Remember to shift by ISP_NUMFIXEDFIELDS-1 there, since Smalltalk does
not see ISP_INTMARK!!
*/

#define ISP_NUMFIXEDFIELDS      16	/* This is a shift count */
#define ISP_INDEXEDVARS		28
#define ISP_ISPOINTERS          16
#define ISP_ISWORDS             8
#define ISP_ISINDEXABLE         4
#define ISP_INTMARK             1


/* the current execution stack/instruction pointer */
extern	OOP		*outer__SP;
extern	Byte		*outer__IP;
#define sp		outer__SP
#define ip		outer__IP

/* This is TRUE if we are doing regression testing, and causes whatever sources
 * of variance to be suppressed (such as printing out execution statistics).
 */
extern	mst_Boolean	regressionTesting;

extern  OOP		thisClass;


#ifdef PROFBLOCK
struct profStruct {
  int numMinorGCs;		/* number of methods realized */
  int numMajorGCs;		/* #times realizeMethodC called */
  int numThisContexts;
  int numMethodAllocs;		/* real malloc type allocations */
  int numMethodReclaims;	/* reclaims from free list */
  int numMethodFrees; 
  int numBlockAllocs;
  int numValues;

  int stackSizeSum;		/* sum at GC time of the saved stack pointers*/
  int stackDepth;		/* num records (blocks(?) and methods on stack*/
  int maxStackDepth;		/* the max it reaches ever */
  double stackDepthAvgSum;
};

extern struct profStruct ps;
#endif


#define TreeNode		voidPtr /* dummy decl */

#define uncheckedPushOOP(oop) \
  *++sp = (oop);

#define uncheckedSetTop(oop) \
  *sp = (oop);

#define pushOOP(oop) \
  uncheckedPushOOP(oop)

#define popOOP() \
  (*sp--)

#define popNOOPs(n) \
  sp -= (n);

#define unPop(n) \
  sp += (n);

#define stackTop() \
  (*sp)

#define setStackTop(oop) \
  uncheckedSetTop(oop)

#define setStackTopInt(i) \
  uncheckedSetTop(fromInt(i))

#define setStackTopBoolean(exp) \
  uncheckedSetTop((exp) ? trueOOP : falseOOP)

#define stackAt(i) \
  (sp[-(i)])

#define pushInt(i) \
  uncheckedPushOOP(fromInt(i))

#define popInt() \
  toInt(popOOP())

#define pushBoolean(exp) \
  uncheckedPushOOP((exp) ? trueOOP : falseOOP)

#define isOOPMarked(oop) \
  ((oop)->flags & F_REACHABLE)

#define isFake(oop) \
  ((oop)->flags & F_FAKE)

#define isOOPFree(oop) \
  ((oop)->flags & F_FREE)


/* This macro should only be used right after an allocOOP, when the
 * emptyBytes field is guaranteed to be zero
 */
#define initEmptyBytes(oop, value) \
    ((oop)->flags |= (SIZEOF_LONG - (value)) & EMPTY_BYTES)

/* Use this one to assign a particular value */
#define setEmptyBytes(oop, value) \
    { (oop)->flags &= ~EMPTY_BYTES; initEmptyBytes(oop, value); }


/* Generally useful conversion functions */
#define size2Bytes(size) \
  ((size) << LONG_SHIFT)

#define bytes2Size(bytes) \
  ((bytes) >> LONG_SHIFT)


/* integer conversions */

#define toInt(oop) \
  ((long)(oop) >> 1) 

#define fromInt(i) \
  (OOP)( ((long)(i) << 1) | 1)

/* Be sure to update these if you change the representation of Integer 
 * objects.  Also update mulWithCheck() in INTERP.C
 * INT_OVERFLOW(i) checks if adding or subtracting two integers resulted
 * in an overflow.  OVERFLOWING_INT is any integer such that
 * INT_OVERFLOW(OVERFLOWING_INT) = true.
 */

#define ST_INT_SIZE        ((SIZEOF_LONG * 8) - 2)
#define MAX_ST_INT         (( ((long) 1) << ST_INT_SIZE) - 1)
#define MIN_ST_INT         ( ~MAX_ST_INT)
#define INT_OVERFLOW(i)    ( (i) > MAX_ST_INT || (i) < MIN_ST_INT )
#define OVERFLOWING_INT    (MAX_ST_INT + 1)

#define incrInt(i)         ((OOP) (((long)i) + 2))		/* 1 << 1 */
#define decrInt(i)         ((OOP) (((long)i) - 2))		/* 1 << 1 */

/* Everything has been included -- define the __GST__ guard */
#define __GST__
#endif /* __GST_FROM_GSTPUB_H__ not defined */

#endif /* __GST__ */
