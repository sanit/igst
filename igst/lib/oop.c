/***********************************************************************
 *
 *	Object Table maintenance module.
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


#include <stdio.h>
#include "gst.h"
#include "alloc.h"
#include "oop.h"
#include "dict.h"
#include "save.h"
#include "comp.h"
#include "callin.h"
#include "lex.h"
#include "lib.h"
#include "sym.h"
#if STDC_HEADERS
#include <stdlib.h>
#include <string.h>
#endif /* STDC_HEADERS */

/* Size of the object semi spaces, in bytes */
#define	K		1024

/* you can increase this value if you need more space, and it won't hurt
 * performance *if* your machine has enough physical memory (otherwise, you
 * thrash the pager) */
#define INIT_MEM_SPACE_SIZE		(1 * K * K) 
#define NEW_GENERATION_SIZE		(128 * K) 


#define INIT_NUM_INCUBATOR_OOPS   50 		/* SWAG */
#define INCUBATOR_CHUNK_SIZE	  20 		/* SWAG */


/* Define this flag to turn on debugging code for OOP table management */
/* #define OOP_DEBUGGING */


#define alignSize(size) \
 ( ((size) + sizeof(OOP) - 1) & ~(sizeof(OOP) - 1) )

/* Turn on the mark bit in OBJ */
#define markObject(obj) \
  (voidPtr)(((long)(obj)) | 1)

#define unmarkObject(obj) \
  (voidPtr)(((long)(obj)) & ~1)

#define isMarked(obj) \
  isObjMarked((obj)->objClass)

#define isObjMarked(obj) \
  ((long)(obj) & 1)




/* These are the real OOPS for nil, true, and false */
OOP			nilOOP, trueOOP, falseOOP;

/* The OOP table.  This contains a pointer to the object, and some flag
   bits indicating whether the object is read-only, reachable and/or fake.
   Some of the bits indicate the difference between the allocated length
   (stored in the object itself), and the real length, because variable
   byte objects may not be an even multiple of sizeof(voidPtr). */
struct OOPStruct	*oopTable;
OOP                     firstFreeOOP, lastUsedOOP;

int			numFreeOOPs;

mst_Boolean		gcState = true, gcMessage = true;
static mst_Boolean	gcFlipping = false;

#ifdef GC_TORTURE
unsigned long		gcWaitFlipCount = 0, allocCounter = 0;
unsigned long		doTortureCounter = 0;
mst_Boolean		doGCTorture = false;
#endif


/* If there is this much space used after a gcFlip, we need to grow the
 * object heap by spaceGrowRate % next time we gcFlip, so that the storage
 * gets copied into the new, larger area.
 */
int			growThresholdPercent = 80;

/* Grow the object heap by this percentage when the amount of space used
 * exceeds growThresholdPercent.
 */
int			spaceGrowRate = 30;

/* This vector holds the storage for all the Character objects in the system.
   Since all character objects are unique, we pre-allocate space for 256 of
   them, and treat them as special built-ins when doing garbage collection.*/
CharObject		charObjectTable[NUM_CHAR_OBJECTS];

/* This is "nil" object in the system.  That is, the single instance of the
   UndefinedObject class, which is called "nil". */
struct NilObjectStruct	nilObject;

/* These represent the two boolean objects in the system, true and false.
   This is the object storage for those two objects. 
   false == &booleanObjects[0], true == &booleanObjects[1] */
struct BooleanObjectStruct booleanObjects[2];

/* This contains the maximum size of the heap.  It is checked at every
 * allocObj; if the space is too small, it is  brought up to spec before
 * being used
 */
unsigned long	maxSpaceSize;

/* This variable represents information about the memory space.  memSpace
   holds the required information: basically the pointer to the base and
   top of the space, and the pointers into it for allocation and copying.
*/
struct memorySpaceStruct memSpace;

/* These variables hold onto the object incubator's state */
OOP 			*incOOPBasePtr, *incOOPPtr, *incOOPEndPtr;



/* static mst_Object	moveObject(); */
static mst_Boolean	growMemory();
static void		initSpace(), displayObject(), 
			displayOOP();
			
static inline void	markOOPs(), sweepOOPs(), markGlobalOOPs(),
			markIncubatorOOPs(), markFinalizableOOPReferences(),
			checkObjectsAfterSweep(),
			checkWeakRefs(), realizeOOPs();

static unsigned long	prepareForSweep();


/*
 *	void initOOPTable()
 *
 * Description
 *
 *	Initialize the OOP table.  Initially, all the OOPs are on the OOP free
 *	list so that's just how we initialize them.  We do as much
 *	initialization as we can, but we're called before classses are
 *	defined, so things that have definite classes must wait until
 *	the classes are defined.
 *
 */
void
initOOPTable()
{
  int i;

  allocOOPTable();

  numFreeOOPs = OOP_TABLE_SIZE;

  nilOOP->flags		= F_READONLY | F_REACHABLE;
  nilOOP->object	= (mst_Object)&nilObject;
  nilObject.objSize	= ROUNDED_WORDS(sizeof(struct NilObjectStruct));

  trueOOP->flags 		= F_READONLY | F_REACHABLE;
  trueOOP->object		= (mst_Object)&booleanObjects[0];
  falseOOP->flags 		= F_READONLY | F_REACHABLE;
  falseOOP->object		= (mst_Object)&booleanObjects[1];
  booleanObjects[0].objSize	= ROUNDED_WORDS(sizeof(struct BooleanObjectStruct));
  booleanObjects[1].objSize	= ROUNDED_WORDS(sizeof(struct BooleanObjectStruct));
  booleanObjects[0].booleanValue= trueOOP;
  booleanObjects[1].booleanValue= falseOOP;

  for (i = 0; i < NUM_CHAR_OBJECTS; i++) {
    charObjectTable[i].objSize = ROUNDED_WORDS(sizeof(charObjectTable[i]));
    charObjectTable[i].charVal = (char) i;
    oopTable[i + CHAR_OBJECT_BASE].object = (mst_Object)&charObjectTable[i];
    oopTable[i + CHAR_OBJECT_BASE].flags = F_READONLY | F_REACHABLE;
  }

  dictInit();			/* ### TEMP HACK ### */
}


void
allocOOPTable()
{
  register long i;

  oopTable = (struct OOPStruct *)xmalloc(
    sizeof(struct OOPStruct) * TOTAL_OOP_TABLE_SLOTS);

  nilOOP	= &oopTable[nilOOPIndex];
  trueOOP	= &oopTable[trueOOPIndex];
  falseOOP	= &oopTable[falseOOPIndex];

  firstFreeOOP  = &oopTable[OOP_TABLE_SIZE - 1];
  lastUsedOOP   = oopTable;
  firstFreeOOP->object = NULL;
  firstFreeOOP->flags = F_FREE;
  i = OOP_TABLE_SIZE - 1;
  while(firstFreeOOP != &oopTable[0]) {
    firstFreeOOP = &oopTable[--i];
    firstFreeOOP->object = (mst_Object) &oopTable[i + 1];
    firstFreeOOP->flags = F_FREE;
  }
}


/*
 *	void initBuiltinObjectsClasses()
 *
 * Description
 *
 *	Initialize the builtin objects after the respective classes
 *	have been created.
 *
 */
void
initBuiltinObjectsClasses()
{
  int		i;

  nilObject.objClass	= undefinedObjectClass;
  booleanObjects[0].objClass	= trueClass;
  booleanObjects[1].objClass	= falseClass;

  for (i = 0; i < NUM_CHAR_OBJECTS; i++) {
    charObjectTable[i].objClass = charClass;
  }
}

/*
 *	void fixupMetaclassObjects()
 *
 * Description
 *
 *	Called after the fundamental class hierarchy has been defined, this
 *	function goes through and fixes up all the objects in the oop table
 *	that don't have a objClass (objClass == nilOOP).  It's a
 *	chicken-and-egg problem: the metaclassClass doesn't yet exist when the
 *	hierarchy is put together, so after it's created, we have to go back
 *	and fix all the metaclasses that we created.
 *
 */
void
fixupMetaclassObjects()
{
  OOP		oop;
  mst_Object	object;

  for (oop = oopTable; oop <= lastUsedOOP; oop++) {
    object = oopToObj(oop);
    if (!(oop->flags & F_FREE) && isNil(object->objClass)) {
      object->objClass = metaclassClass;
    }
  }
}


/*
 *	OOP findAnInstance(classOOP)
 *
 * Description
 *
 *	Finds and returns an instance of the class CLASSOOP.  Returns "nil" if
 *	there are no instances present.
 *
 * Inputs
 *
 *	classOOP: 
 *		OOP for a class for which to find an instance
 *
 * Outputs
 *
 *	The first instance of the given class in the OOP table.
 */
OOP
findAnInstance(classOOP)
     OOP	classOOP;
{
  register OOP	oop;

  for (oop = oopTable; oop <= lastUsedOOP; oop++) {
    if (!(oop->flags & F_FREE) && (oopClass(oop) == classOOP)) {
      return (oop);
    }
  }

  return (nilOOP);
}


#ifdef ACCESSOR_DEBUGGING
/*
 *	long oopIndex(oop)
 *
 * Description
 *
 *	Returns the index within the OOP table of the given OOP.
 *
 * Inputs
 *
 *	oop   : OOP to return index of
 *
 * Outputs
 *
 *	Returned index in the OOP table, in range 0..TOTAL_OOP_TABLE_SLOTS.
 */
long
oopIndex(oop)
     OOP	oop;
{
  return (oopIndexMac(oop));
}
#endif /* ACCESSOR_DEBUGGING */

/*
 *	mst_Boolean oopIndexValid(index)
 *
 * Description
 *
 *	Checks to see if index represents a valid OOP.
 *
 * Inputs
 *
 *	index : a long index into the OOP table, apparently 1 based due to
 *		being called from Smalltalk via a primitive.
 *
 * Outputs
 *
 *	True if the index represents a valid OOP table element, false
 *	otherwise.
 */
mst_Boolean
oopIndexValid(index)
     long	index;
{
  return (index >= 1 && index <= TOTAL_OOP_TABLE_SLOTS);
}

#ifdef ACCESSOR_DEBUGGING

OOP
oopAt(index)
     long	index;
{
  return (oopAtMac(index));
}

#endif  /* ACCESSOR_DEBUGGING */


#ifdef ACCESSOR_DEBUGGING

mst_Boolean
oopValid(oop)
     OOP	oop;
{
  return (oopValidMac(index));
}


mst_Boolean
oopAvailable(index)
     long	index;
{
  return (oopAvailableMac(index));
}

#endif /* ACCESSOR_DEBUGGING */



void
swapObjects(oop1, oop2)
     OOP	oop1, oop2;
{
  struct OOPStruct tempOOP;

  tempOOP = *oop2;		/* note structure assignment going on here */
  *oop2 = *oop1;
  *oop1 = tempOOP;
}

OOP
charOOPAt(c)
     Byte	c;
{
  return (&oopTable[c + CHAR_OBJECT_BASE]);
}

Byte
charOOPValue(charOOP)
     OOP	charOOP;
{
  return (charOOP - &oopTable[CHAR_OBJECT_BASE]);
}


void
printObject(oop)
     OOP	oop;
{
  if (isInt(oop)) {
    printf("%ld", toInt(oop));
  } else if (isNil(oop)) {
    printf("nil");
  } else if (oop == trueOOP) {
    printf("true");
  } else if (oop == falseOOP) {
    printf("false");
  } else if (oopClass(oop) == charClass) {
    printf("$%c", charOOPValue(oop));
  } else if (oopClass(oop) == floatClass) {
    printf("%#g", floatOOPValue(oop));
  } else if (oopClass(oop) == symbolClass) {
    printf("#"); printSymbol(oop);
  } else if (oopClass(oop) == stringClass) {
    /* ### have to quote embedded quote chars */
    printf("'");
    printString(oop);
    printf("'");
  } else {
    printOOPConstructor(oop);
  }
  fflush(stdout);
}

void 
classifyAddr(addr)
     void	*addr;
{
  if (isOOPAddr(addr)) {
    displayOOP(addr);
  } else if (isObjAddr(addr)) {
    displayObject(addr);
  } else if isInt(addr) {
    printf("Smalltalk Integer %ld\n", toInt(addr));
  } else {
    printf("Address %p is not a Smalltalk entity\n", addr);
  }
  fflush(stdout);
}

void
displayOOP(oop)
     OOP	oop;
{
  mst_Boolean	isBuiltin;
  
  if (!isOOPAddr(oop)) {
    printf("Parameter %p does not appear to be an OOP!\n", oop);
    return;
  }

  isBuiltin = (oop >= &oopTable[OOP_TABLE_SIZE]) ? true : false;

  if (!isBuiltin) {
    printf ("OOP %p [%ld]\n", oop, (unsigned long) (oop - oopTable));
  }

  if (oop->flags & F_FREE) {
    printf("Free ");
  }

  if (oop->flags & F_REACHABLE) {
    printf("Reachable ");
  }

  printf("   Empty bytes = %ld\n", (oop->flags & EMPTY_BYTES));
  if (!(oop->flags & F_FREE)) {
    printObject(oop);
  }
  printf("\n");
}


void 
displayObject(obj)
     mst_Object	obj;
{
  if (!isObjAddr(obj)) {
    printf("Parameter %p does not appear to be an object!\n", obj);
    return;
  }
    
  printf("Object at %p, ", obj);
  printf("Size %ld\n", numOOPs(obj));
  printf("Class ");
  printObject(obj->objClass);
  printf("\n");
}


/*
 *	Object allocObj(size)
 *
 * Description
 *
 *	Allocate and return space for an object of SIZE bytes.  This basically
 *	means moving the allocation pointer for the current space up by SIZE
 *	bytes, and, if there isn't enough space left, flipping the garbage
 *	collector after memory is compacted.  The space is merely allocated;
 *      it is not initialized.
 *
 * Inputs
 *
 *	size  : size in bytes of the object to allocate.  This will be rounded
 *		by this routine up to a suitable boundary, typically to a 4
 *		byte boundary.
 *
 * Outputs
 *
 *	Address of the newly allocated object.
 */
mst_Object
allocObj(size)
     unsigned long	size;
{
  register char	*newAllocPtr, *oldAllocPtr;

#ifdef GC_TORTURE
  if (gcState && doGCTorture) {
    minorGCFlip();
  }
#endif  

  size = alignSize(size);

  /* We don't want to have allocPtr pointing to the wrong thing during GC, so
   * we use a local var to hold its new value
   */
  newAllocPtr = memSpace.allocPtr + size;
  if (newAllocPtr >= memSpace.maxPtr) {
    if (!gcFlipping) {
      /* not enough room currently, try to make some more */
      int percent;
      percent = (memSpace.newAllocPtr - memSpace.space)
        * 100 / memSpace.totalSize;

      if (percent > growThresholdPercent) {
        gcFlip();
      } else {
        minorGCFlip();
      }

      newAllocPtr = memSpace.allocPtr + size;
    }
    if (newAllocPtr >= memSpace.maxPtr) {
      /* uh oh, still too big -- we need to grow memory */
      unsigned long spaceInUse;
      unsigned long spaceNeeded;

      spaceInUse = memSpace.allocPtr - memSpace.space;
      spaceNeeded = size + spaceInUse; /* we need what we already are using,
					plus some  */
      spaceNeeded = (unsigned long)(spaceNeeded * (1.0 + spaceGrowRate/100.0));	/* allow breathing room */
      if (!growMemoryTo(spaceNeeded)) {
	/* !!! do something more reasonable in the future */
	errorf("Cannot recover, exiting...");
	exit(1);
      }
      newAllocPtr = memSpace.allocPtr + size;
    }
  }

  oldAllocPtr = memSpace.allocPtr;
  memSpace.allocPtr = newAllocPtr;
  return ((mst_Object)oldAllocPtr);
}



/*
 *	mst_Boolean gcOff()
 *
 * Description
 *
 *	Turns off the garbage collector.  Returns the previous on/off state.
 *
 * Outputs
 *
 *	Previous state of the garbage collector (on or off).
 */
mst_Boolean
     gcOff()
{
  mst_Boolean	oldGCState;

  oldGCState = gcState;
  gcState = false;
  return (oldGCState);
}

/*
 *	void gcOn()
 *
 * Description
 *
 *	Turns on the garbage collector.
 *
 */
mst_Boolean
gcOn()
{
  mst_Boolean	oldGCState;

  oldGCState = gcState;
  gcState = true;
  return (oldGCState);
}

/*
 *	void setGCState(state)
 *
 * Description
 *
 *	Set the garbage collector flag to the specified state (either on or
 *	off).
 *
 * Inputs
 *
 *	state : mst_Boolean, true => gc on.
 *
 */
mst_Boolean
setGCState(state)
     mst_Boolean	state;
{
  if(state) {
    return(gcOn());
  } else {
    return(gcOff());
  }
}



/*
 *	void initMem()
 *
 * Description
 *
 *	Initialize the memory allocator.  The memory space is allocated, and
 *	the various garbage collection flags are set to their initial values.
 *
 */
void
initMem()
{
  maxSpaceSize = INIT_MEM_SPACE_SIZE;
  initSpace(&memSpace);

  gcOff();
  incInitRegistry();
}


mst_Object
curSpaceAddr()
{
  return ((mst_Object)memSpace.space);
}

void
setSpaceInfo(size)
     long	size;
{
  memSpace.newAllocPtr = memSpace.allocPtr = memSpace.space + size;
}


/*
 *	mst_Boolean growTo(spaceSize)
 *
 * Description
 *
 *	Grows the allocated memory to the given size in bytes, if it's not
 *	there already.  In the future, the gc should give some indication that
 *	it was not able to acquire the required number of bytes, and this
 *	function will return false, causing the system primitive to fail.
 *
 * Inputs
 *
 *	spaceSize: 
 *		Size in bytes to grow the spaces to.
 *
 * Outputs
 *
 *	True if the operation was successful, False if the memory could not be
 *	allocated (this is not currently implemented).
 */
mst_Boolean
growTo(spaceSize)
     unsigned long spaceSize;
{
  if (spaceSize > maxSpaceSize) {
    maxSpaceSize = spaceSize;
    gcFlip();
  }
  
  return true;
}


/*
 *	mst_Boolean growMemoryTo(spaceSize)
 *
 * Description
 *
 *	Grows the memory segment to sapceSize.  Shoudl be called after the
 *	sweep has occurred so that things are contiguous.  Ensures that the OOP
 *	table pointers are fixed up to point to the new objects.
 *
 * Inputs
 *
 *	spaceSize: 
 *		The size of memory to grow to.  If it's not larger than the
 *		current allocation, nothing happens.
 *
 * Outputs
 *
 *	True if the operation succeeded, false of the memory could not be
 *	grown. 
 */
mst_Boolean
growMemoryTo(spaceSize)
     unsigned long spaceSize;
{
  if (spaceSize > maxSpaceSize) {
    maxSpaceSize = spaceSize;
    return growMemory();
  }
  
  return true;
}
  


/*
 *	static mst_Boolean growMemory()
 *
 * Description
 *
 *	Grows the memory segment to maxSpaceSize.  Should be called after the
 *	sweep has occurred so that things are contiguous.  Ensures that the OOP
 *	table pointers (the live ones) are fixed up to point to the new
 *	objects. 
 *
 */
mst_Boolean
growMemory()
{
  unsigned long spaceDelta;
  char		*oldSpacePtr;
  OOP		oop;

  fixupObjectPointers();
  oldSpacePtr = memSpace.space;
  memSpace.space = (char *)xrealloc(memSpace.space, maxSpaceSize + NEW_GENERATION_SIZE);
  if (!memSpace.space) {
    printf("\n\n[Memory allocation failure]\nCan't allocate enough memory to continue.\n");
    /* try to recover */
    maxSpaceSize = memSpace.totalSize;
    memSpace.space = oldSpacePtr;
    return (false);
  }
  spaceDelta = memSpace.space - oldSpacePtr;
  memSpace.totalSize = maxSpaceSize;
  memSpace.allocPtr += spaceDelta;
  memSpace.newAllocPtr += spaceDelta;
  memSpace.maxPtr = memSpace.space + memSpace.totalSize;
#ifdef GROW_DEBUG
  printf("old = %8x, new = %8x, delta = %8x\n", oldSpacePtr, memSpace.space, spaceDelta);
#endif

  if (oopTable) {
    /* Fix up the OOP table pointers to objects */
    for (oop = oopTable; oop <= lastUsedOOP; oop++) {
      if (!(oop->flags & (F_FREE | F_FAKE))) {
#ifdef GROW_DEBUG
        voidPtr obj;
        printf("old = %8x, ", obj = (voidPtr) oop->object);
#endif
	oop->object = (mst_Object)(((char *)oop->object) + spaceDelta);
#ifdef GROW_DEBUG
	printf("new = %x, delta = %x\n", oop->object, ((voidPtr)oop->object) - obj);
#endif
      }
    }
  }
  restoreObjectPointers();

  return true;
}



/*
 *	void gcFlip()
 *
 * Description
 *
 *	Mark & sweep the objects.  Starting from the root set, recursively mark
 *	objects as reachable, then compact memory, thus eliminating objects
 *	that are not reachable (sweeping them).  Then, check if the space has
 *      to be grown and, if so, readjust the collector's parameters.
 *
 */
void
gcFlip()
{
  int	lastPercent;

#ifdef PROFBLOCK
  ps.numMajorGCs++;
#endif

  if (gcMessage && !regressionTesting) {
    /* print the first part of this message before we finish scanning 
     * oop table for live ones, so that the delay caused by this scanning
     * is apparent.
     * note the use of stderr for the printed message.  The idea here was
     * that generated output could be treated as Smalltalk code, HTML or
     * whatever else you want without harm.
     */
    fflush(stdout);
    fprintf(stderr, "\"Major GC flip... ");
    fflush(stderr);
  }

  gcFlipping = true;
  fixupObjectPointers();
  markOOPs();
  sweepOOPs(memSpace.space);
  restoreObjectPointers();
  gcFlipping = false;

  /* At this point, storage in memory is compacted and contiguous, so we can
   * examine how much memory we have left, and decide if we need to increase
   * memory some more.
   */
  lastPercent = (memSpace.allocPtr - memSpace.space) * 100
    / memSpace.totalSize; 
  
  if (lastPercent > growThresholdPercent) {
    /* with bad grow rates, can undergrow.  Takes care of it */
    maxSpaceSize += maxSpaceSize / 100 * spaceGrowRate;
    maxSpaceSize &= ~(sizeof(long)-1);		/* round to word boundary */
    if (maxSpaceSize > memSpace.totalSize) {
      growMemory();
    }
    maxSpaceSize = memSpace.totalSize;
  }

  if (gcMessage && !regressionTesting) {
    fprintf(stderr, "done, used space = %i%%\"\n", lastPercent);
    fflush(stderr);
  }

  checkObjectsAfterSweep();
}

/*
 *	void minorGCFlip()
 *
 * Description
 *
 *	Mark & copy the fake objects.  Starting from the root set, recursively
 *	mark objects as reachable, then copy the actually reachable ones to the
 *	old generation.
 *
 */
void
minorGCFlip()
{
  int lastPercent;

#ifdef PROFBLOCK
  ps.numMinorGCs++;
#endif

  if (gcMessage && !regressionTesting) {
    /* print the first part of this message before we finish scanning 
     * oop table for live ones, so that the delay caused by this scanning
     * is apparent.
     * note the use of stderr for the printed message.  The idea here was
     * that generated output could be treated as Smalltalk code, HTML or
     * whatever else you want without harm.
     */
    fflush(stdout);
    fprintf(stderr, "\"Minor GC flip...");
    fflush(stderr);
  }

  gcFlipping = true;
  fixupObjectPointers();
  markOOPs();
  sweepOOPs(memSpace.newAllocPtr);
  restoreObjectPointers();
  gcFlipping = false;

  lastPercent = (memSpace.allocPtr - memSpace.space) * 100
    / memSpace.totalSize;

  if (gcMessage && !regressionTesting) {
    fprintf(stderr, " done, used space = %i%%\"\n", lastPercent);
    fflush(stderr);
  }

}


/*
 *	static void checkObjectsAfterSweep()
 *
 * Description
 *
 *	Performs special checks on weak and finalizable objects.
 *	When one of the objects pointed to by a weak object have no
 *	other references, the slot of the weak object is replaced by
 *	a nil.
 *	When a finalizable object has no references outside weak objects,
 *	prepareForSweep() marks it as F_FINALIZING.  This routine (which
 *	is executed after the sweep) unmarks it and calls back the fina-
 *	lize method.
 *
 */
void
checkObjectsAfterSweep()
{
  register OOP  oop;

  for (oop = oopTable; oop <= lastUsedOOP; oop++) {
    if (oop->flags & F_FINALIZING) {
      /* What happens when a GC occurs during finalization?!? Good question!
         The inner occurrence of checkObjectsAfterSweep will proceed finalizing
         things, unmarking them on the fly.  This way the outer call to this
         function won't ever know of the objects finalized by the inner call.
         Eh eh! */
      oop->flags ^= F_FINALIZING;
      msgSend(oop, finalizeSymbol, nil);
    }
  }
}

void
checkWeakRefs(oop)
     register OOP oop;
{
  mst_Object		object;
  register OOP		*field;
  register int		numFields;
  
  object = oopToObj(oop);
  for (field = object->data, numFields = numOOPs(object); numFields;
       field++, numFields--) {
    if (isInt(*field)) {
      continue;
    }
    if (*field <= oop) {
      /* Not yet scanned */
      if (!isOOPMarked(*field)) {
	*field = nilOOP;
      }
    } else {
      /* Already scanned */
      if (isOOPFree(*field)) {
	*field = nilOOP;
      }
    }
  }
}

void markOOPs()
{
  markGlobalOOPs();
  markProcessorRegisters();
  markRegisteredOOPs();
  markIncubatorOOPs();
  markCompileContext();
  markFinalizableOOPReferences();

#ifdef DEBUG_FREED /* Mon Jul  3 00:38:46 1995 */
/**/  {
/**/    OOP oop; int i;
/**/    for (i = 0, oop = oopTable;  oop < &oopTable[oldOopTableIndex]; oop++, i++) {
/**/      if (!(oop->flags & F_REACHABLE)) {
/**/	printf("[%4d]: "); fflush(stdout);
/**/	printObject(oop);
/**/	printf("\n");
/**/	fflush(stdout);
/**/      }
/**/    }
/**/  }
#endif /* DEBUG_FREED Mon Jul  3 00:38:46 1995 */
}

#define tailMarkOOP(newOOP) { \
  oop = (newOOP); \
  continue;		/* tail recurse!!! */ \
}

#define tailMarkOOPRange(firstOOP, oopAtEnd) { \
  curOOP = (OOP *)(firstOOP); \
  atEndOOP = (OOP *)(oopAtEnd); \
  oop = nil; \
  continue; \
}

/*
 *	static void markAnOOPInternal(oop, curOOP, atEndOOP)
 *
 * Description
 *
 *	The transitive marker.  This function works in two ways:
 *	a) when oop is nil, it walks the list of pointers to OOPs
 *	starting at curOOP (included) and finishing at atEndOOP
 *	(excluded).  Each object in the list is then marked.
 *	b) when oop is not nil, it tail recurses telling itself to
 *	mark the pointers referred to by the object pointed to by oop.
 *	Note that a single function does both these jobs to allow a
 *	fast, tail-recursive implementation of single-object marking.
 *
 *	It is called a transitive marker because the recursive approach
 *	is an algorithmic representation of the transitive property: if
 *	A (in the root set) refers to B, and B refers to C, then A indi-
 *	rectly refers to C and C is to be considered reachable.
 *	This function does not mark weak objects, for obvious reasons.
 *
 * Input
 *	see above
 */
void
markAnOOPInternal(oop, curOOP, atEndOOP)
     register OOP	oop;
     register OOP	*curOOP;
     OOP		*atEndOOP;
{
  for (;;) {
    if (!oop) {						/* in the loop! */
    iterationLoop:
      /* in a loop, do next iteration */
      oop = *curOOP;
      curOOP++;
#ifndef OPTIMIZE
      if (!isInt(oop) && !isOOPAddr(oop)) {
	debug();
	printf("Error! Bad OOP %p is being marked!", oop);
	printf("\n");
	if (curOOP < atEndOOP) {
	  goto iterationLoop;
	} else {
	  break;
	}
      }
#endif
      if (isOOP(oop) && !isOOPMarked(oop)) {
	if (curOOP < atEndOOP) {
	  markAnOOPInternal(oop, nil, nil);
	  goto iterationLoop;
	} else {
	  /* On the last object in the set, reuse the current invocation.
	     oop is valid, so we go to the single-object case */
	  continue;
	}
      }
      /* We reach this point if the object isn't to be marked.  The code above
         contains a continue to tail recurse, so we cannot put the loop in a
         do...while and a goto is necessary here.  Speed is a requirement, so
         I'm doing it. */
      if (curOOP < atEndOOP) {
        goto iterationLoop;
      }
    } else {		/* just starting with this oop */
      OOP		objClass;
      mst_Object	object;
      unsigned long	size;

#ifndef OPTIMIZE
      if (!isFake(oop) && !isObjAddr(oopToObj(oop))) {
        debug();
        printf("Error! Bad OOP at %p (%p) is being marked!", oop, oop->object);
        printf("\n");
        break;
      }
#endif
      /* see if the object has pointers, set up to copy them if so. */
      oop->flags |= F_REACHABLE;
      object = oopToObj(oop);
      objClass = object->objClass;
      if (oop->flags & F_CONTEXT) {
        MethodContext	ctx;
        long		methodSP;
        ctx = (MethodContext) object;
        methodSP = toInt(ctx->spOffset);
	/* printf("setting up for loop on context %x, sp = %d\n", ctx, methodSP); */
        tailMarkOOPRange(&ctx->objClass, ctx->contextStack + methodSP + 1);

      } else if (oop->flags & F_WEAK) {
        if (!isOOPMarked(objClass)) {
 	  tailMarkOOP(objClass);
 	}

      } else {
        size = numOOPs(object);
 	if (size) {
 	  tailMarkOOPRange(&object->objClass, object->data + size);
 	} else if (!isOOPMarked(objClass)) {
 	  tailMarkOOP(objClass);
 	}
      }
    }
    /* This point is reached if and only if nothing has to be marked anymore in
       the current iteration. So exit. */
    break;
  } /* for(;;) */
}

/*
 *	void refreshOOPFreeList()
 *
 * Description
 *
 *	Iterate through the OOP table.  Rebuild the free OOP list.
 *
 */
void
refreshOOPFreeList()
{
  register OOP  newFirstFree, oop;

  /* The free list will be reconstructed */
  newFirstFree = NULL;
  numFreeOOPs = OOP_TABLE_SIZE;

  /* Proceed backwards so that the first free OOPs are at the head of the
     free list.  This minimizes the amount of space used by the OOP table in
     a saved image. */

  for (oop = &oopTable[OOP_TABLE_SIZE - 1]; oop >= oopTable; oop--) {
    if (oop->flags & F_FREE) {
      oop->object  = (mst_Object) newFirstFree;
      newFirstFree = oop;
    } else {
      lastUsedOOP = oop;
      numFreeOOPs--;
      break;
    }
  }
  while(--oop >= oopTable) {
    if (oop->flags & F_FREE) {
      oop->object  = (mst_Object) newFirstFree;
      newFirstFree = oop;
    } else {
      numFreeOOPs--;
    }
  }
  firstFreeOOP = newFirstFree;
}

/*
 *	static unsigned long prepareForSweep(bottom)  [was: reverseOOPPointers]
 *
 * Description
 *
 *	Iterate through the OOP table.  On marked OOPs, store
 *	the pointer to the object's class into the OOP and the OOP pointer
 *	where the object class usually is.  This allows the sweep phase to
 *	change the OOPs as it moves objects in memory.  Also mark specially
 *	objects to be finalized.
 *
 * Inputs
 *
 *	bottom: The base address over which we can start to reverse pointers
 *
 * Outputs
 *
 *	The number of bytes to be allocated for reachable fake objects.
 */
unsigned long
prepareForSweep(bottom)
     voidPtr bottom;
{
  register OOP  	newFirstFree, oop;
  register mst_Object	object;
  register unsigned long size;

  /* The free list will be reconstructed, but the unused OOPs are unchanged */
  newFirstFree = firstFreeOOP;
  numFreeOOPs = OOP_TABLE_SIZE;
  size = 0;

  /* Proceed backwards so that the first free OOPs are at the head of the
     free list.  This minimizes the amount of space used by the OOP table in
     a saved image. */

  for (oop = lastUsedOOP; oop >= oopTable; oop--) {
    if (oop->flags & F_WEAK) {
      checkWeakRefs(oop);
    }

    if (oop->flags & (F_FINALIZE | F_REACHABLE)) {
      if (oop->flags & F_FAKE) {
        /* Reachable fake object.  Realize it. */
        size += size2Bytes(oopToObj(oop)->objSize);

      } else if (((voidPtr) oop->object) >= bottom) {
        /* Object reachable after mark phase.  Reverse the pointer */
        object = oopToObj(oop);
        oop->object = (mst_Object)object->objClass;
        object->objClass = markObject(oop);
      }

      if (!(oop->flags & F_REACHABLE)) {
        /* Object is *going* to be finalized, but it was not yet.  We found a
         * weak reference to it, so we mark it so that finalization will occur
         * soon after the end of the sweep pass.  In the meanwhile, we let it
         * survive and decide the object's fate at the next GC pass */
        oop->flags ^= F_FINALIZE | F_FINALIZING;
      }
      oop->flags &= ~F_REACHABLE;
      numFreeOOPs--;

    } else if (!(oop->flags & F_FREE)) {
      /* Object not marked and not already freed.  Add to OOP free list */
      oop->flags   = F_FREE;
      oop->object  = (mst_Object) newFirstFree;
      newFirstFree = oop;
      if (oop == lastUsedOOP) {
        lastUsedOOP--;
      }
    }
  }
  firstFreeOOP = newFirstFree;

  return (size);
}

/*
 *	static void realizeOOPs(size)
 *
 * Description
 *
 *	Iterate through the OOP table.  Copy fake, marked objects
 *	to the main heap space, free the others.
 *
 */
void
realizeOOPs(object)
     register char *object;
{
  register OOP		 oop;
  register unsigned long size;

  for (oop = lastUsedOOP; oop >= oopTable; oop--) {
    if (oop->flags & F_FAKE) {
      size = size2Bytes(oopToObj(oop)->objSize);
      memcpy(object, oop->object, size);
      oop->object = (mst_Object) object;
      oop->flags &= ~(F_FAKE | F_REACHABLE);
      object += size;
    }
  }
  resetFakeContexts();
}

/* #define SWEEP_DEBUG */
static inline void
sweepOOPs(bottom)
     char *bottom;
{
  register char		*from, *fromStart, *to;
  unsigned long		chunkSize, realizedSize;
  register mst_Object	object;
  OOP			curClass, oop;

  realizedSize = prepareForSweep(bottom);

  /* Algorithm:
     initialize:
     * Start at beginning of allocated space.
     * Skip over the initial contiguous range of marked object, unmarking
       as you go.

     loop:
     * skip over the contiguous range of unmarked objects, leaving "to"
       where it is and advancing "from".
     * if "to" passes the end of allocated storage, we are done.
     * set "fromStart" to "from", and skip over the next contiguous range of
       marked objects, advancing "from".
     * copy the range ["fromStart".."from") to "to".  advance "to" to right
       after the newly copied area.
   */
       
  from = to = bottom;

  while (to < memSpace.allocPtr) {
    object = (mst_Object)to;
    if (!isMarked(object)) {
      /* found the end of the contiguous range */
      break;
    }
    /* unmark this dude */
    oop = unmarkObject(object->objClass);
    curClass = (OOP)oop->object;
    oop->object = object;
    object->objClass = curClass;

    to += size2Bytes(object->objSize);
  }

#ifdef SWEEP_DEBUG
   printf("skipped %d bytes of contig alloc %x space %x max %x\n",
	 to - memSpace.space, memSpace.allocPtr, memSpace.space,
	 memSpace.maxPtr);
#endif /* SWEEP_DEBUG */

  /* we've skipped over the marked initial set of objects, for which
   * no move is necessary.  Now begin the main execution loop
   */

  from = to;
  while (from < memSpace.allocPtr) {
    fromStart = from;		/* debugging only */
    while (from < memSpace.allocPtr) {
      object = (mst_Object)from;
      if (isMarked(object)) {
	/* found a non-free chunk */
	break;
      }

      /* skip over the free memory */
      from += size2Bytes(object->objSize);
    }

#ifdef SWEEP_DEBUG
     printf("skipped free range %x .. %x %d bytes\n", fromStart, from, from - fromStart);
#endif /* SWEEP_DEBUG */

    if (from >= memSpace.allocPtr) {
#ifdef SWEEP_DEBUG
       printf("hit end of memory\n");
#endif /* SWEEP_DEBUG */
      break;			/* we've hit the end of active memory */
    }

    fromStart = from;
    /* span the next in-use contiguous chunk of objects */
    while (from < memSpace.allocPtr) {
      object = (mst_Object)from;
      if (!isMarked(object)) {
	/* found a free chunk */
	break;
      }

      /* unmark this dude & tell the oop where the object *will be*  */
      oop = unmarkObject(object->objClass);
      curClass = (OOP)oop->object;
      oop->object = (mst_Object)(to + ((char *)object - fromStart));
      object->objClass = curClass;

      /* skip over the object */
      from += size2Bytes(object->objSize);
    }


    /* copy the bytes down */
    chunkSize = from - fromStart;
#ifdef SWEEP_DEBUG
     printf("copying range %x .. %x to %x, %d bytes\n",
	   fromStart, from, to, chunkSize);
#endif /* SWEEP_DEBUG */

    memcpy(to, fromStart, chunkSize);
    to += chunkSize;
  }

  memSpace.allocPtr = to;
  
  object = allocObj(realizedSize);
  realizeOOPs((char *) object);
  memSpace.newAllocPtr = memSpace.allocPtr;
}

void
markFinalizableOOPReferences()
{
  register OOP  oop;

  for (oop = oopTable; oop <= lastUsedOOP; oop++) {
    /* A finalizable object will always survive this GC flip, even if it is
       not reachable.  We mark the objects that they refer to, so they survive
       too, keeping the finalizable object marked as unreachable. */

    if ((oop->flags & (F_FINALIZE | F_REACHABLE)) == F_FINALIZE) {
      markAnOOPInternal(oop, nil, nil);
      oop->flags ^= F_REACHABLE;
    }
  }
}

void
markGlobalOOPs()
{
  OOP		**oopPtr, oop;

  /* first mark them as reachable to work on the global objects */
  /* one at a time and have more locality of reference */
  for (oopPtr = globalOOPs; *oopPtr; oopPtr++) {
    oop = **oopPtr;
    oop->flags |= F_REACHABLE;
  }
  /* then do the real marking */
  for (oopPtr = globalOOPs; *oopPtr; oopPtr++) {
    markAnOOPInternal(**oopPtr, nil, nil);
  }
}


/*
 *	static void initSpace(space)
 *
 * Description
 *
 *	Initializes the allocation and copying pointers for space SPACE.
 *
 * Inputs
 *
 *	space : Pointer to the struct memorySpaceStruct to be initialized.
 *
 */
void
initSpace(space)
     struct memorySpaceStruct *space;
{
  space->space = (char *)xmalloc(maxSpaceSize + NEW_GENERATION_SIZE);
  space->totalSize = maxSpaceSize;
  space->allocPtr = space->space;
  space->newAllocPtr = space->space;
  space->maxPtr = space->space + space->totalSize;
}




/*
 *	void debug()
 *
 * Description
 *
 *	Used for debugging.  You set a breakpoint in the debug routine in the
 *	debugger, and have code call it when you want it to stop.  Performs no
 *	action normally.
 *
 */
void
debug()
{
/* getchar(); */
}


#ifdef GC_TORTURE

/*
 *	void startGCTorture()
 *
 * Description
 *
 *	User level debugging routine to verify correct operation in a range of
 *	code.  Invoking this function ensures that the GC torturing is on.
 *	Calls to this function and stopGCTorture nest, so only the outermost
 *	call really has an effect.
 *
 *	GC torturing means that the garbage collector is run on every
 *	allocation.  This means that objects which are not reachable from the
 *	root set will be immediately freed.
 *
 */
void
startGCTorture()
{
  doGCTorture = true;
  doTortureCounter++;
}


/*
 *	void stopGCTorture()
 *
 * Description
 *
 *	User level debugging routine associated with startGCTorture.  Turns off
 *	the GC torturing if it's the outermost (in an invocation-stacking
 *	order) one, otherwise, just decrements the torture count.
 *
 */
void
stopGCTorture()
{
  if (doTortureCounter > 0) {
    if (--doTortureCounter == 0) { /* on the transition to 0... */
      doGCTorture = false;
    }
  }
}

#endif



/**********************************************************************
***********************************************************************
***********************************************************************
*
* Incubator support routines
*
***********************************************************************
***********************************************************************
***********************************************************************/

void
incInitRegistry()
{
  incOOPBasePtr = (OOP*)xmalloc(INIT_NUM_INCUBATOR_OOPS * sizeof(OOP *));
  incOOPPtr = incOOPBasePtr;
  incOOPEndPtr = incOOPBasePtr + INIT_NUM_INCUBATOR_OOPS;
}

void
incGrowRegistry()
{
  OOP*		oldBase;
  unsigned long oldPtrOffset;
  unsigned long oldRegistrySize, newRegistrySize;

  oldBase = incOOPBasePtr;
  oldPtrOffset = incOOPPtr - incOOPBasePtr;
  oldRegistrySize = incOOPEndPtr - incOOPBasePtr;

  newRegistrySize = oldRegistrySize + INCUBATOR_CHUNK_SIZE;

  incOOPBasePtr = (OOP *)xrealloc(incOOPBasePtr, newRegistrySize * sizeof(OOP *));
  incOOPPtr = incOOPBasePtr + oldPtrOffset;
  incOOPEndPtr = incOOPBasePtr + newRegistrySize;
}

void
markIncubatorOOPs()
{
  markOOPRange(incOOPBasePtr, incOOPPtr);
}
