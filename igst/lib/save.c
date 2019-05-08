/***********************************************************************
 *
 *	Binary image save/restore.
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
#include "save.h"
#include "comp.h"
#include "interp.h"
#include "dict.h"
#include "sym.h"
#include "oop.h"		/* defines oopTable */
#include "lex.h"
#include "cint.h"
#include "sysdep.h"
#include <stdio.h>

#ifndef MAXPATHLEN
#define MAXPATHLEN		1024 /* max length of a file and path */
#endif

#define fromBeginning		0 /* symbolic name for file offset modifier */

#define MASK_ENDIANNESS_FLAG 1
#define MASK_LONG_SIZE_FLAG 2

#ifdef WORDS_BIGENDIAN
#  define LOCAL_ENDIANNESS_FLAG MASK_ENDIANNESS_FLAG
#else
#  define LOCAL_ENDIANNESS_FLAG 0
#endif

#if SIZEOF_LONG == 4
#  define LOCAL_LONG_SIZE_FLAG 0
#  define byteInvert(x) \
        ((unsigned long int)((((unsigned long int)(x) & 0x000000ffU) << 24) | \
                             (((unsigned long int)(x) & 0x0000ff00U) <<  8) | \
                             (((unsigned long int)(x) & 0x00ff0000U) >>  8) | \
                             (((unsigned long int)(x) & 0xff000000U) >> 24)))
#else
#  define LOCAL_LONG_SIZE_FLAG MASK_LONG_SIZE_FLAG
#  define byteInvert(x) \
        ((unsigned long int)((((unsigned long int)(x) & 0x00000000000000ffU) << 56) | \
                             (((unsigned long int)(x) & 0x000000000000ff00U) << 40) | \
                             (((unsigned long int)(x) & 0x0000000000ff0000U) << 24) | \
                             (((unsigned long int)(x) & 0x00000000ff000000U) <<  8) | \
                             (((unsigned long int)(x) & 0x000000ff00000000U) >>  8) | \
                             (((unsigned long int)(x) & 0x0000ff0000000000U) >> 24) | \
                             (((unsigned long int)(x) & 0x00ff000000000000U) >> 40) | \
                             (((unsigned long int)(x) & 0xff00000000000000U) >> 56)))
#endif

#define flagChanged(flags, whichFlag)	\
  ((flags ^ LOCAL_##whichFlag) & MASK_##whichFlag)

#define FLAGS_WRITTEN		\
  (LOCAL_ENDIANNESS_FLAG | LOCAL_LONG_SIZE_FLAG)

#define VERSION_REQUIRED	\
  ((ST_MAJOR_VERSION << 16) + (ST_MINOR_VERSION << 8) + ST_EDIT_VERSION)

/*
 * The binary image file has the following format:
 *
 *	header
 *	complete oop table
 *	global oop variable data
 *	object data
 */


typedef struct SaveFileHeaderStruct {
  unsigned char signature[7];	/* 7+1=8 should be enough to align version! */
  unsigned char flags;		/* misc size */
  unsigned long version;	/* the Smalltalk version that made this dump */
  unsigned long	objectDataSize;	/* size of object data section in bytes */
  unsigned long	oopTableSize;	/* size of the oop table at dump */
  unsigned long	semiSpaceSize;	/* size of the semi spaces at dump time */
} SaveFileHeader;


OOP		*globalOOPs[] = {
  &mst_objectClass,
  &magnitudeClass,
  &charClass,
  &timeClass,
  &numberClass,
  &floatClass,
  &integerClass,
  &largeIntegerClass,
  &largeNegativeIntegerClass,
  &largePositiveIntegerClass,
  &largeZeroIntegerClass,
  &fractionClass,
  &associationClass,
  &linkClass,
  &processClass,
  &symLinkClass,
  &collectionClass,
  &sequenceableCollectionClass,
  &linkedListClass,
  &semaphoreClass,
  &arrayedCollectionClass,
  &arrayClass,
  &stringClass,
  &symbolClass,
  &byteArrayClass,
  &compiledMethodClass,
  &intervalClass,
  &orderedCollectionClass,
  &sortedCollectionClass,
  &bagClass,
  &mappedCollectionClass,
  &setClass,
  &dictionaryClass,
  &identityDictionaryClass,
  &systemDictionaryClass,
  &undefinedObjectClass,
  &booleanClass,
  &falseClass,
  &trueClass,
  &processorSchedulerClass,
  &delayClass,
  &sharedQueueClass,
  &behaviorClass,
  &classDescriptionClass,
  &classClass,
  &metaclassClass,
  &messageClass,
  &contextPartClass,
  &methodContextClass,
  &blockContextClass,
  &blockClosureClass,
  &streamClass,
  &positionableStreamClass,
  &readStreamClass,
  &writeStreamClass,
  &readWriteStreamClass,
  &byteStreamClass,
  &cObjectClass,
  &cTypeClass,
  &fileStreamClass,
  &memoryClass,
  &byteMemoryClass,
  &wordMemoryClass,
  &randomClass,
  &cFuncDescriptorClass,
  &tokenStreamClass,
  &methodInfoClass,
  &fileSegmentClass,
  &andColonSymbol,
  &atColonPutColonSymbol,
  &atColonSymbol,
  &atEndSymbol,
  &bitAndColonSymbol,
  &bitOrColonSymbol,
  &bitShiftColonSymbol,
  &blockCopyColonTemporariesColonSymbol,
  &classSymbol,
  &divideSymbol,
  &doColonSymbol,
  &equalSymbol,
  &finalizeSymbol,
  &greaterEqualSymbol,
  &greaterThanSymbol,
  &ifFalseColonIfTrueColonSymbol,
  &ifFalseColonSymbol,
  &ifTrueColonIfFalseColonSymbol,
  &ifTrueColonSymbol,
  &integerDivideSymbol,
  &isNilSymbol,
  &lessEqualSymbol,
  &lessThanSymbol,
  &minusSymbol,
  &newColonSymbol,
  &newSymbol,
  &nextPutColonSymbol,
  &nextSymbol,
  &notEqualSymbol,
  &notNilSymbol,
  &notSameObjectSymbol,
  &orColonSymbol,
  &plusSymbol,
  &remainderSymbol,
  &sameObjectSymbol,
  &sizeSymbol,
  &thisContextSymbol,
  &timesSymbol,
  &timesRepeatColonSymbol,
  &toColonDoColonSymbol,
  &toColonByColonDoColonSymbol,
  &valueColonSymbol,
  &valueColonValueColonSymbol,
  &valueColonValueColonValueColonSymbol,
  &valueWithArgumentsColonSymbol,
  &valueSymbol,
  &whileFalseColonSymbol,
  &whileTrueColonSymbol,
  &yourselfSymbol,
  &orSymbol,
  &andSymbol,
  &superSymbol,
  &nilSymbol,
  &trueSymbol,
  &falseSymbol,
  &selfSymbol,
  &doesNotUnderstandColonSymbol,
  &unknownSymbol,
  &charSymbol,
  &stringSymbol,
  &stringOutSymbol, 
  &symbolSymbol,
  &intSymbol,
  &longSymbol,
  &floatSymbol,
  &doubleSymbol,
  &voidSymbol,
  &variadicSymbol,
  &cObjectSymbol,
  &cObjectPtrSymbol,
  &smalltalkSymbol,
  &byteArraySymbol,
  &byteArrayOutSymbol,
  &booleanSymbol,
  &cObjectTypeCType,
  &processorOOP,
  &symbolTable,
  &smalltalkDictionary,
  nil
};



static void	skipOverHeader(), saveObject(), fixupObject(),
		restoreObject(), saveOOPTable(), fixupOOP(),
		addByteFlagToOOP(), fixupByteOrder(),
		restoreAllOOPs(), restoreInstanceVars(),
		loadOOPTable(), loadNormalOOPs(),
		/* loadCharOOPs(), loadSpecialOOPs(), */
		saveGlobalOOPs(), loadGlobalOOPs(),
		saveFileVersion(), loadFileVersion(),
		fixupInstanceVars(), fixupOOPInstanceVars(),
		skipToHeader();

static int	saveAllObjects(), my_fread();

static mst_Boolean	wrongEndianness;

/* This variable contains the OOP slot index of the highest non-free OOP,
 * excluding the built-in ones (i.e., it will always be < OOP_TABLE_SIZE).
 * This is used for optimizing the size of the saved image, and minimizing
 * the load time when restoring the system. */
static int	maxUsedOOPSlot = 0;

/* convert to a relative offset from start of OOP table.  The offset is 0 mod
 * pointer-size, so it still looks like a pointer to the isInt test.  */
#define toRelative(obj) \
  ( (OOP)((long)(obj) - (long)oopTable) )

/* convert from relative offset to actual oop table address */
#define fromRelative(obj) \
  ( (OOP)((long)(obj) + (long)oopTable) )



mst_Boolean
saveToFile(fileName)
     char	*fileName;
{
  FILE		*imageFile;
  unsigned long	objectDataSize;
  mst_Boolean	oldGCState;

  imageFile = openFile(fileName, "w");
  if (imageFile == NULL) {
    return (false);
  }

  gcFlip();			/* make sure that the world is compact */

  oldGCState = gcOff();

  skipOverHeader(imageFile);
  saveOOPTable(imageFile);

#ifdef OOP_TABLE_TRACE
printf("After saving oopt table: %d\n", ftell(imageFile));
#endif /* OOP_TABLE_TRACE */

  saveGlobalOOPs(imageFile);

#ifdef OOP_TABLE_TRACE
printf("After global oop table: %d\n", ftell(imageFile));
#endif /* OOP_TABLE_TRACE */

  objectDataSize = saveAllObjects(imageFile);

#ifdef OOP_TABLE_TRACE
printf("After saving all objects table: %d\n", ftell(imageFile));
#endif /* OOP_TABLE_TRACE */

  skipToHeader(imageFile);
  saveFileVersion(imageFile, objectDataSize);

  fclose(imageFile);

  setGCState(oldGCState);
  return (true);
} 


static void
skipOverHeader(imageFile)
     FILE	*imageFile;
{
  fseek(imageFile, sizeof(SaveFileHeader), fromBeginning);
}

/*
 *	static void saveOOPTable(imageFile)
 *
 * Description
 *
 *	Writes the OOP table out to the image file.  We need to make all
 *	of the object pointers relative, including free OOP table slots, and
 *	we use a parallel vector containing file offsets for the objects that
 *	we developed during saving of the objects themselves as the fixup
 *	table.
 *
 * Inputs
 *
 *	imageFile: 
 *		A stdio FILE to be written to.  It must be positioned
 *		correctly before this routine is called.
 *
 */
static void
saveOOPTable(imageFile)
     FILE	*imageFile;
{
  int		i;
  OOP		oop;

  maxUsedOOPSlot = 0;

  for (i = 0, oop = oopTable; i < TOTAL_OOP_TABLE_SLOTS; i++, oop++) {
    addByteFlagToOOP(oop);
  }
  for (i = 0, oop = oopTable; i < TOTAL_OOP_TABLE_SLOTS; i++, oop++) {
    fixupOOP(i, oop);
  }

#ifdef OOP_TABLE_TRACE
printf("there are %d free oops out of %d oops, leaving %d\n",
       numFreeOOPs, OOP_TABLE_SIZE, OOP_TABLE_SIZE - numFreeOOPs);
printf("max used is %d\n", maxUsedOOPSlot);
#endif /* OOP_TABLE_TRACE */

  /* save up to the max oop slot in use */
  fwrite(oopTable, sizeof(struct OOPStruct), maxUsedOOPSlot + 1, imageFile);

#ifdef actually_not_needed
/**/  /* then save the constant ones at the end */
/**/  fwrite(&oopTable[OOP_TABLE_SIZE], sizeof(struct OOPStruct),
/**/	 TOTAL_OOP_TABLE_SLOTS - OOP_TABLE_SIZE, imageFile);
#endif

  restoreAllOOPs();
}



static void
addByteFlagToOOP(oop)
     OOP	oop;
{
  if (oopValid(oop)) {
    if ((oopInstanceSpec(oop) & ISP_INDEXEDVARS) == ISP_ISINDEXABLE) {
      oop->flags |= F_BYTE;
    }
  }
}

static void
fixupOOP(i, oop)
     int	i;
     OOP	oop;
{
  if (oopValid(oop)) {
    if (i < OOP_TABLE_SIZE) {
      maxUsedOOPSlot = i;
    }
    oop->object = (mst_Object)toRelative(oop->object);
  }
}


static void
restoreAllOOPs()
{
  int		i;
  OOP		oop;

  for (i = 0, oop = oopTable; i < TOTAL_OOP_TABLE_SLOTS; i++, oop++) {
    if (oopValid(oop)) {
      oop->flags &= ~F_BYTE;
      oop->object = (mst_Object)fromRelative(oop->object);
    }
  }
}

static void
saveGlobalOOPs(imageFile)
     FILE	*imageFile;
{
  OOP		**oopPtr, oop;

  for (oopPtr = globalOOPs; *oopPtr; oopPtr++) {
    oop = toRelative(**oopPtr);
    fwrite(&oop, sizeof(OOP), 1, imageFile);
  }
}


static int
saveAllObjects(imageFile)
     FILE	*imageFile;
{
  long		objectStart, objectEnd;
  int		i;
  OOP		oop;

  objectStart = ftell(imageFile);
  for (i = 0, oop = oopTable; i < OOP_TABLE_SIZE; i++, oop++) {
    if (oopValid(oop)) {
      saveObject(imageFile, oop);
    }
  }

  objectEnd = ftell(imageFile);

#ifdef actually_not_needed
/**/  /* dump out the character objects, nil, true, and false */
/**/  for (i = OOP_TABLE_SIZE; i < TOTAL_OOP_TABLE_SLOTS; i++) {
/**/    saveObject(imageFile, oopAt(i));
/**/  }
#endif

  return (objectEnd - objectStart);
}

static void
saveObject(imageFile, oop)
     FILE	*imageFile;
     OOP	oop;
{
  mst_Object	object;
  int		numFixed;

  object = oopToObj(oop);
  numFixed = numOOPs(object);

  fixupObject(oop, numFixed);
  fwrite(object, sizeof(OOP), object->objSize, imageFile);
  restoreObject(oop, numFixed);
}



static void
fixupObject(oop, numFixed)
     OOP	oop;
     int	numFixed;
{
  OOP		instOOP, classOOP, *i;

  classOOP = oopClass(oop);
  oopToObj(oop)->objClass = toRelative(classOOP);

  i = oopToObj(oop)->data;
  while(numFixed) {
    instOOP = *i;
    if (isOOP(instOOP)) {
      *i = toRelative(instOOP);
    }
    i++;
    numFixed--;
  }
}

static void
restoreObject(oop, numFixed)
     OOP	oop;
     int	numFixed;
{
  mst_Object	object;

  object = oopToObj(oop);
  object->objClass = fromRelative(object->objClass);

  restoreInstanceVars(oop, numFixed);
}

static void
restoreInstanceVars(oop, numFixed)
     OOP	oop;
     int	numFixed;
{
  OOP		instOOP, classOOP, *i;

  classOOP = oopClass(oop);
  i = oopToObj(oop)->data;
  while(numFixed) {
    instOOP = *i;
    if (isOOP(instOOP)) {
      *i = fromRelative(instOOP);
    }
    i++;
    numFixed--;
  }
  if (classOOP == cFuncDescriptorClass) {
    restoreCFuncDescriptor(oop); /* in mstcint.c */
  }
}


static void
skipToHeader(imageFile)
     FILE	*imageFile;
{
  rewind(imageFile);
}

static void
saveFileVersion(imageFile, objectDataSize)
     FILE	*imageFile;
     unsigned long objectDataSize;
{
  SaveFileHeader header;

  memcpy(header.signature, "GSTIm\0\0", 7);
  header.flags = FLAGS_WRITTEN;
  header.version = VERSION_REQUIRED;
  header.objectDataSize = objectDataSize;
  header.oopTableSize = maxUsedOOPSlot + 1; /* n slots, numbered 0..n-1 */
  header.semiSpaceSize = maxSpaceSize;

  fwrite(&header, sizeof(SaveFileHeader), 1, imageFile);
}



/***********************************************************************
 *
 *	Binary loading routines.
 *
 ***********************************************************************/


mst_Boolean
loadFromFile(fileName)
     char	*fileName;
{
  FILE		*imageFile;
  SaveFileHeader header;
  mst_Boolean	oldGCState;

  oldGCState = gcOff();

  imageFile = openFile(fileName, "r");
  if (imageFile == NULL) {
bad:
    fclose(imageFile);
    setGCState(oldGCState);
    return (false);
  }

  loadFileVersion(imageFile, &header);
  if (strcmp(header.signature, "GSTIm\0\0")) goto bad;

  if (wrongEndianness = flagChanged(header.flags, ENDIANNESS_FLAG)) {
    header.objectDataSize = byteInvert(header.objectDataSize);
    header.oopTableSize = byteInvert(header.oopTableSize);
    header.semiSpaceSize = byteInvert(header.semiSpaceSize);
    header.version = byteInvert(header.version);
  }

  /* different sizeof(long) not supported */
  if (flagChanged(header.flags, LONG_SIZE_FLAG)) goto bad;

  /* check for version mismatch; if so this image file is invalid */
  if (header.version != VERSION_REQUIRED) goto bad;
  
  if (header.semiSpaceSize > maxSpaceSize) {
    growMemoryTo(header.semiSpaceSize);
  }

  initOOPTable();
  loadOOPTable(imageFile, header.oopTableSize);

#ifdef OOP_TABLE_TRACE
printf("After loading oopt table: %d\n", ftell(imageFile));
#endif /* OOP_TABLE_TRACE */

  loadGlobalOOPs(imageFile);

#ifdef OOP_TABLE_TRACE
printf("After loading global oop table: %d\n", ftell(imageFile));
#endif /* OOP_TABLE_TRACE */

  loadNormalOOPs(imageFile, header.objectDataSize);
#ifdef actually_not_needed
/**/  loadCharOOPs(imageFile);
/**/  loadSpecialOOPs(imageFile);
#endif

#ifdef OOP_TABLE_TRACE
printf("After loading objects: %d\n", ftell(imageFile));
#endif /* OOP_TABLE_TRACE */

  fixupInstanceVars();
  initBuiltinObjectsClasses();
  refreshOOPFreeList();

  fclose(imageFile);

  setGCState(oldGCState);

  return (true);
}

static void
loadFileVersion(imageFile, headerp)
     FILE	*imageFile;
     SaveFileHeader *headerp;
{
  fread(headerp, sizeof(SaveFileHeader), 1, imageFile);
}

static void
loadOOPTable(imageFile, oldSlotsUsed)
     FILE	*imageFile;
     long 	oldSlotsUsed;
{
  long		i;
  OOP		oop;

  /* load in the valid OOP slots from previous dump */
  my_fread(oopTable, sizeof(struct OOPStruct), oldSlotsUsed, imageFile);
  
  /* mark the remaining ones as available (not a for because of a probable
     bug in GNU C!) */
  i = oldSlotsUsed;
  oop = &oopTable[oldSlotsUsed];
  while(i++ < OOP_TABLE_SIZE) {
    oop->flags = F_FREE;
    oop++;
  }

#ifdef actually_not_needed
/**/  /* read in the constant stuff at the end */
/**/  my_fread(&oopTable[OOP_TABLE_SIZE], sizeof(struct OOPStruct),
/**/	TOTAL_OOP_TABLE_SLOTS - OOP_TABLE_SIZE, imageFile);
#endif

  /* the fixup gets handled by load normal oops */
}


static void
loadGlobalOOPs(imageFile)
     FILE	*imageFile;
{
  OOP		**oopPtr, *oopVec;
  long		numGlobalOOPs;

  /* !!! a) this is clumsy, and b) this is incorrect -- if the number of
   * global oops changes, I think we're hosed, and this code does nothing
   * to detect or prevent this from happening. */
  for (numGlobalOOPs = 0, oopPtr = globalOOPs; *oopPtr;
       oopPtr++, numGlobalOOPs++);

  /* using alloca, doing one fread, and then iterating through the array
     speeds up the thing. */
  oopVec = (OOP *)alloca(numGlobalOOPs * sizeof(OOP));
  my_fread(oopVec, sizeof(OOP), numGlobalOOPs, imageFile);

  for (oopPtr = globalOOPs; *oopPtr; oopPtr++, oopVec++) {
    **oopPtr = fromRelative(*oopVec);
  }
#ifdef old_code /* Wed Apr 26 21:15:38 1989 */ /* <<<== what dedication...3 days before my wedding -- SBB */
/**/  for (oopPtr = globalOOPs; *oopPtr; oopPtr++) {
/**/    my_fread(&oop, sizeof(OOP), 1, imageFile);
/**/    **oopPtr = fromRelative(oop);
/**/  }
#endif /* old_code Wed Apr 26 21:15:38 1989 */
}

static void
loadNormalOOPs(imageFile, objectDataSize)
     FILE	*imageFile;
     unsigned long objectDataSize;
{
  register long	i;
  OOP		oop, prevFreeOOP;
  mst_Object	object, objPtr;

  objPtr = curSpaceAddr();

  prevFreeOOP = nil;

  /* Not everything must be byte-inverted in this part of the file,
     so read everything through good old fread and fix later. */
  fread(objPtr, 1, objectDataSize, imageFile);

  numFreeOOPs = 0;
  for (i = 0, oop = oopTable; i < OOP_TABLE_SIZE; i++, oop++) {
    if (oopValid(oop)) {
      object = objPtr;
      oop->object = object;
      if (wrongEndianness && !(oop->flags & F_BYTE)) {
        fixupByteOrder(object, byteInvert(object->objSize));
      }
      object->objClass = fromRelative(object->objClass);
      objPtr = (mst_Object)((long *)object + object->objSize);
    } else {
      numFreeOOPs++;
      oop->flags = F_FREE;	/* just free */
    }
  }

  /* numOOPs requires access to the instance spec in the class objects. So
     we start by fixing the endianness of ALL the non-byte objects (including
     classes!), for which we can do without numOOPs, then do another pass
     and fix the byte objects using the now correct class objects. */
  if (wrongEndianness) {
    for (i = 0, oop = oopTable; i < OOP_TABLE_SIZE; i++, oop++) {
      /* The cycle above ensures that free OOPs don't have F_BYTE set. */
      if (oop->flags & F_BYTE) {
        object = oopToObj(oop);
        fixupByteOrder(object, 2+numOOPs(object));
        oop->flags &= ~F_BYTE;
      }
    }
  }

  setSpaceInfo(objectDataSize);
}



#ifdef actually_not_needed
/**/static void loadCharOOPs(imageFile)
/**/FILE	*imageFile;
/**/{
/**/  int		i;
/**/
/**/  fread(charObjectTable, sizeof(CharObject), NUM_CHAR_OBJECTS, imageFile);
/**/
/**/  /* ### Inconsistency here... using direct refs to oopTable, instead of oopAts*/
/**/  /* like above.  Probably should convert the whole thing to pointers into*/
/**/  /* the oopTable and be done with it.*/
/**/
/**/  for (i = 0; i < NUM_CHAR_OBJECTS; i++) {
/**/    oopTable[i + CHAR_OBJECT_BASE].object = (mst_Object)&charObjectTable[i];
/**/    charObjectTable[i].objClass = fromRelative(charObjectTable[i].objClass);
/**/  }
/**/}
/**/
/**/static void loadSpecialOOPs(imageFile)
/**/FILE	*imageFile;
/**/{
/**/  fread(&nilObject, sizeof(struct NilObjectStruct), 1, imageFile);
/**/  nilOOP->object = (mst_Object)&nilObject;
/**/  nilOOP->object->objClass = fromRelative(nilOOP->object->objClass);
/**/  /* ### not sure that I can just assign directly into flags...seems ok, though*/
/**/  nilOOP->flags = 0;
/**/
/**/  fread(booleanObjects, sizeof(struct BooleanObjectStruct), 2, imageFile);
/**/  trueOOP->object = (mst_Object)&booleanObjects[0];
/**/  falseOOP->object = (mst_Object)&booleanObjects[1];
/**/  trueOOP->object->objClass = fromRelative(trueOOP->object->objClass);
/**/  falseOOP->object->objClass = fromRelative(falseOOP->object->objClass);
/**/  trueOOP->flags = falseOOP->flags = 0;
/**/}
#endif /* actually_not_needed */

static void
fixupInstanceVars()
{
  int		i;
  OOP		oop;

  for (i = 0, oop = oopTable; i < OOP_TABLE_SIZE; i++, oop++) {
    if (oopValid(oop)) {
      fixupOOPInstanceVars(oop);
    }
  }
}

static void
fixupOOPInstanceVars(oop)
     OOP	oop;
{
  int		numFixed;
  mst_Object	object;

  object = oopToObj(oop);
  numFixed = numOOPs(object);
  restoreInstanceVars(oop, numFixed);
}

static void
fixupByteOrder(p, size)
     unsigned long int	*p;
     int		size;
{
  for (; size--; p++) {
    *p = byteInvert(*p);
  }
}

static int
my_fread(p, size, n, file)
     unsigned long int	*p;
     int		size;
     int		n;
     FILE		*file;
{
  int result;

  result = fread(p, size, n, file);
  if (wrongEndianness) {
    fixupByteOrder(p, result / sizeof(long));
  }
  return (result);
}
