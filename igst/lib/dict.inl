/* this is -*- C -*- ok?!? */

/***********************************************************************
 *
 *	Dictionary Support Module Inlines.
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


#ifndef __GSTDICT_INLINES__
#define  __GSTDICT_INLINES__

#if STDC_HEADERS
#include <string.h>
#include <stdlib.h>
#endif /* STDC_HEADERS */

static inline mst_Boolean isAKindOf(), indexOOPPut(), instVarAtPut();

static inline int 	  instanceSize(), oopNumFields();

static inline void 	  nilFill(), indexStringOOPPut();

static inline mst_Object  newInstanceWith(), newInstance(),
	      		  instantiateWith(), instantiate();

static inline OOP	  instantiateOOPWith(), findClassMethod(),
			  dictionaryAdd(), dictionaryAssociationAt(),
			  dictionaryAt(), associationNew(), indexOOP(),
			  indexStringOOP(), floatNew();



#define stringOOPChars(stringOOP) \
  ((Byte *)((String)oopToObj(stringOOP))->chars)

#define messageSelector(messageOOP) \
  (((Message)oopToObj(messageOOP))->selector)

#define messageArgs(messageOOP) \
  (((Message)oopToObj(messageOOP))->args)

#define cObjectNew(cObjPtr) \
  (cObjectNewTyped(cObjPtr, cObjectTypeCType))

#define cObjectValueObj(cObj) \
  ( ((voidPtr *) cObj) [((mst_Object)cObj)->objSize - 1])

#define setCObjectValueObj(cObj, value) \
  ( ((voidPtr *) cObj) [((mst_Object)cObj)->objSize - 1] = (voidPtr)(value))

#define cObjectValue(cObjOOP) \
  cObjectValueObj(oopToObj(cObjOOP))

#define setCObjectValue(cObjOOP, value) \
  setCObjectValueObj(oopToObj(cObjOOP), value)

#define superClass(classOOP) \
  (((Class)oopToObj(classOOP))->superClass)

#define oopFixedFields(oop) \
  (oopInstanceSpec(oop) >> ISP_NUMFIXEDFIELDS)

#define numIndexableFields(oop) \
	(isInt(oop) ? 0 : oopNumFields(oop) - oopFixedFields(oop))

#define arrayNew(numElts) \
	(allocOOP(instantiateWith(arrayClass, (numElts))))

#define arrayAt(arrayOOP, index) \
	( ((Array)oopToObj(arrayOOP))->elements[(index)-1])

#define arrayAtPut(arrayOOP, index, value) \
	((Array)oopToObj(arrayOOP))->elements[index-1] = value

#define dictionarySize(dictionaryOOP) \
  (toInt(((Dictionary)oopToObj(dictionaryOOP))->tally))

#define dictionaryAtPut(dictionaryOOP, keyOOP, valueOOP) \
  (dictionaryAdd(dictionaryOOP, associationNew(keyOOP, valueOOP)))

#define isAMetaclass(oop) \
  (isOOP(oop) && oopClass(oop) == metaclassClass)

#define isAClass(oop) \
  (isOOP(oop) && oopClass(oopClass(oop)) == metaclassClass)

#define metaclassInstance(metaclassOOP) \
  (((Metaclass)oopToObj(metaclassOOP))->instanceClass)

#define classMethodDictionary(classOOP) \
  return (((Class)oopToObj(classOOP))->methodDictionary);

#define associationValue(associationOOP) \
  (((Association)oopToObj(associationOOP))->value)

#define setAssociationValue(associationOOP, valueOOP) \
  (((Association)oopToObj(associationOOP))->value = valueOOP)

#define classInstanceSpec(classOOP) \
  (((Class)oopToObj(classOOP))->instanceSpec)

#define getInstanceSpec(obj) \
  classInstanceSpec((obj)->objClass)

#define oopInstanceSpec(oop) \
  classInstanceSpec(oopClass(oop))

#define checkIndexableBoundsOf(oop, index) \
  (isOOP(oop) && (index >= 1 && index <= numIndexableFields(oop)))

#define checkBoundsOf(oop, index) \
  (isOOP(oop) && (index >= 1 && index <= oopNumFields(oop)))

#define classIsPointers(classOOP) \
  (classInstanceSpec(classOOP) & ISP_ISPOINTERS)

#define isPointers(oop) \
  (oopInstanceSpec(oop) & ISP_ISPOINTERS)

#define classIsIndexable(classOOP) \
  (classInstanceSpec(classOOP) & ISP_ISINDEXABLE)

#define oopSizeBytes(oop) \
  ((oopToObj(oop)->objSize << LONG_SHIFT) - sizeof(ObjectHeader))

/* return the number of availble longwords in object, excluding the header */
#define numOOPs(obj) \
  ((long) ((classInstanceSpec(obj->objClass) & ISP_ISPOINTERS) \
    ? (1 + (obj)->objSize - sizeof(struct ObjectStruct) / sizeof(OOP)) \
    : (classInstanceSpec(obj->objClass) >> ISP_NUMFIXEDFIELDS) \
  ))



#if (DOUBLE_ALIGNMENT <= SIZEOF_CHAR_P)
#define floatOOPValue(floatOOP) \
	(((FloatObject)oopToObj(floatOOP))->value)

#else
static inline double
floatOOPValue(floatOOP)
OOP	floatOOP;
{
  register mst_Object obj;
  union {
    unsigned long l[2];
    double d;
  } hack;

  /* we may not be aligned properly...fetch things out the hard way */
  obj = oopToObj(floatOOP);
  hack.l[0] = (unsigned long)obj->data[0];
  hack.l[1] = (unsigned long)obj->data[1];
  return (hack.d);
}
#endif



#include "sym.h"
#include "interp.h"
#include "oop.h"
#include "lex.h"

/*
 *	mst_Boolean isAKindOf(memberOOP, classOOP)
 *
 * Description
 *
 *	Checks to see if "memberOOP" is a subclass of "classOOP", returning
 *	true if it is.
 *
 * Inputs
 *
 *	memberOOP: 
 *		A class OOP that's to be checked for (sub)class membership.
 *	classOOP: 
 *		A class OOP that's the conjectured (super)class.
 *
 * Outputs
 *
 *	True if "memberOOP" is a (sub)class of "classOOP".
 */
static inline mst_Boolean
isAKindOf (memberOOP, classOOP)
     OOP	memberOOP, classOOP;
{
  for ( ; !isNil(memberOOP); memberOOP = superClass(memberOOP)) {
    if (memberOOP == classOOP) {
      return (true);
    }
  }
  
  return (false);
}

static inline int
instanceSize (classOOP)
     OOP	classOOP;
{
  register int		numBytes;
  register long        	instanceSpec;

  instanceSpec = classInstanceSpec(classOOP);
  numBytes = instanceSpec >> ISP_NUMFIXEDFIELDS;

  /* Fixed instance vars are always pointers */
  numBytes <<= LONG_SHIFT;

  return (numBytes + sizeof(ObjectHeader));
}



static inline void
nilFill (oopPtr, oopCount)
     register OOP	*oopPtr;
     register long 	oopCount;
{
  extern OOP nilVec[100];
  for (; oopCount >= 100; oopPtr += 100, oopCount -= 100) {
    memcpy(oopPtr, nilVec,  100*sizeof(OOP));
  }
  memcpy(oopPtr, nilVec,  oopCount*sizeof(OOP));
}


static inline mst_Object
newInstanceWith (classOOP, numIndexFields)
     OOP	classOOP;
     long	numIndexFields;
{
  mst_Object	instance;
  register int	numBytes;
  register long	instanceSpec;

  numBytes = instanceSize(classOOP);
  instanceSpec = classInstanceSpec(classOOP);
  if (instanceSpec & (ISP_ISPOINTERS | ISP_ISWORDS)) {
    numIndexFields = size2Bytes(numIndexFields);
  }
  numBytes += numIndexFields;
  numBytes = size2Bytes(ROUNDED_WORDS(numBytes));
  instance = (mst_Object)allocObj(numBytes);
  instance->objSize = numBytes >> LONG_SHIFT;
  instance->objClass = classOOP;
  return (instance);
}


/*
 *	mst_Object newInstance(classOOP)
 *
 * Description
 *
 *	Creates a new instance of class "classOOP".  The space is allocated,
 *	the class and size fields of the class are filled in, and the instance
 *	is returned.  Its fields are NOT INITIALIZED.  "classOOP" must
 *	represent a class with no indexable fields.
 *
 * Inputs
 *
 *	classOOP: 
 *		OOP for the class that the new instance is to be an instance
 *		of.
 *
 * Outputs
 *
 *	The new instance, with objSize and objClass filled in.
 */
static inline mst_Object
newInstance (classOOP)
     register OOP	classOOP;
{
  register mst_Object	instance;
  int			numBytes;

  numBytes = instanceSize(classOOP);
  instance = (mst_Object)allocObj(numBytes);
  instance->objSize = bytes2Size(numBytes);
  instance->objClass = classOOP;
  return (instance);
}


/*
 *	mst_Object instantiateWith(classOOP, numIndexFields)
 *
 * Description
 *
 *	Returns a new, initialized instance with indexable fields.  If the
 *	instance contains pointers, they are initialized to nilOOP, else they
 *	are set to real zero.
 *
 * Inputs
 *
 *	classOOP: 
 *		Class to make an instance of.  An OOP.
 *	numIndexFields: 
 *		The number if indexed instance variables this instance is to
 *		have, possibly zero.  A long.
 *
 * Outputs
 *
 *	New instance with initialized, indexed instance variables.
 */
static inline mst_Object
instantiateWith (classOOP, numIndexFields)
     OOP	classOOP;
     long	numIndexFields;
{
  register mst_Object	instance;
  register long		instanceSpec;
  long			numBytes;
  
  instance = newInstanceWith(classOOP, numIndexFields);
  instanceSpec = classInstanceSpec(classOOP);
  nilFill(instance->data, instanceSpec >> ISP_NUMFIXEDFIELDS);
  if (instanceSpec & ISP_ISPOINTERS) {
    nilFill(&instance->data[instanceSpec >> ISP_NUMFIXEDFIELDS], numIndexFields);
  } else {
    numBytes = numIndexFields;
    if (instanceSpec & ISP_ISWORDS) {
      numBytes = size2Bytes(numBytes);
    }
    /* not strictly necessary -- a convenience */
    memset(&instance->data[instanceSpec >> ISP_NUMFIXEDFIELDS], '\0', numBytes);
  }
  return (instance);
}


/*
 *	OOP instantiateOOPWith(classOOP, numIndexFields)
 *
 * Description
 *
 *	Returns an OOP for a newly allocated instance of "classOOP", with
 *	"numIndexFields" fields.  The OOP is adjusted to reflect any
 *	variance in size (such as a string that's shorter than a word boundary.
 *
 * Inputs
 *
 *	classOOP: 
 *		An OOP for the class to create the instance of.
 *	numIndexFields: 
 *		The number of index fields to create in the instance.  Must be
 *		>= 0.
 *
 * Outputs
 *
 *	A new OOP that holds the newly allocated instance, with possible
 *	correction for size.
 */
static inline OOP
instantiateOOPWith (classOOP, numIndexFields)
     OOP	classOOP;
     long	numIndexFields;
{
  register mst_Object	object;
  register OOP		oop;
  long			instanceSpec;

  object = instantiateWith(classOOP, numIndexFields);
  oop = allocOOP(object);
  instanceSpec = classInstanceSpec(classOOP);
  if (!(instanceSpec & (ISP_ISWORDS | ISP_ISPOINTERS))) {
    initEmptyBytes(oop, numIndexFields);
  }

  return (oop);
}

/*
 *	mst_Object instantiate(classOOP)
 *
 * Description
 *
 *	Create and return a new instance of class "classOOP".  "classOOP" must
 *	be a class with no indexable fields.  The named instance variables of
 *	the new instance are initialized to nilObj, since fixed-field-only 
 *	objects can only have pointers.
 *
 * Inputs
 *
 *	classOOP: 
 *		An OOP for the class to create the instance of.
 *
 * Outputs
 *
 *	The new instance, with its fields initialized.
 */
static inline mst_Object
instantiate (classOOP)
     OOP	classOOP;
{
  register mst_Object	instance;
  register long		instanceSpec;

  instance = newInstance(classOOP);
  instanceSpec = classInstanceSpec(classOOP);
  if (!(instanceSpec & ISP_ISPOINTERS)) {
    errorf("Class with non-pointer instance spec passed to instantiate");
  }

  nilFill(instance->data, instanceSpec >> ISP_NUMFIXEDFIELDS);
  return (instance);
}


static int
findKey (dictionaryOOP, keyOOP)
     OOP	dictionaryOOP, keyOOP;
{
  register long		index;
  register Dictionary	dictionary; 
  long			initindex, numFields;
  OOP			associationOOP;
  Association		association;

  dictionary = (Dictionary)oopToObj(dictionaryOOP);
  numFields = numOOPs(dictionary) - 1;
  index = hash(keyOOP);
  index %= numFields;
  initindex = index;

  do {
    if (isNil(dictionary->assoc[index])) {
      return (index);
    }

    associationOOP = dictionary->assoc[index];
    association = (Association)oopToObj(associationOOP);

    if (equal(association->key, keyOOP)) {
      return (index);
    }
    /* linear reprobe -- it is simple and guaranteed */
    if (++index == numFields) {
      index = 0;
    }
  } while (index != initindex);

  return(-1);
}


static int
findKeyOrNil (dictionaryOOP, keyOOP)
     OOP	dictionaryOOP, keyOOP;
{
  long			count, numFields;
  register long		index;
  register Dictionary	dictionary; 
  OOP			associationOOP;
  Association		association;

  dictionary = (Dictionary)oopToObj(dictionaryOOP);
  numFields = numOOPs(dictionary) - 1;
  index = hash(keyOOP);
  index %= numFields;
  count = numFields;

  for ( ; count; count--) {
    if (isNil(dictionary->assoc[index])) {
      return (index);
    }

    associationOOP = dictionary->assoc[index];
    association = (Association)oopToObj(associationOOP);

    if (equal(association->key, keyOOP)) {
      return (index);
    }

    /* linear reprobe -- it is simple and guaranteed */
    if (++index == numFields) {
      index = 0;
    }
  }
  errorf("Error - searching dictionary for nil, but it is full!\n");
  exit(1);
}

static inline int
identityDictionaryFindKeyOrNil (identityDictionaryOOP, keyOOP)
     OOP	identityDictionaryOOP, keyOOP;
{
  register IdentityDictionary	identityDictionary;
  register long			index;
  long				count;
  long				numFields;
  
  identityDictionary = (IdentityDictionary)oopToObj(identityDictionaryOOP);
  for ( ; ; ) {
    numFields = numOOPs(identityDictionary) - 2;
    index = hash(keyOOP);
    index %= numFields;
    count = numFields;

    for ( ; count; count--) {
      if (isNil(identityDictionary->keys[index])) {
	return (index);
      }

      if (identityDictionary->keys[index] == keyOOP) {
	return (index);
      }

      /* linear reprobe -- it is simple and guaranteed */
      if (++index == numFields) {
        index = 0;
      }
    }

    /*
     * If we get to here, the dictionary is full, but we haven't found
     * the element that we're looking for.  Since we either return the
     * index of the element being sought, or the index of a nil element,
     * and the dictionary was full so that there was no nil element, we
     * grow the dictionary and scan it again.  We're guaranteed to exit
     * this loop via a return after at most two iterations.
     */
    identityDictionary = growIdentityDictionary(identityDictionaryOOP);
  }
}


static inline OOP
findClassMethod (classOOP, selector)
     OOP	classOOP, selector;
{
  Class			class;
  IdentityDictionary	methodDictionary;
  register OOP		methodDictionaryOOP;
  register int		index;

  class = (Class)oopToObj(classOOP);
  methodDictionaryOOP = class->methodDictionary;
  if (isNil(methodDictionaryOOP)) {
    return (nilOOP);
  }

  index = identityDictionaryFindKeyOrNil(methodDictionaryOOP, selector);
  methodDictionary = (IdentityDictionary)oopToObj(methodDictionaryOOP);

  return (arrayAt(methodDictionary->values, index+1));
}


static OOP
dictionaryAdd (dictionaryOOP, associationOOP)
     OOP	dictionaryOOP,  associationOOP;
{
  register long		index;
  Association		association;
  register Dictionary	dictionary;
  OOP			value;
  IncPtr		incPtr;		/* I'm not sure clients are protecting
					   association OOP */

  incPtr = incSavePointer();
  incAddOOP(associationOOP);

  association = (Association)oopToObj(associationOOP);
  dictionary = (Dictionary)oopToObj(dictionaryOOP);
  if (toInt(dictionary->tally) >= numOOPs(dictionary)-1) {
    dictionary = growDictionary(dictionaryOOP);
  }

  index = findKeyOrNil(dictionaryOOP, association->key);
  if (isNil(dictionary->assoc[index])) {
    dictionary->tally = incrInt(dictionary->tally);
    dictionary->assoc[index] = associationOOP;
  } else {
    value = associationValue(associationOOP);
    associationOOP = dictionary->assoc[index];
    setAssociationValue(associationOOP, value);
  }

  incRestorePointer(incPtr);
  return (associationOOP);
}


static inline OOP
dictionaryAssociationAt (dictionaryOOP, keyOOP)
     OOP	dictionaryOOP, keyOOP;
{
  register long		index;
  register Dictionary	dictionary; 

  if (isNil(dictionaryOOP)) {
    return (nilOOP);
  }

  index = findKey(dictionaryOOP, keyOOP);
  if (index == -1) {
    return (nilOOP);
  }
  dictionary = (Dictionary)oopToObj(dictionaryOOP);

  return (dictionary->assoc[index]);
}

static inline OOP
dictionaryAt (dictionaryOOP, keyOOP)
     OOP	dictionaryOOP, keyOOP;
{
  OOP		assocOOP;

  assocOOP = dictionaryAssociationAt(dictionaryOOP, keyOOP);

  if (isNil(assocOOP)) {
    return (nilOOP);
  } else {
    return (associationValue(assocOOP));
  }
}

static inline OOP
associationNew (key, value)
     OOP	key, value;
{
  Association	association;

  association = (Association)newInstance(associationClass);
  association->key = key;
  association->value = value;

  return (allocOOP(association));
}


static inline int
oopNumFields (oop)
     OOP	oop;
{
  register mst_Object	object;
  register long		instanceSpec;

  object = oopToObj(oop);
  instanceSpec = getInstanceSpec(object);

  if (instanceSpec & (ISP_ISPOINTERS | ISP_ISWORDS)) {
    return ((size2Bytes(object->objSize) - sizeof(ObjectHeader)) >> LONG_SHIFT);
  } else {			/* must be bytes */
    return (
      size2Bytes(object->objSize - (instanceSpec >> ISP_NUMFIXEDFIELDS)) -
      sizeof(ObjectHeader) -
      (oop->flags & EMPTY_BYTES) +
      (instanceSpec >> ISP_NUMFIXEDFIELDS));
  }
}

static inline OOP
indexOOP (oop, index)
     OOP	oop;
     int	index;
{
  register long		instanceSpec;
  register mst_Object	object;

  object = oopToObj(oop);
  instanceSpec = getInstanceSpec(object);

  if (instanceSpec & ISP_ISPOINTERS) {
    index += instanceSpec >> ISP_NUMFIXEDFIELDS;
    return (object->data[index-1]);
  } else if (instanceSpec & ISP_ISWORDS) {
    index += instanceSpec >> ISP_NUMFIXEDFIELDS;
    return (fromInt( ((long *)object->data)[index-1] ));
  } else {
    index += (instanceSpec >> ISP_NUMFIXEDFIELDS) * sizeof(OOP);
    return (fromInt( ((Byte *)object->data)[index-1] ));
  }
}

static inline mst_Boolean
indexOOPPut (oop, index, value)
     OOP	oop, value;
     int	index;
{
  register long		instanceSpec;
  register mst_Object	object;

  object = oopToObj(oop);
  instanceSpec = getInstanceSpec(object);

  if (instanceSpec & ISP_ISPOINTERS) {
    index += instanceSpec >> ISP_NUMFIXEDFIELDS;
    object->data[index-1] = value;
  } else if (instanceSpec & ISP_ISWORDS) {
    index += instanceSpec >> ISP_NUMFIXEDFIELDS;
    ((long *)object->data)[index-1] = toInt(value);
  } else {
    index += (instanceSpec >> ISP_NUMFIXEDFIELDS) * sizeof(OOP);
    if (toInt(value) >= 256) {
      return (false);
    }
    ((Byte *)object->data)[index-1] = (Byte)toInt(value);
  }

  return (true);
}

static inline OOP
indexStringOOP (oop, index)
     OOP	oop;
     int	index;
{
  register long		instanceSpec;
  register mst_Object	object;

  object = oopToObj(oop);
  instanceSpec = getInstanceSpec(object);
  index += (instanceSpec >> ISP_NUMFIXEDFIELDS) * sizeof(OOP);

  return (charOOPAt( ((Byte *)object->data)[index-1] ));
}

static inline void
indexStringOOPPut (oop, index, value)
     OOP	oop, value;
     int	index;
{
  register long		instanceSpec;
  register mst_Object	object;

  object = oopToObj(oop);
  instanceSpec = getInstanceSpec(object);
  index += (instanceSpec >> ISP_NUMFIXEDFIELDS) * sizeof(OOP);

  ((Byte *)object->data)[index-1] = charOOPValue(value);
}

static inline OOP
instVarAt (oop, index)
     OOP	oop;
     int	index;
{
  register long		instanceSpec;
  register mst_Object	object;

  object = oopToObj(oop);
  instanceSpec = getInstanceSpec(object);
  if (index <= (instanceSpec >> ISP_NUMFIXEDFIELDS)) {
    /* Fixed instance vars are always pointers */
    instanceSpec |= ISP_ISPOINTERS;
  }

  if (instanceSpec & ISP_ISPOINTERS) {
    return (object->data[index-1]);
  } else if (instanceSpec & ISP_ISWORDS) {
    return (fromInt( ((long *)object->data)[index-1] ));
  } else {
    /* Adjust for the extra space took by fixed fields */
    index -= instanceSpec >> ISP_NUMFIXEDFIELDS;
    index += (instanceSpec >> ISP_NUMFIXEDFIELDS) * sizeof(OOP);
    return (fromInt( ((Byte *)object->data)[index-1] ));
  }
}

static inline mst_Boolean
instVarAtPut (oop, index, value)
     OOP	oop, value;
     int	index;
{
  register long		instanceSpec;
  register mst_Object	object;

  object = oopToObj(oop);
  instanceSpec = getInstanceSpec(object);
  if (index <= (instanceSpec >> ISP_NUMFIXEDFIELDS)) {
    /* Fixed instance vars are always pointers */
    instanceSpec |= ISP_ISPOINTERS;
  }

  if (instanceSpec & ISP_ISPOINTERS) {
    object->data[index-1] = value;
  } else if (instanceSpec & ISP_ISWORDS) {
    ((long *)object->data)[index-1] = toInt(value);
  } else {
    if (toInt(value) >= 256) {
      return (false);
    }
    /* Adjust for the extra space took by fixed fields */
    index -= instanceSpec >> ISP_NUMFIXEDFIELDS;
    index += (instanceSpec >> ISP_NUMFIXEDFIELDS) * sizeof(OOP);
    ((Byte *)object->data)[index-1] = (Byte)toInt(value);
  }

  return (true);
}


static inline OOP
floatNew (f)
     double	f;
{
  FloatObject	floatObject;

  floatObject = (FloatObject)newInstanceWith(floatClass, sizeof(double));

#if (DOUBLE_ALIGNMENT <= SIZEOF_CHAR_P)
  floatObject->value = f;
#else
  {
    /* we may not be aligned properly...store things the hard way */
    mst_Object obj;
    union {
      unsigned long l[2];
      double d;
    } hack;

    obj = (mst_Object)floatObject; /* alias -- avoid left side casts*/
    hack.d = f;
    obj->data[0] = (voidPtr)hack.l[0];
    obj->data[1] = (voidPtr)hack.l[1];
  }
#endif

  return (allocOOP(floatObject));
}

#endif

