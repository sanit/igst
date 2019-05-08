/***********************************************************************
 *
 *	Dictionary Support Module Definitions.
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


#ifndef __GSTDICT__
#define __GSTDICT__

#include "gst.h"

/***********************************************************************
 *
 *	Below are the structural definitions for several of the important
 *	objects present in the Smalltalk system.  Their C representation
 *	corresponds exactly with their Smalltalk representation.
 *
 ***********************************************************************/

 /* Note the use of structural inheritance in C structure definitions here */

typedef struct DictionaryStruct {
  OBJ_HEADER;
  OOP		tally;		/* really, an int */
  OOP		assoc[1];	/* variable sized array of associations */
  /* Other, indexable fields that are the associations for this dictionary */
} *Dictionary;

typedef struct IdentityDictionaryStruct {
  OBJ_HEADER;
  OOP		tally;		/* really, an int */
  OOP		values;		/* an Array */
  OOP		keys[1];	/* variable sized array of OOPS (symbols) */
} *IdentityDictionary;


#define BEHAVIOR_HEADER \
  OBJ_HEADER; \
  OOP		superClass; \
  OOP		subClasses; \
  OOP		methodDictionary; \
  long		instanceSpec

typedef struct BehaviorStruct {
  BEHAVIOR_HEADER;
} *Behavior;

#define CLASS_DESCRIPTION_HEADER \
  BEHAVIOR_HEADER; \
  OOP		name; \
  OOP		comment; \
  OOP		instanceVariables; \
  OOP		category

#define cObjectAnonType		fromInt(-1)
#define cObjectCharType		fromInt(0)
#define cObjectUnsignedCharType	fromInt(1)
#define cObjectShortType	fromInt(2)
#define cObjectUnsignedShortType fromInt(3)
#define cObjectLongType		fromInt(4)
#define cObjectUnsignedLongType	fromInt(5)
#define cObjectFloatType	fromInt(6)
#define cObjectDoubleType	fromInt(7)
#define cObjectStringType	fromInt(8)
#define cObjectSmalltalkType    fromInt(9)
#define cObjectIntType		fromInt(10)
#define cObjectUnsignedIntType	fromInt(11)

typedef struct ClassDescriptionStruct {
  CLASS_DESCRIPTION_HEADER;
} *ClassDescription;

typedef struct AssociationStruct {
  OBJ_HEADER;
  OOP		key;
  OOP		value;
} *Association;

typedef struct ArrayStruct {
  OBJ_HEADER;
  OOP		elements[1];	/* elements of the array */
} *Array;

typedef struct FloatObjectStruct {
  OBJ_HEADER;
  double	value;
} *FloatObject;

typedef struct MessageStruct {
  OBJ_HEADER;
  OOP		selector;
  OOP		args;
} *Message;

typedef struct StringStruct {
  OBJ_HEADER;
  char		chars[1];
} *String;

typedef struct ByteArrayStruct {
  OBJ_HEADER;
  Byte		bytes[1];
} *ByteArray;


typedef struct ClassStruct {
  CLASS_DESCRIPTION_HEADER;
  OOP		classVariables;	/* dictionary of varName, storage */
  OOP		sharedPools;
} *Class;

typedef struct MetaclassStruct {
  CLASS_DESCRIPTION_HEADER;
  OOP		instanceClass;
} *Metaclass;

/* The addr field in this structure is not used anymore.  Instead, we always
   consider the address to reside in the LAST indexed instance variable of the
   receiver; the two definitions usually overlap, but the new one is more
   correct since the second instance variable could actually still be a fixed
   one!!  So the addr field is present for backward compatibility, but you
   should use the new cObjectValueObj and setCObjectValueObj macros; that
   field will eventually disappear. */
typedef struct CObjectStruct {
  OBJ_HEADER;
  OOP		type;
/*voidPtr	maybeSomethingMore[0];   * if there is really something more,
  					 * addr is in the wrong place!! */

  voidPtr	addr;			/* See above */
} *CObject;

typedef struct CTypeStruct {
  OBJ_HEADER;
  OOP		cObjectType;	/* the CObject subclass to use when
				   instantiating this type */
} *CType;


extern OOP		mst_objectClass, magnitudeClass, charClass, timeClass,
			dateClass, fractionClass, contextPartClass,
			numberClass, floatClass, integerClass,
			largeIntegerClass, largeNegativeIntegerClass,
			largeZeroIntegerClass, largePositiveIntegerClass,
			associationClass, linkClass, processClass,
			symLinkClass, collectionClass,
			sequenceableCollectionClass, linkedListClass,
			semaphoreClass,
			arrayedCollectionClass, arrayClass, stringClass,
			symbolClass, byteArrayClass, compiledMethodClass,
			intervalClass, orderedCollectionClass,
			sortedCollectionClass, bagClass, mappedCollectionClass,
			setClass, dictionaryClass, 
			systemDictionaryClass,
			identityDictionaryClass, undefinedObjectClass,
			booleanClass, falseClass, trueClass, 
			processorSchedulerClass, delayClass, sharedQueueClass,
			behaviorClass,
			classDescriptionClass, classClass, metaclassClass,
			smalltalkDictionary, messageClass, methodContextClass,
			blockContextClass, blockClosureClass,
			streamClass, positionableStreamClass, readStreamClass,
			writeStreamClass, readWriteStreamClass, byteStreamClass,
			cObjectClass, cTypeClass, fileStreamClass, memoryClass,
			byteMemoryClass, wordMemoryClass, randomClass,
			cFuncDescriptorClass, tokenStreamClass,
			methodInfoClass, fileSegmentClass, cObjectTypeCType,
			processorOOP;

extern OOP		/* associationNew(), */ dictionaryNew(), /* associationValue(), */
			validClassMethodDictionary(),
			classVariableDictionary(), findSharedPoolVariable(),
			/* dictionaryAt(), dictionaryAtPut(), dictionaryAdd(),
			floatNew(), */ stringNew(), /* indexOOP(), 
			instVarAt(), indexStringOOP(), */
			classMethodDictionary(), findClass(),
			/* dictionaryAssociationAt(), findClassMethod(), */
			messageNewArgs(), getClassSymbol(), dictionaryCopy(),
			instanceVariableArray(), sharedPoolDictionary(),
			/* instantiateOOPWith(), metaclassInstance(), */
			cObjectNewTyped(), cObjectSubtype(),
  			allocCObject(), cTypeNew(), newString(),
  			byteArrayNew(), countedStringNew(),
			identityDictionaryAtPut();

extern Dictionary	growDictionary();

extern IdentityDictionary	growIdentityDictionary();

extern char		*toCString();
extern Byte		*toByteArray();

extern void		/* indexStringOOPPut(), */
			printAssociationKey(), printOOPConstructor(),
			addClassSymbol(), initDictionary(), initRuntimeObjects(),
			setOOPString(), setComment(),
			setOOPBytes(), /* nilFill(), */ freeCObject(),
			/* arrayAtPut(), */
			/* setAssociationValue(), */ printAssociationKey();

extern void		dictInit(); /* ### TEMP HACK ### */

#include "dict.inl"

#endif /* __GSTDICT__ */
