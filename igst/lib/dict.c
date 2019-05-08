/***********************************************************************
 *
 *	Dictionary Support Module.
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
#include "dict.h"
#include "oop.h"
#include "interp.h"
#include "str.h"
#include "lib.h"
#include "gstpub.h"
#include <stdio.h>
#include <float.h>
#if STDC_HEADERS
#include <string.h>
#include <stdlib.h>
#endif /* STDC_HEADERS */

#define INITIAL_DICTIONARY_SIZE		32 /* chosen at random */

/* this must be big enough that the Smalltalk dictionary does not have to grow
   between the time Dictionary is loaded and the time the kernel is initialized.
   Otherwise some of the methods needed to grow the dictionary might not be
   defined yet!! Note that 257 is prime. */
#define INITIAL_SMALLTALK_SIZE		257

typedef struct ClassInfoStruct {
  OOP		*classVar;
  OOP		*superClassPtr;
  mst_Boolean	isPointers;
  mst_Boolean	isWords;
  mst_Boolean	isIndexable;
  char		numFixedFields;
  char		*name;
  char		*instVarNames;
  char		*classVarNames;
  char		*sharedPoolNames;
  char		*comment;
} ClassInfo;

/* Primary class variables.  These variables hold the class objects for
   most of the builtin classes in the system */
OOP			mst_objectClass, magnitudeClass, charClass, timeClass,
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
			systemDictionaryClass, identitySetClass,
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

			processorOOP = nil;

static OOP		identityDictionaryNew(), systemDictionaryNew(),
			newClass(), newMetaclass();
static void		initSmalltalkDictionary(), addSmalltalk(),
			printOOPClassName(), printClassName(),
			createClassesPass1(), createClassesPass2(),
			addSubClass(), addSTDIOObject();

/* static int		identityDictionaryFindKeyOrNil(),
			findKeyOrNil(), findKey(), instanceSize(); */

static char *featureStrings[] = {
#ifdef MACHINE_DEFINES
  MACHINE_DEFINES 
#endif
#ifdef HAVE_DLD
  "DLD",
#endif
  nil
};



/* The class definition structure.  From this structure, the initial set of
   Smalltalk classes are defined.  Note that the comment field is largely
   superfluous, thanks to the comment: primitive and the universal use
   of the class and class comment declarations throughout the Smalltalk
   method definition files (in fact, the comments are not effective now).
   In any dispute, the comment definition in the ".st" file wins. */

static ClassInfo classInfo[] = {
  { &mst_objectClass,		nil,
      true,	false,	false,	0,
      "Object",		nil,	nil,    "Smalltalk CFunctionDescs",
      nil /*"I am the root of the Smalltalk class system. \n\
All classes in the system are subclasses of me."*/ },

  { &magnitudeClass,		&mst_objectClass,
      true,	false,	false,	0,
      "Magnitude",	nil,	nil,	nil,
      nil },

  { &messageClass,		&mst_objectClass,
      true,	false,	false,	2,
      "Message",	"selector args",	nil,	nil,
      nil },

  { &charClass,			&magnitudeClass,
      false,	true,	true,	0, /* really has 1 indexed var */
      "Character",	nil,	"Table",	nil,
      nil /*"My instances represent the 256 characters of the character set.  I provide\n\
messages to translate between integers and character objects, and provide \n\
names for some of the common unprintable characters."*/ },

  { &timeClass,			&magnitudeClass,
      true,	false,	false,	1,
      "Time",		"seconds",	
      "SecondClockAdjustment SecondClockOnStartup",	nil,
      nil },

  { &dateClass,			&magnitudeClass,
      true,	false,	false,	4,
      "Date",		"days day month year",
      "DayNameDict MonthNameDict",	nil,
      nil },

  { &numberClass,		&magnitudeClass,
      true,	false,	false,	0,
      "Number",		nil,	nil,	nil,
      nil },

  { &floatClass,		&numberClass,
      false,	false,	true,	0,	/* really 8, but we're variable sized*/
      "Float",		nil,	nil,	nil,
      nil },

  { &fractionClass,		&numberClass,
      true,	false,	false,	2,
      "Fraction",	"numerator denominator",	"Zero One",	nil,
      nil },

  { &integerClass,	       	&numberClass,
      false,	true,	false,	0,
      "Integer",	nil,	nil,	nil,
      nil },

  { &largeIntegerClass,	       	&integerClass,  /*these four classes added by*/
      false,	false,	true,	0,              /*pb Sep 10 18:06:49 1998 */
      "LargeInteger",	nil,
      "Zero One ZeroBytes OneBytes LeadingZeros TrailingZeros",	nil,
      nil },

  { &largePositiveIntegerClass,	       	&largeIntegerClass,
      false,	false,	true,	0,
      "LargePositiveInteger",	nil,    nil,	nil,
      nil },

  { &largeZeroIntegerClass,	       	&largePositiveIntegerClass,
      false,	false,	true,	0,
      "LargeZeroInteger",	nil,    nil,	nil,
      nil },

  { &largeNegativeIntegerClass,	       	&largeIntegerClass,
      false,	false,	true,	0,
      "LargeNegativeInteger",	nil,    nil,	nil,
      nil },

  { &associationClass,		&magnitudeClass,
      true,	false,	false,	2,
      "Association",	"key value",	nil,	nil,
      nil },

  { &linkClass,			&mst_objectClass,
      true,	false,	false,	1,
      "Link",	        "nextLink",	nil,	nil,
      nil },

  { &processClass,		&linkClass,
      true,	false,	false,	5,
      "Process",	"suspendedContext priority myList name exceptionHandlers",
      nil,	nil,	 nil },

  { &symLinkClass,		&linkClass,
      true,	false,	false,	1,
      "SymLink",	"symbol",	nil,	nil,
      nil },

  { &collectionClass,		&mst_objectClass,
      true,	false,	false,	0,
      "Collection",	nil,	nil,	nil,
      nil },

  { &sequenceableCollectionClass,	&collectionClass,
      true,	false,	false,	0,
      "SequenceableCollection",	nil,	nil,	nil,
      nil },

  { &linkedListClass,		&sequenceableCollectionClass,
      true,	false,	false,	2,
      "LinkedList",	"firstLink lastLink",	nil,	nil,
      nil },

  { &semaphoreClass,		&linkedListClass,
      true,	false,	false,	1,
      "Semaphore",	"signals",	nil,	nil,
      nil },

  { &arrayedCollectionClass,	&sequenceableCollectionClass,
      true,	false,	true,	0,
      "ArrayedCollection",	nil,	nil,	nil,
      nil },

  { &arrayClass,		&arrayedCollectionClass,
      true,	false,	true,	0,
      "Array",  	nil,	nil,	nil,
      nil },

  { &stringClass,		&arrayedCollectionClass,
      false,	false,	true,	0,
      "String",		nil,	nil,	nil,
      nil },

  { &symbolClass,		&stringClass,
      false,	false,	true,	0,
      "Symbol",		nil,	nil,	nil,
      nil },

  { &byteArrayClass,		&arrayedCollectionClass,
      false,	false,	true,	0,
      "ByteArray",	nil,	nil,	nil,
      nil },

  { &compiledMethodClass,	&arrayedCollectionClass,
      false,	false,	true,	3,
      "CompiledMethod",	"descriptor methodHeader literals",	nil,	nil, 
      nil /*"I represent methods that have been compiled.  I can recompile \n\
methods from their source code, I can invoke Emacs to edit the source code \n\
for one of my instances, and I know how to access components of my \n\
instances."*/ },

  { &intervalClass,	&arrayedCollectionClass,
      true,	false,	false,	3,
      "Interval",	"start stop step", nil,	nil,
      nil /*"My instances represent ranges of objects, typically Magnitude type\n\
objects.  I provide iteration/enumeration messages for producing all the\n\
members that my instance represents."*/ },

  { &orderedCollectionClass,	&sequenceableCollectionClass,
      true,	false,	true,	2,
      "OrderedCollection",	"firstIndex lastIndex",	nil,	nil,
      nil },

  { &sortedCollectionClass,	&orderedCollectionClass,
      true,	false,	true,	1,
      "SortedCollection",	"sortBlock",	nil,	nil,
      nil /*"I am a collection of objects, stored and accessed according to some\n\
sorting criteria.  I store things using a bubble sort.  My instances have a \n\
comparison block associated with them; this block takes two arguments and\n\
is a predicate which returns true if the first argument should be sorted \n\
earlier than the second.  The default block is [ :a :b | a <= b ], but I\n\
will accept any block that conforms to the above criteria."*/ },

  { &bagClass,	&collectionClass,
      true,	false,	false,	1,
      "Bag",		"contents",	nil,	nil,
      nil /*"My instances are unordered collections of objects.  You can think\n\
of me as a set with a memory; that is, if the same object is added to me\n\
twice, then I will report that that element has been stored twice."*/ },

  { &mappedCollectionClass,	&collectionClass,
      true,	false,	false,	2,
      "MappedCollection",	"domain map",	nil,	nil,
      nil },

  { &setClass,	&collectionClass,
      true,	false,	true,	1,
      "Set",		"tally",	nil,	nil,
      nil /*"I am the typical set object; I can store any objects uniquely.  I\n\
use the = operator to determine duplication of objects."*/ },

  { &identitySetClass,	&setClass,
      true,	false,	true,	0,
      "IdentitySet",		nil,	nil,	nil,
      nil },

  { &dictionaryClass,	&setClass,
      true,	false,	true,	0,
      "Dictionary",	nil,	nil,	nil,
      nil /*"I implement a dictionary, which is an object that is indexed by \n\
unique objects (typcially instances of Symbol), and associates another \n\
object with that index.  I use the equality operator = to determine \n\
equality of indices."*/ },

  { &identityDictionaryClass,		&dictionaryClass,
      true,	false,	true,	1,
      "IdentityDictionary",	"values",	nil,	nil,
      nil /*"I am similar to dictionary, except that my representation is \n\
different, and I use the object identity comparision message == to \n\
determine equivalence of indices."*/ },

  /* MUST have the same structure as dictionary; they're used interchangeably
   * within the C portion of the system */
  { &systemDictionaryClass,		&dictionaryClass,
      true,	false,	true,	0, 
      "SystemDictionary",	nil,	nil,	nil,
      nil },

  { &streamClass,		&mst_objectClass,
      true,	false,	false,	0,
      "Stream",		nil,	nil,	nil,
      nil },

  { &tokenStreamClass,		&streamClass,
      true,	false,	false,	1,
      "TokenStream",		"charStream",	nil,	nil,
      nil /*"I am not a typical part of the Smalltalk kernel class hierarchy.\n\
I operate on a stream of characters and return distinct \n\
(whitespace-delimited) groups of characters."*/ },

  { &positionableStreamClass,	&streamClass,
      true,	false,	false,	4,
      "PositionableStream",	"collection ptr endPtr access",	nil,	nil,
      nil },

  { &readStreamClass,		&positionableStreamClass,
      true,	false,	false,	0,
      "ReadStream",	nil,	nil,	nil,
      nil },

  { &writeStreamClass,		&positionableStreamClass,
      true,	false,	false,	0,
      "WriteStream",	nil,	nil,	nil,
      nil },

  { &readWriteStreamClass,	&writeStreamClass,
      true,	false,	false,	0,
      "ReadWriteStream",	nil,	nil,	nil,
      nil },

  { &byteStreamClass,		&readWriteStreamClass,
      true,	false,	false,	0,
      "ByteStream",	nil,	nil,	nil,
      nil },

  { &fileStreamClass,		&byteStreamClass,
      true,	false,	false,	4,
      "FileStream",	"file name buffer isPipe",		"Verbose Record Includes",	nil,
      nil /*"My instances are what conventional programmers think of as files.\n\
My instance creation methods accept the name of a disk file (or any named \n\
file object, such as /dev/rmt0 on UNIX or MTA0: on VMS)."*/ },

  { &randomClass,		&streamClass,
      true,	false,	false,	1,
      "Random",		"seed",		nil,	nil,
      nil },

  { &undefinedObjectClass,		&mst_objectClass,
      true,	false,	false,	0,
      "UndefinedObject",	nil,	nil,	nil,
      nil /*"I have the questionable distinction of being a class with only one\n\
instance, which is the object \"nil\".  I suspect that I should be sent\n\
messages when errors occur, but currently I am not."*/ },

  { &booleanClass,		&mst_objectClass,
      true,	false,	false,	0,
      "Boolean",	nil,	nil,	nil,
      nil },

  { &falseClass,		&booleanClass,
      true,	false,	false,	1,
      "False",		"truthValue",	nil,	nil, /* ### what's the inst var name in ST-80? */
      nil },

  { &trueClass,		&booleanClass,
      true,	false,	false,	1,
      "True",		"truthValue",	nil,	nil, /* ### what's the inst var name in ST-80? */
      nil },

  { &processorSchedulerClass,	&mst_objectClass,
      true,	false,	false,	2,
      "ProcessorScheduler",	"processLists activeProcess",	nil,	nil,
      nil },

  { &delayClass,	&mst_objectClass,
      true,	false,	false,	2,
      "Delay",	"resumptionTime isRelative",
      "Queue TimeoutSem MutexSem DelayProcess IdleProcess",	nil,
      nil },

  { &sharedQueueClass,	&mst_objectClass,
      true,	false,	false,	3, 
      "SharedQueue",	"queueSem valueReady queue",	nil,	nil,
      nil },

  /* Change this, classDescription, or Class, and you must change 
   * the implementaion of newMetaclass some */
  { &behaviorClass,		&mst_objectClass,
      true,	false,	false,	4,
      "Behavior",	"superClass subClasses methodDictionary instanceSpec",
      nil,	nil,
      nil },

  { &classDescriptionClass,		&behaviorClass,
      true,	false,	false,	4,
      "ClassDescription",	"name comment instanceVariables category",
      nil,	nil,
      nil },

  { &classClass,		&classDescriptionClass,
      true,	false,	false,	2,
      "Class",	"classVariables sharedPools",	nil,	nil,
      nil },

  { &metaclassClass,		&classDescriptionClass,
      true,	false,	false,	1,
      "Metaclass",	"instanceClass",	nil,	nil,
      nil },

  { &contextPartClass,		&mst_objectClass,
      true,	false,	true,	4,
      "ContextPart",	"parent ip sp hasBlock",	"UnwindPoints",	nil,
      nil },

  { &methodContextClass,		&contextPartClass,
      true,	false,	true,	4,
      "MethodContext",	"method methodClass selector receiver",	nil,	nil,
      nil },

  { &blockContextClass,		&contextPartClass,
      true,	false,	true,	4,
      "BlockContext",	"numArgs numTemps outerContext home",	nil,
     nil,
      nil },

  { &blockClosureClass,		&mst_objectClass,
      false,	false,	true,	1,
      "BlockClosure",	"outerContext",	nil,	nil,
      nil },


/***********************************************************************
 *
 *	End of Standard Smalltalk Class definitions.  The definitions below are
 *	specific to GNU Smalltalk.
 *
 ***********************************************************************/

  { &cObjectClass,		&mst_objectClass,
      false,	true,	true,	1, /* leave this this way */
      "CObject",	"type",	nil,	nil,
      nil /*"I am not part of the standard Smalltalk kernel class hierarchy.\n\
My instances contain values that are not interpreted by the Smalltalk \n\
system; they frequently hold \"pointers\" to data outside of the Smalltalk\n\
environment.  The C callout mechanism allows my instances to be transformed\n\
into their corresponding C values for use in external routines."*/ },

  { &cTypeClass,		&mst_objectClass,
      true,	false,	false,	1,
      "CType",	"cObjectType",	nil,	nil,
      "I am not part of the standard Smalltalk kernel class hierarchy.\n\
I contain type information used by subclasses of CObject, which represents\n\
external C data items." },

  { &cFuncDescriptorClass,	&mst_objectClass,
      true,	false,	true,	4,
      "CFunctionDescriptor",	"cFunction cFunctionName returnType numFixedArgs",
      nil,	nil,
      nil },

  { &memoryClass,		&mst_objectClass,
      false,	true,	true,	0,
      "Memory",		nil,	nil,	nil,
      nil },

  { &byteMemoryClass,		&memoryClass,
      false,	false,	true,	0,
      "ByteMemory",	nil,	nil,	nil,
      nil },

  { &wordMemoryClass,		&memoryClass,
      false,	true,	true,	0,
      "WordMemory",	nil,	nil,	nil,
      nil },

  { &methodInfoClass,		&mst_objectClass,
      true,	false,	false,	2,
      "MethodInfo",	"sourceCode category",	nil,	nil,
      nil },

  { &fileSegmentClass,		&mst_objectClass,
      true,	false,	false,	3,
      "FileSegment",	"fileName startPos length",	nil,	nil,
      nil },

  { nil }

/* Smalltalk classes not defined:
   SmallInteger (integer has its function now)
   (others like Point/Rectangle/RunArray are defined after the kernel starts
 */

};



/*
 *	initDictionary()
 *
 * Description
 *
 *	Creates the kernel classes of the Smalltalk system.  Operates in two
 *	passes: pass1 creates the class objects, but they're not completely
 *	initialized.  pass2 finishes the initialization process.  The garbage
 *	collector can NOT run during this time.
 *
 */
/* runs before GC turned on */
void
initDictionary ()
{
  createClassesPass1();

  initBuiltinObjectsClasses();	/* we can do this now that classes are def'd */
  initSmalltalkDictionary();

  createClassesPass2();

  initRuntimeObjects();
}

/* runs before GC turned on */
void
createClassesPass1 ()
{
  ClassInfo	*ci;
  OOP		parentClassOOP;

  /* Because all of the classes in classInfo are in the root set, we
   * never need to validate them */
  for (ci = classInfo; ci->classVar; ci++) {
    if (ci->superClassPtr == nil) {
      parentClassOOP = (OOP)nil;
    } else {
      parentClassOOP = *ci->superClassPtr;
    }
      
    *ci->classVar = newClass(parentClassOOP, ci->isPointers, ci->isWords,
			     ci->isIndexable, ci->numFixedFields);
  }
}

/* runs before GC turned on */
void
createClassesPass2 ()
{
  ClassInfo	*ci;
  OOP		classOOP, superClassOOP;
  Class		class, superClass;
  long		index;

  /* Because all of the classes in classInfo are in the root set, we
   * never need to validate them */
  for (ci = classInfo; ci->classVar; ci++) {
    classOOP = *ci->classVar;
    class = (Class)oopToObj(classOOP);
    class->name = internString(ci->name);
    addSmalltalk(ci->name, classOOP);
    class->methodDictionary = nilOOP;
    index = toInt(class->subClasses);
    if (classOOP == classClass) {
      /*
       * Object class being a subclass of Class is not an apparent link,
       * and so the index which is the number of subclasses of the class
       * is off by one.  We correct that here.
       */
      index++;
    }
    class->subClasses = arrayNew(index);
    if (index > 0) {
      arrayAtPut(class->subClasses, 1, fromInt(index));
    }
    if (classOOP == classClass) {
      /*
       * we don't want the meta class to have a subclass if we're special
       * casing Object class, so back off the number of sub classes for
       * the meta class.
       */
      index--;
    }
    if (classOOP == mst_objectClass) { /* is this Object? */
      /* nilOOP wasn't available during pass1, but now it is */
      class->superClass = nilOOP;
    } else {
      /* hack the parent's subclass array */
      superClassOOP = class->superClass;
      addSubClass(superClassOOP, classOOP);
      if (classOOP == classClass) {
	/* here's where we patch in Object class is-a-subclass-of Class */
	superClass = (Class)oopToObj(oopClass(mst_objectClass));
	superClass->superClass = classOOP;
	addSubClass(classOOP, oopClass(mst_objectClass));
      }
    }
    class->objClass = newMetaclass(classOOP, index);
    class->instanceVariables =
      makeInstanceVariableArray(class->superClass, ci->instVarNames);
    class->classVariables = makeClassVariableDictionary(class->superClass,
							ci->classVarNames);
    class->sharedPools = makePoolArray(class->superClass, ci->sharedPoolNames);
    if (ci->comment) {
      class->comment = stringNew(ci->comment);
    } else {
      class->comment = nilOOP;	/* mark for later use */
    }

    class->category = nilOOP;	/* not used yet. */
  }
}

/* runs before GC turned on */
OOP
newMetaclass(classOOP, numSubClasses)
OOP	classOOP;
int	numSubClasses;
{
  OOP		superClassOOP, metaclassOOP;
  Metaclass	metaclass;

  metaclass = (Metaclass)newInstance(metaclassClass);
  metaclassOOP = allocOOP(metaclass);
  superClassOOP = superClass(classOOP);

  if (classOOP == mst_objectClass) {
    /* Object case: make this be Class to close the circularity */
    metaclass->superClass = classClass;
  } else {
    metaclass->superClass = oopClass(superClassOOP);
    addSubClass(metaclass->superClass, metaclassOOP);
  }

  /* the specifications here should match what a class should have: instance
     variable names, the right number of instance variables, etc.  We could
     take three passes, and use the instance variable spec for classes once
     it's established, but it's easier to create them here by hand */
  metaclass->name = nilOOP;
  metaclass->comment = nilOOP;
  metaclass->instanceVariables = 
      makeInstanceVariableArray(nilOOP, 
"superClass subClasses methodDictionary instanceSpec \
name comment instanceVariables category \
classVariables sharedPools");

  metaclass->category = nilOOP;
  metaclass->subClasses = arrayNew(numSubClasses);
  if (numSubClasses > 0) {
    arrayAtPut(metaclass->subClasses, 1, fromInt(numSubClasses));
  }
  metaclass->methodDictionary = nilOOP;
  metaclass->instanceSpec = ISP_INTMARK | ISP_ISPOINTERS |
    (((sizeof(struct ClassStruct) - sizeof(ObjectHeader))/sizeof(OOP)) << ISP_NUMFIXEDFIELDS);

  metaclass->instanceClass = classOOP;

  return (metaclassOOP);
}

/* runs before GC turned on */
void
addSubClass (superClassOOP, subClassOOP)
     OOP	superClassOOP, subClassOOP;
{
  ClassDescription superClass;
  int		index;

  superClass = (ClassDescription)oopToObj(superClassOOP);

  if (numOOPs(oopToObj(superClass->subClasses)) > 0) {
    index = toInt(arrayAt(superClass->subClasses, 1));
    arrayAtPut(superClass->subClasses, 1, fromInt(index - 1));
    arrayAtPut(superClass->subClasses, index, subClassOOP);
  } else {
    errorf("Attempt to add subclass to zero sized class");
  }
}

/*
 *	static void initSmalltalkDictionary()
 *
 * Description
 *
 *	This creates the SystemDictionary called Smalltalk and initializes some
 *	of the variables in it.
 *
 */
void
initSmalltalkDictionary ()
{
  OOP		cFunctionDescsDictionary, featuresArray;
  char	    	fullVersionString[200];
  int		i, numFeatures;

  smalltalkDictionary 	   = systemDictionaryNew();
  symbolTable 		   = arrayNew(INITIAL_SYMBOL_TABLE_SIZE);
  cFunctionDescsDictionary = dictionaryNew();
  cObjectTypeCType 	   = cTypeNew(cObjectClass);

  sprintf(fullVersionString, "GNU Smalltalk version %s", versionString);

  initProcessSystem();

  addSmalltalk("Smalltalk",		smalltalkDictionary);
  addSmalltalk("CFunctionDescs",	cFunctionDescsDictionary);
  addSmalltalk("Version", 		stringNew(fullVersionString));
  addSmalltalk("CObjectType",		cObjectTypeCType);
  addSmalltalk("KernelInitialized",	falseOOP);
  addSmalltalk("SymbolTable", 		symbolTable);
  addSmalltalk("Processor",		processorOOP);

  for (numFeatures = 0; featureStrings[numFeatures]; numFeatures++) ;

  featuresArray = arrayNew(numFeatures);

  for (i = 0; i < numFeatures; i++) {
    arrayAtPut(featuresArray, i + 1, internString(featureStrings[i]));
  }

  addSmalltalk("Features", featuresArray);
}

/* runs before GC turned on */
static void addSmalltalk (globalName, globalValue)
     char	*globalName;
     OOP	globalValue;
{
  dictionaryAtPut(smalltalkDictionary, internString(globalName), globalValue);
}

/* runs before GC turned on */
void
initRuntimeObjects ()
{
  addSmalltalk("KernelFilePath", stringNew(kernelFileDefaultPath));
  addSmalltalk("ImageFilePath", stringNew(imageFileDefaultPath));
  addSmalltalk("ImageFileName", stringNew(binaryImageName));

#ifdef WORDS_BIGENDIAN
  addSmalltalk("Bigendian", trueOOP);
#else
  addSmalltalk("Bigendian", falseOOP);
#endif

  addSmalltalk("CIntSize", fromInt(sizeof(int)));
  addSmalltalk("CShortSize", fromInt(sizeof(short)));
  addSmalltalk("CLongSize", fromInt(sizeof(long)));
  addSmalltalk("CFloatSize", fromInt(sizeof(float)));
  addSmalltalk("CDoubleAlignment", fromInt(DOUBLE_ALIGNMENT));
  addSmalltalk("CDoubleSize", fromInt(sizeof(double)));
  addSmalltalk("CPtrSize", fromInt(sizeof(voidPtr)));
  addSmalltalk("CDoubleMin", floatNew(DBL_MIN));
  addSmalltalk("CDoubleMax", floatNew(DBL_MAX));

  addSTDIOObject(stdin, "stdin");
  addSTDIOObject(stdout, "stdout");
  addSTDIOObject(stderr, "stderr");
}

/* runs before GC turned on */
void
addSTDIOObject (file, fileObjectName)
     FILE	*file;
     char	*fileObjectName;
{
  OOP		fileOOP, fileStreamOOP;

  fileOOP = cObjectNew(file);
  fileStreamOOP = allocOOP(instantiate(fileStreamClass));
  setFileStreamFile(fileStreamOOP, fileOOP, stringNew(fileObjectName), false);

  addSmalltalk(fileObjectName, fileStreamOOP);
}


/* runs before GC turned on */
OOP
newClass (superClassOOP, isPointers, isWords, isIndexable, numFixedFields)
     OOP	 superClassOOP;
     mst_Boolean isPointers, isWords, isIndexable;
     int	 numFixedFields;
{
  Class		class, superClass;
  long        	superInstanceSpec;

  if (superClassOOP != (OOP)nil) {
    /* adjust the number of instance variables to account for inheritance */
    superInstanceSpec = classInstanceSpec(superClassOOP);
    numFixedFields += superInstanceSpec >> ISP_NUMFIXEDFIELDS;
    superClass = (Class)oopToObj(superClassOOP);
    superClass->subClasses = fromInt(toInt(superClass->subClasses) + 1);
  }

  class			= (Class)allocObj(sizeof(struct ClassStruct));
  class->objSize	= ROUNDED_WORDS(sizeof(struct ClassStruct));
  class->objClass	= nil;
  class->superClass			= superClassOOP;
  class->instanceSpec   = ISP_INTMARK | 
      (isPointers ? ISP_ISPOINTERS : 0) |
      (isWords ? ISP_ISWORDS : 0) |
      (isIndexable ? ISP_ISINDEXABLE : 0) |
      (numFixedFields << ISP_NUMFIXEDFIELDS);

  class->subClasses			= fromInt(0);

  return (allocOOP(class));
}


void
setComment (classDescOOP, commentOOP)
     OOP	classDescOOP, commentOOP;
{
    Class	class;

    class = (Class)oopToObj(classDescOOP);
    class->comment = commentOOP;
}


void
printOOPConstructor (oop)
     OOP	oop;
{
  long		instanceSpec;
  OOP		classOOP;

  if (isAMetaclass(oop)) {
    classOOP = findAnInstance(oop);
    if (isNil(classOOP)) {
      printf("<name unknown>");		/* we're a nameless class */
    } else {
      printClassName(classOOP);
    }
    printf(" class");
    return;
  }

  if (isAClass(oop)) {
    printClassName(oop);
    return;
  }

  printOOPClassName(oop);

  classOOP = oopClass(oop);
  instanceSpec = classInstanceSpec(classOOP);
  if (instanceSpec & ISP_ISINDEXABLE) {
    printf(" new: %ld ", numIndexableFields(oop));
  } else {
    printf(" new ");
  }

  if (regressionTesting) {
    printf("\"<%#x>\"", 0);
  } else {
    printf("\"<%#lx>\"", (unsigned long) oop);
  }
}

void
printOOPClassName (oop)
     OOP	oop;
{
  OOP		classOOP;

  if (isInt(oop)) {
    classOOP = integerClass;
  } else {
    classOOP = oopClass(oop);
  }

  printClassName(classOOP);
}


void
printClassName (classOOP)
     OOP	classOOP;
{
  Class		class;

  class = (Class)oopToObj(classOOP);
  if (isNil(class->name)) {
    printf("<no class name>");
  } else {
    printString(class->name);
  }
}

OOP
getClassSymbol (classOOP)
     OOP	classOOP;
{
  Class		class;

  class = (Class)oopToObj(classOOP);
  return (class->name);
  /* this is the case when we have a metaclass,
     ??? I don't think that this is right, but I don't know what else to do
     here */
}



OOP findClass (classNameOOP)
     OOP	classNameOOP;
{
  return (dictionaryAt(smalltalkDictionary, classNameOOP));
}



/*
 *	OOP validClassMethodDictionary(classOOP)
 *
 * Description
 *
 *	Gets the method dictionary associated with "classOOP", and returns it.
 *	If the methodDictionary associated with "classOOP" is nil, one is
 *	created and installed into that class.
 *
 * Inputs
 *
 *	classOOP: 
 *		Class to get the method dictionary of.
 *
 * Outputs
 *
 *	A non-nil object of type MethodDictionary.
 */
OOP
validClassMethodDictionary (classOOP)
     OOP	classOOP;
{
  Class		class;

  /* ??? check for non-class objects */
  class = (Class)oopToObj(classOOP);
  if (isNil(class->methodDictionary)) {
    OOP identDict;
    identDict = identityDictionaryNew();
    class = (Class)oopToObj(classOOP);
    class->methodDictionary = identDict;
  }

  return (class->methodDictionary);
}

OOP
classVariableDictionary (classOOP)
     OOP	classOOP;
{
  Class		class;

  /* ??? check for non-class objects */
  class = (Class)oopToObj(classOOP);
  return (class->classVariables);
}

OOP
instanceVariableArray (classOOP)
     OOP	classOOP;
{
  Class		class;

  /* ??? check for non-class  objects */
  class = (Class)oopToObj(classOOP);
  return (class->instanceVariables);
}

OOP
sharedPoolDictionary (classOOP)
     OOP	classOOP;
{
  Class		class;

  /* ??? check for non-class objects */
  class = (Class)oopToObj(classOOP);
  return (class->sharedPools);
}


OOP
findSharedPoolVariable (classOOP, symbol)
     OOP	classOOP, symbol;
{
  OOP		assocOOP, poolDictionaryOOP;
  Class		class;
  int		numPools, i;

  /* ??? check for non-class objects */
  class = (Class)oopToObj(classOOP);

  /* ??? shared pools are currently represented as arrays, from the book
     I conjecture that their shared pools are implemented as sets. */
  numPools = numOOPs(oopToObj(class->sharedPools));
  for (i = 0; i < numPools; i++) {
    poolDictionaryOOP = arrayAt(class->sharedPools, i+1);
    assocOOP = dictionaryAssociationAt(poolDictionaryOOP, symbol);
    if (!isNil(assocOOP)) {
      return (assocOOP);
    }
  }

  return (nilOOP);
}


/*
 *	Dictionary growDictionary(dictionaryOOP)
 *
 * Description
 *
 *	Called when a dictionary becomes full, this routine replaces the
 *	dictionary instance that "dictionaryOOP" is pointing to with a new,
 *	larger dictionary, and returns this new dictionary as its value.
 *
 * Inputs
 *
 *	dictionaryOOP: 
 *		Object pointer to the dictionary that's to be expanded
 *
 * Outputs
 *
 *	New dictionary, with all of the old elements rehashed into it. 
 */
Dictionary 
growDictionary (dictionaryOOP)
     OOP	dictionaryOOP;
{
  Dictionary	oldDictionary, dictionary;
  long		oldNumFields, numFields, i, index;
  OOP		associationOOP;
  Association	association;


  oldDictionary = (Dictionary)oopToObj(dictionaryOOP);
  oldNumFields = numOOPs(oldDictionary) - 1;

  numFields = oldNumFields * 2;

  /* no need to use the incubator here.  We are instantiating just
     one object, the new dictionary itself */

  dictionary = (Dictionary)instantiateWith(oopClass(dictionaryOOP), numFields);
  dictionary->tally = oldDictionary->tally;
  oldDictionary = (Dictionary)oopToObj(dictionaryOOP);
  setOOPObject(dictionaryOOP, dictionary);

  /* rehash all associations from old dictionary into new one */
  for (i = 0; i < oldNumFields; i++) {
    if (!isNil(oldDictionary->assoc[i])) {
      associationOOP = oldDictionary->assoc[i];
      association = (Association)oopToObj(associationOOP);
      index = findKeyOrNil(dictionaryOOP, association->key);
      dictionary->assoc[index] = associationOOP;
    }
  }

  return (dictionary);
}

IdentityDictionary
growIdentityDictionary (identityDictionaryOOP)
     OOP	identityDictionaryOOP;
{
  IdentityDictionary oldIdentityDictionary, identityDictionary;
  Array		values, oldValues;
  OOP		key, valuesOOP, oldValuesOOP, oldOOP;
  long		oldNumFields, numFields, i, index;
  IncPtr	incPtr;


  oldIdentityDictionary = (IdentityDictionary)oopToObj(identityDictionaryOOP);
  oldNumFields = numOOPs(oldIdentityDictionary) - OBJ_HEADER_SIZE_WORDS;

  numFields = oldNumFields * 2;

  incPtr = incSavePointer();

  oldValuesOOP = oldIdentityDictionary->values;
  oldValues = (Array)oopToObj(oldValuesOOP);
  valuesOOP = arrayNew(numFields);
  incAddOOP(valuesOOP);

  identityDictionary =
    (IdentityDictionary)instantiateWith(identityDictionaryClass, numFields);

  oldIdentityDictionary = (IdentityDictionary)oopToObj(identityDictionaryOOP);
  identityDictionary->tally = oldIdentityDictionary->tally;
  setOOPObject(identityDictionaryOOP, identityDictionary);
  identityDictionary = (IdentityDictionary)oopToObj(identityDictionaryOOP);

  values = (Array)oopToObj(valuesOOP);
  identityDictionary->values = valuesOOP;

  /* rehash all associations from old dictionary into new one */
  for (i = 0; i < oldNumFields; i++) {
    key = oldIdentityDictionary->keys[i];
    if (!isNil(key)) {
      index = identityDictionaryFindKeyOrNil(identityDictionaryOOP, key);
      identityDictionary->keys[index] = key;
      oldOOP = oldValues->elements[i];
      values->elements[index] = oldOOP;
    }
  }

  incRestorePointer(incPtr);
  return (identityDictionary);
}


OOP
identityDictionaryNew ()
{
  IdentityDictionary identityDictionary;
  IncPtr	incPtr;
  OOP		valuesOOP;

  incPtr = incSavePointer();
  valuesOOP = arrayNew(INITIAL_DICTIONARY_SIZE);
  incAddOOP(valuesOOP);

  identityDictionary =
    (IdentityDictionary)instantiateWith(identityDictionaryClass,
				       INITIAL_DICTIONARY_SIZE);
  identityDictionary->tally = fromInt(0);
  identityDictionary->values = valuesOOP;

  incRestorePointer(incPtr);
  return (allocOOP(identityDictionary));
}

OOP
identityDictionaryAtPut (identityDictionaryOOP, keyOOP, valueOOP)
     OOP	identityDictionaryOOP, keyOOP, valueOOP;
{
  IdentityDictionary identityDictionary;
  Array		valuesArray;
  long		index;
  
  index = identityDictionaryFindKeyOrNil(identityDictionaryOOP, keyOOP);
  identityDictionary = (IdentityDictionary)oopToObj(identityDictionaryOOP);

  /* Dictionary may grow during find key or nil, so ensure that both the 
   * key and the value are in the right place. 
   */

  if (isNil(identityDictionary->keys[index])) {
    identityDictionary->tally = incrInt(identityDictionary->tally);
  }
  identityDictionary->keys[index] = keyOOP;
  valuesArray = (Array)oopToObj(identityDictionary->values);
  valuesArray->elements[index] = valueOOP;

  return (keyOOP);
}

OOP
systemDictionaryNew()
{
  Dictionary	dictionary;

  /* How I'd like ^super new! */
  dictionary = (Dictionary)instantiateWith(systemDictionaryClass,
					   INITIAL_SMALLTALK_SIZE);
  dictionary->tally = fromInt(0);
  return (allocOOP(dictionary));
}

OOP
dictionaryNew ()
{
  Dictionary	dictionary;

  dictionary = (Dictionary)instantiateWith(dictionaryClass,
					   INITIAL_DICTIONARY_SIZE);
  dictionary->tally = fromInt(0);

  return (allocOOP(dictionary));
}


/*
 *	OOP dictionaryCopy(dictionaryOOP)
 *
 * Description
 *
 *	Create and return an exact copy of "dictionaryOOP", which is a normal
 *	dictionary object.  This is a "shallow copy"; all the associations in
 *	the dictionary are the exact same ones that are in the original
 *	dictionary.  If passed nil, returns nil.
 *
 * Inputs
 *
 *	dictionaryOOP: 
 *		A dictionary object that a copy is to be made of.
 *
 * Outputs
 *
 *	An exact copy of the dictionary that we were passed.
 */
OOP
dictionaryCopy (dictionaryOOP)
     OOP	dictionaryOOP;
{
  Dictionary	oldDictionary, dictionary;
  long		numFields;

  if (isNil(dictionaryOOP)) {
    return (nilOOP);
  }

  oldDictionary = (Dictionary)oopToObj(dictionaryOOP);
  numFields = numOOPs(oldDictionary) - 1;

  /* ??? we may want to create a copy object routine that just mallocs and
     copies the contents verbatim; this routine would then be just a call to
     that routine. */
  dictionary = (Dictionary)instantiateWith(dictionaryClass, numFields);
  memcpy(dictionary, oldDictionary, size2Bytes(oldDictionary->objSize));

  return (allocOOP(dictionary));
}

void
printAssociationKey (associationOOP)
     OOP	associationOOP;
{
  Association	association;

  if (!isClass(associationOOP, associationClass)) {
    printf("<non-association in association context>");
    return;
  }

  association = (Association)oopToObj(associationOOP);
  if (oopClass(association->key) != symbolClass) {
    printf("<unprintable key type>");
  } else {
    printSymbol(association->key);
  }
}


OOP nilVec[100];

void
dictInit ()
{
  int i;
  for (i = 0; i < 100; i++) {
    nilVec[i] = nilOOP;
  }
}



OOP
newString (len)
     int	len;
{
  String	string;
  OOP		stringOOP;

  string = (String)newInstanceWith(stringClass, len);
  stringOOP = allocOOP(string);
  initEmptyBytes(stringOOP, len);

  return (stringOOP);
}

OOP
stringNew (s)
     char	*s;
{
  String	string;
  int		len;
  OOP		stringOOP;

  len = strlen(s);
  string = (String)newInstanceWith(stringClass, len);
  strncpy(string->chars, s, len);

  stringOOP = allocOOP(string);
  initEmptyBytes(stringOOP, len);

  return (stringOOP);
}

OOP
countedStringNew (s, len)
     char	*s;
     int	len;
{
  String	string;
  OOP		stringOOP;

  string = (String)newInstanceWith(stringClass, len);
  memcpy(string->chars, s, len);

  stringOOP = allocOOP(string);
  initEmptyBytes(stringOOP, len);

  return (stringOOP);
}

void
setOOPString (stringOOP, s)
     OOP	stringOOP;
     char	*s;
{
  String	string;
  long		len;

  len = strlen(s);
  string = (String)newInstanceWith(stringClass, len);
  strncpy(string->chars, s, len);

  setOOPObject(stringOOP, string);
  setEmptyBytes(stringOOP, len);
}

/* Allocates and returns a new C (ASCIZ) string that has the same contents
 * as "stringOOP" */
char *
toCString (stringOOP)
     OOP	stringOOP;
{
  char		*result;
  int		len;
  String	string;

  string = (String)oopToObj(stringOOP);
  len = oopNumFields(stringOOP);
  result = (char *)xmalloc(len + 1);
  strncpy(result, string->chars, len);
  result[len] = '\0';

  return (result);
}

OOP
byteArrayNew (bytes, len)
     Byte	*bytes;
     int	len;
{
  ByteArray	byteArray;
  OOP		byteArrayOOP;

  byteArray = (ByteArray)newInstanceWith(byteArrayClass, len);
  memcpy(byteArray->bytes, bytes, len);

  byteArrayOOP = allocOOP(byteArray);
  initEmptyBytes(byteArrayOOP, len);

  return (byteArrayOOP);
}



Byte *
toByteArray (byteArrayOOP)
     OOP	byteArrayOOP;
{
  Byte		*result;
  int		len;
  ByteArray	byteArray;

  byteArray = (ByteArray)oopToObj(byteArrayOOP);
  len = oopNumFields(byteArrayOOP);
  result = (Byte *)xmalloc(len);
  memcpy(result, byteArray->bytes, len);

  return (result);
}

void
setOOPBytes (byteArrayOOP, bytes)
     OOP	byteArrayOOP;
     Byte	*bytes;
{
  ByteArray	byteArray;
  long		len;

  len = oopNumFields(byteArrayOOP);
  byteArray = (ByteArray)oopToObj(byteArrayOOP);
  memcpy(byteArray->bytes, bytes, len);
}



OOP
messageNewArgs (selectorOOP, argsArray)
     OOP	selectorOOP, argsArray;
{
  Message	message;

  message = (Message)newInstance(messageClass);
  message->selector = selectorOOP;
  message->args = argsArray;

  return (allocOOP(message));
}

OOP
cObjectNewTyped (cObjPtr, typeOOP)
     voidPtr	cObjPtr;
     OOP	typeOOP;
{
  CObject	cObject;
  CType		cType;

  cType = (CType)oopToObj(typeOOP);

  cObject = (CObject)newInstanceWith(cType->cObjectType, 1);
  cObject->type = typeOOP;
  setCObjectValueObj(cObject, cObjPtr);

  return (allocOOP(cObject));
}

OOP
allocCObject (classOOP, size)
     OOP		  classOOP;
     unsigned long	size;
{
  voidPtr	space;
  OOP		typeOOP, cobjOOP;
  IncPtr	incPtr;

  space = (voidPtr)xmalloc((int)size);

  incPtr = incSavePointer();
  typeOOP = cTypeNew(classOOP);
  incAddOOP(typeOOP);

  cobjOOP = cObjectNewTyped(space, typeOOP);

  incRestorePointer(incPtr);

  return cobjOOP;
}

void
freeCObject (cObjOOP)
     OOP	cObjOOP;
{
  CObject	cObject;

  cObject = (CObject)oopToObj(cObjOOP);
  free ((voidPtr)cObjectValueObj(cObject));

  /* at least make it not point to falsely valid storage */
  setCObjectValueObj(cObject, nil);
}

/* Create and return a new instance of class CType.  The cObjectSubclassOOP
   parameter is used to create the appropriate subclass of CObject when a
   CObject is created from this type. */
OOP
cTypeNew (cObjectSubclassOOP)
     OOP cObjectSubclassOOP;
{
  CType		cType;

  cType = (CType)newInstance(cTypeClass);
  cType->cObjectType = cObjectSubclassOOP;
  return (allocOOP(cType));
}
