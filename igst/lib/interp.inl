/* this is -*- C -*- ok?!? */

/***********************************************************************
 *
 *	Byte code interpreter inlines
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


#ifndef __GSTINTERP_INLINES__
#define __GSTINTERP_INLINES__

#include "comp.h"

static inline long 		mulWithCheck();

#ifdef OPTIMIZE
#define equal(oop1, oop2) ((oop1) == (oop2))
#define hash(oop) oopIndex(oop)
#else
static inline mst_Boolean	equal();
static inline long		hash();
#endif

/* Enabling this means that some accessors for method object pieces, such
   as instance variables or literals, are implemented as routines, instead
   of being in-line code via macros */
/* #define ACCESSOR_DEBUGGING */

#if defined(OPTIMIZE)
/* Turn this off when we're optimizing. */
#undef ACCESSOR_DEBUGGING
#endif


#ifdef OPTIMIZE
#define receiverVariableInternal(receiver, index) \
  (oopToObj(receiver)->data[index])
#else
#define receiverVariableInternal(receiver, index) \
  (inBounds(receiver, index) ? oopToObj(receiver)->data[index] \
    : (errorf("Index out of bounds %d", index), debug(), nilOOP))
#endif /* OPTIMIZE */


#define getStackReceiverInternal(numArgs) \
  (stackAt(numArgs))

#define methodTemporaryInternal(index) \
  (temporaries[index])


#define methodLiteralInternal(index) \
  (literals[index])

#define methodVariableInternal(index) \
  (associationValue(literals[index]))

#define getMethodByteCodesInternal(methodOOP) \
  (isNil(methodOOP) ? (Byte *)nil \
   : ((Method)oopToObj(methodOOP))->bytecodes)

#define getMethodLiterals(methodOOP) \
  (oopToObj(((Method)oopToObj(methodOOP))->literals)->data)

#define getMethodStackDepth(methodOOP) \
  (getMethodHeaderInternal(methodOOP).stackDepth)

#define noParentContextInternal(methodOOP) \
  (isNil(((MethodContext)oopToObj(methodContextOOP))->sender))

#define getMethodHeaderInternal(methodOOP) \
  (((Method)oopToObj(methodOOP))->header)

#define getMethodClassInternal(contextOOP) \
  (((MethodContext)oopToObj(getMethodContext(contextOOP)))->methodClass)


#ifdef OPTIMIZE
#define storeReceiverVariableInternal(receiver, index, oop) \
  oopToObj(receiver)->data[index] = (oop)

#else
#define storeReceiverVariableInternal(receiver, index, oop) \
{  \
  OOP __storeRecVarOOP = (oop); \
  if (!inBounds(receiver, index)) { \
    errorf("Index out of bounds %d", index); \
    debug(); \
  } \
  oopToObj(receiver)->data[index] = __storeRecVarOOP; \
}
#endif /* OPTIMIZE */


#define storeMethodTemporaryInternal(index, oop) \
  temporaries[index] = (oop)

#define storeMethodVariableInternal(index, oop) \
  setAssociationValue(literals[index], oop)

#define storeMethodLiteralInternal(index, oop) \
  (literals[index] = (oop))

#ifdef OPTIMIZE
#define inBoundsInternal(oop, index) true
#else /* Not optimize */
#define inBoundsInternal(oop, index) \
  ((index) >= 0 && (index) < numOOPs(oopToObj(oop)))
#endif

#define isBlockContextInternal(contextOOP) \
  (oopClass(contextOOP) == blockContextClass)

#define relativeByteIndexInternal(bp, methodOOP) \
  ((int) ((bp) - getMethodByteCodes(methodOOP)))

#define getMethodContextInternal(contextOOP) \
  ( (isBlockContext(contextOOP)) \
    ? ((BlockContext)oopToObj(contextOOP))->home \
    : (contextOOP) )


#ifndef ACCESSOR_DEBUGGING
#define receiverVariable		receiverVariableInternal
#define getStackReceiver		getStackReceiverInternal
#define methodTemporary			methodTemporaryInternal
#define methodVariable			methodVariableInternal
#define getMethodByteCodes		getMethodByteCodesInternal
#define getMethodClass			getMethodClassInternal
#define storeReceiverVariable		storeReceiverVariableInternal
#define storeMethodTemporary		storeMethodTemporaryInternal
#define storeMethodVariable		storeMethodVariableInternal
#define inBounds			inBoundsInternal
#define isBlockContext			isBlockContextInternal
#define noParentContext			noParentContextInternal

#define methodLiteral			methodLiteralInternal
#define getMethodHeader			getMethodHeaderInternal
#define storeMethodLiteral		storeMethodLiteralInternal
#define relativeByteIndex		relativeByteIndexInternal
#define getMethodContext		getMethodContextInternal
#endif /* !ACCESSOR_DEBUGGING */

#include "oop.h"
#include "dict.h"
#include <stdio.h>


#define sendToSuper(sendSelector, sendArgs, dummy) \
    { register OOP __mclass; \
      __mclass = getMethodClass(thisContextOOP); \
      sendMessageInternal(sendSelector, sendArgs, self, superClass(__mclass)); }

#define sendMessage(sendSelector, sendArgs, dummy) \
    { register OOP __receiver; \
      __receiver = getStackReceiver(sendArgs); \
      sendMessageInternal(sendSelector, sendArgs, __receiver, \
	isInt(__receiver) ? integerClass : oopClass(__receiver)); \
    }



/*
 *	long mulWithCheck(a, b)
 *
 * Description
 *
 *	Called to handle multiplication with overflow checking.  In case of
 *	an overflow, answer OVERFLOWING_INT so that we can work it out the
 *	same way we do with adds and subtracts.
 *	Is there a better way to do this?!?
 *
 * Inputs
 *
 *	a, b   : The two factors
 *
 * Outputs
 *
 *	either a valid integer in the MIN_ST_INT to MAX_ST_INT range, or
 *      OVERFLOWING_INT
 */

static inline long
mulWithCheck(a, b)
     register long a;
     register long b;
{
#define LIMIT              ( ((long)1) << (ST_INT_SIZE / 2))
#define HIGHPART(x)        ((x) >> (ST_INT_SIZE / 2))
#define LOWPART(x)         ((x) & (LIMIT - 1))
#define COMPOSE(u, h, l)   ((l) | ((h) * LIMIT) | ((u) * LIMIT * LIMIT) )

  if ((a | b) < LIMIT) {
    return (a * b);
  } else {
    long upperBits, highBits, lowBits;

#ifdef DEBUG_BIGINT
    printf("(%ld << 15 + %ld) * (%ld << 15 + %ld) = %ld << 30 + %ld << 15 + %ld",
      HIGHPART(a), LOWPART(a), HIGHPART(b), LOWPART(b),
      HIGHPART(a) * HIGHPART(b),
      LOWPART(a) * HIGHPART(b) + LOWPART(b) * HIGHPART(a),
      LOWPART(a) * LOWPART(b));
#endif

    upperBits = HIGHPART(a) * HIGHPART(b);

    if (upperBits != 0 && upperBits != -1)
      return (OVERFLOWING_INT);

    lowBits = LOWPART(a) * LOWPART(b);
    highBits = LOWPART(a) * HIGHPART(b) + LOWPART(b) * HIGHPART(a);

    highBits += HIGHPART(lowBits);
    lowBits  &= LIMIT - 1;

    if (highBits >= LIMIT || highBits < -LIMIT)
      return (OVERFLOWING_INT);

    return (COMPOSE(upperBits, highBits, lowBits));
  }
}



#ifndef OPTIMIZE

/*
 *	mst_Boolean equal(oop1, oop2)
 *
 * Description
 *
 *	Internal definition of equality.  Returns true if "oop1" and "oop2" are
 *	the same object, false if they are not, and false and an error if they
 *	are not the same and not both Symbols.
 *
 * Inputs
 *
 *	oop1  : An OOP to be compared, typically a Symbol.
 *	oop2  : An OOP to be compared, typically a Symbol.
 *
 * Outputs
 *
 *	True if the two objects are the same object, false if not, and an error
 *	message if they are not the same and not both symbols.
 */

static inline mst_Boolean
equal(oop1, oop2)
     OOP	oop1, oop2;
{
  if (oop1 == oop2) {
    /* no brain case (ha ha ha) */
    return (true);
  }
  if (isClass(oop1, symbolClass) && isClass(oop2, symbolClass)) {
    return (false);
  }

  errorf("Internal #= called with invalid object types\n");
  printf("Object 1: ");
  printObject(oop1);
  printf("\nObject 2: ");
  printObject(oop2);
  printf("\n");
  return (false);
}

/*
 *	long hash(oop)
 *
 * Description
 *
 *	Internal hash function.  Currently defined only for symbols, but may be
 *	extended as needed for other objects.  The definition of the hash
 *	function used here must be the same as that defined in Smalltalk
 *	methods.
 *
 * Inputs
 *
 *	oop   : An OOP to be hashed.
 *
 * Outputs
 *
 *	Hash value of the OOP, or 0 and an error message if the OOP does not
 *	have a defined has value (that this routine knows how to compute).
 */
static inline long
hash(oop)
     OOP	oop;
{
  if (isOOP(oop) && oopClass(oop) == symbolClass) {
    return (oopIndex(oop));
  }

  errorf("Internal #hash called with invalid object type\n");
  printObject(oop);
  printf("\n");
  return (0);
}
#endif


#endif
