/* this is -*- C -*- ok?!? */

/***********************************************************************
 *
 *	Byte code interpreter primitives include file
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


#ifndef __GSTPRIM_INLINES__
#define __GSTPRIM_INLINES__

#define intBinOp(op, noOverflow)                \
    oop2 = popOOP();				\
    oop1 = popOOP();				\
    if (isInt(oop1) && isInt(oop2)) {		\
      register long iarg1, iarg2;		\
      iarg1 = toInt(oop1);			\
      iarg2 = toInt(oop2);			\
						\
      op;                                       \
      if(noOverflow || !INT_OVERFLOW(iarg1)) {	\
        pushInt(iarg1);			        \
        return (false);				\
      }                                         \
    }						\
    unPop(2);					\
    return (true)

#define boolBinOp(operator)				\
    oop2 = popOOP();					\
    oop1 = popOOP();					\
    if (isInt(oop1) && isInt(oop2)) {			\
      pushBoolean( ((long)oop1) operator ((long)oop2) )	\
      return (false);					\
    }							\
    unPop(2);						\
    return (true)

/*
 *	static mst_Boolean executePrimitiveOperation(primitive, numArgs, methodOOP)
 *
 * Description
 *
 *	This routine provides the definitions of all of the primitive methods
 *	in the GNU Smalltalk system.  It normally removes the arguments to the
 *	primitive methods from the stack, but if the primitive fails, the
 *	arguments are put back onto the stack and this routine returns false,
 *	indicating failure to invoke the primitive.
 *
 * Inputs
 *
 *	primitive: 
 *		A C int that indicates the number of the primitive to invoke.
 *		Must be > 0.
 *	numArgs: 
 *		The number of arguments that the primitive has.
 *	methodOOP: 
 *		The OOP for the currently executing method.  This allows
 *		primitives to poke around in the method itself, to get at
 *		pieces that they need.  Normally, this is only used by the C
 *		callout routine to get at the compiled-in descriptor for the
 *		called C function.
 *
 * Outputs
 *
 *	False if the execution of the primitive operation succeeded, true if it
 *	failed for some reason.
 */
static inline mst_Boolean executePrimitiveOperation(primitive, numArgs, methodOOP)
     int	primitive, numArgs;
     OOP	methodOOP;
{
  mst_Boolean	atEof, *boolAddr;
  register OOP	oop1;
  OOP		oop2, oop3, oop4, oopVec[4], byteArrayOOP;
  register long	arg2;
  long		arg1, arg3, arg4;
  int		i, ch;
  char		*fileName, *fileMode, *realFileName;
  FILE		*file;
  FileStream	fileStream;
  Semaphore	sem;

#ifdef PROFBLOCK
  primitives[primitive]++;
#endif

  switch (primitive) {
  case 1: intBinOp(iarg1 += iarg2, false);		/* Integer + arg  */
  case 2: intBinOp(iarg1 -= iarg2, false);		/* Integer - arg */
  case 3: boolBinOp(<);		        /* Integer < arg */
  case 4: boolBinOp(>);		        /* Integer > arg */
  case 5: boolBinOp(<=);	        /* Integer <= arg */
  case 6: boolBinOp(>=);	        /* Integer >= arg */
  case 7: boolBinOp(==);	        /* Integer =, == arg */
  case 8: boolBinOp(!=);        	/* Integer ~=, ~~ arg */
  case 9: intBinOp(iarg1 = mulWithCheck(iarg1, iarg2), false);	/* Integer * arg */
  case 10:			        /* Integer / arg */
				        /* quotient as exact as possible */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop1) && isInt(oop2)) {
      register long iarg1, iarg2;
      iarg1 = toInt(oop1);
      iarg2 = toInt(oop2);
      if (iarg2 != 0 && (iarg1 % iarg2) == 0) {
	pushInt(iarg1 / iarg2);
	return (false);
      }
    }
    unPop(2);
    return (true);

  case 11:			/* Integer \\ arg */
				/* remainder truncated towards -infinity */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop1) && isInt(oop2)) {
      register long iarg1, iarg2;
      iarg1 = toInt(oop1);
      iarg2 = toInt(oop2);
      if (iarg2 != 0) {
	if ((iarg1 ^ iarg2) < 0) {
	  /* ??? help...is there a better way to do this? */
	  pushInt(iarg1 - ((iarg1 - (iarg2-1)) / iarg2) * iarg2);
	  return (false);
	} else {
	  pushInt(iarg1 % iarg2);
	  return (false);
	}
      }
    }
    unPop(2);
    return (true);

  case 12:			/* Integer // arg */
				/* quotient truncated towards -infinity */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop1) && isInt(oop2)) {
      register long iarg1, iarg2;
      iarg1 = toInt(oop1);
      iarg2 = toInt(oop2);
      if (iarg2 != 0) {
	if ((iarg1 ^ iarg2) < 0) { /* differing signs => negative result */
	  pushInt((iarg1 - (iarg2-1)) / iarg2);
	  return (false);
	} else {
	  pushInt(iarg1 / iarg2);
	  return (false);
	}
      }
    }
    unPop(2);
    return (true);

  case 13:			/* Integer quo: arg */
				/* quotient truncated towards 0 */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop1) && isInt(oop2)) {
      register long iarg1, iarg2;
      iarg1 = toInt(oop1);
      iarg2 = toInt(oop2);
      if (iarg2 != 0) {
	pushInt(iarg1 / iarg2);
	return (false);
      }
    }
    unPop(2);
    return (true);

  case 14: intBinOp(iarg1 &= iarg2, true);		/* Integer bitAnd: arg */
  case 15: intBinOp(iarg1 |= iarg2, true);		/* Integer bitOr: arg */
  case 16: intBinOp(iarg1 ^= iarg2, true);		/* Integer bitXor: arg */
  case 17:			        /* Integer bitShift: arg */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop1) && isInt(oop2)) {
      register long iarg1, iarg2;
      iarg1 = toInt(oop1);
      iarg2 = toInt(oop2);
      if (iarg2 > 0 && iarg2 <= ST_INT_SIZE) {
        long guard = iarg1 >> (ST_INT_SIZE - iarg2);
        if(guard == (guard >> 1)) {
	  pushInt(iarg1 << iarg2);
	  return (false);
	}
      } else if (iarg2 <= 0) {
	pushInt(iarg1 >> -iarg2);
        return (false);
      }
    }
    unPop(2);
    return (true);


  case 40:
    oop1 = popOOP();
    if (isInt(oop1)) {
      pushOOP(floatNew((double)toInt(oop1)));
      return (false);
    }
    unPop(1);
    return (true);

  case 41: case 42: case 43: case 44:
  case 45: case 46: case 47: case 48:
  case 49: case 50:
    oop2 = popOOP();
    oop1 = popOOP();
    if (isClass(oop1, floatClass) && isClass(oop2, floatClass)) {
      register double farg1, farg2;
      farg1 = floatOOPValue(oop1);
      farg2 = floatOOPValue(oop2);
      switch (primitive) {
      case 41:	pushOOP(floatNew(farg1 + farg2));	break;
      case 42:	pushOOP(floatNew(farg1 - farg2));	break;
      case 43:	pushBoolean(farg1 < farg2); 		break;
      case 44:	pushBoolean(farg1 > farg2);		break;
      case 45:	pushBoolean(farg1 <= farg2);		break;
      case 46:	pushBoolean(farg1 >= farg2);		break;
      case 47:	pushBoolean(farg1 == farg2);		break;
      case 48:	pushBoolean(farg1 != farg2);		break;
      case 49:	pushOOP(floatNew(farg1 * farg2));	break;
      case 50:
	if (farg2 != 0.0) {
	  pushOOP(floatNew(farg1 / farg2));
	}
	else {
          unPop(2);
          return (true);
        }
      }
      return (false);
    }

    unPop(2);
    return (true);

  case 51:			/* Float truncated */
    oop1 = popOOP();
    if (isClass(oop1, floatClass)) {
      double oopValue = floatOOPValue(oop1);
      if ((oopValue >= MIN_ST_INT) && oopValue <= MAX_ST_INT) {
	pushInt(/* 0 + ?why?*/(long)oopValue);
	return (false);
      }
    }
    unPop(1);
    return (true);

  case 52:			/* Float fractionPart */
    oop1 = popOOP();
    if (isClass(oop1, floatClass)) {
      register double farg1;
      double fdummy;
      farg1 = floatOOPValue(oop1);
      if (farg1 < 0.0) {
	farg1 = -farg1;
      }
      pushOOP(floatNew(modf(farg1, &fdummy)));
      return (false);
    } 
    unPop(1);
    return (true);

  case 53:			/* Float exponent */
    oop1 = popOOP();
    if (isClass(oop1, floatClass)) {
      register double farg1;
      int intArg1;

      farg1 = floatOOPValue(oop1);
      if (farg1 == 0.0) {
	intArg1 = 1;
      } else {
	frexp(floatOOPValue(oop1), &intArg1);
      }
      pushInt(intArg1-1);
      return (false);
    }
    unPop(1);
    return (true);

  case 54:			/* Float timesTwoPower: */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isClass(oop1, floatClass) && isInt(oop2)) {
      register double farg1;
      register long iarg2;
      farg1 = floatOOPValue(oop1);
      iarg2 = toInt(oop2);
#if !defined(HAVE_LDEXP)
      pushOOP(floatNew(scalbn(farg1, iarg2)));
#else
      pushOOP(floatNew(ldexp(farg1, iarg2)));
#endif
      return (false);
    }
    unPop(2);
    return (true);


  case 60:			/* Object at:, Object basicAt: */
    oop2 = popOOP();
    oop1 = stackTop();
    if (isInt(oop2)) {
      arg2 = toInt(oop2);
      if (checkIndexableBoundsOf(oop1, arg2)) {
	setStackTop(indexOOP(oop1, arg2));
	return (false);
      }
    }
    unPop(1);
    return (true);

  case 61:			/* Object at:put:, Object basicAt:put: */
    oop3 = popOOP();
    oop2 = popOOP();
    oop1 = stackTop();
    if (isInt(oop2) && !oopIsReadOnly(oop1)) {
      arg2 = toInt(oop2);
      if (checkIndexableBoundsOf(oop1, arg2)) {
	if (indexOOPPut(oop1, arg2, oop3)) {
	  setStackTop(oop3);
	  return (false);
	}
      }
    }

    unPop(2);
    return (true);

  case 62:			/* Object basicSize; Object size; String size;
				   ArrayedCollection size */
    oop1 = popOOP();
    pushInt(numIndexableFields(oop1));
    return (false);

  case 63:			/* String at:; String basicAt: */
    oop2 = popOOP();
    oop1 = stackTop();
    if (isInt(oop2)) {
      arg2 = toInt(oop2);
      if (checkIndexableBoundsOf(oop1, arg2)) {
	setStackTop(indexStringOOP(oop1, arg2));
	return (false);
      }
    }

    unPop(1);
    return (true);

  case 64:			/* String basicAt:put:; String at:put: */
    oop3 = popOOP();
    oop2 = popOOP();
    oop1 = stackTop();
    if (isInt(oop2) && isClass(oop3, charClass) && !oopIsReadOnly(oop1)) {
      arg2 = toInt(oop2);
      if (checkIndexableBoundsOf(oop1, arg2)) {
	indexStringOOPPut(oop1, arg2, oop3);
	setStackTop(oop3);
	return (false);
      }
    }

    unPop(2);
    return (true);

#ifdef old_code
/**/  case 68:			/* CompiledMethod objectAt: */
/**/    oop2 = popOOP();
/**/    oop1 = stackTop();
/**/    if (isOOPCompiledMethod(oop1) && isInt(oop2)) {
/**/      arg2 = toInt(oop2);
/**/      if (validMethodIndex(oop1, arg2)) {
/**/	setStackTop(compiledMethodAt(oop1, arg2));
/**/	return (false);
/**/      }
/**/    }
/**/
/**/    unPop(1);
/**/    return (true);
/**/
/**/  case 69:			/* CompiledMethod objectAt:put: */
/**/    oop3 = popOOP();
/**/    oop2 = popOOP();
/**/    oop1 = stackTop();
/**/    if (isOOPCompiledMethod(oop1) && isInt(oop2) && !oopIsReadOnly(oop1)) {
/**/      arg2 = toInt(oop2);
/**/      if (validMethodIndex(oop1, arg2)) {
/**/	compiledMethodAtPut(oop1, arg2, oop3);
/**/	return (false);
/**/      }
/**/    }
/**/
/**/    unPop(2);
/**/    return (true);
#endif


  case 70:			/* Behavior basicNew; Behavior new;
				   Interval class new */
    oop1 = stackTop();
    if (isOOP(oop1)) {
      if (!classIsIndexable(oop1)) {
	oop1 = allocOOP(instantiate(oop1));
	setStackTop(oop1);
	return (false);
      }
    }
    return (true);

  case 71:			/* Behavior new:; Behavior basicNew: */
    oop2 = popOOP();
    oop1 = stackTop();
    if (isOOP(oop1) && isInt(oop2)) {
      if (classIsIndexable(oop1)) {
	arg2 = toInt(oop2);
	if (arg2 >= 0) {
	  oop1 = instantiateOOPWith(oop1, arg2);
	  setStackTop(oop1);
	  return (false);
	}
      }
    }

    unPop(1);
    return (true);

  case 72:			/* Object become: */
    oop2 = popOOP();
    oop1 = stackTop();
    if (isOOP(oop1) && !oopIsReadOnly(oop1)) {
      if (isOOP(oop2) && !oopIsReadOnly(oop2)) {
        swapObjects(oop1, oop2);
        /* ??? maybe we should do `self = oop2' here? */
        return (false);
      }
    }
    unPop(1);
    return (true);

  case 73:			/* Object instVarAt: */
    oop2 = popOOP();
    oop1 = stackTop();
    if (isInt(oop2)) {
      arg2 = toInt(oop2);
      if (checkBoundsOf(oop1, arg2)) {
	setStackTop(instVarAt(oop1, arg2));
	return (false);
      }
    }
    unPop(1);
    return (true);

  case 74:			/* Object instVarAt:put: */
    oop3 = popOOP();
    oop2 = popOOP();
    oop1 = stackTop();
    if (isInt(oop2)) {
      arg2 = toInt(oop2);
      if (checkBoundsOf(oop1, arg2)) {
	if (instVarAtPut(oop1, arg2, oop3)) {
	  return (false);
	}
      }
    }
    unPop(2);
    return (true);

  case 75:			/* Object asOop; Object hash; Symbol hash */
    oop1 = popOOP();
    if (isOOP(oop1)) {
      pushInt(oopIndex(oop1));
      return (false);
    }
    unPop(1);
    return (true);

  case 76:			/* SmallInteger asObject;
				   SmallInteger asObjectNoFail */
    oop1 = stackTop();
    arg1 = toInt(oop1);
    if (oopIndexValid(arg1)) {
      setStackTop(oopAt(arg1));
      return (false);
    }

    return (true);

  case 77:			/* Behavior someInstance */
    oop1 = stackTop();
    for (oop2 = oopTable; oop2 < &oopTable[TOTAL_OOP_TABLE_SLOTS]; oop2++) {
      if (oopValid(oop2) && oop1 == oopClass(oop2)) {
	setStackTop(oop2);
	return (false);
      }
    }
    return (true);

  case 78:			/* Object nextInstance */
    oop1 = stackTop();
    if (isInt(oop1) || isNil(oop1) || oop1 == trueOOP || oop1 == falseOOP) {
      /* There is just one nil, one true, one false */
      return (true);

    } else if (oopClass(oop1) == charClass) {
      /* Characters are one after another - at the end there is nilOOP */
      oop2 = oop1 + 1;
      if (charClass == oopClass(oop2)) {
	setStackTop(oop2);
	return (false);
      }
    } else {
      /* Other objects can be found in the main OOP table */
      register OOP classOOP = oopClass(oop1);
      for (++oop1; oop1 <= lastUsedOOP; oop1++) {
	if (oopValid(oop1) && classOOP == oopClass(oop1)) {
	  setStackTop(oop1);
	  return (false);
	}
      }
    }
    return(true);

#ifdef old_code
/**/  case 79:			/* CompiledMethod class newMethod:header: */
/**/    oop3 = popOOP();
/**/    oop2 = popOOP();
/**/    if (isInt(oop3) && isInt(oop2)) {
/**/      arg3 = toInt(oop3);
/**/      arg2 = toInt(oop2);
/**/      oop1 = methodNewOOP(arg2, arg3);
/**/      oopClass(oop1) = stackTop();
/**/      setStackTop(oop1);
/**/      return (false);
/**/    }
/**/    unPop(2);
/**/    return (true);
#endif

  case 79:
    /*  CompiledMethod literals:numArgs:numTemps:primitive:bytecodes:depth: */
    {
      int depth = toInt(popOOP());
      OOP bytecodesOOP = popOOP();
      int primitive = toInt(popOOP());
      int numTemps = toInt(popOOP());
      int numArgs = toInt(popOOP());
      OOP literals = popOOP();
      ByteCodes bytecodes = extractByteCodes(bytecodesOOP);

      OOP method = makeNewMethod(primitive, numArgs, numTemps, depth, literals, bytecodes);
      setStackTop(method);
      return(false);
    }


  case 80:			/* ContextPart blockCopy:temporaries: */
    oop3 = popOOP();
    oop2 = popOOP();
    oop1 = stackTop();
    if (isInt(oop2) && isInt(oop3)) {
      BlockClosure closure = (BlockClosure)newInstanceWith(blockClosureClass,
      	NUM_BLOCK_CLOSURE_EXTRA_BYTES);
      BlockContext outer = (BlockContext)oopToObj(oop1);
      OOP closureOOP;

      arg3 = toInt(oop3);
      arg2 = toInt(oop2);
      closureOOP = allocOOP(closure);
#ifdef not_needed_anymore
      if (isFake(getMethodContext(oop1))) {
        printf("############## Fake in block copy!\n");
      }
#endif
      if (outer->hasBlock != nilOOP) {
         outer->hasBlock = trueOOP;
      }
      closure->outerContext = oop1;
      closure->numArgs = toInt(oop2);
      closure->numTemps = toInt(oop3);
      /* the +2 here is to skip over the jump byte codes that follow the
	 invocation of blockCopy, so that the ipIndex points to the first
	 byte code of the block. */
      closure->initialIP = relativeByteIndex(ip, thisMethod) + 2;
      setStackTop(closureOOP);
      return (false);
    }
    unPop(2);
    return (true);

  case 81:			/* BlockContext value
				   BlockContext value:
				   BlockContext value:value:
				   BlockContext value:value:value: */
    return(sendBlockValue(numArgs));

  case 82:			/* BlockContext valueWithArguments: */
    oop2 = popOOP();
    oop1 = stackTop();
    if (isClass(oop2, arrayClass)) {
      numArgs = numIndexableFields(oop2);
      for (i = 1; i <= numArgs; i++) {
	pushOOP(arrayAt(oop2, i));
      }
      if(sendBlockValue(numArgs)) {
        popNOOPs(numArgs);
        pushOOP(oop2);
        return (true);
      } else {
        return (false);
      }
    }
    unPop(1);
    return (true);

  case 83:			/* Object perform:
				   Object perform:with:
				   Object perform:with:with:
				   Object perform:with:with:with: */
    /* pop off the arguments (if any) */
    for (i = 0; i < numArgs - 1; i++) {
      oopVec[i] = popOOP();
    }
    oop1 = popOOP();		/* the selector */
    if (isClass(oop1, symbolClass)) {
      /* push the args back onto the stack */
      for (; --i >= 0; ) {
	pushOOP(oopVec[i]);
      }
      sampleCounter++;
      sendMessage(oop1, numArgs - 1, false);
      return (false);
    }
    unPop(numArgs);
    return (true);

  case 84:			/* Object perform:withArguments: */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isClass(oop2, arrayClass) && isClass(oop1, symbolClass)) {
      numArgs = numIndexableFields(oop2);
      for (i = 1; i <= numArgs; i++) {
	pushOOP(arrayAt(oop2, i));
      }
      sampleCounter++;
      sendMessage(oop1, numArgs, false);
      return (false);
    }
    unPop(2);
    return (true);


  case 85:			/* Semaphore signal */
    oop1 = stackTop();
    {
      IntState	oldSigMask;
      oldSigMask = disableInterrupts(); /* block out everything! */

      syncSignal(oop1);
      enableInterrupts(oldSigMask);
    }
    return (false);

  case 86:			/* Semaphore wait */
    oop1 = stackTop();
    {
      IntState	oldSigMask;
      oldSigMask = disableInterrupts(); /* block out everything! */
      sem = (Semaphore)oopToObj(oop1);
      if (toInt(sem->signals) <= 0) {
        /* have to suspend */
        addLastLink(oop1, getActiveProcess());		/* move to the end of the list */
	activeProcessYield();
      } else {
        sem->signals = decrInt(sem->signals);
      }
      enableInterrupts(oldSigMask);
    }
    return (false);

  case 87:			/* Process resume */
    oop1 = stackTop();
    return (!resumeProcess(oop1));

  case 88:			/* Process yield */
    oop1 = stackTop();
    if (oop1 == getActiveProcess()) {
      setStackTop(nilOOP);		/* this is our return value */
      if (isProcessReady(oop1)) {
        sleepProcess(oop1);		/* move to the end of the list */
      }
      activeProcessYield();
      return (false);
    }
    return (true);


  case 89: /* Behavior flushCache */
    invalidateMethodCache();
    return(false);
 

  case 98:			/* Time class secondClock
				 *  -- note: this primitive has different
				 *     semantics from those defined in the
				 *     book.  This primitive returns the
				 *     seconds since/to Jan 1, 2000 00:00:00
				 *     instead of Jan 1,1901.
				 *     pb 18-9-98 -- fixed to avoid overflow
				 */
    (void)popOOP();
    pushInt(getTime() - 86400*10957);  /* 10957 = days between 1970 and 2000 */
    return (false);

  case 99:			/* Time class millisecondClock
				 * -- Note: the semantics of this primitive
				 *    are different than those described in
				 *    the book.  This primitive returns the
				 *    number of milliseconds since midnight
				 *    today. */
    (void)popOOP();
    pushInt(getMilliTime() % (24*60*60*1000));
    return (false);

  case 100:			/* Processor signal: semaphore
				 *           atMilliseconds: deltaMilliseconds
			         */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop2)) {
      arg2 = toInt(oop2);
      if (arg2 <= 0) {
	IntState oldSigMask;
	oldSigMask = disableInterrupts(); /* block out everything! */
        timeoutSem = nilOOP;
	syncSignal(oop1);
	enableInterrupts(oldSigMask);
      } else {
        timeoutSem = oop1;
        signalAfter(arg2, timeoutHandler);
        return (false);
      }
    }

    unPop(2);
    return (true);

  case 101:			/* Processor isTimeoutProgrammed */
    setStackTopBoolean(!isNil(timeoutSem));
    return (false);


  case 104:			/* String hash */
    {
      unsigned long hash;
      long	    spec;
      Byte	    *base;
      oop1 = stackTop();
      spec = oopInstanceSpec(oop1);
      if (!(spec & ((~0 << ISP_NUMFIXEDFIELDS) | ISP_ISPOINTERS | ISP_ISWORDS))) {
        base = stringOOPChars(oop1);
        hash = hashString(base, numIndexableFields(oop1));
        setStackTopInt(hash);
        return (false);
      }
      return(true);
    }

  case 105:			/* ByteArray primReplaceFrom:to:with:startingAt
				 * ByteArray replaceFrom:to:withString:startingAt:
				 * String replaceFrom:to:withByteArray:startingAt:
				 * String primReplaceFrom:to:with:startingAt:*/
    {
      OOP	srcIndexOOP, srcOOP, dstEndIndexOOP, dstStartIndexOOP, dstOOP;
      int	dstEndIndex, dstStartIndex, srcIndex, dstLen, srcLen,
      		dstRangeLen;
      long	spec;
      Byte	*dstBase, *srcBase;

      srcIndexOOP = popOOP();
      srcOOP = popOOP();
      dstEndIndexOOP = popOOP();
      dstStartIndexOOP = popOOP();
      if (isInt(srcIndexOOP) && isInt(dstStartIndexOOP)
	  && isInt(dstEndIndexOOP) && !isInt(srcOOP)) {
	spec = oopInstanceSpec(srcOOP);
	if ( !(spec & (ISP_ISWORDS | ISP_ISPOINTERS)) ) {
	  /* dstEnd is inclusive: (1 to: 1) has length 1 */
	  dstEndIndex = toInt(dstEndIndexOOP);
	  dstStartIndex = toInt(dstStartIndexOOP);
	  srcIndex = toInt(srcIndexOOP);
	  dstOOP = stackTop();
	  dstLen = numIndexableFields(dstOOP);
	  srcLen = numIndexableFields(srcOOP);
	  dstRangeLen = dstEndIndex - dstStartIndex + 1;
	  if ((dstRangeLen >= 0 && dstEndIndex <= dstLen
	       && dstStartIndex > 0)) {
	    if (dstRangeLen > 0) { /* don't do it unless somethings to copy */
	      if ((srcIndex <= srcLen) && (srcIndex > 0)
		  && (srcIndex + dstRangeLen - 1 <= srcLen)) {
		/* do the copy */
		dstBase = stringOOPChars(dstOOP);
		srcBase = stringOOPChars(srcOOP);
		memcpy(&dstBase[dstStartIndex-1], &srcBase[srcIndex-1],
		       dstRangeLen);
	      }
	    }
	    return (false);
	  }
	}
      }
	
      unPop(4);
      return (true);
    }


  case 110:			/* Object ==, Character = */
    oop2 = popOOP();
    oop1 = popOOP();
    pushBoolean(oop1 == oop2);
    return (false);

  case 111:			/* Object class */
    oop1 = popOOP();
    if (isInt(oop1)) {
      pushOOP(integerClass);
    } else {
      pushOOP(oopClass(oop1));
    }
    return (false);

    /* 112 currently not in use */


  case 113:			/* quitPrimitive */
    exit(0);
    break;			/* This does nothing :-) */

    /* 114 - 116 currently not in use */

  case 117:			/* quitPrimitive: status */
    oop1 = stackTop();
    if (isInt(oop1)) {
      arg1 = toInt(oop1);
      exit(arg1);
    }
    return (true);

    /* 118 - 127 currently not in use */


/* ------- GNU Smalltalk specific primitives begin here -------------------- */

  case 128:			/* Dictionary at: */
    oop2 = popOOP();
    oop1 = stackTop();
    setStackTop(dictionaryAt(oop1, oop2));
    return (false);

  case 129:			/* Dictionary at: put: */
    oop3 = popOOP();
    oop2 = popOOP();
    oop1 = stackTop();
    dictionaryAtPut(oop1, oop2, oop3);
    setStackTop(oop3);
    return (false);

    /* This is not defined in terms of error: in a .st file because some of
     * the required functionality may not be present when it gets first
     * invoked, say during the loading of the kernel files.  We'll redefine
     * it later.
     */
  case 130:			/* doesNotUnderstand: message */
  case 131: {
    oop2 = popOOP();
    oop1 = stackTop();
    printObject(oop1);
    if (primitive == 130) {
      printf(" did not understand selector '");
      printSymbol(messageSelector(oop2));
      printf("'\n\n");
    } else {
      printf(" error: ");
      printString(oop2);
      printf("\n\n");
    }
    showBacktrace();
    /* cannot halt - not a problem, since this is only temporary.
       The worst thing that can happen is that the image is not
       rebuilt correctly, but they'll understand it because it will
       result in loads of error messages. */

    return (false);
  }


  case 132:			/* Character class value: */
    oop2 = popOOP();
    oop1 = stackTop();
    if (isInt(oop2)) {
      arg2 = toInt(oop2);
      if (arg2 >= 0 && arg2 <= 255) {
	setStackTop(charOOPAt(arg2));
	return (false);
      }
    }
    unPop(1);
    return (true);

  case 133:			/* Character asciiValue */
    oop1 = popOOP();
    pushInt(charOOPValue(oop1));
    return (false);

  case 134:			/* Symbol class intern: aString */
    oop2 = stackTop();		/* keeps this guy referenced while we intern */
    if (isClass(oop2, stringClass)) {
      OOP internedString;
      internedString = internStringOOP(oop2);
      (void)popOOP();
      
      setStackTop(internedString);
      return (false);
    }
    unPop(1);
    return (true);

  case 135:			/* Dictionary new */
    setStackTop(dictionaryNew());
    return (false);


  case 136:			/* ByteMemory at: */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop2)) {
      arg2 = toInt(oop2);
      pushInt(*(Byte *)arg2);
      return (false);
    }
    unPop(2);
    return (true);
    
  case 137:			/* ByteMemory at:put: */
    oop3 = popOOP();
    oop2 = popOOP();
    if (isInt(oop2) && isInt(oop3)) {
      arg2 = toInt(oop2);
      arg3 = toInt(oop3);
      if (arg3 >= 0 && arg3 <= 255) {
	*(Byte *)arg2 = (Byte)arg3;
	return (false);
      }
    }
    unPop(2);
    return (true);
    
  case 138:			/* Memory addressOfOOP: oop */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isOOP(oop2)) {
      pushInt((long)oop2);
      return (false);
    }
    unPop(2);
    return (true);

  case 139:			/* Memory addressOf: oop */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isOOP(oop2)) {
      pushInt((long)oopToObj(oop2));
      return (false);
    }
    unPop(2);
    return (true);


  case 140:			/* SystemDictionary backtrace */
    showBacktrace();
    return (false);

  case 141:			/* SystemDictionary getTraceFlag: anIndex */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop2)) {
      arg2 = toInt(oop2);
      boolAddr = boolAddrIndex(arg2);
      if (boolAddr != NULL) {
	oop1 = *boolAddr ? trueOOP : falseOOP;
	pushOOP(oop1);
	return (false);
      }
    }

    unPop(2);
    return (true);

  case 142:			/* SystemDictionary setTraceFlag: anIndex
				                    to: aBoolean */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop1)) {
      arg1 = toInt(oop1);
      boolAddr = boolAddrIndex(arg1);
      if (boolAddr != NULL) {
	*boolAddr = (oop2 == trueOOP);
	setExceptFlag(true);
	return (false);
      }
    }
    
    unPop(2);
    return (true);

#ifdef unused
/**/  case 143:			/* ClassDescription comment: aString */
/**/    oop2 = popOOP();
/**/    oop1 = stackTop();
/**/    /* !!! type check oop1 */
/**/    setComment(oop1, oop2);
/**/    return (false);
#endif


  case 144:			/* CObject class alloc: nBytes */
    oop2 = popOOP();
    oop1 = stackTop();
    if (isInt(oop2)) {
      arg2 = toInt(oop2);
      setStackTop(allocCObject(oop1, arg2));
      return (false);
    }
    unPop(1);
    return (true);

  case 145:			/* Memory (?) type: aType at: anAddress */
    oop3 = popOOP();
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop3) && isInt(oop2)) {
      arg1 = toInt(oop2);
      arg2 = toInt(oop3);
      switch (arg1) {
      case 0:			/* char */
	/* may want to use Character instead? */
	pushOOP(charOOPAt(*(char *)arg2));
	return (false);
      case 1:			/* unsigned char */
	pushOOP(charOOPAt(*(unsigned char *)arg2));
	return (false);
      case 2:			/* short */
	pushInt(*(short *)arg2);
	return (false);
      case 3:			/* unsigned short */
	pushInt(*(unsigned short *)arg2);
	return (false);
      case 4:			/* long */
	pushInt(*(long *)arg2);
	return (false);
      case 5:			/* unsigned long */
	pushInt(*(unsigned long *)arg2);
	return (false);
      case 6:			/* float */
	pushOOP(floatNew(*(float *)arg2));
	return (false);
      case 7:			/* double */
	pushOOP(floatNew(*(double *)arg2));
	return (false);
      case 8:			/* string */
	if (*(char **)arg2) {
	  pushOOP(stringNew(*(char **)arg2));
	} else {
	  pushOOP(nilOOP);
	}
	return (false);
      case 9:			/* OOP */
	pushOOP(*(OOP *)arg2);
	return (false);
      case 10:			/* int */
	pushInt(*(int *)arg2);
	return (false);
      case 11:			/* unsigned int */
	pushInt(*(unsigned int *)arg2);
	return (false);
      }
    }

    unPop(3);
    return (true);


  case 146:			/* Memory (?) type: aType at: anAddress
				   put: aValue */
    oop4 = popOOP();
    oop3 = popOOP();
    oop2 = popOOP();
    /* don't pop the receiver */
    if (isInt(oop3) && isInt(oop2)) {
      arg1 = toInt(oop2);
      arg2 = toInt(oop3);
      switch (arg1) {
      case 0:			/* char */
      case 1:			/* unsigned char */
	/* may want to use Character instead? */
	if (isClass(oop4, charClass)) {
	  *(char *)arg2 = charOOPValue(oop4);
	  return (false);
	} else if (isInt(oop4)) {
	  *(char *)arg2 = (char)toInt(oop4);
	  return (false);
	}
	break;
      case 2:			/* short */
      case 3:			/* unsigned short */
	if (isInt(oop4)) {
	  *(short *)arg2 = (short)toInt(oop4);
	  return (false);
	}
	break;
      case 4:			/* long */
      case 5:			/* unsigned long */
	if (isInt(oop4)) {
	  *(long *)arg2 = toInt(oop4);
	  return (false);
	}
	break;
      case 6:			/* float */
	if (isClass(oop4, floatClass)) {
	  *(float *)arg2 = (float)floatOOPValue(oop4);
	  return (false);
	}
	break;
      case 7:			/* double */
	if (isClass(oop4, floatClass)) {
	  *(double *)arg2 = floatOOPValue(oop4);
	  return (false);
	}
	break;
      case 8:			/* string */
	if (isClass(oop4, stringClass) || isClass(oop4, symbolClass)) {
	  /* Char* cast on the right side needed because toCString returns Byte * */
	  *(char **)arg2 = (char *)toCString(oop4);
	  return (false);
	}
	break;
      case 9:			/* OOP */
	*(OOP *)arg2 = oop4;
	return (false);
      case 10:			/* int */
      case 11:			/* unsigned int */
	if (isInt(oop4)) {
	  *(int *)arg2 = toInt(oop4);
	  return (false);
	}
	break;
      }
    }

    unPop(3);
    return (true);

    /* primitives 147/148 were <CObject> at:type: and at:put:type:
       They became 183/185 because we needed the ability to dere-
       ference pointers (primitives 182/184) and wanted to use
       consecutive primitive numbers (182/189).  Their old code can
       be found at primitive 183/185 */
    
  case 149:			/* <CObject> type */
    oop1 = stackTop();
    
    if (isAKindOf(oopClass(oop1), cObjectClass)) {
      register CObject	cObject;
      cObject = (CObject)oopToObj(oop1);
      setStackTop(cObject->type);
      return (false);
    }
    return (true);
    

  case 150:			/* methodsFor: category */
    setCompilationCategory(popOOP());
    setCompilationClass(stackTop());
    displayCompilationTrace("Compiling");
    compileCode = true;
    return (false);

  case 151:			/* methodsFor: category ifTrue: condition */
    oop2 = popOOP();
    setCompilationCategory(popOOP());
    setCompilationClass(stackTop());
    if (oop2 == trueOOP) {
      displayCompilationTrace("Conditionally compiling");
    } else {
      skipCompilation = true;
      displayCompilationTrace("Conditionally skipping");
    }
    compileCode = true;
    return (false);
      


  case 152:			/* ProcessorScheduler signal: aSemaphore
				                      onInterrupt: anInteger */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop2)) {
      arg2 = toInt(oop2);
      semIntVec[arg2] = oop1;
      setSignalHandler(arg2, semIntHandler);
      /* should probably package up the old interrupt state here for return
       * so that it can be undone */
      return(false);
    }

    unPop(2);
    return(true);


  case 153:			/* SystemDictionary spaceGrowRate */
    setStackTop(floatNew((double) spaceGrowRate));
    return (false);

  case 154:			/* SystemDictionary spaceGrowRate: */
    oop1 = popOOP();
    if (isClass(oop1, floatClass)) {
      arg1 = (int) floatOOPValue(oop1);
    } else if (isInt(oop1)) {
      arg1 = toInt(oop1);
    } else {
      unPop(1);
      return (true);
    }

    if (arg1 > 0 && arg1 <= 500) {
      spaceGrowRate = arg1;
      return (false);
    }

    unPop(1);
    return (true);
    
  case 155:			/* SystemDictionary growThresholdPercent */
    setStackTop(floatNew((double) growThresholdPercent));
    return (false);

  case 156:			/* SystemDictionary growThresholdPercent: */
    oop1 = popOOP();
    if (isClass(oop1, floatClass)) {
      arg1 = (int) floatOOPValue(oop1);
    } else if (isInt(oop1)) {
      arg1 = toInt(oop1);
    } else {
      unPop(1);
      return (true);
    }
    if (arg1 > 0 && arg1 < 100) {
      growThresholdPercent = arg1;
      return (false);
    }

    unPop(1);
    return (true);

  case 157:			/* SystemDictionary growTo: numBytes */
    oop1 = popOOP();
    if (isInt(oop1)) {
      arg1 = toInt(oop1);
      if (growTo(arg1)) {
	return (false);
      }
    }

    unPop(1);
    return (true);


  case 158:			/* CObject class alloc: nbytes type: aType */
    oop3 = popOOP();
    oop2 = popOOP();
    if (isInt(oop2)) {
      arg2 = toInt(oop2);
      arg2 = (long)xmalloc(arg2);

      setStackTop(cObjectNewTyped(arg2, oop3));
      return (false);
    }
    unPop(2);
    return (true);


  case 160:			/* exp */
    oop1 = stackTop();
    if (isClass(oop1, floatClass)) {
      double farg1 = floatOOPValue(oop1);
      setStackTop(floatNew(exp(farg1)));
      return (false);
    }
    return (true);

  case 161:			/* ln */
    oop1 = stackTop();
    if (isClass(oop1, floatClass)) {
      double farg1 = floatOOPValue(oop1);
      if (isNaN(farg1) || farg1 > 0.0) {
	setStackTop(floatNew(log(farg1)));
	return (false);
      }
    }
    return (true);

#ifdef PROFBLOCK
  case 162:			/* SystemDictionary resetStatistics */
    ps.numThisContexts = 0;
    ps.numMethodAllocs = 0;
    ps.numMethodReclaims = 0;
    ps.numMethodFrees = 0;
    ps.numBlockAllocs = 0;
    ps.numValues = 0;
    ps.stackSizeSum = 0;
    ps.stackDepth = 0;
    ps.maxStackDepth = 0;
    ps.stackDepthAvgSum = 0.0;
    ps.numMinorGCs = 0;
    ps.numMajorGCs = 0;
    initByteCodeCounter();
    return (false);

  case 163:			/* SystemDictionary printStatistics */
    printf("%d thisContexts, %d methodAllocs %d reclaims %d frees\n",
	   ps.numThisContexts, ps.numMethodAllocs, ps.numMethodReclaims,
	   ps.numMethodFrees);
    printf("%d blockAllocs %d value: methods\n",
	   ps.numBlockAllocs, ps.numValues);
    printf("%d maxDepth\n", ps.maxStackDepth);
    printf("%d gc's (%d minor, %d major)\n",
    	   ps.numMinorGCs + ps.numMajorGCs, ps.numMinorGCs, ps.numMajorGCs);

    printByteCodeCounts();
    return (false);
#endif

  case 164:			/* raisedTo: aNumber -- receiver ** aNumber */
    oop2 = popOOP();
    oop1 = stackTop();
    if (isClass(oop1, floatClass) && isClass(oop2, floatClass)) {
      double farg1, farg2;
      farg1 = floatOOPValue(oop1);
      farg2 = floatOOPValue(oop2);
      setStackTop(floatNew(pow(farg1, farg2)));
      return (false);
    }
    unPop(1);
    return (true);


  case 165:			/* CObject free */
    oop1 = stackTop();
    if (isAKindOf(oopClass(oop1), cObjectClass)) {
      freeCObject(oop1);	/* free allocated space */
      setStackTop(nilOOP);
      return (false);
    }
    return (true);

  case 166:			/* sqrt -- floating result */
    oop1 = stackTop();
    if (isClass(oop1, floatClass)) {
      double farg1 = floatOOPValue(oop1);
      if (farg1 < 0.0) {
	return (true);
      }
      setStackTop(floatNew(sqrt(farg1)));
      return (false);
    }
    return (true);

  /* >>>>>> 167: HOLE <<<<<< */

  case 168:			/* ceiling */
  case 169:			/* floor */
    oop1 = stackTop();
    if (isClass(oop1, floatClass)) {
      double farg1 = floatOOPValue(oop1);
      if (isFinite(farg1)) {
	switch (primitive) {
	case 168: setStackTopInt((long)ceil(farg1));	return (false);
	case 169: setStackTopInt((long)floor(farg1));	return (false);
        }
      }
    }
    return (true);



  case 176:			/* tan */
    oop1 = stackTop();
    if (isClass(oop1, floatClass)) {
      double farg1 = floatOOPValue(oop1);
      farg1 = tan(farg1);
      if (!isFinite(farg1) || (farg1 > -1.0e8 && farg1 < 1.0e8)) {
        /* Already infinite, or in a decent range - ok */
        setStackTop(floatNew(farg1));
	return (false);
      } else {
        /* Huge, push infinity (either positive or negative) */
        setStackTop(floatNew(farg1 / 0.0));
	return (false);
      }
    }
    return (true);

  case 177:			/* sin */
  case 178:			/* cos */
  case 179:			/* arcTan */
    oop1 = stackTop();
    if (isClass(oop1, floatClass)) {
      double farg1 = floatOOPValue(oop1);
      switch (primitive) {
      case 177: setStackTop(floatNew(sin(farg1)));	return (false);
      case 178: setStackTop(floatNew(cos(farg1)));	return (false);
      case 179: setStackTop(floatNew(atan(farg1)));	return (false);
      }
    }
    return (true);

  case 180:			/* arcSin */
  case 181:			/* arcCos */
    oop1 = stackTop();
    if (isClass(oop1, floatClass)) {
      double farg1 = floatOOPValue(oop1);
      if (isNaN(farg1)) {
        return (false);		/* return NaN */
      } else if (farg1 < -1.0 || farg1 > 1.0) {
        return(true);
      }
      switch (primitive) {
      case 180: setStackTop(floatNew(asin(farg1)));	return (false);
      case 181: setStackTop(floatNew(acos(farg1)));	return (false);
      }
    }
    return (true);


  case 182: /* deref first */
  case 183: /* the real mccoy */
				/* CObject at: byteoffset type: aType */
    oop3 = popOOP();
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop2)) {
      char*  addr;
      arg2 = toInt(oop2);
      if (primitive == 182) {
	addr = *(char **)cObjectValue(oop1);
	if (addr == 0) {
	  pushOOP(nilOOP);
	  return (false);
	} 
      } else {
	addr = cObjectValue(oop1);
      }

      addr += arg2;		/* compute effective address */
      if (isInt(oop3)) {	/* int type spec means a scalar type */
	arg3 = toInt(oop3);
	
	switch (arg3) {
	case 0:
	  pushOOP(charOOPAt(*(Byte *)addr));
	  return (false);

	case 1:
	  pushOOP(charOOPAt(*(Byte *)addr));
	  return (false);

	case 2:
	  pushInt(*(short *)(addr));
	  return (false);

	case 3:
	  pushInt(*(unsigned short *)(addr));
	  return (false);

	case 4:
	  pushInt(*(long *)(addr));
	  return (false);

	case 5:
	  pushInt(*(unsigned long *)(addr));
	  return (false);

	case 6:
	  pushOOP(floatNew(*(float *)addr));
	  return (false);

	case 7:
	  pushOOP(floatNew(*(double *)addr));
	  return (false);

	case 8:
	  {
	    char **strAddr;
	    strAddr = (char **)addr;
	    if (*strAddr) {
	      pushOOP(stringNew(*strAddr));
	      return (false);
	    } else {
	      pushOOP(nilOOP);
	      return (false);
	    }
	  }
	case 9:
	  pushOOP(*(OOP *)addr);
	  return (false);

	case 10:
	  pushInt(*(int *)(addr));
	  return (false);

	case 11:
	  pushInt(*(unsigned int *)(addr));
	  return (false);
	} 

      } else {			/* non int type means use the type as the 
				   type of the effective address */
	/* It's an oddball case, but it does seem possible that
	 * oop3 could get GC'ed out of existence before it gets used,
	 * since oop3 is not on the stack, and if cObjectNewTyped could
	 * cause a GC
	 */
	IncPtr		incPtr;

	incPtr = incSavePointer();
	incAddOOP(oop3);
	setStackTop(cObjectNewTyped(addr, oop3));
	incRestorePointer(incPtr);
	return (false);
      }
    }
    unPop(3);
    return (true);

  case 184: /* set the value , deref first */
  case 185: /* set the value */
      /* CObject at: byteOffset put: aValue type: aType */
      /* I don't think that this deals properly with setting the pointer
	 value as opposed to setting the pointed-to value. */
    oop4 = popOOP();
    oop3 = popOOP();
    oop2 = popOOP();
    oop1 = stackTop();
      
    if (isInt(oop2)) {
      char*  addr;
      arg2 = toInt(oop2);
      if (primitive == 184) {
	addr = *(char **)cObjectValue(oop1);
      } else {
	addr = cObjectValue(oop1);
      }

      addr += arg2;		/* compute effective address */
      if (isInt(oop4)) {	/* int type spec means a scalar type */
	arg4 = toInt(oop4);
	switch (arg4) {
	case 0:			/* char */
	  if (isClass(oop3, charClass)) {
	    *addr = charOOPValue(oop3);
	    return (false);
	  }
	  break;

	case 1:			/* uchar */
	  if (isClass(oop3, charClass)) {
	    unsigned char *ucharAddr;
	    ucharAddr = (unsigned char *)addr;
	    *ucharAddr = charOOPValue(oop3);
	    return (false);
	  }
	  break;

	case 2:			/* short */
	  if (isInt(oop3)) {
	    short *shortAddr;
	    shortAddr = (short *)addr;
	    *shortAddr = (short)toInt(oop3);
	    return (false);
	  }
	  break;

	case 3:			/* ushort */
	  if (isInt(oop3)) {
	    unsigned short *ushortAddr;
	    ushortAddr = (unsigned short *)addr;
	    *ushortAddr = (unsigned short)toInt(oop3);
	    return (false);
	  }
	  break;

	case 4:			/* long */
	  if (isInt(oop3)) {
	    long *longAddr;
	    longAddr = (long *)addr;
	    *longAddr = toInt(oop3);
	    return (false);
	  }
	  break;

	case 5:			/* ulong */
	  if (isInt(oop3)) {
	    unsigned long *ulongAddr;
	    ulongAddr = (unsigned long *)addr;
	    *ulongAddr = toInt(oop3);
	    return (false);
	  }
	  break;

	case 6:
	  {
	    float *floatAddr;
	    floatAddr = (float *)addr;
	    if (isInt(oop3)) {
	      *floatAddr = (float)toInt(oop3);
	      return (false);
	    } else if (isClass(oop3, floatClass)) {
	      *floatAddr = (float)floatOOPValue(oop3);
	      return (false);
	    } 
	  }
	  break;

	case 7:			/* double */
	  {
	    double *doubleAddr;
	    doubleAddr = (double *)addr;
	    if (isInt(oop3)) {
	      *doubleAddr = toInt(oop3);
	      return (false);
	    } else if (isClass(oop3, floatClass)) {
	      *doubleAddr = floatOOPValue(oop3);
	      return (false);
	    }
	  }
	  break;

	case 8:			/* string */
	  {			/* note that this does not allow for
				 * replacemnt in place */
				/* to replace in place, use replaceFrom: */
	    char **strAddr;
	    strAddr = (char **)addr;
	    if (oop3 == nilOOP) {
	      *strAddr = (char *)0;
	      return (false);
	    } else {
	      if (isAKindOf(oopClass(oop3), stringClass)) {
		*strAddr = (char *)toCString(oop3);
		return (false);
	      }
	    }
	  }

	case 9:
	  *(OOP *)addr = oop3;
	  return (false);

	case 10:			/* long */
	  if (isInt(oop3)) {
	    int *longAddr;
	    longAddr = (int *)addr;
	    *longAddr = toInt(oop3);
	    return (false);
	  }
	  break;

	case 11:			/* ulong */
	  if (isInt(oop3)) {
	    unsigned int *ulongAddr;
	    ulongAddr = (unsigned int *)addr;
	    *ulongAddr = toInt(oop3);
	    return (false);
	  }
	  break;

	}
      } else {		/* non int type means use the type as the 
				   type of the effective address */
	*(voidPtr *)addr = cObjectValue(oop3);    /* IS THIS RIGHT?!? */
	return (false);
      }
    }

    unPop(3);
    return (true);

      /* subtract two "pointers" and return the difference.  The difference
	 is scaled by the scale factor parameter. */
  case 186:			/* CObject ptrDiff: subtractedPtr 
				           elementSize: anInteger
					   derefFirst: aBoolean */
    oop4 = popOOP();
    oop3 = popOOP();		/* element size */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isInt(oop3)) {
      CObject cobj1, cobj2;
      unsigned long addr1, addr2;
      arg3 = toInt(oop3);
      if (arg3 > 0) {		/* sanity check */
	cobj1 = (CObject)oopToObj(oop1);
	cobj2 = (CObject)oopToObj(oop2);
	if (oop4 == trueOOP) {
	  addr1 = *(unsigned long*)cObjectValueObj(cobj1);
	  addr2 = *(unsigned long*)cObjectValueObj(cobj2);
	} else {
	  addr1 = (unsigned long)cObjectValueObj(cobj1);
	  addr2 = (unsigned long)cObjectValueObj(cobj2);
	}

	pushInt((addr1 - addr2) / arg3);
	return (false);
      }
    }

    unPop(4);
    return (true);

  case 187:			/* {CPtr,CString} adjPtrBy: byteValue */
    oop2 = popOOP();
    oop1 = stackTop();
    if (isInt(oop2)) {
      char **addr;
      arg2 = toInt(oop2);
      addr = cObjectValue(oop1);
      *addr += arg2;
      return (false);
    }
    unPop(1);
    return (true);

  case 188:			/* CString replaceWith: aString */
    oop2 = popOOP();
    oop1 = stackTop();
    
    /* assumes the receiver is already pointing at an area of memory that is
     * the correct size; does not (re)allocate receiver's string at all.
     */

    if (isClass(oop2, stringClass) || isClass(oop2, byteArrayClass)) {
      unsigned long srcLen;
      Byte	*dstBase, *srcBase;
      srcBase = stringOOPChars(oop2);
      srcLen = numIndexableFields(oop2);

      dstBase = *(Byte **)cObjectValue(oop1);
      memcpy(dstBase, srcBase, srcLen);
      dstBase[srcLen] = '\0';	/* since it's a CString type, we NUL term it */
      return (false);
    }
    unPop(1);
    return (true);

  /* >>>>>> 189-199: HOLE <<<<<< */

    /* ??? Is this used at all */
  case 200:			/* ByteArray class fromCdata: aCObject
				                   size: anInteger */
    oop3 = popOOP();
    oop2 = popOOP();
    oop1 = stackTop();
    if (isInt(oop3)) {
      arg3 = toInt(oop3);
      byteArrayOOP = byteArrayNew(cObjectValue(oop2), arg3);
      setStackTop(byteArrayOOP);
      return (false);
    }
    unPop(2);
    return (true);

    /* ??? Is this used at all */
  case 201:			/* String class fromCData: aCObject
				   	        size: anInteger  */
    oop3 = popOOP();
    oop2 = popOOP();
    oop1 = stackTop();
    if (isInt(oop3)) {
      OOP stringOOP;
      arg3 = toInt(oop3);
      stringOOP = countedStringNew(cObjectValue(oop2), arg3);
      setStackTop(stringOOP);
      return (false);
    }
    unPop(2);
    return (true);

  case 203:			/* String asCdata: aCType */
  case 204:			/* ByteArray asCdata: aCType */
    {
      voidPtr data;
      int     size;

      oop2 = popOOP();
      oop1 = stackTop();
      if ((isClass(oop2, stringClass) && primitive == 203)
          || (isClass(oop2, byteArrayClass) && primitive == 204)) {

	if(isAKindOf(oopClass(oop2), cTypeClass)) {
          size = numIndexableFields(oop1);
          data = xmalloc(size);
          if (data) {
	    memcpy(data, oopToObj(oop1)->data, size);
	    setStackTop(cObjectNewTyped(data, oop2));
	    return(false);
	  }
	}
      }
    }
    unPop(1);
    return (true);

  /* >>>>>> 205-229: HOLE <<<<<< */


  case 230:			/* SystemDictionary monitor: aBoolean */
    oop1 = popOOP();
#ifdef USE_MONCONTROL    
    if (oop1 == trueOOP) {
      moncontrol(1);
    } else {
      moncontrol(0);
    }
#endif /* USE_MONCONTROL */
    return (false);


  case 231:			/* SystemDictionary byteCodeCounter */
    setStackTopInt(byteCodeCounter);
    return (false);

  case 232:			/* SystemDictionary debug */
    debug();			/* used to allow dbx to stop based on
				 * Smalltalk execution paths.
				 */
    return (false);


  case 233:                             /* Object isReadOnly */
    oop1 = stackTop();
    setStackTopBoolean(oopIsReadOnly(oop1));
    return(false);

  case 234:                             /* Object makeReadOnly: */
    oop2 = popOOP();
    oop1 = stackTop();
    if (oop2 == trueOOP) {
      makeOOPReadOnly(oop1, true);
      return (false);
    } else if (oop2 == falseOOP) {
      makeOOPReadOnly(oop1, false);
      return (false);
    } else {
      unPop(1);
      return (true);
    }


  case 235:			/* Behavior compileString: aString */
    oop2 = popOOP();
    oop1 = popOOP();
    if (isClass(oop2, stringClass)) {
      compileCode = true;	/* tell the lexer we do internal compiles */
      pushSmalltalkString(oop2);
      setCompilationClass(oop1);
      parseStream();
      popStream(false);		/* don't close a String! */
      pushOOP(latestCompiledMethod);
      return (false);
    }
    unPop(2);
    return (true);

  case 236:			/* Behavior compileString: aString
				            ifError: aBlock */
    oop3 = popOOP();
    oop2 = popOOP();
    oop1 = popOOP();
    if (isClass(oop2, stringClass) && isClass(oop3, blockClosureClass)) {
      mst_Boolean	oldReportErrors = reportErrors;

      if (oldReportErrors) {
	/* only clear out these guys on first transition */
	firstErrorStr = firstErrorFile = NULL;
      }
      reportErrors = false;
      compileCode = true;	/* tell the lexer we do internal compiles */
      pushSmalltalkString(oop2);
      setCompilationClass(oop1);
      parseStream();
      popStream(false);		/* don't close a String! */
      if (firstErrorStr != NULL) {
	pushOOP(oop3);		/* block context */
	if (firstErrorFile != NULL) {
	  pushOOP(stringNew(firstErrorFile));
	  xfree(firstErrorFile);
	} else {
	  pushOOP(nil);
	}
	pushInt(firstErrorLine);
	pushOOP(stringNew(firstErrorStr));
	xfree(firstErrorStr);
	firstErrorStr = firstErrorFile = NULL;
        reportErrors = oldReportErrors;
	return (sendBlockValue(3));
      } else {
        reportErrors = oldReportErrors;
	pushOOP(latestCompiledMethod);
      }
      return (false);
    }
    unPop(3);
    return (true);



  case 247:			/* FileStream fileIn */
    oop1 = stackTop();
    fileStream = (FileStream)oopToObj(oop1);
    if (isNil(fileStream->file)) {
      return (true);
    }
    file = (FILE *)cObjectValue(fileStream->file);
    fileName = toCString(fileStream->name);
    if (fileIsReadable(fileName)) {
      pushUNIXFile(file, fileName);
      parseStream();
      popStream(false);		/* we didn't open it, so we don't close it */
      return (false);
    }
    xfree(fileName);
    return (true);

  case 248:			/* FileStream fileInLine: lineNum
				 *            fileName: aString
				 *	      at: charPosInt
				 */
    oop4 = popOOP();
    oop3 = popOOP();
    oop2 = popOOP();
    oop1 = stackTop();
    fileStream = (FileStream)oopToObj(oop1);
    if (isNil(fileStream->file)) {
      return (true);
    }
    file = (FILE *)cObjectValue(fileStream->file);
    fileName = toCString(fileStream->name);
    realFileName = nil;
    if (fileIsReadable(fileName)) {
      if (isInt(oop2)
	  && (isNil(oop3) || (isClass(oop3, stringClass) && isInt(oop4)))) {
	arg2 = toInt(oop2);
	if (!isNil(oop3)) {
	  arg4 = toInt(oop4);
	  realFileName = toCString(oop3);
	} else {
	  arg4 = 0;
	}

	pushUNIXFile(file, fileName);
	setStreamInfo(arg2, realFileName, arg4);
	parseStream();
	popStream(false);		/* we didn't open it, so we don't close it */
	return (false);
      }
    }
    xfree(fileName);
    if (realFileName) {
      xfree(realFileName);
    }
    unPop(3);
    return (true);


  case 249:			/* Behavior makeDescriptorFor: funcNameString
				            returning: returnTypeSymbol
					    withArgs: argsArray */
    oop4 = popOOP();
    oop3 = popOOP();
    oop2 = popOOP();
    oop1 = popOOP();

    if (isClass(oop2, stringClass)
	&& (isClass(oop3, symbolClass)
	    || isAKindOf(oopClass(oop3), cTypeClass))
	&& (isClass(oop4, arrayClass)
	    || isClass(oop4, undefinedObjectClass))) {
      pushOOP(makeDescriptor(oop2, oop3, oop4));
      return (false);
    }
    unPop(4);
    return (true);

  /* >>>>>> 250: HOLE <<<<<< */

  case 251:			/* Object snapshot: aString */
    oop2 = popOOP();
    if (isClass(oop2, stringClass)) {
      fileName = toCString(oop2);
      saveToFile(fileName);
      xfree(fileName);
      return (false);
    }
    unPop(1);
    return (true);
    
  case 252:			/* Object basicPrint */
    printf("Object: ");
    printObject(stackTop());
    return (false);

    /* pb 1998 Nov 98 - added 253 for weak objects */
  case 253:			/* Object makeWeak */
    makeOOPWeak(stackTop());
    return (false);


  case 254:			/* FileStream>>#fileOp..., variadic */
    for (i = numArgs; --i >= 0; ) {
      oopVec[i] = popOOP();
    }
    oop1 = stackTop();
    if (!isInt(oopVec[0])) {
      unPop(numArgs);
      return(true);
    }

    arg1 = toInt(oopVec[0]);
    if (arg1 == ENUM_INT(openFilePrim) || arg1 == ENUM_INT(popenFilePrim)) {
      /* open: fileName[1] mode: mode[2] or
       * popen: command[1] dir: direction[2] */
      fileName = toCString(oopVec[1]);
      fileMode = toCString(oopVec[2]);
      if (arg1 == ENUM_INT(openFilePrim)) {
          file = openFile((char *)fileName, (char *)fileMode);
      } else {
          file = openPipe(fileName, fileMode);
      }
      if (file == NULL) {
            xfree(fileName);
            xfree(fileMode);
            unPop(numArgs);
            return(true);
      }
      setFileStreamFile(oop1, cObjectNew(file), oopVec[1], arg1 == ENUM_INT(popenFilePrim));
      xfree(fileName);
      xfree(fileMode);
      return (false);
    }

    fileStream = (FileStream)oopToObj(oop1);
    if (isNil(fileStream->file)) {
      unPop(numArgs);
      return (true);
    }
    file = (FILE *)cObjectValue(fileStream->file);
    switch (arg1) {

    case closeFilePrim:	/* FileStream close */
      if (fileStream->isPipe == trueOOP) {
	setStackTopInt(closePipe(file));
      } else {
	setStackTopInt(fclose(file));
      }
      return (false);
	
    case getCharPrim:	/* FileStream next */
      ch = getc(file);
      if (ch == EOF) {	/* cause nil to be returned */
        setStackTop(nilOOP);
      } else {
        setStackTop(charOOPAt(ch));
      }
      return (false);
	  
    case putCharPrim:	/* FileStream nextPut: aChar */
      if (isClass(oopVec[1], charClass)) {
        ch = charOOPValue(oopVec[1]);
      } else if (isInt(oopVec[1])) {
        ch = toInt(oopVec[1]);
        if (ch < -128 || ch > 255) {
          break;
        } else if (ch < 0) {
          ch += 256;
        }
      } else {
	break;
      }
      fputc(ch, file);
      gst_output(ch) ;
      return (false);

    case seekPrim:		/* FileStream position: position */
      if (fseek(file, toInt(oopVec[1]), 0) == EOF) {
	break;
      } else {
        return (false);
      }

    case tellPrim:		/* FileStream position */
      setStackTopInt(ftell(file));
      if (toInt(stackTop()) == EOF) {
	break;
      } else {
        return (false);
      }

    case eofPrim:		/* FileStream atEnd */ 
      ch = getc(file);
      atEof = feof(file);
      setStackTopBoolean(atEof);
      ungetc(ch, file);
      return (false);

    case sizePrim:
      oop1 = getOpenFileSize(fileno(file));
      if (isNil(oop1)) {
	break;
      } else {
	setStackTop(oop1);
	return (false);
      }

    case putCharsPrim:	/* only works for strings currently */
      if (isAKindOf(oopClass(oopVec[1]), stringClass)) {
#ifndef ios_device
          //fwrite(stringOOPChars(oopVec[1]), numIndexableFields(oopVec[1]), 1, file);
#endif
          // start add here
          size_t  charLen =numIndexableFields(oopVec[1]) ;
          if (charLen==0){
              charLen = 1 ;
          }
          char *result = (char*)malloc(charLen) ;
          strcpy(result,"") ;
          char* src = stringOOPChars(oopVec[1]) ;
          strncpy(result,src,numIndexableFields(oopVec[1])) ;
          gst_output(result) ;
          // end add here
          return (false);
      } else {
	break;
      }

    case getCharsPrim:	/* only works for strings */
      if (isInt(oopVec[1])) {
        OOP stringOOP;
	arg2 = toInt(oopVec[1]);
	stringOOP = newString(arg2);
	if (fread(stringOOPChars(stringOOP), arg2, 1, file) == 0) {
	  break;
	}
	setStackTop(stringOOP);
	return (false);
      }

    case flushPrim:
      fflush(file);
      return(false);

    case getBytePrim:
      ch = getc(file);
      if (ch == EOF) {	/* cause nil to be returned */
        setStackTop(nilOOP);
      } else {
        setStackTopInt(ch);
      }
      return (false);
    }

    unPop(numArgs);
    return(true);


  case 255:			/* C callout primitive */
    {
      InterpJmpBuf	localJmpBuf, *oldJmpBuf;
      mst_Boolean	oldInCCode;

      inInterpreter = false;
      oldInCCode = inCCode;
      inCCode = true;
#ifdef STACK_JMPBUFS
      oldJmpBuf = cCalloutJmpBuf;
      cCalloutJmpBuf = &localJmpBuf;
      if (setjmp(cCalloutJmpBuf->jmpBuf) == 0) {
	invokeCRoutine(numArgs, methodOOP);
      }
      cCalloutJmpBuf = oldJmpBuf;
#else
      /* ??? doesn't this have to stack somehow, in the presence of C callins? */
      if (setjmp(cCalloutJmpBuf) == 0) {
	invokeCRoutine(numArgs, methodOOP);
      }
#endif
      inCCode = oldInCCode;
      inInterpreter = !inCCode;
    }
    return (false);


    /* pb 1998 Nov 98 - added 256-257 for finalization */
  case 256:
    markOOPToFinalize(stackTop(), true);
    return (false);
    
  case 257:
    markOOPToFinalize(stackTop(), false);
    return (false);

/* brd Sun Oct 24 15:36:15 PDT 1993 */
/* added primitives 256-264 to support the browser */

 case 260: /* SystemDictionary compact */
    gcFlip();
    return(false);

    /* Using SystemDictionary>>#halt caused SIGSEGVs when done inside a C
       call-in: so I'm not using the primitive, and I made this method delegate
       to ContextPart class>>#unwind. */
#ifdef not_needed_and_much_problematic
/**/ case 261: /* SystemDictionary halt */
/**/    stopExecuting(0);
/**/    return(false);
#endif

  /* >>>>>> 261-262: HOLE <<<<<< */
  
  case 263:			/* Object specialBasicAt: */
    oop2 = popOOP();
    oop1 = stackTop();
    if (isInt(oop2)) {
      arg2 = toInt(oop2);
      if  (!isInt(oop1)) {
	setStackTop(indexOOP(oop1, arg2));
	return (false);
      }
    }
    unPop(1);
    return (true);

  case 264: /* SystemDictionary enableGC: aBoolean */
    oop1 = popOOP();
    enableGC = (oop1 == trueOOP);
    return(false);

  default:
    errorf("Unhandled primitive operation %d", primitive);
    return (true);
  }

  unPop(numArgs);
  return (true);
}

#undef intBinOp
#undef boolBinOp

#endif
