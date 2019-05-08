/***********************************************************************
 *
 *	C Callin facility
 *
 *	This module provides the routines necessary to allow C code to
 *	invoke Smalltalk messages on objects.
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
#include "lib.h"
#include "interp.h"
#include "callin.h"
#include "dict.h"
#include "sym.h"
#include "oop.h"
#include "comp.h"
#include "lex.h"
#include <stdio.h>
#if STDC_HEADERS
#include <stdlib.h>
#endif /* STDC_HEADERS */

#ifdef HAVE_STDARG_H
# include <stdarg.h>
#else
# include <varargs.h>
#endif


/* Simple control over oop registry size */
#define INITIAL_REGISTRY_SIZE	100

/*
 * The registry of OOPs which have been passed to C code.  A vector of
 * of oops, running from 0 to registryIndex, some of which may be nilOOP.
 * the current allocated size of the registry is registrySize, and the
 * registry may be reallocated to a larger size as need.  The registry
 * is examined at GC time to ensure that OOPs that C code knows about don't
 * go away.  "C code" here means user level C code, not Smalltalk internal
 * code.
 */
static OOP	*oopRegistry;
static int	registrySize, registryIndex;


#ifdef HAVE_STDARG_H
OOP
msgSend (OOP receiver, OOP selector, ...)
#else
OOP
msgSend (va_alist)
     va_dcl
#endif
{
  va_list	args;
#ifndef HAVE_STDARG_H
  OOP 		receiver, selector;
#endif
  OOP		anArg, result;
  int		numArgs;

#ifdef HAVE_STDARG_H
  va_start(args, selector);
#else
  va_start(args);
#endif

  if (!smalltalkInitialized) { initSmalltalk(); }

#ifndef HAVE_STDARG_H
  receiver = va_arg(args, OOP);
  selector = va_arg(args, OOP);
#endif
  
  /* These objects don't need to be incubated because they are being
   * pushed onto the Smalltalk stack which will make them visible to the GC
   */
  prepareExecutionEnvironment();
  pushOOP(receiver);
  for (numArgs = 0; (anArg = va_arg(args, OOP)) != nil; numArgs++) {
    pushOOP(anArg);
  }

  sendMessage(selector, numArgs, false);
  interpret();
  result = popOOP();
  finishExecutionEnvironment();

  return (result);
}

OOP
vmsgSend (receiver, selector, args)
     OOP receiver;
     OOP selector;
     OOP *args;
{
  OOP		anArg, result;
  int		numArgs;

  if (!smalltalkInitialized) { initSmalltalk(); }

  /* These objects don't need to be incubated because they are being
   * pushed onto the Smalltalk stack which will make them visible to the GC
   */
  prepareExecutionEnvironment();
  pushOOP(receiver);
  for (numArgs = 0; (anArg = *args++) != nil; numArgs++) {
    pushOOP(anArg);
  }

  sendMessage(selector, numArgs, false);
  interpret();
  result = popOOP();
  finishExecutionEnvironment();

  return (result);
}

OOP
nvmsgSend (receiver, selector, args, nargs)
     OOP receiver;
     OOP selector;
     OOP *args;
     int nargs;
{
  OOP		result;
  int		numArgs;

  if (!smalltalkInitialized) { initSmalltalk(); }

  /* These objects don't need to be incubated because they are being
   * pushed onto the Smalltalk stack which will make them visible to the GC
   */
  prepareExecutionEnvironment();
  pushOOP(receiver);
  for (numArgs = 0; nargs--; numArgs++) {
    pushOOP(*args++);
  }

  sendMessage(selector, numArgs, false);
  interpret();
  result = popOOP();
  finishExecutionEnvironment();

  return (result);
}

#ifdef HAVE_STDARG_H
OOP
strMsgSend (OOP receiver, ...)
#else
OOP
strMsgSend (va_alist)
     va_dcl
#endif
{
  va_list	args;
#ifndef HAVE_STDARG_H
  OOP 		receiver;
#endif
  OOP 		selector, anArg, result;
  int		numArgs;
  IncPtr	incPtr;

#ifdef HAVE_STDARG_H
  va_start(args, receiver);
#else
  va_start(args);
#endif

  if (!smalltalkInitialized) { initSmalltalk(); }

#ifndef HAVE_STDARG_H
  receiver = va_arg(args, OOP);
#endif
  selector = internString(va_arg(args, char *));
  
  /* It's possible (there is an existing case) that in sendMessage a GC can
     occur, and selector is not protected
     pb: but isn't the selector a Symbol, and hence in the root set? */
  incPtr = incSavePointer();
  incAddOOP(selector);

  prepareExecutionEnvironment();
  pushOOP(receiver);
  for (numArgs = 0; (anArg = va_arg(args, OOP)) != nil; numArgs++) {
    pushOOP(anArg);
  }

  sendMessage(selector, numArgs, false);
  interpret();
  result = popOOP();
  finishExecutionEnvironment();

  incRestorePointer(incPtr);
  return (result);
}

/* Use like printf */
#ifdef HAVE_STDARG_H
void
msgSendf (voidPtr resultPtr, ...)
#else
void
msgSendf (va_alist)
     va_dcl
#endif
{
  va_list	args;
  OOP 		selector, anArg, result;
  int		numArgs;
#ifndef HAVE_STDARG_H
  voidPtr	*resultPtr;
#endif
  char		*fmt, *fp, *s, selectorBuf[256];
  IncPtr	incPtr;

#ifdef HAVE_STDARG_H
  va_start(args, resultPtr);
#else
  va_start(args);
#endif

  if (!smalltalkInitialized) { initSmalltalk(); }

#ifndef HAVE_STDARG_H
  resultPtr = va_arg(args, voidPtr *);
#endif

  fmt = va_arg(args, char *);
  
  incPtr = incSavePointer();

  prepareExecutionEnvironment();

  numArgs = -1;
  for (s = selectorBuf, fp = &fmt[2]; *fp; fp++) {
    if (*fp == '%') {
      fp++;
      numArgs++;
      switch (*fp) {
      case 'i':
	pushInt(va_arg(args, long));
	break;

      case 'f':
	anArg = floatNew(va_arg(args, double));
	pushOOP(anArg);
	break;

      case 'b':
	if (va_arg(args, int)) {
	  pushOOP(trueOOP);
	} else {
	  pushOOP(falseOOP);
	}
	break;

      case 'c':
	anArg = charOOPAt(va_arg(args, char));
	pushOOP(anArg);
	break;

      case 'C':
	anArg = cObjectNew(va_arg(args, voidPtr));
	pushOOP(anArg);
	break;
	
      case 's':
	anArg = stringNew(va_arg(args, char *));
	pushOOP(anArg);
	break;

      case 'S':
	anArg = internString(va_arg(args, char *));
	pushOOP(anArg);
	break;

      case 'o':
	anArg = va_arg(args, OOP);
	pushOOP(anArg);
	break;

      case 't':			/* type string, followed by a void * */
	{
	  OOP		ctype;
	  ctype = typeNameToOOP(va_arg(args, char *));
	  incAddOOP(ctype);

	  anArg = cObjectNewTyped(va_arg(args, voidPtr), ctype);
	  pushOOP(anArg);
	}
	break;
	  
		
      case 'T':			/* existing type instance, and a void * */
	{
	  OOP		ctype;
	  ctype = va_arg(args, OOP);
	  anArg = cObjectNewTyped(va_arg(args, voidPtr), ctype);
	  pushOOP(anArg);
	}
	break;

      case '%':
	*s++ = '%';
	numArgs--;
	break;
      }
    } else if (*fp != ' ' && *fp != '\t') {
      *s++ = *fp;
    }
  }

  *s = '\0';

  selector = internString(selectorBuf);

  incAddOOP(selector);		/* not automatically protected! */
  /* pb: but isn't the selector a Symbol, and hence in the root set? */

  sendMessage(selector, numArgs, false);
  interpret();
  result = popOOP();
  finishExecutionEnvironment();

  if (resultPtr) {
    switch (fmt[1]) {
    case 'i':
      *(int *)resultPtr = toInt(result);
      break;

    case 'c':
      *(char *)resultPtr = charOOPValue(result);
      break;

    case 'C':
      /* !!! Fix this -- it is ugly, but OS/2 compilers don't like it without */
      *(voidPtr *)resultPtr = cObjectValue(result);
      break;

    case 's':
      *(char **)resultPtr = (char *)toCString(result);
      break;

    case 'b':
      *(int *)resultPtr = (result == trueOOP);
      break;

    case 'f':
      *(double *)resultPtr = floatOOPValue(result);
      break;

    case 'v':			/* don't care about the result */
      break;			/* "v" for "void"  */

    case '?':
      *(long *)resultPtr = OOPToC(result);
      break;

    case 'o':
    default:
      *(OOP *)resultPtr = result;
      break;
    }
  }

  incRestorePointer(incPtr);
}

OOP
typeNameToOOP (name)
     char *name;
{
  OOP		result;
  char		buf[300];

  sprintf(buf, "^%s!", name);

  result = evalExpr(buf);
  return (result);
}


void
evalCode (str)
     char	*str;
{
  if (!smalltalkInitialized) { initSmalltalk(); }
  compileCode = true;
  /* initLexer(); */
  pushCString(str);
  parseStream();
  popStream(false);
}


/*
 *	OOP evalExpr(str)
 *
 * Description
 *
 *	Evaluate a single Smalltalk expression and return the result.
 *
 * Inputs
 *
 *	str   : A Smalltalk method body.  Can have local variables, but no
 *		parameters.  This is much like the immediate expression
 *		evaluation that the command interpreter provides.
 *
 * Outputs
 *
 *	
 */
OOP
evalExpr (str)
     char	*str;
{
  OOP		result;

  if (!smalltalkInitialized) { initSmalltalk(); }

  //compileCode = true;
  /* initLexer(); */
  pushCString(str);
  parseStream();
  popStream(false);
  result = lastReturnedValue;
  
  return (result);
}

OOP
objectAlloc (classOOP, size)
     OOP classOOP;
     unsigned long size;
{
  if (classIsIndexable(classOOP)) {
    return (instantiateOOPWith(classOOP, size));
  } else {
    return (allocOOP(instantiate(classOOP)));
  }
}


/***********************************************************************
 *
 *	Conversion *to* Smalltalk datatypes routines
 *
 ***********************************************************************/

OOP
classNameToOOP (name)
     char *name;
{
  OOP		result, key;

  key = symbolToOOP(name);			   /* this inits Smalltalk */
  result = dictionaryAt(smalltalkDictionary, key);
  return (result);
}


OOP
intToOOP (i)
     long	i;
{
  if (!smalltalkInitialized) { initSmalltalk(); }

  return (fromInt(i));
}

OOP
floatToOOP (f)
     double	f;
{
  return (registerOOP(floatNew(f)));
}

OOP
boolToOOP (b)
     int	b;
{
  if (!smalltalkInitialized) { initSmalltalk(); }

  if (b) {
    return (trueOOP);
  } else {
    return (falseOOP);
  }
}


OOP
charToOOP (c)
     char	c;
{
  if (!smalltalkInitialized) { initSmalltalk(); }

  return (charOOPAt(c));
}


/* !!! Add in byteArray support sometime soon */

OOP
stringToOOP (str)
     char	*str;
{
  if (!smalltalkInitialized) { initSmalltalk(); }

  if (str == nil) {
    return (nilOOP);
  } else {
    return (registerOOP(stringNew(str)));
  }
}

OOP
symbolToOOP (str)
     char	*str;
{
  if (!smalltalkInitialized) { initSmalltalk(); }

  if (str == nil) {
    return (nilOOP);
  } else {
    /* Symbols don't get freed, so the new OOP doesn't need to be registered */
    return (internString(str));
  }
}

OOP
cObjectToOOP (co)
     voidPtr co;
{
  if (!smalltalkInitialized) { initSmalltalk(); }

  if (co == nil) {
    return (nilOOP);
  } else {
    return (registerOOP(cObjectNew(co)));
  }
}

OOP
cObjectToTypedOOP (co, typeOOP)
     voidPtr co;
     OOP typeOOP;
{
  if (!smalltalkInitialized) { initSmalltalk(); }

  if (co == nil) {
    return (nilOOP);
  } else {
    return (registerOOP(cObjectNewTyped(co, typeOOP)));
  }
}


/***********************************************************************
 *
 *	Conversion *from* Smalltalk datatypes routines
 *
 ***********************************************************************/

/* ### need a type inquiry routine */

long
OOPToC (oop)
     OOP	oop;
{
  if (!smalltalkInitialized) { initSmalltalk(); }

  if (isInt(oop)) {
    return (toInt(oop));
  
  } else if (oopClass(oop) == trueClass || oopClass(oop) == falseClass) {
    return (oop == trueOOP);
  
  } else if (oopClass(oop) == charClass) {
    return (charOOPValue(oop));
  
  } else if (isNil(oop)) {
    return (nil);
  
  } else if (isAKindOf(oopClass(oop), cObjectClass)) {
    return ((long) cObjectValue(oop));

  } else {
    return (nil);
  }
}

long
OOPToInt (oop)
     OOP	oop;
{
  if (!smalltalkInitialized) { initSmalltalk(); }

  return (toInt(oop));
}

double OOPToFloat (oop)
     OOP	oop;
{
  if (!smalltalkInitialized) { initSmalltalk(); }

  return (floatOOPValue(oop));
}

int
OOPToBool (oop) 
     OOP	oop;
{
  if (!smalltalkInitialized) { initSmalltalk(); }

  return (oop == trueOOP);
}

char
OOPToChar (oop)
     OOP	oop;
{
  if (!smalltalkInitialized) { initSmalltalk(); }

  return (charOOPValue(oop));
}

char
*OOPToString (oop)
     OOP	oop;
{
  if (!smalltalkInitialized) { initSmalltalk(); }

  if (isNil(oop)) {
    return (nil);
  } else {
    return ((char *)toCString(oop));
  }
}

/* !!! add in byteArray support soon */

voidPtr
OOPToCObject (oop)
     OOP	oop;
{
  if (!smalltalkInitialized) { initSmalltalk(); }

  if (isNil(oop)) {
    return (nil);
  } else {
    return (cObjectValue(oop));
  }
}



/***********************************************************************
 *
 *	Bookkeeping routines
 *
 ***********************************************************************/


void
initOOPRegistry ()
{
  oopRegistry = (OOP *)xmalloc(INITIAL_REGISTRY_SIZE * sizeof(OOP));
  registrySize = INITIAL_REGISTRY_SIZE;
  registryIndex = 0;
}

OOP
registerOOP (oop)
     OOP	oop;
{
  if (!smalltalkInitialized) { initSmalltalk(); }

  if (registryIndex >= registrySize) {
    registrySize += INITIAL_REGISTRY_SIZE;
    oopRegistry = (OOP *)xrealloc(oopRegistry, registrySize * sizeof(OOP));
  }

  oopRegistry[registryIndex++] = oop;
  return (oop);
}

void
unregisterOOP (oop)
     OOP	oop;
{
  signed int	i;

  if (!smalltalkInitialized) { initSmalltalk(); }

  for (i = registryIndex; i >= 0; i--) {
    if (oopRegistry[i] == oop) {
      oopRegistry[i] = oopRegistry[--registryIndex];
      break;
    }
  }

}



/*
 *	void markRegisteredOOPs()
 *
 * Description
 *
 *	Called at GC time, marks registered objects
 *	and compresses out unregistered objects.
 *
 */
void
markRegisteredOOPs ()
{
  int		maxIndex, i;
  OOP		oop;

  maxIndex = 0;
  for (i = 0; i < registryIndex; i++) {
    oop = oopRegistry[i];
    maybeMarkOOP(oop);
  }

  registryIndex = maxIndex;
}

