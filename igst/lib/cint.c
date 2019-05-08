/***********************************************************************
 *
 *	C - Smalltalk Interface module
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
#include "interp.h"
#include "dict.h"
#include "oop.h"
#include "sym.h"
#include "callin.h"
#include "lex.h"
#include "lib.h"
#include "sysdep.h"
#include "dld_gst.h"
#ifdef STDC_HEADERS
# include <string.h>
# include <stdlib.h>
#endif /* STDC_HEADERS */

/* brd 6/7/95 */
#include <sys/types.h>
#include <stdio.h>
#include <errno.h>
#include <sys/stat.h>

#ifdef HAVE_DIRENT_H
# include <dirent.h>
# define NAMLEN(dirent) strlen((dirent)->d_name)
#else
# define dirent direct
# define NAMLEN(dirent) (dirent)->d_namlen
# ifdef HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# ifdef HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
# ifdef HAVE_NDIR_H
#  include <ndir.h>
# endif
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif 
#ifdef HAVE_IO_H
# include <io.h>
#endif

#define ARG_VEC_SIZE		20 /* 20 shorts, 10 longs or ptrs, 5 dbls */
#define CFUNC_VEC_CHUNK_SIZE	25 /* grow in 25 slot chunks  */


typedef enum {
  intAlign,
  longAlign,
  ptrAlign,
  floatAlign,
  doubleAlign
} AlignmentType;

typedef enum {			/* types for C parameters */
  unknownType,			/* when there is no type a priori */
  charType,
  stringType,
  stringOutType,		/* for things that modify string params */
  symbolType,
  byteArrayType,
  byteArrayOutType,
  booleanType,
  intType,
  longType,
  floatType, 
  doubleType,
  voidType,			/* valid only as a return type */
  variadicType,			/* for parameters, this param is an array
				   to be interpreted as arguments.  Note that
				   only simple conversions are performed in
				   this case. */
  cObjectType,			/* a C object is being passed */
  cObjectPtrType,		/* a C object pointer is being passed */
  smalltalkType,		/* no conversion to-from C...C sees this
				   as "void *".  */
  selfType			/* pass self as the corresponding argument  */
} CDataType;

typedef struct CFuncDescriptorStruct {
  OBJ_HEADER;
  OOP		cFunction;	/* CObject whose C value is func addr */
  OOP		cFunctionName;	/* Name of C function in mapping table */
  OOP		returnType;	/* Smalltalk return type */
  OOP		numFixedArgs;	/* number of real arguments passed from */
				/* smalltalk (excluding "self" parameters */
				/* which are synthetically added by the */
				/* callout mechanism when calling the C */
				/* function). */
  OOP		argTypes[1];	/* variable length, really numFixedArgs long */
} *CFuncDescriptor;

typedef struct SymbolTypeMapStruct {
  OOP		*symbol;
  CDataType	type;
} SymbolTypeMap;

/* Holds onto allocated C strings and Smalltalk strings for the duration of
 * a call to a C function.  Used in invokeCRoutine and pushSmalltalkObj. */
typedef struct StringInfoStruct {
  Byte		*cString;
  OOP		stringOOP;
  CDataType	returnedType;
} StringInfo;

typedef union CParamUnionUnion {
  int		intVal;
  long		longVal;
  voidPtr	ptrVal;
  float		floatVal;
  double	doubleVal;
  int		valueVec[sizeof(double) / sizeof(int)];
} CParamUnion;

/* Element type for the name-to-C-function mapping table. */ 
typedef struct 	CFuncInfoStruct {
  char		*funcName;
  void		(*funcAddr)();
} CFuncInfo;

typedef struct GstStatStruct {
  unsigned short st_mode;     /* protection */
  long           st_size;     /* total size, in bytes */
  long           st_aTime;    /* time of last access */
  long           st_mTime;    /* time of last modification */
  long           st_cTime;    /* time of last change */
} GstStat;



extern mst_Boolean 		enableGC;
extern void			initUserCFuncs();


void				defineCFunc();

static void 			marli(), pushObj(), callCFunction(),
				badType(), pushSmalltalkObj(), initCFuncVec(),
				testCallin(), testCString(), testCStringArray();
static CDataType 		getCType();
static CFuncDescriptor 		getCFuncDescriptor();
static OOP			classifyTypeSymbol();
static int			savedErrno, getErrno(), my_stat(), my_putenv(),
				getArgc();
static char			*extractDirentName(), *getArgv();
static CFuncInfo		*cFuncInfo, *cFuncIndex;
static unsigned long		cFuncVecSize;

static int			cArgVec[ARG_VEC_SIZE];
static int			*cArg;
static StringInfo		*stringInfoBase, *sip;
/* printable names for corresponding C types */
static char			*cTypeName[] = {
  "void?",			/* unknownType */
  "char",			/* charType */
  "char *",			/* stringType */
  "char *",			/* stringOutType */
  "char *",			/* symbolType */
  "char *",			/* byteArrayType */
  "char *",			/* byteArrayOutType */
  "int",			/* booleanType */
  "int",			/* intType */
  "long",			/* longType */
  "float",		        /* floatType */
  "double",			/* doubleType */
  "void?",			/* voidType */
  "...",			/* variadicType */
  "void *",			/* cObjectType -- this is misleading */
  "void *",			/* cObjectPtrType */
  "void *",			/* smalltalkType */
  "self",			/* selfType */
};

static SymbolTypeMap	symbolTypeMap[] = {
  { &unknownSymbol, unknownType },
  { &charSymbol, charType },
  { &stringSymbol, stringType },
  { &stringOutSymbol, stringOutType },
  { &symbolSymbol, symbolType },
  { &byteArraySymbol, byteArrayType },
  { &byteArrayOutSymbol, byteArrayOutType },
  { &booleanSymbol, booleanType },
  { &intSymbol, intType },
  { &longSymbol, longType },
  { &floatSymbol, floatType },
  { &doubleSymbol, doubleType },
  { &voidSymbol, voidType },
  { &variadicSymbol, variadicType },
  { &cObjectSymbol, cObjectType },
  { &cObjectPtrSymbol, cObjectPtrType },
  { &smalltalkSymbol, smalltalkType },
  { &selfSymbol, selfType },
  { nil, unknownType }
};

/* the arg vec pointer must be = 0 mod alignments[align] */
/* This is quite likely to be machine dependent.  Currently it is set up
 * to work correctly on sun2's, sun3's and sun4's, and other RISC (type size == alignment) platforms */
/* The order of the array elements for alignments and typeSizes must equal the 
 * enum value order in AlignmentType */
static int 		alignments[] = {
  sizeof(int),			/* intType */
  sizeof(long),			/* longType */
  sizeof(voidPtr),		/* ptrType */
  sizeof(float),		/* floatType */
  DOUBLE_ALIGNMENT		/* doubleType */
};

static int		typeSizes[] = {
  sizeof(int),			/* intType */
  sizeof(long),			/* longType */
  sizeof(voidPtr),		/* ptrType */
  sizeof(float),		/* floatType */
  sizeof(double)		/* doubleType */
};



/*
 *	static void marli(n)
 *
 * Description
 *
 *	Test/example C function.
 *
 * Inputs
 *
 *	n     : number of times to emit message.
 *
 */
void
marli (n)
     int n;
{
  int		i;

  for (i = 0; i < n; i++) {
    printf("Marli loves Steve!!!\n");
  }
}

int
getErrno ()
{
  int returned;
  returned = savedErrno;
  savedErrno = 0;
  return (returned);
}
 
/*
 *	static int my_stat(name, out)
 *
 * Description
 *
 *	Does a stat library call.  Exists because struct stat is defined in
 *	many different ways.
 *
 * Inputs
 *
 *	str   : String to stuff into the environment.  Of the form name=value.
 *
 * Outputs
 *
 *	Returned value from putenv() call.
 */
int
my_stat (name, out)
     char	*name;
     GstStat *out;
{
  int			result;
  static struct stat	statOut;

  result = stat(name, &statOut);
  if (!errno) {
    out->st_mode  = statOut.st_mode;
    out->st_size  = statOut.st_size;
    out->st_aTime = adjustTimeZone(statOut.st_atime) - 86400*10959;
    out->st_mTime = adjustTimeZone(statOut.st_mtime) - 86400*10959;
    out->st_cTime = adjustTimeZone(statOut.st_ctime) - 86400*10959;
  }
  return (result);
}

/* ??? Maybe use #ifdef HAVE_LSTAT */
#ifdef not_used_yet
/**/static long my_lstat(name, out)
/**/char	*name;
/**/GstStat *out;
/**/{
/**/  int			result;
/**/  static struct stat	statOut;
/**/
/**/  result = lstat(name, &statOut);
/**/  if (!errno) {
/**/    out->st_mode  = statOut.st_mode;
/**/    out->st_size  = statOut.st_size;
/**/    out->st_aTime = adjustTimeZone(statOut.st_atime) - 86400*10959;
/**/    out->st_mTime = adjustTimeZone(statOut.st_mtime) - 86400*10959;
/**/    out->st_cTime = adjustTimeZone(statOut.st_ctime) - 86400*10959;
/**/  }
/**/  return (result);
/**/}
#else
#define my_lstat my_stat
#endif


/*
 *	static int my_putenv(str)
 *
 * Description
 *
 *	Does a putenv library call.  Exists because putenv (at least Sun's)
 *	expects that the string passed in will exist for the duration, and
 *	Smalltalk will free the string it passed to this this routine when
 *	control returns to it.
 *
 * Inputs
 *
 *	str   : String to stuff into the environment.  Of the form name=value.
 *
 * Outputs
 *
 *	Returned value from putenv() call.
 */
int
my_putenv (str)
     char	*str;
{
  char		*clone;
  int		len;

  len = strlen(str) + 1;	/* hold the null */
  clone = (char *)xmalloc(len);
  strcpy(clone, str);
  return (putenv(clone));
}


void
testCallin (oop)
     OOP	oop;
{
  OOP		o, sel;
  double	f;
  strMsgSend(oop, "inspect", nil);

  o = stringToOOP("abc");
  sel = symbolToOOP("printNl");
  strMsgSend(strMsgSend(o, ",", o, nil), "printNl", nil);
  msgSendf(nil, "%s %s printNl", "this is a test");
  msgSendf(&f, "%f %i + %f", 3, 4.7);
  printf("result = %f\n", f);
}

void
testCString (string)
     char **string;
{
  printf("The string is %s\n", *string);
}

void
testCStringArray (numElements, string)
     int numElements;
     char **string;
{
  int i;
  for (i = 0; i < numElements; i++) {
    printf("Str[%d] = %s\n", i, string[i]);
  }
}

char *
extractDirentName (dir)
     struct dirent *dir;
{
  return (dir->d_name);
}

int
getArgc ()
{
  return (smalltalkPassedArgc);
}

char *
getArgv (n)
     int n;
{
  return (n <= smalltalkPassedArgc ? smalltalkPassedArgv[n-1] : nil);
}



void
initCFuncs ()
{
  extern int system();
  extern char *getenv();
  initCFuncVec();

  /* Access to command line args */
  defineCFunc("getArgc", getArgc);
  defineCFunc("getArgv", getArgv);

  /* Test functions */
  defineCFunc("testCallin", testCallin);
  defineCFunc("testCString", testCString);
  defineCFunc("testCStringArray", testCStringArray);

  /* Access to C library */
  //defineCFunc("system", system);
  defineCFunc("getenv", getenv);
  defineCFunc("putenv", my_putenv);

  defineCFunc("errno", getErrno);
  defineCFunc("stat", my_stat);
  defineCFunc("lstat", my_lstat);

  defineCFunc("opendir", opendir);
  defineCFunc("closedir", closedir);
  defineCFunc("readdir", readdir);
  defineCFunc("rewinddir", rewinddir);
  defineCFunc("extractDirentName", extractDirentName);

  defineCFunc("unlink", unlink);
  defineCFunc("rename", rename);
  defineCFunc("rmdir", rmdir);
  defineCFunc("chdir", chdir);
  defineCFunc("getCurDirName", getCurDirName);

  /* Define stubs, at least */
  initDldLib();

  /* Non standard routines */
  defineCFunc("marli", marli);

  /* Initialize any user C function definitions. initUserCFuncs,
     defined in cfuncs.c, is overridden by explicit definition
     before linking with the Smalltalk library. */
  initUserCFuncs();		
}


/*
 *	static void initCFuncVec()
 *
 * Description
 *
 *	Initialize the name-to-C-function mapping table.  The map can grow
 *	as needed without limit.
 *
 */
void
initCFuncVec ()
{
  cFuncVecSize = CFUNC_VEC_CHUNK_SIZE;
  cFuncInfo = (CFuncInfo *)xmalloc(cFuncVecSize * sizeof(CFuncInfo));
  cFuncIndex = cFuncInfo;
}



/*
 *	void defineCFunc(funcName, funcAddr)
 *
 * Description
 *
 *	Defines the mapping between a string function name and the address
 *	of that function, for later use in lookupFunction.  The mapping
 *	table will expand as needed to hold new entries as they are added.
 *
 * Inputs
 *
 *	funcName: 
 *		The string name of the function to register.
 *	funcAddr: 
 *		The address of the C function to register in the mapping table.
 *
 */
void
defineCFunc (funcName, funcAddr)
     char	*funcName;
     voidPtr	funcAddr;
{
  if (cFuncIndex - cFuncInfo >= (long)cFuncVecSize) {
    unsigned long oldIndex;
    
    oldIndex = cFuncIndex - cFuncInfo;
    cFuncVecSize += CFUNC_VEC_CHUNK_SIZE;
    cFuncInfo = (CFuncInfo *)xrealloc(cFuncInfo, cFuncVecSize * sizeof(CFuncInfo));
    cFuncIndex = cFuncInfo + oldIndex;
  }

#ifdef debug_trace_on
  printf("Define C func %s(%x)\n", funcName, funcAddr);
  printf("CFuncIndex %x info %x\n", cFuncIndex, cFuncInfo);
#endif 

  cFuncIndex->funcName = strdup(funcName);
  cFuncIndex->funcAddr = funcAddr;
  cFuncIndex++;

#ifdef debug_trace_on
  printf("after defining index %d name %s(%x)\n", cFuncIndex,
	 cFuncIndex[-1].funcName, cFuncIndex[-1].funcAddr);
#endif
}



/*
 *	void (*lookupFunction(funcName))()
 *
 * Description
 *
 *	Returns the address of a C function which has been registered using
 *	defineCFunc with the name "funcName".  Returns nil if there is no
 *	such function.
 *
 * Inputs
 *
 *	funcName: 
 *		The name of the function to lookup.
 *
 * Outputs
 *
 *	Address of a C function, or nil if non was registered with "funcName"
 */
void
(*lookupFunction (funcName))()
     char	*funcName;
{
  CFuncInfo	*fip;

  for (fip = cFuncInfo; fip < cFuncIndex; fip++) {
    if (strcmp(funcName, fip->funcName) == 0) {
      return (fip->funcAddr);
    }
  }
  return (nil);
}



/*
 *	void invokeCRoutine(numArgs, methodOOP)
 *
 * Description
 *
 *	Invokes a C routine.  The Smalltalk arguments have been popped off the
 *	Smalltalk stack when this routine returns.
 *
 * Inputs
 *
 *	numArgs: 
 *		The number of actual arguments to the C function.
 *	methodOOP: 
 *		The C function wrapper method which contains the CObject
 *		which contains the C function descriptor used to control the
 *		mapping of argument types from Smalltalk to C types and
 *		determines the mapping of the C function's return type into
 *		a Smalltalk type.
 *
 */
void
invokeCRoutine (numArgs, methodOOP)
     long	numArgs;
     OOP	methodOOP;
{
  CFuncDescriptor desc;
  CDataType	cType;
  OOP		oop;
  OOP		selfObject;
  int		i, si, fixedArgs;
  StringInfo	stringInfo[ARG_VEC_SIZE], *localSip;

  cArg = cArgVec;
  
  desc = getCFuncDescriptor(methodOOP);

  stringInfoBase = stringInfo;
  sip = stringInfo;

  selfObject = stackAt(numArgs); /* 0 == stop of stack, stack at n is n away */
				 /* from the top, the receiver is pushed */
				 /* first before the other arguments */
  fixedArgs = toInt(desc->numFixedArgs);

  for (si = i = 0; i < fixedArgs; i++) {
    cType = getCType(desc, i);
    if (cType == selfType) {
      pushSmalltalkObj(selfObject, unknownType);
    } else if (cType != voidType) {
      /* Do nothing if it is a void */
      oop = stackAt(numArgs - si - 1);
      pushSmalltalkObj(oop, cType);
      si++;
    }
  }

  popNOOPs(numArgs);

  localSip = sip;

  callCFunction(desc);
  /* Fixup all returned string variables */
  for ( ; localSip-- != stringInfo; ) {
    if (localSip->returnedType == stringOutType) {
      setOOPString(localSip->stringOOP, localSip->cString);
    } else if (localSip->returnedType == byteArrayOutType) {
      setOOPBytes(localSip->stringOOP, localSip->cString);
    }
    xfree(localSip->cString);
  }
}

/*
 *	static CFuncDescriptor getCFuncDescriptor(methodOOP)
 *
 * Description
 *
 *	Retrieves the C function descriptor (a C structure, not an OOP), from a
 *	method.  This function is part of the policy that the 0th method
 *	literal in a method which is a C callout method is the holder of a
 *	CObject instance for the C function descriptor.
 *
 * Inputs
 *
 *	methodOOP: 
 *		The method to retrieve the C function descriptor from.
 *
 * Outputs
 *
 *	The C data structure which is the function descriptor.
 */
CFuncDescriptor
getCFuncDescriptor (methodOOP)
     OOP	methodOOP;
{
  OOP		associationOOP, descOOP;

  associationOOP = oopToObj(((Method)oopToObj(methodOOP))->literals)->data[0];
  descOOP = associationValue(associationOOP);
  return ((CFuncDescriptor)oopToObj(descOOP));
}

/*
 *	static CDataType getCType(desc, index)
 *
 * Description
 *
 *	Returns the C type (the internal enumeration value of argument "index"
 *	in descriptor "desc".  If index is outside the range, returns
 *	unknownType (presumably for dealing with variadic parameter lists).
 *
 * Inputs
 *
 *	desc  : A CFunction descriptor (not the OOP, the real C structure)
 *	index : Index of the argument in the descriptor to get the C data type
 *		of.  0 based.
 *
 * Outputs
 *
 *	The CDataType enumeration value corresponding to the type of the
 *	index'th parameter.
 */
CDataType
getCType (desc, index)
     CFuncDescriptor desc;
     int	index;
{
  if (index < toInt(desc->numFixedArgs)) {
    return ((CDataType)toInt(desc->argTypes[index]));
  } else {
    return (unknownType);
  }
}

void
pushSmalltalkObj (oop, cType)
     OOP	oop;
     CDataType cType;
{
  OOP		class;
  int		i;
  CParamUnion	u;

  if (cArg - cArgVec >= ARG_VEC_SIZE) {
    errorf("Attempt to push more than %d ints; extra parameters ignored",
	   ARG_VEC_SIZE);
    return;
  }

  if (isInt(oop)) {
    class = integerClass;
  } else if (oop == trueOOP || oop == falseOOP) {
    class = booleanClass;
  } else {
    class = oopClass(oop);
  }

  if (cType == smalltalkType) {
    u.ptrVal = (voidPtr)oop;
/* brd Sun Mar 19 20:52:56 GMT-0800 1995 */
/* if enableGC is true, do not put object in registry */
    if (!enableGC) {
      registerOOP(oop);		/* make sure it doesn't get gc'd */
    }
    pushObj(&u, ptrAlign);
  } else if (cType == cObjectPtrType) {
    if (isAKindOf(class, cObjectClass)) {
      u.ptrVal = cObjectValue(oop);
      pushObj(&u, ptrAlign);
    } else {
      errorf("Attempted to pass a non-CObject as a cObjectPtr");
    }
  } else if (class == integerClass) {
    if (cType == longType || cType == unknownType) {
      u.longVal = toInt(oop);
      pushObj(&u, longAlign);
    } else if (cType == intType || cType == charType) {
      u.intVal = toInt(oop);
      pushObj(&u, intAlign);
    } else {
      badType("Integer", cType);
    }
  } else if (class == booleanClass) {
    if (cType == intType || cType == booleanType || cType == charType || cType == unknownType) {
      u.intVal = (oop == trueOOP);
      pushObj(&u, intAlign);
    } else if (cType == longType) {
      u.longVal = (oop == trueOOP);
      pushObj(&u, longAlign);
    } else {
      badType("Boolean", cType);
    }
  } else if (class == charClass) {
    if (cType == charType || cType == unknownType) {
      u.intVal = charOOPValue(oop);
      pushObj(&u, intAlign);
    } else {
      badType("Character", cType);
    }
  } else if (class == stringClass) {
    if (cType == stringType || cType == stringOutType
	|| cType == unknownType) {
      if (sip - stringInfoBase >= ARG_VEC_SIZE) {
	errorf("Too many string arguments, max is %d.  Extras ignored.",
	       ARG_VEC_SIZE);
	return;
      }
      sip->cString = (Byte *)toCString(oop);
      u.ptrVal = (voidPtr)sip->cString;
      sip->stringOOP = oop;
      sip->returnedType = cType;
      sip++;
      pushObj(&u, ptrAlign);
    } else {
      badType("String", cType);
    }
  } else if (class == symbolClass) {
    if (cType == symbolType || cType == stringType || cType == unknownType) {
      if (sip - stringInfoBase >= ARG_VEC_SIZE) {
	errorf("Too many string arguments, max is %d.  Extras ignored.",
	       ARG_VEC_SIZE);
	return;
      }
      sip->cString = (Byte *)toCString(oop);
      u.ptrVal = (voidPtr)sip->cString;
      sip->stringOOP = oop;
      sip->returnedType = cType;
      sip++;
      pushObj(&u, ptrAlign);
    } else {
      badType("Symbol", cType);
    }
  } else if (class == byteArrayClass) {
    if (cType == byteArrayType || cType == byteArrayOutType
	|| cType == unknownType) {
      if (sip - stringInfoBase >= ARG_VEC_SIZE) {
	errorf("Too many string arguments, max is %d.  Extras ignored.",
	       ARG_VEC_SIZE);
	return;
      }
      sip->cString = toByteArray(oop);
      u.ptrVal = (voidPtr)sip->cString;
      sip->stringOOP = oop;
      sip->returnedType = cType;
      sip++;
      pushObj(&u, ptrAlign);
    } else {
      badType("ByteArray", cType);
    }
  } else if (class == floatClass) {
    if (cType == doubleType || cType == unknownType) {
      u.doubleVal = floatOOPValue(oop);
      pushObj(&u, doubleAlign);
    } else if (cType == floatType) {
      u.floatVal = (float)floatOOPValue(oop);
      pushObj(&u, floatAlign);
    } else {
      badType("Float", cType);
    }
  } else if (class == cObjectClass) { 
    if (cType == cObjectType || cType == unknownType) {
      u.ptrVal = cObjectValue(oop);
      pushObj(&u, ptrAlign);
    } else {
      badType("CObject", cType);
    }
  } else if ((cType == cObjectType || cType == unknownType)
	     && isAKindOf(class, cObjectClass)) {
    u.ptrVal = cObjectValue(oop);
    pushObj(&u, ptrAlign);
  } else if (class == undefinedObjectClass) { /* how to encode nil */
    switch (cType) {
    case cObjectType:
    case stringType:
    case symbolType:
    case unknownType:
      u.ptrVal = nil;
      pushObj(&u, ptrAlign);
      break;

    default:
      badType("UndefinedObject", cType);
    }
  } else if (cType == unknownType) {
    u.ptrVal = (voidPtr)oop;
    /* brd Sun Mar 19 20:52:56 GMT-0800 1995 */
    /* if enableGC is true, do not put object in registry */
    if (!enableGC) {
      registerOOP(oop);		/* make sure it doesn't get gc'd */
    }
    pushObj(&u, ptrAlign);
  } else if (class == arrayClass) {
    if (cType == variadicType) {
      for (i = 1; i <= numOOPs(oopToObj(oop)); i++) {
        pushSmalltalkObj(arrayAt(oop, i), unknownType);
      }
    } else {
      badType("Array", cType);
    }
  } else {
    badType("Unrecognized class", cType);
  }
}

void
pushObj (up, align)
     CParamUnion *up;
     AlignmentType align;
{
  unsigned	i;
  int		alignInts;

  alignInts = alignments[ENUM_INT(align)] / sizeof(int);

  /* Align the stack properly */
  if ((cArg - cArgVec) % alignInts) {
    cArg += alignInts - ((cArg - cArgVec) % alignInts);
  }
  
  for (i = 0; i < typeSizes[ENUM_INT(align)] / sizeof(int); i++) {
    if (cArg - cArgVec >= ARG_VEC_SIZE) {
      errorf("Too many parameters, max = %d.  Extra parameters ignored",
	     ARG_VEC_SIZE);
      return;
    }
    *cArg++ = up->valueVec[i];
  }
}

void
callCFunction (desc)
     CFuncDescriptor desc;
{  
  int		intResult;
  long		longResult;
  double	doubleResult;
  int		(*cFunction)();
  CDataType	returnType;
  OOP		returnTypeOOP;

  errno = 0;

#define callCFunction(type)		\
	(* (type (*)()) cFunction)(					\
	      cArgVec[0],  cArgVec[1],  cArgVec[2],  cArgVec[3],	\
	      cArgVec[4],  cArgVec[5],  cArgVec[6],  cArgVec[7],	\
	      cArgVec[8],  cArgVec[9],  cArgVec[10], cArgVec[11],	\
	      cArgVec[12], cArgVec[13], cArgVec[14], cArgVec[15],	\
	      cArgVec[16], cArgVec[17], cArgVec[18], cArgVec[19]);

  cFunction = (int (*)())cObjectValue(desc->cFunction);
  if (!cFunction) {
    char *funcName;
    funcName = toCString(desc->cFunctionName);
    errorf ("Called invalid C call-out %s", funcName);
    free(funcName);
    setStackTop(nilOOP);
    return;
  }

  if (isInt(desc->returnType)) {
    returnType = (CDataType)toInt(desc->returnType);
    returnTypeOOP = nil;
  } else {
    returnTypeOOP = desc->returnType;
    returnType = cObjectType;
  }
    
  switch (returnType) {
  case voidType:
    callCFunction(void);
    break;

  case charType:
    intResult = callCFunction(int);
    setStackTop(charOOPAt((Byte)intResult));
    break;

  case booleanType: 
    intResult = callCFunction(int);
    setStackTopBoolean(intResult);
    break;

  case intType:
    intResult = callCFunction(int);
    setStackTopInt((long)intResult);
    break;

  case longType:
    longResult = callCFunction(long);
    setStackTop(fromInt(longResult));
    break;

  case stringType:
  case stringOutType:
  case symbolType:
  case cObjectType:
  case smalltalkType:
    longResult = callCFunction(long);
    if (longResult == 0) {
      setStackTop(nilOOP);
      break;
    }
    if(returnType == symbolType) {
      setStackTop(internString((char *)longResult));
      break;
    }
    if(returnType == cObjectType) {
      if (returnTypeOOP) {
        setStackTop(cObjectNewTyped((voidPtr)longResult, returnTypeOOP));
      } else {
        setStackTop(cObjectNew((voidPtr)longResult));
      }
      break;
    }
    if(returnType == smalltalkType) {
      setStackTop((OOP)longResult);
      break;
    }
    setStackTop(stringNew((char *)longResult));
    if(returnType == stringOutType) {
      xfree((char *)longResult);
    }
    break;

  case doubleType:
    doubleResult = callCFunction(double);
    setStackTop(floatNew(doubleResult));
    break;

  case floatType:
    doubleResult = (double) callCFunction(float);
    setStackTop(floatNew(doubleResult));
    break;

  default:
    errorf("Invalid C function return type specified, index %d\n",
	   returnType);
    break;
  }

  /* ENOTEMPTY and EEXIST are synonymous; some systems use one, and some use
     the other. We always uses EEXIST which is provided by all systems. */

#ifdef ENOTEMPTY
  savedErrno = (errno == ENOTEMPTY) ? EEXIST : errno;
#else
  savedErrno = errno;
#endif
}

void
badType (smalltalkTypeName, cType)
     char	*smalltalkTypeName;
     CDataType cType;
{
  errorf("Attempt to pass a %s as a %s", smalltalkTypeName,
	 cTypeName[ENUM_INT(cType)]);
}



/*
 *	OOP makeDescriptor(funcNameOOP, returnTypeOOP, argsOOP)
 *
 * Description
 *
 *	Makes a C based descriptor for a callout-able function.  Returns a
 *	CFuncDescriptor object which holds onto the descriptor.  This
 *	descriptor is subsequently used when the called out function is
 *	invoked. 
 *
 * Inputs
 *
 *	funcNameOOP: 
 *		A Smalltalk string which contains the name of the C function to
 *		be invoked.
 *	returnTypeOOP: 
 *		A CType object or a symbol which indicates the return type.
 *	argsOOP: 
 *		An Array containing the argument type names.
 *
 * Outputs
 *
 *	CFuncDescriptor object which is used later when the callout function is
 *	invoked. 
 */
OOP
makeDescriptor (funcNameOOP, returnTypeOOP, argsOOP)
     OOP	funcNameOOP, returnTypeOOP, argsOOP;
{
  char		*funcName;
  void		(*funcAddr)();
  int		numArgs, i;
  CFuncDescriptor desc;
  IncPtr	incPtr;
  OOP		cFunction, cFunctionName;

  funcName = (char *)toCString(funcNameOOP);
  funcAddr = lookupFunction(funcName);
#ifdef now_handled_in_smalltalk
/**/  if (funcAddr == nil) {
/**/    errorf("No C function called %s is registered", funcName);
/**/  }
#endif

  if (argsOOP == nilOOP) {
    numArgs = 0;
  } else {
    numArgs = numOOPs(oopToObj(argsOOP));
  }

  /*
   * since these are all either ints or new objects, I'm not moving the
   * oops
   */
  incPtr = incSavePointer();

  cFunction = cObjectNew(funcAddr);
  incAddOOP(cFunction);

  cFunctionName = stringNew(funcName);
  incAddOOP(cFunctionName);

  desc = (CFuncDescriptor)newInstanceWith(cFuncDescriptorClass, numArgs);
  desc->cFunction = cFunction;
  desc->cFunctionName = cFunctionName;
  desc->numFixedArgs = fromInt(numArgs);
  desc->returnType = classifyTypeSymbol(returnTypeOOP, true);
  for (i = 1; i <= numArgs; i++) {
    desc->argTypes[i - 1] = classifyTypeSymbol(arrayAt(argsOOP, i), false);
  }

  xfree(funcName);
  incRestorePointer(incPtr);
  return (allocOOP(desc));
}

OOP
classifyTypeSymbol (symbolOOP, isReturn)
     OOP	symbolOOP;
     mst_Boolean	isReturn;
{
  SymbolTypeMap	*sp;
  char		*symbolName;

  for (sp = symbolTypeMap; sp->symbol != nil; sp++) {
    if (*sp->symbol == symbolOOP) {
      return (fromInt(sp->type));
    }
  }

  if (isReturn) {
    if (isAKindOf(oopClass(symbolOOP), cTypeClass)) {
      return (symbolOOP);	/* this is the type we want! */
    }
  }

  symbolName = toCString(symbolOOP); /* yeah, yeah...but they have the same
				        representation! */
  errorf("Unknown data type symbol: %s", symbolName);

  xfree(symbolName);

  return (fromInt(unknownType));
}

/*
 *	void restoreCFuncDescriptor(cFuncDescOOP)
 *
 * Description
 *
 *	This routine is called during image loading to restore a C function
 *	descriptor pointer.  This is because between the time that the image
 *	was made and now, the executable image may have changed, so any
 *	reference to the C function address may be invalid.  We therefore just
 *	perform the function lookup again and use that value.
 *
 * Inputs
 *
 *	cFuncDescOOP: 
 *		A C function descriptor object to be adjusted.  Contains the
 *		name of the function to be looked up.
 *
 */
void
restoreCFuncDescriptor (cFuncDescOOP)
     OOP	cFuncDescOOP;
{
  CFuncDescriptor desc;
  void		(*funcAddr)();
  char		*funcName;

  desc = (CFuncDescriptor)oopToObj(cFuncDescOOP);
  funcName = (char *)toCString(desc->cFunctionName);
  funcAddr = lookupFunction(funcName);
  xfree(funcName);
  setCObjectValue(desc->cFunction, funcAddr);
}

