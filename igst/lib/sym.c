/***********************************************************************
 *
 *	Symbol Table module.
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
#include "sym.h"
#include "oop.h"
#include "comp.h"
#include "dict.h"
#include "tree.h"
#include "lex.h"
#include <stdio.h>
#include <ctype.h>
#if STDC_HEADERS
#include <stdlib.h>
#include <string.h>
#endif /* STDC_HEADERS */

#if !defined(max)
#define	max(x, y) \
  ( ((x) > (y)) ? (x) : (y) )
#endif

#define isSymbol(oop) \
   ( !isNil(oop) && (oopClass(oop) ==  symbolClass) )

typedef struct {
  OBJ_HEADER;
  OOP		nextLink;
  OOP		symbol;
} *SymLink;

typedef struct SymbolListStruct *SymbolList;

struct SymbolListStruct {
  OOP		symbol;
  mst_Boolean	readOnly;
  int		index;
  SymbolList	prevSymbol;
};
  

/* Represents all the identifiers, both arguments and temporaries, which are
 * declared in a given scope.  Nested scopes result in nested instances of
 * the scope struct, with the current scope always being in innermost scope
 * at any point during the compilation.
 */
typedef struct ScopeStruct *Scope;
struct ScopeStruct {
  Scope		prevScope;
  unsigned int	numArguments;
  unsigned int	numTemporaries;
  SymbolList	symbols;
};



OOP			andColonSymbol, atColonPutColonSymbol, atColonSymbol,
			atEndSymbol, bitAndColonSymbol, bitOrColonSymbol,
			bitShiftColonSymbol, blockCopyColonTemporariesColonSymbol, 
			classSymbol, isNilSymbol, notNilSymbol,
			divideSymbol, doColonSymbol, equalSymbol,
			greaterEqualSymbol, greaterThanSymbol,
			ifFalseColonIfTrueColonSymbol, ifFalseColonSymbol,
			ifTrueColonIfFalseColonSymbol, ifTrueColonSymbol,
			integerDivideSymbol, lessEqualSymbol, lessThanSymbol,
			minusSymbol, newColonSymbol, newSymbol,
			nextPutColonSymbol, nextSymbol, notEqualSymbol,
			notSameObjectSymbol, orColonSymbol, plusSymbol,
			remainderSymbol, sameObjectSymbol, sizeSymbol,
			thisContextSymbol, finalizeSymbol, timesSymbol, 
			toColonDoColonSymbol, timesRepeatColonSymbol,
			toColonByColonDoColonSymbol,
			valueColonSymbol, valueColonValueColonSymbol,
			valueColonValueColonValueColonSymbol,
 			valueWithArgumentsColonSymbol,
			valueSymbol, yourselfSymbol,
			whileFalseColonSymbol, whileTrueColonSymbol, orSymbol,
			andSymbol, superSymbol, nilSymbol, trueSymbol,
			falseSymbol, selfSymbol,
			doesNotUnderstandColonSymbol, unknownSymbol,
			charSymbol, stringSymbol, stringOutSymbol, 
			symbolSymbol, intSymbol, longSymbol, floatSymbol, 
			doubleSymbol, booleanSymbol,
			voidSymbol, variadicSymbol, cObjectSymbol,
			cObjectPtrSymbol,  smalltalkSymbol, symbolTable,
			byteArraySymbol, byteArrayOutSymbol;

#ifdef unused
/**/int			adds = 0, reused = 0, reprobes = 0,
/**/			hitsOn[OOP_TABLE_SIZE];
#endif /* unused */

void 			printString();

static SymbolEntry	allocSymbolEntry();
static mst_Boolean	isSameString(), isWhiteSpace();
unsigned long		hashString();
static void		freeScopeSymbols(), parseVariableName();
static OOP		scanName(), internCountedString();
static int		instanceVariableIndex(), localVarIndex();

static Scope		curScope = nil;



int
getArgCount()
{
  return (curScope->numArguments);
}

int
getTempCount()
{
  return (curScope->numTemporaries);
}

void
pushNewScope()
{
  Scope		newScope;
  newScope = (Scope)xmalloc(sizeof(*newScope));
  newScope->prevScope = curScope;
  newScope->symbols = nil;
  newScope->numArguments = 0;
  newScope->numTemporaries = 0;
  curScope = newScope;
}

void
popOldScope()
{
  Scope		oldScope;

  oldScope = curScope;
  curScope = curScope->prevScope;
  freeScopeSymbols(oldScope);
  xfree(oldScope);
}


int
declareArguments(args)
     TreeNode args;
{
  if (args->nodeType == unaryExprType) {
    return (0);
  } else if (args->nodeType == binaryExprType) {
    declareName(args->vExpr.expression->vList.name, false);
  } else {
    for(args = args->vExpr.expression; args != nil; args = args->vList.next) {
      declareName(args->vList.value->vList.name, false);
    }
  }
  /* Arguments are always declared first! */
  curScope->numArguments = curScope->numTemporaries;
  curScope->numTemporaries = 0;
  return (curScope->numArguments);
}
  
int
declareTemporaries(temps)
     TreeNode temps;
{
  int n;
  for(n = 0; temps != nil; n++, temps = temps->vList.next) {
    declareName(temps->vList.name, true);
  }
  return (n);
}

int
declareBlockArguments(args)
     TreeNode args;
{
  for(; args != nil; args = args->vList.next) {
    declareName(args->vList.name, false);
  }
  /* Arguments are always declared first! */
  curScope->numArguments = curScope->numTemporaries;
  curScope->numTemporaries = 0;
  return (curScope->numArguments);
}

void
undeclareName()
{
  SymbolList	oldList;

  oldList = curScope->symbols;
  curScope->symbols = curScope->symbols->prevSymbol;
  free(oldList);
}


int
declareName(name, writeable)
     char	*name;
     mst_Boolean writeable;
{
  SymbolList	newList;

  newList = (SymbolList)xmalloc(sizeof(struct SymbolListStruct));
  newList->symbol = internString(name);
  newList->index = curScope->numArguments + curScope->numTemporaries;
  newList->readOnly = !writeable;
  newList->prevSymbol = curScope->symbols;

  /* Arguments are always declared first, so we can assume it is a
     temporary -- if it is not, declareArguments and declareBlockArguments
     will fix it. */
  curScope->numTemporaries++;
  curScope->symbols = newList;
  return (newList->index);
}


static void
freeScopeSymbols(scope)
     Scope	scope;
{
  SymbolList	oldList;

  for (oldList = scope->symbols; oldList != nil; oldList = scope->symbols) {
    scope->symbols = oldList->prevSymbol;
    xfree(oldList);
  }
}


OOP
findClassVariable(varName)
     OOP	varName;
{
  OOP		classOOP, assocOOP, classVariableOOP;

  if (oopClass(thisClass) == behaviorClass
      || oopClass(thisClass) == classDescriptionClass) {
    /* classDescriptions and above don't have class or pool variables */
    /* ### this isn't quite the right test; we probably should be testing
       for if we have a classClass or some derivative of that */
    return (nilOOP);
  }

  for (classOOP = thisClass; !isNil(classOOP);
       classOOP = superClass(classOOP)) {
    if (oopClass(classOOP) == metaclassClass) {
      /* pretend that metaclasses have the class variables and shared
	 pools that their instance classes do */
      classVariableOOP = metaclassInstance(classOOP);
    } else {
      classVariableOOP = classOOP;
    }
    assocOOP =
      dictionaryAssociationAt(classVariableDictionary(classVariableOOP),
			      varName);
    if (!isNil(assocOOP)) {
      return (assocOOP);
    }

    assocOOP = findSharedPoolVariable(classVariableOOP, varName);
    if (!isNil(assocOOP)) {
      return (assocOOP);
    }
  }

  return (nilOOP);
}


SymbolEntry
findVariable(varName, mustStore)
     char		*varName;
     mst_Boolean	mustStore;
{
  OOP		varAssoc, symbol;
  int		index;
  unsigned int	scopeDistance;
  Scope		scope;

  symbol = internString(varName);

  for (scope = curScope, scopeDistance = 0; scope != nil;
       scope = scope->prevScope, scopeDistance++) {
    index = localVarIndex(scope, symbol, mustStore);
    if (index >= 0) {
      return (allocSymbolEntry(temporaryScope, symbol, index, scopeDistance));
    }
  }

  index = instanceVariableIndex(symbol);
  if (index >= 0) {
    return (allocSymbolEntry(receiverScope, symbol, index, 0));
  }

  varAssoc = findClassVariable(symbol);
  if (isNil(varAssoc)) {
    return (nil);
  }

  index = addForcedObject(varAssoc);

  return (allocSymbolEntry(globalScope, varAssoc, index, 0));
}

static int
instanceVariableIndex(symbol)
     OOP	symbol;
{
  OOP		arrayOOP;
  int		index, numVars;


  if (oopClass(thisClass) == behaviorClass) {
    /* behaviors have no instance variables */
    return (-1);
  }

  arrayOOP = instanceVariableArray(thisClass);
  numVars = numOOPs(oopToObj(arrayOOP));

  for (index = 1; index <= numVars; index++) {
    if (arrayAt(arrayOOP, index) == symbol) {
      return (index-1);
    }
  }

  return (-1);
}

static int
localVarIndex(scope, symbol, mustStore)
     Scope		scope;
     OOP		symbol;
     mst_Boolean	mustStore;
{
  SymbolList	s;

  for (s = scope->symbols; s != nil && symbol != s->symbol; s = s->prevSymbol);

  if (s != nil) {		/* found one */
    return ((mustStore && s->readOnly) ? -1 : s->index);
  } else {
    return (-1);
  }
}

static SymbolEntry
allocSymbolEntry(scope, symbol, index, scopeDistance)
     ScopeType		scope;
     OOP		symbol;
     int		index;
     unsigned int	scopeDistance;
{
  SymbolEntry	ent;

  ent = (SymbolEntry)xmalloc(sizeof(struct SymbolEntryStruct));
  ent->scope = scope;
  ent->symbol = symbol;
  ent->varIndex = index;
  ent->scopeDistance = scopeDistance;
  return (ent);
}


void
freeSymbolEntry(ent)
     SymbolEntry ent;
{
  xfree(ent);
}



OOP
makeInstanceVariableArray(superclassOOP, variableString)
     OOP	superclassOOP;
     Byte	*variableString;
{
  OOP		arrayOOP, superArrayOOP, name;
  int		index, numInstanceVars, superInstanceVars;
  Byte		*p;
  IncPtr	incPtr;			

  if (variableString == nil) {
    variableString = (Byte *)"";
  }


  if (isNil(superclassOOP)) {
    superArrayOOP = nilOOP;
    superInstanceVars = numInstanceVars = 0;
  } else {
    superArrayOOP = instanceVariableArray(superclassOOP);
    superInstanceVars = numInstanceVars = numOOPs(oopToObj(superArrayOOP));
  }

  for (p = variableString; *p; ) {
    /* skip intervening whitespace */
    name = scanName(&p);
    if (!isNil(name)) {
      numInstanceVars++;
    }
  }

  if (numInstanceVars == 0) {
    return (nilOOP);		/* no instances here */
  }

  incPtr = incSavePointer();

  arrayOOP = arrayNew(numInstanceVars);
  incAddOOP(arrayOOP);

  /* inherit variables from parent */
  for (index = 1; index <= superInstanceVars; index++) {
    arrayAtPut(arrayOOP, index, arrayAt(superArrayOOP, index));
  }

  /* now add our own variables */
  for (p = variableString; *p; index++) {
    /* skip intervening whitespace */
    name = scanName(&p);
    /* don't need to add name to incubator
     * -- it's a symbol so it's already held onto
     */
    if (!isNil(name)) {
      arrayAtPut(arrayOOP, index, name);
    }
  }

  incRestorePointer(incPtr);
  return (arrayOOP);
}

OOP
makeClassVariableDictionary(classOOP, variableNames)
     OOP	classOOP;
     Byte	*variableNames;
{
  OOP		dictionaryOOP, name;
  Byte		*p;
  IncPtr	incPtr;

  if (variableNames == nil) {
    variableNames = (Byte *)"";
  }

  incPtr = incSavePointer();

  dictionaryOOP = dictionaryNew();
  incAddOOP(dictionaryOOP);

  for (p = variableNames; *p; ) {
    name = scanName(&p);
    if (!isNil(name)) {
      /* ### error if already exists */
      /* don't need to add name to incubator
       * -- it's a symbol so it's already held onto
       */
      dictionaryAtPut(dictionaryOOP, name, nilOOP);
    }
  }

  if (dictionarySize(dictionaryOOP) == 0) {
    dictionaryOOP = nilOOP;
  }

  incRestorePointer(incPtr);
  return (dictionaryOOP);
}

OOP
makePoolArray(classOOP, poolNames)
     OOP	classOOP;
     Byte 	*poolNames;
{
  OOP		pools, name;
  int		numPools, i;
  Byte		*p, *e;
  IncPtr	incPtr;

  if (poolNames == nil) {
    poolNames = (Byte *)"";
  }
  
  /* count the number of new pool names */
  for (p = poolNames, numPools = 0; *p; ) {
    parseVariableName(&p, &e);
    if (p != e) {
      numPools++;
      p = e;
    }
  }

  incPtr = incSavePointer();

  pools = nilOOP;	/* ### maybe change this to leave empty array */

  for (p = poolNames, i = 1; *p; i++) {
    name = scanName(&p);
    if (!isNil(name)) {
      /* don't need to add name to incubator -- it's a symbol so it's
       * already held onto.
       */

      /* ### error if already exists in parent?,
	 or if value isn't a dictionary */
      /* ### should I keep these as names?  or associations?  Should
	 I look up the names somewhere other than in the smalltalk
	 dictionary?  Need to check for undefineds? */
      if (pools == nilOOP) {
	pools = arrayNew(numPools);
	incAddOOP(pools);
      }
      arrayAtPut(pools, i, dictionaryAt(smalltalkDictionary, name));
    }
  }


  incRestorePointer(incPtr);
  return (pools);
}



/*
 *	static OOP scanName(pp)
 *
 * Description
 *
 *	Scan a variable name (letters and digits, initial letter), and return a
 *	symbol for it.
 *
 * Inputs
 *
 *	pp    : pointer to a pointer to the start of the string to be scanned.
 *		May be pointing at either whitespace or start of variable.  At
 *		end, points to first character after the parsed variable name,
 *		which may be NUL. 
 *
 * Outputs
 *
 *	Symbol for variable name, or nilOOP if none found.  pp points to first
 *	char after symbol name (if any).
 */
static OOP
scanName(pp)
     char	**pp;
{
  char		*end, *str;
  long		len;

  parseVariableName(pp, &end);
  len = end - *pp;
  if (len == 0) {
    return (nilOOP);
  }


  str = (char *)alloca(len + 1);
  strncpy(str, *pp, len);
  str[len] = '\0';

  *pp = end;

  return (internString(str));
}

static void
parseVariableName(pp, endp)
     Byte	**pp, **endp;
{
  register Byte	*p, *e;

  p = *pp;
  e = *endp;

  while (isWhiteSpace(*p)) {
    p++;
  }

  /* ### check for non-null here and not alnum; we've jammed on a bogus char
     and it's an error */

  /* variable name extends from p to e-1 */
  for (e = p; *e; e++) {
    if (!isalnum(*e)) {
      break;
    }
  }

  *pp = p;
  *endp = e;
}

static mst_Boolean
isWhiteSpace(c)
Byte	c;
{
   return (c == ' ' || c == '\t' || c == '\n' || c == '\f');
}



OOP
internStringOOP(stringOOP)
     OOP	stringOOP;
{
  unsigned int	len;
  char		copyBuf[100], *copyPtr;
  OOP		symbolOOP;

  len = stringOOPLen(stringOOP);
  /* do this slightly more complicated bit of code because:
   * 1) we don't want to call malloc/free if we can help it
   * 2) if we just used stringOOPChars (as we used to), we pass the
   *    *dereferenced* value of the stringOOP.  internCountedString
   *    can do allocations.  If it allocates, and the gc runs, stringOOP
   *    can move, meaning the dereferenced set of chars becomes invalid.
   *    So instead we make a non-moving copy and use that.
   */
  if (len < sizeof(copyBuf)) {
    copyPtr = copyBuf;
  } else {
    copyPtr = (char *)xmalloc(len);
  }

  memcpy(copyPtr, stringOOPChars(stringOOP), len);

  symbolOOP = internCountedString(copyPtr, len);

  if (len >= sizeof(copyBuf)) {
    xfree(copyPtr);
  }

  return symbolOOP;
}

OOP
internString(str)
     char	*str;
{
  int		len;

  len = strlen(str);
  return (internCountedString(str, len));
}

static OOP
internCountedString(str, len)
     char	*str;
     int	len;
{
  unsigned long	index;
  SymLink	link;
  Symbol	symbol;
  OOP		symbolOOP, linkOOP;
  IncPtr	incPtr;

  index = (hashString(str, len) % INITIAL_SYMBOL_TABLE_SIZE) + 1;
  for (linkOOP = arrayAt(symbolTable, index); !isNil(linkOOP);
       linkOOP = link->nextLink) {
    link = (SymLink)oopToObj(linkOOP);
    if (isSameString(str, link->symbol, len)) {
      return (link->symbol);
    }
  }

  /* no match, have to add it to head of list */

  incPtr = incSavePointer();
  symbol = (Symbol)newInstanceWith(symbolClass, (long)len);
  strncpy(symbol->symString, str, len);
  symbolOOP = allocOOP(symbol);
  initEmptyBytes(symbolOOP, len);
  symbolOOP->flags |= F_READONLY;
  incAddOOP(symbolOOP);


  link = (SymLink)newInstance(symLinkClass);
  link->nextLink = arrayAt(symbolTable, index);
  link->symbol = nilOOP;	/* keep the GC happy */

  link->symbol = symbolOOP;
  arrayAtPut(symbolTable, index, allocOOP(link));

  incRestorePointer(incPtr);
  return (symbolOOP);
}


static mst_Boolean
isSameString(str, oop, len)
     char	*str;
     OOP	oop;
     int	len;
{
  if (stringOOPLen(oop) == len) {
    return (strncmp(str, ((Symbol)oopToObj(oop))->symString, len) == 0);
  }

  return (false);
}

int
stringOOPLen(oop)
     OOP	oop;
{
  return (oopSizeBytes(oop) - (oop->flags & EMPTY_BYTES));
}


unsigned long
hashString(str, len)
     char	*str;
     int	len;
{
  unsigned long hashVal = 0;
  mst_Boolean multiply = false;

  for ( ; len > 0; multiply ^= 1, str++, len--) {
    if (multiply) {
      hashVal *= ((int)*str) + 1;
    } else {
      hashVal += *str;
    }
  }

  /* Mix and match upper bits into lower ones */
  hashVal *= ((int)(hashVal >> 24)) + 1;
  hashVal ^= hashVal >> 16;

  /* Force the two highest bits to 0 so that Smalltalk hashes are positive */
  return (hashVal & (((unsigned long) ~0 ) >> 2));
}

void
printSymbol(symbol)
     OOP	symbol;
{
  if (isNil(symbol)) {
    printf(nilName);
  } else {
    printString(symbol);
  }
}

void
printString(string)
     OOP	string;
{
  int		len;

  len = stringOOPLen(string);
  fwrite(oopToObj(string)->data, sizeof(Byte), len, stdout);
  fflush(stdout);
}

/*
 *	char *symbolAsString(symbolOOP)
 *
 * Description
 *
 *	Given a symbol, this routine returns a C string that corresponds to the
 *	name of the symbol.  The returned value is a pointer to a static area,
 *	so if it's to be used for anything other than immediate output, the
 *	caller needs to make a copy of the retured string.
 *
 * Inputs
 *
 *	symbolOOP: 
 *		An OOP for a symbol
 *
 * Outputs
 *
 *	Pointer to a C string that contains the symbol name.
 */
char *
symbolAsString(symbolOOP)
     OOP	symbolOOP;
{
  static char	stringBuf[256];	/* probably large enough for most symbols */
  unsigned int	len;
  Symbol	symbol;

  symbol = (Symbol)oopToObj(symbolOOP);

  len = stringOOPLen(symbolOOP);
  if (len >= sizeof(stringBuf)) {
    errorf("symbol name too long: %d, max is %d", len, sizeof(stringBuf));
  }
  strncpy(stringBuf, symbol->symString, len);
  stringBuf[len] = '\0';
  return (stringBuf);
}



void
checkSymbolChain()
{
  unsigned long	i;

  for (i = 1; i <= INITIAL_SYMBOL_TABLE_SIZE; i++) {
    SymLink	link;
    OOP		linkOOP;
    for (linkOOP = arrayAt(symbolTable, i); !isNil(linkOOP);
	 linkOOP = link->nextLink) {
      link = (SymLink)oopToObj(linkOOP);
      if (oopClass(linkOOP) != symLinkClass ||
	  oopClass(link->symbol) != symbolClass) {
	printf("Bad symbol %p\n", linkOOP);
	debug();
      }
    }
  }
}

void
printSymbolChain(i)
     unsigned long i;
{
  SymLink	link;
  OOP		linkOOP;
  for (linkOOP = arrayAt(symbolTable, i); !isNil(linkOOP);
       linkOOP = link->nextLink) {
    link = (SymLink)oopToObj(linkOOP);
    printf("%ld:", (unsigned long) (linkOOP - oopTable));
    printSymbol(link->symbol);
    printf("->");
  }
}


/*
 *	void printSymbols()
 *
 * Description
 *
 *	This routine is used for symbol table debugging only.
 *
 */
void
printSymbols()
{
  unsigned long	i;
  
  for (i = 1; i <= INITIAL_SYMBOL_TABLE_SIZE; i++) {
    printf("Table[%ld]:\n", i);
    printSymbolChain(i);
    printf("\n");
  }
}


void
printSymLink(symLinkOOP)
     OOP	symLinkOOP;
{
  SymLink	link;

  link = (SymLink)oopToObj(symLinkOOP);

  printObject(link->symbol);
}



void
initSymbols()
{
  andColonSymbol 	        = internString("and:");
  andSymbol			= internString("&");
  atColonPutColonSymbol         = internString("at:put:");
  atColonSymbol 	        = internString("at:");
  atEndSymbol 		        = internString("atEnd");
  bitAndColonSymbol 	        = internString("bitAnd:");
  bitOrColonSymbol 	        = internString("bitOr:");
  bitShiftColonSymbol 	        = internString("bitShift:");
  blockCopyColonTemporariesColonSymbol
		 	        = internString("blockCopy:temporaries:");
  byteArraySymbol		= internString("byteArray");
  byteArrayOutSymbol		= internString("byteArrayOut");
  booleanSymbol			= internString("boolean");
  charSymbol                    = internString("char");
  classSymbol 		        = internString("class");
  cObjectSymbol                 = internString("cObject");
  cObjectPtrSymbol              = internString("cObjectPtr");
  divideSymbol 		        = internString("/");
  doColonSymbol 	        = internString("do:");
  doesNotUnderstandColonSymbol  = internString("doesNotUnderstand:");
  floatSymbol			= internString("float");
  doubleSymbol                  = internString("double");
  equalSymbol 		        = internString("=");
  falseSymbol			= internString("false");
  finalizeSymbol		= internString("finalize");
  greaterEqualSymbol 	        = internString(">=");
  greaterThanSymbol 	        = internString(">");
  ifFalseColonIfTrueColonSymbol = internString("ifFalse:ifTrue:");
  ifFalseColonSymbol 	        = internString("ifFalse:");
  ifTrueColonIfFalseColonSymbol = internString("ifTrue:ifFalse:");
  ifTrueColonSymbol 	        = internString("ifTrue:");
  integerDivideSymbol 	        = internString("//");
  intSymbol                     = internString("int");
  isNilSymbol 	                = internString("isNil");
  lessEqualSymbol 	        = internString("<=");
  lessThanSymbol 	        = internString("<");
  longSymbol                    = internString("long");
  minusSymbol 		        = internString("-");
  newColonSymbol 	        = internString("new:");
  newSymbol 		        = internString("new");
  nextPutColonSymbol 	        = internString("nextPut:");
  nextSymbol 		        = internString("next");
  nilSymbol			= internString("nil");
  notEqualSymbol 	        = internString("~=");
  notNilSymbol 	                = internString("notNil");
  notSameObjectSymbol		= internString("~~");
  orColonSymbol 	        = internString("or:");
  orSymbol			= internString("|");
  plusSymbol 		        = internString("+");
  remainderSymbol 	        = internString("\\");
  sameObjectSymbol 	        = internString("==");
  selfSymbol			= internString("self");
  sizeSymbol 		        = internString("size");
  smalltalkSymbol		= internString("smalltalk");
  stringOutSymbol               = internString("stringOut");
  stringSymbol                  = internString("string");
  superSymbol			= internString("super");
  symbolSymbol                  = internString("symbol");
  thisContextSymbol		= internString("thisContext");
  timesSymbol 		        = internString("*");
  timesRepeatColonSymbol	= internString("timesRepeat:");
  toColonByColonDoColonSymbol	= internString("to:by:do:");
  toColonDoColonSymbol		= internString("to:do:");
  trueSymbol			= internString("true");
  unknownSymbol                 = internString("unknown");
  valueColonSymbol 	        = internString("value:");
  valueColonValueColonSymbol	= internString("value:value:");
  valueColonValueColonValueColonSymbol = internString("value:value:value:");
  valueSymbol 		        = internString("value");
  valueWithArgumentsColonSymbol = internString("valueWithArguments:");
  variadicSymbol                = internString("variadic");
  voidSymbol                    = internString("void");
  whileFalseColonSymbol	        = internString("whileFalse:");
  whileTrueColonSymbol 	        = internString("whileTrue:");
  yourselfSymbol 	        = internString("yourself");
}
