/***********************************************************************
 *
 *	Symbol Table declarations
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


#ifndef __GSTSYM__
#define __GSTSYM__

#define INITIAL_SYMBOL_TABLE_SIZE	521

/* Uncomment this to enable profiling of the symbol table */
/*#define symbol_table_profiling */

typedef enum {
  temporaryScope,
  receiverScope,
  poolScope,
  globalScope
} ScopeType;



typedef struct SymbolStruct {
  OBJ_HEADER;			/* I love inheritance */
  char		symString[1];
} *Symbol;

typedef struct SymbolEntryStruct {
  ScopeType	scope;
  OOP		symbol;
  int		varIndex;	/* index of receiver or temporary */
  unsigned int	scopeDistance;	/* how many frames up the stack is this
				   variable from where we are? */
} *SymbolEntry;

extern SymbolEntry	findVariable();

extern OOP		atColonSymbol, atColonPutColonSymbol, sizeSymbol,
			nextSymbol, nextPutColonSymbol, atEndSymbol, 
  			classSymbol, blockCopyColonTemporariesColonSymbol, 
			valueSymbol, isNilSymbol, notNilSymbol,
			valueColonValueColonSymbol,
			valueColonValueColonValueColonSymbol,
 			valueWithArgumentsColonSymbol,
			valueColonSymbol, doColonSymbol, newSymbol,
			newColonSymbol, plusSymbol, minusSymbol,
			lessThanSymbol, greaterThanSymbol, lessEqualSymbol,
			greaterEqualSymbol, equalSymbol, notEqualSymbol,
			toColonDoColonSymbol, timesSymbol,
			toColonByColonDoColonSymbol,
			timesRepeatColonSymbol, divideSymbol, remainderSymbol,
			bitShiftColonSymbol, integerDivideSymbol,
			bitAndColonSymbol, bitOrColonSymbol, sameObjectSymbol,
  			notSameObjectSymbol, finalizeSymbol, yourselfSymbol,
  			whileTrueColonSymbol, whileFalseColonSymbol,
			ifTrueColonSymbol, ifFalseColonSymbol, 
			ifTrueColonIfFalseColonSymbol,
  			ifFalseColonIfTrueColonSymbol, andColonSymbol,
			orColonSymbol, selfSymbol, superSymbol, trueSymbol,
  			falseSymbol, nilSymbol, orSymbol, andSymbol,
			doesNotUnderstandColonSymbol, unknownSymbol,
			charSymbol, stringSymbol, stringOutSymbol, 
			symbolSymbol, intSymbol, longSymbol, floatSymbol,
			doubleSymbol, booleanSymbol,
			voidSymbol, variadicSymbol, cObjectSymbol,
			cObjectPtrSymbol, smalltalkSymbol, symbolTable,
			thisContextSymbol, byteArraySymbol, byteArrayOutSymbol;

extern OOP		internString(),
  			makeInstanceVariableArray(),
			makeClassVariableDictionary(),
			makePoolArray(), 
			internStringOOP();

extern char		*symbolAsString();

extern unsigned long	hashString();

extern int		stringOOPLen(), getArgCount(), getTempCount(),
			declareArguments(), declareTemporaries(),
			declareBlockArguments(), declareName();

extern void		printString(), printSymbol(), initSymbols(),
			pushNewScope(), popOldScope(), undeclareName(),
			freeSymbolEntry();


#endif /* __GSTSYM__ */
