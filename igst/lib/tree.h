/***********************************************************************
 *
 *	Semantic Tree information declarations.
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



#ifndef __GSTTREE__
#define __GSTTREE__

extern char *nilName;

typedef enum {
  methodNodeType,
  unaryExprType,
  binaryExprType,
  keywordExprType,
  variableNodeType,
  keywordListType,
  variableListType,
  statementListType,
  returnExprType,
  assignExprType,
  constExprType,
  symbolNodeType,
  arrayEltListType,
  blockNodeType,
  cascadedMessageNodeType,
  messageListType
} NodeType;

#undef TreeNode
typedef struct TreeNodeStruct *TreeNode;

typedef struct ListNodeStruct {
  char		*name;
  TreeNode 	value;
  TreeNode 	next;
  TreeNode	*nextAddr;
} ListNode;

typedef struct ExprNodeStruct {
  TreeNode	receiver;
  OOP		selector;
  TreeNode	expression;
} ExprNode;

typedef enum {
  intConst,
  floatConst,
  charConst,
  stringConst,
  symbolConst,
  arrayConst
} ConstType;

typedef struct ConstNodeStruct {
  ConstType	constType;
  union {
    long	iVal;
    double	fVal;
    Byte	cVal;
    char	*sVal;
    OOP		symVal;
    TreeNode	aVal;
  } val;
} ConstNode;

typedef struct MethodNodeStruct {
  TreeNode 	selectorExpr;
  TreeNode 	temporaries;
  int		primitiveIndex;
  TreeNode 	statements;
} MethodNode;

typedef struct BlockNodeStruct {
  TreeNode 	arguments;
  TreeNode 	temporaries;
  TreeNode 	statements;
} BlockNode;


struct TreeNodeStruct {
  NodeType	nodeType;
  union {
    ListNode		nvList;
    ExprNode		nvExpr;
    ConstNode		nvConst;
    MethodNode		nvMethod;
    BlockNode		nvBlock;
  } 		nodeVal;
};

#define vBlock		nodeVal.nvBlock
#define vList		nodeVal.nvList
#define vExpr		nodeVal.nvExpr
#define vConst		nodeVal.nvConst
#define vMethod		nodeVal.nvMethod


extern TreeNode		makeMethod(), makeUnaryExpr(), makeBinaryExpr(),
			makeKeywordExpr(), makeVariable(), makeKeywordList(),
			makeVariableList(), makeStatementList(), makeAssign(),
			makeReturn(), makeIntConstant(), makeFloatConstant(),
			makeCharConstant(),makeSymbolConstant(),
			makeStringConstant(), makeArrayConstant(),
			internIdent(), internBinOP(), internKeywordList(),
			makeArrayElt(), makeBlock(), makeCascadedMessage(),
			makeMessageList();

extern void		addNode(), freeTree(), printTree();

extern mst_Boolean 		hadError;


#endif /* __GSTTREE__ */
