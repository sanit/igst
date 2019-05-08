/***********************************************************************
 *
 *	Byte Code definitions.
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


#ifndef __GSTBYTE__
#define __GSTBYTE__

/* ??? I don't like these being defines, but you can't do math on enums, and
 * you can't switch on variables.  I like the looks of these (lexically
 * speaking) but it does violate the guidelines about when to use defines.
 */

#define pushReceiverVariable	0
#define pushTemporaryVariable	16
#define pushLitConstant		32
#define pushLitVariable		64
#define popReceiverVariable	96
#define popTemporaryVariable	104
#define pushSpecial		112
#define returnIndexed		120
#define returnMethodStackTop	124
#define returnBlockStackTop	125
#define bigLiteralsBytecode 	126
#define breakpointBytecode 	127
#define pushIndexed		128
#define storeIndexed		129
#define popStoreIndexed		130
#define sendSelector1ExtByte	131
#define sendSelector2ExtByte	132
#define sendSuper1ExtByte	133
#define sendSuper2ExtByte	134
#define popStackTop		135
#define dupStackTop		136
#define pushActiveContext	137
#define outerTempBytecode	138
#define nopBytecode		139
#define replaceSelf		140
#define replaceOne		141	/* chosen for `1 to: ... do:' */
#define replaceIndexed		142
#define jumpShort		144
#define popJumpFalseShort	152
#define jumpLong		160
#define popJumpTrue		168
#define popJumpFalse		172
#define plusSpecial		176
#define minusSpecial		177
#define lessThanSpecial		178
#define greaterThanSpecial	179
#define lessEqualSpecial	180
#define greaterEqualSpecial	181
#define equalSpecial		182
#define notEqualSpecial		183
#define timesSpecial		184
#define divideSpecial		185
#define remainderSpecial	186
#define bitShiftColonSpecial	188
#define integerDivideSpecial	189
#define bitAndColonSpecial	190
#define bitOrColonSpecial	191
#define atColonSpecial		192
#define atColonPutColonSpecial	193
#define sizeSpecial		194
#define nextSpecial		195
#define nextPutColonSpecial	196
#define atEndSpecial		197
#define sameObjectSpecial	198
#define classSpecial		199
#define blockCopyColonSpecial	200
#define valueSpecial		201
#define valueColonSpecial	202
#define doColonSpecial		203
#define newSpecial		204
#define newColonSpecial		205
#define isNilSpecial		206
#define notNilSpecial		207
#define sendSelectorNoArg	208
#define sendSelector1Arg	224
#define sendSelector2Arg	240

#define receiverIndex		0
#define trueIndex		1
#define falseIndex		2
#define nilIndex		3
#define litMinusOneIndex	4
#define litZeroIndex		5
#define litOneIndex		6
#define litTwoIndex		7

#define receiverLocation	(0 << 6)
#define temporaryLocation	(1 << 6)
#define litConstLocation	(2 << 6)
#define litVarLocation		(3 << 6)

#define locationMask		(3 << 6)

#define pushLiteral		(0 << 6)
#define pushVariable		(1 << 6)
#define popStoreVariable	(2 << 6)
#define storeVariable		(3 << 6)

struct ByteCodeArray {
  Byte		*base;		/* base of the byte code array */
  Byte		*ptr;		/* current byte+1 of byte code array */
  int		maxLen;		/* max allocated len of byte code array
				   can be changed as byte code array grows,
				   and is used to tell when to grow the byte
				   code array. */
};

typedef struct ByteCodeArray	*ByteCodes;

extern ByteCodes	getByteCodes(), saveByteCodeArray(),
			optimizeByteCodes(), extractByteCodes();

extern void		truncateByteCodes(),
			compileByte(), compileAndFreeByteCodes(),
  			restoreByteCodeArray(), freeByteCodes(),
  			copyByteCodes(), printByteCodeName(), 
			printByteCodes(); 

extern	int		currentByteCodeLength(), isSimpleReturn(), 
			byteCodeLength(), byteCodeSize();

extern	int		byteCodeSizeTable[];

#define byteCodeSize(b)		byteCodeSizeTable[b]


/*
 Interpretation of the virtual machine byte codes

0-15 push receiver variable 	0000iiii
16-31 push temporary location	0001iiii
32-63 push literal constant	001iiiii
64-95 push literal variable	010iiiii
96-103 pop & store rec var	01100iii
104-111 pop & store temp loc	01101iii
112-119 push indexed		01110iii receiver true false nil -1 0 1 2
120-123 return indexed		011110ii receiver true false nil
124-125 return st top from	0111110i message, block
126	literal > 63 operation  01111110 yyxxxxxx xxxxxxxx: xx...xx index yy op
						(push const, push var,
						store var, pop/store var)
127     breakpoint patch        01111111 send #breakpoint:return:, reexecute
128	push indir		10000000 jjkkkkkk (receiver var, temp loc,
						   lit const, lit var)
						   [jj] #kkkkkk
129	store indir		10000001 jjkkkkkk (rv, tl, invalid, lv)
130	pop & store indir	10000010 jjkkkkkk (like store indir)
131	send lit selector	10000011 jjjkkkkk sel #kkkkk with jjj args
132	send lit selector	10000100 jjjjjjjj kkkkkkkk (as 131)
133	send lit sel to super	10000101 jjjkkkkk as 131
134	send lit to super	10000110 jjjjjjjj kkkkkkkk like 132
135	pop stack top		10000111
136	duplicate stack top	10001000
137	push active context	10001001
138	outer var operation	10001010 yyindex- scopes-- yy op
					(invalid, push, store, pop/store)
139	nop-needed by optimizer	10001011
140	stack top = self	10001100
141	stack top = one		10001101
142	indir replace stack top	10001100 jjkkkkkk (rv, tl, invalid, lv)
143	unused
144-151	jmp iii+1		10010iii
152-159	pop & jmp false iii+1	10011iii
160-167	jmp (iii-4)*256+jjjjjjjj10100iii jjjjjjjj
168-171 pop & jmp on true	101010ii jjjjjjjj ii*256+jjjjjjjj
172-175 pop & jmp on false	101011ii jjjjjjjj like 168
176-191 send arith message	1011iiii
192-207	send special message	1100iiii
208-223 send lit sel #iiii	1101iiii with no arguments
224-239 send lit sel #iiii	1110iiii with 1 argument
240-255 send lit sel #iiii	1111iiii with 2 arguments
*/

#endif /* __GSTBYTE__ */
