/***********************************************************************
 *
 * Byte code array utility routines.
 *
 * $Revision: 1.6.2$
 * $Date: 1999/08/31 11:23:18$
 * $Author: pb$
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
 * GNU Smalltalk; see the file COPYING.	 If not, write to the Free Software
 * Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 ***********************************************************************/


#include "gst.h"
#include "alloc.h"
#include "byte.h"
#include "dict.h"
#if STDC_HEADERS
#include <string.h> /* for memcpy */
#include <stdlib.h>
#endif /* STDC_HEADERS */
#include <stdio.h>

/* Define this to disable the peephole bytecode optimizer.  It works
 * well and increases a bit performance, so there's no reason to do that
 * unless you're debugging the compiler. */
#undef NO_OPTIMIZE


#define BYTE_CODE_CHUNK_SIZE  64

int byteCodeSizeTable[256] = {
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 0 */
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 16 */
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 32 */
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 48 */
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 64 */
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 80 */
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 96 */
1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,1, /* 112 */
2,2,2,2,3,2,3,1,1,1,3,1,1,1,2,1, /* 128 */
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 144 */
2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, /* 160 */
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 176 */
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 192 */
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 208 */
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 224 */
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}; /* 240 */

int isPush[256] = {
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 0 */
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 16 */
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 32 */
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 48 */
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 64 */
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 80 */
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* 96 */
1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0, /* 112 */
1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0, /* 128 */
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* 144 */
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* 160 */
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* 176 */
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* 192 */
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* 208 */
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* 224 */
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}; /* 240 */

ByteCodes  currentByteCodes = nil;

static ByteCodes allocByteCodes();
static mst_Boolean optimizeBasicBlock();
static void  compileByteCodes(), reallocByteCodes();


/* These are used to decode special send byte codes. */
static char  *mathMessageName[] = {
  "+",
  "-",
  "<",
  ">",
  "<=",
  ">=",
  "=",
  "~=",
  "*",
  "/",
  "\\",
  "@",
  "bitShift:",
  "//",
  "bitAnd:",
  "bitOr:"
};

/* These are the selectors for special case send bytecodes that the compiler
 * generates.  These are used only to print out the generated byte codes. */
static char  *specialMessageName[] = {
  "at:",
  "at:put:",
  "size",
  "next",
  "nextPut:",
  "atEnd",
  "==",
  "class",
  "blockCopy:temporaries:",
  "value",
  "value:",
  "do:",
  "new",
  "new:",
  "isNil",
  "notNil"
};

/* This structure and the following one are used when optimizing the
 * bytecodes.
 *
 * This one, in particular, defines where basic blocks start in the non-
 * optimized bytecodes.	 byte is nothing more than an offset in those
 * bytecodes; id is used to pair jump bytecodes with their destinations:
 * in the initial scan, when we encounter a jump bytecode we fill two
 * BlockBoundaries -- one has positive id and represents the destination
 * of the jump, one has negative id (but with the same absolute value)
 * and represents the jump bytecode itself.
 */
typedef struct BlockBoundaryStruct {
  short byte;
  short id;
} BlockBoundary;

/* This structure defines how to fix the jumps after the optimized basic
 * blocks are put together.  Everything is done after the peephole pass
 * because this allows us to handle forward jumps and backward jumps in
 * the same way.
 * When single blocks are optimized, the sorted BlockBoundaries are
 * examined one at a time.  As we process blocks, we fill an array of
 * Jump structures with optimized bytecode offsets.  In the end, we'll
 * have paired BlockBoundaries whose id's absolute value is the same
 * (with different signs, of course).
 */
typedef struct JumpStruct {
  int	from;  /* where the jump bytecode lies */
  int	dest;  /* where the jump bytecode leaps */
} Jump;





ByteCodes
extractByteCodes (byteArrayOOP)
     OOP byteArrayOOP;
{
  ByteCodes result;
  int  len;
  ByteArray byteArray;

  byteArray = (ByteArray)oopToObj(byteArrayOOP);
  len = oopNumFields(byteArrayOOP);
  result = (ByteCodes) xmalloc(len + sizeof(struct ByteCodeArray));

  result->base = (Byte *)(result + 1);
  result->ptr = result->base + len;
  result->maxLen = len;
  memcpy (result->base, byteArray->bytes, len);
  return (result);
}

void
compileByte (byte)
     Byte byte;
{
  if (currentByteCodes == nil) {
    currentByteCodes = allocByteCodes(BYTE_CODE_CHUNK_SIZE);
  } else {
    if ((currentByteCodes->ptr - currentByteCodes->base)
	>= currentByteCodes->maxLen) {
      reallocByteCodes(currentByteCodes, BYTE_CODE_CHUNK_SIZE);
    }
  }

  *currentByteCodes->ptr++ = byte;
}

void
freeByteCodes (byteCodes)
     ByteCodes byteCodes;
{
  if (byteCodes != nil) {
    xfree(byteCodes->base);
    xfree(byteCodes);
  }
}

void
compileAndFreeByteCodes (byteCodes)
     ByteCodes byteCodes;
{
  compileByteCodes(byteCodes->base, byteCodes->ptr);
  freeByteCodes(byteCodes);
}

/*
 * ByteCodes getByteCodes()
 *
 * Description
 *
 * Called when byte code compilation is complete, this routine returns the
 * set of byte codes that were compiled.  Since compilation is complete,
 * this routine also resets the internal state of the byte code compiler
 * in preparation for next time.
 *
 * Outputs
 *
 * A pointer to a byte code array that contains the bytes for the current
 * compilation.
 */
ByteCodes
getByteCodes ()
{
  ByteCodes curByteCodes;

  curByteCodes = currentByteCodes;
  currentByteCodes = nil;

  return (curByteCodes);
}


/*
 * ByteCodes saveByteCodeArray()
 *
 * Description
 *
 * Called to save the set of byte codes currently being compiled and
 * prepare for a new compilation of byte codes.	 The current set of byte
 * codes being compiled is returned for the caller to keep and to later
 * use in a restoreByteCodeArray call.
 *
 * Outputs
 *
 * A byte code array pointer to the current byte codes.
 */
ByteCodes
saveByteCodeArray ()
{
  ByteCodes curByteCodes;

  curByteCodes = currentByteCodes;
  currentByteCodes = nil;

  return (curByteCodes);
}


/*
 * void restoreByteCodeArray(byteCodes)
 *
 * Description
 *
 * Restores the internal state of the byte code compiler so that it can
 * continue compiling byte codes into the byte code array "byteCodes",
 * which should have been returned at some previous point from
 * saveByteCodeArray().
 *
 * Inputs
 *
 * byteCodes:
 *  The byte code array pointer to be restored.	 Should have come
 *  from a saveByteCodeArray call.
 *
 */
void
restoreByteCodeArray (byteCodes)
     ByteCodes byteCodes;
{
  currentByteCodes = byteCodes;
}

int
byteCodeLength(byteCodes)
ByteCodes byteCodes;
{
  if (byteCodes == nil) {
    return (0);
  }
  return (byteCodes->ptr - byteCodes->base);
}


/*
 * int currentByteCodeLength()
 *
 * Description
 *
 * Return the current number of byte codes that have been compiled.
 *
 * Outputs
 *
 * Number of byte codes present in the current byte code array right now.
 */
int
currentByteCodeLength ()
{
  if (currentByteCodes == nil) {
    return (0);
  }

  return (currentByteCodes->ptr - currentByteCodes->base);
}

void
copyByteCodes (dest, byteCodes)
     Byte *dest;
     ByteCodes byteCodes;
{
  memcpy(dest, byteCodes->base, byteCodeLength(byteCodes));
}

void
truncateByteCodes (here, byteCodes)
     Byte *here;
     ByteCodes byteCodes;
{
  byteCodes->ptr = here;
}



void
printByteCodes (byteCodes, literalVec)
     ByteCodes byteCodes;
     OOP literalVec[];
{
  Byte	*b;
  int  ip;

  if (byteCodes == nil) {
    return;
  }

  for (b = byteCodes->base; b < byteCodes->ptr; b+= byteCodeSize(*b)) {
    ip = b - byteCodes->base;
    printf("%5d:\t", ip);
    printByteCodeName(b, ip, literalVec);
    printf("\n");
  }
  printf("\n");
}

void
printIndexed (byte, literalVec, constOk)
     Byte byte;
     OOP literalVec[];
     mst_Boolean constOk;
{
  switch (byte >> 6) {
  case 0:
    printf("Instance Variable[%d]", byte & 63);
    break;
  case 1:
    printf("Temporary[%d]", byte & 63);
    break;
  case 2:
    printf("Constant[%d]", byte & 63);
    if (!constOk) {
      printf(" -- INVALID!");
    }
    break;
  case 3:
    printf("Global Variable[%d] = ", byte & 63);
    printAssociationKey(literalVec[byte & 63]);
    break;
  }
}

void
printByteCodeName (bp, ip, literalVec)
     Byte bp[];
     int ip;
     OOP literalVec[];
{
  switch (bp[0]) {
  case	0: case	 1: case  2: case  3:
  case	4: case	 5: case  6: case  7:
  case	8: case	 9: case 10: case 11:
  case 12: case 13: case 14: case 15:
    printf("push Instance Variable[%d]", bp[0] & 15);
    break;

  case 16: case 17: case 18: case 19:
  case 20: case 21: case 22: case 23:
  case 24: case 25: case 26: case 27:
  case 28: case 29: case 30: case 31:
    printf("push Temporary[%d]", bp[0] & 15);
    break;

  case 32: case 33: case 34: case 35:
  case 36: case 37: case 38: case 39:
  case 40: case 41: case 42: case 43:
  case 44: case 45: case 46: case 47:
  case 48: case 49: case 50: case 51:
  case 52: case 53: case 54: case 55:
  case 56: case 57: case 58: case 59:
  case 60: case 61: case 62: case 63:
    printf("push Literal[%d]", bp[0] & 31);
    break;

  case 64: case 65: case 66: case 67:
  case 68: case 69: case 70: case 71:
  case 72: case 73: case 74: case 75:
  case 76: case 77: case 78: case 79:
  case 80: case 81: case 82: case 83:
  case 84: case 85: case 86: case 87:
  case 88: case 89: case 90: case 91:
  case 92: case 93: case 94: case 95:
    printf("push Global Variable[%d] = ", bp[0] & 31);
    printAssociationKey(literalVec[bp[0] & 31]);
    break;

  case	96: case  97: case  98: case  99:
  case 100: case 101: case 102: case 103:
    printf("pop and store Instance Variable[%d]", bp[0] & 7);
    break;

  case 104: case 105: case 106: case 107:
  case 108: case 109: case 110: case 111:
    printf("pop and store Temporary[%d]", bp[0] & 7);
    break;

  case 112:
    printf("push self");
    break;

  case 113:
    printf("push true");
    break;

  case 114:
    printf("push false");
    break;

  case 115:
    printf("push nil");
    break;

  case 116:
    printf("push -1");
    break;

  case 117:
    printf("push 0");
    break;

  case 118:
    printf("push 1");
    break;

  case 119:
    printf("push 2");
    break;

  case 120:
    printf("return self");
    break;

  case 121:
    printf("return true");
    break;

  case 122:
    printf("return false");
    break;

  case 123:
    printf("return nil");
    break;

  case 124:
    printf("return explicitly from method");
    break;

  case 125:
    printf("return stack top");
    break;

  case 126:
    switch (bp[1] >> 6) {
    case 0:
      printf("push Literal");
      break;
    case 1:
      printf("push Global Variable");
      break;
    case 2:
      printf("pop and store Global Variable");
      break;
    case 3:
      printf("store Global Variable");
      break;
    }
    printf("[%d]", (bp[1] & 63) * 256 + bp[2]);
    if (bp[1] >> 6) {
      printf(" = ");
      printAssociationKey(literalVec[(bp[1] & 63) * 256 + bp[2]]);
    }
    break;

  case 127:
    printf("breakpoint bytecode");
    break;

  case 128:
    printf("push ");
    printIndexed(bp[1], literalVec, true);
    break;

  case 129:
    printf("store ");
    printIndexed(bp[1], literalVec, false);
    break;

  case 130:
    printf("pop and store ");
    printIndexed(bp[1], literalVec, false);
    break;

  case 131:
    printf("send selector %d, %d args = ", bp[1] & 31, bp[1] >> 5);
    printSymbol(literalVec[bp[1] & 31]);
    break;

  case 132:
    printf("send selector %d, %d args = ", bp[2], bp[1]);
    printSymbol(literalVec[bp[2]]);
    break;

  case 133:
    printf("send to super selector %d, %d args = ", bp[1] & 31, bp[1] >> 5);
    printSymbol(literalVec[bp[1] & 31]);
    break;

  case 134:
    printf("send to super selector %d, %d args = ", bp[2], bp[1]);
    printSymbol(literalVec[bp[2]]);
    break;

  case 135:
    printf("pop stack top");
    break;

  case 136:
    printf("duplicate stack top");
    break;


  case 137:
    printf("push current context");
    break;

  case 138:
    switch (bp[1] >> 6) {
    case 0:
      printf("(invalid)");
      break;
    case 1:
      printf("push");
      break;
    case 2:
      printf("pop and store");
      break;
    case 3:
      printf("store");
      break;
    }
    printf(" outer var scopes = %d varIndex = %d", bp[2], bp[1] & 63);
    break;

  case 139:
    printf("no operation");
    break;

  case 140:
    printf("set stack top to self");
    break;
  
  case 141:
    printf("set stack top to 1");
    break;
  
  case 142:
    printf("set stack top to ");
    printIndexed(bp[1], literalVec, true);
    break;

  case 143:
    printf("Invalid bytecode %d!!!", bp[0]);
    break;

  case 144: case 145: case 146: case 147:
  case 148: case 149: case 150: case 151:
    printf("jump to %d", (bp[0] & 7) + ip + 1 + 1);
    break;

  case 152: case 153: case 154: case 155:
  case 156: case 157: case 158: case 159:
    printf("jump to %d if false", (bp[0] & 7) + ip + 1 + 1);
    break;

  case 160: case 161: case 162: case 163:
  case 164: case 165: case 166: case 167:
    printf("jump to %d", ((bp[0]&7)-4) * 256 + bp[1] + ip + 2);
    break;

  case 168: case 169: case 170: case 171:
    printf("pop and jump to %d if true", (bp[0]&3) * 256 + bp[1] + ip + 2);
    break;

  case 172: case 173: case 174: case 175:
    printf("pop and jump to %d if false", (bp[0]&3) * 256 + bp[1] + ip + 2);
    break;

  case 176: case 177: case 178: case 179:
  case 180: case 181: case 182: case 183:
  case 184: case 185: case 186: case 187:
  case 188: case 189: case 190: case 191:
    printf("send arithmetic message \"%s\"", mathMessageName[bp[0] & 15]);
    break;

  case 192: case 193: case 194: case 195:
  case 196: case 197: case 198: case 199:
  case 200: case 201: case 202: case 203:
  case 204: case 205: case 206: case 207:
    printf("send special message \"%s\"", specialMessageName[bp[0] & 15]);
    break;

  case 208: case 209: case 210: case 211:
  case 212: case 213: case 214: case 215:
  case 216: case 217: case 218: case 219:
  case 220: case 221: case 222: case 223:
    printf("send selector %d, 0 args = ", bp[0] & 15);
    printSymbol(literalVec[bp[0] & 15]);
    break;

  case 224: case 225: case 226: case 227:
  case 228: case 229: case 230: case 231:
  case 232: case 233: case 234: case 235:
  case 236: case 237: case 238: case 239:
    printf("send selector %d, 1 arg = ", bp[0] & 15);
    printSymbol(literalVec[bp[0] & 15]);
    break;

  case 240: case 241: case 242: case 243:
  case 244: case 245: case 246: case 247:
  case 248: case 249: case 250: case 251:
  case 252: case 253: case 254: case 255:
    printf("send selector %d, 2 args = ", bp[0] & 15);
    printSymbol(literalVec[bp[0] & 15]);
    break;
  }
}



int
isSimpleReturn (byteCodes)
     ByteCodes byteCodes;
{
  Byte	*bytes;
  long	byteCodeLen;

  if (byteCodes == nil) {
    return (0);
  }

  byteCodeLen = byteCodeLength(byteCodes);
  bytes = byteCodes->base;

  /* check for ^self */
  if (byteCodeLen == 1 && bytes[0] == (returnIndexed | receiverIndex)) {
    return (1);
  }

  /* check for ^instanceVariable */
  if (byteCodeLen == 2) {
    if ((bytes[0] & ~15) == pushReceiverVariable
	&& bytes[1] == returnMethodStackTop) {
      return (((bytes[0] & 0x0F) << 8) | 2);
    }
  } else if (byteCodeLen == 3) {
    if (bytes[0] == pushIndexed
	&& (bytes[1] & locationMask) == receiverLocation
	&& bytes[2] == returnMethodStackTop) {
      return (((bytes[1] & ~locationMask) << 8) | 2);
    }
  }

  /* check for ^firstLiteral */
  if (byteCodeLen == 1) {
    if (bytes[0] == (returnIndexed | trueIndex)) {
      addForcedObject(trueOOP);
      return (3);
    } else if (bytes[0] == (returnIndexed | falseIndex)) {
      addForcedObject(falseOOP);
      return (3);
    } else if (bytes[0] == (returnIndexed | nilIndex)) {
      addForcedObject(nilOOP);
      return (3);
    } else {
      return (0);
    }
  }

  if (byteCodeLen == 2) {
    if (bytes[1] != returnMethodStackTop) {
      return (0);
    }

    if (bytes[0] == pushLitConstant) {
      return (3);
    } else if (bytes[0] == (pushSpecial | litMinusOneIndex)) {
      addForcedObject(fromInt(-1));
      return (3);
    } else if (bytes[0] == (pushSpecial | litZeroIndex)) {
      addForcedObject(fromInt(0));
      return (3);
    } else if (bytes[0] == (pushSpecial | litOneIndex)) {
      addForcedObject(fromInt(1));
      return (3);
    } else if (bytes[0] == (pushSpecial | litTwoIndex)) {
      addForcedObject(fromInt(2));
      return (3);
    }
  }

  return (0);
}



/*
 * static ByteCodes optimizeByteCodes(byteCodes)
 *
 * Description
 *
 * Divide the byte codes of a method in basic blocks, optimize each one,
 * reunite the optimized blocks and return a new vector of byte codes that
 * represents the optimized byte code stream.  Actual optimization of the
 * basic blocks is optimizeBasicBlock's task.
 *
 * Inputs
 *
 * byteCodes:
 *  A vector of byte codes to be optimized.
 *
 * Outputs
 *
 * *ANOTHER* vector of byte codes (even though it currently contains the
 * same bytecodes that were passed in).
 */

static int compare_blocks();

int
compare_blocks (a, b)
     register BlockBoundary *a, *b;
{
  return (a->byte - b->byte);
}

ByteCodes
optimizeByteCodes (byteCodes)
     ByteCodes byteCodes;
{
  static int  jumpOffsets[] =  {
    -1022, -766, -510, -254, 2, 258, 514, 770,
	2,  258,  514,	770, 2, 258, 514, 770
  };
  register BlockBoundary *blocks, *current;
  register Jump	  *jumps;
  register Byte	  *bp;
  Byte	  *end, *first;
  int	 num;

#ifdef NO_OPTIMIZE
  return (byteCodes);
#endif

  bp = byteCodes->base;
  end = byteCodes->ptr;
  blocks = alloca(sizeof(BlockBoundary) * (end - bp + 1));
  memset(blocks, 0, sizeof(BlockBoundary) * (end - bp + 1));

  /* 1) Split into basic blocks.  This part cheats so that the final fixup
   * also performs jump optimization. */
  for(current = blocks, num = 0; bp != end; bp += byteCodeSize(*bp)) {
    Byte	*dest = bp;
    mst_Boolean	 canOptimizeJump;
    do {
      canOptimizeJump = false;
      switch (*dest) {
	/* short jump */
	case 144:
	  if (dest[2] == popStackTop) {
	    /* The bytecodes can only be those produced by #ifTrue:/#ifFalse:
	     *	    0: jump to 2
	     *	    1: push nil
	     *	    2: pop stack top
	     * This could not be optimized to a single pop, cause bytecodes 1
	     * and 2 lie in different basic blocks! So we rewrite it to a
	     * functionally equivalent but optimizable bytecode sequence. */
	    *dest = popStackTop;
	    break;
	  }
	  /* Fall through */

	case 145: case 146: case 147:
	case 148: case 149: case 150: case 151:
	  /* If bp == dest, we could end up writing a 2-byte jump bytecode
	   * where space was only reserved for a 1-byte jump bytecode!
	   * But if we jump to a return, we can safely optimize -- returns
	   * are always one byte */
	  canOptimizeJump = (bp != dest);
	  dest += *dest;
	  dest -= 142;
	  canOptimizeJump |= (*dest >= 120 || *dest <= 125);
	  break;

	/* pop and short jump if false */
	case 152: case 153: case 154: case 155:
	case 156: case 157: case 158: case 159:
	  /* UNCONDITIONAL jumps to CONDITIONAL jumps must not be touched! */
	  if (bp == dest) {
	    dest += *dest;
	    dest -= 150;
	  }
	  break;

	/* long jump, pop and long jump if true, pop and long jump if false */
	case 160: case 161: case 162: case 163:
	case 164: case 165: case 166: case 167:
	  /* 2-byte unconditional jump, we can indeed optimize it */
	  canOptimizeJump = true;
	  dest += ((signed int)dest[1]) + jumpOffsets[*dest & 15];
	  break;

	case 168: case 169: case 170: case 171:
	case 172: case 173: case 174: case 175:
	  /* UNCONDITIONAL jumps to CONDITIONAL jumps must not be touched! */
	  if (bp == dest) {
	    dest += ((signed int)dest[1]) + jumpOffsets[*dest & 15];
	  }
	  break;

	case 120: case 121: case 122: case 123:
	case 124: case 125:
	  /* Return bytecode - patch the original jump to return directly */
	  if (bp[-1] != blockCopyColonSpecial) {
	    if (*bp >= jumpLong) {
	      bp[1] = *dest;		/* fill both 2 bytes */
	    }
	    *bp = *dest;
	    /* This in fact eliminated the jump, don't split in basic blocks */
	    dest = bp;
	  }
	  break;
      }
    } while (canOptimizeJump);
    if (bp != dest) {
      current->byte = dest - byteCodes->base;
      current->id   = ++num;
      current++;
      current->byte = bp - byteCodes->base;
      current->id   = -num;
      current++;
    }
  }

  /* 2) Get the "real" block boundaries by sorting them according to where they
   * happen in the original bytecode.  Note that a simple bucket sort is not
   * enough because multiple jumps could end on the same bytecode, and the
   * same bytecode could be both the start and the destination of a jump! */
  qsort(blocks, current - blocks, sizeof(BlockBoundary), compare_blocks);

  /* 3) Optimize the single basic blocks, and reorganize into `jumps' the
   * data that was put in blocks */
  jumps = alloca(sizeof(Jump) * num);

  for (bp = byteCodes->base; blocks != current; blocks++) {
    first = bp;
    bp = byteCodes->base + blocks->byte;
    optimizeBasicBlock(first, bp);
    if (blocks->id > 0) {
      jumps[ blocks->id - 1].dest = byteCodeLength(currentByteCodes);
    } else {
      jumps[-blocks->id - 1].from = byteCodeLength(currentByteCodes);
    }
  }
  optimizeBasicBlock(bp, end);

  /* 4) Fix the jumps so that they correctly point to the start of the same
   * basic block */
  for(; num--; jumps++) {
    short offset;

    bp = currentByteCodes->base + jumps->from;
    offset = jumps->dest - jumps->from - 2;
    if (offset == -1) {			/* jump to following bytecode do */
      if (*bp > 159) {			/* not exist - use other bytecodes */
	bp[1] = nopBytecode;		/* 2 byte jumps = nop+nop or pop+nop */
      }
      if (*bp & 8) {
	*bp = popStackTop;		/* pop stack top for conditional */
      } else {				/* jumps */
	*bp = nopBytecode;		/* nop for unconditional jumps */
      }
      continue;
    }
    switch (*bp) {
      /* short jump */
      case 144: case 145: case 146: case 147:
      case 148: case 149: case 150: case 151: *bp = 144 + offset; continue;

      /* pop and short jump if false */
      case 152: case 153: case 154: case 155:
      case 156: case 157: case 158: case 159: *bp = 152 + offset; continue;

      /* long jump, pop and long jump if true, pop and long jump if false */
      case 160: case 161: case 162: case 163:
      case 164: case 165: case 166: case 167: *bp = 164;   break;
      case 168: case 169: case 170: case 171: *bp = 168;   break;
      case 172: case 173: case 174: case 175: *bp = 172;   break;
    }
    *bp++  += offset >> 8;
    *bp	   =  offset & 255;
  }

  freeByteCodes(byteCodes);
  return (getByteCodes());
}

/***********************************************************************
 *
 * Internal routines.
 *
 ***********************************************************************/

/*
 * static ByteCodes optimizeBasicBlock(from, to)
 *
 * Description
 *
 * Scan the bytecodes between the given addresses, and compile an
 * optimized version by calling compileByte().
 *
 * Inputs
 *
 * from:
 *  Pointer to the first bytecode to be optimized.
 * to:
 *  Pointer past the last bytecode to be optimized.
 *
 * Outputs
 *
 * Return whether at least a bytecode was generated.
 */

#define NEXT(size) {	\
  n = size;		\
  opt += size;		\
  continue;		\
}

#define REPLACE(size) {	\
  n = size;		\
  continue;		\
}

#define COPY {			\
  n = byteCodeSize(byte);	\
  opt++;			\
  if(n != 1) *opt++ = *bp++;	\
  if(n == 3) *opt++ = *bp++;	\
  continue;			\
}

#define BEGIN	if(0) {
#define BYTECODE(n) } else if(byte == (n)) {
#define RANGE(a, b) } else if(byte >= (a) && byte <= (b)) {
#define ANY(a, b) } else if(byte == (a) || byte == (b)) {
#define CONDITION(c) } else if(c) {
#define NO_MATCH } else {
#define END  }

mst_Boolean
optimizeBasicBlock (from, to)
     Byte *from;
     Byte *to;
{
  register Byte byte, *bp, *opt;
  int  n;

  bp = opt = from;
  n = 0;
  while (bp != to) {
    byte = *opt = *bp++;
    BEGIN
      RANGE(120, 125)
	/* this performs unreachable code optimization! */
	opt++;
	break;

      BYTECODE(nopBytecode)
	REPLACE(n);			/* used when compiling yourself */

      BYTECODE(notEqualSpecial)
	if (!n) NEXT(1);
	if (opt[-1] == (pushSpecial | nilIndex)) {  /* x ~= nil */
	  opt[-1] = notNilSpecial;
	  REPLACE(1);
	}
	NEXT(1);

      ANY(sameObjectSpecial, equalSpecial)
	if (!n) NEXT(1);
	if (opt[-1] == (pushSpecial | nilIndex)) {  /* x = nil, x == nil */
	  opt[-1] = isNilSpecial;
	  REPLACE(1);
	}
	NEXT(1);

      BYTECODE(popStackTop)
 	if (n) {
	  byte = opt[-n];
	  BEGIN
	    CONDITION(isPush[byte])   /* push/pop */
	      opt -= n;
	      NEXT(0);

	    BYTECODE(storeIndexed)   /* store/pop */
	      byte = *--opt;	 /* get data byte */
	      opt--;	  /* move to opcode */
	      if (byte < 8) {
		*opt = popReceiverVariable | byte;
		NEXT(1);
	      } else if (byte >= 64 && byte < 72) {
		*opt = popTemporaryVariable | (byte & 63);
		NEXT(1);
	      } else {
		*opt = popStoreIndexed;
		NEXT(2);
	      }

	    ANY(bigLiteralsBytecode, outerTempBytecode)
	      byte = opt[-2];	  /* get second byte */
	      if (byte < popStoreVariable) {
		opt -= n;    /* push/pop */
		NEXT(0);
	      } else if (byte >= storeVariable) {
		opt[-2] ^= (popStoreVariable ^ storeVariable);
		REPLACE(3);
	      }
	  END
	}

	if (bp != to && (*bp & ~3) == returnIndexed) {
	  *opt++ = *bp++;   /* pop/return */
	  break;    /* kill unreachable code */
	}
	NEXT(1);

      CONDITION(isPush[byte])	 /* push/push -> dup */
	if (!n) COPY;
	if(opt[-n] == *opt) {
	  if (n == 1) {
	    *opt = dupStackTop;
	    NEXT(1);
	  } else if (opt[-1] == *bp) {
	    *opt = dupStackTop;
	    bp++;
	    NEXT(1);
	  }
	}

	BEGIN			/* pop-store/push -> store */
	  RANGE(pushReceiverVariable, pushReceiverVariable | 7)
	    if (opt[-n] == (popReceiverVariable | (byte & 15))) {
	      opt[-1] = storeIndexed;
	      *opt++ = receiverLocation | (byte & 15);
	      REPLACE(2);
	    }
	    if(opt[-n] == popStackTop) {	/* pop/push -> replace */
	      opt--;
	      *opt++ = replaceIndexed;
	      *opt++ = receiverLocation | (byte & 15);
	      REPLACE(2);
	    }

	  RANGE(pushReceiverVariable | 8, pushReceiverVariable | 15)
	    if (opt[-n] == popStoreIndexed
		&& opt[-1] == (receiverLocation | (byte & 15))) {
	      opt[-2] = storeIndexed;
	      REPLACE(2);
	    }
	    if(opt[-n] == popStackTop) {
	      opt--;
	      *opt++ = replaceIndexed;
	      *opt++ = receiverLocation | (byte & 15);
	      REPLACE(2);
	    }

	  RANGE(pushTemporaryVariable, pushTemporaryVariable | 7)
	    if (opt[-n] == (popTemporaryVariable | (byte & 15))) {
	      opt[-1] = storeIndexed;
	      *opt++ = temporaryLocation | (byte & 15);
	      REPLACE(2);
	    }
	    if(opt[-n] == popStackTop) {
	      opt--;
	      *opt++ = replaceIndexed;
	      *opt++ = temporaryLocation | (byte & 15);
	      REPLACE(2);
	    }

	  RANGE(pushTemporaryVariable | 8, pushTemporaryVariable | 15)
	    if (opt[-n] == popStoreIndexed
		&& opt[-1] == (temporaryLocation | (byte & 15))) {
	      opt[-2] = storeIndexed;
	      REPLACE(2);
	    }
	    if(opt[-n] == popStackTop) {
	      opt--;
	      *opt++ = replaceIndexed;
	      *opt++ = temporaryLocation | (byte & 15);
	      REPLACE(2);
	    }

	  RANGE(pushLitVariable, pushLitVariable | 31)
	    if (opt[-n] == popStoreIndexed
		&& opt[-1] == (litVarLocation | (byte & 31))) {
	      opt[-2] = storeIndexed;
	      REPLACE(2);
	    }
	    if(opt[-n] == popStackTop) {
	      opt--;
	      *opt++ = replaceIndexed;
	      *opt++ = litVarLocation | (byte & 31);
	      REPLACE(2);
	    }

	  RANGE(pushLitConstant, pushLitConstant | 31)
	    if(opt[-n] == popStackTop) {
	      opt--;
	      *opt++ = replaceIndexed;
	      *opt++ = litConstLocation | (byte & 31);
	      REPLACE(2);
	    }

	  BYTECODE(pushSpecial | receiverIndex)
	    if (opt[-n] == popStackTop) {
	      opt[-1] = replaceSelf;
	      REPLACE(1);
	    }

	  BYTECODE(pushSpecial | litOneIndex)
	    if (opt[-n] == popStackTop) {
	      opt[-1] = replaceOne;
	      REPLACE(1);
	    }

	  BYTECODE(pushIndexed)
	    byte = *bp++;
	    if (opt[-n] == popStoreIndexed) {
	      opt[-2] = storeIndexed;
	      if (opt[-1] == byte) {
	        REPLACE(2);
	      } else {
	        *opt = replaceIndexed;
	      }
	    } else if (opt[-n] == popStackTop) {
	      *--opt = replaceIndexed;
	    }
	    opt[1] = byte;
	    NEXT(2);
	END;

	NEXT(1);	/* no match */

      ANY(bigLiteralsBytecode, outerTempBytecode)
	if (!n || opt[-n] != byte) COPY;

	byte = opt[-2];    /* get second byte */
	if (byte < popStoreVariable) {
	  if (byte == *bp && opt[-1] == bp[1]) {	/* push/push -> dup */
	    *opt = dupStackTop;
	    bp += 2;
	    NEXT(1);
	  }
	} else if (byte < storeVariable) {	/* pop-store/push -> store */
	  if ((byte & 63) == (*bp & 63) && opt[-1] == bp[1]) {
	    opt[-2] ^= (popStoreVariable ^ storeVariable);
	    bp += 2;
	    REPLACE(3);
	  }
        }

	opt++;
	*opt++ = *bp++;
	*opt++ = *bp++;
	REPLACE(3);

      NO_MATCH
        COPY;
    END;
  }
  compileByteCodes(from, opt);
  return (opt != from);
}



void compileByteCodes (from, to)
     register Byte *from, *to;
{
  if (currentByteCodes == nil) {
    currentByteCodes = allocByteCodes(BYTE_CODE_CHUNK_SIZE + (to - from));
  } else {
    register int free;
    free = currentByteCodes->maxLen -
      (currentByteCodes->ptr - currentByteCodes->base);

    if (free < (to - from)) {
      memcpy(currentByteCodes->ptr, from, free);
      currentByteCodes->ptr += free;
      from += free;
      reallocByteCodes(currentByteCodes, BYTE_CODE_CHUNK_SIZE + (to - from));
    }
  }

  memcpy(currentByteCodes->ptr, from, to - from);
  currentByteCodes->ptr += to - from;
}

ByteCodes
allocByteCodes (size)
     int size;
{
  ByteCodes newByteCodes;

  newByteCodes = (ByteCodes)xmalloc(sizeof(struct ByteCodeArray));
  newByteCodes->base = (Byte *)xmalloc(size);
  newByteCodes->ptr = newByteCodes->base;
  newByteCodes->maxLen = size;

  return (newByteCodes);
}

void
reallocByteCodes (byteCodes, delta)
     ByteCodes byteCodes;
     int   delta;
{
#ifndef OPTIMIZE
  if (byteCodes->maxLen != (byteCodes->ptr - byteCodes->base)) {
    errorf("reallocByteCodes called with maxLen != byteCode len");
  }
#endif

  byteCodes->base = (Byte *)xrealloc(byteCodes->base, byteCodes->maxLen + delta);
  byteCodes->ptr  = byteCodes->base + byteCodes->maxLen;
  byteCodes->maxLen += delta;
}

