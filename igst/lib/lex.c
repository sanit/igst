/***********************************************************************
 *
 *	Lexer Module.
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
#include "lex.h"
#include "lib.h"
#if defined(MSDOS) | defined (__OS2__)
#include "gst_tab.h"
#else
#include "gst.tab.h"
#endif
#include "str.h"
#include "dict.h"
#include "comp.h"
#include "tree.h"
#include "sysdep.h"
#include <stdio.h>
#include <math.h>
#ifdef HAVE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif /* HAVE_READLINE */

#if STDC_HEADERS
#  include <string.h>
#endif /* STDC_HEADERS */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_IO_H
#include <io.h>
#endif

/* Define this if you want the lexer to print all the tokens that it scans,
 * before passing them to the parser.
 */
#undef LEXDEBUG

/* Define this if you're debugging the lexer and you don't want the parser
 * to be ran -- only lexical scanning will take place.
 */
#undef NO_PARSE



#define WHITE_SPACE		1
#define DIGIT			2
#define ID_CHAR			4
#define SPECIAL_CHAR		8

#define EMACS_PROCESS_MARKER	'\001' /* ^A as marker -- random choice */

#if defined(_WIN32) || defined(__OS2__) || defined(MSDOS)
#define HAVE_DOS_STYLE_PATH_NAMES
#endif

typedef struct StringStreamStruct {
  char		*strBase;	/* base of asciz string */
  char		*str;		/* pointer into asciz string */
} StringStream;

struct StreamStruct {
  StreamType	type;
  int		pushedBackChars[2]; /* holds the 2 characters of buffering */
  int		pushedBackCount; /* number of chars pushed back */
  int		line;
  int		column;
  char		*fileName;
  int		fileOffset;
  OOP		fileNameOOP;	/* holds full path name for file as an oop */
  mst_Boolean	prompt;
  union {
    FILE	*u_st_file;
    StringStream u_st_str;
  } st;
  Stream	prevStream;
};

#define st_file		st.u_st_file
#define st_str		st.u_st_str

/* This is necessary so that the grammar knows when it must switch to
   compile mode */

mst_Boolean			compileCode = false;

Stream	inStream = nil;
char	*processingFile = nil;

mst_Boolean		reportErrors = true;
char		*firstErrorStr = nil;
char		*firstErrorFile = nil;
long		firstErrorLine;

/* Controls the use of the changes file, for recording source text.
   If nil, no recording */
char		*changeFileName = nil;

static FILE	*changeStr = nil;

static int			methodStartPos = 0;

static mst_Boolean		isDigit(), isBaseDigit(), parsePrimitive(),
				parseDigits(), parseFraction();
static int			invalid(), comment(), charLiteral(),
				parseBinOP(), stringLiteral(), parseNumber(),
				parseIdent(), myGetC(), parseColon(),
  				nextChar(), digitToInt();
static char			*scanStringoid();
static void 			unreadChar(), lineStamp(), /* myUngetC(), */
				myClose();
static Stream			pushNewStream();

#ifdef LEXDEBUG
static void			printToken()
#endif



typedef struct {
  int		(*lexFunc)();
  int		retToken;
  int		charClass;
} LexTabElt;

static LexTabElt charTab[128] = {
/*   0 */  { invalid,		0,		0 },
/*   1 */  { invalid,		0,		0 },
/*   2 */  { invalid,		0,		0 },
/*   3 */  { invalid,		0,		0 },
/*   4 */  { invalid,		0,		0 },
/*   5 */  { invalid,		0,		0 },
/*   6 */  { invalid,		0,		0 },
/*   7 */  { invalid,		0,		0 },
/*   8 */  { invalid,		0,		0 },
/*   9 */  { 0,			0,		WHITE_SPACE },
/*  10 */  { 0,			0,		WHITE_SPACE },
/*  11 */  { invalid,		0,		0 },
/*  12 */  { 0,			0,		WHITE_SPACE }, 
/*  13 */  { 0,			0,		WHITE_SPACE },
/*  14 */  { invalid,		0,		0 },	
/*  15 */  { invalid,		0,		0 },
/*  16 */  { invalid,		0,		0 },
/*  17 */  { invalid,		0,		0 },
/*  18 */  { invalid,		0,		0 },
/*  19 */  { invalid,		0,		0 },
/*  20 */  { invalid,		0,		0 },
/*  21 */  { invalid,		0,		0 },
/*  22 */  { invalid,		0,		0 },
/*  23 */  { invalid,		0,		0 },
/*  24 */  { invalid,		0,		0 },
/*  25 */  { invalid,		0,		0 },
/*  26 */  { invalid,		0,		0 },
/*  27 */  { invalid,		0,		0 },
/*  28 */  { invalid,		0,		0 },
/*  29 */  { invalid,		0,		0 },
/*  30 */  { invalid,		0,		0 },
/*  31 */  { invalid,		0,		0 },
/*     */  { 0,			0,		WHITE_SPACE },
/*   ! */  { 0,			BANG,		0 }, 
/*   " */  { comment,		0,		0 },
/*   # */  { 0,			SHARP,		0 },
/*   $ */  { charLiteral,	0,		0 },
/*   % */  { parseBinOP,	0,		SPECIAL_CHAR },
/*   & */  { parseBinOP,	0,		SPECIAL_CHAR },
/*   ' */  { stringLiteral,	0,		0 },
/*   ( */  { 0,			OPEN_PAREN,	0 },
/*   ) */  { 0,			CLOSE_PAREN,	0 },
/*   * */  { parseBinOP,	0,		SPECIAL_CHAR },
/*   + */  { parseBinOP,	0,		SPECIAL_CHAR },
/*   , */  { parseBinOP,	0,		SPECIAL_CHAR },
/*   - */  { parseBinOP,	0,		SPECIAL_CHAR },
/*   . */  { 0,			DOT,		0 },
/*   / */  { parseBinOP,	0,		SPECIAL_CHAR },
/*   0 */  { parseNumber,	0,		DIGIT | ID_CHAR },
/*   1 */  { parseNumber,	0,		DIGIT | ID_CHAR },
/*   2 */  { parseNumber,	0,		DIGIT | ID_CHAR },
/*   3 */  { parseNumber,	0,		DIGIT | ID_CHAR },
/*   4 */  { parseNumber,	0,		DIGIT | ID_CHAR },
/*   5 */  { parseNumber,	0,		DIGIT | ID_CHAR },
/*   6 */  { parseNumber,	0,		DIGIT | ID_CHAR },
/*   7 */  { parseNumber,	0,		DIGIT | ID_CHAR },
/*   8 */  { parseNumber,	0,		DIGIT | ID_CHAR },
/*   9 */  { parseNumber,	0,		DIGIT | ID_CHAR },
/*   : */  { parseColon,	0,		0 },
/*   ; */  { 0,			SEMICOLON, 	0 },
/*   < */  { parseBinOP,	0,		SPECIAL_CHAR },
/*   = */  { parseBinOP,	0,		SPECIAL_CHAR },
/*   > */  { parseBinOP,	0,		SPECIAL_CHAR },
/*   ? */  { parseBinOP,	0,		SPECIAL_CHAR },
/*   @ */  { parseBinOP,	0,		SPECIAL_CHAR },
/*   A */  { parseIdent,	0,		ID_CHAR },
/*   B */  { parseIdent,	0,		ID_CHAR },
/*   C */  { parseIdent,	0,		ID_CHAR },
/*   D */  { parseIdent,	0,		ID_CHAR },
/*   E */  { parseIdent,	0,		ID_CHAR },
/*   F */  { parseIdent,	0,		ID_CHAR },
/*   G */  { parseIdent,	0,		ID_CHAR },
/*   H */  { parseIdent,	0,		ID_CHAR },
/*   I */  { parseIdent,	0,		ID_CHAR },
/*   J */  { parseIdent,	0,		ID_CHAR },
/*   K */  { parseIdent,	0,		ID_CHAR },
/*   L */  { parseIdent,	0,		ID_CHAR },
/*   M */  { parseIdent,	0,		ID_CHAR },
/*   N */  { parseIdent,	0,		ID_CHAR },
/*   O */  { parseIdent,	0,		ID_CHAR },
/*   P */  { parseIdent,	0,		ID_CHAR },
/*   Q */  { parseIdent,	0,		ID_CHAR },
/*   R */  { parseIdent,	0,		ID_CHAR },
/*   S */  { parseIdent,	0,		ID_CHAR },
/*   T */  { parseIdent,	0,		ID_CHAR },
/*   U */  { parseIdent,	0,		ID_CHAR },
/*   V */  { parseIdent,	0,		ID_CHAR },
/*   W */  { parseIdent,	0,		ID_CHAR },
/*   X */  { parseIdent,	0,		ID_CHAR },
/*   Y */  { parseIdent,	0,		ID_CHAR },
/*   Z */  { parseIdent,	0,		ID_CHAR },
/*   [ */  { 0,			OPEN_BRACKET, 	0 },
/*   \ */  { parseBinOP,	0,		SPECIAL_CHAR },
/*   ] */  { 0,			CLOSE_BRACKET,	0 },
/*   ^ */  { 0,			UPARROW, 	0 },
/*   _ */  { 0,			ASSIGN,		ID_CHAR },
/*   ` */  { invalid,		0,		0 },
/*   a */  { parseIdent,	0,		ID_CHAR },
/*   b */  { parseIdent,	0,		ID_CHAR },
/*   c */  { parseIdent,	0,		ID_CHAR },
/*   d */  { parseIdent,	0,		ID_CHAR },
/*   e */  { parseIdent,	0,		ID_CHAR },
/*   f */  { parseIdent,	0,		ID_CHAR },
/*   g */  { parseIdent,	0,		ID_CHAR },
/*   h */  { parseIdent,	0,		ID_CHAR },
/*   i */  { parseIdent,	0,		ID_CHAR },
/*   j */  { parseIdent,	0,		ID_CHAR },
/*   k */  { parseIdent,	0,		ID_CHAR },
/*   l */  { parseIdent,	0,		ID_CHAR },
/*   m */  { parseIdent,	0,		ID_CHAR },
/*   n */  { parseIdent,	0,		ID_CHAR },
/*   o */  { parseIdent,	0,		ID_CHAR },
/*   p */  { parseIdent,	0,		ID_CHAR },
/*   q */  { parseIdent,	0,		ID_CHAR },
/*   r */  { parseIdent,	0,		ID_CHAR },
/*   s */  { parseIdent,	0,		ID_CHAR },
/*   t */  { parseIdent,	0,		ID_CHAR },
/*   u */  { parseIdent,	0,		ID_CHAR },
/*   v */  { parseIdent,	0,		ID_CHAR },
/*   w */  { parseIdent,	0,		ID_CHAR },
/*   x */  { parseIdent,	0,		ID_CHAR },
/*   y */  { parseIdent,	0,		ID_CHAR },
/*   z */  { parseIdent,	0,		ID_CHAR },
/*   { */  { invalid,		0,		0 },
/*   | */  { parseBinOP,	0,		SPECIAL_CHAR },
/*   } */  { invalid,		0,		0 },
/*   ~ */  { parseBinOP,	0,		SPECIAL_CHAR },
/*  ^? */  { invalid,		0,		0 }
};

int
yylex(lvalp)
     YYSTYPE *lvalp;
{
#if defined(USE_MONCONTROL) || defined(LEXDEBUG)
  int result;

#ifdef USE_MONCONTROL
  moncontrol(1);
  result = myyylex(lvalp);
  moncontrol(0); 
#else
  result = myyylex(lvalp);
  printToken(result, lvalp);
#endif

  return (result);
}

int
myyylex(lvalp, llocp)
     YYSTYPE *lvalp;
{
#endif /* USE_MONCONTROL */
  int		ic, result, tokenStartPos = 0;

  if (compileCode) {
    /* The internal token is a trick to make the grammar be "conditional".
     * Normally, the allowable syntax for internal compilation is that of
     * a single method (without the terminating BANG).  However, would make
     * for an ambiguous grammar since a method declaration could look like
     * an immediate expression.  So, when the compiler is called internally,
     * we force the first token returned by the lexer to be INTERNAL_TOKEN
     * and make the top most production in the grammar use INTERNAL_TOKEN as
     * the first token of an internal method.
     * The same happens when you have a call to methodsFor: - INTERNAL_TOKEN
     * is pushed so that the grammar switches to the method_list production. */
    compileCode = false;
    return (INTERNAL_TOKEN);
  }

  while ((ic = nextChar()) != EOF) {
    if ((charTab[ic].charClass & WHITE_SPACE) == 0) {
      if (methodStartPos < 0) {
	tokenStartPos = getCurFilePos();
      }

      if (charTab[ic].lexFunc) {
	result = (*charTab[ic].lexFunc)(ic, lvalp);
	if (result) {
	  if (methodStartPos < 0) { /* only do this here to ignore comments */
	    methodStartPos = tokenStartPos - 1;
	  }
	  return (result);
	}
      } else if (charTab[ic].retToken) {
	return (charTab[ic].retToken);
      } else {
	errorf("Unknown character %d in input stream, ignoring", ic);
	hadError = true;
      }
    }
  }
  return (0);			/* keep fussy compilers happy */
}

#ifdef unused
/**/void
/**/initLexer()
/**/{
/**/}
#endif



int
invalid(c, lvalp)
     char	c;
     YYSTYPE *lvalp;
{
  char		charName[3], *cp;

  cp = charName;

  if (c < ' ' || c > '~') {
    *cp++ = '^';
    c &= 127;			/* strip high bit */
    c ^= 64;			/* uncontrolify */
  }

  *cp++ = c;
  *cp++ = '\0';
  
  errorf("Invalid character %s", charName);
  hadError = true;

  return (0);			/* tell the lexer to ignore this */
}


/*
 *	static int comment(c, lvalp)
 *
 * Description
 *
 *	Scan a comment, but return 0 to indicate to the lexer that it's to be
 *	ignored (since it is a comment).
 *
 * Inputs
 *
 *	c     : first character of the comment (the delimiter).  Not terribly
 *		useful, but it's what the lexer calls us with.
 *	lvalp : ignored...passed because we're invoked indirectly and some of
 *		the functions that could be called require this parameter.
 *
 * Outputs
 *
 *	0, which tells the lexer to ignore this token.
 */
int
comment(c, lvalp)
     char	c;
     YYSTYPE *lvalp;
{
  scanStringoid(c, "comment");

  return (0);
}

/*
 *	static int charLiteral(c, lvalp)
 *
 * Description
 *
 *	parse a character literal.
 *
 * Inputs
 *
 *	c     : input character -- ignored
 *	lvalp : ignored -- passed because we're called indirectly.
 *
 * Outputs
 *
 *	CHAR_LITERAL token normally; 0 on EOF.
 */
int
charLiteral(c, lvalp)
     char	c;
     YYSTYPE *lvalp;
{
  int		ic;

  ic = nextChar();
  if (ic == EOF) {
    errorf("Unterminated character literal, attempting recovery");
    unreadChar(ic);
    hadError = true;
    return (0);
  } else {
    lvalp->cval = ic;
    return (CHAR_LITERAL);
  }
}

int
parseColon(c, lvalp)
     char	c;
     YYSTYPE *lvalp;
{
  int		ic;

  ic = nextChar();
  if (ic == '=') {		/* parse :=, compatibility mode assign */
    return (ASSIGN);
  } else {
    unreadChar(ic);
  }

  return (COLON);
}


int
parseBinOP(c, lvalp)
     char	c;
     YYSTYPE *lvalp;
{
  char		buf[3], *bp;
  int		ic;

  bp = buf;
  *bp++ = c;

  ic = nextChar();
  if (c == '<') {
    if (ic == 'p') {
      if (parsePrimitive(ic, lvalp)) {
	return (PRIMITIVE_START);
      }
    }
  }

  if (ic != EOF && (charTab[ic].charClass & SPECIAL_CHAR)) {
    *bp++ = ic;			/* accumulate next char */
  } else {
    unreadChar(ic);
    if (c == '-' && isDigit(ic)) {
      return (parseNumber('-', lvalp));
    }
  }
#ifdef old_code /* Fri Apr 19 20:37:57 1991 */
/**/  if (c == '-') {
/**/    if (ic == '>') {		/* -> */
/**/      *bp++ = ic;
/**/    } else {
/**/      unreadChar(ic);
/**/      if (isDigit(ic)) {
/**/	return (parseNumber('-', lvalp));
/**/      }
/**/    }
/**/  } else {
/**/    if (ic != EOF && (charTab[ic].charClass & SPECIAL_CHAR)) {
/**/      *bp++ = ic;
/**/    } else {
/**/      unreadChar(ic);
/**/    }
/**/  }
#endif /* old_code Fri Apr 19 20:37:57 1991 */

  *bp = '\0';

#ifdef old_code /* Fri Apr 19 20:37:57 1991 */
/**/  if (strcmp(buf, "!") == 0) {
/**/    /* technically, token BANG has no string value, it's just a simple token, */
/**/    /* so we return from here before we set the token's value */
/**/    return (BANG);
/**/  } else if (strcmp(buf, "!!") == 0) {
/**/    unreadChar('!');
/**/    return (BANG);
/**/  }
#endif
  
  lvalp->sval = strdup(buf);
  
  if (strcmp(buf, "|") == 0) {
    return (VERTICAL_BAR);
  } else {
    return (BINOP);
  }
}

int
stringLiteral(c, lvalp)
     char	c;
     YYSTYPE *lvalp;
{
  lvalp->sval = strdup(scanStringoid(c, "string"));
  return (STRING_LITERAL);
}

mst_Boolean
parsePrimitive(c, lvalp)
     char	c;
     YYSTYPE *lvalp;
{
  mst_Boolean 	result;

  parseIdent(c, lvalp);
  result = (strcmp(lvalp->sval, "primitive:") == 0);
  xfree(lvalp->sval);
  return (result);
}

int
parseIdent(c, lvalp)
     char	c;
     YYSTYPE *lvalp;
{
  int		ic, identType;
  
  initStrBuf();
  addStrBufChar(c);
  
  identType = IDENTIFIER;
  
  for (;;) {
    while (((ic = nextChar()) != EOF) && (charTab[ic].charClass & ID_CHAR)) {
      addStrBufChar(ic);
    }
    
    if (ic == ':') {		/* we have a keyword if id ends with : */
      ic = nextChar();
      unreadChar(ic);
      if (ic == '=') {		/* except if we have 'foo:=' => 'foo :=' */
	if (identType != IDENTIFIER) {
	  errorf("Malformed symbol literal");
	  break;
	}
	unreadChar(':');
	break;
      }
      addStrBufChar(':');
      if (ic == EOF || (charTab[ic].charClass & ID_CHAR) == 0
	  || (charTab[ic].charClass & DIGIT) != 0) {
	if (identType == IDENTIFIER) {
	  /* first time through */
	  identType = KEYWORD;
	}
	break;
      }
      identType = SYMBOL_KEYWORD;
    } else {
      unreadChar(ic);
      break;
    }
  }
  
  lvalp->sval = strdup(curStrBuf());
  
  return (identType);
}

int
parseNumber(c, lvalp)
     char	c;
     YYSTYPE *lvalp;
{
  int		base, exponent, ic;
  double	num, floatExponent;
  mst_Boolean	mantissaParsed = false, isNegative = false, dotSeen = false;
  double	scale;
  
  base = 10;
  exponent = 0;
  scale = 0.0;
  ic = c;

  if (ic != '-') {
    parseDigits(ic, 10, &num);
    ic = nextChar();
    if (ic == 'r') {
      base = (int)num;
      ic = nextChar();
    } else {
      mantissaParsed = true;
    }
  }

  /*
   * here we've either
   *  a) parsed base, an 'r' and are sitting on the following character
   *  b) parsed the integer part of the mantissa, and are sitting on the char
   *     following it, or
   *  c) parsed nothing and are sitting on a - sign.
   */
  if (!mantissaParsed) {
    if (ic == '-') {
      isNegative = true;
      ic = nextChar();
    }
    
    parseDigits(ic, base, &num);
    ic = nextChar();
  }

  if (ic == '.') {
    ic = nextChar();
    if (!isDigit(ic)) {
      /* OOPS...we gobbled the '.' by mistake...it was a statement boundary
	 delimiter.  We have an integer that we need to return, and need to
	 push back both the . and the character that we just read. */
      unreadChar(ic);
      ic = '.';
    } else {
      dotSeen = true;
      parseFraction(ic, base, &num, &scale);
      ic = nextChar();
    }
  }

  if (ic == 'e') {
    ic = nextChar();
    if (ic == '-') {
      parseDigits(nextChar(), 10, &floatExponent);
      exponent -= (int)floatExponent;
    } else {
      parseDigits(ic, 10, &floatExponent);
      exponent += (int)floatExponent;
    }
  } else {
    unreadChar(ic);
  }

  if (scale != 0.0) {
    num *= scale/*pow((double)base, (double)exponent)*/;
  }

  if (exponent != 0) {
    num *= pow((double)base, (double)exponent);
  }

  if (isNegative) {
    num = -num;
  }
    
  if (dotSeen) {
    lvalp->fval = num;
    return (FLOATING_LITERAL);
  } else {
    lvalp->ival = (long)num;

    /* I don't use 1L because I am not certain all C compilers handle it */
    if (num < -(((long)1) << ((SIZEOF_LONG * 8) - 2))
	|| num >= ( ((long)1) << ((SIZEOF_LONG * 8) - 2))) {
      /* at least warn the guy... */
      errorf("Integer literal too large to be represented in Smalltalk", num);
      hadError = true;
    }
      
    return (INTEGER_LITERAL);
  }
}

mst_Boolean
parseDigits(c, base, numPtr)
     char	c;
     int	base;
     double	*numPtr;
{
  double	result;
  mst_Boolean	oneDigit = false;

  for (result = 0.0; isBaseDigit(c, base); c = nextChar()) {
    result *= base;
    oneDigit = true;
    result += digitToInt(c, base);
  }

  if (!oneDigit) {
    errorf("Unexpected EOF while scanning number");
    hadError = true;
  }

  unreadChar(c);

  *numPtr = result;

  return (true);
}

mst_Boolean
parseFraction(c, base, numPtr, scalePtr)
     char	c;
     int	base;
     double	*numPtr, *scalePtr;
{
  double	scale;
  double	num;

  scale = 1.0;

  for (num = *numPtr; isBaseDigit(c, base); c = nextChar()) {
    num *= base;
    num += digitToInt(c, base);
    scale /= base;
  }

  unreadChar(c);

  *numPtr = num;
  *scalePtr = scale;

  return (true);
}


int
digitToInt(c, base)
     char	c;
     int	base;
{
  if (c < '0' || (c > '9' && c < 'A') || c > 'Z') {
    errorf("Invalid digit %c in number", c);
    hadError = true;
    return (0);
  }

  if (c >= 'A') {
    c = c - 'A' + 10;
  } else {
    c -= '0';
  }

  if (c >= base) {
    errorf("Digit '%c' too large for base %d", c, base);
    hadError = true;
    return (0);
  }

  return (c);
}

mst_Boolean
isBaseDigit(c, base)
     char	c;
     int	base;
{
  if (c < '0' || (c > '9' && c < 'A') || c > 'Z') {
    return (false);
  }
  
  if (c >= 'A') {
    c = c - 'A' + 10;
  } else {
    c -= '0';
  }

  return (c < base);
}


mst_Boolean
isDigit(ic)
     int	ic;
{
  return ((charTab[ic].charClass & DIGIT) != 0);
}

char *
scanStringoid(startChar, typeName)
     char	startChar, *typeName;
{
  int		ic;

  initStrBuf();

  for (;;) {
    ic = nextChar();
    if (ic == EOF) {
      errorf("Unterminated %s, attempting recovery", typeName);
      hadError = true;
      return (curStrBuf());
    }
    if (ic == startChar) {
      /* check for doubled delimiters */
      ic = nextChar();
      if (ic != startChar) {
	unreadChar(ic);
	return (curStrBuf());
      }
    }
    addStrBufChar(ic);
  }
  
}

/*
 * Report an error to the user.  ### Will show position in the text of
 * the error at some point.
 */

#define ERROR_ARGS	arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8
void
errorf(str, ERROR_ARGS)
     char	*str;
     long	ERROR_ARGS;
{
  char		errStr[256];

  if (reportErrors) {
    fflush(stdout);
  }
  lineStamp();
  if (reportErrors) {
    fprintf(stderr, str, ERROR_ARGS);
    fprintf(stderr, "\n");
    fflush(stderr);
  } else {
    if (firstErrorStr == nil) {
      sprintf(errStr, str, ERROR_ARGS);
      firstErrorStr = strdup(errStr);
    }
  }
}
#undef ERROR_ARGS



/* ??? is void the right type */
void
yyerror(s)
     char	*s;
{
  errorf("%s", s);
}

void
lineStamp()
{
  if (reportErrors) {
    if (inStream) {
      if (inStream->fileName) {
	fprintf(stderr, "%s:", inStream->fileName);
      }
      fprintf(stderr, "%d: ", inStream->line);
    } else {
      fprintf(stderr, "gst: ");
    }
  } else {			/* called internally with error handling */
    if (inStream) {
      if (inStream->fileName) {
	if (firstErrorStr == NULL) {
	  firstErrorFile = strdup(inStream->fileName);
	}
      }
      if (firstErrorStr == NULL) {
	firstErrorLine = inStream->line;
      }
    } else {
      if (firstErrorStr == NULL) {
	firstErrorLine = -1;
      }
    }
  }
}



StreamType
getCurStreamType()
{
  if (inStream) {
    return (inStream->type);
  } else {
    return (unknownStreamType);
  }
}

OOP
getCurString()
{
  if (inStream && inStream->type == stringStreamType) {
    return (stringNew(inStream->st_str.strBase));
  } else {
    return (nilOOP);
  }
}

OOP
getCurFileName()
{
  char		*fullFileName;

  if (inStream && inStream->type == fileStreamType) {
    if (inStream->fileNameOOP == nilOOP) {
      if (strcmp(inStream->fileName, "stdin") == 0) {
	fullFileName = strdup(inStream->fileName);
      } else {
	fullFileName = getFullFileName(inStream->fileName);
      }
      inStream->fileNameOOP = stringNew(fullFileName);
    }
    return (inStream->fileNameOOP);
  } else {
    return (nilOOP);
  }
}

#ifdef HAVE_READLINE
OOP
getCurReadline()
{
  if (inStream && inStream->type == readlineStreamType) {
    return (stringNew(inStream->st_str.strBase));
  } else {
    return (nilOOP);
  }
}
#endif /* HAVE_READLINE */

int
getMethodStartPos()
{
  return (methodStartPos);
}

void
clearMethodStartPos()
{
  methodStartPos = -1;
}

int
getCurFilePos()
{
  /* ### not sure about this particular use -- the current file pos must be
   * from the change log, but currently the FileSegments we produce are from
   * the input file... so they lose.
   * The original reason for implementing the change log was to make recom-
   * piles of a method lying in a changed file work... but this way not even
   * recompiles of methods lying in an unchanged file do work!
   */
#ifdef not_working_for_now
/**/  if (changeStr) {
/**/    return (ftell(changeStr));
/**/  } else
#endif

  if (inStream && inStream->type == fileStreamType) {
    return (ftell(inStream->st_file) + inStream->fileOffset);
  } else {
    return (-1);
  }
}

void
initChangesStream()
{
  if (changeFileName) {
    changeStr = fopen(changeFileName, "a");
  }
}

/*
 *	void resetChangesFile()
 *
 * Description
 *
 *	Resets the changes file to be zero length.
 *
 */
void
resetChangesFile()
{
  if (!changeFileName) {
    return;
  }
  if (changeStr) {
    fclose(changeStr);
  }
  remove(changeFileName);
  if (changeStr) {
    initChangesStream();
  }
}


int
nextChar()
{
  register int		ic;

  if (inStream->pushedBackCount > 0) {
    ic = inStream->pushedBackChars[--inStream->pushedBackCount];
    return (ic);
  } else {
    if (inStream->column == 0 && inStream->prompt) {
      if (emacsProcess) {
	printf("%c", EMACS_PROCESS_MARKER);
      }
      printf("st> ");
    }
    ic = myGetC(inStream);

    if (changeStr) {
      if (ic == EOF) {
	fflush(changeStr);
      } else {
	fputc(ic, changeStr);
      }
    }

    if (ic == '\n') {		/* a new line that was not pushed back */
      inStream->line++;
      inStream->column = 0;
    } else {
      inStream->column++;
    }
    return (ic);
  }
}

/*
 *	static void unreadChar(ic)
 *
 * Description
 *
 *	Push character 'ic' back into the input queue.  Allows for two
 *	character pushback currently.  This solves the problem of lexing 3. and
 *	then finding out that what we should have lexed was 3 followed by . as
 *	a statement terminator.
 *
 * Inputs
 *
 *	ic    : character to push back into the input stream.
 *
 */
void
unreadChar(ic)
int	ic;
{
  inStream->pushedBackChars[inStream->pushedBackCount++] = ic;
}



/***********************************************************************
 *
 *	Generic "stream" interface.  A stream is an abstraction for input and
 *	output.  It is most like common lisp streams.  Basically, these streams
 *	provide transparent reading from either a Smalltalk string, or a UNIX
 *	file.  They stack, and the previous stream can be restored by doing a
 *	"popStream" which optionally "closes" the current stream and goes back 
 *	to using the previous stream.
 *
 * 	The `readline()' interface:
 *
 * 		The behavior is like the Smalltalk-String interface.
 * 		The end-of-string or a NULL strBase-pointer decides
 * 		to read in a new line.  The prompt is still shown by
 * 		the readline() call.
 * 		
 ***********************************************************************/

void
parseStream()
{
#ifdef NO_PARSE
  YYSTYPE yylval;
  while(yylex(&yylval)) ;
#else /* debugging the lexer */
  extern int yyparse();
  yyparse();
#endif /* NO_PARSE */
}


void
popStream(closeIt)
     mst_Boolean closeIt;
{
  Stream	oldStream;

  oldStream = inStream;
  inStream = inStream->prevStream;

  if (closeIt && oldStream) {
    myClose(oldStream);
    xfree(oldStream);
  }
}

void
pushUNIXFile(file, fileName)
     FILE	*file;
     char	*fileName;
{
  Stream	newStream;

  newStream = pushNewStream(fileStreamType);
  newStream->st_file = file;
  newStream->fileName = fileName;
  newStream->prompt = isatty(fileno(file));
}

void
pushSmalltalkString(stringOOP)
     OOP	stringOOP;
{
  Stream	newStream;

  newStream = pushNewStream(stringStreamType);

  newStream->st_str.strBase = (char *)toCString(stringOOP);
  newStream->st_str.str = newStream->st_str.strBase;
  newStream->fileName = "a Smalltalk string";
  newStream->prompt = false;
}

void 
pushCString(string)
     char	*string;
{
  Stream	newStream;

  newStream = pushNewStream(stringStreamType);

  newStream->st_str.strBase = string;
  newStream->st_str.str = newStream->st_str.strBase;
  newStream->fileName = "a C string";
  newStream->prompt = false;
}

#ifdef HAVE_READLINE
void
pushReadlineString()
{
  Stream	newStream;

  newStream = pushNewStream(readlineStreamType);

  newStream->st_str.strBase = 0;	/* force readline() but no free() */
  newStream->st_str.str = 0;
  newStream->fileName = "stdin";	/* that's where we get input from */
  newStream->prompt = false;		/* prompt is shown by readline() */
}
#endif /* HAVE_READLINE */

Stream
pushNewStream(type)
     StreamType type;
{
  Stream	newStream;

  newStream = (Stream)xmalloc(sizeof(struct StreamStruct));

  newStream->pushedBackCount = 0;
  newStream->line = 1;
  newStream->column = 0;
  newStream->fileOffset = 0;
  newStream->type = type;
  newStream->fileNameOOP = nilOOP;
  newStream->prevStream = inStream;
  inStream = newStream;

  return (newStream);
}


/*
 *	void setStreamInfo(line, fileName, fileOffset)
 *
 * Description
 *
 *	This function overrides the file type information for the current stream.
 *	It is typically used by fileIn type methods when filing in a subsection
 *	of a real file via a temporary file what the real source of the text
 *	is.
 *
 * Inputs
 *
 *	line  : line number that the input starts on
 *	fileName: 
 *		The name of the file, only used for file type streams.
 *	fileOffset: 
 *		The byte offset from the start of the actual file where this
 *		given portion of code starts.
 *
 */
void
setStreamInfo(line, fileName, fileOffset)
     int	line, fileOffset;
     char	*fileName;
{
  inStream->line = line;
  if (inStream->type == fileStreamType) {
    inStream->fileName = fileName;
    inStream->fileOffset = fileOffset;
  }
}

int
myGetC(stream)
     Stream	stream;
{
  int		ic = 0;

  if (stream->type == stringStreamType) {
    ic = *stream->st_str.str++;
    return ((ic == '\0') ? EOF : ic);
  } else if (stream->type == fileStreamType) {
    ic = getc(stream->st_file);
    return (ic);
#ifdef HAVE_READLINE
  } else if (stream->type == readlineStreamType) {
    char *r_line;
    int r_len;

    if (stream->st_str.strBase) {
      ic = *stream->st_str.str++;
      if (ic) {
        return (ic);
      }
      /* If null, read a new line */
    }

    if(stream->st_str.strBase) {
      xfree(stream->st_str.strBase);
      stream->st_str.strBase = 0;
    }
    r_line = readline("st> ");
    if (!r_line) {
      /* return value of NULL indicates EOF */
      return (EOF);
    }
    if (*r_line) {
      /* add only non-empty lines. */
      add_history(r_line);
    }

    /* tack on the newline, not returned by readline() */
    r_len = strlen(r_line);
    r_line = xrealloc(r_line, (unsigned) (r_len + 2));
    if (!r_line) {
      errorf("Out of memory reallocating linebuffer space");
      stream->st_str.str = stream->st_str.strBase = 0;
      ic = '\n';			/* return a newline ... */
    } else {
      r_line[r_len] = '\n';
      r_line[r_len+1] = '\0';
      stream->st_str.str = stream->st_str.strBase = r_line;
      ic = *stream->st_str.str++;
    }
#endif /* HAVE_READLINE */
  } else {
    errorf("Bad stream type passed to myGetC");
    hadError = true;
  }
  return (ic);
}

#ifdef old_code /* Sat Mar 23 21:00:38 1991 */
/**/static void myUngetC(ic, stream)
/**/int	ic;
/**/Stream	stream;
/**/{
/**/  if (stream->type == stringStreamType) {
/**/    stream->st_str.str--;
/**/  } else if (stream->type == fileStreamType) {
/**/    ungetc(ic, stream->st_file);
/**/#ifdef HAVE_READLINE
/**/  } else if (stream->type == readlineStreamType) {
/**/    if (stream->st_str.str > stream->st_str.strBase) {
/**/      *--stream->st_str.str = ic;
/**/    } else {
/**/      errorf("Cannot unget character to readline stream");
/**/    }
/**/#endif /* HAVE_READLINE */
/**/  } else {
/**/    errorf("Bad stream type passed to myUngetC");
/**/    hadError = true;
/**/  }
/**/}
#endif /* old_code Sat Mar 23 21:00:38 1991 */

void
myClose(stream)
     Stream	stream;
{
  if (stream->type == stringStreamType) {
    xfree(stream->st_str.strBase);
  } else if (stream->type == fileStreamType) {
    fclose(stream->st_file);
#ifdef HAVE_READLINE
  } else if (stream->type == readlineStreamType) {
    if (stream->st_str.strBase) {
      xfree(stream->st_str.strBase);
      stream->st_str.strBase = 0;
    }
#endif /* HAVE_READLINE */
  } else {
    errorf("Bad stream type passed to myClose");
    hadError = true;
  }
}


#ifdef HAVE_READLINE

void
initializeReadline()
{
  /* Allow conditional parsing of the ~/.inputrc file. */
  rl_readline_name = "Smalltalk";
  rl_bind_key ('\t', rl_insert);
}
     
#endif /* HAVE_READLINE */




/* Return true if current compilation stream is a FileStream associated
 * with a path relative to the kernel directory. 
 */ 
mst_Boolean
isKernelFile()
{
  char		*fullFileName, *fullFileNameTest;
  mst_Boolean	result;	

  if (storeNoSource) {
    return (true);
  }
  if ((strcmp(inStream->fileName, "stdin") == 0)
      || (getMethodStartPos() > getCurFilePos())) {
    return (true);
  }

  fullFileName = fullFileNameTest = getFullFileName(inStream->fileName);
#if defined(MSDOS) || defined(_WIN32) || defined(__OS2__)
  if (fullFileName[1] == ':') {
    fullFileName += 2;		/* skip over "<drive>:"  */
  }
#endif
  result = (strstr(fullFileNameTest, KERNEL_PATH)  == fullFileNameTest);
  xfree(fullFileName);

  return (result);
}

/* Return current method source string from open FileStream */
char *
getMethodSourceFromCurFile()
{
  char 		*buf;
  int		endPos, length, startPos;

  startPos = getMethodStartPos();
  endPos = getCurFilePos(); 
  /* The second -1 removes the terminating '!' */
  length = endPos  - startPos - 1 - 1;
  buf = (char *)xmalloc(length + 1);
  fseek(inStream->st_file, startPos, 0);
  fread(buf, length, 1, inStream->st_file);
  buf[length] = '\0';
  /* Restore old file position */
  fseek(inStream->st_file, endPos, 0); 
  return(buf);
}



#ifdef LEXDEBUG
void
printToken(token, yylval)
     int token;
     YYSTYPE *yylval;
{
  switch(token) {
    case 0:
      break;
    case DOT:
      printf("DOT\n");
      break;
    case BANG:
      printf("BANG\n");
      break;
    case COLON:
      printf("COLON\n");
      break;
    case VERTICAL_BAR:
      printf("VERTICAL_BAR\n");
      break;
    case UPARROW:
      printf("UPARROW\n");
      break;
    case ASSIGN:
      printf("ASSIGN\n");
      break;
    case SHARP:
      printf("SHARP\n");
      break;
    case SEMICOLON:
      printf("SEMICOLON\n");
      break;
    case OPEN_PAREN:
      printf("OPEN_PAREN\n");
      break;
    case CLOSE_PAREN:
      printf("CLOSE_PAREN\n");
      break;
    case PRIMITIVE_START:
      printf("OPEN_BRACKET\n");
      break;
    case OPEN_BRACKET:
      printf("OPEN_BRACKET\n");
      break;
    case CLOSE_BRACKET:
      printf("CLOSE_BRACKET\n");
      break;
    case INTERNAL_TOKEN:
      printf("INTERNAL_TOKEN\n");
      break;
    case IDENTIFIER:
      printf("IDENTIFIER: %s\n", yylval->sval);
      break;
    case KEYWORD:
      printf("KEYWORD: %s\n", yylval->sval);
      break;
    case SYMBOL_KEYWORD:
      printf("SYMBOL_KEYWORD: %s\n", yylval->sval);
      break;
    case INTEGER_LITERAL:
      printf("INTEGER_LITERAL: %d\n", yylval->ival);
      break;
    case FLOATING_LITERAL:
      printf("FLOATING_LITERAL: %g\n", yylval->fval);
      break;
    case CHAR_LITERAL:
      printf("CHAR_LITERAL: %c\n", yylval->cval);
      break;
    case STRING_LITERAL:
      printf("STRING_LITERAL: %s\n", yylval->sval);
      break;
    case BINOP:
      printf("BINOP: %s\n", yylval->sval);
      break;
  }
}
#endif
