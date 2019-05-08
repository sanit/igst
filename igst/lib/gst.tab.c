
/*  A Bison parser, made from gst.y
 by  GNU Bison version 1.25
  */

#define YYBISON 1  /* Identify Bison output.  */

#define	BANG	258
#define	COLON	259
#define	UPARROW	260
#define	DOT	261
#define	ASSIGN	262
#define	SHARP	263
#define	SEMICOLON	264
#define	OPEN_PAREN	265
#define	CLOSE_PAREN	266
#define	OPEN_BRACKET	267
#define	CLOSE_BRACKET	268
#define	PRIMITIVE_START	269
#define	INTERNAL_TOKEN	270
#define	IDENTIFIER	271
#define	KEYWORD	272
#define	STRING_LITERAL	273
#define	SYMBOL_KEYWORD	274
#define	BINOP	275
#define	VERTICAL_BAR	276
#define	INTEGER_LITERAL	277
#define	FLOATING_LITERAL	278
#define	CHAR_LITERAL	279

#line 61 "gst.y"

#include "gst.h"
#include "sym.h"
#include "tree.h"
#include "dict.h"
#include "comp.h"
#include "lex.h"
#include <stdio.h>
#if defined(STDC_HEADERS)
#include <string.h>
#endif

#define YYDEBUG 1

extern mst_Boolean		quietExecution;

#line 86 "gst.y"
typedef union{
  char		cval;
  double	fval;
  long		ival;
  char		*sval;
  TreeNode	node;
} YYSTYPE;
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		142
#define	YYFLAG		-32768
#define	YYNTBASE	25

#define YYTRANSLATE(x) ((unsigned)(x) <= 279 ? yytranslate[x] : 71)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    23,    24
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     3,     5,     6,     8,    10,    13,    15,    19,    24,
    26,    30,    33,    36,    40,    45,    47,    50,    52,    54,
    56,    58,    60,    62,    65,    69,    71,    72,    76,    77,
    80,    84,    86,    89,    90,    92,    96,    98,   102,   106,
   107,   109,   111,   114,   117,   121,   123,   125,   127,   129,
   131,   133,   137,   139,   141,   143,   145,   147,   149,   151,
   154,   157,   159,   161,   163,   165,   167,   169,   172,   175,
   179,   181,   184,   186,   188,   190,   192,   194,   200,   201,
   204,   207,   211,   213,   215,   217,   220,   222,   224,   228,
   230,   232,   235,   238,   242,   245,   248,   252,   254,   257
};

static const short yyrhs[] = {    26,
    31,     0,    27,     0,     0,    15,     0,    28,     0,    27,
    28,     0,    29,     0,    29,    26,     3,     0,    29,    26,
    30,     3,     0,     3,     0,    39,    41,     3,     0,     1,
     3,     0,    31,     3,     0,    30,    31,     3,     0,    32,
    39,    38,    41,     0,    33,     0,    34,    35,     0,    36,
     0,     1,     0,    16,     0,    20,     0,    21,     0,    16,
     0,    37,    35,     0,    36,    37,    35,     0,    17,     0,
     0,    14,    22,    20,     0,     0,    21,    21,     0,    21,
    40,    21,     0,    35,     0,    40,    35,     0,     0,    42,
     0,     5,    44,    43,     0,    44,     0,    44,     6,    41,
     0,     1,     6,    41,     0,     0,     6,     0,    46,     0,
    45,    46,     0,    35,     7,     0,    45,    35,     7,     0,
    47,     0,    61,     0,    68,     0,    35,     0,    48,     0,
    58,     0,    10,    44,    11,     0,    49,     0,    50,     0,
    52,     0,    53,     0,    54,     0,    22,     0,    23,     0,
     8,    51,     0,     8,    18,     0,    16,     0,    34,     0,
    19,     0,    17,     0,    24,     0,    18,     0,     8,    55,
     0,    10,    11,     0,    10,    56,    11,     0,    57,     0,
    56,    57,     0,    49,     0,    51,     0,    53,     0,    52,
     0,    55,     0,    12,    59,    39,    41,    13,     0,     0,
    60,    21,     0,     4,    35,     0,    60,     4,    35,     0,
    62,     0,    64,     0,    66,     0,    63,    33,     0,    47,
     0,    62,     0,    65,    34,    63,     0,    63,     0,    64,
     0,    65,    67,     0,    37,    65,     0,    67,    37,    65,
     0,    61,    69,     0,     9,    70,     0,    69,     9,    70,
     0,    33,     0,    34,    63,     0,    67,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   121,   123,   124,   127,   131,   133,   136,   138,   139,   143,
   145,   152,   156,   168,   181,   187,   189,   190,   191,   198,
   202,   204,   207,   211,   213,   218,   222,   224,   232,   234,
   235,   239,   241,   245,   247,   250,   254,   255,   258,   265,
   267,   270,   272,   275,   277,   282,   284,   285,   288,   290,
   291,   292,   295,   297,   298,   299,   300,   303,   305,   308,
   310,   313,   315,   316,   317,   321,   325,   329,   333,   335,
   339,   341,   346,   348,   349,   350,   351,   354,   359,   361,
   367,   369,   374,   376,   377,   380,   384,   386,   389,   394,
   396,   399,   404,   407,   412,   417,   419,   424,   426,   428
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","BANG","COLON",
"UPARROW","DOT","ASSIGN","SHARP","SEMICOLON","OPEN_PAREN","CLOSE_PAREN","OPEN_BRACKET",
"CLOSE_BRACKET","PRIMITIVE_START","INTERNAL_TOKEN","IDENTIFIER","KEYWORD","STRING_LITERAL",
"SYMBOL_KEYWORD","BINOP","VERTICAL_BAR","INTEGER_LITERAL","FLOATING_LITERAL",
"CHAR_LITERAL","program","internal_marker","file_in","doit_and_method_list",
"doit","method_list","method","message_pattern","unary_selector","binary_selector",
"variable_name","keyword_variable_list","keyword","primitive","temporaries",
"variable_names","statements","non_empty_statements","optional_dot","expression",
"assigns","simple_expression","primary","literal","number","symbol_constant",
"symbol","character_constant","string","array_constant","array","array_constant_list",
"array_constant_elt","block","opt_block_variables","block_variable_list","message_expression",
"unary_expression","unary_object_description","binary_expression","binary_object_description",
"keyword_expression","keyword_binary_object_description_list","cascaded_message_expression",
"semi_message_list","message_elt", NULL
};
#endif

static const short yyr1[] = {     0,
    25,    25,    25,    26,    27,    27,    28,    28,    28,    29,
    29,    29,    30,    30,    31,    32,    32,    32,    32,    33,
    34,    34,    35,    36,    36,    37,    38,    38,    39,    39,
    39,    40,    40,    41,    41,    42,    42,    42,    42,    43,
    43,    44,    44,    45,    45,    46,    46,    46,    47,    47,
    47,    47,    48,    48,    48,    48,    48,    49,    49,    50,
    50,    51,    51,    51,    51,    52,    53,    54,    55,    55,
    56,    56,    57,    57,    57,    57,    57,    58,    59,    59,
    60,    60,    61,    61,    61,    62,    63,    63,    64,    65,
    65,    66,    67,    67,    68,    69,    69,    70,    70,    70
};

static const short yyr2[] = {     0,
     2,     1,     0,     1,     1,     2,     1,     3,     4,     1,
     3,     2,     2,     3,     4,     1,     2,     1,     1,     1,
     1,     1,     1,     2,     3,     1,     0,     3,     0,     2,
     3,     1,     2,     0,     1,     3,     1,     3,     3,     0,
     1,     1,     2,     2,     3,     1,     1,     1,     1,     1,
     1,     3,     1,     1,     1,     1,     1,     1,     1,     2,
     2,     1,     1,     1,     1,     1,     1,     2,     2,     3,
     1,     2,     1,     1,     1,     1,     1,     5,     0,     2,
     2,     3,     1,     1,     1,     2,     1,     1,     3,     1,
     1,     2,     2,     3,     2,     2,     3,     1,     2,     1
};

static const short yydefact[] = {     0,
     0,    10,     4,     0,     0,     0,     5,     7,     0,    12,
    23,    30,    32,     0,    19,    20,    26,    21,    22,     1,
    29,    16,     0,    18,     0,     6,     0,     0,     0,     0,
     0,    79,    67,    58,    59,    66,    49,     0,    35,    37,
     0,    42,    46,    50,    53,    54,    55,    56,    57,    51,
    47,    83,    90,    84,     0,    85,    48,    31,    33,    27,
    17,     0,    24,     8,     0,     0,     0,    40,     0,    62,
    65,    61,    64,    63,    60,    68,     0,     0,    29,     0,
    44,    11,     0,    49,    43,     0,    95,    86,     0,     0,
    92,     0,     0,    25,     9,     0,    13,    39,    41,    36,
    69,    73,    74,    76,    75,    77,     0,    71,    52,    81,
     0,     0,    80,    38,    45,    98,     0,   100,    96,     0,
    49,    87,    88,    89,    91,    93,     0,     0,    15,    14,
    70,    72,     0,    82,    99,    97,    94,    28,    78,     0,
     0,     0
};

static const short yydefgoto[] = {   140,
     5,     6,     7,     8,    65,    20,    21,    22,    23,    37,
    24,    25,    93,     9,    14,    38,    39,   100,    40,    41,
    42,    43,    44,    45,    46,   103,    47,    48,    49,   106,
   107,   108,    50,    79,    80,    51,    52,    53,    54,    55,
    56,   118,    57,    87,   119
};

static const short yypact[] = {   104,
    28,-32768,-32768,     6,    55,   129,-32768,    19,   203,-32768,
-32768,-32768,-32768,    80,-32768,-32768,-32768,-32768,-32768,-32768,
    17,-32768,    24,    38,    24,-32768,    44,    51,   251,    71,
   251,    66,-32768,-32768,-32768,-32768,    70,    91,-32768,    92,
   251,-32768,     9,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
    93,    62,    84,   123,   148,-32768,-32768,-32768,-32768,    89,
-32768,    24,-32768,-32768,   237,   103,   154,   105,   260,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,   106,    24,    17,    20,
-32768,-32768,   154,   108,-32768,   235,   109,-32768,   251,   251,
    38,    99,   179,-32768,-32768,   121,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,   275,-32768,-32768,-32768,
   223,    24,-32768,-32768,-32768,-32768,   251,    38,-32768,   235,
-32768,-32768,-32768,    84,-32768,    46,   251,   111,-32768,-32768,
-32768,-32768,   120,-32768,    84,-32768,    46,-32768,-32768,   135,
   136,-32768
};

static const short yypgoto[] = {-32768,
   130,-32768,   140,-32768,-32768,   -22,-32768,   -51,   -27,    -4,
-32768,   -23,-32768,   -17,-32768,   -60,-32768,-32768,   -13,-32768,
   101,   -81,-32768,   -63,-32768,   118,   -55,   -54,-32768,   119,
-32768,    49,-32768,-32768,-32768,-32768,   -78,   -69,   -77,   -73,
-32768,   116,-32768,-32768,    40
};


#define	YYLAST		299


static const short yytable[] = {    13,
    62,    88,    74,    60,    66,   102,    98,   122,   122,    59,
   123,   123,   125,   104,   105,    68,   126,    77,    61,   124,
    63,    11,   114,   112,   -87,   -87,    12,    89,   -87,   -87,
    10,    90,   129,     3,   116,   122,    84,     4,   123,    11,
   113,    74,    96,   102,    15,   122,    64,   135,   123,   125,
   133,   104,   105,   137,    17,    15,    67,    94,   117,    16,
    17,   111,    90,    18,    19,    18,    19,   127,   116,    78,
    16,    17,    88,   110,    18,    19,    81,   -88,   -88,    74,
    69,   -88,   -88,    88,   121,   121,    70,    71,    72,    73,
    18,    19,   117,    82,   127,    11,    90,    83,    89,    16,
    58,    86,    92,    -3,     1,    97,     2,   134,   -29,    89,
    99,   -29,   121,   -29,   115,   -29,   109,   120,     3,   -29,
   128,   -29,   121,   130,     4,   -29,   -29,   -29,    -2,     1,
   138,     2,   139,   -29,   141,   142,   -29,    27,   -29,   -91,
   -29,    85,   -91,   -91,   -29,    26,   -29,    75,    76,     4,
   -29,   -29,   -29,   -34,    28,   132,   -34,     0,    29,   136,
     0,    30,     0,    31,    17,    32,   -34,    18,    19,    11,
    91,    33,     0,     0,     0,    34,    35,    36,   -34,    28,
     0,   -34,     0,    29,     0,     0,    30,     0,    31,     0,
    32,     0,     0,     0,    11,     0,    33,     0,     0,     0,
    34,    35,    36,    28,     0,   -34,     0,    29,     0,     0,
    30,     0,    31,     0,    32,     0,     0,     0,    11,     0,
    33,     0,     0,    28,    34,    35,    36,    29,     0,     0,
    30,     0,    31,     0,    32,   -34,     0,    15,    11,    95,
    33,     0,     0,     0,    34,    35,    36,     0,     0,     0,
    16,    17,    16,    17,    18,    19,    18,    19,    30,     0,
    31,     0,    32,     0,     0,     0,    11,     0,    33,    69,
   101,     0,    34,    35,    36,    70,    71,    33,    73,    18,
    19,    34,    35,    36,    69,   131,     0,     0,     0,     0,
    70,    71,    33,    73,    18,    19,    34,    35,    36
};

static const short yycheck[] = {     4,
    24,    53,    30,    21,    27,    69,    67,    89,    90,    14,
    89,    90,    90,    69,    69,    29,    90,    31,    23,    89,
    25,    16,    83,     4,    16,    17,    21,    55,    20,    21,
     3,    55,    93,    15,    86,   117,    41,    21,   117,    16,
    21,    69,    65,   107,     1,   127,     3,   117,   127,   127,
   111,   107,   107,   127,    17,     1,     6,    62,    86,    16,
    17,    79,    86,    20,    21,    20,    21,    91,   120,     4,
    16,    17,   124,    78,    20,    21,     7,    16,    17,   107,
    10,    20,    21,   135,    89,    90,    16,    17,    18,    19,
    20,    21,   120,     3,   118,    16,   120,     6,   126,    16,
    21,     9,    14,     0,     1,     3,     3,   112,     5,   137,
     6,     8,   117,    10,     7,    12,    11,     9,    15,    16,
    22,    18,   127,     3,    21,    22,    23,    24,     0,     1,
    20,     3,    13,     5,     0,     0,     8,     8,    10,    17,
    12,    41,    20,    21,    16,     6,    18,    30,    30,    21,
    22,    23,    24,     0,     1,   107,     3,    -1,     5,   120,
    -1,     8,    -1,    10,    17,    12,    13,    20,    21,    16,
    55,    18,    -1,    -1,    -1,    22,    23,    24,     0,     1,
    -1,     3,    -1,     5,    -1,    -1,     8,    -1,    10,    -1,
    12,    -1,    -1,    -1,    16,    -1,    18,    -1,    -1,    -1,
    22,    23,    24,     1,    -1,     3,    -1,     5,    -1,    -1,
     8,    -1,    10,    -1,    12,    -1,    -1,    -1,    16,    -1,
    18,    -1,    -1,     1,    22,    23,    24,     5,    -1,    -1,
     8,    -1,    10,    -1,    12,    13,    -1,     1,    16,     3,
    18,    -1,    -1,    -1,    22,    23,    24,    -1,    -1,    -1,
    16,    17,    16,    17,    20,    21,    20,    21,     8,    -1,
    10,    -1,    12,    -1,    -1,    -1,    16,    -1,    18,    10,
    11,    -1,    22,    23,    24,    16,    17,    18,    19,    20,
    21,    22,    23,    24,    10,    11,    -1,    -1,    -1,    -1,
    16,    17,    18,    19,    20,    21,    22,    23,    24
};
#define YYPURE 1

/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/cygnus/b19/share/bison.simple"

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi)
#include <alloca.h>
#else /* not sparc */
#if defined (MSDOS) && !defined (__TURBOC__)
#include <malloc.h>
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
#include <malloc.h>
 #pragma alloca
#else /* not MSDOS, __TURBOC__, or _AIX */
#ifdef __hpux
#ifdef __cplusplus
extern "C" {
void *alloca (unsigned int);
};
#else /* not __cplusplus */
void *alloca ();
#endif /* not __cplusplus */
#endif /* __hpux */
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc.  */
#endif /* not GNU C.  */
#endif /* alloca not defined.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	return(0)
#define YYABORT 	return(1)
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, &yylloc, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval, &yylloc)
#endif
#else /* not YYLSP_NEEDED */
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif /* not YYLSP_NEEDED */
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
int yyparse (void);
#endif

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_memcpy(TO,FROM,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (to, from, count)
     char *to;
     char *from;
     int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (char *to, char *from, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 196 "/cygnus/b19/share/bison.simple"

/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
#ifdef __cplusplus
#define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else /* not __cplusplus */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
#endif /* not __cplusplus */
#else /* not YYPARSE_PARAM */
#define YYPARSE_PARAM_ARG
#define YYPARSE_PARAM_DECL
#endif /* not YYPARSE_PARAM */

int
yyparse(YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
      yyss = (short *) alloca (yystacksize * sizeof (*yyssp));
      __yy_memcpy ((char *)yyss, (char *)yyss1, size * sizeof (*yyssp));
      yyvs = (YYSTYPE *) alloca (yystacksize * sizeof (*yyvsp));
      __yy_memcpy ((char *)yyvs, (char *)yyvs1, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yystacksize * sizeof (*yylsp));
      __yy_memcpy ((char *)yyls, (char *)yyls1, size * sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 1:
#line 122 "gst.y"
{ compileMethod(yyvsp[0].node); ;
    break;}
case 4:
#line 128 "gst.y"
{ clearMethodStartPos(); ;
    break;}
case 8:
#line 138 "gst.y"
{ skipCompilation = false; ;
    break;}
case 9:
#line 139 "gst.y"
{ skipCompilation = false; ;
    break;}
case 11:
#line 146 "gst.y"
{ if (yyvsp[-1].node && !hadError) {
					    executeStatements(yyvsp[-2].node, yyvsp[-1].node,
							    quietExecution); 
                                          }
					  hadError = false;
                                        ;
    break;}
case 12:
#line 152 "gst.y"
{ hadError = false;
					  yyerrok; ;
    break;}
case 13:
#line 157 "gst.y"
{ if (!hadError) {
					    if (skipCompilation) {
					      freeTree(yyvsp[-1].node);
					    } else {
					      compileMethod(yyvsp[-1].node);
					      clearMethodStartPos();
					    }
					  } else {
					    hadError = false;
					  }
					;
    break;}
case 14:
#line 168 "gst.y"
{ if (!hadError) {
					    if (skipCompilation) {
					      freeTree(yyvsp[-1].node);
					    } else {
					      compileMethod(yyvsp[-1].node);
					      clearMethodStartPos();
					    }
					  } else {
					    hadError = false;
					  }
					;
    break;}
case 15:
#line 183 "gst.y"
{ yyval.node = makeMethod(yyvsp[-3].node, yyvsp[-2].node, yyvsp[-1].ival, yyvsp[0].node); ;
    break;}
case 16:
#line 188 "gst.y"
{ yyval.node = makeUnaryExpr(nil, yyvsp[0].sval); ;
    break;}
case 17:
#line 189 "gst.y"
{ yyval.node = makeBinaryExpr(nil, yyvsp[-1].sval, yyvsp[0].node); ;
    break;}
case 18:
#line 190 "gst.y"
{ yyval.node = makeKeywordExpr(nil, yyvsp[0].node); ;
    break;}
case 19:
#line 191 "gst.y"
{ errorf("Invalid message pattern");
					  hadError = true;
					  /* 14-Jul-95 00:29:27 removed this yyerrok */
					  /* yyerrok; */
					  yyval.node = nil; ;
    break;}
case 23:
#line 208 "gst.y"
{ yyval.node = makeVariable(yyvsp[0].sval); ;
    break;}
case 24:
#line 212 "gst.y"
{ yyval.node = makeKeywordList(yyvsp[-1].sval, yyvsp[0].node); ;
    break;}
case 25:
#line 214 "gst.y"
{ addNode(yyvsp[-2].node, makeKeywordList(yyvsp[-1].sval, yyvsp[0].node));
					  yyval.node = yyvsp[-2].node; ;
    break;}
case 27:
#line 223 "gst.y"
{ yyval.ival = 0; ;
    break;}
case 28:
#line 225 "gst.y"
{ yyval.ival = yyvsp[-1].ival;
					  if (strcmp(yyvsp[0].sval, ">") != 0) {
					    YYERROR;
					  }
					;
    break;}
case 29:
#line 233 "gst.y"
{ yyval.node = nil; ;
    break;}
case 30:
#line 234 "gst.y"
{ yyval.node = nil; ;
    break;}
case 31:
#line 236 "gst.y"
{ yyval.node = yyvsp[-1].node; ;
    break;}
case 32:
#line 240 "gst.y"
{ yyval.node = makeVariableList(yyvsp[0].node); ;
    break;}
case 33:
#line 241 "gst.y"
{ addNode(yyvsp[-1].node, makeVariableList(yyvsp[0].node));
					  yyval.node = yyvsp[-1].node; ;
    break;}
case 34:
#line 246 "gst.y"
{ yyval.node = nil; ;
    break;}
case 36:
#line 252 "gst.y"
{ yyval.node = makeStatementList(makeReturn(yyvsp[-1].node),
				       			nil); ;
    break;}
case 37:
#line 254 "gst.y"
{ yyval.node = makeStatementList(yyvsp[0].node, nil); ;
    break;}
case 38:
#line 257 "gst.y"
{ yyval.node = makeStatementList(yyvsp[-2].node, yyvsp[0].node); ;
    break;}
case 39:
#line 258 "gst.y"
{ yyval.node = yyvsp[0].node;
				  yyerrok;
				  errorf("Error in expression");
				  hadError = true;
				;
    break;}
case 43:
#line 272 "gst.y"
{ yyval.node = makeAssign(yyvsp[-1].node, yyvsp[0].node); ;
    break;}
case 44:
#line 276 "gst.y"
{ yyval.node = makeVariableList(yyvsp[-1].node); ;
    break;}
case 45:
#line 278 "gst.y"
{ addNode(yyvsp[-2].node, makeVariableList(yyvsp[-1].node));
					  yyval.node = yyvsp[-2].node; ;
    break;}
case 52:
#line 292 "gst.y"
{ yyval.node = yyvsp[-1].node; ;
    break;}
case 58:
#line 304 "gst.y"
{ yyval.node = makeIntConstant(yyvsp[0].ival); ;
    break;}
case 59:
#line 305 "gst.y"
{ yyval.node = makeFloatConstant(yyvsp[0].fval); ;
    break;}
case 60:
#line 309 "gst.y"
{ yyval.node = makeSymbolConstant(yyvsp[0].node); ;
    break;}
case 61:
#line 310 "gst.y"
{ yyval.node = makeSymbolConstant(internIdent(yyvsp[0].sval)); ;
    break;}
case 62:
#line 314 "gst.y"
{ yyval.node = internIdent(yyvsp[0].sval); ;
    break;}
case 63:
#line 315 "gst.y"
{ yyval.node = internBinOP(yyvsp[0].sval); ;
    break;}
case 64:
#line 316 "gst.y"
{ yyval.node = internIdent(yyvsp[0].sval); ;
    break;}
case 65:
#line 317 "gst.y"
{ yyval.node = internIdent(yyvsp[0].sval); ;
    break;}
case 66:
#line 322 "gst.y"
{ yyval.node = makeCharConstant(yyvsp[0].cval); ;
    break;}
case 67:
#line 326 "gst.y"
{ yyval.node = makeStringConstant(yyvsp[0].sval); ;
    break;}
case 68:
#line 330 "gst.y"
{ yyval.node = makeArrayConstant(yyvsp[0].node); ;
    break;}
case 69:
#line 334 "gst.y"
{ yyval.node = nil; ;
    break;}
case 70:
#line 336 "gst.y"
{ yyval.node = yyvsp[-1].node; ;
    break;}
case 71:
#line 340 "gst.y"
{ yyval.node = makeArrayElt(yyvsp[0].node); ;
    break;}
case 72:
#line 342 "gst.y"
{ addNode(yyvsp[-1].node, makeArrayElt(yyvsp[0].node));
					  yyval.node = yyvsp[-1].node; ;
    break;}
case 78:
#line 356 "gst.y"
{ yyval.node = makeBlock(yyvsp[-3].node, yyvsp[-2].node, yyvsp[-1].node); ;
    break;}
case 79:
#line 360 "gst.y"
{ yyval.node = nil; ;
    break;}
case 81:
#line 368 "gst.y"
{ yyval.node = makeVariableList(yyvsp[0].node); ;
    break;}
case 82:
#line 370 "gst.y"
{ addNode(yyvsp[-2].node, makeVariableList(yyvsp[0].node));
					  yyval.node = yyvsp[-2].node; ;
    break;}
case 86:
#line 381 "gst.y"
{ yyval.node = makeUnaryExpr(yyvsp[-1].node, yyvsp[0].sval); ;
    break;}
case 89:
#line 391 "gst.y"
{ yyval.node = makeBinaryExpr(yyvsp[-2].node, yyvsp[-1].sval, yyvsp[0].node); ;
    break;}
case 92:
#line 401 "gst.y"
{ yyval.node = makeKeywordExpr(yyvsp[-1].node, yyvsp[0].node); ;
    break;}
case 93:
#line 406 "gst.y"
{ yyval.node = makeKeywordList(yyvsp[-1].sval, yyvsp[0].node); ;
    break;}
case 94:
#line 408 "gst.y"
{ addNode(yyvsp[-2].node, makeKeywordList(yyvsp[-1].sval, yyvsp[0].node));
					  yyval.node = yyvsp[-2].node; ;
    break;}
case 95:
#line 414 "gst.y"
{ yyval.node = makeCascadedMessage(yyvsp[-1].node, yyvsp[0].node); ;
    break;}
case 96:
#line 418 "gst.y"
{ yyval.node = makeMessageList(yyvsp[0].node); ;
    break;}
case 97:
#line 420 "gst.y"
{ addNode(yyvsp[-2].node, makeMessageList(yyvsp[0].node));
					  yyval.node = yyvsp[-2].node; ;
    break;}
case 98:
#line 425 "gst.y"
{ yyval.node = makeUnaryExpr(nil, yyvsp[0].sval); ;
    break;}
case 99:
#line 427 "gst.y"
{ yyval.node = makeBinaryExpr(nil, yyvsp[-1].sval, yyvsp[0].node); ;
    break;}
case 100:
#line 429 "gst.y"
{ yyval.node = makeKeywordExpr(nil, yyvsp[0].node); ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 498 "/cygnus/b19/share/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;
}
#line 433 "gst.y"

/*     
 * ADDITIONAL C CODE
 */

