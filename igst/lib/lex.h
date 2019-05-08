/***********************************************************************
 *
 *	External definitions for the Lexer module.
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


#ifndef __GSTLEX__
#define __GSTLEX__

typedef enum {
  unknownStreamType,
  fileStreamType,
  stringStreamType
#ifdef HAVE_READLINE
  , readlineStreamType
#endif /* HAVE_READLINE */
} StreamType;


extern mst_Boolean		reportErrors, compileCode;
extern char			*firstErrorStr, *firstErrorFile;
extern long			firstErrorLine;
extern char			*changeFileName;

#ifdef HAVE_READLINE
extern OOP			getCurReadline();
extern void			pushReadlineString(), initializeReadline();
#endif /* HAVE_READLINE */

extern OOP			getCurFileName(), getCurString();

extern void			pushUNIXFile(), pushSmalltalkString(), 
				popStream(), clearMethodStartPos(),
  				setStreamInfo(), initChangesStream(),
				resetChangesFile(), pushCString(), /* initLexer(), */
				errorf(), yyerror(), parseStream();

extern int			getCurFilePos(), getMethodStartPos(), yylex();

extern StreamType		getCurStreamType();

extern char			*getMethodSourceFromCurFile();

extern mst_Boolean 		isKernelFile();

#endif /* __GSTLEX__ */
