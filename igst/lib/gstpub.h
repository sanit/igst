/***********************************************************************
 *
 *	Public definitions for extensions to Smalltalk.
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


#ifndef __GSTPUB__
#define __GSTPUB__

#ifdef __cplusplus
extern "C" {
#endif

#define __GST_FROM_GSTPUB_H__
#include "gst.h"
#undef __GST_FROM_GSTPUB_H__

#ifndef HAVE_STDARG_H
# define __GST_PROTO(args) ()
# define __DOTS
#else					/* must be ANSI */
# define __GST_PROTO(args) args
# define __DOTS		, ...
#endif

extern OOP	msgSend __GST_PROTO ((OOP receiver, OOP selector __DOTS));
extern OOP      vmsgSend __GST_PROTO ((OOP receiver, OOP selector, OOP *args));
extern OOP      nvmsgSend __GST_PROTO ((OOP receiver, OOP selector, OOP *args, int nargs));
extern OOP	strMsgSend __GST_PROTO ((OOP receiver __DOTS));

extern void	msgSendf __GST_PROTO ((voidPtr resultPtr __DOTS));

/* Actually funcAddr is a function pointer, but we don't know
   the returned type so we must declare it as voidPtr */
extern void	defineCFunc __GST_PROTO(( char *funcName, voidPtr funcAddr));
extern OOP      registerOOP __GST_PROTO((OOP oop));
extern OOP	evalExpr __GST_PROTO((char *str));
extern void	unregisterOOP __GST_PROTO((OOP oop));
extern void	evalCode __GST_PROTO((char *str));
extern void	asyncSignal __GST_PROTO((OOP semaphoreOOP));
extern void	initSmalltalk __GST_PROTO((void));
extern void	smalltalkArgs __GST_PROTO((int argc, char **argv));
extern void	topLevelLoop __GST_PROTO((void));

/* Convert C datatypes to Smalltalk types */

extern OOP	intToOOP __GST_PROTO((long i));
extern OOP	floatToOOP __GST_PROTO((double	f));
extern OOP	boolToOOP __GST_PROTO((int b));
extern OOP	charToOOP __GST_PROTO((char c));
extern OOP	classNameToOOP __GST_PROTO((char *name));
extern OOP	stringToOOP __GST_PROTO((char *str));
extern OOP	symbolToOOP __GST_PROTO((char *str));
extern OOP	cObjectToOOP __GST_PROTO((voidPtr co));
extern OOP	typeNameToOOP __GST_PROTO((char *name));

/* Convert Smalltalk datatypes to C data types */

extern long	OOPToC __GST_PROTO((OOP oop));	/* sometimes answers a voidPtr */
extern long	OOPToInt __GST_PROTO((OOP oop)); 
extern double	OOPToFloat __GST_PROTO((OOP oop));
extern int 	OOPToBool __GST_PROTO((OOP oop));
extern char	OOPToChar __GST_PROTO((OOP oop));
extern char	*OOPToString __GST_PROTO((OOP oop));
extern voidPtr	OOPToCObject __GST_PROTO((OOP oop));

#define indexedWord(obj, n)   ( ((long *) ((obj) + 1))		    [(n)-1] )
#define indexedByte(obj, n)   ( ((char *) ((obj) + 1))		    [(n)-1] )
#define indexedOOP(obj, n)    ( ((OOP  *) ((obj) + 1))		    [(n)-1] )
#define arrayOOPAt(obj, n)    ( ((OOP  *) ((mst_Object) obj)->data) [(n)-1] )
#define stringOOPAt(obj, n)   ( ((char *) ((mst_Object) obj)->data) [(n)-1] )

#ifdef __cplusplus
extern "C" }
#endif

#undef __GST_PROTO
#undef __DOTS

#endif /* __GSTPUB__ */
