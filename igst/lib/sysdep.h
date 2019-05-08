/***********************************************************************
 *
 *	System specific module declarations
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


#ifndef __GSTSYSDEP__
#define __GSTSYSDEP__


#include <stdio.h>
#include <signal.h>

#ifndef INTERRUPT_STATE_TYPE
typedef int			IntState;
#else
typedef INTERRUPT_STATE_TYPE	IntState;
#endif


/* These nice tests are simple, guaranteed and independent of byte order. */
#define isFinite(dbl)		(((dbl) == (dbl)) && ((dbl) != ((dbl) * 1.5 + 1.0)))
#define isNaN(dbl)		 ((dbl) != (dbl))

extern IntState		disableInterrupts();
extern void		enableInterrupts(), signalAfter(), setSignalHandler(),
			initSysdep();
extern unsigned long	getMilliTime();
extern long		getTime(), getFileModifyTime(),
			adjustTimeZone(), currentTimeZoneBias();
extern char		*getFullFileName(), *getCurDirName();
extern mst_Boolean	fileIsReadable();
extern FILE		*openPipe(), *openFile();
extern int		closePipe();
extern OOP		getOpenFileSize();

#ifdef HAVE_STDARG_H
extern void		debugf(const char *, ...);
#else
extern void		debugf();
#endif

#endif /* __GSTSYSDEP__ */
