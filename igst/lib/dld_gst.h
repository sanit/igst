/***********************************************************************
 *
 *	Dynamic Linking for GNU Smalltalk
 *
 *	$Revision: 1.6.2$
 *	$Date: 1999/08/31 11:23:18$
 *	$Author: pb$
 *
 ***********************************************************************/

/***********************************************************************
 *
 * Copyright 1990, 91, 92, 94, 95, 99 Free Software Foundation, Inc.
 * Written by Paolo Bonzini.
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

#ifndef __GSTDLD__
#define __GSTDLD__

#include "gstpub.h"

/* Given a library name (char *) return an handle to the library. */
extern		void	*gst_dld_open();

/* Given a library handle (voidPtr) and function name (char *) return NULL if
   an error occurred, else a pointer to the function. */
extern		voidPtr	gst_dld_sym();

/* Describe the last error occurred. */
extern		char	*gst_dld_error();

/* Initialize the library, return 0 if successful.  The only parameter received
   is argv[0] as passed to the program. */
extern		int	gst_dld_init();

/* Extensions to be tried if the filename "as is" cannot be linked. */
extern		char	*gst_dld_exts[];

/* Define the C call-out functions needed by Smalltalk to access the DLD
   functions above. */
extern		void	initDldLib();

#endif
