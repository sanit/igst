/***********************************************************************
 *
 *	No Dynamic Linking stub code
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


#include <stdio.h>
#include "dld_gst.h"

char	*gst_dld_exts[] = { NULL };

voidPtr
gst_dld_open (path)
     char *path; 
{
  return (NULL);
}

voidPtr
gst_dld_sym (handle, symbol)
     voidPtr handle;
     char *symbol;
{
  return (NULL);
}

char *
gst_dld_error ()
{
  return "Dynamic linking not supported";
}


int
gst_dld_init (dldArgv0)
     char *dldArgv0;
{
  return 0;
}
