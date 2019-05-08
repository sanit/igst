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


#include "gst.h"
#include "dld_gst.h"
#include "alloc.h"
#include <stdio.h>

#if STDC_HEADERS
#  include <string.h>
#endif /* STDC_HEADERS */

static void
testDLD ()
{
  void (*func)(), *handle;
  printf("linking a file %p\n", handle = gst_dld_open("xxx.o"));
  
  func = (void (*)()) gst_dld_sym(handle, "foopartner");
  printf("func is %p\n", func);
  (*func)();
}

static voidPtr
gst_dld_openext (filename)
     char *filename;
{
  voidPtr handle;
  char	*completed, *ext, **i;
  int	len;

  if (handle = gst_dld_open(filename)) {
    /* the file name as given worked */
    return (handle);
  }

  if (!filename || !filename[0]) {
    /* no hope to succeed */
    return (handle);
  }

  len = strlen(filename);
  completed = xmalloc(len + 20);
  strcpy(completed, filename);
  for (i = gst_dld_exts; i; i++) {
    int n;
    char *src;
    for (src = *i, n = 0, ext = completed + len; *src && (n < 20); n++) {
      *ext++ = *src++;
    }
    *ext = '\0';

    if (handle = gst_dld_open(completed)) {
      break;
    }
  }
  xfree(completed);
  return (handle);
}


void
initDldLib ()
{
  defineCFunc("defineCFunc", defineCFunc);
  defineCFunc("dldLink", gst_dld_openext);
  defineCFunc("dldGetFunc", gst_dld_sym);
  defineCFunc("dldError", gst_dld_error);
  defineCFunc("testDLD", testDLD);
}
