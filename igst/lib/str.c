/***********************************************************************
 *
 *	String Functions
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



#include <stdio.h>
#include "gst.h"
#include "alloc.h"
#include "str.h"
#if STDC_HEADERS
#include <stdlib.h>
#include <string.h>
#endif /* STDC_HEADERS */

#define	STRING_GRANULARITY	128

static void	reallocStrBase();

static char	*strBase = nil, *strPtr;
static long	maxStrLen;


/*
 *	void initStrBuf()
 *
 * Description
 *
 *	Initializes the string buffer to accumulate new characters.
 *
 */
void
initStrBuf()
{
  if (strBase == NULL) {
    reallocStrBase(0L, STRING_GRANULARITY);
  }
  
  strPtr = strBase;
}

/*
 *	char *curStrBuf()
 *
 * Description
 *
 *	Returns the currently accumulated string, as a C string.  Note that the
 *	actual string returned is not a unique string, and should be copied as
 *	soon as possible.
 *
 * Outputs
 *
 *	Pointer to NUL terminated C string that is the accumulated string.
 */
char *
curStrBuf()
{
  addStrBufChar('\0');
  return (strBase);
}

/*
 *	void addStrBufChar(c)
 *
 * Description
 *
 *	Adds a character "c" to the string being accumulated.  The character can
 *	be any valid ASCII character.
 *
 * Inputs
 *
 *	c     : the character to add to the string.
 *
 */
void
addStrBufChar(c)
     char	c;
{
  if (strPtr - strBase  >= maxStrLen) {
    reallocStrBase(maxStrLen, STRING_GRANULARITY);
  }

  *strPtr++ = c;
}


/*
 *	static void reallocStrBase(len, delta)
 *
 * Description
 *
 *	Called to allocate a new string to be accumulated.  If there is an
 *	existing string, it is copied into the new string and then free;
 *	otherwise the new string is created.
 *
 * Inputs
 *
 *	len   : current length of current string, in bytes
 *	delta : increment to add to string length in bytes.  New string length
 *		is len+delta.
 *
 * Outputs
 *
 *	strBase, maxStrLen, and strPtr are all globals that are affected by
 *	this routine.
 */
static void
reallocStrBase(len, delta)
     long	len;
     int	delta;
{
  char		*newStr;
  long		l;

  maxStrLen = len + delta;

  if (strBase) {
    l = strPtr - strBase;
    newStr = (char *)xrealloc(strBase, maxStrLen);
  } else {
    l = 0L;
    newStr = (char *)xmalloc(maxStrLen);
  }

  strBase = newStr;
  strPtr = strBase + l;
}

void
resizeString(str)
     char **str;
{
  *str = realloc(*str, strlen(*str) + 1);
}
