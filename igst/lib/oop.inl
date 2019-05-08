/* this is -*- C -*- ok?!? */

/***********************************************************************
 *
 *	Object table module Inlines.
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


#ifndef __GSTOOP_INLINES__
#define  __GSTOOP_INLINES__

static inline OOP allocOOP();

/* Enabling this means that some accessors for the OOP table are
   implemented as routines, instead of being in-line code via macros */
/* #define ACCESSOR_DEBUGGING */

#if defined(OPTIMIZE)
/* Turn this off when we're optimizing. */
#undef ACCESSOR_DEBUGGING
#endif

#define maybeMarkOOP(oop) \
  if (isOOP(oop) && !isOOPFree(oop) && !isOOPMarked(oop)) { \
    markAnOOPInternal(oop, nil, nil); \
  }

#define markOOPRange(startOOP, endOOP) \
  if ((startOOP) < (endOOP)) { \
    markAnOOPInternal(nil, startOOP, endOOP); \
  }


  /* In the non-incremental GC world, being FREE is all that matters for
   * validity.
   */
#define oopValidMac(oop) \
  (!(oop->flags & F_FREE))

#define oopAtMac(index) \
  ( &oopTable[index] )

#define oopAvailableMac(index) \
  ( oopTable[index].flags & F_FREE )

#define oopIndexMac(oop) \
  ( (OOP)(oop) - oopTable )

#ifndef ACCESSOR_DEBUGGING

#define oopAt		oopAtMac
#define oopAvailable	oopAvailableMac
#define oopValid	oopValidMac
#define oopIndex	oopIndexMac

#else

extern OOP		oopAt();
extern mst_Boolean	oopAvailable(), oopValid();
extern long		oopIndex();

#endif /* ACCESSOR_DEBUGGING */

#include "gst.h"
#include "lex.h"
#include "dict.h"

#define setOOPObject(oop, obj) {				\
  (oop)->object = (mst_Object) (obj);				\
}

#define freeOOP(oop) \
{ \
  extern OOP firstFreeOOP; \
  register OOP __theOOP = (oop); \
  __theOOP->flags   = F_FREE; \
  __theOOP->object  = (mst_Object) firstFreeOOP; \
  firstFreeOOP = __theOOP; \
  numFreeOOPs++; \
}

#define isOOPAddr(addr) \
  ((OOP)(addr) >= oopTable && (OOP)(addr) < &oopTable[TOTAL_OOP_TABLE_SLOTS] \
    && (((long)addr & (SIZEOF_LONG - 1)) == 0))

#define isObjAddr(addr) \
  ((char *)(addr) >= memSpace.space && (char *)(addr) < memSpace.maxPtr \
    && (((long)(addr) & (SIZEOF_LONG - 1)) == 0))



/*
 *	OOP allocOOP(obj)
 *
 * Description
 *
 *	Given an object OBJ, this routine allocates an OOP table slot for it
 *	and returns it.  It marks the OOP so that it indicates the object is in
 *	new space, and that the oop has been referenced on this pass (to keep
 *	the OOP table reaper from reclaiming this OOP).
 *
 * Inputs
 *
 *	obj   : Object that the new OOP should point to.
 *
 * Outputs
 *
 *	An OOP, which is the address of an element in the OOP table.
 */
static inline OOP allocOOP(obj)
     mst_Object	obj;
{
  register OOP		oop;
  extern   OOP 		firstFreeOOP;

  if(numFreeOOPs == 0) {
    gcFlip();
    if(numFreeOOPs == 0) {
      errorf("Ran out of OOP Table slots!!!");
      exit(1);
    }
  }

  oop = firstFreeOOP;
  firstFreeOOP = (OOP)oop->object;
  if (oop > lastUsedOOP) {
    lastUsedOOP = oop;
  }

#ifndef OPTIMIZE
  if (!(oop->flags & F_FREE)) {
    errorf("Non-free OOP in OOP free list!!!");
    exit(1);
  }
#endif

  numFreeOOPs--;
  oop->object = obj;
  oop->flags = 0;				/* no flags on this one */
  return (oop);
}

#endif
