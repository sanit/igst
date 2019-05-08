/***********************************************************************
 *
 *	xmalloc, xrealloc and xfree for Smalltalk
 *
 *	$Revision: 1.6.2$
 *	$Date: 1999/08/31 11:23:18$
 *	$Author: pb$
 *
 ***********************************************************************/

/***********************************************************************
 *
 * Based on code from Tcl 8.0 which is
 * Copyright (c) 1983 Regents of the University of California.
 * Copyright (c) 1996-1997 Sun Microsystems, Inc.
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
 * The file license.terms, as found in Tcl 8.0, is below.
 * NOTE: THE FOLLOWING APPLIES ONLY TO PORTIONS DERIVED FROM TCL 8.0.
 * THE LICENSE FOR THIS FILE IS GPL, AS STATED ABOVE AND ALLOWED BELOW.
 * 
 * -- BEGIN license.terms --
 * This software is copyrighted by the Regents of the University of
 * California, Sun Microsystems, Inc., and other parties.  The following
 * terms apply to all files associated with the software unless explicitly
 * disclaimed in individual files.
 * 
 * The authors hereby grant permission to use, copy, modify, distribute,
 * and license this software and its documentation for any purpose, provided
 * that existing copyright notices are retained in all copies and that this
 * notice is included verbatim in any distributions. No written agreement,
 * license, or royalty fee is required for any of the authorized uses.
 * Modifications to this software may be copyrighted by their authors
 * and need not follow the licensing terms described here, provided that
 * the new terms are clearly indicated on the first page of each file where
 * they apply.
 * 
 * IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
 * ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
 * DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
 * IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
 * NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 * MODIFICATIONS.
 * 
 * GOVERNMENT USE: If you are acquiring this software on behalf of the
 * U.S. government, the Government shall have only "Restricted Rights"
 * in the software and related documentation as defined in the Federal 
 * Acquisition Regulations (FARs) in Clause 52.227.19 (c) (2).  If you
 * are acquiring the software on behalf of the Department of Defense, the
 * software shall be classified as "Commercial Computer Software" and the
 * Government shall have only "Restricted Rights" as defined in Clause
 * 252.227-7013 (c) (1) of DFARs.  Notwithstanding the foregoing, the
 * authors grant the U.S. Government and others acting in its behalf
 * permission to use and distribute the software in accordance with the
 * terms specified in this license.
 * -- END license.terms --
 ***********************************************************************/


#include <string.h>
#include "gst.h"
#include "alloc.h"
#include "sysdep.h"
#include <sys/types.h>

extern void errorf();

/********************* PORTABILITY DEFINITIONS FOLLOW **********************/

#ifdef _WIN32
  /* By default, use Win32 APIs for low level allocation and replace libc's
     malloc/realloc/free with our own custom memory allocator.
     This is because the Win32 port of GCC does not allow one to allocate a
     single block which is bigger than the available memory, while our
     allocator makes it possible. */

  #define DEFAULT_USE_CUSTOM_ALLOCATOR	1
  #define USES_MALLOC_INTERNALLY	0

#else
  /* By default, use C library malloc for every allocation, wrapping it
     in xmalloc/xfree/xrealloc for portability. Optionally, you can use
     the custom allocator, but it will still use malloc internally. */

  #define DEFAULT_USE_CUSTOM_ALLOCATOR	0
  #define USES_MALLOC_INTERNALLY	1
#endif


/********************** END OF PORTABILITY DEFINITIONS *********************/


#ifndef USE_CUSTOM_ALLOCATOR
  /* Don't override a -D definition */
  #define USE_CUSTOM_ALLOCATOR DEFAULT_USE_CUSTOM_ALLOCATOR
#endif

#if !USE_CUSTOM_ALLOCATOR || USES_MALLOC_INTERNALLY
   /* When we use regular libc malloc, or when we are using the BSD allocator
    * and relying on malloc for low level allocation, we have to include the
    * appropriate header to use malloc. */

  #if STDC_HEADERS
    #include <stdlib.h>
  #else
    #include <malloc.h>
  #endif /* STDC_HEADERS */
#endif

#if USE_CUSTOM_ALLOCATOR
#if USES_MALLOC_INTERNALLY
  /* my_xxx are calls to the functions defined in this file.  In the program
   * they are used through xmalloc; and libc's code still uses regular malloc.
   * So they should be static. */
  static char *my_malloc(), *my_realloc();
  static void my_free();
#else
  /* my_xxx are calls to the functions defined in this file.  In the program
   * they are used through xmalloc, but libc must use these too. So they
   * should be #defined to the function names in libc and made extern. */

  #define my_malloc	malloc
  #define my_free	free
  #define my_realloc	realloc
  extern char *my_malloc(), *my_realloc(), *calloc();
  extern void my_free();
#endif

#else /* !USE_CUSTOM_ALLOCATOR */
  /* When we are not using the custom allocator, my_xxx are calls to libc. */
  #define my_malloc	malloc
  #define my_free	free
  #define my_realloc	realloc
#endif


/********************** END OF OTHER CONDITIONAL STUFF *********************/


char *
xmalloc (n)
     register size_t n;
{
  register char *block;

  block = (char*)my_malloc(n);
  if (block) {
    memset((voidPtr) block, '0', n);
  } else {
    errorf("Failed to allocate %d bytes!!!", n);
    exit(1);
  }

  return (block);
}

char *
xrealloc (p, n)
     register char *p;
     register size_t n;
{
  register char *block;

  /* We must call malloc explicitly when BLOCK is 0, since some
     reallocs don't do this.  */
  if (!p) {
    block = (char*) my_malloc (n);
    if (!block) {
      errorf("Failed to allocate %d bytes!!!", n);
      exit(1);
    }
  } else {
    block = (char*) my_realloc(p, n);
  }

  /* !!! check to see if realloc frees existing memory if it cannot
   *  allocate more -- hmmm, ANSI says it shouldn't */ 
  return (block);
}

void
xfree (p)
     register char *p;
{
  my_free(p);
}


#if USE_CUSTOM_ALLOCATOR

/* 
 *	xmalloc/xfree/xrealloc use this storage allocator, which is very fast.
 *	It allocates blocks of a small number of different sizes, and keeps
 *	free lists of each size.  Blocks that don't exactly fit are passed
 *	up to the next larger size.  Blocks over a certain size are directly
 *	allocated from the system.
 *
 * Copyright (c) 1983 Regents of the University of California.
 * Copyright (c) 1996-1997 Sun Microsystems, Inc.
 *
 * Portions contributed by Chris Kingsley, Jack Jansen and Ray Johnson.
 *
 */

 
#ifdef _WIN32
  #include <windows.h>

  /* Note that the Win32 port of GCC does not allow one to allocate a single
     block which is bigger than the available memory. Instead, this is possible
     if we directly use GlobalXXX functions. */

  #define sysMalloc(size)	  			\
	GlobalLock(GlobalAlloc(GMEM_MOVEABLE, (DWORD)size))

  #define sysFree(ptr) { 				\
	GlobalUnlock(GlobalHandle(ptr)); 		\
	GlobalFree(GlobalHandle(ptr)); 			\
  }

  static char inline *sysRealloc(ptr, size) 
  char *ptr;
  size_t size;
  {
    GlobalUnlock(GlobalHandle(ptr));
    return(GlobalLock(GlobalReAlloc(GlobalHandle(ptr), (DWORD)size, 0)));
  }


#else /* not _WIN32 */

  /* my_malloc/my_free/my_realloc are still going to use libc for low-level
   * allocation */
  #define sysMalloc		malloc
  #define sysFree		free
  #define sysRealloc		realloc
#endif


/*
 * The overhead on a block is at least 4 bytes.  When free, this space
 * contains a pointer to the next free block, and the bottom two bits must
 * be zero.  When in use, the first byte is set to MAGIC, and the second
 * byte is the size index.  The remaining bytes are for alignment.
 * If range checking is enabled then a second word holds the size of the
 * requested block, less 1, rounded up to a multiple of sizeof(RMAGIC).
 * The order of elements is critical: ov_magic must overlay the low order
 * bits of ov_next, and ov_magic can not be a valid ov_next bit pattern.
 */

typedef union OverheadUnion {
    union OverheadUnion	*ov_next;	/* when free */
    struct {
	unsigned char	ovu_magic0;	/* magic number */
	unsigned char	ovu_index;	/* bucket # */
	unsigned char	ovu_unused;	/* unused */
	unsigned char	ovu_magic1;	/* other magic number */
#ifdef DEBUG_MEMORY
	unsigned short	ovu_rmagic;	/* range magic number */
	unsigned long	ovu_size;	/* actual block size */
#endif
    } ovu;
} Overhead;

#define ov_magic0	ovu.ovu_magic0
#define ov_magic1	ovu.ovu_magic1
#define ov_index	ovu.ovu_index
#define ov_rmagic	ovu.ovu_rmagic
#define ov_size		ovu.ovu_size

#define MAGIC		0xef		/* magic # on accounting info */
#define RMAGIC		0x5555		/* magic # on range info */

#ifdef DEBUG_MEMORY
#define	RSLOP		sizeof (unsigned short)
#else
#define	RSLOP		0
#endif

#define OVERHEAD 	(sizeof(Overhead) + RSLOP)

/*
 * nextf[i] is the pointer to the next free block of size 2^(i+3).  The
 * smallest allocatable block is 8 bytes.  The Overhead information
 * precedes the data area returned to the user.
 */

#define NBUCKETS	13
#define MAXMALLOC	(1<<(NBUCKETS+2))
static	Overhead	*nextf[NBUCKETS];

#ifdef DEBUG_MEMORY

#include <stdio.h>
#define	ASSERT(p)							     \
  if (!(p)) {								     \
    fprintf(stderr, "%s:%s: Assertion failed %s", __FILE__,  __LINE__, # p); \
    exit(1);								     \
  }

#else
#define	ASSERT(p)
#endif

/*
 *----------------------------------------------------------------------
 *
 * MoreCore --
 *
 *	Allocate more memory to the indicated bucket.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Attempts to get more memory from the system.
 *
 *----------------------------------------------------------------------
 */

static void MoreCore();

void
MoreCore (bucket)
     int bucket;		/* What bucket to allocat to. */
{
    register Overhead *op;
    register long sz;		/* size of desired block */
    long amt;			/* amount to allocate */
    int nblks;			/* how many blocks we get */

    /*
     * sbrk_size <= 0 only for big, FLUFFY, requests (about
     * 2^30 bytes, I think) or for a negative arg.
     */
    sz = 1 << (bucket + 3);
    ASSERT(sz > 0);

    amt = MAXMALLOC;
    nblks = amt / sz;
    ASSERT(nblks*sz == amt);

    op = (Overhead *)sysMalloc(amt);
    /* no more room! */
    if (op == NULL) {
	return;
    }
    
    /*
     * Add new memory allocated to that on
     * free list for this hash bucket.
     */
    nextf[bucket] = op;
    while (--nblks > 0) {
	op->ov_next = (Overhead *)((caddr_t)op + sz);
	op = (Overhead *)((caddr_t)op + sz);
    }
    op->ov_next = (Overhead *)NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * my_malloc --
 *
 *	Allocate more memory.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

char *
my_malloc (nbytes)
     size_t nbytes;
{
    register Overhead *op;
    register long bucket;
    register unsigned amt;

    /*
     * First the simple case: we simple allocate big blocks directly
     */
    if (nbytes + OVERHEAD >= MAXMALLOC) {
	op = (Overhead *)sysMalloc(nbytes+OVERHEAD);
	if (op == NULL) {
	    return NULL;
	}
	op->ov_magic0 = op->ov_magic1 = MAGIC;
	op->ov_index = 0xff;
#ifdef DEBUG_MEMORY
	/*
	 * Record allocated size of block and
	 * bound space with magic numbers.
	 */
	op->ov_size = (nbytes + RSLOP - 1) & ~(RSLOP - 1);
	op->ov_rmagic = RMAGIC;
	*(unsigned short *)((caddr_t)(op + 1) + op->ov_size) = RMAGIC;
#endif
	return (voidPtr)(op+1);
    }
    /*
     * Convert amount of memory requested into closest block size
     * stored in hash buckets which satisfies request.
     * Account for space used per block for accounting.
     */
#ifndef DEBUG_MEMORY
    amt = 8;	/* size of first bucket */
    bucket = 0;
#else
    amt = 16;	/* size of first bucket */
    bucket = 1;
#endif
    while (nbytes + OVERHEAD > amt) {
	amt <<= 1;
	if (amt == 0) {
	    return (NULL);
	}
	bucket++;
    }
    ASSERT( bucket < NBUCKETS );

    /*
     * If nothing in hash bucket right now,
     * request more memory from the system.
     */
    if ((op = nextf[bucket]) == NULL) {
	MoreCore(bucket);
	if ((op = nextf[bucket]) == NULL) {
	    return (NULL);
	}
    }
    /*
     * Remove from linked list
     */
    nextf[bucket] = op->ov_next;
    op->ov_magic0 = op->ov_magic1 = MAGIC;
    op->ov_index = (unsigned char) bucket;
#ifdef DEBUG_MEMORY
    /*
     * Record allocated size of block and
     * bound space with magic numbers.
     */
    op->ov_size = (nbytes + RSLOP - 1) & ~(RSLOP - 1);
    op->ov_rmagic = RMAGIC;
    *(unsigned short *)((caddr_t)(op + 1) + op->ov_size) = RMAGIC;
#endif
    return ((char *)(op + 1));
}


/*
 *----------------------------------------------------------------------
 *
 * my_free --
 *
 *	Free memory.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

void
my_free (cp)
     char *cp;		/* Pointer to memory to free. */
{   
    register long size;
    register Overhead *op;

    if (cp == NULL) {
	return;
    }

    op = (Overhead *)((caddr_t)cp - sizeof (Overhead));

    ASSERT(op->ov_magic0 == MAGIC);		/* make sure it was in use */
    ASSERT(op->ov_magic1 == MAGIC);
    if (op->ov_magic0 != MAGIC || op->ov_magic1 != MAGIC) {
	return;
    }

    ASSERT(op->ov_rmagic == RMAGIC);
    ASSERT(*(unsigned short *)((caddr_t)(op + 1) + op->ov_size) == RMAGIC);
    size = op->ov_index;
    if ( size == 0xff ) {
	sysFree(op);
	return;
    }
    ASSERT(size < NBUCKETS);
    op->ov_next = nextf[size];	/* also clobbers ov_magic */
    nextf[size] = op;
}

/*
 *----------------------------------------------------------------------
 *
 * my_realloc --
 *
 *	Reallocate memory.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

char *
my_realloc (cp, nbytes)
     char *cp;			/* Pointer to alloced block. */
     size_t nbytes;			/* New size of memory. */
{   
    int i;
    Overhead *op;
    int expensive;
    unsigned long maxsize;

    if (cp == NULL) {
	return (my_malloc(nbytes));
    }

    op = (Overhead *)((caddr_t)cp - sizeof (Overhead));

    ASSERT(op->ov_magic0 == MAGIC);		/* make sure it was in use */
    ASSERT(op->ov_magic1 == MAGIC);
    if (op->ov_magic0 != MAGIC || op->ov_magic1 != MAGIC) {
	return NULL;
    }

    ASSERT(op->ov_rmagic == RMAGIC);
    ASSERT(*(unsigned short *)((caddr_t)(op + 1) + op->ov_size) == RMAGIC);
    i = op->ov_index;

    /*
     * If the block isn't in a bin, just realloc it.
     */

    if (i == 0xff) {
	op = (Overhead *) sysRealloc(op, nbytes+OVERHEAD);
	if (op == NULL) {
	    return NULL;
	}
#ifdef DEBUG_MEMORY
	/*
	 * Record allocated size of block and update magic number bounds.
	 */

	op->ov_size = (nbytes + RSLOP - 1) & ~(RSLOP - 1);
	*(unsigned short *)((caddr_t)(op + 1) + op->ov_size) = RMAGIC;
#endif
	return (char *)(op+1);
    }
    maxsize = 1 << (i+3);
    expensive = 0;
    if ( nbytes + OVERHEAD > maxsize ) {
	expensive = 1;
    } else if ( i > 0 && nbytes + OVERHEAD < (maxsize/2) ) {
	expensive = 1;
    }

    if (expensive) {
	voidPtr newp;
		
	newp = my_malloc(nbytes);
	if ( newp == NULL ) {
	    return NULL;
	}
	maxsize -= OVERHEAD;
	if ( maxsize < nbytes )
	    nbytes = maxsize;
	memcpy((voidPtr) newp, (voidPtr) cp, (size_t) nbytes);
	my_free(cp);
	return newp;
    }
    
    /*
     * Ok, we don't have to copy, it fits as-is
     */
#ifdef DEBUG_MEMORY
    op->ov_size = (nbytes + RSLOP - 1) & ~(RSLOP - 1);
    *(unsigned short *)((caddr_t)(op + 1) + op->ov_size) = RMAGIC;
#endif
    return(cp);
}


#if !USES_MALLOC_INTERNALLY
/* When we are not using malloc, allocator replaces libc's
 * so we have to define calloc too! */

char *
calloc (n, s)
     register size_t n;
     size_t s;
{
  register char *block;
  n *= s;
  block = malloc(n);
  if (block) {
    memset((voidPtr) block, '0', n);
  }
  return (block);
}

#endif


#endif /* USE_CUSTOM_ALLOCATOR */
