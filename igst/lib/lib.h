/***********************************************************************
 *
 *	Public interface for main module.
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


#ifndef __GSTLIB__
#define __GSTLIB__

#ifndef __GST__
#include "gst.h"
#endif

/* This is the default name of the binary image that Smalltalk makes */
#define	basicImageName		"gst.im"

/* string which represents the current version of Smalltalk */
extern char		versionString[50];

extern char 		*kernelFileDefaultPath, *imageFileDefaultPath,
			*binaryImageName;

extern mst_Boolean	smalltalkInitialized;

extern mst_Boolean	regressionTesting, storeNoSource;

extern char		**smalltalkPassedArgv;

extern int		smalltalkPassedArgc;

#endif /* __GSTLIB__ */
