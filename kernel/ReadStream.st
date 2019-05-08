"======================================================================
|
|   ReadStream Method Definitions
|
|   $Revision: 1.6.2$
|   $Date: 1999/08/31 11:23:18$
|   $Author: pb$
|
 ======================================================================"


"======================================================================
|
| Copyright 1990, 91, 92, 94, 95, 99 Free Software Foundation, Inc.
| Written by Steve Byrne.
|
| This file is part of GNU Smalltalk.
|
| GNU Smalltalk is free software; you can redistribute it and/or modify it
| under the terms of the GNU General Public License as published by the Free
| Software Foundation; either version 2, or (at your option) any later version.
| 
| GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
| ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
| FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
| details.
| 
| You should have received a copy of the GNU General Public License along with
| GNU Smalltalk; see the file COPYING.  If not, write to the Free Software
| Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
|
 ======================================================================"


PositionableStream subclass: #ReadStream
		   instanceVariableNames: ''
		   classVariableNames: ''
		   poolDictionaries: ''
		   category: 'Streams-Collections'
!

ReadStream comment: 
'I implement the set of read-only stream objects.  You may read from
my objects, but you may not write to them.' !


!ReadStream class methodsFor: 'instance creation'!

on: aCollection
    "Answer a new stream working on aCollection from its start."
    ^(self new initCollection: aCollection) access: 1
!!



!ReadStream methodsFor: 'accessing-reading'!

size
    "Answer the receiver's size."
    ^collection size
!

reverseContents
    "May be faster than generic stream reverseContents."
    | aCollection i numElts |
    numElts := collection size.
    aCollection := collection copyEmpty: numElts.
    i := 0.
    [ i := i + 1. 
      i <= endPtr ] whileTrue:
    	[ aCollection at: numElts - i + 1
	    	      put: (collection at: i) ].
    ^aCollection
!!



!ReadStream methodsFor: 'private methods'!

initCollection: aCollection
    collection := aCollection.
    ptr := 1.
    endPtr := collection size

!!
