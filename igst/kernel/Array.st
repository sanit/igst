"======================================================================
|
|   Array Method Definitions
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


ArrayedCollection variableSubclass: #Array
		  instanceVariableNames: ''
		  classVariableNames: ''
		  poolDictionaries: ''
		  category: 'Collections-Sequenceable'
!

Array comment: 
'My instances are objects that have array-like properties: they are directly 
indexable by integers starting at 1, and they are fixed in size.  I inherit
object creation behavior messages such as #with:, as well as iteration
and general access behavior from SequenceableCollection.' !


!Array methodsFor: 'printing'!

printOn: aStream
    "Print a representation for the receiver on aStream"
    aStream nextPut: $(.
    self do:
	[ :elt | elt printOn: aStream.
		 aStream space ].
    aStream nextPut: $)
! !


!Array methodsFor: 'mutating objects'!

multiBecome: anArray
    "Transform every object in the receiver in each corresponding
     object in anArray. anArray and the receiver must have the same
     size"

    | index |
    index := 0.
    ^self collect: [:object |
        index := index + 1.
        object become: (anArray at: index)
    ]
!!



!Array methodsFor: 'testing'!

isArray
    ^false
!!