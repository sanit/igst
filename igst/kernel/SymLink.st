"======================================================================
|
|   SymLink Method Definitions
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


Link subclass: #SymLink
     instanceVariableNames: 'symbol'
     classVariableNames: ''
     poolDictionaries: ''
     category: 'Language-Implementation'
!

SymLink comment: 
'I am used to implement the Smalltalk symbol table.  My instances are
links that contain symbols, and the symbol table basically a hash table that
points to chains of my instances.' !



!SymLink methodsFor: 'iteration'!

do: aBlock
    "Evaluate aBlock for each symbol in the list"
    aBlock value: symbol.
    nextLink notNil ifTrue: [ nextLink do: aBlock ]
!!



!SymLink methodsFor: 'printing'!

printOn: aStream
    "Print a representation of the receiver on aStream."
    aStream nextPut: $[.
    self do: 
	[ :elt | elt printOn: aStream.
		 aStream nextPut: Character space ].
    aStream nextPut: $].
!!
