"======================================================================
|
|   WriteStream Method Definitions
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
| This file is part of GNU-Smalltalk.
|
| GNU-Smalltalk is free software; you can redistribute it and/or modify it
| under the terms of the GNU General Public License as published by the Free
| Software Foundation; either version 2, or (at your option) any later version.
| 
| GNU-Smalltalk is distributed in the hope that it will be useful, but WITHOUT
| ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
| FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
| details.
| 
| You should have received a copy of the GNU General Public License along with
| GNU-Smalltalk; see the file COPYING.  If not, write to the Free Software
| Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
|
 ======================================================================"


PositionableStream subclass: #WriteStream
		   instanceVariableNames: ''
		   classVariableNames: ''
		   poolDictionaries: ''
		   category: 'Streams-Collection'
!

WriteStream comment: 
'I am the class of writeable streams.  I only allow write operations to
my instances; reading is strictly forbidden.' !


!WriteStream class methodsFor: 'instance creation'!

on: aCollection
    "Answer a new instance of the receiver which streams on aCollection.
     Every item of aCollection is discarded."
    ^(self new initCollection: aCollection)
        access: 2; 
        truncate;
        yourself
!

with: aCollection
    "Answer a new instance of the receiver which streams from the end
     of aCollection."
    ^(self new initCollection: aCollection)
        access: 2; 
        moveToEnd;
        yourself
!

with: aCollection from: firstIndex to: lastIndex
    "Answer a new instance of the receiver which streams from the
     firstIndex-th item of aCollection to the lastIndex-th. The
     pointer is moved to the last item in that range."
    | stream |
    stream := self on: aCollection from: firstIndex to: lastIndex.
    stream moveToEnd.
    ^stream
! !


!WriteStream methodsFor: 'accessing'!

size
    "Answer how many objects have been written"
    ^ptr - 1
! !



!WriteStream methodsFor: 'accessing-writing'!

nextPut: anObject
    "Store anObject as the next item in the receiver. Grow the
     collection if necessary"

    "(access bitAnd: 2) = 0
       ifTrue: [ ^self shouldNotImplement ]."
    ptr > collection size ifTrue: [ self growCollection ].
    collection at: ptr put: anObject.
    ptr > endPtr ifTrue: [ endPtr := ptr ].
    ptr := ptr + 1.
    ^anObject
! !



!WriteStream methodsFor: 'positioning'!

position: anInteger
    "Skip to the anInteger-th item backwards in the stream. Fail if
     anInteger>self position. The stream is truncated after the pointer"
    anInteger > ptr ifTrue: [
        ^self error: 'cannot move forwards in a write stream'
    ].
    super position: anInteger.
    self truncate.
!

skip: anInteger
    "Skip (anInteger negated) items backwards in the stream. Fail if
     anInteger>0. The stream is truncated after the pointer"
    anInteger > 0 ifTrue: [
        ^self error: 'cannot skip forwards in a write stream'
    ].
    super skip: anInteger.
!

emptyStream
    "Extension - Reset the stream"
    ptr := 1.
    endPtr := 0
! !


!WriteStream methodsFor: 'private methods'!

initCollection: aCollection
    collection := aCollection.
    ptr := 1.
    endPtr := 0.
!

moveToEnd
    endPtr := collection size.
    self setToEnd
!

growCollection
   "Private - Double the size of the collection or, if its size < 8,
    grow it to 8 places"
   | size |
   size := collection size.
   collection growBy: (size max: 8 - size).
!!

