"======================================================================
|
|   PositionableStream Method Definitions
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

Stream subclass: #PositionableStream
       instanceVariableNames: 'collection ptr endPtr access'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Streams-Collections'
!

PositionableStream comment: 
'My instances represent streams where explicit positioning is permitted.
Thus, my streams act in a manner to normal disk files: you can read
or write sequentially, but also position the file to a particular place
whenever you choose.  Generally, you''ll want to use ReadStream, WriteStream
or ReadWriteStream instead of me to create and use streams.' !



!PositionableStream class methodsFor: 'instance creation'!

on: aCollection
    "Answer an instance of the receiver streaming on the
    whole contents of aCollection"
    self subclassResponsiblity
!

on: aCollection from: firstIndex to: lastIndex
    "Answer an instance of the receiver streaming from
     the firstIndex-th item of aCollection to the lastIndex-th"
    ^self on: (aCollection copyFrom: firstIndex to: lastIndex)

!!



!PositionableStream methodsFor: 'accessing-reading'!

next
    "Answer the next item of the receiver"
    | element |
    (access bitAnd: 1) = 0
    	ifTrue: [ ^self shouldNotImplement ].
    self atEnd ifTrue: [ ^self error: 'end of stream reached' ].
    element := collection at: ptr.
    ptr := ptr + 1.
    ^element
!

peek
    "Returns the next element of the stream without moving the pointer.
    Returns nil when at end of stream."
    | peekValue |
    self atEnd ifTrue: [ ^nil ].
    peekValue := self next.
    self skip: -1.
    ^peekValue    
!

peekFor: anObject
    "Returns true and gobbles the next element from the stream of it is
    equal to anObject, returns false and doesn't gobble the next element
    if the next element is not equal to anObject."
    (self peek) = anObject
    	ifTrue: [ self next.
	    	  ^true ]
    	ifFalse: [ ^false ]
!

upToEnd
    "Answer every item in the collection on which the receiver is
     streaming, from the next one to the last"
    | ws |
    ws := WriteStream on: (collection copyEmpty: 8).
    [ self atEnd ] whileFalse: [ ws nextPut: self next ].
    ^ws contents
!

copyFrom: start to: end
    "Answer the collection on which the receiver is streaming, from
     the start-th item to the end-th"

    ^collection copyFrom: start to: end
!

nextLine
    "Returns a collection of the same type that the stream accesses, up to 
    but not including the object anObject.  Returns the entire rest of the 
    stream's contents if anObject is not present. "
    | next ws |
    ws := WriteStream on: (collection copyEmpty: 40).
    [ self atEnd or: [ (next := self next) = Character cr | (next = Character nl) ] ]
        whileFalse: [ ws nextPut: next ].
    ^ws contents
!

upToAll: aCollection
    "If there is a sequence of objects remaining in the stream that is
     equal to the sequence in aCollection, set the stream position just
     past that sequence and answer the elements up to, but not including,
     the sequence. Else, set the stream position to its end and answer
     all the remaining elements."
    | buffer next nextSource pos ws |

    buffer := ReadWriteStream onEmpty: (collection copyEmpty: 8).
    ws := WriteStream on: (collection copyEmpty: 8).
    [   [   nextSource := self nextWithBuffer: buffer.
            nextSource isNil ifTrue: [ ^ws contents ].
            ws nextPut: (next := nextSource next).
            next = aCollection first
        ]   whileFalse: [ ].

        "Push back the first character of the (possible) match"
        nextSource == self ifTrue: [ buffer nextPut: next ].
        pos := buffer position.
        buffer skip: -1.

        aCollection allSatisfy: [ :each |
            nextSource := self nextWithBuffer: buffer.
            nextSource isNil ifTrue: [ ^ws contents ].
            next := nextSource next.
            nextSource == self ifTrue: [ buffer nextPut: next ].
            each = next
        ].
    ]   whileFalse: [ buffer position: pos ].
    "Remove from the answered stream the first element of aCollection"
    ^ws
        skip: -1;
        contents
!

upTo: anObject
    "Returns a collection of the same type that the stream accesses, up to 
    but not including the object anObject.  Returns the entire rest of the 
    stream's contents if anObject is not present. "
    | next ws |
    ws := WriteStream on: (collection copyEmpty: 8).
    [ self atEnd or: [ (next := self next) = anObject ] ] whileFalse: [
        ws nextPut: next
    ].
    ^ws contents
!

contents
    "Returns a collection of the same type that the stream accesses, up to 
    and including the final element."
    ^collection copyFrom: 1 to: endPtr
!

reverseContents
    "Returns a collection of the same type that the stream accesses, up to 
    and including the final element, but in reverse order."
    | newCollection |
    newCollection := collection copyEmpty: endPtr.
    1 to: endPtr do:
        [ :i | newCollection at: i put: (collection at: endPtr - i + 1) ].
    ^newCollection
! !



!PositionableStream methodsFor: 'testing'!

atEnd
    "Answer whether the objects in the stream have reached an end"
    ^ptr > endPtr
!

basicAtEnd
    "Answer whether the objects in the stream have reached an end.
     This method must NOT be overridden."
    ^ptr > endPtr
!

isEmpty
    "Answer whether the stream has no objects"
    ^endPtr = 0
! !


!PositionableStream methodsFor: 'truncating'!

truncate
    "Truncate the receiver to the current position - only valid
     for writing streams"
    (access bitAnd: 2) = 0
        ifTrue: [ self shouldNotImplement ]
        ifFalse: [ endPtr := ptr - 1 ]
!!

!PositionableStream methodsFor: 'positioning'!

position
    "Answer the current value of the stream pointer"
    ^ptr
!

position: anInteger
    "Move the stream pointer to the anInteger-th object"
    (anInteger between: 1 and: endPtr + 1)
	ifTrue: [ ptr := anInteger ]
	ifFalse: [ endPtr printNl. ^self error: 'position out of range' ]
!

basicPosition: anInteger
    "Move the stream pointer to the anInteger-th object"
    (anInteger between: 1 and: endPtr + 1)
	ifTrue: [ ptr := anInteger ]
	ifFalse: [ ^self error: 'position out of range' ]
!

reset
    "Move the stream back to its first element. For write-only streams,
    the stream is truncated there."
    self position: 1
!

skipToAll: aCollection
    "If there is a sequence of objects remaining in the stream that is
     equal to the sequence in aCollection, set the stream position just
     past that sequence and answer true. Else, set the stream position
     to its end and answer false."
    | buffer next nextSource pos |

    buffer := ReadWriteStream onEmpty: (collection copyEmpty: 8).
    [   [   nextSource := self nextWithBuffer: buffer.
            nextSource isNil ifTrue: [ ^false ].
            next := nextSource next.
            next = aCollection first
        ]   whileFalse: [ ].

        "Push back the first character of the (possible) match"
        nextSource == self ifTrue: [ buffer nextPut: next ].
        pos := buffer position.
        buffer skip: -1.

        aCollection allSatisfy: [ :each |
            nextSource := self nextWithBuffer: buffer.
            nextSource isNil ifTrue: [ ^false ].
            next := nextSource next.
            nextSource == self ifTrue: [ buffer nextPut: next ].
            each = next
        ].
    ]   whileFalse: [ buffer position: pos ].
    ^true
!

setToEnd
    "Move the current position to the end of the stream."
    ptr := endPtr + 1
!

skip: anInteger
    "Move the current position by anInteger places, either forwards or
    backwards."
    self position: ((ptr + anInteger max: 1) min: endPtr).
!

skipTo: anObject
    "Move the current position to after the next occurrence of anObject
    and return true if anObject was found.  If anObject doesn't exist, the 
    pointer is atEnd, and false is returned."

    [ self atEnd ] whileFalse: [
    	self next = anObject ifTrue: [ ^true ]
    ].
    ^false

! !


!PositionableStream methodsFor: 'private'!

nextWithBuffer: buffer
    "Answer whether the next character must be read from the receiver
     or from the read-ahead buffer contained in buffer (another Po-
     sitionableStream). If both the receiver and the buffer contain
     no data, nil is answered, else the correct stream is answered."
    ^buffer atEnd
        ifTrue: [
            self atEnd ifTrue: [ nil ] ifFalse: [ self ]
        ]
        ifFalse: [ buffer ]
!

status: aString
    "When working with a stream on strings, this method can be useful!
     Format of the output:
        'ABCDEFG'   aString
           ^                    "
    Transcript
       print: (collection copyFrom: 1 to: endPtr);
       nextPutAll: '    ';
       nextPutAll: aString;
       nl;
       next: self position put: Character space;
       nextPut: $^;
       nl
!

access: anInteger
    access := anInteger
! !


!PositionableStream methodsFor: 'class type methods'!

species
    "The collections returned by #upTo: etc. are the same kind as
    those returned by the collection with methods such as #select:"
    ^collection species
!!
