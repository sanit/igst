"======================================================================
|
|   ArrayedCollection Method Definitions
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


SequenceableCollection variableSubclass: #ArrayedCollection
		       instanceVariableNames: ''
		       classVariableNames: ''
		       poolDictionaries: ''
		       category: 'Collections-Sequenceable'
!

ArrayedCollection comment: 
'My instances are objects that are generally fixed size, and are accessed
by an integer index.  The ordering of my instance''s elements is determined 
externally; I will not rearrange the order of the elements.' !


!ArrayedCollection class methodsFor: 'instance creation'!

new
    "Answer an empty collection"
    ^self new: 0
!

new: size withAll: anObject
    "Answer a collection with the given size, whose elements are all set to
     anObject"
    ^(self new: size) atAllPut: anObject; yourself
!

withAll: aCollection
    "Answer a collection whose elements are the same as those in aCollection"
    | anArrayedCollection index |
    anArrayedCollection := self new: aCollection size.
    index := 1.
    aCollection do: [ :each |
        anArrayedCollection at: index put: each.
        index := index + 1
    ].
    ^anArrayedCollection
!

with: element1
    "Answer a collection whose only element is element1"
    | anArrayedCollection |
    anArrayedCollection := self new: 1.
    anArrayedCollection at: 1 put: element1.
    ^anArrayedCollection
!

with: element1 with: element2
    "Answer a collection whose only elements are the parameters in the order
     they were passed"
    | anArrayedCollection |
    anArrayedCollection := self new: 2.
    anArrayedCollection at: 1 put: element1.
    anArrayedCollection at: 2 put: element2.
    ^anArrayedCollection
!

with: element1 with: element2 with: element3
    "Answer a collection whose only elements are the parameters in the order
     they were passed"
    | anArrayedCollection |
    anArrayedCollection := self new: 3.
    anArrayedCollection at: 1 put: element1.
    anArrayedCollection at: 2 put: element2.
    anArrayedCollection at: 3 put: element3.
    ^anArrayedCollection
!

with: element1 with: element2 with: element3 with: element4
    "Answer a collection whose only elements are the parameters in the order
     they were passed"
    | anArrayedCollection |
    anArrayedCollection := self new: 4.
    anArrayedCollection at: 1 put: element1.
    anArrayedCollection at: 2 put: element2.
    anArrayedCollection at: 3 put: element3.
    anArrayedCollection at: 4 put: element4.
    ^anArrayedCollection
! !



!ArrayedCollection methodsFor: 'basic'!

add: value
    self shouldNotImplement
!

, aSequenceableCollection
    "Answer a new instance of an ArrayedCollection containing all the elements
     in the receiver, followed by all the elements in aSequenceableCollection"
    | result |
    ^(self copyEmpty: (self size + aSequenceableCollection size))
        replaceFrom: 1
               to: self size
               with: self;
        replaceFrom: (self size) + 1
    	       to: self size + aSequenceableCollection size
	       with: aSequenceableCollection;
	yourself
!

copyWithout: oldElement
    "Answer a copy of the receiver to which all occurrences of oldElement are
     removed"
    | newCollection numOccurrences i |
    numOccurrences := 0.
    self do: [ :element |
	element = oldElement
	    ifTrue: [ numOccurrences := numOccurrences + 1 ]
    ].
    newCollection := self copyEmpty: (self size - numOccurrences).
    i := 1.
    self do: [ :element |
	element = oldElement ifFalse: [
	    newCollection at: i put: element.
	    i := i + 1
	]
    ].
    ^newCollection
!

copyWith: anElement
    "Answer a new instance of an ArrayedCollection containing all the elements
     in the receiver, followed by the single item anElement"
    | result |
    ^(self copyEmpty: (self size + 1))
        replaceFrom: 1
               to: self size
               with: self;
        at: (self size) + 1 put: anElement;
	yourself
! !


!ArrayedCollection methodsFor: 'enumerating the elements of a collection'!

select: aBlock
    "Answer a new instance of an ArrayedCollection containing all the elements
     in the receiver which, when passed to aBlock, answer true"

    | newCollection |
    newCollection := WriteStream on: (self copyEmpty: self size).
    self do: [ :element | (aBlock value: element)
                            ifTrue: [ newCollection nextPut: element ]
	     ].
    ^newCollection contents
!

reject: aBlock
    "Answer a new instance of an ArrayedCollection containing all the elements
     in the receiver which, when passed to aBlock, answer false"

    | newCollection |
    newCollection := WriteStream on: (self copyEmpty: self size).
    self do: [ :element | (aBlock value: element)
                            ifFalse: [ newCollection nextPut: element ]
	     ].
    ^newCollection contents
!

collect: aBlock
    "Answer a new instance of an ArrayedCollection containing all the results
     of evaluating aBlock passing each of the receiver's elements"

    | newCollection index |
    index := 1.
    newCollection := self copyEmpty: self size.
    self do: [ :element |
        newCollection at: index put: (aBlock value: element).
        index := index + 1 ].
    ^newCollection
!!


!ArrayedCollection methodsFor: 'copying Collections'!

copyWith: newElement
    "Answer a new instance of an ArrayedCollection containing all the
     receiver's elements and, at the end, newElement"

    ^(self copyEmpty: self size + 1)
          replaceFrom: 1 to: self size with: self startingAt: 1;
          at: self size + 1 put: newElement;
          yourself
!

reverse
    "Answer the receivers' contents in reverse order"
    | result |
    result := self copyEmpty.
    self with: (self size to: 1 by: -1) do: [ :each :newIndex |
        result at: newIndex put: each
    ].
    ^result
!!


!ArrayedCollection methodsFor: 'storing'!

storeOn: aStream
    "Store Smalltalk code compiling to the receiver on aStream"

    | index |
    aStream nextPutAll: '(('; nextPutAll: self classNameString; 
        nextPutAll: ' basicNew: '.
    self basicSize printOn: aStream.
    aStream nextPut: $).
    index := 1.
    self do:
    	[ :element | aStream nextPutAll: ' at: '.
	    	     index printOn: aStream.
		     aStream nextPutAll: ' put: '.
    	    	     element storeOn: aStream.
		     aStream nextPut: $;.
    	    	     index := index + 1 ].
    index > 1 ifTrue: [ aStream nextPutAll: ' yourself' ].
    aStream nextPut: $)
!!


!ArrayedCollection methodsFor: 'private'!

grow
    "Private - Grow by some amount"
    self growBy: self growSize
!

copyGrowTo: newSize
    "Private - Answer a copy of the receiver grown to newSize elements"
    | newCollection |
    newCollection := self copyEmpty: newSize.
    newCollection replaceFrom: 1 to: self size with: self.
    ^newCollection
!

growBy: delta
    "Private - Make the receiver grow by delta elements"
    ^self become: (self copyGrowTo: self basicSize + delta)
!

growTo: newSize
    "Private - Make the receiver grow to a size of newSize"
    ^self become: (self copyGrowTo: newSize)
!!

