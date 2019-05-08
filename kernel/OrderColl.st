"======================================================================
|
|   OrderedCollection Method Definitions
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


SequenceableCollection variableSubclass: #OrderedCollection
		       instanceVariableNames: 'firstIndex lastIndex'
		       classVariableNames: ''
		       poolDictionaries: ''
		       category: 'Collections-Sequenceable'
!

OrderedCollection comment: 
'My instances represent ordered collections of arbitrary typed objects which
are not directly accessible by an index.  They can be accessed indirectly
through an index, and can be manipulated by adding to the end or based
on content (such as add:after:)' !


!OrderedCollection class methodsFor: 'instance creation'!

new: anInteger
    "Answer an OrderedCollection of size anInteger"
    ^(self basicNew: anInteger) initIndices
!

new
    "Answer an OrderedCollection of default size"
    ^self new: 16

! !



!OrderedCollection methodsFor: 'accessing'!

at: anIndex
    "Answer the anIndex-th item of the receiver"
    | index |
    index := anIndex + firstIndex - 1.
    (index >= firstIndex and: [ index <= lastIndex ])
    	ifTrue: [ ^self basicAt: index ]
	ifFalse: [ ^self error: 'index out of bounds for ordered collection' ]
!

at: anIndex put: anObject
    "Store anObject at the anIndex-th item of the receiver, answer anObject"
    | index |
    index := anIndex + firstIndex - 1.
    (index >= firstIndex and: [ index <= lastIndex ])
    	ifTrue: [ ^self basicAt: index put: anObject ]
	ifFalse: [ ^self error: 'index out of bounds for ordered collection' ]
!    

after: oldObject
    "Return the element after oldObject.  Error if oldObject not found or
    if no following object is available"
    firstIndex to: lastIndex do:
    	[ :index |		"should we use '=' or '==' here?"
	    (self basicAt: index) = oldObject
	    	ifTrue: [
		    index < lastIndex
		    	ifTrue: [ ^self basicAt: index + 1 ]
			ifFalse: [ ^self error: 'no following object' ] ]
    	].
    ^self error: 'object not found'
!

before: oldObject
    "Return the element after oldObject.  Error if oldObject not found or
    if no following object is available"
    firstIndex to: lastIndex do:
    	[ :index |		"should we use '=' or '==' here?"
	    (self basicAt: index) = oldObject
	    	ifTrue: [
		    index > firstIndex
		    	ifTrue: [ ^self basicAt: index - 1 ]
			ifFalse: [ ^self error: 'no preceding object' ] ]
    	].
    ^self error: 'object not found'
!

size
    "Return the number of objects in the receiver"
    ^lastIndex - firstIndex + 1
! !



!OrderedCollection methodsFor: 'adding'!

add: anObject
    "Add anObject in the receiver, answer it"

    self makeRoomLastFor: 1.
    lastIndex := lastIndex + 1.
    ^self basicAt: lastIndex put: anObject
!

add: newObject after: oldObject
    "Add newObject in the receiver just after oldObject, answer it.
     Fail if oldObject can't be found"

    ^self
       add: newObject
       afterIndex: (self
           indexOf: oldObject
           ifAbsent: [^self error: 'object not found in collection' ])
!

add: newObject before: oldObject
    "Add newObject in the receiver just before oldObject, answer it.
     Fail if oldObject can't be found"

    ^self
       add: newObject
       afterIndex: (self
           indexOf: oldObject
           ifAbsent: [^self error: 'object not found in collection' ]) - 1
!

add: newObject afterIndex: i
    "Add newObject in the receiver just after the i-th, answer it.
     Fail if i < 0 or i > self size "

    | index |
    index := i + firstIndex.

    self makeRoomLastFor: 1.

    lastIndex to: index by: -1 do: [ :i |
        self basicAt: i + 1 put: (self basicAt: i)
    ].

    lastIndex := lastIndex + 1.

    ^self basicAt: index put: newObject
!

add: newObject beforeIndex: i
    "Add newObject in the receiver just before the i-th, answer it.
     Fail if i < 1 or i > self size + 1"

    ^self add: newObject afterIndex: i - 1
!

addAll: aCollection
    "Add every item of aCollection to the receiver, answer it"

    | index |
    self makeRoomLastFor: aCollection size.
    index := lastIndex + 1.
    lastIndex := lastIndex + aCollection size.

    aCollection do: [ :element |
        self basicAt: index put: element. index := index + 1 ].

    ^aCollection
!

addAll: newCollection after: oldObject
    "Add every item of newCollection to the receiver just after
     oldObject, answer it. Fail if oldObject is not found"

    ^self
       addAll: newCollection
       afterIndex: (self indexOf: oldObject ifAbsent: [^self error: 'object not found in collection' ])
!

addAll: newCollection afterIndex: i
    "Add every item of newCollection to the receiver just after
     the i-th, answer it. Fail if i < 0 or i > self size"

    | index |

    index := i + firstIndex.

    self makeRoomLastFor: newCollection size.

    lastIndex to: index by: -1 do: [ :i |
        self basicAt: i + newCollection size put: (self basicAt: i)
    ].
    lastIndex := lastIndex + newCollection size.

    newCollection doWithIndex: [:each :i |
        self basicAt: index + i - 1 put: each
    ].

    ^newCollection
!

addAll: newCollection before: oldObject
    "Add every item of newCollection to the receiver just before
     oldObject, answer it. Fail if oldObject is not found"

    ^self
       addAll: newCollection
       afterIndex: (self indexOf: oldObject ifAbsent: [^self error: 'object not found in collection' ]) - 1
!

addAll: newCollection beforeIndex: i
    "Add every item of newCollection to the receiver just before
     the i-th, answer it. Fail if i < 1 or i > self size + 1"

    ^self add: newCollection afterIndex: i - 1
!

addAllFirst: aCollection
    "Add every item of newCollection to the receiver right at the start
     of the receiver. Answer aCollection"

    | index |
    self makeRoomFirstFor: aCollection size.
    index := firstIndex := firstIndex - aCollection size.

    aCollection do: [ :element |
        self basicAt: index put: element. index := index + 1 ].

    ^aCollection
!
    
addAllLast: aCollection
    "Add every item of newCollection to the receiver right at the end
     of the receiver. Answer aCollection"

    | index |
    self makeRoomLastFor: aCollection size.
    index := lastIndex + 1.
    lastIndex := lastIndex + aCollection size.

    aCollection do: [ :element |
        self basicAt: index put: element. index := index + 1 ].

    ^aCollection
!
    
addFirst: newObject
    "Add newObject to the receiver right at the start of the receiver.
    Answer newObject"

    self makeRoomFirstFor: 1.
    firstIndex := firstIndex - 1.
    ^self basicAt: firstIndex put: newObject
!
    
addLast: newObject
    "Add newObject to the receiver right at the end of the receiver.
    Answer newObject"

    self makeRoomLastFor: 1.
    lastIndex := lastIndex + 1.
    ^self basicAt: lastIndex put: newObject
! !



!OrderedCollection methodsFor: 'removing'!

removeFirst
    "Remove an object from the start of the receiver. Fail if the receiver
     is empty"

    | answer |
    lastIndex < firstIndex
         ifTrue: [ ^self error: 'attempted to remove from an empty collection' ].

    answer := self basicAt: firstIndex.   "Get the element"
    self basicAt: firstIndex put: nil.   "Leave it to be garbage collected"
    firstIndex := firstIndex + 1.
    ^answer
!

removeLast
    "Remove an object from the end of the receiver. Fail if the receiver
     is empty"

    | answer |
    lastIndex < firstIndex
         ifTrue: [ ^self error: 'attempted to remove from an empty collection' ].

    answer := self basicAt: lastIndex.   "Get the element"
    self basicAt: lastIndex put: nil.    "Allow it to be garbage collected"
    lastIndex := lastIndex - 1.
    ^answer
!

remove: anObject ifAbsent: aBlock
    "Remove anObject from the receiver. If it can't be found, answer the 
     result of evaluating aBlock"

    ^self removeAtIndex:
        (self indexOf: anObject startingAt: 1 ifAbsent: [ ^aBlock value ])
!

removeAtIndex: anIndex
    "Remove the object at index anIndex from the receiver. Fail if the
     index is out of bounds"

    | answer |
    lastIndex < firstIndex
         ifTrue: [ ^self error: 'attempted to remove from an empty collection' ].

    (anIndex < 1 or: [anIndex > self size])
        ifTrue: [^self error: 'index out of bounds' ].

    answer := self basicAt: anIndex + firstIndex - 1.
    anIndex + firstIndex to: lastIndex do: [ :index |
         self basicAt: index - 1 put: (self basicAt: index)
    ].
    self basicAt: lastIndex put: nil.
    lastIndex := lastIndex - 1.
    ^answer
! !


!OrderedCollection methodsFor: 'private methods'!

basicAddLast: newObject
    "Private - Add to the end of the receiver newObject, answer newObject.
     Don't override this method!"

    self makeRoomLastFor: 1.
    lastIndex := lastIndex + 1.
    ^self basicAt: lastIndex put: newObject
!

basicAddAllLast: aCollection
    "Private - Add to the end of the receiver all the items in aCollection,
     answer newObject. Don't override this method!"

    | index |
    self makeRoomLastFor: aCollection size.
    index := lastIndex + 1.
    lastIndex := lastIndex + aCollection size.

    aCollection do: [ :element |
        self basicAt: index put: element. index := index + 1 ].

    ^aCollection
!

initIndices
    firstIndex := self basicSize // 2 max: 1.
    lastIndex := firstIndex - 1
!

firstIndex: first lastIndex: last
    firstIndex := first.
    lastIndex := last
!

makeRoomFirstFor: n
    "Private - Make room for n elements at the start of the collection"

    firstIndex <= n
        ifTrue: [
            self
                growBy: (n max: self growSize)
                shiftBy: (n max: self growSize)
        ]
!

makeRoomLastFor: n
    "Private - Make room for n elements at the end of the collection"

    (lastIndex + n) > self basicSize
        ifTrue: [
            self
                growBy: (n max: self growSize)
                shiftBy: 0
        ]
!

grow
    "Make growSize room in the collection, putting the old contents in the
    middle."

    self growBy: self growSize shiftBy: self growSize // 2
!

growBy: delta shiftBy: shiftCount
    "Make room for delta more places in the collection, shifting the old
     contents by shiftCount places"

    | newOrderedCollection |
    newOrderedCollection := self copyEmpty: self basicSize + delta.

    firstIndex to: lastIndex do:
        [ :index | newOrderedCollection basicAt: index + shiftCount
	    	    	    	    	put: (self basicAt: index) ].
    newOrderedCollection
        firstIndex: firstIndex + shiftCount
        lastIndex: lastIndex + shiftCount.

    self become: newOrderedCollection    
! !

