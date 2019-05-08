"======================================================================
|
|   Set Method Definitions
|
|   $Revision: 1.6.2$
|   $Date: 1999/08/31 11:23:18$
|   $Author: pb$
|
 ======================================================================"


"======================================================================
|
| Copyright 1990, 91, 92, 94, 95, 99 Free Software Foundation, Inc.
| Written by Steve Byrne and Paolo Bonzini.
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


Collection variableSubclass: #Set
	   instanceVariableNames: 'tally'
	   classVariableNames: ''
	   poolDictionaries: ''
	   category: 'Collections-Unordered'
!

Set comment:
'I am the typical set object; I can store any objects uniquely.  I
use the = operator to determine duplication of objects.' !


!Set class methodsFor: 'instance creation'!

new
    "Answer a new instance of the receiver with a default size"
    ^self new: 5
!

new: anInteger
    "Answer a new instance of the receiver with the given size"
    ^(super new: (5 max: anInteger)) setTally
!!



!Set methodsFor: 'accessing'!

at: index
    self shouldNotImplement
!

at: index put: value
    self shouldNotImplement
!

add: newObject
    "Add newObject to the set, if and only if the set doesn't already contain
     an occurrence of it. Don't fail if a duplicate is found. Answer anObject"
    | index absent |
    newObject isNil ifTrue: [ ^newObject ].
    absent := false.
    index := self findIndex: newObject ifAbsent: [ absent := true ].
    self primAt: index put: newObject.
    absent ifTrue: [ self incrementTally ].
    ^newObject
!!



!Set methodsFor: 'Removing from a collection'!

remove: oldObject ifAbsent: anExceptionBlock
    "Remove oldObject to the set. If it is found, answer oldObject.
     Otherwise, evaluate anExceptionBlock and return its value."
    | index |
    index := self findIndex: oldObject ifAbsent: [ ^anExceptionBlock value ].
    self primAt: index put: nil.
    self decrementTally.
    self rehashObjectsAfter: index.
    ^oldObject
!!



!Set methodsFor: 'testing collections'!

includes: anObject
    "Answer whether the receiver contains an instance of anObject."
    self findIndex: anObject ifAbsent: [^false].
    ^true
!

isEmpty
    "Answer whether the receiver is empty."
    ^tally == 0
!

occurrencesOf: anObject
    "Return the number of occurrences of anObject.  Since we're a set, this
    is either 0 or 1.  Nil is never directly in the set, so we special case
    it (the result is always 1)."
    anObject isNil ifTrue: [ ^1 ].
    ^(self includes: anObject)
    	ifTrue: [ 1 ]
	ifFalse: [ 0 ]
!

capacity
    "Answer how many elements the receiver can hold before having to grow."
    ^self basicSize * 3 // 4
!

size
    "Answer the receiver's size"
    ^tally
!

hash
    "Return the hash code for the members of the set.  Since order is
    unimportant, we use a commutative operator to compute the hash value."
    | hashValue |
    hashValue := tally.
    self do: [ :member |
        hashValue := hashValue bitXor: (self hashFor: member)
    ].
    ^hashValue
!

= aSet
    "Returns true if the two sets have the same membership, false if not"
    self class == aSet class ifFalse: [ ^false ].
    self == aSet ifTrue: [ ^true ].
    tally = aSet size  ifFalse: [ ^false ].
    self do: [ :element | (aSet includes: element)
    	    	    	    ifFalse: [ ^false ] ].
    ^true
!!



!Set methodsFor: 'enumerating the elements of a collection'!

do: aBlock
    "Enumerate all the non-nil members of the set"
    1 to: self basicSize do:
    	[ :i | (self primAt: i) notNil
	    	  ifTrue: [ aBlock value: (self primAt: i) ] ]
!!



!Set methodsFor: 'storing'!

storeOn: aStream
    "Store on aStream some Smalltalk code which compiles to the receiver"
    | hasElements |
    aStream nextPut: $(;
            nextPutAll: self classNameString;
            nextPutAll: ' new'.

    hasElements := false.
    self do:
    	[ :element | aStream nextPutAll: ' add: '.
		     element storeOn: aStream.
		     aStream nextPut: $;.
		     hasElements := true ].
    hasElements ifTrue: [ aStream nextPutAll: ' yourself' ].
    aStream nextPut: $).
!!


!Set methodsFor: 'rehashing'!

rehash
    "Rehash the receiver"
    | objects element |
    objects := self copy.
    1 to: self basicSize do: [:i | self primAt: i put: nil ].
    1 to: self basicSize do: [ :i |
        element := objects primAt: i.
        element isNil ifFalse: [
            self primAt: (self findIndex: element) put: element
        ]
    ].
!!


!Set methodsFor: 'private methods'!

setTally
    "Instance variable initialization."
    tally := 0
!

incrementTally
    "Watch out!! This uses become:"
    tally := tally + 1.
    tally >= (self basicSize * 3 // 4) ifTrue: [ self growBy: self growSize ].
!

decrementTally
    tally := tally - 1.
!

rehashObjectsAfter: index
    "Private - Rehashes all the objects in the collection after index to
     see if any of them hash to index.  If so, that object is copied to
     index, and the process repeats with that object's index, until a nil
     is encountered."
    | i j size element |
    i := index.
    size := self basicSize.
    [   i = size ifTrue: [ i := 1 ] ifFalse: [ i := i + 1 ].
        element := self primAt: i.
        element notNil
    ]   whileTrue: [
        j := self findIndex: element.
        (self primAt: j) isNil ifTrue: [
            self primAt: j put: element.
            self primAt: i put: nil
        ]
    ]
!

primAt: index
    "Private - Answer the index-th item of the hash table for the receiver.
     Using this instead of basicAt: allows for easier changes in the
     representation"
    <primitive: 60>
    ^self error: 'index out of bounds'
!

primAt: index put: value
    "Private - Store value in the index-th item of the hash table for the
     receiver. Using this instead of basicAt:put: allows for easier
     changes in the representation"
    <primitive: 61>

   (index < 1)
        ifTrue: [ ^self error: 'index out of bounds' ].

   (index > self basicSize)
        ifTrue: [ ^self error: 'index out of bounds' ]
        ifFalse: [ ^self error: 'invalid argument type' ]
!


hashFor: anElement

    "Answer the hash value for anElement"

    ^anElement hash
!

is: anElement sameAs: searchedObject

    "Answer whether findIndex: should stop scanning the receiver: anElement has
     been found and findIndex:'s parameter was searchedObject"

    ^anElement = searchedObject
!

findIndex: anObject
    "Tries to see if anObject exists as an indexed variable. As soon as nil
    or anObject is found, the index of that slot is answered"

    | index size element |
    size := self basicSize.
    index := (self hashFor: anObject) \\ size + 1.
    [
        element := self primAt: index.
        element isNil or: [ self is: element sameAs: anObject ]
    ]   whileFalse: [
        index = size ifTrue: [ index := 1 ] ifFalse: [ index := index + 1 ].
    ].
    ^index
!
	    
findIndex: anObject ifAbsent: aBlock
    "Finds the given object in the set and returns its index.  If the set
    doesn't contain the object, aBlock is evaluated; the index is always
    answered, even if the set doesn't contain anObject"

    | index |
    index := self findIndex: anObject.
    (self primAt: index) isNil ifTrue: [ aBlock value ].
    ^index
!

grow
    ^self growBy: self growSize
!

growBy: delta
    | newSet |
    newSet := self copyEmpty: self basicSize + delta.
    self do: [ :element | newSet add: element ].
    ^self become: newSet
!

growSize
    "this will almost double the size, keeping it an odd number; if size < 7,
     it will make size = 7"
    | size |
    size := self basicSize.
    ^(size - 1) max: (7 - size)
!!


!Set methodsFor: 'awful ST-80 compatibility hacks'!

findObjectIndex: object
    "Tries to see if anObject exists as an indexed variable. As soon as nil
    or anObject is found, the index of that slot is answered"
    ^self findIndex: object
!!
