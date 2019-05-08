"======================================================================
|
|   IdentityDictionary Method Definitions
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

Dictionary variableSubclass: #IdentityDictionary
	   instanceVariableNames: 'values'
	   classVariableNames: ''
	   poolDictionaries: ''
	   category: 'Collections-Keyed'
!

IdentityDictionary comment:
'I am similar to dictionary, except that my representation is
different, and I use the object identity comparision message == to
determine equivalence of indices.' !


!IdentityDictionary class methodsFor: 'instance creation'!

new
    "Create a new dictionary with a default size"
    ^self new: 5
!

new: anInteger
    "Create a new dictionary with the given size"
    ^(super new: anInteger) initValues

!!



!IdentityDictionary methodsFor: 'accessing'!

add: anAssociation
    "Add the anAssociation key to the receiver"
    self at: anAssociation key put: anAssociation value.
    ^anAssociation
!

at: key put: value
    "Store value as associated to the given key"
    | index absent |
    absent := false.
    index := self findIndex: key ifAbsent: [ absent := true ].
    self primAt: index put: key.
    values at: index put: value.
    absent ifTrue: [ self incrementTally ].
    ^value
!

at: key ifAbsent: aBlock
    "Answer the value associated to the given key, or the result of evaluating
     aBlock if the key is not found"
    | index |
    index := self findIndex: key ifAbsent: [ ^aBlock value ].
    ^values at: index
!    
    
at: aKey ifPresent: aBlock
    "If aKey is absent, answer nil. Else, evaluate aBlock passing the
     associated value and answer the result of the invocation"
    | index |
    index := self findIndex: aKey ifAbsent: [^nil ].
    ^aBlock value: (values at: index)
!

associationAt: key ifAbsent: aBlock
    "Answer the key/value Association for the given key. Evaluate aBlock
     (answering the result) if the key is not found"
    | value |
    value := self at: key ifAbsent: [ ^aBlock value ].
    ^Association key: key value: value
!!



!IdentityDictionary methodsFor: 'dictionary removing'!

removeKey: key ifAbsent: aBlock
    "Remove the passed key from the dictionary, answer the result of
     evaluating aBlock if it is not found"
    | index value |
    index := self findIndex: key ifAbsent: [ ^aBlock value ].
    value := values at: index.
    self primAt: index put: nil.
    values at: index put: nil.
    self decrementTally.
    self rehashObjectsAfter: index.
    ^ value
!!



!IdentityDictionary methodsFor: 'dictionary enumerating'!

associationsDo: aBlock
    "Pass each association in the dictionary to aBlock"
    self keysAndValuesDo: [ :key :val | aBlock value:
        (Association key: key value: val)
    ]
!

keysAndValuesDo: aBlock
    "Pass each key/value pair in the dictionary as two distinct parameters
     to aBlock"

    1 to: self basicSize do: [:i |
        (self primAt: i) notNil
            ifTrue: [ aBlock value: (self primAt: i) value: (values at: i) ]
    ]
!!



!IdentityDictionary methodsFor: 'rehashing'!

rehash
    "Rehash the receiver"
    | copy |
    copy := self copy.

    1 to: self basicSize do: [:i |
        self primAt: i put: nil.
        values at: i put: nil ].

    copy keysAndValuesDo: [ :key :val | self at: key put: val ].
!!


!IdentityDictionary methodsFor: 'storing'!

storeOn: aStream
    "Print Smalltalk code compiling to the receiver on aStream"
    | hasElements |
    aStream nextPutAll: '(', self class name , ' new'.
    hasElements := false.
    self keysAndValuesDo:
    	[ :key :value | aStream nextPutAll: ' at: ';
		   store: key;
		   nextPutAll: ' put: ';
		   store: value;
		   nextPut: $;.
	       hasElements := true ].
    hasElements ifTrue: [ aStream nextPutAll: ' yourself' ].
    aStream nextPut: $)
!!



!IdentityDictionary methodsFor: 'private methods'!

initValues
    values := Array new: self basicSize
!

rehashObjectsAfter: index
    "Rehashes all the objects in the collection after index to see if any of
    them hash to index.  If so, that object is copied to index, and the
    process repeats with that object's index, until a nil is encountered."

    | i j size count key |
    i := index.
    size := self basicSize.
    [   i = size ifTrue: [ i := 1 ] ifFalse: [ i := i + 1 ].
        key := self primAt: i.
        key notNil
    ]   whileTrue: [
        j := self findIndex: key.
        (self primAt: j) isNil ifTrue: [
            self primAt: j put: key.
            values at: j put: (values at: i).
            self primAt: i put: nil.
            values at: i put: nil
        ]
    ]
!

growBy: delta
    "Private - Grow by the receiver by delta places"

    | newDict |
    newDict := self copyEmpty: self basicSize + delta.
    self keysAndValuesDo: [ :key :val | newDict whileGrowingAt: key put: val ].
    ^self become: newDict
!

addWhileGrowing: association
    self shouldNotImplement
!

whileGrowingAt: key put: value
    "Private - Add the given key/value pair to the receiver. Don't check for
     the dictionary to be full nor for the key's presence - we want SPEED!"
    | index |
    self
        primAt: (index := self findIndex: key)
        put: key.
    
    values at: index put: value.
    tally := tally + 1.
    ^value
!

hashFor: anElement

    "Answer the hash value for anElement"

    ^anElement identityHash
!

keysClass
    "Answer the class answered by #keys"
    ^IdentitySet
!

is: anElement sameAs: searchedObject

    "Answer whether findIndex: should stop scanning the receiver: anElement has
     been found and findIndex:'s parameter was searchedObject"

    ^anElement == searchedObject
!!
