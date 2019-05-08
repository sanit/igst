"======================================================================
|
|   Dictionary Method Definitions
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


Set variableSubclass: #Dictionary
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Collections-Keyed'
!

Dictionary comment: 
'I implement a dictionary, which is an object that is indexed by
unique objects (typcially instances of Symbol), and associates another
object with that index.  I use the equality operator = to determine
equality of indices.' !

"### The initblocks variable should not be globally visible, I think"
"This is a HACK HACK HACK.  We want to reference the InitBlocks global variable
from within some methods in System Dictionary.  However, after this file
redefines at:put: from the built-in one, and until UndefinedObject.st is 
loaded, defining isNil for nil, at:put: for dictionaries does not work
properly.  So we do it here.  The basic problem is that InitBlocks should
maybe be kept elsewhere, and not be globally visible."
Smalltalk at: #InitBlocks put: nil!


!Dictionary class methodsFor: 'instance creation'!

new
    "Create a new dictionary with a default size"
    "Builtins defines a #new method, so that during bootstrap there is a way
     to create dictionaries.  Unfortunately, this #new method only creates
     dictionaries, so subclasses when trying to use this method, lose big.
     This fixes the problem."
    ^self new: 31
! !



!Dictionary methodsFor: 'accessing'!

add: newObject
    "Add the newObject association to the receiver"
    | index absent |
    absent := false.
    index := self findIndex: newObject key ifAbsent: [ absent := true ].
    absent
        ifTrue: [
            self primAt: index put: newObject.
            self incrementTally
        ]
        ifFalse: [
            (self primAt: index) value: newObject value
        ].
    ^newObject
!

at: key put: value
    "Store value as associated to the given key"
    | index assoc |
    index := self findIndex: key.
    (assoc := self primAt: index) isNil
        ifTrue: [
            self primAt: index put: (Association key: key value: value).
            self incrementTally
        ]
        ifFalse: [ assoc value: value ].

    ^value
!

at: key
    "Answer the value associated to the given key. Fail if the key
     is not found"
    ^self at: key ifAbsent: [ ^self error: 'key not found' ]
!

at: key ifAbsent: aBlock
    "Answer the value associated to the given key, or the result of evaluating
     aBlock if the key is not found"
    | index |
    index := self findIndex: key ifAbsent: [^aBlock value ].
    ^(self primAt: index) value
!
    
at: aKey ifAbsentPut: aBlock
    "Answer the value associated to the given key. If the key is not found,
     evaluate aBlock and associate the result to aKey before returning."
    ^self at: aKey ifAbsent: [ self at: aKey put: aBlock value ]
!

at: aKey ifPresent: aBlock
    "If aKey is absent, answer nil. Else, evaluate aBlock passing the
     associated value and answer the result of the invocation"

    | index |
    index := self findIndex: aKey ifAbsent: [^nil ].
    ^aBlock value: (self primAt: index) value
!

associationAt: key
    "Answer the key/value Association for the given key. Fail if the key
     is not found"
    ^self associationAt: key ifAbsent: [ ^self error: 'key not found' ]
!

associationAt: key ifAbsent: aBlock
    "Answer the key/value Association for the given key. Evaluate aBlock
     (answering the result) if the key is not found"
    | index |
    index := self findIndex: key ifAbsent: [ ^aBlock value ].
    ^self primAt: index
!

keyAtValue: value ifAbsent: exceptionBlock
    "Answer the key associated to the given value. Evaluate exceptionBlock
     (answering the result) if the value is not found.
     IMPORTANT: == is used to compare values"
    self keysAndValuesDo: [:key :val |
        value == val ifTrue: [^key]
    ].
    ^exceptionBlock value
!

keyAtValue: value
    "Answer the key associated to the given value. Evaluate exceptionBlock
     (answering the result) if the value is not found"
    ^self keyAtValue: value ifAbsent: [ ^self error: 'value not found' ]
!

keys
    "Answer a kind of Set containing the keys of the receiver"
    | aSet |
    aSet := self keysClass new: tally * 4 // 3.
    self keysAndValuesDo: [ :key :value | aSet add: key ].
    ^aSet
!

values
    "Answer a Bag containing the values of the receiver"
    | aBag |
    aBag := Bag new.
    self keysAndValuesDo: [ :key :value | aBag add: value ].
    ^aBag
!!



!Dictionary methodsFor: 'dictionary testing'!

includesAssociation: anAssociation
    "Answer whether the receiver contains the key which is
     anAssociation's key and its value is anAssociation's value"
    | value |
    value := self at: anAssociation key ifAbsent: [ ^false ].
    ^value = anAssociation value
!

includesKey: key
    "Answer whether the receiver contains the given key"

    ^super includes: key
!

includes: anObject
    "Answer whether the receiver contains anObject as
     one of its values"
    self do: [ :element | element = anObject ifTrue: [ ^true ] ].
    ^false
!

occurrencesOf: aValue
    "Answer whether the number of occurrences of aValue as
     one of the receiver's values"
    | count |
    count := 0.
    self do: [ :element | element = aValue
    			    ifTrue: [ count := count + 1] ].
    ^count
!!



!Dictionary methodsFor: 'dictionary removing'!

removeAllKeys: keys
    "Remove all the keys in keys, without raising any errors"
    keys do: [ :key |
        self removeKey: key ifAbsent: [ ] ]
!

removeAllKeys: keys ifAbsent: aBlock
    "Remove all the keys in keys, passing the missing keys as parameters
     to aBlock as they're encountered"
    keys do: [ :key |
        self removeKey: key ifAbsent: [ aBlock value: key ] ]
!

removeAssociation: anAssociation
    "Remove anAssociation's key from the dictionary"
    "### does this check the value as well as the key?"
    self removeKey: anAssociation key ifAbsent: [].
    ^anAssociation
!

removeKey: key
    "Remove the passed key from the dictionary, fail if it is not found"
    ^self removeKey: key ifAbsent: [ ^self error: 'key not found' ]
!

removeKey: key ifAbsent: aBlock
    "Remove the passed key from the dictionary, answer the result of
     evaluating aBlock if it is not found"
    | index assoc |
    index := self findIndex: key ifAbsent: [ ^aBlock value ].
    assoc := self primAt: index.
    self primAt: index put: nil.
    self decrementTally.
    self rehashObjectsAfter: index.
    ^assoc value
!

remove: anObject
    self shouldNotImplement
!

remove: anObject ifAbsent: aBlock
    self shouldNotImplement
! !



!Dictionary methodsFor: 'dictionary enumerating'!

associationsDo: aBlock
    "Pass each association in the dictionary to aBlock"
    super do: [ :assoc | aBlock value: assoc ]
!

keysDo: aBlock
    "Pass each key in the dictionary to aBlock"
    self keysAndValuesDo: [ :key :val | aBlock value: key ]
!

do: aBlock
    "Pass each value in the dictionary to aBlock"
    self keysAndValuesDo: [ :key :val | aBlock value: val ]
!

keysAndValuesDo: aBlock
    "Pass each key/value pair in the dictionary as two distinct parameters
     to aBlock"
    super do: [ :assoc | aBlock value: assoc key value: assoc value ]
!

collect: aBlock
    "Answer a new dictionary where the keys are the same and the values are
     obtained by passing each value to aBlock and collecting the return values"
    | aDictionary |
    aDictionary := self copyEmpty: self basicSize.
    self keysAndValuesDo: [ :key :value |
        aDictionary at: key put: (aBlock value: value) ].
    ^aDictionary
!

select: aBlock
    "Answer a new dictionary containing the key/value pairs for which aBlock
     returns true. aBlock only receives the value part of the pairs."
    | newDict |
    newDict := self copyEmpty: self basicSize.
    self associationsDo:
    	[ :assoc | (aBlock value: assoc value)
		     ifTrue: [ newDict add: assoc ] ].
    ^newDict
!

reject: aBlock
    "Answer a new dictionary containing the key/value pairs for which aBlock
     returns false. aBlock only receives the value part of the pairs."
    | newDict |
    newDict := self copyEmpty: self basicSize.
    self associationsDo:
    	[ :assoc | (aBlock value: assoc value)
                     ifFalse: [ newDict add: assoc ] ].
    ^newDict
!!



!Dictionary methodsFor: 'testing'!

= aDictionary
    "Answer whether the receiver and aDictionary are equal"
    self class == aDictionary class ifFalse: [ ^false ].
    self == aDictionary ifTrue: [ ^true ].
    tally = aDictionary size ifFalse: [ ^false ].
    self keysAndValuesDo: [:key :val |
        val = (aDictionary at: key ifAbsent: [ ^false ])
            ifFalse: [ ^false ]
    ].
    ^true
!

hash
    "Answer the hash value for the receiver"
    | hashValue |
    hashValue := tally.
    self keysAndValuesDo: [:key :val |
        hashValue := hashValue bitXor: (self hashFor: key).

        "hack needed because the Smalltalk dictionary contains itself"
        val == self ifFalse: [
            hashValue := hashValue bitXor: val hash.
        ]
    ].

    ^hashValue
! !



!Dictionary methodsFor: 'printing'!

printOn: aStream
    "Print a representation of the receiver on aStream"
    aStream nextPutAll: self classNameString , ' (' ; nl.
    self keysAndValuesDo:
    	[ :key :value | aStream tab;
		   print: key;
		   nextPutAll: '->';
		   print: value;
		   nl ].
    aStream nextPut: $)
! !



!Dictionary methodsFor: 'storing'!

storeOn: aStream
    "Print Smalltalk code compiling to the receiver on aStream"
    | hasElements |
    aStream
        nextPutAll: '(', self classNameString, ' new: ';
        print: self size;
        nextPut: $).

    hasElements := false.
    self associationsDo:
    	[ :assoc | aStream nextPutAll: ' at: ';
		       store: assoc key;
		       nextPutAll: ' put: ';
		       store: assoc value;
		       nextPut: $;.
		   hasElements := true ].
    hasElements ifTrue: [ aStream nextPutAll: ' yourself' ].
    aStream nextPut: $)
! !



!Dictionary methodsFor: 'private methods'!

addWhileGrowing: association
    "Private - Add the newObject association to the receiver. Don't check for
     the dictionary to be full - we want SPEED! - nor copy the association -
     that would break existing CompiledMethods."
    self
        primAt: (self findIndex: association key)
        put: association.
    tally := tally + 1.
    ^association
!

rehashObjectsAfter: index
    "Private - Rehashes all the objects in the collection after index to
    see if any of them hash to index.  If so, that object is copied to
    index, and the process repeats with that object's index, until a nil
    is encountered."
    | i j size assoc |
    i := index.
    size := self basicSize.
    [   i = size ifTrue: [ i := 1 ] ifFalse: [ i := i + 1 ].
        assoc := self primAt: i.
        assoc notNil
    ]   whileTrue: [
        j := self findIndex: assoc key.
        (self primAt: j) isNil ifTrue: [
            self primAt: j put: assoc.
            self primAt: i put: nil
        ]
    ]
!

keysClass
    "Private - Answer the class answered by #keys"
    ^Set
!

hashFor: anElement
    "Private - Answer the hash value for anElement"

    ^anElement hash
!

is: anElement sameAs: searchedObject
    "Private - Answer whether findIndex: should stop scanning the receiver:
     anElement has been found and findIndex:'s parameter was searchedObject"

    ^anElement key = searchedObject
!

growBy: delta
    "Private - Grow by the receiver by delta places"

    | newDict |
    newDict := self copyEmpty: self basicSize + delta.
    self associationsDo: [ :assoc | newDict addWhileGrowing: assoc ].
    ^self become: newDict
! !


!Dictionary methodsFor: 'awful ST-80 compatibility hacks'!

findKeyIndex: key
    "Tries to see if key exists as a the key of an indexed variable. As soon
    as nil or an association with the correct key is found, the index of that
    slot is answered"
    ^self findIndex: key
!!
