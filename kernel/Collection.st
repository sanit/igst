"======================================================================
|
|   Collection Method Definitions
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



Object subclass: #Collection
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Collections'
!

Collection comment: 
'I am an abstract class.  My instances are collections of objects.  My
subclasses may place some restrictions or add some definitions to how
the objects are stored or organized; I say nothing about this.  I merely
provide some object creation and access routines for general collections
of objects.' !



!Collection class methodsFor: 'instance creation'!

withAll: aCollection
    "Answer a collection whose elements are all those in aCollection"
    ^self new addAll: aCollection; yourself
!

with: anObject
    "Answer a collection whose only element is anObject"
    ^self new add: anObject; yourself
!

with: firstObject with: secondObject
    "Answer a collection whose only elements are the parameters in the order
     they were passed"
    ^self new add: firstObject; add: secondObject; yourself
!

with: firstObject with: secondObject with: thirdObject
    "Answer a collection whose only elements are the parameters in the order
     they were passed"
    ^self new add: firstObject; add: secondObject; add: thirdObject; yourself
!

with: firstObject with: secondObject with: thirdObject with: fourthObject
    "Answer a collection whose only elements are the parameters in the order
     they were passed"
    ^self new add: firstObject; add: secondObject; add: thirdObject;
    	add: fourthObject; yourself
! !



!Collection methodsFor: 'Adding to a collection'!

add: newObject
    "Add newObject to the receiver, answer it"
    self subclassResponsibility
!

addAll: aCollection
    "Adds all the elements of 'aCollection' to the receiver, answer
     aCollection"
    aCollection do: [ :element | self add: element ].
    ^aCollection
! !



!Collection methodsFor: 'Removing from a collection'!

remove: oldObject ifAbsent: anExceptionBlock
    "Remove oldObject from the receiver. If absent, evaluate anExceptionBlock
     and answer the result, else answer oldObject."

    self subclassResponsibility
!

remove: oldObject
    "Remove oldObject from the receiver. If absent, fail, else
     answer oldObject."

    self remove: oldObject
         ifAbsent: [ ^self error: 'Object not found' ].
    ^oldObject
!

removeAll: aCollection
    "Remove each object in aCollection, answer aCollection, fail if some
     of them is absent. Warning: this could leave the collection in a
     semi-updated state."

    aCollection do: [ :element | self remove: element ].
    ^aCollection
!

removeAll: aCollection ifAbsent: aBlock
    "Remove each object in aCollection, answer aCollection; if some
     element is absent, pass it to aBlock."

    aCollection do: [ :element |
        self remove: element ifAbsent: [ aBlock value: element ]
    ].
    ^aCollection
!!



!Collection methodsFor: 'testing collections'!

capacity
    "Answer how many elements the receiver can hold before having to grow."
    ^self basicSize
!

includes: anObject
    "Answer whether we include anObject"
    self do: [ :element | anObject = element ifTrue: [ ^true ] ].
    ^false
!

isEmpty
    "Answer whether we are (still) empty"
    ^self size == 0
!

notEmpty
    "Answer whether we include at least one object"
    ^self size > 0
!

occurrencesOf: anObject
    "Answer how many occurrences of anObject we include"
    | count |
    count := 0.
    self do: [ :element | anObject == element ifTrue: [ count := count + 1 ] ].
    ^count
!

size
    "Answer how many objects we include"
    | count |
    count := 0.
    self do: [ :element | count := count + 1].
    ^count
! !



!Collection methodsFor: 'enumerating the elements of a collection'!

anyOne
    "Answer an unspecified element of the collection. Example usage:
        ^coll inject: coll anyOne into: [ :max :each |
            max max: each ]

     to be used when you don't have a valid lowest-possible-value
     (which happens in common cases too, such as with arbitrary numbers"

    self do: [ :each | ^each ]
!

do: aBlock
    "Enumerate each object of the receiver, passing them to aBlock"
    self subclassResponsibility
!

do: aBlock separatedBy: separatorBlock
    "Enumerate each object of the receiver, passing them to aBlock.
     Between every two invocations of aBlock, invoke separatorBlock"
    | first |
    first := true.
    self do: [ :each |
        aBlock value: each.
        first
            ifTrue: [ first := false ]
            ifFalse: [ separatorBlock value ]
    ]
!

select: aBlock
    "Answer a new instance of a Collection containing all the elements
     in the receiver which, when passed to aBlock, answer true"

    | newCollection |
    newCollection := self copyEmpty.
    self do: [ :element | (aBlock value: element)
    	    	    	    ifTrue: [ newCollection add: element ]
	     ].
    ^newCollection
!

reject: aBlock
    "Answer a new instance of a Collection containing all the elements
     in the receiver which, when passed to aBlock, don't answer true"

    | newCollection |
    newCollection := self copyEmpty.
    self do: [ :element | (aBlock value: element)
    	    	    	    ifFalse: [ newCollection add: element ]
	     ].
    ^newCollection
!

collect: aBlock
    "Answer a new instance of a Collection containing all the results
     of evaluating aBlock passing each of the receiver's elements"

    | newCollection |
    newCollection := self copyEmpty.
    self do: [ :element | newCollection add: (aBlock value: element) ].
    ^newCollection
!

detect: aBlock ifNone: exceptionBlock
    "Search the receiver for an element for which aBlock returns true.
     If some does, answer it. If none does, answer the result of evaluating
     aBlock"

    self do: [ :element | (aBlock value: element) ifTrue: [ ^element ] ].
    ^exceptionBlock value    
!
    
allSatisfy: aBlock
    "Search the receiver for an element for which aBlock returns false.
     Answer true if none does, false otherwise."
    self do: [ :element | (aBlock value: element) ifFalse: [ ^false ] ].
    ^true
!

anySatisfy: aBlock
    "Search the receiver for an element for which aBlock returns true.
     Answer true if some does, false otherwise."
    self do: [ :element | (aBlock value: element) ifTrue: [ ^true ] ].
    ^false
!

conform: aBlock
    "Search the receiver for an element for which aBlock returns false.
     Answer true if none does, false otherwise."
    self do: [ :element | (aBlock value: element) ifFalse: [ ^false ] ].
    ^true
!

contains: aBlock
    "Search the receiver for an element for which aBlock returns true.
     Answer true if some does, false otherwise."
    self do: [ :element | (aBlock value: element) ifTrue: [ ^true ] ].
    ^false
!

detect: aBlock
    "Search the receiver for an element for which aBlock returns true.
     If some does, answer it. If none does, fail"
    ^self detect: aBlock
          ifNone: [ ^self error: 'No item satisfies detect: condition']
!

inject: thisValue into: binaryBlock
    "Pass to binaryBlock receiver thisValue and the first element of the
     receiver; for each subsequent element, pass the result of the previous
     evaluation and an element. Answer the result of the last invocation."

    | result |
    result := thisValue.
    self do: [ :element |
        result := binaryBlock value: result value: element
    ].
    ^result
! !



!Collection methodsFor: 'converting'!

asArray
    "Answer an Array containing all the elements in the receiver"

    | anArray index |
    anArray := Array new: self size.
    index := 1.
    self do: [ :element | anArray at: index put: element. index := index + 1 ].
    ^anArray
!

asByteArray
    "Answer a ByteArray containing all the elements in the receiver"

    | anArray index |
    anArray := ByteArray new: self size.
    index := 1.
    self do: [ :element | anArray at: index put: element. index := index + 1 ].
    ^anArray
!

asBag
    "Answer a Bag containing all the elements in the receiver"

    | aBag |
    aBag := Bag new.
    self do: [ :element | aBag add: element ].
    ^aBag
!

asSet
    "Answer a Set containing all the elements in the receiver with no
     duplicates"

    | aSet |
    aSet := Set new: self size * 2.
    self do: [ :element | aSet add: element ].
    ^aSet
!

asOrderedCollection
    "Answer an OrderedCollection containing all the elements in the receiver"
    | anOrderedCollection |
    anOrderedCollection := OrderedCollection new: self size * 2.
    self do: [ :element | anOrderedCollection add: element ].
    ^anOrderedCollection
!

asSortedCollection
    "Answer a SortedCollection containing all the elements in the receiver
     with the default sort block - [ :a :b | a <= b ]"

    ^self asSortedCollection: SortedCollection defaultSortBlock
!

asSortedCollection: aBlock
    "Answer a SortedCollection whose elements are the elements of the receiver,
     sorted according to the sort block aBlock"

    | sorted |
    (sorted := SortedCollection basicNew: self size + 10)
        setSortBlock: aBlock;
        firstIndex: 5 lastIndex: 4.

    self do: [ :elem | sorted basicAddLast: elem ].
    self size < 2 ifFalse: [ sorted sortFrom: 5 to: 4 + self size ].
    ^sorted
!!



!Collection methodsFor: 'copying Collections'!

copyReplacing: targetObject withObject: newObject
    "Copy replacing each object which is = to targetObject
     with newObject"

    ^self collect: [:each |
        each = targetObject ifFalse: [each] ifTrue: [newObject]
    ]
!

copyWith: newElement
    "Answer a copy of the receiver to which newElement is added"

    ^(self copyEmpty: self basicSize + 1)
       addAll: self;
       add: newElement;
       yourself
!

copyWithout: oldElement
    "Answer a copy of the receiver to which all occurrences of oldElement are
     removed"

    ^self reject: [ :element | element = oldElement ].
!!



!Collection methodsFor: 'private'!

copyEmpty
    "Private - Answer an empty copy of the receiver"
    ^self copyEmpty: self basicSize
!

copyEmpty: newSize
    "Private - Answer an empty copy of the receiver"
    ^self species new: newSize
!

rehash
    "Private - Do nothing, present for consistency in protocol"
! !
    


!Collection methodsFor: 'printing'!

inspect
    "Print all the instance variables and objects in the receiver on the
     Transcript"

    | class instVars i |
    class := self class.
    instVars := class allInstVarNames.
    Transcript nextPutAll: 'An instance of '.
    class printNl.
    1 to: instVars size do:
    	[ :i | Transcript nextPutAll: '  ';
		   nextPutAll: (instVars at: i);
		   nextPutAll: ': '.
	       (self instVarAt: i)  printNl ].

    Transcript nextPutAll: '  contents: ['; nl.
    self do: [ :obj |
        Transcript 
           nextPutAll: '    ';
           print: obj;
           nl
    ].
    Transcript nextPutAll: '  ]'; nl
!

printOn: aStream
    "Print a representation of the receiver on aStream"
    aStream nextPutAll: self classNameString.
    aStream nextPutAll: ' ('.
    self do:
    	[ :element | element printOn: aStream.
		     aStream space ].
    aStream nextPut: $)
! !



!Collection methodsFor: 'storing'!

storeOn: aStream
    "Store Smalltalk code compiling to the receiver on aStream"
    | noElements |
    aStream nextPut: $(.
    aStream nextPutAll: self classNameString.
    aStream nextPutAll: ' new'.
    noElements := true.
    self do:
    	[ :element | aStream nextPutAll: ' add: '.
    	    	     element storeOn: aStream.
		     aStream nextPut: $;.
		     noElements := false ].
    noElements ifFalse: [ aStream nextPutAll: ' yourself' ].
    aStream nextPut: $)
! !

