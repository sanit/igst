"======================================================================
|
|   SequenceableCollection Method Definitions
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


Collection subclass: #SequenceableCollection
	   instanceVariableNames: ''
	   classVariableNames: ''
	   poolDictionaries: ''
	   category: 'Collections-Sequenceable'
!

SequenceableCollection comment: 
'My instances represent collections of objects that are ordered.  I provide
some access and manipulation methods.' !



!SequenceableCollection methodsFor: 'testing'!

inspect
    "Print all the instance variables and context of the receiver on the
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
    self doWithIndex: [ :obj :i |
        Transcript
            nextPutAll: '    [';
            print: i;
            nextPutAll: ']: ';
            print: obj;
            nl
    ].
    Transcript nextPutAll: '  ]'; nl
!


= aCollection
    "Answer whether the receiver's items match those in aCollection"

    self class == aCollection class ifFalse: [^false].
    self size = aCollection size ifFalse: [^false].

    self with: aCollection do: [ :selfElement :argElement |
        selfElement = argElement ifFalse: [^false]
    ].
    ^true
!

hash
    "Answer an hash value for the receiver"

    "Don't like this hash function; it can be made much better"
    | hash carry |
    hash := self size.
    self do: [ :element |
        carry := (hash bitAnd: 16r20000000) > 0.
        hash := hash bitAnd: 16r1FFFFFFF.
        hash := hash bitShift: 1.
        carry ifTrue: [ hash := hash bitOr: 1 ].

        hash := hash bitXor: element hash
    ].
    ^hash
!!



!SequenceableCollection methodsFor: 'basic'!

atAll: aCollection put: anObject
    "Put anObject at every index contained in aCollection"
    aCollection do: [ :index | self at: index put: anObject ]
!

atAllPut: anObject
    "Put anObject at every index in the receiver"
    1 to: self size do: [ :i | self at: i put: anObject ]
!

after: oldObject
    "Return the element after oldObject.  Error if oldObject not found or
    if no following object is available"

    self doWithIndex:
        [ :element :index |
            element = oldObject
                ifTrue: [ ^self at: index + 1 ].
        ].

    ^self error: 'object not found'
!

before: oldObject
    "Return the element before oldObject.  Error if oldObject not found or
    if no preceding object is available"

    self doWithIndex:
        [ :element :index |
            element = oldObject
                ifTrue: [ ^self at: index - 1 ].
        ].

    ^self error: 'object not found'
!

first
    "Answer the first item in the receiver"
    ^self at: 1
!

last
    "Answer the last item in the receiver"
    self size < 1 ifTrue: [ ^self error: 'last not defined with no elements' ].
    ^self at: self size
!

indexOfSubCollection: aSubCollection startingAt: anIndex ifAbsent: exceptionBlock
    "Answer the first index > anIndex at which starts a sequence of
     items matching aSubCollection.
     Invoke exceptionBlock and answer its result if no such sequence is found"

    | selfSize subSize |
    subSize  := aSubCollection size.
    selfSize := self size.
    anIndex + subSize - 1 <= selfSize ifTrue: [
	anIndex to: selfSize - subSize + 1 do: [ :index |
	    (self at: index) = (aSubCollection at: 1) ifTrue: [
	        (self matchSubCollection: aSubCollection startingAt: index)
                    ifTrue: [^index]
            ]
	]
    ].
    ^exceptionBlock value
!

indexOfSubCollection: aSubCollection startingAt: anIndex
    "Answer the first index > anIndex at which starts a sequence of
     items matching aSubCollection. Answer 0 if no such sequence is found."

    ^self indexOfSubCollection: aSubCollection startingAt: anIndex
    	ifAbsent: [ ^0 ]
!


indexOf: anElement startingAt: anIndex ifAbsent: exceptionBlock
    "Answer the first index > anIndex which contains anElement.
     Invoke exceptionBlock and answer its result if no item is found"
    anIndex to: self size do: [ :index |
        (self at: index) = anElement ifTrue: [ ^index ].
    ].
    ^exceptionBlock value
!

indexOfSubCollection: aSubCollection
    "Answer the first index > anIndex at which starts a sequence of
     items matching aSubCollection. Answer 0 if no such sequence is found."

    ^self indexOfSubCollection: aSubCollection startingAt: 1
    	ifAbsent: [ ^0 ]
!

indexOfSubCollection: aSubCollection ifAbsent: exceptionBlock
    "Answer the first index > anIndex at which starts a sequence of
     items matching aSubCollection. Answer 0 if no such sequence is found."

    ^self indexOfSubCollection: aSubCollection startingAt: 1
    	ifAbsent: exceptionBlock
!



indexOf: anElement startingAt: anIndex
    "Answer the first index > anIndex which contains anElement.
     Answer 0 if no item is found"
    ^self indexOf: anElement startingAt: anIndex ifAbsent: [ ^0 ]
!

indexOf: anElement ifAbsent: exceptionBlock
    "Answer the index of the first occurrence of anElement in the receiver.
     Invoke exceptionBlock and answer its result if no item is found"
    ^self indexOf: anElement startingAt: 1 ifAbsent: exceptionBlock
!

indexOf: anElement
    "Answer the index of the first occurrence of anElement in the receiver.
     Answer 0 if no item is found"
    ^self indexOf: anElement startingAt: 1 ifAbsent: [ ^0 ]
!!


!SequenceableCollection methodsFor: 'replacing items'!

replaceFrom: start to: stop with: replacementCollection startingAt: repStart
    "Replace the items from start to stop with replacementCollection's items
     from repStart to repStart+stop-start"

    "speed this up by making it zero based, otherwise we have to subtract 1
     from each use of index.  Note that stop - start is not computed on
     every iteration."

    0 to: stop - start do: [ :i |
        self at: (start + i)
    	     put: (replacementCollection at: (repStart + i))
    ]
!

replaceFrom: start to: stop with: replacementCollection
    "Replace the items from start to stop with replacementCollection's items
     from 1 to stop-start+1"

    (stop - start + 1) ~= replacementCollection size
    	ifTrue: [ ^self error: 'replacement range does not equal size of
replacement collection' ].
    self replaceFrom: start to: stop with: replacementCollection startingAt: 1
!

replaceFrom: anIndex to: stopIndex withObject: replacementObject
    "Replace every item from start to stop with replacementObject."

    anIndex to: stopIndex do: [ :index |
        self at: index put: replacementObject.
    ]
!!



!SequenceableCollection methodsFor: 'copying SequenceableCollections'!

copyReplaceFrom: start to: stop withObject: anObject
    "Answer a new collection of the same class as the receiver that contains the
    same elements as the receiver, in the same order, except for elements from
    index `start' to index `stop'.
    
    If start < stop, these are replaced by the single element anObject.
    Instead, If start = (stop + 1), then every element of the receiver
    will be present in the answered copy; the operation will be an append if
    stop is equal to the size of the receiver or, if it is not, an insert before
    index `start'."

    | newSize repSize |
    (stop - start < -1)
        ifTrue: [ self error: 'to insert, stop must be start - 1' ].

    newSize := self size - (stop - start).
    ^(self copyEmpty: newSize)
        replaceFrom: 1 to: start - 1 with: self startingAt: 1;
        at: start put: anObject;
        replaceFrom: start + 1 to: newSize with: self startingAt: stop + 1;
        yourself
!

, aSequenceableCollection
    "Append aSequenceableCollection at the end of the receiver (using #add:),
     and answer a new collection"

    ^(self copyEmpty: self size + aSequenceableCollection size)
        addAll: self;
        addAll: aSequenceableCollection;
        yourself
!

copyFrom: start to: stop
    "Answer a new collection containing all the items in the receiver from the
     start-th and to the stop-th"

    | len |
    len := stop - start + 1.
    ^(self copyEmpty: len)
        replaceFrom: 1 to: len with: self startingAt: start;
        yourself
!

copyReplaceAll: oldSubCollection with: newSubCollection
    "Answer a new collection in which all the sequences matching
     oldSubCollection are replaced with newSubCollection"

    | numOld newCollection sizeDifference newSubSize oldSubSize
      newStart oldStart copySize index |
    numOld := self countSubCollectionOccurrencesOf: oldSubCollection.
    newSubSize := newSubCollection size.
    sizeDifference := newSubSize - oldSubCollection size + 1.
    newCollection := self copyEmpty: (self size - (sizeDifference * numOld)).
    oldStart := newStart := 1.

    [ true ] whileTrue: [
        index := self indexOfSubCollection: oldSubCollection startingAt: oldStart
            ifAbsent: [
                "Copy the remaining part of self onto the tail of the new collection."
                newCollection replaceFrom: newStart
    	    	    to: newCollection size
		    with: self
		    startingAt: oldStart.
                ^newCollection
            ].

        copySize := index - oldStart + 1.
        newCollection replaceFrom: newStart
	    	      to: newStart + copySize - 1
		      with: self
		      startingAt: oldStart.
        newStart := newStart + copySize - 1.
        newCollection replaceFrom: newStart
	    	      to: newStart + newSubSize - 1
		      with: newSubCollection
		      startingAt: 1.

    	oldStart := oldStart + copySize.
    	newStart := newStart + newSubSize
    ].
!

copyReplaceFrom: start to: stop with: replacementCollection
    "Answer a new collection of the same class as the receiver that contains the
    same elements as the receiver, in the same order, except for elements from
    index `start' to index `stop'.
    
    If start < stop, these are replaced by the contents of the
    replacementCollection.  Instead, If start = (stop + 1), like in
    `copyReplaceFrom: 4 to: 3 with: anArray', then every element of the receiver
    will be present in the answered copy; the operation will be an append if
    stop is equal to the size of the receiver or, if it is not, an insert before
    index `start'."

    | newSize repSize |
    (stop - start < -1)
        ifTrue: [ self error: 'to insert, stop must be start - 1' ].

    repSize := replacementCollection size.
    newSize := self size + repSize - (stop - start + 1).
    ^(self copyEmpty: newSize)
        replaceFrom: 1 to: start - 1 with: self startingAt: 1;
        replaceFrom: start to: start + repSize - 1 with: replacementCollection;
        replaceFrom: start + repSize to: newSize with: self startingAt: stop + 1;
        yourself
!!



!SequenceableCollection methodsFor: 'enumerating'!

do: aBlock
    "Evaluate aBlock for all elements in the sequenceable collection"
    1 to: self size do: [ :i | aBlock value: (self at: i) ].
!

doWithIndex: aBlock
    "Evaluate aBlock for all elements in the sequenceable collection, passing
     the index of each element as the second parameter"
    1 to: self size do: [ :i | aBlock value: (self at: i) value: i ].
!

from: startIndex to: stopIndex do: aBlock
    "Evaluate aBlock for all elements in the sequenceable collection whose
     indices are in the range index to stopIndex"

    startIndex to: stopIndex do: [ :i | aBlock value: (self at: i) ].
!

from: startIndex to: stopIndex doWithIndex: aBlock
    "Evaluate aBlock for all elements in the sequenceable collection whose
     indices are in the range index to stopIndex, passing the index of each
     element as the second parameter"

    startIndex to: stopIndex do: [ :i | aBlock value: (self at: i) value: i ].
!

findFirst: aBlock
    "Returns the index of the first element of the sequenceable collection
    for which aBlock returns true, or 0 if none"

    self doWithIndex: [ :each :i |
    	(aBlock value: each) ifTrue: [ ^i ].
    ].
    ^0
!

findLast: aBlock
    "Returns the index of the last element of the sequenceable collection
    for which aBlock returns true, or 0 if none does"

    | i |
    i := self size.
    self reverseDo: [ :each |
    	(aBlock value: each) ifTrue: [ ^i ].
    	i := i - 1
    ].
    ^0
!

readStream
    "Answer a ReadStream streaming on the receiver"
    ^ReadStream on: self
!

readWriteStream
    "Answer a ReadWriteStream which streams on the receiver"
    ^ReadWriteStream on: self
!

reverse
    "Answer the receivers' contents in reverse order"
    | result |
    result := self copyEmpty.
    self reverseDo: [ :each | result add: each ].
    ^result
!

reverseDo: aBlock
    "Evaluate aBlock for all elements in the sequenceable collection, from the
     last to the first."

    self size to: 1 by: -1 do: [ :i | aBlock value: (self at: i) ].
!

with: aSequenceableCollection do: aBlock
    "Evaluate aBlock for each pair of elements took respectively from the re-
     ceiver and from aSequenceableCollection. Fail if the receiver has not
     the same size as aSequenceableCollection."

    self size = aSequenceableCollection size
    	ifFalse:
	    [ ^self error: 'sequenceable collections must have same size' ].

    self doWithIndex: [ :each :i |
    	aBlock value: each value: (aSequenceableCollection at: i).
    ]
!

writeStream
    "Answer a WriteStream streaming on the receiver"
    ^WriteStream on: self
! !



!SequenceableCollection methodsFor: 'private methods'!

swap: anIndex with: anotherIndex
    "Private - Swap the item at index anIndex with the item at index
     another index"

    | saved |
    saved := self basicAt: anIndex.
    self basicAt: anIndex put: (self basicAt: anotherIndex).
    self basicAt: anotherIndex put: saved
!

matchSubCollection: aSubCollection startingAt: anIndex
    "Private - Answer whether the items from index anIndex match those in
     aSubCollection. The first item is ignored"
    | ourIndex |
    ourIndex := anIndex.
    2 to: aSubCollection size do: [ :index |
        ourIndex := ourIndex + 1.
        (self at: ourIndex) = (aSubCollection at: index)
	    ifFalse: [ ^false ].
    ].
    ^true
!

countSubCollectionOccurrencesOf: aSubCollection
    | colIndex subColIndex count |
    colIndex := 1.
    count := 0.
    [ subColIndex := self indexOfSubCollection: aSubCollection
    	    	    	 startingAt: colIndex.
      subColIndex > 0 ] whileTrue:
      	[ count := count + 1.
	  colIndex := colIndex + aSubCollection size ].
    ^count
!

growSize
    ^(self size bitShift: -1) bitOr: 1		"a randomly chosen factor"
!!

