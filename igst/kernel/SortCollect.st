"======================================================================
|
|   SortedCollection Method Definitions
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


OrderedCollection variableSubclass: #SortedCollection
		  instanceVariableNames: 'sortBlock'
		  classVariableNames: ''
		  poolDictionaries: ''
		  category: 'Collections-Sequenceable'
!

SortedCollection comment:
'I am a collection of objects, stored and accessed according to some
sorting criteria.  I store things using a bubble sort.  My instances have a
comparison block associated with them; this block takes two arguments and
is a predicate which returns true if the first argument should be sorted
earlier than the second.  The default block is [ :a :b | a <= b ], but I
will accept any block that conforms to the above criteria.' !


!SortedCollection class methodsFor: 'hacking'!

defaultSortBlock
    "Answer a default sort block for the receiver."
    ^[ :a :b | a <= b ]
! !


!SortedCollection class methodsFor: 'instance creation'!

new
    "Answer a new collection with a default size and sort block"
    ^self sortBlock: self defaultSortBlock
!

new: aSize
    "Answer a new collection with a default sort block and the given size"
    ^(super new: aSize) setSortBlock: self defaultSortBlock
!

sortBlock: aSortBlock
    "Answer a new collection with a default size and the given sort block"
    ^super new setSortBlock: aSortBlock

!!



!SortedCollection methodsFor: 'basic'!

addFirst: anObject
    self shouldNotImplement
!

addLast: anObject
    self shouldNotImplement
!

at: index put: anObject
    self shouldNotImplement
!

add: anObject afterIndex: i
    self shouldNotImplement
!

addAll: aCollection afterIndex: i
    self shouldNotImplement
!

addAllFirst: aCollection
    self shouldNotImplement
!

addAllLast: aCollection
    self shouldNotImplement
!

add: anObject
    "Add anObject into the collection at the proper place"

    | element |
    "Allocate a slot in the ordered collection"
    self basicAddLast: nil.

    "That -1 skips the last element we added"
    lastIndex - 1 to: firstIndex by: -1 do: [ :index |
         element := self basicAt: index.
         (sortBlock value: element value: anObject)
               ifTrue: [ ^self basicAt: index + 1 put: anObject ]
               ifFalse: [ self basicAt: index + 1 put: element ]
    ].
    ^self basicAt: firstIndex put: anObject
!

addAll: aCollection
    "Add all the elements in aCollection to the receiver in their proper
     places"

    | i add |

    i := lastIndex.
    add := aCollection size.

    "Make space into the collection"
    self basicAddAllLast: aCollection.

    "Merge elements into the collection"
    (aCollection asSortedCollection: self sortBlock) reverseDo: [:element |
        i := self moveElementsFrom: i by: add toMerge: element.
        self basicAt: i + add put: element.
        add := add - 1
    ].
    ^aCollection
!

reverse
    "Answer an OrderedCollection containing the data in the
    receiver in reverse ordering"

    | newOrderedCollection |
    newOrderedCollection := OrderedCollection new: self size.
    self reverseDo: [ :element | newOrderedCollection add: element ].
    ^newOrderedCollection
!!


!SortedCollection methodsFor: 'instance protocol'!

sortBlock
    "Answer the receiver's sort criteria"
    ^sortBlock
!

sortBlock: aSortBlock
    "Change the sort criteria for a sorted collection, resort the elements of 
    the collection, and return it."

    | adjust |
    self size <= 2 ifFalse: [
        sortBlock := aSortBlock.
        self sortFrom: firstIndex to: lastIndex.
    ].
    ^self
!!


!SortedCollection methodsFor: 'searching'!

indexOf: anObject startingAt: index ifAbsent: aBlock
    "Answer the first index > anIndex which contains anElement.
     Invoke exceptionBlock and answer its result if no item is found"

    | i j |

    (index < 1) | (index > self size) ifTrue: [ ^self error: 'Index out of range' ].
    i := self
        binarySearch: anObject
        low: index + firstIndex - 1
        high: lastIndex
        ifAbsent: [ ^aBlock value].

    j := i - firstIndex + 1.
    [ j ~= index and: [ (self basicAt: i - 1) = anObject ] ]
        whileTrue: [ i := i - 1. j := j - 1 ].

    ^j
!

includes: anObject
    "Private - Answer whether the receiver includes an item which is
     equal to anObject"
    self indexOf: anObject ifAbsent: [ ^false ].
    ^true
!

occurrencesOf: anObject
    "Answer how many occurrences of anObject can be found in the receiver"

    "Find first the index of 'anObject' and then look at the both sides to
     count repetitions of 'anObject', if there are."

    | upper lower max count |

    upper := self indexOf: anObject ifAbsent: [ ^0 ].
    lower := upper - 1.
    max := self size.

    [ lower > 1 and: [ (self at: lower) = anObject ] ]
        whileTrue: [ lower := lower - 1 ].

    [ upper < max and: [ (self at: upper) = anObject ] ]
        whileTrue: [ upper := upper + 1 ].

    ^upper - lower
!!

!SortedCollection methodsFor: 'enumerating'!

collect: aBlock
    "Answer an OrderedCollection containing the data in the receiver filtered
     through the given block"
    | newOrderedCollection |
    newOrderedCollection := OrderedCollection new: self size.
    self do: [ :element | newOrderedCollection add: (aBlock value: element) ].
    ^newOrderedCollection
!!



!SortedCollection methodsFor: 'copying'!

copyEmpty: newSize
    "Answer an empty copy of the receiver, with the same sort block as the
     receiver"
    ^(super copyEmpty: newSize) setSortBlock: sortBlock
!!



!SortedCollection methodsFor: 'private methods'!

median: ia median: ib median: ic
    "Private - Calculate the middle of a, b and c. Needed for selecting
     the quicksort's pivot item"
    | a b c |
    a := self basicAt: ia.
    b := self basicAt: ib.
    c := self basicAt: ic.
    (sortBlock value: a value: b)
        ifTrue: [
            (sortBlock value: b value: c) ifTrue: [ ^ib ].
            (sortBlock value: a value: c) ifTrue: [ ^ic ] ifFalse: [ ^ia ]
        ]
        ifFalse: [
            (sortBlock value: a value: c) ifTrue: [ ^ia ].
            (sortBlock value: b value: c) ifTrue: [ ^ic ] ifFalse: [ ^ib ]
        ]
!

sortFrom: first to: last
    "Private - Perform a quicksort on the indexed variables
     from the first-th to the last-th (using basicAt: indices!). Recursive."
    | pivot mid smaller larger |

    last - first < 2 ifTrue: [
	last > first ifTrue: [
	    (self sortBlock value: (self basicAt: last) value: (self basicAt: first))
		ifTrue: [ self swap: first with: last ].
        ].
        ^self
    ].

    pivot := self
        median: first
        median: last
        median: (mid := (first + last) // 2).

    pivot = mid ifFalse: [ self swap: pivot with: mid ].

    pivot := self basicAt: pivot.
    smaller := first.
    larger := last.
    [ 
         [ (smaller <= last) and:
           [ sortBlock value: (self basicAt: smaller) value: pivot ] ]
             whileTrue: [ smaller := smaller + 1 ].

         [ (larger >= first) and:
           [ sortBlock value: pivot value: (self basicAt: larger) ] ]
             whileTrue: [ larger := larger - 1 ].

         smaller < larger ifTrue: [
             self swap: smaller with: larger.
             smaller := smaller + 1.
             larger := larger - 1.
         ].
         smaller < larger
    ]    whileTrue: [ ].

    "((last = larger) & (first < larger)) |
    ((first = smaller) & (smaller < last)) ifTrue: [
        (first to: last) do: [ :each | (self basicAt: each) printNl ].
        ^self halt ]."

    first < larger ifTrue: [ self sortFrom: first to: larger ].
    smaller < last ifTrue: [ self sortFrom: smaller to: last ]
!

binarySearch: anObject low: low high: high ifAbsent: aBlock
    "Private - Perform a binary search on the receiver, searching between
     indexes i and j (indexes are referenced with #basicAt:). If anObject
     is not found, answer the result of evaluating aBlock, else answer one
     of the indices containing anObject"

    | i j mid element |
    i := low.
    j := high.

    [ i > j ] whileFalse: [
        mid := (i + j + 1) // 2.
        element := self basicAt: mid.
        element = anObject ifTrue: [ ^mid ].
        (sortBlock value: anObject value: element)
            ifTrue: [ j := mid - 1]
            ifFalse: [ i := mid + 1 ].
    ].
    ^aBlock value
!

moveElementsFrom: i by: add toMerge: element
    "Private - Move by add spaces all the elements before the i-th until
     we have created a place to insert element. Answer the new i (element will
     be inserted at position i + add)."

    | anObject |

    i to: firstIndex by: -1 do: [ :index |
         anObject := self basicAt: index.
         (sortBlock value: anObject value: element)
               ifTrue: [ ^index ]
               ifFalse: [ self basicAt: index + add put: anObject ]
    ].
    ^firstIndex - 1
!

setSortBlock: aSortBlock
    sortBlock := aSortBlock
!!
