"======================================================================
|
|   MappedCollection Method Definitions
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


Collection subclass: #MappedCollection
	   instanceVariableNames: 'domain map'
	   classVariableNames: ''
	   poolDictionaries: ''
	   category: 'Collections-Keyed'
!

MappedCollection comment: 
'I represent collections of objects that are indirectly indexed by names.
There are really two collections involved: domain and a map.  The map maps
between external names and indices into domain, which contains the
real association.  In order to work properly, the domain must be an instance
of a subclass of SequenceableCollection, and the map must be an instance of
Dictionary, or of a subclass of SequenceableCollection.

As an example of using me, consider implenting a Dictionary whose elements
are indexed. The domain would be a SequenceableCollection with n elements,
the map a Dictionary associating each key to an index in the domain. To
access by key, to perform enumeration, etc. you would ask an instance of me;
to access by index, you would access the domain directly.
' !


!MappedCollection class methodsFor: 'instance creation'!

collection: aCollection map: aMap
    "Answer a new MappedCollection using the given domain (aCollection)
     and map"
    ^self new setCollection: aCollection andMap: aMap
!

new
    "self shouldNotImplement"
    ^self error: 'new not available for MappedCollections; use collection:map:'
!!



!MappedCollection methodsFor: 'basic'!

at: key
    "Answer the object at the given key"
    ^domain at: (map at: key)
!

at: key put: value
    "Store value at the given key"
    ^domain at: (map at: key) put: value
!

size
    "Answer the receiver's size"
    ^domain size
!

add: anObject
    self shouldNotImplement
!

contents
    "Answer a bag with the receiver's values"
    | aBag |
    aBag := Bag new.
    map do: [ :value | aBag add: (domain at: value) ].
    ^aBag
!

do: aBlock
    "Evaluate aBlock for each object"
    map do: [ :value | aBlock value: (domain at: value) ]
!

domain
    "Answer the domain"
    ^domain
!

map
    "Answer the map"
    ^map
!

collect: aBlock
    "Answer a MappedCollection with a copy of the receiver's map
     and a domain obtained by passing each object through aBlock"
    | newDomain |
    newDomain := domain collect: aBlock.
    ^self species collection: newDomain map: map copy.
!

reject: aBlock
    "Answer the objects in the domain for which aBlock returns false"

    | aStream map |
    aStream := WriteStream on: (domain species new: self size).
    self do: [ :value | (aBlock value: value) ifFalse:
    	    	[ aStream nextPut: value ] ].

    ^aStream contents
!

select: aBlock
    "Answer the objects in the domain for which aBlock returns true"

    | aStream |
    aStream := WriteStream on: (domain species new: self size).
    self do: [ :value | (aBlock value: value) ifFalse:
    	    	[ aStream nextPut: value ] ].

    ^aStream contents
!!



!MappedCollection methodsFor: 'private'!

setCollection: aCollection andMap: aMap
    domain := aCollection.
    map := aMap
!

species
    ^self class
!!
