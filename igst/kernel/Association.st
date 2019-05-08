"======================================================================
|
|   Association Method Definitions
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


Magnitude subclass: #Association
	  instanceVariableNames: 'key value'
	  classVariableNames: ''
	  poolDictionaries: ''
	  category: 'Language-Data types'
!

Association comment: 
'My instances represent a mapping between two objects.  Typically, my
"key" object is a symbol, but I don''t require this.  My "value" object has 
no conventions associated with it; it can be any object at all.' !
	      


!Association class methodsFor: 'basic'!

key: aKey value: aValue
    "Answer a new association with the given key and value"
    ^self new key: aKey value: aValue
!!



!Association methodsFor: 'accessing'!

value: aValue
    "Set the association's value to aValue"
    value := aValue
!

key: aKey value: aValue
    "Set the association's key to aKey, and its value to aValue"
    key := aKey.
    value := aValue
!

key
    "Answer the association's key"
    ^key
!

value
    "Answer the association's value"
    ^value
!!



!Association methodsFor: 'testing'!

< anAssociation
    "Answer whether the association's key is less than anAssociation's"
    ^self key < anAssociation key
!

= anAssociation
    "Answer whether the association's key and value are the same as
     anAssociation's, or false if anAssociation is not an Association"
    ^self class == anAssociation class and: [
        self key = anAssociation key and: [ self value = anAssociation value ] ]
!

hash
    "Answer an hash value for the receiver"
    ^key hash + value hash

!!




!Association methodsFor: 'printing'!

printOn: aStream
    "Put on aStream a representation of the receiver"
    self key printOn: aStream.
    aStream nextPutAll: '->'.
    self value printOn: aStream
!!



!Association methodsFor: 'storing'!

storeOn: aStream
    "Put on aStream some Smalltalk code compiling to the receiver"
    aStream nextPutAll: '(Association key: '.
    self key storeOn: aStream.
    aStream nextPutAll: ' value: '.
    self value storeOn: aStream.
    aStream nextPut: $)
!!

