"======================================================================
|
|   False Method Definitions
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


Boolean subclass: #False
	instanceVariableNames: 'truthValue' " ### What's the real ST-80 name? "
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Language-Data types'
!

False comment: 
'I always tell lies. 
I have a single instance in the system, which represents the value false.' !


!False methodsFor: 'C hacks'!

asCBooleanValue
    ^0
!!


!False methodsFor: 'basic'!

ifTrue: trueBlock ifFalse: falseBlock
    "We are false -- evaluate the falseBlock"
    ^falseBlock value
!

ifFalse: falseBlock ifTrue: trueBlock
    "We are false -- evaluate the falseBlock"
    ^falseBlock value
!

ifTrue: trueBlock
    "We are false -- answer nil"
    ^nil
!

ifFalse: falseBlock
    "We are false -- evaluate the falseBlock"
    ^falseBlock value
!

not
    "We are false -- answer true"
    ^true
!

& aBoolean
    "We are false -- anded with anything, we always answer false"
    ^false
!

| aBoolean
    "We are false -- ored with anything, we always answer the other operand"
    ^aBoolean
!

eqv: aBoolean
    "Answer whether the receiver and aBoolean represent the
    same boolean value"
    ^aBoolean not
!

xor: aBoolean
    "Answer whether the receiver and aBoolean represent different
    boolean values"
    ^aBoolean
!

and: aBlock
    "We are false -- anded with anything, we always answer false"
    ^false
!

or: aBlock
    "We are false -- ored with anything, we always answer the other operand,
    so evaluate aBlock"
    ^aBlock value
!!



!False methodsFor: 'printing'!

printOn: aStream
    "Print a representation of the receiver on aStream"
    aStream nextPutAll: 'false'
!!
