"======================================================================
|
|   True Method Definitions
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
| This file is part of GNU-Smalltalk.
|
| GNU-Smalltalk is free software; you can redistribute it and/or modify it
| under the terms of the GNU General Public License as published by the Free
| Software Foundation; either version 2, or (at your option) any later version.
| 
| GNU-Smalltalk is distributed in the hope that it will be useful, but WITHOUT
| ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
| FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
| details.
| 
| You should have received a copy of the GNU General Public License along with
| GNU-Smalltalk; see the file COPYING.  If not, write to the Free Software
| Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
|
 ======================================================================"


Boolean subclass: #True
	instanceVariableNames: 'truthValue'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Language-Data types'
!

True comment: 
'I represent truth and justice in the world.  My motto is "semper veritatis".'
!


!True methodsFor: 'C hacks'!

asCBooleanValue
    ^1
!!


!True methodsFor: 'basic'!

ifTrue: trueBlock ifFalse: falseBlock
    "We are true -- evaluate trueBlock"
    ^trueBlock value
!

ifFalse: falseBlock ifTrue: trueBlock
    "We are true -- evaluate trueBlock"
    ^trueBlock value
!

ifTrue: trueBlock
    "We are true -- evaluate trueBlock"
    ^trueBlock value
!

ifFalse: falseBlock
    "We are true -- answer nil"
    ^nil
!

not
    "We are true -- answer false"
    ^false
!

& aBoolean
    "We are true -- anded with anything, we always answer the other operand"
    ^aBoolean
!

| aBoolean
    "We are true -- ored with anything, we always answer true"
    ^true
!

eqv: aBoolean
    "Answer whether the receiver and aBoolean represent the
    same boolean value"
    ^aBoolean
!

xor: aBoolean
    "Answer whether the receiver and aBoolean represent different
    boolean values"
    ^aBoolean not
!

and: aBlock
    "We are true -- anded with anything, we always answer the other operand,
    so evaluate aBlock"
    ^aBlock value
!

or: aBlock
    "We are true -- ored with anything, we always answer true"
    ^true
!!



!True methodsFor: 'printing'!

printOn: aStream
    "Print a representation of the receiver on aStream"
    aStream nextPutAll: 'true'
!!
