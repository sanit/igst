"======================================================================
|
|   Boolean Method Definitions
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


Object subclass: #Boolean
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Data types'
!

Boolean comment:
'I have two instances in the Smalltalk system: true and false.  I provide
methods that are conditional on boolean values, such as conditional
execution and loops, and conditional testing, such as conditional and and
conditional or.  I should say that I appear to provide those operations; 
my subclasses True and False actually provide those operations.' !


!Boolean class methodsFor: 'testing'!

isIdentity
    "Answer whether x = y implies x == y for instances of the receiver"
    ^true
!

isImmediate
    "Answer whether, if x is an instance of the receiver, x copy == x"
    ^true
! !



!Boolean methodsFor: 'overriding'!

shallowCopy
    ^self			"We only have one instance"
!

deepCopy
    ^self			"it's about as deep as we need to get"
! !


!Boolean methodsFor: 'storing'!

storeOn: aStream
    "Store on aStream some Smalltalk code which compiles to the receiver"

    self printOn: aStream		"representation is the same"

! !


!Boolean methodsFor: 'C hacks'!

asCBooleanValue
    self subclassResponsibility
!!


!Boolean methodsFor: 'basic'!

ifTrue: trueBlock ifFalse: falseBlock
    self subclassResponsibility
!

ifFalse: falseBlock ifTrue: trueBlock
    self subclassResponsibility
!

ifTrue: trueBlock
    self subclassResponsibility
!

ifFalse: falseBlock
    self subclassResponsibility
!

not
    self subclassResponsibility
!

& aBoolean
    self subclassResponsibility
!

| aBoolean
    self subclassResponsibility
!

eqv: aBoolean
    self subclassResponsibility
!

xor: aBoolean
    self subclassResponsibility
!

and: aBlock
    self subclassResponsibility
!

or: aBlock
    self subclassResponsibility
!!
