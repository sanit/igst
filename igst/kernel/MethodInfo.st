"======================================================================
|
|   MethodInfo Method Definitions
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


Object subclass: #MethodInfo
       instanceVariableNames: 'sourceCode category'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Implementation'
!

MethodInfo comment: 
'I provide information about particular methods.  I can produce the category
that a method was filed under, and can be used to access the source
code of the method.' !


!MethodInfo methodsFor: 'accessing'!

category
    "Answer the method category"
    ^category
!

category: aCategory
    "Set the method category"
    category := aCategory
!

stripSourceCode
    "Remove the reference to the source code for the method"
    sourceCode := nil
!

sourceCode
    "Answer a FileSegment or String or nil containing the method source code"
    ^sourceCode
!

sourceString
    "Answer a String containing the method source code"
    ^sourceCode asString
!

sourceFile
    "Answer the name of the file where the method source code is"
    ^sourceCode fileName
!

sourcePos
    "Answer the starting position of the method source code in the sourceFile"
    ^sourceCode filePos
!!


!MethodInfo methodsFor: 'equality'!

= aMethodInfo
    "Compare the receiver and aMethodInfo, answer whether they're equal"

    self class == aMethodInfo class ifFalse: [ ^false ].
    self == aMethodInfo ifTrue: [ ^true ].

    sourceCode = aMethodInfo sourceCode ifFalse: [ ^false ].
    ^category = aMethodInfo category
!

hash
    "Answer an hash value for the receiver"
    ^sourceCode hash + category hash
! !


!MethodInfo methodsFor: 'private'!

setSourceCode: source
    sourceCode := source
!!
