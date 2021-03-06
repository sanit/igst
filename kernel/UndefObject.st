"======================================================================
|
|   UndefinedObject (nil) Method Definitions
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


Object subclass: #UndefinedObject
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Implementation'
!

UndefinedObject comment:
'I have the questionable distinction of being a class with only one
instance, which is the object "nil".  I suspect that I should be sent
messages when errors occur, but currently I am not.' !



!UndefinedObject methodsFor: 'testing'!

isNil
    "Answer whether the receiver is the undefined object nil. Always
     answer true."
    ^true
!

notNil
    "Answer whether the receiver is not the undefined object nil. Always
     answer false."
    ^false
!

ifNil: nilBlock
    "Evaluate nilBlock if the receiver is nil, else answer nil"
    ^nilBlock value
!

ifNil: nilBlock ifNotNil: notNilBlock
    "Evaluate nilBlock if the receiver is nil, else evaluate
     notNilBlock, passing the receiver."
    ^nilBlock value
!

ifNotNil: notNilBlock
    "Evaluate notNilBlock if the receiver is not nil, passing the receiver.
     Else answer nil"
    ^nil
!

ifNotNil: notNilBlock ifNotNil: nilBlock
    "Evaluate nilBlock if the receiver is nil, else evaluate
     notNilBlock, passing the receiver."
    ^nilBlock value
!!



!UndefinedObject methodsFor: 'printing'!

printOn: aStream
    "Print a representation of the receiver on aStream."
    aStream nextPutAll: 'nil'
!!



!UndefinedObject methodsFor: 'storing'!

storeOn: aStream
    "Store Smalltalk code compiling to the receiver on aStream."
    self printOn: aStream

!!


!UndefinedObject methodsFor: 'class creation - alternative'!

subclass: classNameString instanceVariableNames: stringInstVarNames classVariableNames: stringOfClassVarNames poolDictionaries: stringOfPoolNames
    "Don't use this, it is only present to file in from IBM Smalltalk"
    ^self
        subclass: classNameString
        instanceVariableNames: stringInstVarNames
        classVariableNames: stringOfClassVarNames
        poolDictionaries: stringOfPoolNames
        category: 'no category'
!

subclass: classNameString classInstanceVariableNames: stringClassInstVarNames instanceVariableNames: stringInstVarNames classVariableNames: stringOfClassVarNames poolDictionaries: stringOfPoolNames
    "Don't use this, it is only present to file in from IBM Smalltalk"
    ^(self
        subclass: classNameString
        instanceVariableNames: stringInstVarNames
        classVariableNames: stringOfClassVarNames
        poolDictionaries: stringOfPoolNames
        category: 'no category')
        classInstanceVariableNames: stringClassInstVarNames;
        yourself
!

variableSubclass: classNameString instanceVariableNames: stringInstVarNames classVariableNames: stringOfClassVarNames poolDictionaries: stringOfPoolNames
    "Don't use this, it is only present to file in from IBM Smalltalk"
    ^self
        variableSubclass: classNameString
        instanceVariableNames: stringInstVarNames
        classVariableNames: stringOfClassVarNames
        poolDictionaries: stringOfPoolNames
        category: 'no category'
!

variableSubclass: classNameString classInstanceVariableNames: stringClassInstVarNames instanceVariableNames: stringInstVarNames classVariableNames: stringOfClassVarNames poolDictionaries: stringOfPoolNames
    "Don't use this, it is only present to file in from IBM Smalltalk"
    ^(self
        variableSubclass: classNameString
        instanceVariableNames: stringInstVarNames
        classVariableNames: stringOfClassVarNames
        poolDictionaries: stringOfPoolNames
        category: 'no category')
        classInstanceVariableNames: stringClassInstVarNames;
        yourself
!

variableByteSubclass: classNameString instanceVariableNames: stringInstVarNames classVariableNames: stringOfClassVarNames poolDictionaries: stringOfPoolNames
    "Don't use this, it is only present to file in from IBM Smalltalk"
    ^self
        variableByteSubclass: classNameString
        instanceVariableNames: stringInstVarNames
        classVariableNames: stringOfClassVarNames
        poolDictionaries: stringOfPoolNames
        category: 'no category'
!

variableByteSubclass: classNameString classInstanceVariableNames: stringClassInstVarNames instanceVariableNames: stringInstVarNames classVariableNames: stringOfClassVarNames poolDictionaries: stringOfPoolNames
    "Don't use this, it is only present to file in from IBM Smalltalk"
    ^(self
        variableByteSubclass: classNameString
        instanceVariableNames: stringInstVarNames
        classVariableNames: stringOfClassVarNames
        poolDictionaries: stringOfPoolNames
        category: 'no category')
        classInstanceVariableNames: stringClassInstVarNames;
        yourself
!

variableWordSubclass: classNameString instanceVariableNames: stringInstVarNames classVariableNames: stringOfClassVarNames poolDictionaries: stringOfPoolNames
    "Don't use this, it is only present to file in from IBM Smalltalk"
    ^self
        variableWordSubclass: classNameString
        instanceVariableNames: stringInstVarNames
        classVariableNames: stringOfClassVarNames
        poolDictionaries: stringOfPoolNames
        category: 'no category'
!

variableWordSubclass: classNameString classInstanceVariableNames: stringClassInstVarNames instanceVariableNames: stringInstVarNames classVariableNames: stringOfClassVarNames poolDictionaries: stringOfPoolNames
    "Don't use this, it is only present to file in from IBM Smalltalk"
    ^(self
        variableWordSubclass: classNameString
        instanceVariableNames: stringInstVarNames
        classVariableNames: stringOfClassVarNames
        poolDictionaries: stringOfPoolNames
        category: 'no category')
        classInstanceVariableNames: stringClassInstVarNames;
        yourself
! !


!UndefinedObject methodsFor: 'class creation'!

subclass: classNameString
    instanceVariableNames: stringInstVarNames
    classVariableNames: stringOfClassVarNames
    poolDictionaries: stringOfPoolNames
    category: categoryNameString
    "Define a fixed subclass of the receiver with the given name, instance
     variables, class variables, pool dictionaries and category. If the
     class is already defined, if necessary, recompile everything needed."

    | meta basicPools |
    KernelInitialized ifFalse: [
        ^(Smalltalk at: classNameString) category: categoryNameString
    ].

    meta := self metaclassFor: classNameString.
    basicPools := ''.
    Smalltalk classAt: classNameString ifAbsent: [
        basicPools := 'Smalltalk CFunctionDescs '
    ].
    ^meta name: classNameString
	  environment: Smalltalk
	  subclassOf: self
	  instanceVariableNames: stringInstVarNames
	  variable: false
	  words: false
	  pointers: true
	  classVariableNames: stringOfClassVarNames
	  poolDictionaries: basicPools, stringOfPoolNames
	  category: categoryNameString
!

variableSubclass: classNameString
    instanceVariableNames: stringInstVarNames
    classVariableNames: stringOfClassVarNames
    poolDictionaries: stringOfPoolNames
    category: categoryNameString
    "Define a variable pointer subclass of the receiver with the given
     name, instance variables, class variables, pool dictionaries and
     category. If the class is already defined, if necessary, recompile
     everything needed."

    | meta basicPools |
    KernelInitialized ifFalse: [
        ^(Smalltalk at: classNameString) category: categoryNameString
    ].

    meta := self metaclassFor: classNameString.
    basicPools := ''.
    Smalltalk classAt: classNameString ifAbsent: [
        basicPools := 'Smalltalk CFunctionDescs '
    ].
    ^meta name: classNameString
	  environment: Smalltalk
	  subclassOf: self
	  instanceVariableNames: stringInstVarNames
	  variable: true
	  words: false
	  pointers: true
	  classVariableNames: stringOfClassVarNames
	  poolDictionaries: basicPools, stringOfPoolNames
	    category: categoryNameString
!

variableWordSubclass: classNameString
  instanceVariableNames: stringInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryNameString
    "Define a word variable subclass of the receiver with the given
     name, instance variables, class variables, pool dictionaries and
     category. If the class is already defined, if necessary, recompile
     everything needed."

    | meta basicPools |
    KernelInitialized ifFalse: [
        ^(Smalltalk at: classNameString) category: categoryNameString
    ].

    meta := self metaclassFor: classNameString.
    basicPools := ''.
    Smalltalk classAt: classNameString ifAbsent: [
        basicPools := 'Smalltalk CFunctionDescs '
    ].
    ^meta name: classNameString
            environment: Smalltalk
            subclassOf: self
            instanceVariableNames: stringInstVarNames
            variable: true
            words: true
            pointers: false
            classVariableNames: stringOfClassVarNames
            poolDictionaries: basicPools, stringOfPoolNames
            category: categoryNameString
!

variableByteSubclass: classNameString
  instanceVariableNames: stringInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryNameString
    "Define a byte variable subclass of the receiver with the given
     name, instance variables, class variables, pool dictionaries
     and category. If the class is already defined, if necessary,
     recompile everything needed."

    | meta basicPools |
    KernelInitialized ifFalse: [
        ^(Smalltalk at: classNameString) category: categoryNameString
    ].

    meta := self metaclassFor: classNameString.
    basicPools := ''.
    Smalltalk classAt: classNameString ifAbsent: [
        basicPools := 'Smalltalk CFunctionDescs '
    ].
    ^meta name: classNameString
            environment: Smalltalk
            subclassOf: self
            instanceVariableNames: stringInstVarNames
            variable: true
            words: false
            pointers: false
            classVariableNames: stringOfClassVarNames
            poolDictionaries: basicPools, stringOfPoolNames
            category: categoryNameString
!

metaclassFor: classNameString
    "Create a Metaclass object for the given class name. The metaclass
     is a subclass of Class"
    | className class |
    className := classNameString asSymbol.
    class := Smalltalk
        classAt: className 
	ifAbsent: [ ^Metaclass subclassOf: Class ].

    ^class class
!!