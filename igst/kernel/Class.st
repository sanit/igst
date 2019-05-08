"======================================================================
|
|   Class Method Definitions
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


ClassDescription subclass: #Class
		 instanceVariableNames: 'classVariables sharedPools'
		 classVariableNames: ''
		 poolDictionaries: ''
		 category: 'Language-Implementation'
!

Class comment: 
'I am THE class object.  My instances are the classes of the system' !




!Class methodsFor: 'accessing instances and variables'!

addClassVarName: aString
    "Add a class variable with the given name to the class pool dictionary"
    | sym |
    sym := aString asSymbol.	"### maybe this should remain a string? "
    (classVariables notNil and: [classVariables includesKey: sym])
    	     ifTrue: [ ^self error: 'class variable already present' ].

    classVariables at: sym put: nil
!

removeClassVarName: aString
    "Removes the class variable from the class, error if not present, or
     still in use."
    | sym |
    sym := aString asSymbol.	"### maybe this should remain a string? "
    " ### test for use in sub method "
    (classVariables notNil and: [classVariables includesKey: sym])
    	ifTrue: [ ^classVariables removeKey: sym ].

	self error: 'class variable not present' 
!

classPool
    "Answer the class pool dictionary"
    classVariables isNil ifTrue: [ classVariables := Dictionary new ].
    ^classVariables
!

classVarNames
    "Answer the names of the variables in the class pool dictionary"
    ^classVariables notNil
        ifTrue: [ classVariables keys ]
        ifFalse: [ Set new ].
!

allClassVarNames
    "Answer the names of the variables in the receiver's class pool dictionary
     and in each of the superclasses' class pool dictionaries"
    | superVarNames |
    superVarNames :=  self classVarNames.
    self allSuperclasses do: [ :each |
        superVarNames addAll: each classVarNames].
    ^superVarNames.  
!

addSharedPool: aDictionary
    "Add the given shared pool to the list of the class' pool dictionaries"
    (sharedPools includes: aDictionary)
    	ifFalse: [ sharedPools := sharedPools copyWith: aDictionary ]
!

removeSharedPool: aDictionary
    "Remove the given dictionary to the list of the class' pool dictionaries"
    sharedPools := sharedPools copyWithout: aDictionary
!

sharedPools
    "Return the names of the shared pools defined by the class"
    | s |
    s := Set new.
    sharedPools notNil ifTrue: [
	Smalltalk associationsDo: [ :each |
	    (sharedPools includes: each value)
	        ifTrue: [ s add: each key ]
	]
    ].
    ^s
!

initialize			"redefined in children (?)"
    ^self
! !



!Class methodsFor: 'testing'!

= aClass
    "Returns true if the two class objects are to be considered equal."
    "^(aClass isKindOf: Class) and: [name = aClass name]"
    ^self == aClass
! !



!Class methodsFor: 'instance creation - alternative'!

categoriesFor: method are: categories
    "Don't use this, it is only present to file in from IBM Smalltalk"
    (self >> method) methodCategory: (categories at: 1)
!

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


!Class methodsFor: 'instance creation'!


subclass: classNameString
    instanceVariableNames: stringInstVarNames
    classVariableNames: stringOfClassVarNames
    poolDictionaries: stringOfPoolNames
    category: categoryNameString
    "Define a fixed subclass of the receiver with the given name, instance
     variables, class variables, pool dictionaries and category. If the
     class is already defined, if necessary, recompile everything needed."

    | meta |
    KernelInitialized ifFalse: [
        ^(Smalltalk at: classNameString) category: categoryNameString
    ].
    meta := self metaclassFor: classNameString.
    ^meta name: classNameString
	  environment: Smalltalk
	  subclassOf: self
	  instanceVariableNames: stringInstVarNames
	  variable: false
	  words: false
	  pointers: true
	  classVariableNames: stringOfClassVarNames
	  poolDictionaries: stringOfPoolNames
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

    | meta |
    KernelInitialized ifFalse: [
        ^(Smalltalk at: classNameString) category: categoryNameString
    ].
    meta := self metaclassFor: classNameString.
    ^meta name: classNameString
	  environment: Smalltalk
	  subclassOf: self
	  instanceVariableNames: stringInstVarNames
	  variable: true
	  words: false
	  pointers: true
	  classVariableNames: stringOfClassVarNames
	  poolDictionaries: stringOfPoolNames
          category: categoryNameString
!

variableWordSubclass: classNameString
  instanceVariableNames: stringInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryNameString
    "Define a word variable subclass of the receiver with the given
     name, instance variables (must be ''), class variables, pool
     dictionaries and category. If the class is already defined, if
     necessary, recompile everything needed."

    | meta |
    KernelInitialized ifFalse: [
        ^(Smalltalk at: classNameString) category: categoryNameString
    ].

    meta := self metaclassFor: classNameString.
    ^meta name: classNameString
            environment: Smalltalk
            subclassOf: self
            instanceVariableNames: stringInstVarNames
            variable: true
            words: true
            pointers: false
            classVariableNames: stringOfClassVarNames
            poolDictionaries: stringOfPoolNames
            category: categoryNameString
!

variableByteSubclass: classNameString
  instanceVariableNames: stringInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryNameString
    "Define a byte variable subclass of the receiver with the given
     name, instance variables (must be ''), class variables, pool
     dictionaries and category. If the class is already defined, if
     necessary, recompile everything needed."

    | meta |
    KernelInitialized ifFalse: [
        ^(Smalltalk at: classNameString) category: categoryNameString
    ].

    meta := self metaclassFor: classNameString.
    ^meta name: classNameString
            environment: Smalltalk
            subclassOf: self
            instanceVariableNames: stringInstVarNames
            variable: true
            words: false
            pointers: false
            classVariableNames: stringOfClassVarNames
            poolDictionaries: stringOfPoolNames
            category: categoryNameString
!!



!Class methodsFor: 'printing'!

printOn: aStream
    "Print a representation of the receiver on aStream"
    aStream nextPutAll: self name
!

storeOn: aStream
    "Store Smalltalk code compiling to the receiver on aStream"
    aStream nextPutAll: self name
! !



!Class methodsFor: 'private'!

classInstanceVariableNames: stringClassInstVarNames
    self class instanceVariableNames: stringClassInstVarNames
!

setClassVariables: aDictionary
    classVariables := aDictionary
!

setSharedPools: aDictionary
    sharedPools := aDictionary
!

metaclassFor: classNameString
    "Create a Metaclass object for the given class name. The metaclass
     is a subclass of the receiver's metaclass"
    | className class |
    className := classNameString asSymbol.
    class := Smalltalk
        classAt: className 
	ifAbsent: [ ^Metaclass subclassOf: self class ].

    ^class class
! !


!Class methodsFor: 'testing functionality'!

asClass
    ^self
!

isClass
    ^true
!!


!Class methodsFor: 'filing'!

fileOutHeaderOn: aFileStream
    "Write date and time stamp to aFileStream"
    | now |
    aFileStream nextPutAll: '"Filed out from ';
	nextPutAll: Smalltalk version;
	nextPutAll: ' on '.
    now := Date dateAndTimeNow.
    aFileStream print: (now at: 1);
	nextPutAll:  '  ';
	print: (now at: 2);
	nextPutAll: '"!

'!

fileOutDeclarationOn: aFileStream
    "File out class definition to aFileStream"
    | aSet printFormattedSet |
    self isMetaclass ifTrue: [ ^self].

    printFormattedSet := [ 
        aFileStream nextPutAll: ''''. 
        (aSet isEmpty) not ifTrue: [
            aSet do: [ :element |
                aFileStream nextPutAll: element; space ]. 
            aFileStream nextPutAll: ''''.
        ].
    ].

    aFileStream print: self superclass; space;
        nextPutAll: self kindOfSubclass; space;
        store: name asSymbol.

    aFileStream nl; tab; nextPutAll: 'instanceVariableNames: '.
        ((aSet := self instVarNames ) isEmpty)
            ifTrue:[aFileStream nextPutAll: '''''']
            ifFalse: printFormattedSet.

    aFileStream nl; tab; nextPutAll: 'classVariableNames: '.
        (aSet := self classVarNames) isEmpty
            ifTrue:[aFileStream nextPutAll: '''''']
            ifFalse: printFormattedSet.

    aFileStream nl; tab; nextPutAll: 'poolDictionaries: '.
        (aSet := self sharedPools) isEmpty
            ifTrue:[aFileStream nextPutAll: '''''']
            ifFalse: printFormattedSet.

    aFileStream nl; tab; nextPutAll: 'category: ';
        store: category;
        nextPut: $!;
        nl; nl;
        print: self;
        nextPutAll: ' comment: ';
        nl;
        store: self comment;
        nextPut: $!;
        nl; nl.

    ((aSet := self class instVarNames) isEmpty)
        ifTrue:[ ^self ].

    aFileStream print: self class; nextPutAll: ' instanceVariableNames: '.
    printFormattedSet value.
    aFileStream nextPut: $!; nl; nl.
!

fileOutOn: aFileStream
    "File out complete class description:  class definition, class and
     instance methods"

    self fileOutHeaderOn: aFileStream. 
    self fileOutDeclarationOn: aFileStream.

    self class collectCategories do:
        [ :category | self class fileOutCategory: category toStream: aFileStream ].

    self collectCategories do:
        [ :category | self fileOutCategory: category toStream: aFileStream ].

    (self class includesSelector: #initialize) ifTrue: [
        aFileStream nl; print: self; nextPutAll: ' initialize!'.
    ].
    aFileStream nl
!!
