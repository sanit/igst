"======================================================================
|
|   ClassDescription Method Definitions
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



Behavior subclass: #ClassDescription
	 instanceVariableNames: 'name comment instanceVariables category'
	 classVariableNames: ''
	 poolDictionaries: ''
	 category: 'Language-Implementation'
!

ClassDescription comment: 
'My instances record information generally attributed to classes and
metaclasses; namely, the class name, class comment (you wouldn''t be
reading this if it weren''t for me), a list of the instance variables
of the class, and the class category.  I provide methods that
access classes by category, and allow whole categories of classes to be
filed out to external disk files.' !



!ClassDescription methodsFor: 'accessing class description'!

name
    "Answer the class name"
    ^name
!

comment
    "Answer the class comment"
    ^comment
!

comment: aString
    "Change the class name"
    comment := aString
!

addInstVarName: aString
    "Add the given instance variable to instance of the receiver"
    instanceVariables := instanceVariables copyWith: aString.
    instanceSpec := instanceSpec + 32768.    "Highly implementation specific"

    self
        updateInstanceVars: instanceVariables
        variable: self isVariable
        words: self isWords
        pointers: self isPointers
!

removeInstVarName: aString
    "Remove the given instance variable from the receiver and recompile
     all of the receiver's subclasses"
    instanceVariables := instanceVariables copyWithout: aString.
    instanceSpec := instanceSpec - 32768.    "Highly implementation specific"
    self compileAll: self.
    self compileAllSubclasses: self.

    self
        updateInstanceVars: instanceVariables
        variable: self isVariable
        words: self isWords
        pointers: self isPointers
! !



!ClassDescription methodsFor: 'organization of messages and classes'!

createGetMethod: what default: value
    "Create a method accessing the variable `what', with a default value
    of `value', using lazy initialization"

    ^(super createGetMethod: what default: value)
        methodCategory: 'accessing'
!

createGetMethod: what
    "Create a method accessing the variable `what'."

    ^(super createGetMethod: what)
        methodCategory: 'accessing'
!

createSetMethod: what
    "Create a method which sets the variable `what'."

    ^(super createSetMethod: what)
        methodCategory: 'accessing'
!

defineCFunc: cFuncNameString
  withSelectorArgs: selectorAndArgs
  returning: returnTypeSymbol 
  args: argsArray
    "See documentation. Too complex to describe it here ;-)"

    ^(super defineCFunc: cFuncNameString
             withSelectorArgs: selectorAndArgs
             returning: returnTypeSymbol 
             args: argsArray) methodCategory: 'C functions'
!

category
    "Answer the class category"
    ^category
!

category: aString
    "Change the class category to aString"
    category := aString
!

removeCategory: aString
    "Remove from the receiver every method belonging to the given category"
    | selector method category |
    methodDictionary isNil
    	ifTrue: [ ^self ].

    methodDictionary copy associationsDo:
    	[ :assoc | method := assoc value.
	    	   method methodCategory = category
		   	ifTrue: [ methodDictionary removeAssociation: assoc ] ].
!

whichCategoryIncludesSelector: selector
    "Answer the category for the given selector, or nil if the selector is
     not found"
    
    | method |
    methodDictionary isNil
    	ifTrue: [ ^nil ].
    method := methodDictionary at: selector ifAbsent: [ ^nil ].
    ^method methodCategory
! !



!ClassDescription methodsFor: 'copying'!

copy: selector from: aClass
    "Copy the given selector from aClass, assigning it the same category"
    | method |
    method := aClass compiledMethodAt: selector.
    methodDictionary at: selector put: method deepCopy.
!

copy: selector from: aClass classified: categoryName
    "Copy the given selector from aClass, assigning it the given category"
    | method |
    method := (aClass compiledMethodAt: selector) deepCopy.
    method methodCategory: categoryName.
    methodDictionary at: selector put: method
!

copyAll: arrayOfSelectors from: class
    "Copy all the selectors in arrayOfSelectors from class, assigning them the
     same category they have in class"
    arrayOfSelectors do:
	[ :selector | self copy: selector
			   from: class ]
!

copyAll: arrayOfSelectors from: class classified: categoryName
    "Copy all the selectors in arrayOfSelectors from aClass, assigning them
     the given category"
    arrayOfSelectors do:
	[ :selector | self copy: selector
			   from: class
			   classified: categoryName ]
!

copyAllCategoriesFrom: aClass
    "Copy all the selectors in aClass, assigning them the original category"
    | method |
    aClass selectors do:
	[ :selector | self copy: selector from: aClass ]
!

copyCategory: categoryName from: aClass
    "Copy all the selectors in from aClass that belong to the given category"
    | method |
    aClass selectors do:
	[ :selector | method := aClass compiledMethodAt: selector.
		      method methodCategory = categoryName
			  ifTrue: [ self copy: selector from: aClass ] ]
!

copyCategory: categoryName from: aClass classified: newCategoryName
    "Copy all the selectors in from aClass that belong to the given category,
     reclassifying them as belonging to the given category"
    | method |
    aClass selectors do:
	[ :selector | method := aClass compiledMethodAt: selector.
		      method methodCategory = categoryName
			  ifTrue: [ self copy: selector
					 from: aClass
					 classified: newCategoryName ] ]
! !



!ClassDescription methodsFor: 'compiling'!

compile: code classified: categoryName
    "Compile code in the receiver, assigning the method to the given category.
     Answer the newly created CompiledMethod, or nil if an error was found."
    | method |
    method := self compile: code.
    method notNil 
                ifTrue: [method methodCategory: categoryName].
    ^method
!

compile: code classified: categoryName ifError: block
    "Compile method source and install in method category, categoryName.  If
     there are parsing errors, invoke exception block, 'block' (see
     compile:ifError:).  Return the method"
    | method |
    method := self compile: code ifError: [ ^block value ].
    method notNil 
                ifTrue: [method methodCategory: categoryName].
    ^method
!

compile: code classified: categoryName notifying: requestor
    "Compile method source and install in method category, categoryName.  If
     there are parsing errors, send an error message to requestor"
    | method |
    method := self compile: code notifying: requestor.
    method notNil 
                ifTrue: [method methodCategory: categoryName].
    ^method
! !



!ClassDescription methodsFor: 'accessing instances and variables'!

instVarNames
    "Answer an Array containing the instance variables defined by the receiver"
    | superVars |
    instanceVariables isNil ifTrue: [ ^#() ].
    superClass isNil
	ifTrue: [ ^instanceVariables copy ]
	ifFalse: [ superVars := superClass allInstVarNames.
		   ^instanceVariables copyFrom: superVars size+1 
				      to: instanceVariables size ]
!

subclassInstVarNames
    "Answer the names of the instance variables the receiver inherited from its
     superclass"
    | superVars |
    instanceVariables isNil ifTrue: [ ^#() ].
    superClass isNil ifTrue: [ ^#() ].

    ^superClass allInstVarNames.
!

allInstVarNames
    "Answer the names of every instance variables the receiver contained in the
     receiver's instances"
    ^instanceVariables isNil ifTrue: [#()] ifFalse: [instanceVariables].
! !



!ClassDescription methodsFor: 'printing'!

classVariableString
    self subclassResponsibility
!

instanceVariableString
    "Answer a string containing the name of the receiver's instance variables."
    | stream |
    instanceVariables isNil ifTrue: [ ^'' ].
    stream := WriteStream on: (String new: 0).
    instanceVariables do: [ :instVarName |
        stream nextPutAll: instVarName; nextPut: ($ ) ].
    ^stream contents
!

sharedVariableString
    self subclassResponsibility
! !



!ClassDescription methodsFor: 'filing'!

fileOutOn: aFileStream
    "File out complete class description:  class definition, class and
     instance methods"

    self subclassResponsibility
!

fileOut: fileName
    "Open the given file and to file out a complete class description to it"

    | aFileStream  |
    aFileStream := FileStream open: fileName mode: FileStream write
        ifFail: [ ^self error: 'Failed to open ''', fileName, '''' ].

    Transcript nextPutAll: 'Filing out class to: '; nextPutAll: fileName.
    
    self fileOutOn: aFileStream. 
    aFileStream close.
!

fileOutCategory: categoryName to: fileName
    "File out all the methods belonging to the method category, categoryName,
     to the fileName file"

    | aFileStream |
    aFileStream := FileStream open: fileName mode: FileStream write
        ifFail: [ ^self error: 'Failed to open ''', fileName, '''' ].

    Transcript nextPutAll: 'Filing out a category to: '; nextPutAll: fileName.

    self fileOutCategory: categoryName toStream: aFileStream.
    aFileStream close
!
        
fileOutCategory: category toStream: aFileStream
    "File out all the methods belonging to the method category, categoryName,
     to aFileStream"

    | methods |
    self selectors isNil ifTrue: [ ^self ].

    aFileStream
        nextPut: $!;
        print: self;
        nextPutAll: ' methodsFor: ';
        store: category;
        nextPut: $!.

    methods := self selectors select: [ :selector |
        (self compiledMethodAt: selector) methodCategory = category
    ].

    methods asSortedCollection do: [ :selector |
        aFileStream
            nextPutAll: '

';          nextPutAll: (self sourceCodeAt: selector);
            nextPut: $!
    ].
    aFileStream nextPutAll: '!

'
!

fileOutSelector: selector to: fileName
    "File out the given selector to fileName"

    | aFileStream |
    aFileStream := FileStream open: fileName mode: FileStream write
        ifFail: [ ^self error: 'Failed to open ''', fileName, '''' ].

    Transcript nextPutAll: 'Filing out a selector to: '; nextPutAll: fileName.
    self fileOutHeaderOn: aFileStream.
    aFileStream nextPutAll: '!';
        print: self;
        nextPutAll: ' methodsFor: ';
        store: (self compiledMethodAt: selector) methodCategory;
        nextPut: $!;
        nl;
        nextPutAll: (self sourceCodeAt: selector) ;
        nextPutAll: '! !';
        close
!!


!ClassDescription methodsFor: 'private'!

updateInstanceVars: variableArray
    variable: variableBoolean
    words: wordBoolean 
    pointers: pointerBoolean

    "Update instance variables and instance spec of the class and all its subclasses"

    | instVarMap startOfInstanceVars endOfInstanceVars newInstanceVars 
       oldInstVars map iv representationChanges |
         
    startOfInstanceVars := self superclass instSize + 1.
    endOfInstanceVars :=  self instSize.
    newInstanceVars := variableArray
        copyFrom: startOfInstanceVars 
        to: variableArray size. 

    oldInstVars :=  self allInstVarNames. 
    instVarMap := Array new: newInstanceVars size.
    startOfInstanceVars to: endOfInstanceVars do: [ :i | 
        map := newInstanceVars indexOf: (oldInstVars  at: i). 
        map > 0 ifTrue: [instVarMap at: map put: i]
    ].

    "Fix up all subclasses."
    self allSubclassesDo: [ :sc |
        iv := sc allInstVarNames
            copyReplaceFrom: startOfInstanceVars 
            to: endOfInstanceVars
            with: newInstanceVars.

        sc setInstanceVariables: iv.

        "If a class is changed from a non-variable to variable class,
         update all its non-variable subclasses to be the same.  Update size
         information of class's instanceSpec"

        (self isVariable ~~ variableBoolean) & variableBoolean & sc isFixed
            ifTrue: [ sc setInstanceSpec: variableBoolean
                         words: wordBoolean
                         pointers: pointerBoolean
                         instVars: sc allInstVarNames size]
            ifFalse: [ sc setInstanceSpec: sc isVariable
                          words: sc isWords
                          pointers: sc isPointers
                          instVars: sc allInstVarNames size].

        "Mutate all instances of the class to conform to new memory model
         of the class."

        sc allInstancesDo: [ :each |
            each mutate: instVarMap
                 startAt: startOfInstanceVars
                 instSizeDiff: (oldInstVars size - variableArray size)
        ]
    ].

    "Now update this class' instance vars "       
    self setInstanceVariables: variableArray.

    self setInstanceSpec: variableBoolean words: wordBoolean
        pointers: pointerBoolean instVars: variableArray size.

    self allInstancesDo: [ :each |
        each
            mutate: instVarMap
            startAt: startOfInstanceVars
            instSizeDiff: (oldInstVars size  - variableArray size) 
    ].
!

collectCategories
    "Answer all the method categories, sorted by name"

    | categories |
    methodDictionary isNil ifTrue: [ ^#() ].

    categories := Set new.
    methodDictionary do:
        [ :method | categories add: (method methodCategory) ].

    ^categories asSortedCollection
!

setName: aSymbol
    name := aSymbol
!

setInstanceVariables: instVariableArray
    instanceVariables := instVariableArray
!

setSuperclass: aClass
    "Set the superclass of the receiver to be 'aClass'.  Also
     adds the receiver as a subclass of 'aClass'"
    self superclass == aClass
	ifTrue: [ ^self ].	"don't need to change anything"
    self superclass notNil	"remove any old knowledge of this class"
	ifTrue: [ self superclass removeSubclass: self ].
    self superclass: aClass.
    aClass addSubclass: self
! !
