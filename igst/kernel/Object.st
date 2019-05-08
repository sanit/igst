"======================================================================
|
|   Object Method Definitions
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


nil subclass: #Object
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Implementation'
!

Object comment: 
'I am the root of the Smalltalk class system. 
All classes in the system are subclasses of me.' !

"Make sure that these symbols are defined, even if they don't work just yet."

Smalltalk at: #Dependencies put: nil!
Smalltalk at: #Debugger put: nil!
Smalltalk at: #Transcript put: nil!


!Object methodsFor: 'Relational operators'!

~= anObject
    "Answer whether the receiver and anObject are not equal"
    ^(self = anObject) == false
!

~~ anObject
    "Answer whether the receiver and anObject are not the same object"
    ^(self == anObject) == false 
! !



!Object methodsFor: 'testing functionality'!

isKindOf: aClass
    "Answer whether the receiver's class is aClass or
     a subclass of aClass"
    ^self class == aClass or:
    	[ self class inheritsFrom: aClass ]
!

isMemberOf: aClass
    "Returns true if the receiver is an instance of the class 'aClass'"
    ^self class == aClass
!

respondsTo: aSymbol
    "Returns true if the receiver understands the given selector"
    ^self class canUnderstand: aSymbol
!

isNil
    "Answer whether the receiver is nil"
    ^false
!

notNil
    "Answer whether the receiver is not nil"
    ^true
!

ifNil: nilBlock
    "Evaluate nilBlock if the receiver is nil, else answer self"
    ^self
!

ifNil: nilBlock ifNotNil: notNilBlock
    "Evaluate nilBlock if the receiver is nil, else evaluate
     notNilBlock, passing the receiver."
    ^notNilBlock value: self
!

ifNotNil: notNilBlock
    "Evaluate notNiilBlock if the receiver is not nil, passing the receiver.
     Else answer nil."
    ^notNilBlock value: self
!

ifNotNil: notNilBlock ifNotNil: nilBlock
    "Evaluate nilBlock if the receiver is nil, else evaluate
     notNilBlock, passing the receiver."
    ^notNilBlock value: self
!

isString
    ^false
!

isSymbol
    ^false
!

isCharacter
    ^false
!

isNumber
    ^false
!

isFloat
    ^false
!

isInteger
    ^false
!

isSmallInteger
    ^false
!

isClass
    ^false
!

isArray
    ^false
!

isMeta
    "Same as isMetaclass"
    ^self isMetaclass
!

isMetaClass
    "Same as isMetaclass"
    ^self isMetaclass
!

isMetaclass
    ^false
!!


!Object methodsFor: 'copying'!

copy
    "Returns a shallow copy of the receiver (the instance variables are
     not copied). The shallow copy receives the message postCopy and is
     then passed back"
    ^self shallowCopy
        postCopy;
        yourself
!

postCopy
    "Performs any changes required to do on a copied object. This is the
     place where one could, for example, put code to replace objects with
     copies of the objects"
    ^self
!

shallowCopy
    "Returns a shallow copy of the receiver (the instance variables are
     not copied)"
    | class aCopy i |
    class := self class.
    class isVariable
    	ifTrue: [ aCopy := class basicNew: self basicSize ]
	ifFalse: [ aCopy := class basicNew ].

    " copy the instance variables (if any) "
    i := class instSize.
    [ i > 0 ] whileTrue: [
    	aCopy instVarAt: i put: (self instVarAt: i).
    	i := i - 1 ].

    " copy the indexed variables (if any) "
    i := self basicSize.
    [ i > 0 ] whileTrue: [
    	aCopy basicAt: i put: (self basicAt: i).
    	i := i - 1 ].
    ^aCopy
!

deepCopy
    "Returns a shallow copy of the receiver (the instance variables are
     copies of the receiver's instance variables)"
    | class aCopy i |
    class := self class.
    class isVariable
    	ifTrue: [ aCopy := class basicNew: self basicSize ]
	ifFalse: [ aCopy := class basicNew ].

    " copy the instance variables (if any) "
    i := class instSize.
    [ i > 0 ] whileTrue: [
    	aCopy instVarAt: i put: (self instVarAt: i) copy.
    	i := i - 1 ].

    " copy the indexed variables (if any) "
    i := self basicSize.
    [ i > 0 ] whileTrue: [
    	aCopy basicAt: i put: (self basicAt: i) copy.
    	i := i - 1 ].
    ^aCopy
! !



!Object methodsFor: 'class type methods'!

species
    "This method has no unique definition. Generally speaking, methods which
     always return the same type usually don't use #class, but #species.
     For example, a PositionableStream's species is the class of the collection
     on which it is streaming (used by upTo:, upToAll:, upToEnd). Stream uses
     species for obtaining the class of next:'s return value, Collection uses
     it in its #copyEmpty: message, which in turn is used by all collection-re-
     turning methods. An Interval's species is Array (used by collect:, select:,
     reject:, etc.)."
    ^self class
!

yourself
    "Answer the receiver"
    ^self
! !



!Object methodsFor: 'dependents access'!

addDependent: anObject
    "Add anObject to the set of the receiver's dependents. Important:
     if an object has dependents, it won't be garbage collected."

    ^(Dependencies at: self ifAbsentPut: [ IdentitySet new ])
        add: anObject
!

removeDependent: anObject
    "Remove anObject to the set of the receiver's dependents. No problem
     if anObject is not in the set of the receiver's dependents."
    | dependencies |
    dependencies := Dependencies at: self ifAbsent: [ ^anObject ].
    dependencies remove: anObject ifAbsent: [].
    dependencies size < 1 ifTrue: [ self release ].
    ^anObject
!

dependents
    "Answer a collection of the receiver's dependents."
    | dependencies |
    dependencies := Dependencies at: self ifAbsent: [ ^OrderedCollection new ].
    ^dependencies asOrderedCollection
!

release
    "Remove all of the receiver's dependents from the set and allow the
     receiver to be garbage collected."

    Dependencies removeKey: self ifAbsent: [ ]
! !


!Object methodsFor: 'finalization'!

finalize
    "Do nothing by default"
! !


!Object methodsFor: 'change and update'!

changed
    "Send update: for each of the receiver's dependents, passing them the
     receiver"
    self changed: self
!

changed: aParameter
    "Send update: for each of the receiver's dependents, passing them
     aParameter"
    | dependencies |
    dependencies := Smalltalk dependenciesAt: self.
    dependencies notNil ifTrue:
    	[ dependencies do:
	    [ :dependent | dependent update: aParameter ] ]
!

update: aParameter
    "Default behavior is to do nothing. Called by #changed and #changed:"
!

broadcast: aSymbol
    "Send the unary message aSymbol to each of the receiver's dependents"
    | dependencies |
    dependencies := Smalltalk dependenciesAt: self.
    dependencies notNil ifTrue:
    	[ dependencies do:
	    [ :dependent | dependent perform: aSymbol ] ]
!

broadcast: aSymbol with: anObject
    "Send the message aSymbol to each of the receiver's dependents, passing
     anObject"
    | dependencies |
    dependencies := Smalltalk dependenciesAt: self.
    dependencies notNil ifTrue:
    	[ dependencies do:
	    [ :dependent | dependent perform: aSymbol with: anObject ] ]
!

broadcast: aSymbol withBlock: aBlock
    "Send the message aSymbol to each of the receiver's dependents, passing
     the result of evaluating aBlock with each dependent as the parameter"
    | dependencies |
    dependencies := Smalltalk dependenciesAt: self.
    dependencies notNil ifTrue:
    	[ dependencies do:
            [ :dependent | dependent
                perform: aSymbol
                with: (aBlock value: dependent) ] ]
!

broadcast: aSymbol withArguments: anArray
    "Send the message aSymbol to each of the receiver's dependents, passing
     the parameters in anArray"
    | dependencies |
    dependencies := Smalltalk dependenciesAt: self.
    dependencies notNil ifTrue:
    	[ dependencies do:
            [ :dependent | dependent perform: aSymbol withArguments: anArray ] ]
!!



!Object methodsFor: 'syntax shortcuts'!

-> anObject
    "Creates a new instance of Association with the receiver being the key
     and the argument becoming the value"
    ^Association key: self value: anObject
! !




!Object methodsFor: 'printing'!

printString
    "Answer a String representing the receiver"
    | stream |
    stream := WriteStream on: (String new: 0).
    self printOn: stream.
    ^stream contents
!

printOn: aStream
    "Print a represention of the receiver on aStream"
    | article nameString |
    nameString := self classNameString.
    article := nameString first isVowel ifTrue: [ 'an ' ] ifFalse: [ 'a ' ].
    aStream nextPutAll: article;
	    nextPutAll: nameString
!

print
    "Print a represention of the receiver on the Transcript (stdout the GUI
     is not active)"
    Transcript show: self printString
!

printNl
    "Print a represention of the receiver on stdout, put a new line
     the Transcript (stdout the GUI is not active)"
    Transcript showCr: self printString
!

basicPrintNl
    "Print a basic representation of the receiver, followed by a new line."
    self basicPrint.
    stdout nl
! !



!Object methodsFor: 'storing'!

storeString
    "Answer a String of Smalltalk code compiling to the receiver"
    | stream |
    stream := WriteStream on: (String new: 0).
    self storeOn: stream.
    ^stream contents
!

storeOn: aStream
    "Put Smalltalk code compiling to the receiver on aStream"
    | class hasSemi |
    class := self class.
    aStream nextPut: $(.
    aStream nextPutAll: self classNameString.
    hasSemi := false.
    class isVariable
    	ifTrue: [ aStream nextPutAll: ' basicNew: '.
	    	  self basicSize printOn: aStream ]
    	ifFalse: [ aStream nextPutAll: ' basicNew' ].
    1 to: class instSize do:
    	[ :i | aStream nextPutAll: ' instVarAt: '.
	       i printOn: aStream.
	       aStream nextPutAll: ' put: '.
	       (self instVarAt: i) storeOn: aStream.
	       aStream nextPut: $;.
	       hasSemi := true ].
    class isVariable ifTrue: 
    	[ 1 to: self validSize do:
	    [ :i | aStream nextPutAll: ' basicAt: '.
	    	   i printOn: aStream.
		   aStream nextPutAll: ' put: '.
		   (self basicAt: i) storeOn: aStream.
		   aStream nextPut: $;.
		   hasSemi := true ] ].
    hasSemi ifTrue: [ aStream nextPutAll: ' yourself' ].
    aStream nextPut: $)
!

store
    "Put a String of Smalltalk code compiling to the receiver on
     the Transcript (stdout the GUI is not active)"
    Transcript show: self storeString
!

storeNl
    "Put a String of Smalltalk code compiling to the receiver, followed by
     a new line, on the Transcript (stdout the GUI is not active)"
    Transcript showCr: self storeString
! !



!Object methodsFor: 'debugging'!

breakpoint: context return: return
    "Called back by the system. Must return the value passed through the
     second parameter"

    context method removeBreakpointAt: context ip.
    ^return
!

inspect
    "Print all the instance variables of the receiver on the Transcript"
    | class instVars instVal |
    class := self class.
    instVars := class allInstVarNames.
    Transcript nextPutAll: 'An instance of '.
    class printNl.
    1 to: instVars size do:
    	[ :i | Transcript nextPutAll: '  ';
		   nextPutAll: (instVars at: i);
		   nextPutAll: ': '.
	       (self instVarAt: i)  printNl ].

    1 to: self validSize do:
        [ :i | Transcript nextPutAll: '  [';
               print: i;
               nextPutAll: ']: ';
               print: (self basicAt: i);
               nl ] 
!

validSize
    "Answer how many elements in the receiver should be inspected"
    ^self basicSize
! !



!Object methodsFor: 'private'!

classNameString
    "Answer the name of the receiver's class"
    | name |
    name := self class name.
    name isNil
    	ifTrue: [ name := self name , ' class' ].
    ^name

!

mutate: instVarMap startAt: start instSizeDiff: adjustment
    "Private - Mutate object to a new class representation. Subtract
     adjustment to the size. instVarMap maps between old instVarAt: indices 
     and new instVarAt:put: indices. start is the first instance variable
     to change."

    | class aCopy mappedValue end numIndexedVariables  |
    class := self class.

    aCopy := class isVariable
        ifTrue: [
            "Allocate a new object based on the new class size derived from
             basicSize - adjustment"
            class basicNew: (numIndexedVariables := (self basicSize - adjustment))
        ]
        ifFalse: [ class basicNew ].

    end := instVarMap size + start - 1.
        
    "Copy the instance variables, if any"
    1 to: start - 1 do: [ :i |
        aCopy instVarAt: i
              put: (self instVarAt: i)
    ].
                                 
    "Copy old instance variables to their new positions using instVarMap"
    start to: end do: [ :i |
        mappedValue := instVarMap at: i - start + 1. 
        mappedValue notNil ifTrue: [
            aCopy instVarAt: i
                  put: (self instVarAt: mappedValue)
        ]
    ].

    end + 1 to: class instSize do: [ :i |
        aCopy instVarAt: i 
              put: (self instVarAt: i + adjustment)
    ].

    "Copy the indexed variables, if any.

    Note that conversions to different kind of indexed variables is not handled 
    correctly.  This should probably be disallowed because the representation is 
    entirely different.  We really need to have the old object in order to use its 
    old instanceSpec"

    class isVariable ifTrue: [
        1 to: numIndexedVariables do: [ :i |
             aCopy basicAt: i
                   put: (self specialBasicAt: i + adjustment).
        ]
    ].
    ^self become: aCopy
! !
