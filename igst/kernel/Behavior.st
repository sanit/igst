"======================================================================
|
|   Behavior Method Definitions
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


Object subclass: #Behavior
       instanceVariableNames: 'superClass subClasses 
    	    	    	       methodDictionary instanceSpec'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Implementation'
!

Behavior comment: 
'I am the parent class of all "class" type methods.  My instances know
about the subclass/superclass relationships between classes, contain
the description that instances are created from, and hold the method
dictionary that''s associated with each class.  I provide methods for
compiling methods, modifying the class inheritance hierarchy, examining the
method dictionary, and iterating over the class hierarchy.' !

"Make sure that this symbol is defined, even if it doesn't work just
yet."
Smalltalk at: #GnuDecompiler put: nil!

CFunctionDescs at: #CFunctionGensym put: 1!


!Behavior class methodsFor: 'C interface'!

defineCFunc: cFuncNameString
    withSelectorArgs: selectorAndArgs
    forClass: aClass 
    returning: returnTypeSymbol 
    args: argsArray

    "Lookup the part on the C interface in this manual -- it is too complex to
     describe it here ;-) Anyway this is private and kept for backward com-
     patibility. You should use defineCFunc:withSelectorArgs:returning:args:
     which also checks if the function is present in the DLD libraries."

    | code gensym descriptor |
    "This is pretty complex.  What I want to provide is a very efficient way
     of calling a C function.  I create a descriptor object that holds the
     relevant information regarding the C function.  I then compile the
     method that's to be invoked to call the C function.  This method uses the
     primitive #255 to perform the actual call-out.  To let the primitive
     know which descriptor to use, I arrange for the first and only method
     literal of the compiled method to be an association that contains as
     its value the C function descriptor object.  I add new associations to
     the global shared pool 'CFunctionDescs', and reference the newly
     generated key in the text of the compiled method."

    gensym := ('CFunction' , CFunctionGensym printString) asSymbol.
    descriptor := self
        makeDescriptorFor: cFuncNameString
    	returning: returnTypeSymbol
	withArgs: argsArray.

    descriptor isValid ifFalse: [
        ('C function ', cFuncNameString, ' not defined') printNl
    ].

    CFunctionDescs at: gensym put: descriptor.
    CFunctionGensym := CFunctionGensym + 1.

    code := ('%1
    "C call-out to %2. Do not modify!"
    <primitive: 255>
    ^%3')
	
	bindWith: selectorAndArgs
	with: cFuncNameString
	with: gensym asString.

    ^aClass compile: code
! !


!Behavior class methodsFor: 'creating lightweight classes'!

new
    "Create a new class. Having an instanceSpec defined this way won't be
     very useful, but at least it doesn't result in access violations!"
    ^self basicNew
        setInstanceSpec: false words: false pointers: true instVars: 0;
        yourself
! !


!Behavior methodsFor: 'creating method dictionary'!

createGetMethod: what default: value
    "Create a method accessing the variable `what', with a default value
    of `value', using lazy initialization"

    ^self compile: ('%1
    "Answer the receiver''s %1. Its default value is %2"
    %1 isNil ifTrue: [ %1 := %2 ].
    ^%1' bindWith: what with: value)
!

createGetMethod: what
    "Create a method accessing the variable `what'."

    ^self compile: ('%1
    "Answer the receiver''s %1"
    ^%1' bindWith: what)
!

createSetMethod: what
    "Create a method which sets the variable `what'."

    | parameter |
    parameter := (what = 'value') ifTrue: [ 'theValue' ] ifFalse: [ 'value' ].
    
    ^self compile: ('%1: %2
    "Set the receiver''s %1 to %2"
    %1 := %2' bindWith: what with: parameter)
!

defineCFunc: cFuncNameString
    withSelectorArgs: selectorAndArgs
    returning: returnTypeSymbol 
    args: argsArray

    "Lookup the C interface in the manual. Too complex to describe it here ;-)"
    ^Behavior
	defineCFunc: cFuncNameString
	withSelectorArgs: selectorAndArgs
	forClass: self
	returning: returnTypeSymbol 
	args: argsArray
!

methodDictionary
    "Answer the receiver's method dictionary. Don't modify the method
     dictionary unless you exactly know what you're doing"
    ^methodDictionary
!

methodDictionary: aDictionary
    "Set the receiver's method dictionary to aDictionary"
    methodDictionary := aDictionary.
    Behavior flushCache.
!

addSelector: selector withMethod: compiledMethod
    "Add the given compiledMethod to the method dictionary, giving it
     the passed selector. Answer compiledMethod"
    methodDictionary isNil ifTrue: [ methodDictionary := IdentityDictionary new ].
    methodDictionary at: selector put: compiledMethod.
    Behavior flushCache.
    ^compiledMethod
!

removeSelector: selector
    "Remove the given selector from the method dictionary, answer
     the CompiledMethod attached to that selector"
    ^self removeSelector: selector ifAbsent: [ self error: 'selector not found' ]
!

removeSelector: selector ifAbsent: aBlock
    "Remove the given selector from the method dictionary, answer
     the CompiledMethod attached to that selector. If the selector cannot
     be found, answer the result of evaluating aBlock."

    methodDictionary isNil ifTrue: [ ^aBlock value ].
    (methodDictionary includesKey: selector) ifFalse: [ ^aBlock value ].
    Behavior flushCache.
    ^methodDictionary removeKey: selector ifAbsent: [ self error: 'huh?!?' ]
!

compile: code
    "Compile method source.  If there are parsing errors, answer nil.
     Else, return a CompiledMethod result of compilation"
    (code isKindOf: PositionableStream)
    	ifTrue: [ ^self compileString: code contents ].
    (code isMemberOf: String)
    	ifFalse: [ ^self compileString: code asString ].
    ^self compileString: code
!

compile: code ifError: block
    "Compile method source.  If there are parsing errors, invoke
     exception block, 'block' passing file name, line number and error.
     description. Return a CompiledMethod result of compilation"
    (code isKindOf: PositionableStream)
        ifTrue: [ ^self compileString: code contents ifError: block ].
    (code isMemberOf: String)
        ifFalse: [ ^self compileString: code asString ifError: block ].
    ^self compileString: code ifError: block.
!

compile: code notifying: requestor
    "Compile method source.  If there are parsing errors, send #error:
     to the requestor object, else return a CompiledMethod result of
     compilation"
    | method |
    method := self compile: code.
    method isNil ifTrue:
	[ ^requestor error: 'Compilation failed' ].
    ^method
!

compileAllSubclasses: aNotifier
    "Recompile all selector of all subclasses. Notify aNotifier by sen-
     ding #error: messages if something goes wrong."
    self allSubclassesDo: [ :subclass | subclass compileAll: aNotifier ]
!

compileAllSubclasses
    "Recompile all selector of all subclasses. Notify aNotifier by sen-
     ding #error: messages if something goes wrong."
    self allSubclassesDo: [ :subclass | subclass compileAll ]
!

recompile: selector
    "Recompile the given selector, answer nil if something goes wrong or
     the new CompiledMethod if everything's ok."

    Transcript
        nextPutAll: 'Recompiling selector: ';
        print: selector asSymbol; nl.
    ^self compile: (self sourceCodeAt: selector)
          classified: (self compiledMethodAt: selector) methodCategory.
!

recompile: selector notifying: aNotifier
    "Recompile the given selector.  If there are parsing errors, send #error:
     to the aNotifier object, else return a CompiledMethod result of
     compilation"

    Transcript
        nextPutAll: 'Recompiling selector: ';
        print: selector asSymbol; nl.
    ^self compile: (self sourceCodeAt: selector)
         classified: ((self compiledMethodAt: selector) methodCategory)
         notifying: aNotifier.
!

decompile: selector
    "Decompile the bytecodes for the given selector."

    | method source |
    method := self compiledMethodAt: selector.
    source := method methodSourceString.
    source isNil
        ifTrue: [ GnuDecompiler decompile: selector in: self ]
    	ifFalse: [ ^source ]
!

edit: selector
    "Open Emacs to edit the method with the passed selector, then compile it"
    | method sourceFile sourcePos |
    method := self compiledMethodAt: selector.
    sourceFile := method methodSourceFile.
    sourceFile isNil
    	ifTrue: [ ^self error: 'decompiler can''t edit methods without source' ].
    sourcePos := method methodSourcePos.
    Smalltalk system: 'emacs -l st -smalltalk ', sourceFile, ' ', sourcePos printString

"   Possible Windows code follows: ""
    method := self >> selector.
    sourceFile := method methodSourceCode asString.
    sourcePos := sourceFile size.

    (sourceFile at: sourcePos) = $!
        ifTrue: [ sourcePos := sourcePos - 1 ].

    (FileStream open: 'st.tmp' mode: FileStream write)
        nextPutAll: (sourceFile copyFrom: 1 to: sourcePos);
        close.

    Smalltalk system: 'notepad st.tmp'.

    sourceFile := FileStream open: 'st.tmp' mode: FileStream read.
    self compile: sourceFile contents.
    sourceFile close.

    (self >> selector) methodCategory: method methodCategory.

    Smalltalk system: 'del st.tmp' "
!

compileAll
    "Recompile all selectors in the receiver. Ignore errors."
    Transcript nextPutAll: 'Recompiling class: '; print: self; nl.
    methodDictionary notNil
        ifTrue: [ methodDictionary keysDo: 
                      [ :selector | self recompile: selector ] ]
!

compileAll: aNotifier
    "Recompile all selectors in the receiver. Notify aNotifier by sen-
     ding #error: messages if something goes wrong."
    Transcript nextPutAll: 'Recompiling class: '; print: self; nl.
    methodDictionary notNil
        ifTrue: [ methodDictionary keysDo: 
                      [ :selector | self recompile: selector notifying: aNotifier] ]
! !



!Behavior methodsFor: 'evaluating'!

evalString: aString to: anObject
    "Answer the stack top at the end of the evaluation of the code in
     aString. The code is executed as part of anObject"
    | s result next method |
    s := ReadStream on: aString.
    [
        next := self extractEvalChunk: s.
        method := anObject class
            compileString: 'Doit ^ [
', next, ' ] value ' ifError: [ :fname :line :error | nil ].

        method isNil | (next allSatisfy: [ :each | each = Character space ])
            ifFalse: [ [ result := anObject Doit ] valueWithUnwind ].

        s atEnd ] whileFalse: [ ].

    "anObject class removeSelector: #Doit."
    ^result
!

evalString: aString to: anObject ifError: aBlock
    "Answer the stack top at the end of the evaluation of the code in
     aString. If aString cannot be parsed, evaluate aBlock (see
     compileString:ifError:). The code is executed as part of anObject"
    | s result next method |

    s := ReadStream on: aString.
    [
        next := self extractEvalChunk: s.
        method := anObject class
            compileString: 'Doit ^ [
', next, ' ] value '
            ifError: [:fname :lineNo :errorString | 
                aBlock value: fname value: lineNo - 1 value: errorString.
                nil
            ].

        method isNil | (next allSatisfy: [ :each | each = Character space ])
            ifFalse: [ [ result := anObject Doit ] valueWithUnwind ].

        s atEnd ] whileFalse: [ ].

    "anObject class removeSelector: #Doit."
    ^result
!

evaluate: code
    "Evaluate Smalltalk expression in 'code' and return result."

    (code isKindOf: PositionableStream)
        ifTrue: [ ^self evalString: code contents to: nil ].
    (code isMemberOf: String)
        ifFalse: [ ^self evalString: code asString to: nil ].
    ^self evalString: code to: nil
!


evaluate: code ifError: block
    "Evaluate 'code'.  If a parsing error is detected, invoke 'block'"

    (code isKindOf: PositionableStream)
        ifTrue: [ ^self evalString: code contentsto: nil ifError: block ].
    (code isMemberOf: String)
        ifFalse: [ ^self evalString: code asString to: nil ifError: block ].
    ^self evalString: code to: nil ifError: block.
!

evaluate: code to: anObject ifError: block
   "Evaluate Smalltalk expression as part of anObject's method definition.  This 
    method is used to support Inspector expression evaluation.  If a parsing error 
    is encountered, invoke error block, 'block'" 

   (code isKindOf: PositionableStream)
        ifTrue: [ ^self evalString: code contents to: anObject ifError: block ].
    (code isMemberOf: String)
        ifFalse: [ ^self evalString: code asString to: anObject ifError: block ].
    ^self evalString: code to: anObject ifError: block.
!

evaluate: code to: anObject 
    "Evaluate Smalltalk expression as part of anObject's method definition"

    (code isKindOf: PositionableStream)
        ifTrue: [ ^self evalString: code contents to: anObject ].
    (code isMemberOf: String)
        ifFalse: [ ^self evalString: code asString to: anObject ].
    ^self evalString: code to: anObject.
!

evaluate: code notifying: requestor
    "Evaluate Smalltalk expression in 'code'. If a parsing error is encountered, 
    invoke error block, 'block'"
    | method aStream |
    ^self evaluate: code ifError: [:fname :lineNo :errorString | 
                requestor error: ('line ', lineNo printString, ': ' , errorString) ].
!!


!Behavior methodsFor: 'creating a class hierarchy'!

superclass: aClass
    "Set the receiver's superclass."
    superClass := aClass.
    instanceSpec isNil ifTrue: [
        instanceSpec := aClass isNil
            ifTrue: [ 0 ]
            ifFalse: [ aClass instanceSpec ]
    ].
!

addSubclass: aClass
    "Add aClass asone of the receiver's subclasses."
    subClasses isNil 
	ifTrue: [ subClasses := Array new: 0 ]
	ifFalse: [ "remove old class if any"
		  subClasses := subClasses copyWithout: aClass ].
    subClasses := subClasses copyWith: aClass
!

removeSubclass: aClass
    "Remove aClass from the list of the receiver's subclasses"
    subClasses := subClasses copyWithout: aClass
! !



!Behavior methodsFor: 'accessing the methodDictionary'!

selectors
    "Answer a Set of the receiver's selectors"
    methodDictionary isNil
    	ifTrue: [ ^Set new ]
	ifFalse: [ ^methodDictionary keys ]
!

allSelectors
    "Answer a Set of all the selectors understood by the receiver"
    | aSet |
    aSet := self selectors.
    self allSuperclassesDo:
    	[ :superclass | aSet addAll: superclass selectors ].
    ^aSet
!

compiledMethodAt: selector
    "Return the compiled method associated with selector, from the local
    method dictionary.  Error if not found."
    ^methodDictionary at: selector
!

>> selector
    "Return the compiled method associated with selector, from the local
    method dictionary.  Error if not found."
    ^methodDictionary at: selector
!

selectorAt: method
    "Return selector for the given compiledMethod"
    ^methodDictionary keyAtValue: method.
!

sourceCodeAt: selector
    "Answer source code (if available) for the given compiledMethod"
    | source |
    source := (self compiledMethodAt: selector) methodSourceCode.
    source isNil ifTrue: [ '" *** SOURCE CODE NOT AVAILABLE *** "' copy ].
    ^source asString
!

sourceMethodAt: selector
    "This is too dependent on the original implementation"
    self shouldNotImplement
! !



!Behavior methodsFor: 'accessing instances and variables'!

allInstances
    "Returns a set of all instances of the receiver - you might get some
     nils in it: just discard them."
    | result cnt anInstance |
    result := Array new: self instanceCount.
    cnt := 1.
    anInstance := self someInstance.
    [ anInstance notNil ] whileTrue: [
        result at: cnt put: anInstance.
    	cnt := cnt + 1.
    	anInstance := anInstance nextInstance ].

    ^result makeWeak; yourself
!

instanceCount
    "Return a count of all the instances of the receiver"
    | count anInstance |
    count := 0.
    anInstance := self someInstance.
    [ anInstance notNil ]
    	whileTrue: [ count := count + 1.
	    	     anInstance := anInstance nextInstance ].    
    ^count
!
    
instVarNames
    "Return all the instance variables added to the hierarchy by the receiver"
    ^#()
!

subclassInstVarNames
    "Return all the instance variables inherited from the
     hierarchy by the receiver"
    ^self superclass isNil
        ifTrue: [ Array new: 0 ]
        ifFalse: [ self superclass allInstVarNames ]
!

allInstVarNames
    "Answer all the instance variables for instances of the receiver"
    ^self superclass allInstVarNames 
!

classVarNames
    "Answer all the class variables for instances of the receiver"
    ^self superclass isNil
        ifTrue: [ #() ]
        ifFalse: [ self superclass classVarNames ]
!

allClassVarNames
    "Return all the class variables understood by the receiver"
    | result |
    result := WriteStream with: self classVarNames.
    self allSuperclassesDo: [ :each |
        result nextPutAll: each classVarNames
    ].
    ^result contents
!

classPool
    "Answer the class pool dictionary. Since Behavior does not support
     classes with class variables, we answer an empty one; adding
     variables to it results in an error."
    ^Dictionary new makeReadOnly: true; yourself
!

sharedPools
    "Return the names of the shared pools defined by the class"
    ^self superclass isNil
        ifTrue: [ #() ]
        ifFalse: [ self superclass sharedPool ]
!

allSharedPools
    "Return the names of the shared pools defined by the class and any of
     its superclasses"
    | result |
    result := self sharedPools asOrderedCollection.
    self allSuperclassesDo: [ :each |
        result addAllLast: each sharedPools
    ].
    ^result asArray
!!



!Behavior methodsFor: 'accessing class hierarchy'!

subclasses
    "Answer the direct subclasses of the receiver in a Set"
    subClasses isNil
	ifTrue: [ ^Set new ]
	ifFalse: [ ^subClasses asSet ]
!

allSubclasses
    "Answer the direct and indirect subclasses of the receiver in a Set"
    | aSet |
    aSet := Set new.
    self allSubclassesDo: [ :subclass | aSet add: subclass ].
    ^aSet
!

withAllSubclasses
    "Answer a Set containing the receiver together with its direct and
     indirect subclasses"
    | aSet |
    aSet := Set with: self.
    aSet addAll: (self allSubclasses).
    ^aSet
!

superclass
    "Answer the receiver's superclass (if any, otherwise answer nil)"
    ^superClass
!

withAllSuperclasses
    "Answer the receiver and all of its superclasses in a collection"
    | supers |
    supers := OrderedCollection with: self.
    self allSuperclassesDo:
    	[ :superclass | supers addLast: superclass ].
    ^supers
!


allSuperclasses
    "Answer all the receiver's superclasses in a collection"
    | supers |
    supers := OrderedCollection new.
    self allSuperclassesDo:
    	[ :superclass | supers addLast: superclass ].
    ^supers
! !



!Behavior methodsFor: 'testing the method dictionary'!

whichSelectorsReferToByteCode: aByteCode
    "Return the collection of selectors in the class which
     reference the byte code, aByteCode"
    | s method specialSelector |
    s := Set new.
    methodDictionary isNil
        ifTrue: [ ^s ].
    methodDictionary associationsDo:
        [ :assoc |  method := assoc value.
                                ((1 to: method numBytecodes) detect:
                                [ :i | aByteCode = (method bytecodeAt: i)]
                                ifNone: [0]) > 0 
                                        ifTrue: [ s add: assoc key ].
        ].
    ^s
!

hasMethods
    "Return whether the receiver has any methods defined"

    ^methodDictionary notNil and: [ methodDictionary size ~= 0 ]
!

includesSelector: selector
    "Returns true if the local method dictionary contains the given selector"
    ^methodDictionary notNil and: [ methodDictionary includesKey: selector ]
!

canUnderstand: selector
    "Returns true if the instances of the receiver understand
     the given selector"
    (self includesSelector: selector)
    	ifTrue: [ ^true ].
    self allSuperclassesDo:
    	[ :superclass | (superclass includesSelector: selector)
	    	    	    ifTrue: [ ^true ] ].
    ^false
!

whichClassIncludesSelector: selector
    "Answer which class in the receiver's hierarchy contains the implementation
     of selector used by instances of the class (nil if none does)"
    self withAllSuperclassesDo:
    	[ :superclass | (superclass includesSelector: selector)
	    	    	    ifTrue: [ ^superclass ] ].
    ^nil
!

whichSelectorsAccess: instVarName
    "Answer a Set of selectors which access the given instance variable"
    | md index |
    index := self allInstVarNames
        indexOf: instVarName
        ifAbsent: [ ^Set new ].   " ### should it raise an error?"
    
    md := self methodDictionary.
    md isNil ifTrue: [ ^Set new ].
    ^(md select: [ :each | each accesses: index - 1 ])
        keys
!

whichSelectorsReferTo: anObject
    "Returns a Set of selectors that refer to anObject"
    | md |
    md := self methodDictionary.
    md isNil ifTrue: [ ^Set new ].
    ^(md select: [ :each | each refersTo: anObject ])
        keys
!

scopeHas: name ifTrue: aBlock
    "If methods understood by the receiver's instances have access to a
     symbol named 'name', evaluate aBlock"

    | nameSym |
    nameSym := name asSymbol.
    ((self allInstVarNames) includes: nameSym) ifTrue: [ ^aBlock value ].
    ((self allClassVarNames) includes: nameSym) ifTrue: [ ^aBlock value ].
    (self allSharedPools) do:
	[ :dictName | ((Smalltalk at: dictName asSymbol) includesKey: nameSym)
			  ifTrue: [ ^aBlock value ] ]
! !



!Behavior methodsFor: 'testing the form of the instances'!

isPointers
    "Answer whether the instance variables of the receiver's instances
     are objects"
    ^(self instanceSpec bitAt: 4) ~= 0
!

isIdentity
    "Answer whether x = y implies x == y for instances of the receiver"
    ^false
!

isImmediate
    "Answer whether, if x is an instance of the receiver, x copy == x"
    ^false
!

isBits
    "Answer whether the instance variables of the receiver's instances
     are bytes or words"
    ^self isPointers not
!

isBytes
    "Answer whether the instance variables of the receiver's
     instances are bytes"
    ^self isPointers not & self isWords not
!

isWords
    "Answer whether the instance variables of the receiver's
     instances are words"
    ^(self instanceSpec bitAt: 3) ~= 0
!

isFixed
    "Answer whether the receiver's instances have no indexed
     instance variables"
    ^self isVariable not
!

isVariable
    "Answer whether the receiver's instances have indexed instance variables"
    ^(self instanceSpec bitAt: 2) ~= 0
!

"Bit 1 (Smalltalk, bit 0 C) is now reserved for the 'hasFinalize' flag "

instSize
    "Answer how many fixed instance variables are reserved to each of the
     receiver's instances"
    ^(self instanceSpec bitShift: -15) bitAnd: self instVarMask
! !



!Behavior methodsFor: 'testing the class hierarchy'!

inheritsFrom: aClass
    "Returns true if aClass is a superclass of the receiver"
    | sc |
    sc := self.
    [ sc := sc superclass.
      sc isNil ]
    	whileFalse:
	    [ sc == aClass ifTrue: [ ^true ] ].
    ^false
!

kindOfSubclass
    "Return a string indicating the type of class the receiver is"
    self isVariable
    	ifTrue: [ self isBytes ifTrue: [ ^'variableByteSubclass: ' ].
	    	  self isPointers
		    ifTrue: [ ^'variableSubclass: ' ]
		    ifFalse: [ ^'variableWordSubclass: ' ] ]
	ifFalse: [ ^'subclass: ' ]
! !



!Behavior methodsFor: 'enumerating'!

allSubclassesDo: aBlock
    "Invokes aBlock for all subclasses, both direct and indirect."
    subClasses notNil
	ifTrue: [ subClasses do: [ :class | aBlock value: class.
					    class allSubclassesDo: aBlock ]
		      ]
!

allSuperclassesDo: aBlock
    "Invokes aBlock for all superclasses, both direct and indirect."
    | class superclass |
    class := self.
    [ superclass := class superclass.
      class := superclass.
      superclass notNil ] whileTrue:
      	[ aBlock value: superclass ]
!

withAllSubclassesDo: aBlock
    "Invokes aBlock for the receiver and all subclasses, both direct
     and indirect."
    aBlock value: self.
    subClasses notNil ifTrue: [
        subClasses do: [ :subclass |
            aBlock value: subclass.
            subclass allSubclassesDo: aBlock
        ]
    ]
!

withAllSuperclassesDo: aBlock
    "Invokes aBlock for the receiver and all superclasses, both direct
     and indirect."
    | class |
    class := self.
    [ aBlock value: class.
      class := class superclass.
      class notNil ] whileTrue: [ ]
!

allInstancesDo: aBlock
    "Invokes aBlock for all instances of the receiver"
    self allInstances do: [ :each |
        each isNil ifFalse: [ aBlock value: each ]
    ]
!

allSubinstancesDo: aBlock
    "Invokes aBlock for all instances of each of the receiver's subclasses."
    self allSubclassesDo:
    	[ :subclass | subclass allInstancesDo: aBlock ]
!

selectSubclasses: aBlock
    "Return a Set of subclasses of the receiver satisfying aBlock."
    | aSet |
    aSet := Set new.
    self allSubclassesDo: [ :subclass | (aBlock value: subclass)
    	    	    	    	    	    ifTrue: [ aSet add: subclass ] ].
    ^aSet
!

selectSuperclasses: aBlock
    "Return a Set of superclasses of the receiver satisfying aBlock."
    | aSet |
    aSet := Set new.
    self allSuperclassesDo: [ :superclass | (aBlock value: superclass)
    	    	    	    	    	    ifTrue: [ aSet add: superclass ] ].
    ^aSet
!

subclassesDo: aBlock
    "Invokes aBlock for all direct subclasses."
    subClasses notNil ifTrue: [
        subClasses do: [ :subclass | aBlock value: subclass ]
    ]
!!



!Behavior methodsFor: 'compilation (alternative)'!

methods
    "Don't use this, it's only present to file in from Smalltalk/V"
    ^self methodsFor: 'no category'
!

methodsFor
    "Don't use this, it's only present to file in from Dolphin Smalltalk"
    ^self methodsFor: 'no category'
!

methodsFor: category ifFeatures: features
    "Start compiling methods in the receiver if this implementation of
     Smalltalk has the given features, else skip the section"
    ^self methodsFor: category ifTrue: (Smalltalk hasFeatures: features)
!

methodsFor: category stamp: notUsed
    "Don't use this, it's only present to file in from Squeak"
    ^self methodsFor: category
!

privateMethods
    "Don't use this, it's only present to file in from IBM Smalltalk"
    ^self methodsFor: 'private'
!

publicMethods
    "Don't use this, it's only present to file in from IBM Smalltalk"
    ^self methodsFor: 'public'
! !



!Behavior methodsFor: 'support for lightweight classes'!

name
    "Answer the class name - a dummy one, since Behavior does not support
     names.  This class name is used, for example, to print the receiver."
    ^'<no name>'
! !


!Behavior methodsFor: 'private'!

extractEvalChunk: aStream
    "Private - Extract the code in the next evaluation chunk (i.e. the code
     until the next bang which is outside string literals or comments)"

    | code ch |
    code := WriteStream on: (String new: 100).
    [   aStream atEnd ] whileFalse: [
        ch := aStream next.
        ch = $! ifTrue: [ ^code contents ].

        code nextPut: ch.
        ch = $" ifTrue: [ code nextPutAll: (aStream upTo: ch); nextPut: ch ].
        ch = $' ifTrue: [
            [
                code
                    nextPutAll: (aStream upTo: ch);
                    nextPut: $'.
                aStream atEnd not and: [ aStream peekFor: ch ]
            ]   whileTrue: [
                code nextPut: ch.
            ]
        ].
    ].
    ^code contents
!

instanceSpec
    ^instanceSpec
!

setInstanceSpec: variableBoolean
  words: wordsBoolean
  pointers: pointersBoolean
  instVars: anIntegerSize
    "hasFinalize is cleared by this method -- should it be? "
    instanceSpec := 0.
    pointersBoolean
    	ifTrue: [ instanceSpec := instanceSpec bitOr: ( 1 bitShift: 3 ) ].
    wordsBoolean
        ifTrue: [ instanceSpec := instanceSpec bitOr: ( 1 bitShift: 2 ) ].
    variableBoolean
        ifTrue: [ instanceSpec := instanceSpec bitOr: ( 1 bitShift: 1 ) ].
    instanceSpec := instanceSpec bitOr: (anIntegerSize bitShift: 15).
!

instVarMask
    ^16r03FFF
! !
