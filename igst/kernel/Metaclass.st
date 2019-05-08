"======================================================================
|
|   Metaclass Method Definitions
|
|   $Revision: 1.6.2$
|   $Date: 1999/08/31 11:23:18$
|   $Author: pb$
|
 ======================================================================"


"======================================================================
|
| Copyright 1990, 91, 92, 94, 95, 99 Free Software Foundation, Inc.
| Written by Steve Byrne, Brad Diller and Paolo Bonzini.
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


ClassDescription subclass: #Metaclass
		 instanceVariableNames: 'instanceClass'
		 classVariableNames: ''
		 poolDictionaries: ''
		 category: 'Language-Implementation'
!

Metaclass comment: 
'I am the root of the class hierarchy.  My instances are metaclasses, one for
each real class.  My instances have a single instance, which they hold
onto, which is the class that they are the metaclass of.  I provide methods
for creation of actual class objects from metaclass object, and the creation
of metaclass objects, which are my instances.  If this is confusing to you,
it should be...the Smalltalk metaclass system is strange and complex.' !


!Metaclass class methodsFor: 'instance creation'!

subclassOf: superMeta
    "Answer a new metaclass representing a subclass of superMeta"

    | newMeta |
    newMeta := self new.
    newMeta superclass: superMeta.
    superMeta addSubclass: newMeta.
    newMeta initMetaclass: superMeta.
    ^newMeta

! !



!Metaclass methodsFor: 'delegation'!

addClassVarName: aString
    "Add a class variable with the given name to the class pool dictionary"
    
    ^self instanceClass addClassVarName: aString
!

removeClassVarName: aString
    "Removes the class variable from the class, error if not present, or
     still in use."

    ^self instanceClass removeClassVarName: aString
!

classPool
    "Answer the class pool dictionary"

    ^self instanceClass classPool
!

classVarNames
    "Answer the names of the variables in the class pool dictionary"

    ^self instanceClass classVarNames
!

allClassVarNames
    "Answer the names of the variables in the receiver's class pool dictionary
     and in each of the superclasses' class pool dictionaries"

    ^self instanceClass allClassVarNames
!

addSharedPool: aDictionary
    "Add the given shared pool to the list of the class' pool dictionaries"

    ^self instanceClass addSharedPool: aDictionary
!

removeSharedPool: aDictionary
    "Remove the given dictionary to the list of the class' pool dictionaries"

    ^self instanceClass removeSharedPool: aDictionary
!

sharedPools
    "Return the names of the shared pools defined by the class"

    ^self instanceClass sharedPools
!

allSharedPools
    "Return the names of the shared pools defined by the class and any of
     its superclasses"

    ^self instanceClass allSharedPools
! !


!Metaclass methodsFor: 'basic'!

instanceVariableNames: classInstVarNames

    "Set the class-instance variables for the receiver to be those
     in classInstVarNames"

    | variableArray variableString |
    variableString := self superclass instanceVariableString,
        classInstVarNames.
    variableArray := self parseVariableString: variableString.

    1 to: variableArray size do:
        [ :i | variableArray at: i put: (variableArray at: i) asSymbol ].

    "If instance variables change, update  instance variables and
     instance spec of the class and all its subclasses "

    variableArray = self allInstVarNames ifTrue: [ ^self ].

    self
        updateInstanceVars: variableArray 
        variable: false
        words: false
        pointers: true.

    "If no variable has been removed, no need to recompile"
    (self allInstVarNames allSatisfy: [ :each | variableArray includes: each ])
        ifTrue: [ ^self ].
        
    Transcript nextPutAll: 'Recompiling Classes...'; nl.
    self compileAll.
    self compileAllSubclasses
!

name: newName
    environment: aSystemDictionary
    subclassOf: superclass
    instanceVariableNames: stringOfInstVarNames
    variable: variableBoolean
    words: wordBoolean
    pointers: pointerBoolean
    classVariableNames: stringOfClassVarNames
    poolDictionaries: stringOfPoolNames
    category: categoryName

    "Private - create a full featured class and install it, or change an
     existing one"

    | aClass variableString variableArray sharedPoolNames poolName pool 
      className classVarDict  needToRecompileMetaclasses needToRecompileClasses |

    "Initialize a metaclass"

    className := newName asSymbol.
    aClass := aSystemDictionary at: className ifAbsent: [ nil ].
    aClass isNil ifTrue: [ ^self newMeta: newName
        environment: aSystemDictionary
        subclassOf: superclass
        instanceVariableNames: stringOfInstVarNames
        variable: variableBoolean
        words: wordBoolean
        pointers: pointerBoolean
        classVariableNames: stringOfClassVarNames
        poolDictionaries: stringOfPoolNames
        category: categoryName
    ].

    needToRecompileMetaclasses := false.

    (aClass isVariable & aClass isBits) ifTrue: [
        (variableBoolean not | pointerBoolean) ifTrue: [
            ^self error: 'Cannot change non-pointer subclass to pointer subclass'
        ].
        aClass isWords ~~ wordBoolean ifTrue: [
            ^self error: 'Cannot change representation of non-pointer subclass'
        ]
    ].
    (aClass isFixed & (aClass allInstVarNames size > 0)) &
     (variableBoolean & pointerBoolean not) ifTrue: [
        ^self error: 'Cannot change pointer subclass with variables to non-pointer subclass'].

    "If the class is changed to a variable word or byte subclass,
     then check to see whether all variable subclasses are the same"
""" variableBoolean 
        ifTrue: [
            ((aClass isWords ~~ wordBoolean)
             | (aClass isPointers ~~ pointerBoolean))
             & (pointerBoolean not)
                ifTrue: [
                    aClass allSubclassesDo: [:sc | 
                        sc isVariable ifTrue: [
                             sc isWords ~~ wordBoolean
                                ifTrue: [^self error: 'cannot change to a variable word 
                                subclass because class has variable non-word subclasses']. 

                             sc isPointers ~~ pointerBoolean
                                ifTrue: [^self error: 'cannot change class to a variable byte 
                                class because class has variable non-byte subclasses' ]
                        ]
                    ]
                ]
        ].
"""
    "Inherit instance variables from parent"
    variableString := superclass notNil 
        ifTrue: [ superclass instanceVariableString, stringOfInstVarNames ]
        ifFalse: [ stringOfInstVarNames ].

    variableArray := self parseVariableString: variableString.
    1 to: variableArray size do:
        [ :i | variableArray at: i put: (variableArray at: i) asSymbol ].

    "If instance or indexed variables change, update 
     instance variables and instance spec of the class and all its subclasses "

    (needToRecompileClasses := variableArray ~= aClass allInstVarNames) 
        | (aClass isVariable ~~ variableBoolean)
        | (aClass isWords ~~ wordBoolean )
        | (aClass isPointers ~~ pointerBoolean)

            ifTrue: [
	        Smalltalk compact.
                aClass
                    updateInstanceVars: variableArray 
                    variable: variableBoolean
                    words: wordBoolean
                    pointers: pointerBoolean
            ].

   needToRecompileMetaclasses := false.
                
   classVarDict := (self parseToDict: stringOfClassVarNames).

   aClass classPool isNil 
       ifTrue: [ aClass setClassVariables: classVarDict ]
       ifFalse: [
            classVarDict keysDo: [ :key |
                 (aClass classPool includesKey: key)
                     ifFalse: [ aClass addClassVarName: key ]
            ]. 

            aClass classPool keys do: [ :aKey |
                 (classVarDict includesKey: aKey)
                     ifFalse: [
                         aClass removeClassVarName: aKey.
                         needToRecompileMetaclasses := true
                     ]
            ]
        ].

    sharedPoolNames := self parseVariableString: stringOfPoolNames.
    1 to: sharedPoolNames size do: [ :i |
        poolName := (sharedPoolNames at: i) asSymbol.

        aSystemDictionary
           at: poolName
           ifAbsent: [ ^self error: 'Pool ', poolName , ' does not exist' ].

        sharedPoolNames at: i put: poolName
    ].
    aClass sharedPools isNil 
        ifTrue: [ aClass setSharedPools: sharedPoolNames ]
        ifFalse: [
            sharedPoolNames do: [ :dict |
                (aClass sharedPools includes: dict)
                    ifFalse: [ aClass addSharedPool: dict ]
            ].

            aClass sharedPools copy do: [ :dict |
                (sharedPoolNames includes: dict)
                    ifFalse: [
                        aClass removeSharedPool: dict.
                        needToRecompileMetaclasses := true
                    ]
            ]
        ].

    (aClass superclass ~~ superclass) ifTrue: [
         "Fix references between classes..."
         aClass superclass removeSubclass: aClass.
         superclass addSubclass: aClass.
         aClass superclass: superclass.
         needToRecompileClasses := true.

         "...and between metaclasses..."
         self superclass removeSubclass: self.
         superclass class addSubclass: self.
         self superclass: superclass class.
         needToRecompileMetaclasses := true.
    ].

    aClass category: categoryName.

    "Please note that I need to recompile the classes in this sequence;
    otherwise, the same error is propagated to each selector which is compiled
    after an error is detected even though there are no further compilation 
    errors. Apparently, there is a bug in the primitive compileString.  This
    can be cleaned up later"
         
    (needToRecompileClasses | needToRecompileMetaclasses)
        ifTrue: [
            Transcript nextPutAll: 'Recompiling Classes...'; nl.
            aClass compileAll.
            needToRecompileMetaclasses
                ifTrue: [ aClass class compileAll ].

            aClass compileAllSubclasses.
            needToRecompileMetaclasses
                ifTrue: [ aClass class compileAllSubclasses ]
       ].

    Behavior flushCache.
    ^aClass
!


newMeta: newName
    environment: aSystemDictionary
    subclassOf: superclass
    instanceVariableNames: stringOfInstVarNames
    variable: variableBoolean
    words: wordBoolean
    pointers: pointerBoolean
    classVariableNames: stringOfClassVarNames
    poolDictionaries: stringOfPoolNames
    category: categoryName

    "Private - create a full featured class and install it"

    | aClass variableString variableArray sharedPoolNames poolName pool |

    sharedPoolNames := self parseVariableString: stringOfPoolNames.
    1 to: sharedPoolNames size do: [ :i |
        poolName := (sharedPoolNames at: i) asSymbol.
        pool := aSystemDictionary
            at: poolName
            ifAbsent: [ ^self error: 'Pool ', poolName , ' does not exist' ].

        sharedPoolNames at: i put: pool
    ].
    aClass := self new.
    instanceClass := aClass.
    aSystemDictionary at: (newName asSymbol) put: aClass.
    aClass superclass: superclass.
    aClass setName: newName asSymbol.
    superclass notNil
        ifTrue: [
            "Inherit instance variables from parent"
            variableString := superclass instanceVariableString.
            superclass addSubclass: aClass
        ]
        ifFalse: [ variableString := '' ].

    variableString := variableString , stringOfInstVarNames.
    variableArray := self parseVariableString: variableString.
    1 to: variableArray size do:
        [ :i | variableArray at: i put: (variableArray at: i) asSymbol ].

    Behavior flushCache.
    ^aClass
        setInstanceVariables: variableArray;
        setInstanceSpec: variableBoolean
            words: wordBoolean
            pointers: pointerBoolean
            instVars: variableArray size;
        setClassVariables: (self parseToDict: stringOfClassVarNames);
        setSharedPools: sharedPoolNames;
        category: categoryName;
        yourself
! !



!Metaclass methodsFor: 'accessing'!

primaryInstance
    "Answer the only instance of the metaclass - present for compatibility"
    ^instanceClass
!

soleInstance
    "Answer the only instance of the metaclass - present for compatibility"
    ^instanceClass
!

instanceClass
    "Answer the only instance of the metaclass"
    ^instanceClass
! !




!Metaclass methodsFor: 'printing'!

printOn: aStream
    "Print a represention of the receiver on aStream"
    instanceClass printOn: aStream.
    aStream nextPutAll: ' class'
!

storeOn: aStream
    "Store Smalltalk code compiling to the receiver on aStream"
    self printOn: aStream
! !



!Metaclass methodsFor: 'private'!

initMetaclass: superclass
    instanceVariables := superclass allInstVarNames.
    instanceSpec := superclass instanceSpec
!

parseVariableString: aString
    | stream tokens |
    stream := TokenStream on: aString.
    tokens := stream contents.
    tokens do: [ :token | self isLegalIdentifier: token ].
    ^tokens
!

parseToDict: aString
    | tokenArray dict |
    tokenArray := self parseVariableString: aString.
    dict := Dictionary new.
    tokenArray do:
    	[ :element | dict at: element asSymbol put: nil ].
    ^dict
!

isLegalIdentifier: token
    "Token is a string or string-oid"
    | firstTime |
    firstTime := true.
    token do: [ :ch |
        (ch isLetter | (firstTime not and: [ ch = $_ | ch isDigit ]))
	    ifFalse: [ ^self error: 'Invalid Smalltalk identifier: ', token printString ].

	firstTime := false
    ].
    ^true
!
	       

growClassInstance
    | newClass numInstVars |
    newClass := self new.
    numInstVars := self instSize.
    numInstVars printNl.
    1 to: numInstVars - 1 do:
	[ :i | newClass instVarAt: i put: 
		   (instanceClass instVarAt: i) ].

    instanceClass become: newClass.
! !

!Metaclass methodsFor: 'testing functionality'!

asClass
    ^instanceClass
!

isMetaclass
    ^true
!!


!Metaclass methodsFor: 'filing'!

fileOutOn: aFileStream
    "File out complete class description:  class definition, class and
     instance methods"

    instanceClass fileOutOn: aFileStream
! !