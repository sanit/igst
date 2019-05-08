"======================================================================
|
|   SystemDictionary Method Definitions
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


Dictionary variableSubclass: #SystemDictionary
	   instanceVariableNames: ''
	   classVariableNames: ''
	   poolDictionaries: ''
	   category: 'Language-Implementation'
!

SystemDictionary comment: 
'I am a special form of dictionary.  Typically I only have one instance,
called "Smalltalk", which is known to the Smalltalk interpreter.  I define
several methods that are "system" related, such as #quitPrimitive.
My instance also helps keep track of dependencies between objects.' !


">>> See also builtins.st for some non-primitive but highly useful
SystemDictionary method definitions."


!SystemDictionary methodsFor: 'basic'!

halt
    "Interrupt interpreter"
    ContextPart unwind
!

initialize
    "Initialize the globals"
    self at: #Dependencies put: (IdentityDictionary new).
    self addFeature: #Kernel.
    KernelInitialized := true.
!

printOn: aStream
    "Print a representation of the receiver"
    aStream nextPutAll: 'Smalltalk'
!

storeOn: aStream
    "Store Smalltalk code compiling to the receiver"
    aStream nextPutAll: 'Smalltalk'
! !


!SystemDictionary methodsFor: 'initialization'!

addInit: aBlock
    "Adds 'aBlock' to the array of blocks to be invoked after every start
     of the system.
     If the kernel has been already initialized, evaluate the block"
    InitBlocks isNil ifTrue: [
        InitBlocks := OrderedCollection new.
    ].
    InitBlocks add: aBlock.
    KernelInitialized ifTrue: [ aBlock value ]
!

doInits
    "Called after the system has loaded the image, this will invoke any
     init blocks that have been installed."
    InitBlocks isNil ifFalse: [
        InitBlocks do: [ :aBlock | aBlock value ]
    ].
! !


!SystemDictionary methodsFor: 'miscellaneous'!

arguments
    "Return the command line arguments after the -a switch"
    self getArgc > 0 ifFalse: [ ^#() ].

    ^(1 to: self getArgc)
        collect: [ :i | self getArgv: i ]
!

backtrace
    "Print a backtrace on the Transcript."
    "This replaces the primitive in builtins.st"
    thisContext parentContext backtrace
!

snapshot
    "Save a snapshot on the image file that was loaded on startup"
    self snapshot: File image
! !


!SystemDictionary methodsFor: 'special accessing'!

allBehaviorsDo: aBlock
    "Evaluate aBlock once for each class and metaclass in the system.
     Trick: the class objects for classes directly derived from nil (for
     example `Object class') are subclasses of *Class* (see
     UndefinedObject>>#subclass:instanceVariableNames:...)"
    Behavior withAllSubclassesDo: [ :subclass |
        subclass == Class ifFalse: [
            subclass == Metaclass ifFalse: [
                subclass allInstancesDo: aBlock
            ]
        ]
    ].
    "Special case classes and metaclasses because #allInstancesDo: is very
     slow - the less we use it, the better it is."
    Class allSubclassesDo: [ :each |
        aBlock
             value: each;
             value: each instanceClass
    ]
!

allClassesDo: aBlock
    "Evaluate aBlock once for each class in the system.
     Trick: the class objects for classes directly derived from nil (for
     example `Object class') are subclasses of *Class* (see
     UndefinedObject>>#subclass:instanceVariableNames:...)"
    Class allSubclassesDo: [ :each | aBlock value: each instanceClass ]
!

allClassObjectsDo: aBlock
    "Evaluate aBlock once for each class and metaclass in the system.
     Trick: the class objects for classes directly derived from nil (for
     example `Object class') are subclasses of *Class* (see
     UndefinedObject>>#subclass:instanceVariableNames:...)"
    Class allSubclassesDo: [ :each |
        aBlock
            value: each;
            value: each instanceClass ]
!

allMetaclassesDo: aBlock
    "Evaluate aBlock once for each metaclass in the system.
     Trick: the class objects for classes directly derived from nil (for
     example `Object class') are subclasses of *Class* (see
     UndefinedObject>>#subclass:instanceVariableNames:...)"
    Class allSubclassesDo: aBlock
!

classAt: aKey ifAbsent: aBlock
    "Answer the value corrisponding to aKey if it is a class. Evaluate
     aBlock and answer its result if either aKey is not found or it is
     associated to something different from a class."
    | class |
    class := self at: aKey asGlobalKey ifAbsent: [ ^aBlock value ].
    class isClass ifFalse: [ ^aBlock value ].
    ^class
!

classAt: aKey
    "Answer the value corrisponding to aKey if it is a class. Fail
     if either aKey is not found or it is associated to something different
     from a class."
    ^self classAt: aKey ifAbsent: 
        [ ^self error: 'class not found - ', aKey ].
!

dependenciesAt: anObject
    "Answer the dependants of anObject (or nil if there's no key for anObject
     in the Dependencies IdentityDictionary)"
    ^Dependencies at: anObject ifAbsent: [ ^nil ]
!

hasFeatures: features
    " Returns true if the feature or features in 'features' is one of the 
      implementation dependent features present"

    features class == Symbol
	ifTrue: [ ^Features includes: features ]
	ifFalse: [
	    features do: [ :feature |
	        (Features includes: feature asSymbol) ifTrue: [ ^true ]
	    ].
	    ^false
	]
!

addFeature: aFeature
    "Add the aFeature feature to the Features set"
    Features class == Set ifFalse: [ Features := Features asSet ].
    Features add: aFeature asSymbol.
!

removeFeature: aFeature
    "Remove the aFeature feature to the Features set"
    Features class == Set ifFalse: [ Features := Features asSet ].
    Features remove: aFeature ifAbsent: [ ]
!

version
    "Answer the current version of the GNU Smalltalk environment"
    ^Version
!!

