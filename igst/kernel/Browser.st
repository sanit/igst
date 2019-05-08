"======================================================================
|
|   Emacs based class browser support
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

Object subclass: #Browser
       instanceVariableNames: ''
       classVariableNames: 'BrowserClassesValid '
       poolDictionaries: ''
       category: 'Language-Implementation'
!


!Browser class methodsFor: 'browsing'!

initialize
    "Initialize the Emacs browsing system"
    BrowserClassesValid := false
!!


!Browser class methodsFor: 'browsing'!

startEmacsMessage
    "Start a message to be processed by emacs as Lisp"
    stdout nextPut: (Character value: 2)
!

finishEmacsMessage
    "Finish a message to be processed by emacs - does nothing for now"
!

withGcOff: aBlock
    "Evaluate aBlock while the `GC flipping...' message is off"
    | oldFlag |
    oldFlag := Smalltalk gcMessage: false.
    aBlock value.
    Smalltalk gcMessage: oldFlag
!

emacsFunction: funcName on: aBlock
    "Send to Emacs something like (funcName <aBlock is evaluated here>)"
    self withGcOff:
	[ self startEmacsMessage.
	  stdout nextPut: $(;
	      nextPutAll: funcName; nl.
	  aBlock value.
	  stdout nextPut: $); nl.    
    	  self finishEmacsMessage ]
!

emacsListFunction: funcName on: aBlock
    "Send to Emacs something like (funcName '(<aBlock is evaluated here>))"
    self emacsFunction: funcName on:
	[ stdout nextPutAll: '''('; nl.
	  aBlock value.
	  stdout nextPut: $) ]
!

oldShowInstanceMethods: class
    "Send to Emacs code that browses instance methods for class"
    | methods |
    methods := self getMethods: class.
    self withGcOff:
	[ self startEmacsMessage.
	  stdout nextPutAll: '(smalltalk-browse "instance methods"'; nl;
	      nextPutAll: '''test-func'; nl;
	      nextPutAll: 't'; nl;
	      nextPutAll: '''('; nl.
	  class allSelectors do:
	      [ :sel | stdout nextPutAll: '("';
    	    	           print: sel;
    	    	           nextPutAll: '" . "';
		           print: sel;
		       	   nextPutAll: '")'; nl ].
    	  stdout nextPutAll: ')'; nl.    
    	  self finishEmacsMessage ]
!

showMethods: class for: methodType 
    "Send to Emacs code that browses methods of the given type for class
    (methodType is either `class' or `instance')"
    | methods |
    "Experimental version"
    methods := class newGetMethods.    
    self emacsFunction: 'smalltalk-browse' on: 
	[ stdout nextPut: $";
	      nextPutAll: methodType;
	      nextPutAll: ' methods"'; nl;
	      nextPutAll: '''test-func'; nl;
	      nextPutAll: 't'; nl;
	      nextPutAll: '''('; nl.
	  methods associationsDo:
	      [ :sel | sel value value methodSourceFile notNil 
			   ifTrue: 
			       [ stdout nextPutAll: '("'; 
				     print: sel key; tab; tab;
				     print: sel value key;
				     nextPutAll: '" . ("'; 
				     nextPutAll: sel value value methodSourceFile;
nextPutAll: '" . ';
				     nextPutAll: sel value value methodSourcePos printString;
				     nextPut: $).
				 stdout nextPut: $); nl ] 
			   ifFalse:
			       [ stdout nextPutAll: '("'; 
				     print: sel key; tab; tab;
				     print: sel value key;
				     nextPutAll: '" . ("'; 
				     nextPutAll: sel value value methodSourceString
				     nextPut: $).
				 stdout nextPut: $); nl ]
			   ].
    	  stdout nextPut: $) ]
!
    
showDirectMethods: class inBuffer: bufferName
    "Send to Emacs code that browses methods defined in the given class,
    in a buffer with the given name"

    | methods |
    "Experimental version"
    methods := class getDirectMethods.
    self browseMethods: methods forClass: class inBuffer: bufferName .
!

showAllMethods: class inBuffer: bufferName
    "Send to Emacs code that browses ALL the methods understood by
    instances of the given class, in a buffer with the given name"

    | methods |
    "Experimental version"
    methods := class getAllMethods.
    self browseMethods: methods forClass: class inBuffer: bufferName .
!

showIndirectMethods: class inBuffer: bufferName
    "Send to Emacs code that browses the methods inherited (and not
    overridden) by the given class, in a buffer with the given name"
    | methods |
    "Experimental version"
    methods := class getIndirectMethods.
    self browseMethods: methods forClass: class inBuffer: bufferName.
!

getAllSelectors: selector inBuffer: bufferName
    "Send to Emacs code that browses the implementors of the given
    selectors in a buffer with the given name"
    | methods |
    methods := self getMethodsFor: selector.
    self browseMethods: methods forClass: Object inBuffer: bufferName.
!    

browseMethods: methods forClass: class inBuffer: bufferName
    "Send to Emacs code that browses the methods in the `methods'
    Dictionary, showing them as part of the `class' class in a
    buffer with the given name"
    self emacsFunction: 'smalltalk-method-browse' on:
	[ stdout nextPut: $";
	      nextPutAll: bufferName;
	      nextPutAll: '" ''('; nl.
	  methods associationsDo:
	      [ :sel | sel value value methodSourceFile notNil 
			   ifTrue:
			       [ stdout nextPutAll: '("'; 
				     print: sel key; 
				     nextPutAll: '" . ("'; 
				     nextPutAll: sel value value methodSourceFile;
				     nextPutAll: '" . ';
				     nextPutAll: sel value value methodSourcePos printString;
				     nextPutAll: '))'; nl ] 
			   ifFalse:
			       [ stdout nextPutAll: '("'; 
				     print: sel key; 
				     nextPutAll: '" . ("'; 
				     print: class;
				     nextPutAll: '" "';
				     nextPutAll: sel value value methodCategory;
				     nextPut: $"; nl;
				     nextPut: $";
				     nextPutAll: sel value value methodSourceString;
				     nextPutAll: '")'.
				 stdout nextPut: $); nl ]
			   ].
	  stdout nextPutAll: ')'; nl ]
!

oldShowMethods: class for: methodType
    "Send to Emacs code that browses methods of the given type for class
    (methodType is either `class' or `instance')"
    | methods |
    methods := class getMethods.    
    self withGcOff:
	[ self startEmacsMessage.
	  stdout nextPutAll: '(smalltalk-browse "';
	      nextPutAll: methodType;
	      nextPutAll: ' methods"'; nl;
	      nextPutAll: '''test-func'; nl;
	      nextPutAll: 't'; nl;
	      nextPutAll: '''('; nl.
	  methods associationsDo:
	      [ :sel | sel value methodSourceFile notNil ifTrue:
			   [ stdout nextPutAll: '("'; 
			         print: sel key;
    	    	    	    	 nextPutAll: '" . ("'; 
				 nextPutAll: sel value methodSourceFile;
				 nextPutAll: '" . ';
				 nextPutAll: sel value methodSourcePos printString;
				 nextPutAll: ')'.
			     stdout nextPutAll: ')'; nl ] ].
    	  stdout nextPutAll: '))'; nl.    
    	  self finishEmacsMessage ]
!

oldloadClassNames
    "Tell Emacs the class names"
    self withGcOff:
        [ self startEmacsMessage. 
    	  stdout nextPutAll: '(smalltalk-set-class-names ''('; nl. 
	  Object withAllSubclasses do: 
	      [ :class | class name == nil
			     ifFalse: [ stdout nextPutAll: class name; nl. ]
			     ].
	  stdout nextPutAll: '))'.
    	  self finishEmacsMessage ]
!

loadClassNames
    "Tell Emacs the class names (new version)"
    BrowserClassesValid 
	ifTrue: [ self emacsListFunction: 'identity' on: 
		      [ stdout nextPutAll: 'nil'; nl ]
		]
	ifFalse: 
	    [ self emacsListFunction: 'smalltalk-set-class-names' on:
		  [ Smalltalk associationsDo:
			[ :assoc | (assoc value isKindOf: Behavior)
				       ifTrue: [ stdout nextPutAll: assoc key;
						     nl ]
			] 
		  ].
	      BrowserClassesValid := true.
	    ]
!

selectorsForEmacs
    "Tell Emacs the names of ALL the defined selectors"
    | md |
    self emacsListFunction: 'smalltalk-set-all-methods' on:
	[ Smalltalk associationsDo:
	      [ :assoc | (assoc value isKindOf: Behavior)
			     ifTrue: 
				 [ (md := assoc value methodDictionary)
				       isNil ifFalse:
					   [ md keysDo:
						 " also spit out class methods"
						 [ :key | stdout nextPut: $";
							      print: key;
							      nextPut: $"; nl 
							      ]
						 ]
				       ]
			     ]
	      ]
!

browseHierarchy
    "Tell Emacs tp browse the Smalltalk class hierarchy"
    self emacsListFunction: 'smalltalk-hier-browser' on: 
	[ Object printHierarchyEmacs ]
!

testMethods: aClass for: methodType
    "Send to Emacs code that browses methods of the given type for class
    (methodType is either `class' or `instance')"
    | classes methods md |
    classes := (aClass allSuperclasses). 
    classes addFirst: aClass.
    self withGcOff:
	[ self startEmacsMessage.
	  stdout nextPutAll: '(smalltalk-fast-browse "';
	      nextPutAll: methodType;
	      nextPutAll: ' methods"'; nl;
	      nextPutAll: '''test-func'; nl;
	      nextPutAll: '''('; nl.
	  classes do:
	      [ :cl | md := cl methodDictionary.
		      md notNil ifTrue:
			  [ md associationsDo: 
				[ :meth | stdout nextPutAll: '("';
					      nextPutAll: meth key;
					      nextPutAll: '" . ("'; 
					      nextPutAll: meth value methodSourceFile;
					      nextPutAll: '" . ';
					      nextPutAll: meth value methodSourcePos printString;
					      nextPutAll: '))';
					      nl ]
				]
			  ].
	  stdout nextPutAll: '))'.
	  self finishEmacsMessage ].
!!


!Behavior methodsFor: 'browsing'!

methodDictionary
    "Answer the receiver's method dictionary"
    ^methodDictionary
!

getMethods
    "Answer the receiver's complete method dictionary - including
     inherited and not overridden methods"
    | classes methods md |
    methods := Dictionary new.
    self allSuperclasses reverseDo:
	[ :superclass | md := superclass methodDictionary.
			md notNil ifTrue: 
			    [ md associationsDo:
				  [ :assoc | methods add: assoc ] ] ].
    methodDictionary notNil ifTrue: 
	[ methodDictionary associationsDo:
	      [ :assoc | methods add: assoc ] ].
    ^methods
!

newGetMethods
    "Answer the receiver's complete method dictionary - including
     inherited and not overridden methods. Each value in the dictionary
     is an Association, whose key is the class which defines the
     method, and whose value is the actual CompiledMethod"
    | classes methods md b |
    methods := Dictionary new.
    b := [ :md :class | md associationsDo:
		  [ :assoc | methods 
				 add: (Association key: assoc key 
						   value: (Association 
							       key: class 
							       value: assoc value)) ] ].
    self allSuperclasses reverseDo:
	[ :superclass | md := superclass methodDictionary.
			md notNil ifTrue: 
			    [ b value: md value: superclass ] ].
    methodDictionary notNil ifTrue: 
	[ b value: methodDictionary value: self ].
    ^methods
!

getIndirectMethods
    "Answer a dictionary of the receiver's inherited and not
     overridden methods. Each value in the dictionary
     is an Association, whose key is the class which defines the
     method, and whose value is the actual CompiledMethod"

    | methods md |
    methods := Dictionary new.
    self allSuperclasses reverseDo: [ :superclass |
        md := superclass methodDictionary.
	md notNil ifTrue: [
            methodDictionary associationsDo: [ :assoc |
                methods at: assoc key put: (superclass -> assoc value)
            ]
        ]
    ].
    ^methods
!

getAllMethods
    "Answer the receiver's complete method dictionary - including
     inherited and not overridden methods. Each value in the dictionary
     is an Association, whose key is the class which defines the
     method, and whose value is the actual CompiledMethod"

    ^self getIndirectMethods
        addAll: self getDirectMethods;
        yourself
!

getDirectMethods
    "Answer the receiver's method dictionary; each value in the
     dictionary is not a CompiledMethod, but an Association, whose
     key is the class which defines the method (always the receiver),
     and whose value is the actual CompiledMethod"
    | methods |
    methods := Dictionary new.
    methodDictionary isNil ifTrue: [ ^methods ].
    methodDictionary associationsDo: [ :assoc |
        methods at: assoc key put: (self -> assoc value)
    ].
!

getMethodsFor: aSelector
    "Get a dictionary with all the definitions of the given selector
     along the hierarchy. Each key in the dictionary is a class which
     defines the method, and each value in the dictionary is an
     Association, whose key is the class again, and whose value is
     the actual CompiledMethod"
    | methods dict subclass |
    methods := Dictionary new.

    Class allSubclassesDo: [ :meta |
        subclass := meta instanceClass.
        dict := subclass methodDictionary.
	dict notNil ifTrue: [
	    dict at: aSelector ifPresent: [ :method |
		methods at: subclass put: (subclass -> method)
	    ]
	]
    ].
    ^methods
! !

!Behavior methodsFor: 'hierarchy browsing'!

printHierarchyEmacs
    "Print my entire subclass hierarchy on the terminal, in a
     format suitable for Emacs parsing."
    self printSubclasses: 0 using: [ :name :level |
        stdout
            nextPutAll: '("';
            print: name;
	    nextPutAll: '" . ';
	    print: level;
	    nextPutAll: ')'; nl.
    ]
!

printHierarchy
    "Print my entire subclass hierarchy on the terminal."
    self printSubclasses: 0 using: [ :name :level |
        stdout
            next: level * self hierarchyIndent put: Character space;
            nextPutAll: name;
            nl
    ]
!!


!Behavior methodsFor: 'private'!

printSubclasses: level using: aBlock
    "I print my name, and then all my subclasses, each indented according
     to its position in the hierarchy. I pass aBlock a class name and a level"
    | mySubclasses |
    aBlock value: self name value: level.

    mySubclasses := self subclasses asSortedCollection: [ :a :b |
        (a isMetaclass | b isMetaclass) or: [ a name <= b name ]
    ].
    mySubclasses do: [ :subclass |
        subclass isMetaclass
	    ifFalse: [ subclass printSubclasses: level + 1
	    	    	        using: aBlock ]
    ]
!

hierarchyIndent
    "Answer the indent to be used by #printHierarchy - 4 by default"
    ^4
!!

Browser initialize!
