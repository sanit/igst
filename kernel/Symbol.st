"======================================================================
|
|   Symbol Method Definitions
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


String variableByteSubclass: #Symbol
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Implementation'
!

Symbol comment: 
'My instances are unique throughout the Smalltalk system.  My instances 
behave for the most part like strings, except that they print differently,
and I guarantee that any two instances that have the same printed 
representation are in fact the same instance.' !


!Symbol class methodsFor: 'instance creation'!

isSymbolString: aString
    "Answer whether aString has already been interned. Note that this works
     because String>>#hash calculates the same hash value used when interning
     strings into the SymbolTable. Changing one of the hashing methods without
     changing the other will break this method."
    | link hash |
    hash := aString asString hash \\ SymbolTable size.
    link := SymbolTable at: hash + 1.

    link do: [ :each |
	| ok |
	each size = aString size ifTrue: [
	    ok := true.
	    each with: aString do: [ :a :b | a = b ifFalse: [ ok := false ] ].
	    ok ifTrue: [ ^true ].
	].
    ].
    ^false
!

internCharacter: aCharacter
    "Answer the one-character symbol associated to the given character."
    | s |
    s := String new: 1.
    s at: 1 put: aCharacter.
    ^self intern: s
!

new
    self shouldNotImplement
!

new: size
    self shouldNotImplement
!

with: element1
    "Answer a collection whose only element is element1"
    | s |
    s := String new: 1.
    s at: 1 put: element1.
    ^self intern: s
!

with: element1 with: element2
    "Answer a collection whose only elements are the parameters in the order
     they were passed"
    | s |
    s := String new: 2.
    s at: 1 put: element1.
    s at: 2 put: element2.
    ^self intern: s
!

with: element1 with: element2 with: element3
    "Answer a collection whose only elements are the parameters in the order
     they were passed"
    | s |
    s := String new: 3.
    s at: 1 put: element1.
    s at: 2 put: element2.
    s at: 3 put: element3.
    ^self intern: s
!

with: element1 with: element2 with: element3 with: element4
    "Answer a collection whose only elements are the parameters in the order
     they were passed"
    | s |
    s := String new: 4.
    s at: 1 put: element1.
    s at: 2 put: element2.
    s at: 3 put: element3.
    s at: 4 put: element4.
    ^self intern: s
! !



!Symbol methodsFor: 'converting'!

asString
    "Answer a String with the same characters as the receiver"
    ^self copyFrom: 1 to: self size
!

asSymbol
    "But we are already a Symbol, and furthermore, Symbols are identity objects!
    So answer the receiver."
    ^self
! !



!Symbol methodsFor: 'basic'!

argumentCount
    "Answer the number of arguments supported by the receiver, which is supposed
     to be a valid message name (#+, #not, #printOn:, #ifTrue:ifFalse:, etc.)"
    (self at: 1) isLetter ifFalse: [ ^1 ].
    ^(self at: self size) = $:
	ifTrue: [ self occurrencesOf: $: ]
	ifFalse: [ 0 ]
!!


!Symbol methodsFor: 'misc'!

species
    ^String
! !



!Symbol methodsFor: 'storing'!

storeOn: aStream
    "Store Smalltalk code compiling to the receiver on aStream"
    aStream nextPut: $#.
    (self allSatisfy: [ :none | none isSeparator not ])
	ifTrue: [ aStream nextPutAll: self ]
        ifFalse: [ super storeOn: aStream ]
! !

!Symbol methodsFor: 'testing functionality'!

isString
    ^false
!

isSymbol
    ^true
!!

