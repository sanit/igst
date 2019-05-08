"======================================================================
|
|   GNU Dynamic Loader Method Definitions
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


Object subclass: #DLD
       instanceVariableNames: ''
       classVariableNames: 'LibraryList'
       poolDictionaries: ''
       category: 'Language-C interface'
!

DLD comment: '...and Gandalf said:
``Many folk like to know beforehand what is to be set on the
table; but those who have laboured to prepare the feast like
to keep their secret; for wonder makes the words of praise
louder.''''

I am just an ancillary class used to reference some C functions.
Most of my actual functionality is used by redefinitions of methods
in CFunctionDescriptor and Behavior.'!


DLD class defineCFunc: 'dldLink'
	  withSelectorArgs: 'linkFile: aFileName'
	  returning: #cObject
	  args: #(string)
!

DLD class defineCFunc: 'dldGetFunc'
	  withSelectorArgs: 'library: libHandle getFunc: aFuncString'
	  returning: #cObject
	  args: #(cObject string)
!

DLD class defineCFunc: 'defineCFunc'
	  withSelectorArgs: 'defineCFunc: aName as: aFuncAddr'
	  returning: #void
	  args: #(string cObject)
!


!CFunctionDescriptor class methodsFor: 'testing'!

isFunction: function
    "Answer whether a function is registered (on the C side) with the
     given name or is dynamically loadable."

    | descriptor |
    descriptor := self
        makeDescriptorFor: function
    	returning: #void		"dummy"
	withArgs: #().			"dummy"

    ^descriptor isValid or: [ DLD defineExternFunc: function ]
!!


!Behavior methodsFor: 'C callout'!

defineCFunc: cFuncName
    withSelectorArgs: selector
    returning: aReturnType
    args: argsArray

    "Too complex to describe it here - Look up the C interface in the manual."

    CFunctionDescriptor isFunction: cFuncName.

    ^Behavior
	defineCFunc: cFuncName
	withSelectorArgs: selector
	forClass: self
	returning: aReturnType
	args: argsArray
!!


!DLD class methodsFor: 'Dynamic Linking'!

defineExternFunc: aFuncName
    "This method calls #primDefineExternFunc: to try to link to a function with
     the given name, and answers whether the linkage was successful. You can
     redefine this method to restrict the ability to do dynamic linking."
    ^self primDefineExternFunc: aFuncName
!

primDefineExternFunc: aFuncName
    "This method tries to link to a function with the given name, and answers
     whether the linkage was successful. It should not be overridden."
    | funcAddr |
    
    self libraryList do: [ :lib |
        lib value notNil ifTrue: [
            funcAddr := self library: lib value getFunc: aFuncName.
            funcAddr notNil ifTrue: [ 
                self defineCFunc: aFuncName as: funcAddr.
                ^true
            ]
        ]
    ].
    self libraryList
        detect: [ :lib |
            lib value isNil and: [
                lib value: (self linkFile: lib key).
                funcAddr := self library: lib value getFunc: aFuncName.
                funcAddr notNil
            ]
        ]
        ifNone: [ ^false ].

    self defineCFunc: aFuncName as: funcAddr.
    ^true
!

defineExternFunc: aFuncName
    withSelectorArgs: selector
    forClass: aClass
    returning: aReturnType
    args: argsArray

    "Obsolete - Mantained for compatibility.  This method does NOT
    look for statically linked functions."

    (self defineExternFunc: aFuncName) ifFalse: [
        self error: 'function ', aFuncName, ' not found'
    ].

    ^Behavior
	defineCFunc: aFuncName
	withSelectorArgs: selector
	forClass: aClass
	returning: aReturnType
	args: argsArray
!

initialize
    "Private - Initialize the receiver's class variables"
    LibraryList := OrderedCollection new
!

reset
    "Called on startup - Make DLD forget the handles of the libraries it
     used in the last session."
    LibraryList do: [ :each | each value: nil ]
!

libraryList
    "Answer a copy of the search path of libraries to be used by DLD"
    ^LibraryList copy
!

addLibrary: library
    "Add library to the search path of libraries to be used by DLD"
    LibraryList add: library -> nil

! !

DLD initialize!
Smalltalk addInit: [ DLD reset ]!
