"======================================================================
|
|   CFunctionDescriptor Method Definitions
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


Object variableSubclass: #CFunctionDescriptor
       instanceVariableNames: 'cFunction cFunctionName returnType numFixedArgs'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-C interface'
!

CFunctionDescriptor comment: 
'I am not part of the Smalltalk definition.  My instances contain information
about C functions that can be called from within Smalltalk, such as number
and type of parameters.  This information is used by the C callout mechanism
to perform the actual call-out to C routines.' !



!CFunctionDescriptor class methodsFor: 'testing'!

addressOf: function
    "Answer the address (CObject) of the function which is registered (on
     the C side) with the given name, or zero if no such a function is
     registered."
    | descriptor |
    descriptor := self
        makeDescriptorFor: function
    	returning: #void		"dummy"
	withArgs: #().			"dummy"

    ^descriptor address
!

isFunction: function
    "Answer whether a function is registered (on the C side) with the
     given name."
    | descriptor |
    descriptor := self
        makeDescriptorFor: function
    	returning: #void		"dummy"
	withArgs: #().			"dummy"

    ^descriptor isValid
! !


!CFunctionDescriptor methodsFor: 'accessing'!

address
    "Answer the address (CObject) of the function represented by
     the receiver"
    ^cFunction
!

name
    "Answer the name of the function (on the C side) represented by the
    receiver"
    ^cFunctionName
!

isValid
    "Answer whether the function represented by the receiver is actually
    a registered one"
    ^cFunction address ~= 0
! !


!CFunctionDescriptor methodsFor: 'printing'!

printOn: aStream
    "Print a representation of the receiver onto aStream"
    aStream
        print: self class;
        nextPut: $(;
        nextPutAll: self name;
        nextPutAll: ' @ ';
        nextPutAll: (self address address radix: 16);
        nextPut: $)
! !

"A couple of simple, but useful callout functions, as examples."

SystemDictionary
	 defineCFunc: 'system'
         withSelectorArgs: 'system: aString'
	 returning: #int
	 args: #(string)!

SystemDictionary
	 defineCFunc: 'getenv'
         withSelectorArgs: 'getenv: aString'
	 returning: #string
	 args: #(string)!

SystemDictionary
	 defineCFunc: 'putenv'
         withSelectorArgs: 'putenv: aString'
	 returning: #int
	 args: #(string)!

SystemDictionary
	 defineCFunc: 'getArgc'
         withSelectorArgs: 'getArgc'
	 returning: #int
	 args: #()!

SystemDictionary
	 defineCFunc: 'getArgv'
         withSelectorArgs: 'getArgv: index'
	 returning: #string
	 args: #(int)!
