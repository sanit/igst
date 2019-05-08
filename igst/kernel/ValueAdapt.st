"======================================================================
|
|   ValueAdaptor hierarchy Method Definitions
|
|   $Revision: 1.6.2$
|   $Date: 1999/08/31 11:23:18$
|   $Author: pb$
|
 ======================================================================"


"======================================================================
|
| Copyright 1990, 91, 92, 94, 95, 99 Free Software Foundation, Inc.
| Written by Paolo Bonzini.
|
| This file is part of GNU-Smalltalk.
|
| GNU-Smalltalk is free software; you can redistribute it and/or modify it
| under the terms of the GNU General Public License as published by the Free
| Software Foundation; either version 2, or (at your option) any later version.
| 
| GNU-Smalltalk is distributed in the hope that it will be useful, but WITHOUT
| ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
| FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
| details.
| 
| You should have received a copy of the GNU General Public License along with
| GNU-Smalltalk; see the file COPYING.  If not, write to the Free Software
| Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
|
 ======================================================================"


Object subclass: #ValueAdaptor
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Data types'!

ValueAdaptor comment: 
'My subclasses are used to access data from different objects with a consistent
protocol. However, I''m an abstract class.' !

!ValueAdaptor methodsFor: 'basic'!

printOn: aStream
    "Print a representation of the receiver"
    aStream
        print: self class;
        nextPut: $(;
        print: self value;
        nextPut: $)
!!


!ValueAdaptor methodsFor: 'accessing'!

value: anObject
    "Set the value of the receiver. Must be implemented by ValueAdaptor's
     subclasses"
    self subclassResponsibility
!

value
    "Retrive the value of the receiver. Must be implemented by ValueAdaptor's
     subclasses"
    self subclassResponsibility
!!

!ValueAdaptor class methodsFor: 'creating instances'!

new
    "We don't know enough of subclasses to have a shared implementation of new"
    self shouldNotImplement
!!


ValueAdaptor subclass: #NullValueHolder
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Data types'
!

NullValueHolder comment: 
'I pretend to store my value in a variable, but I don''t actually.
You can use the only instance of my class (returned by `ValueHolder null'')
if you''re not interested in a value that is returned as described in
ValueHolder''s comment.'!

!NullValueHolder class methodsFor: 'creating instances'!

new
    "Not used -- use `ValueHolder null' instead"
    ^self shouldNotImplement
!

!NullValueHolder methodsFor: 'accessing'!

value: anObject
    "Set the value of the receiver. Do nothing, discard the value"
!

value
    "Retrive the value of the receiver. Always answer nil"
    ^nil
!!


ValueAdaptor subclass: #ValueHolder
       instanceVariableNames: 'value'
       classVariableNames: 'Null'
       poolDictionaries: ''
       category: 'Language-Data types'
!

ValueHolder comment: 
'I store my value in a variable. For example, you can use me to pass num-
bers by reference. Just instance me before calling a method and ask for
my value after that method. There are a lot of other creative uses for
my intances, though.' !

!ValueHolder methodsFor: 'accessing'!

value: anObject
    "Set the value of the receiver."
    value := anObject
!

value
    "Get the value of the receiver."
    ^value
!!

!ValueHolder class methodsFor: 'creating instances'!

new
    "Create a ValueHolder whose starting value is nil"
    ^self basicNew value: nil
!

null
    Null isNil ifTrue: [ Null := NullValueHolder basicNew ].
    ^Null
!

with: anObject
    "Create a ValueHolder whose starting value is anObject"
    ^self basicNew value: anObject
!!


ValueAdaptor subclass: #PluggableAdaptor
       instanceVariableNames: 'getBlock putBlock'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Data types'
!

PluggableAdaptor comment: 
'I mediate between complex get/set behavior and the #value/#value:
protocol used by ValueAdaptors. The get/set behavior can be implemented
by two blocks, or can be delegated to another object with messages 
such as #someProperty to get and #someProperty: to set.' !

!PluggableAdaptor methodsFor: 'accessing'!

value: anObject
    "Set the value of the receiver."
    putBlock value: anObject
!

value
    "Get the value of the receiver."
    ^getBlock value
!!

!PluggableAdaptor methodsFor: 'private'!

getBlock: get putBlock: put
    getBlock := get.
    putBlock := put.
    ^self
!!

!PluggableAdaptor class methodsFor: 'creating instances'!

getBlock: getBlock putBlock: putBlock
    "Answer a PluggableAdaptor using the given blocks to implement
     #value and #value:"
    ^self basicNew
        getBlock: getBlock
        putBlock: putBlock
!

on: anObject getSelector: getSelector putSelector: putSelector
    "Answer a PluggableAdaptor using anObject's getSelector message to
     implement #value, and anObject's putSelector message to implement
     #value:"

    ^self basicNew
        getBlock: [ anObject perform: getSelector ]
        putBlock: [ :value | anObject perform: putSelector with: value ]
!

on: anObject aspect: aSymbol
    "Answer a PluggableAdaptor using anObject's aSymbol message to
     implement #value, and anObject's aSymbol: message (aSymbol
     followed by a colon) to implement #value:"

    ^self
        on: anObject
        getSelector: aSymbol
        putSelector: (aSymbol, ':') asSymbol
!

on: anObject index: anIndex
    "Answer a PluggableAdaptor using anObject's #at: and #at:put:
     message to implement #value and #value:; the first parameter
     of #at: and #at:put: is anIndex"

    ^self
        getBlock: [ anObject at: anIndex ]
        putBlock: [ :value | anObject at: anIndex put: value ]
!

on: aDictionary key: aKey
    "Same as #on:index:. Provided for clarity and completeness."
    ^self
        on: aDictionary
        index: aKey
!!


PluggableAdaptor subclass: #DelayedAdaptor
       instanceVariableNames: 'value delayed'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Data types'
!

DelayedAdaptor comment: 
'I can be used where many expensive updates must be performed. My in-
stances buffer the last value that was set, and only actually set the
value when the #trigger message is sent. Apart from this, I''m equi-
valent to PluggableAdaptor.' !

!DelayedAdaptor methodsFor: 'accessing'!

trigger
    "Really set the value of the receiver."
    delayed ifTrue: [
        delayed := false.
        super value: value.
    ]
!

value: anObject
    "Set the value of the receiver - actually, the value is cached and
     is not set until the #trigger method is sent."
    value := anObject.
    delayed := true.
!

value
    "Get the value of the receiver."
    ^delayed
        ifTrue: [ value ]
        ifFalse: [ getBlock value ]
!!

!DelayedAdaptor methodsFor: 'private'!

getBlock: get putBlock: put
    delayed := false.
    ^super getBlock: get putBlock: put
!!
