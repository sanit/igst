"======================================================================
|
|   Message Method Definitions
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


Object subclass: #Message
       instanceVariableNames: 'selector args'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Implementation'
!

Message comment: 
'I am a virtually existent class.  By that I mean that logically instances of 
my class are created whenever a message is sent to an object, but in reality
my instances are only created to hold a message that has failed, so that
error reporting methods can examine the sender and arguments.' !
	   


!Message methodsFor: 'basic'!

printOn: aStream
    "Print a representation of the receiver on aStream"

    (self selector occurrencesOf: $:) = 0
        ifTrue: [
            self selector printOn: aStream.
            self selector first isLetter
                ifFalse: [ self arguments first printOn: aStream ]
        ]
        ifFalse: [
            (self selector substrings: $:) with: self arguments do: [:sel :arg |
                sel printOn: aStream.
                ': <' printOn: aStream.
                arg printOn: aStream.
                '> ' printOn: aStream
            ]
        ]
!

sendTo: aReceiver
    "Resend to aReceiver"
    ^aReceiver perform: selector withArguments: args
!

reinvokeFor: aReceiver
    "Resend to aReceiver - present for compatibility"
    ^aReceiver perform: selector withArguments: args
!!


!Message methodsFor: 'accessing'!

selector: aSymbol
    "Set the receiver's selector"
    selector := aSymbol
!

arguments: anArray
    "Set the receiver's arguments"
    args := anArray
!

selector
    "Answer the receiver's selector"
    ^selector
!

arguments
    "Answer the receiver's arguments"
    ^args
!!


!Message class methodsFor: 'creating instances'!

selector: aSymbol arguments: anArray
    "Create a new Message with the given selector and arguments"
    ^self new selector: aSymbol; arguments: anArray
!!

