"======================================================================
|
|   DirectedMessage Method Definitions
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



Message subclass: #DirectedMessage
    instanceVariableNames: 'receiver '
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Language-Implementation'!

DirectedMessage comment: 
'I represent a message send: I contain the receiver, selector and arguments
for a message.'!
	   

!DirectedMessage methodsFor: 'basic'!

printOn: aStream
    "Print a representation of the receiver on aStream"

    aStream
        print: receiver;
        nextPutAll: ': '.

    super printOn: aStream
!

send
    "Send the message"
    ^self sendTo: receiver
!!


!DirectedMessage methodsFor: 'accessing'!

receiver: anObject
    "Change the receiver"
    receiver := anObject
!

receiver
    "Answer the receiver"
    ^receiver
!!


!DirectedMessage class methodsFor: 'creating instances'!

selector: aSymbol arguments: anArray 
    self shouldNotImplement
!

selector: aSymbol arguments: anArray receiver: anObject
    "Create a new instance of the receiver"

    ^(super selector: aSymbol arguments: anArray)
        receiver: anObject
!!

