"======================================================================
|
|   Smalltalk Transcript object
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
 ======================================================================
"

Stream subclass: #TranscriptInterface
    instanceVariableNames: 'semaphore receiver selector'
    classVariableNames: 'Singleton'
    poolDictionaries: ''
    category: 'Streams'
!


!TranscriptInterface class methodsFor: 'accessing'!

new
    "Answer the only instance of the receiver -- which is stored in the
     global variable Transcript."
    Singleton isNil ifTrue: [ Singleton := self basicNew initialize ].
    ^Singleton
! !


!TranscriptInterface methodsFor: 'set up'!

message
    "Private - Answer an association representing the message to be sent
    to perform write operations.  The key is the receiver, the value is the
    selector"
    ^receiver -> selector
!

message: anAssociation
    "Private - Set the message to be sent to perform write operations
     to the one represented by anAssociation.  anAssociation's key is the
     receiver, while its value is the selector"
    receiver := anAssociation key.
    selector := anAssociation value
!!


!TranscriptInterface methodsFor: 'accessing'!

cr
    "Emit a new-line (carriage return) to the Transcript"
    self nl
!

nextPut: aCharacter
    "Emit aCharacter to the Transcript"
    self nextPutAll: (String with: aCharacter)
!

next: anInteger put: anObject
    "Write anInteger copies of anObject to the Transcript"
    self nextPutAll: (String new: anInteger withAll: anObject)
!

nextPutAll: aString
    "Write aString to the Transcript"
    semaphore critical: [
	receiver perform: selector with: aString.
	Delay idle
    ]
!

show: aString
    "Write aString to the Transcript"
    semaphore critical: [
	receiver perform: selector with: aString.
	Delay idle
    ]
!

showCr: aString
    "Write aString to the Transcript, followed by a new-line character"
    semaphore critical: [
	receiver perform: selector with: aString.
	receiver perform: selector with: Character nl asString.
	Delay idle
    ]
!

showOnNewLine: aString
    "Write aString to the Transcript, preceded by a new-line character"
    semaphore critical: [
	receiver perform: selector with: Character nl asString.
	receiver perform: selector with: aString.
	Delay idle
    ]
!!


!TranscriptInterface methodsFor: 'printing'!

print: anObject
    "Print anObject's representation to the Transcript"
    semaphore critical: [
	receiver perform: selector with: anObject printString.
	Delay idle
    ]
!

printOn: aStream
    "Print a representation of the receiver onto aStream"
    aStream nextPutAll: 'Transcript'
!!


!TranscriptInterface methodsFor: 'storing'!

store: anObject
    "Print Smalltalk code which evaluates to anObject on the Transcript"
    semaphore critical: [
	receiver perform: selector with: anObject storeString.
	Delay idle
    ]
!

storeOn: aStream
    "Print Smalltalk code which evaluates to the receiver onto aStream"
    aStream nextPutAll: 'Transcript'
!!


!TranscriptInterface methodsFor: 'private'!

initialize
    "Private - Initialize the receiver's instance variables"
    semaphore := Semaphore forMutualExclusion
!!

Smalltalk at: #Transcript put: TranscriptInterface new!
Transcript message: stdout -> #nextPutAll:!
