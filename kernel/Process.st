"======================================================================
|
|   Process Method Definitions
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


Link subclass: #Process
     instanceVariableNames: 'suspendedContext priority myList name exceptionHandlers'
     classVariableNames: ''
     poolDictionaries: ''
     category: 'Language-Processes'
!

Process comment: 
'I represent a unit of computation.  My instances are independantly
executable blocks that have a priority associated with them, and they
can suspend themselves and resume themselves however they wish.' !


!Process class methodsFor: 'basic'!

on: aBlockContext at: aPriority
    "Private - Create a process running aBlockContext at the given priority"
    ^self new onBlock: aBlockContext at: aPriority
! !



!Process methodsFor: 'basic'!

suspend
    "Do nothing if we're already suspended. Note that the blue book made
     suspend a primitive - but the real primitive is yielding control to
     another process. Suspending is nothing more than taking ourselves out
     of every scheduling list and THEN yield control to another process"

    self isSuspended ifTrue: [ ^nil ].
    myList remove: self ifAbsent: [ ].
    myList := nil.
    self yield
!

terminate
    "Terminate the receiver - This is nothing more than prohibiting to
     resume the process, then suspending it."
    suspendedContext := nil.
    self suspend
!

forceResume
    "Private - Force a resume of the process from whatever status it was in
     (even if it was waiting on a semaphore).  This is BAD practice, it is
     present only for some future possibility."
    self isReady ifTrue: [ ^self ].
    self isSuspended ifFalse: [ myList remove: self ifAbsent: [ ] ].
    myList := Processor processesAt: self priority.
    myList addLast: self.
    self resume
! !



!Process methodsFor: 'printing'!

printOn: aStream
    "Print a representation of the receiver on aStream"

    aStream
        print: self class;
        nextPut: $(;
        print: name;
        nextPutAll: ' at ';
        nextPutAll: (Processor priorityName: self priority);
        nextPut: $,.

     "The order here is important!"
     self isActive ifTrue: [ aStream nextPutAll: ' active)'. ^self ].
     self isTerminated ifTrue: [ aStream nextPutAll: ' terminated)'. ^self ].
     self isWaiting ifTrue: [ aStream nextPutAll: ' waiting on a semaphore)'. ^self ].
     self isSuspended ifTrue: [ aStream nextPutAll: ' suspended)'. ^self ].
     self isReady ifTrue: [ aStream nextPutAll: ' ready to run)'. ^self ].
     aStream nextPutAll: ' undefined state)'.
!!


!Process methodsFor: 'accessing'!

name
    ^name
!

name: aString
    "Give the name aString to the process"
    name := aString
!

priority
    "Answer the receiver's priority"
    ^priority
!

priority: anInteger
    "Change the receiver's priority to anInteger"
    
    "If we are running, change the list"
    self isReady
        ifTrue: [ myList := Processor changePriorityOf: self to: anInteger ].
    priority := anInteger
!

queueInterrupt: aBlock
    "Force the receiver to be interrupted and to evaluate aBlock as soon as it
     becomes the active process (this could mean NOW if the receiver is active).
     Answer the receiver"

    self isActive
        ifTrue: [ aBlock value. ^self ].

    self isTerminated
        ifTrue: [ self error: 'cannot interrupt terminated process' ].

    suspendedContext := aBlock asContext: suspendedContext.
! !


!Process methodsFor: 'private'!

exceptionHandlers
    exceptionHandlers isNil ifTrue: [ exceptionHandlers := Dictionary new ].
    ^exceptionHandlers
!

exceptionHandlers: handlers
    exceptionHandlers := handlers.
!

onBlock: aBlock at: aPriority
    suspendedContext := aBlock.
    priority := aPriority.
    "By default we're suspended, so don't set myList!"
!

isActive
    "Answer whether the receiver is running"
    ^self == Processor activeProcess
!

isReady
    "Answer whether the receiver is not suspended nor waiting on a
     semaphore (maybe it is active, maybe it is not, though)"
    ^myList == (Processor processesAt: priority)
!

isSuspended
    "Answer whether the receiver is suspended through #suspend"
    ^myList isNil
!

isTerminated
    "Answer whether the receiver has already terminated"
    ^suspendedContext isNil
!

isWaiting
    "Answer whether the receiver is wating on a semaphore"
    ^self isReady not & self isSuspended not
! !

