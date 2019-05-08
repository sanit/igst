"======================================================================
|
|   Delay Method Definitions
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


Object subclass: #Delay
       instanceVariableNames: 'resumptionTime isRelative'
       classVariableNames: 'Queue TimeoutSem MutexSem DelayProcess IdleProcess'
       poolDictionaries: ''
       category: 'Language-Processes'
!

Delay comment: 
'I am the ultimate agent for frustration in the world.  I cause things to wait 
(typically much more than is appropriate, but it is those losing operating
system''s fault).  When a process sends one of my instances a wait message,
that process goes to sleep for the interval specified when the instance was
created.'
!
	   


!Delay class methodsFor: 'instance creation'!

forMilliseconds: millisecondCount
    "Answer a Delay waiting for millisecondCount milliseconds"
    ^self new init: millisecondCount isRelative: true
!

forSeconds: secondCount
    "Answer a Delay waiting for secondCount seconds"
    ^self forMilliseconds: secondCount * 1000
!

untilMilliseconds: millisecondCount
    "Answer a Delay waiting for millisecondCount milliseconds after midnight"
    ^self new init: millisecondCount isRelative: false
! !



!Delay class methodsFor: 'general inquiries'!

millisecondClockValue
    "Private - Answer the number of milliseconds since midnight"
    ^Time primMillisecondClock
! !



!Delay class methodsFor: 'initialization'!

initialize
    "Private - Initialize the receiver and the associated process"
    "'initalizing Delays' printNl."
    Queue := SortedCollection sortBlock:
	[ :a :b | (a key) <= (b key) ].
    MutexSem := Semaphore forMutualExclusion.
    TimeoutSem := Semaphore new.
    self startDelayLoop
!!


!Delay class methodsFor: 'private'!

idle
    "Private - Do nothing by default"
!

startDelayLoop
    "Private - Start the process for Delays"
    IdleProcess isNil ifTrue: [
	"This process is only here so that, if a process is waiting on a
	delay, no deadlock situation (no runnable process) occurs"
	IdleProcess := [
	    [ true ] whileTrue: [ self idle. Processor yield ]
	] newProcess.
	IdleProcess
	    name: 'idle';
	    priority: Processor systemBackgroundPriority.
    ].

    DelayProcess := [
        [ true ] whileTrue: [
            IdleProcess resume.
	    TimeoutSem wait.
	    IdleProcess suspend.
	    MutexSem critical: [
		Queue removeFirst value signal.
		Queue isEmpty
		    ifFalse: [ self timeout: Queue first key ]
	    ]
        ]
    ]   forkAt: Processor timingPriority.

    DelayProcess name: 'timeout'.
!

timeout: milliseconds
    "Private - Signal the TimeoutSem after the given number of milliseconds.
    Delays across midnight are gracefully handled."
    | resumeMillis |
    resumeMillis := milliseconds - Delay millisecondClockValue.
    resumeMillis := (resumeMillis \\ Time millisecondsPerDay) asInteger.
    Processor
        signal: TimeoutSem
        atMilliseconds: resumeMillis
! !



!Delay methodsFor: 'accessing'!

resumptionTime
    "Answer the time when a process waiting on a Delay will resume"
    isRelative
	ifTrue: [ ^Delay millisecondClockValue + resumptionTime ] 
	ifFalse: [ ^resumptionTime ] 
! !



!Delay methodsFor: 'process delay'!

wait
    "Wait until the amount of time represented by the instance of Delay
     elapses"
    | elt sem |
    Processor isTimeoutProgrammed
        ifFalse: [
            DelayProcess isNil ifFalse: [ DelayProcess terminate ].
            Delay initialize ].

    sem := Semaphore new.
    elt := Association key: self resumptionTime value: sem.
    MutexSem critical: [
        Queue add: elt.
	"If we've become the head of the list, we need to
	 alter the interrupt time"
	Queue first == elt ifTrue: [ Delay timeout: elt key ]
    ].
    sem wait
! !



!Delay methodsFor: 'comparing'!

= aDelay
    "Answer whether the receiver and aDelay denote the same delay"
    self class == aDelay class ifFalse: [ ^false ].

    ^(isRelative = aDelay isRelative)
	and: [ resumptionTime = aDelay basicResumptionTime ]
!

hash
    "Answer an hash value for the receiver"
    ^resumptionTime
! !



!Delay methodsFor: 'private'!

basicResumptionTime
    ^resumptionTime
!

init: milliseconds isRelative: aBoolean
    isRelative := aBoolean.
    resumptionTime := milliseconds
!

isRelative
    ^isRelative
! !
