"======================================================================
|
|   ProcessorScheduler Method Definitions
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


Object subclass: #ProcessorScheduler
       instanceVariableNames: 'processLists activeProcess'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Processes'
!

ProcessorScheduler comment: 
'I provide methods that control the execution of processes.' !


!ProcessorScheduler methodsFor: 'basic'!

activeProcess
    "Answer the active process"
    ^activeProcess
!

activePriority
    "Answer the active process' priority"
    ^self activeProcess priority
!

changePriorityOf: aProcess to: aPriority
    "Private - Move aProcess to the execution list for aPriority,
     answer the new execution list"

    aProcess priority isNil ifFalse: [
        (processLists at: aProcess priority) remove: aProcess ifAbsent: [ ]
    ].
    ^(processLists at: aPriority) addLast: aProcess; yourself
!

processesAt: aPriority
    "Private - Answer a linked list of processes at the given priority"
    ^processLists at: aPriority
!

terminateActive
    "Private - Terminate the active process"
    self activeProcess terminate
!

yield
    "Let the active process yield control to other processes"
    self activeProcess yield
! !


!ProcessorScheduler methodsFor: 'priorities'!

priorityName: priority
    "Private - Answer a name for the given process priority"
    ^#('priority 1' 'systemBackgroundPriority' 'userBackgroundPriority'
       'userSchedulingPriority' 'userInterruptPriority' 'lowIOPriority'
       'highIOPriority' 'timingPriority' 'priority 9') at: priority
!

timingPriority
    "Answer the priority for system real-time processes"
    ^8
!

highIOPriority
    "Answer the priority for system high-priority I/O processes"
    ^7
!

lowIOPriority
    "Answer the priority for system low-priority I/O processes"
    ^6
!

userInterruptPriority
    "Answer the priority for user interrupt-priority processes"
    ^5
!

userSchedulingPriority
    "Answer the priority for user standard-priority processes"
    ^4
!

userBackgroundPriority
    "Answer the priority for user background-priority processes"
    ^3
!

systemBackgroundPriority
    "Answer the priority for system background-priority processes"
    ^2

!!
