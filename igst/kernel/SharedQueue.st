"======================================================================
|
|   SharedQueue Method Definitions
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


Object subclass: #SharedQueue
       instanceVariableNames: 'queueSem valueReady queue'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Processes'
!

SharedQueue comment: 
'My instances provide a guaranteed safe mechanism to allow for communication
between processes.  All access to the underlying data structures is
controlled with critical sections so that things proceed smoothly.' !



!SharedQueue class methodsFor: 'instance creation'!

new
    "Create a new instance of the receiver"
    ^self basicNew init
!!



!SharedQueue methodsFor: 'accessing'!

next
    "Wait for an object to be on the queue, then remove it and answer it"
    | result |
    valueReady wait.
    queueSem critical: [ result := queue removeFirst ].
    ^result
!

nextPut: value
    "Put value on the queue and answer it"
    queueSem critical: [ queue addLast: value ].
    valueReady signal.
    ^value
!!



!SharedQueue methodsFor: 'private'!

init
    queue := OrderedCollection new: 10.
    valueReady := Semaphore new.
    queueSem := Semaphore forMutualExclusion
!!
