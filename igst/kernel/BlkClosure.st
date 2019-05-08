"======================================================================
|
|   BlockClosure Method Definitions
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


Object variableByteSubclass: #BlockClosure
       instanceVariableNames: 'outerContext'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Implementation'
!

BlockClosure comment: 
'I am a factotum class.  My instances represent Smalltalk blocks, portions
of executeable code that have access to the environment that they were
declared in, take parameters, and can be passed around as objects to be
executed by methods outside the current class.
Block closures are sent a message to compute their value and create a new
execution context; this property can be used in the construction of
control flow methods.  They also provide some methods that are used in the
creation of Processes from blocks.

Implementation note: here is the use of the variable storage:
  - byte 1    = number of args
  - byte 2    = number of temps
  - bytes 3/4 = initial IP in machine order.' !



!BlockClosure methodsFor: 'private'!

asContext: parent
    "Answer a context which will evaluate the receiver without effects on
     the given context's stack (the return value won't be pushed), as
     soon as it becomes the VM's thisContext.
     parent can be nil - in which case reaching the end of the block will
     probably crash Smalltalk.
     Note that the block has no home, so it cannot contain returns."
    | block top |
    block := BlockContext basicNew: 64.
    parent isNil
        ifTrue: [ block initBlock: self ]
        ifFalse: [
            top := (parent sp == 0)
                ifTrue: [ parent receiver ]
                ifFalse: [ parent at: parent sp ].

            parent sp: parent sp - 1.
            block initBlock: [ self value. top ].
	    block returnTo: parent.
	].
    ^block
!!
    

!BlockClosure methodsFor: 'exception handling'!

valueWithUnwind
    "Evaluate the receiver. Any errors caused by the block will cause a
     backtrace, but execution will continue in the method that sent
     #valueWithUnwind, after that call. Example:
         [ 1 / 0 ] valueWithUnwind.
         'unwind works!' printNl.

     Important: this method is public, but it is intended to be used in
     very special cases. You should usually rely on #ensure: and #on:do: "
    | result |
    thisContext mark.
    result := self value.
    thisContext unmark.
    ^result
! !


!BlockClosure methodsFor: 'basic'!

whileTrue: aBlock
    "Evaluate the receiver. If it returns true, evaluate aBlock and re-
     start"

    "When the receiver is a block expression, whileTrue: is optimized
     by the compiler"
    [ self value ] whileTrue: [ aBlock value ].
    ^nil
!

whileFalse: aBlock
    "Evaluate the receiver. If it returns false, evaluate aBlock and re-
     start"

    "When the receiver is a block expression, whileFalse: is optimized
     by the compiler"
    [ self value ] whileFalse: [ aBlock value ].
    ^nil
!

whileTrue
    "Evaluate the receiver until it returns false"
    ^[ self value ] whileTrue: []
!

whileFalse
    "Evaluate the receiver until it returns true"
    ^[ self value ] whileFalse: []
!!



!BlockClosure methodsFor: 'multiple process'!

fork
    "Create a new process executing the receiver and start it"
    ^self newProcess resume; yourself
!

forkAt: priority
    "Create a new process executing the receiver with given priority
     and start it"
    ^(self newProcess priority: priority) resume; yourself
!

newProcess
    "Create a new process executing the receiver in suspended state.
     The priority is the same as for the calling process. The receiver
     must not contain returns"
    | closure |
    closure := [ self value.
    	      Processor terminateActive ].
    ^Process
        on: (closure asContext: nil)
        at: Processor activePriority
!

newProcessWith: anArray
    "Create a new process executing the receiver with the passed
     arguments, and leave it in suspended state. The priority is
     the same as for the calling process. The receiver must not
     contain returns"
    | closure |
    closure := [ self valueWithArguments: anArray.
    	      Processor terminateActive ].
    ^Process
        on: (closure asContext: nil)
        at: Processor activePriority
!!



!BlockClosure methodsFor: 'accessing'!

initialIP
    "Answer the initial instruction pointer into the receiver"
    | b1 b2 |
    b1 := self basicAt: 3.
    b2 := self basicAt: 4.
    ^Bigendian ifTrue: [ b1 * 256 + b2 ] ifFalse: [ b2 * 256 + b1 ]
!

argumentCount
    "Answer the number of arguments passed to the receiver"
    ^self basicAt: 1
!

numTemps
    "Answer the number of temporary variables used by the receiver"
    ^self basicAt: 2
!

outerContext
    "Answer the method/block context which is the immediate outer of
     the receiver"
    ^outerContext
! !
