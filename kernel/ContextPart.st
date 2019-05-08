"======================================================================
|
|   ContextPart Method Definitions
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



Object variableSubclass: #ContextPart
       instanceVariableNames: 'parent ip sp hasBlock'
       classVariableNames: 'UnwindPoints'
       poolDictionaries: ''
       category: 'Language-Implementation'
!

ContextPart comment: 
'My instances represent executing Smalltalk code, which represent the local
environment of executable code. They contain a stack and also provide some
methods that can be used in inspection or debugging.' !


!ContextPart class methodsFor: 'exception handling'!

backtrace
    "Print a backtrace from the caller to the bottom of the stack on the
     Transcript"
    thisContext parentContext backtraceOn: Transcript
!

backtraceOn: aStream
    "Print a backtrace from the caller to the bottom of the stack on aStream"
    thisContext parentContext backtraceOn: aStream
!

removeLastUnwindPoint
    "Private - Return and remove the last context marked as an unwind point,
     or our environment if the last unwind point belongs to another
     environment."

    | point environment |
    environment := thisContext environment.
    self unwindPoints isEmpty ifTrue: [ ^environment ].

    point := self unwindPoints removeLast.
    ^point value = environment
        ifTrue: [ point key ] ifFalse: [ environment ].
!

lastUnwindPoint
    "Private - Return the last context marked as an unwind point, or our en-
     vironment if the last unwind point belongs to another environment."

    | point environment |
    environment := thisContext environment.
    self unwindPoints isEmpty ifTrue: [ ^environment ].

    point := self unwindPoints at: self unwindPoints size.
    ^point value = environment
        ifTrue: [ point key ] ifFalse: [ environment ].
!

unwind
    "Return execution to the last context marked as an unwind point, returning
     nil on that stack."
    self unwind: nil
!

unwind: returnValue
    "Return execution to the last context marked as an unwind point, returning
     returnValue on that stack."
    | point |
    point := self removeLastUnwindPoint.
    point isProcess ifTrue: [
        Processor terminateActive
        "Bye bye, we never get past here."
    ].
    thisContext returnTo: point.
    ^returnValue
!

unwindPoints
    "Answer an OrderedCollection of contexts marked as unwind points."
    UnwindPoints isNil ifTrue: [ ^UnwindPoints := OrderedCollection new ].
    ^UnwindPoints
! !


!ContextPart methodsFor: 'printing'!

backtrace
    "Print a backtrace from the receiver to the bottom of the stack on the
     Transcript."
    self backtraceOn: Transcript
!

backtraceOn: aStream
    "Print a backtrace from the caller to the bottom of the stack on aStream."
    | ctx last |
    last := thisContext environment.
    ctx := self.
    [ ctx == last ] whileFalse: [
        ctx printOn: aStream.
        aStream nl.
        ctx := ctx parentContext
    ]
! !


!ContextPart methodsFor: 'accessing'!

environment
    "To create a valid execution environment for the interpreter even before
     it starts, GST creates a fake context whose selector is nil and which
     can be used as a marker for the current execution environment. This
     method answers that context.
     For processes, it answers the process block itself"
    | ctx next |
    ctx := self.
    [   next := ctx parentContext.
        ctx selector isNil | next isNil
    ]   whileFalse: [ ctx := next ].

    ^ctx
!

isProcess
    "Answer whether the receiver represents a process context, i.e. a context
     created by BlockClosure>>#newProcess. Such a context can be recognized
     because it has no parent but its selector is not nil, unlike the contexts
     created by the VM's prepareExecutionEnvironment function."
    ^self parentContext isNil & self selector notNil
!

parentContext
    "Answer the context that called the receiver"
    ^parent
!

ip
    "Answer the current instruction pointer into the receiver"
    "This funny implementation thwarts the compiler's optimizing effort"
    ^ip bitOr: 0
!

ip: newIP
    "Set the instruction pointer for the receiver"
    "Fixed typing isn't usually good, but this is too important"
    newIP isSmallInteger
        ifFalse: [ ^self error: 'invalid new ip' ].
    ip := newIP
!

size
    "Answer the number of valid fields for the receiver. Any read
     access from (self size + 1) to (self basicSize) has undefined
     results - even crashing"
    ^self sp
!

sp
    "Answer the current stack pointer into the receiver"
    "This funny implementation thwarts the compiler's optimizing effort"
    ^sp bitOr: 0
!

validSize
    "Answer how many elements in the receiver should be inspected"
    ^self size
!

sp: newSP
    "Set the stack pointer for the receiver"
    "Fixed typing isn't usually good, but this is too important"
    newSP isSmallInteger
        ifFalse: [ ^self error: 'invalid new sp' ].
    sp := newSP
!

hasBlock
    "Answer whether the receiver is the outer context for a BlockContext"
    ^hasBlock
!

method
    "Return the CompiledMethod being executed"
    self subclassResponsibility
!

isBlock
    "Answer whether the receiver is a block context"
    self subclassResponsibility
!

receiver
    "Return the receiver (self) for the method being executed"
    self subclassResponsibility
!

selector
    "Return the selector for the method being executed"
    self subclassResponsibility
!

home
    "Answer the MethodContext to which the receiver refers"
    self subclassResponsibility
! !


!ContextPart methodsFor: 'exception handling'!

mark
    "Add the receiver as a possible unwind point"
    ^self class unwindPoints addLast: self -> self environment
!

returnTo: aContext
    "Set the context to which the receiver will return"
    
    "Fixed typing isn't usually good, but this is too important"
    aContext class superclass == ContextPart
        ifFalse: [ ^self error: 'invalid returning context' ].
    parent := aContext
!

unmark
    "Remove the receiver from the contexts to which an unwind operation
     might return"
    | index |
    index := self class unwindPoints findLast: [ :each | each key == self ].
    index = 0 ifTrue: [ ^self ].

    self class unwindPoints size to: index do: [ :i |
        self class unwindPoints removeLast
    ]
! !



!ContextPart methodsFor: 'processes'!

nowHasBlock
    "Private - Tell the VM that the receiver has references in other
     objects"
    
    hasBlock := true
!!

