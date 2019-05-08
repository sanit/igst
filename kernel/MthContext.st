"======================================================================
|
|   MethodContext Method Definitions
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


ContextPart variableSubclass: #MethodContext
       instanceVariableNames: 'method methodClass selector receiver'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Implementation'
!

MethodContext comment: 
'My instances represent an actively executing method.  They record various
bits of information about the execution environment, and contain the
execution stack.' !


!MethodContext methodsFor: 'printing'!

printOn: aStream
    "Print a representation for the receiver on aStream"
    self receiver class printOn: aStream.

    self receiver class == self methodClass ifFalse: [
        aStream
            nextPut: $(;
            print: self methodClass;
            nextPut: $)
    ].
    aStream
        nextPutAll: '>>';
        print: self selector
! !



!MethodContext methodsFor: 'accessing'!

sender
    "Return the context from which the receiver was sent"
    ^self parentContext
!

method
    "Return the CompiledMethod being executed"
    ^method
!

methodClass
    "Return the class in which the CompiledMethod being executed is defined"
    ^methodClass
!

selector
    "Return the selector for the method being executed"
    ^selector
!

home
    "Answer the MethodContext to which the receiver refers
     (i.e. the receiver itself)"
    ^self
!

isBlock
    "Answer whether the receiver is a block context"
    ^false
!

receiver
    "Return the receiver (self) for the method being executed"
    ^receiver
! !
