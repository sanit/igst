"======================================================================
|
|   BlockContext Method Definitions
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


ContextPart variableSubclass: #BlockContext
       instanceVariableNames: 'numArgs numTemps outerContext home'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Implementation'
!

BlockContext comment: 
'My instances represent executing Smalltalk blocks, which are portions of
executeable code that have access to the environment that they were declared
in, take parameters, and result from BlockClosure objects created to be
executed by methods outside the current class.  Block contexts are
created by messages sent to compute a closure''s value.  They contain a stack
and also provide some methods that can be used in inspection or debugging.' !



!BlockContext methodsFor: 'printing'!

printOn: aStream
    "Print a representation for the receiver on aStream"
    aStream
        nextPutAll: '[] in ';
        print: self home methodClass;
        nextPutAll: '>>';
        print: self selector
! !


!BlockContext methodsFor: 'accessing'!

caller
    "Answer the context that called the receiver"
    ^self parentContext
!

numArgs
    "Answer the number of arguments passed to the receiver"
    ^numArgs
!

numTemps
    "Answer the number of temporaries used by the receiver"
    ^numTemps
!

outerContext
    "Answer the outer block/method context for the receiver"
    ^outerContext
!

method
    "Return the CompiledMethod being executed"
    ^home method
!

isBlock
    "Answer whether the receiver is a block context"
    ^true
!

receiver
    "Return the receiver (self) for the method being executed"
    ^home receiver
!

selector
    "Return the selector for the method being executed"
    ^home selector
!

home
    "Answer the MethodContext to which the receiver refers"
    ^home
! !


!BlockContext methodsFor: 'private'!

initBlock: closure
    "Private - Prepare the receiver to be started as a process"

    numArgs := closure argumentCount.
    numTemps := closure numTemps.
    outerContext := closure outerContext.
    home := outerContext home.
    ip := closure initialIP.
    sp := 0.
    parent := nil.
    outerContext nowHasBlock.
!!
