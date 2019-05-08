"======================================================================
|
|   Exception handling classes Definitions
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



"Create these symbols. They'll be defined at the end of the file. Also create
 some classes"

Smalltalk at: #ExAll put: nil.
Smalltalk at: #ExHalt put: nil.
Smalltalk at: #ExError put: nil.
Smalltalk at: #ExDoesNotUnderstand put: nil.
Smalltalk at: #ExUserBreak put: nil.

Object subclass: #ExceptionHandler
    instanceVariableNames: 'onDoBlock exitBlock handlerBlock context'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Language-Exceptions'.

Object subclass: #Signal
    instanceVariableNames: 'exception arguments resumeBlock exitBlock onDoBlock'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Language-Exceptions'.

Object subclass: #TrappableEvent
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Language-Exceptions'!

TrappableEvent subclass: #Exception
    instanceVariableNames: 'parent resumable description defaultHandler'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Language-Exceptions'.

TrappableEvent subclass: #ExceptionCollection
    instanceVariableNames: 'collection'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Language-Exceptions'!

"The classes were created. Add their comments."

ExceptionHandler comment: '
I am used internally by the exception handling system. My instances record the
handler for an exception, the receiver of the #on:do:... method that
established that handler, and a block that leaves that #on:do:... method'.

TrappableEvent comment: '
I am an abstract class for arguments passed to #on:do:... methods in
BlockClosure. I define a bunch of methods that apply to Exceptions and
ExceptionCollections: they allow you to create ExceptionCollections and
examine all the exceptions to be trapped.'.

Exception comment: '
My instances describe a single event that can be trapped using #on:do:...,
contain whether such execution can be resumed after such an event, a
description of what happened, and a block that is used as an handler by
default. Using my methods you can raise exceptions and create new exceptions.
Exceptions are organized in a kind of hierarchy (different from the class
hierarchy): intercepting an exception will intercept all its children too.'.

ExceptionCollection comment: '
My instances are not real exceptions: they can only be used as arguments to
#on:do:... methods in BlockClosure. They act as shortcuts that allows you to
use the same handler for many exceptions without having to write duplicate
code'.

Signal comment: '
My instances describe an exception that has happened, and are passed to
exception handlers. Apart from containing information on the generated
exception and its arguments, they contain methods that allow you to resume
execution, leave the #on:do:... snippet, and pass the exception to an handler
with a lower priority.'!


!ExceptionHandler methodsFor: 'accessing'!

onDoBlock
    ^onDoBlock
!

exitBlock
    ^exitBlock
!

handlerBlock
    ^handlerBlock
!

isDisabled
    "Answer whether the receiver is disabled by a #valueWithUnwind."
    ^context ~~ ContextPart lastUnwindPoint
!

onDoBlock: wdBlock exitBlock: eBlock handlerBlock: hBlock
    "Initialize the receiver's instance variables."
    context := ContextPart lastUnwindPoint.
    onDoBlock := wdBlock.
    exitBlock := eBlock.
    handlerBlock := hBlock.
    ^self
!!


!ExceptionHandler class methodsFor: 'instance creation'!

onDoBlock: wdBlock exitBlock: eBlock handlerBlock: hBlock
    "Answer an ExceptionHandler instance with the given values
    for its instance variables."
    ^self new
         onDoBlock: wdBlock exitBlock: eBlock handlerBlock: hBlock
!!


!TrappableEvent methodsFor: 'instance creation'!

, aTrappableEvent
    "Answer an ExceptionCollection containing all the exceptions in the
     receiver and all the exceptions in aTrappableEvent"

    ^ExceptionCollection new
        add: self;
        add: aTrappableEvent;
        yourself
!!


!TrappableEvent methodsFor: 'enumerating'!

allExceptionsDo: aBlock
    "Execute aBlock, passing it an Exception for every exception in the
     receiver."

    self subclassResponsibility
!

handles: exception
    "Answer whether the receiver handles `exception'."

    self subclassResponsibility
!!


!TrappableEvent methodsFor: 'private'!

registerHandler: anExceptionHandler
    "Private - Register anExceptionHandler as an exception handler for the
     receiver"

    self subclassResponsibility
!

whenSignalledIn: onDoBlock do: handlerBlock exitBlock: exitBlock

    "Private - Create an ExceptionHandler from the arguments and register it"

    self registerHandler: (ExceptionHandler
        onDoBlock: onDoBlock
        exitBlock: exitBlock
        handlerBlock: handlerBlock)
!!


!Exception methodsFor: 'basic'!

copy
    "Answer a copy of the receiver"
    ^super copy
        description: self description copy;
        yourself
!!


!Exception methodsFor: 'accessing'!

defaultHandler
    "Answer the default handler for the receiver"

    ^defaultHandler
!

defaultHandler: aBlock
    "Set the default handler of the receiver to aBlock. A Signal object will
     be passed to aBlock"

    defaultHandler := aBlock
!

description
    "Answer a description of the receiver"
    ^description
!

description: aString
    "Set the description of the receiver to aString"
    description := aString
!

parent
    "Answer the parent of the receiver"
    ^parent
!

isResumable
    "Answer true if the receiver is resumable"
    ^resumable
!

isResumable: aBoolean
    "Set the resumable flag of the receiver to aBoolean"
    resumable := aBoolean
!!


!Exception methodsFor: 'instance creation'!

newChild
    "Answer a child exception of the receiver. Its properties are set to those
     of the receiver"

    ^self species basicNew
        description: self description copy;
        isResumable: self isResumable;
        defaultHandler: nil;
        parent: self;
        yourself
!!


!Exception methodsFor: 'exception handling'!

signal
    "Raise the exception described by the receiver, passing no parameters"
    ^self signalWithArguments: #()
!

signalWith: arg
    "Raise the exception described by the receiver, passing the parameter arg"
    ^self signalWithArguments: ((Array new: 1)
        at: 1 put: arg;
        yourself)
!

signalWith: arg with: arg2
    "Raise the exception described by the receiver, passing the parameters arg
     and arg2"

    ^self signalWithArguments: ((Array new: 2)
        at: 1 put: arg;
        at: 2 put: arg2;
        yourself)
!

signalWithArguments: args
    "Raise the exception described by the receiver, passing the parameters in
     args"

    | handlerData sig |

    handlerData := self nextHandler.
    sig := (Signal new)
        resumeBlock: [:object | ^object ]
        onDoBlock: handlerData onDoBlock
        exitBlock: handlerData exitBlock
        arguments: args
        exception: self.

    handlerData handlerBlock value: sig.
    sig pass
!!


!Exception methodsFor: 'enumerating'!

allExceptionsDo: aBlock
    "Private - Evaluate aBlock for every exception in the receiver. As it contains just one
     exception, evaluate it just once, passing the receiver"

    aBlock value: self
!

handles: exception
    "Answer whether the receiver handles `exception'."

    ^exception == self
!!


!Exception methodsFor: 'private'!

hasHandler
    "Private -  Determine whether the receiver exception has another handler
     for the same exception."
    self class handlers at: self ifPresent: [:val |
        "If there are no handlers, of course, proceed with the parent.
         But the handler is NOT valid even if there is an intervening
         call to #valueWithUnwind, so that's why we check with #isDisabled."
         ^val atEnd
             ifTrue: [ false ]
             ifFalse: [ val peek isDisabled not ]
    ].
    ^false
!

nextHandler
    "Private - Answer an handler for the anException exception. If it
     cannot found, look for an handler for its parent, until one is found or
     ExAll if reached and there is no handler. In this case, answer the default
     handler for anException. The answer is given as an ExceptionHandler."

    | exc handler |
    exc := self.
    [    self class handlers at: exc ifPresent: [:val |
             "If there are no handlers, of course, proceed with the parent.
              But the handler is NOT valid even if there is an intervening
              call to #valueWithUnwind, so that's why we check with #isDisabled."
             val atEnd ifFalse: [
                 val peek isDisabled ifFalse: [ ^val next ]
             ]
         ].
         exc == ExAll
    ]    whileFalse: [
         exc := exc parent
    ].
    ^ExceptionHandler
        onDoBlock: nil
        exitBlock: nil
        handlerBlock: self actualDefaultHandler
!

resetHandlers
    "Private - Reset the handlers for the receiver; that is, the next handler used will be
     the first that was declared"

    self class handlers at: self ifPresent: [:val | val reset]
!

actualDefaultHandler
    "Private - Answer the default handler for the receiver. It differs from
     #defaultHandler because if the default handler of the parent has to be
     used #defaultHandler answers nil, while #actualDefaultHandler calls
     #actualDefaultHandler for the parent and answers its result"

    ^defaultHandler isNil
        ifTrue: [self parent actualDefaultHandler]
        ifFalse: [defaultHandler]
!

registerHandler: anExceptionHandler
    "Private - Register anExceptionHandler as the information on a handler for
     the receiver"

    | currentContents |

    currentContents := #().
    self class handlers at: self ifPresent: [:val |
        val setToEnd.
        currentContents := val contents.
    ].

    self class handlers at: self put: 
        (ReadStream on: (Array with: anExceptionHandler), currentContents).
!!


!Exception methodsFor: 'private - accessing'!

parent: anException
    "Private - Set the parent of the receiver to anException"
    parent := anException
!!


!Exception class methodsFor: 'instance creation'!

new
    "Create a new exception whose parent is ExAll"

    ^ExAll newChild
!!


!Exception class methodsFor: 'private'!

handlers
    "Private, class - Answer the currently active handlers"

    ^Processor activeProcess exceptionHandlers
!

handlers: handlers
    "Private, class - Set the currently active handlers to the argument"

    Processor activeProcess exceptionHandlers: handlers
!

resetAllHandlers
    "Private, class - Reset the handlers for all the exceptions; that is, the
     next handlers used will be the first to be declared"

    self handlers do: [:each | each reset]
!

initialize
    "Private, class - Initialize the system exceptions"
    | nargs |

    (ExAll := self basicNew)
        description: 'An exception has occurred';
        isResumable: true;
        defaultHandler: [:sig |
            | receiver description |
            receiver := sig argumentCount > 1
                ifTrue: [ sig arguments at: 2 ]
                ifFalse: [ nil ].

            description := sig argumentCount > 0
                ifTrue: [ sig argument ]
                ifFalse: [ sig description ].
            
            receiver primError: description
        ].

    (ExError := ExAll newChild)
        description: '#error: was sent'.

    (ExHalt := ExAll newChild)
        description: 'halt encountered'.

    (ExUserBreak := ExAll newChild)
        description: 'interrupted!!!';
        isResumable: false.

    (ExDoesNotUnderstand := ExError newChild)
        description: 'did not understand selector';
        defaultHandler: [:sig |
            (sig arguments at: 2) primError:
                ('did not understand ', sig argument printString)
        ]
!!


!ExceptionCollection class methodsFor: 'instance creation'!

new
    "Private - Answer a new, empty ExceptionCollection"
    ^self basicNew
        collection: Set new
!!


!ExceptionCollection methodsFor: 'enumerating'!

allExceptionsDo: aBlock
    "Private - Evaluate aBlock for every exception in the receiver. Answer the
     receiver"

    collection do: aBlock
!

handles: exception
    "Answer whether the receiver handles `exception'."

    ^collection includes: exception
!!


!ExceptionCollection methodsFor: 'private'!

registerHandler: eh

    self allExceptionsDo: [:exc | exc registerHandler: eh ]
!!


!ExceptionCollection methodsFor: 'private - accessing'!

add: aTrappableEvent
    "Private - Add aTrappableEvent to the receiver and answer aTrappableEvent"
    aTrappableEvent allExceptionsDo: [ :exc |
        collection add: exc ].

    ^aTrappableEvent
!

collection: aSet
    "Private - Set the collection of exception included in the receiver to
     aSet"
    collection := aSet.
    ^self
!!


!Signal methodsFor: 'accessing'!

argumentCount
    "Answer how many arguments the receiver has"
    ^arguments size
!

argument
    "Answer the first argument of the receiver"
    ^arguments at: 1
!

arguments
    "Answer the arguments of the receiver"
    ^arguments
!

description
    "Answer the description of the raised exception"
    ^self exception description
!

exception
    "Answer the exception that was raised"
    ^exception
!!


!Signal methodsFor: 'exception handling'!

defaultAction
    "Execute the default handler for the raised exception"
    self exception actualDefaultHandler value: self
!

isNested
    "Answer whether the current exception handler is within the scope of
     another handler for the same exception."
    ^self exception hasHandler
!

outer
    "Raise the exception that instantiated the receiver, passing the same
     parameters.
     If the receiver is resumable and the evaluated exception action resumes
     then the result returned from #outer will be the resumption value of the
     evaluated exception action. If the receiver is not resumable or if the
     exception action does not resume then this message will not return, and
     #outer will be equivalent to #pass."

    ^self exception signalWithArguments: self arguments
!

pass
    "Yield control to the enclosing exception action for the receiver.
     Similar to #outer, but control does not return to the currently active exception
     handler."

    self return: self outer
!

resume
    "If the exception is resumable, resume the execution of the block that
     raised the exception; the method that was used to signal the exception
     will answer the receiver.
     Use this method IF AND ONLY IF you know who caused the exception and if
     it is possible to resume it in that particular case"

    self exception isResumable ifFalse: [
        self primError: 'Exception not resumable - #resume failed'
    ].

    self exception resetHandlers.
    resumeBlock value: self
!

resume: anObject
    "If the exception is resumable, resume the execution of the block that
     raised the exception; the method that was used to signal the exception
     will answer anObject.
     Use this method IF AND ONLY IF you know who caused the exception and if
     it is possible to resume it in that particular case"

    self exception isResumable ifFalse: [
        self primError: 'Exception not resumable - #resume: failed'
    ].

    self exception resetHandlers.
    resumeBlock value: anObject
!

retry
    "Re-execute the receiver of the #on:do: message. All handlers are
     reinstated: watch out, this can easily cause an infinite loop."

    onDoBlock isNil ifTrue: [
        self primError: 'No exception handler effective - #retry failed'
    ].
    Exception resetAllHandlers.
    self return: onDoBlock value
!

retryUsing: aBlock
    "Execute aBlock reinstating all handlers, and return its result from
     the #signal method."

    Exception resetAllHandlers.
    self return: aBlock value
!

return
    "Exit the #on:do: snippet, answering anObject to its caller"

    exitBlock isNil ifTrue: [
        self primError: 'No exception handler effective - #return failed'
    ].
    exitBlock value: nil
!

return: anObject
    "Exit the #on:do: snippet, answering anObject to its caller"

    exitBlock isNil ifTrue: [
        self primError: 'No exception handler effective - #return: failed'
    ].
    exitBlock value: anObject
!!


!Signal methodsFor: 'private'!

resumeBlock: rBlock onDoBlock: wdBlock exitBlock: eBlock arguments: args exception: exc
    "Private - Set the block to be executed to leave the #on:do:... message to eBlock;
     set the exception that was raised to exc; set its arguments to args
     set the receiver of the #on:do:... message to wdBlock;
     set the block to be executed to resume execution to rBlock"

    resumeBlock := rBlock.
    onDoBlock := wdBlock.
    exitBlock := eBlock.
    arguments := args.
    exception := exc.
    ^self
!!


!BlockClosure methodsFor: 'exception handling'!

ensure: aBlock
    "Evaluate the receiver; when any exception is signaled exit returning the
     result of evaluating aBlock; if no exception is raised, return the result
     of evaluating aBlock when the receiver has ended"

    | completing |

    completing := false.
    ^[  self value.
        completing := true.
        aBlock value
     ]  on: ExAll
        do: [:sig |
            completing ifFalse: [ completing := true. aBlock value ].
            sig pass ]
!

on: anException do: aBlock
    "Evaluate the receiver; when anException is signaled, evaluate aBlock
     passing a Signal describing the exception. Answer either the result of
     evaluating the receiver or the parameter of a Signal>>#return:"

    | handlers exitBlock |
    handlers := Exception handlers copy.
    exitBlock := [:returnValue |
        Exception handlers: handlers.
        ^returnValue].

    anException whenSignalledIn: self do: aBlock exitBlock: exitBlock.
    exitBlock value: self value
!

on: e1 do: b1 on: e2 do: b2
    "Evaluate the receiver; when e1 or e2 are signaled, evaluate respectively
     b1 or b2, passing a Signal describing the exception. Answer either the
     result of evaluating the receiver or the argument of a Signal>>#return:"

    | handlers exitBlock |
    handlers := Exception handlers copy.
    exitBlock := [:returnValue |
        Exception handlers: handlers.
        ^returnValue].

    e1 whenSignalledIn: self do: b1 exitBlock: exitBlock.
    e2 whenSignalledIn: self do: b2 exitBlock: exitBlock.
    exitBlock value: self value
!

on: e1 do: b1 on: e2 do: b2 on: e3 do: b3
    "Evaluate the receiver; when e1, e2 or e3 are signaled, evaluate
     respectively b1, b2 or b3, passing a Signal describing the exception.
     Answer either the result of evaluating the receiver or the parameter of a
     Signal>>#return:"

    | handlers exitBlock |
    handlers := Exception handlers copy.
    exitBlock := [:returnValue |
        Exception handlers: handlers.
        ^returnValue].

    e1 whenSignalledIn: self do: b1 exitBlock: exitBlock.
    e2 whenSignalledIn: self do: b2 exitBlock: exitBlock.
    e3 whenSignalledIn: self do: b3 exitBlock: exitBlock.
    exitBlock value: self value
!

on: e1 do: b1 on: e2 do: b2 on: e3 do: b3 on: e4 do: b4
    "Evaluate the receiver; when e1, e2, e3 or e4 are signaled, evaluate
     respectively b1, b2, b3 or b4, passing a Signal describing the exception.
     Answer either the result of evaluating the receiver or the parameter of a
     Signal>>#return:"

    | handlers exitBlock |
    handlers := Exception handlers copy.
    exitBlock := [:returnValue |
        Exception handlers: handlers.
        ^returnValue].

    e1 whenSignalledIn: self do: b1 exitBlock: exitBlock.
    e2 whenSignalledIn: self do: b2 exitBlock: exitBlock.
    e3 whenSignalledIn: self do: b3 exitBlock: exitBlock.
    e4 whenSignalledIn: self do: b4 exitBlock: exitBlock.
    exitBlock value: self value
!

on: e1 do: b1 on: e2 do: b2 on: e3 do: b3 on: e4 do: b4 on: e5 do: b5
    "Evaluate the receiver; when e1, e2, e3, e4 or e5 are signaled, evaluate
     respectively b1, b2, b3, b4 or b5, passing a Signal describing the exception.
     Answer either the result of evaluating the receiver or the parameter of a
     Signal>>#return:"

    | handlers exitBlock |
    handlers := Exception handlers copy.
    exitBlock := [:returnValue |
        Exception handlers: handlers.
        ^returnValue].

    e1 whenSignalledIn: self do: b1 exitBlock: exitBlock.
    e2 whenSignalledIn: self do: b2 exitBlock: exitBlock.
    e3 whenSignalledIn: self do: b3 exitBlock: exitBlock.
    e4 whenSignalledIn: self do: b4 exitBlock: exitBlock.
    e5 whenSignalledIn: self do: b5 exitBlock: exitBlock.
    exitBlock value: self value
!

ifCurtailed: aBlock
    "Evaluate the receiver; when any exception is signaled exit returning the
     result of evaluating aBlock; if no exception is raised, return the result
     of evaluating the receiver"

    ^self on: ExAll do: [:sig |
        aBlock value.
        sig pass
    ]
! !

Exception initialize!


!Object methodsFor: 'built ins'!

primError: message

    "This might start the debugger... Note that we use #basicPrint
     'cause #printOn: might invoke an error."
    
    Transcript initialize.
    stdout flush.
    self basicPrint.
    stdout
        nextPutAll: ' error: ';
        nextPutAll: message;
        nl.

    ContextPart backtrace; unwind
!

doesNotUnderstand: aMessage
    "Called by the system when a selector was not found. message is a
     Message containing information on the receiver"
    ExDoesNotUnderstand signalWith: aMessage selector with: self
!

error: message
    "Display a walkback for the receiver, with the given error message.
     Signal ExError"
    ExError signalWith: message with: self
!

halt: message
    "Display a walkback for the receiver, with the given error message.
     Signal ExHalt"
    ExHalt signalWith: message with: self
!

userInterrupt
    "Display a walkback for the receiver, signalling ExUserBreak."
    ExUserBreak signalWith: 'interrupted!!' with: self
!!

"
An example of exception handling follows:

| loopExitException random |
random := Random new.
loopExitException := ExAll newChild.
^[ [ true ] whileTrue: [
      random next < 0.2 ifTrue: [ loopExitException signal ].
      1 error: 'hello!'
   ]
 ] on: loopExitException do: [:sig | sig return: 'bye' ]
   on: ExError do: [:sig | sig arguments printNl. sig resume ]

"

