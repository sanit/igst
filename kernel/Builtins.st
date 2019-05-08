"=====================================================================
|
|   Smalltalk built in methods.  These are read in by the system 
|   initially, to prepare the execution environment.
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


!Integer methodsFor: 'built ins'!

+ arg
    "Sum the receiver and arg and answer another Number"
    <primitive: 1>
    ^self generality == arg generality
        ifFalse: [ self retry: #+ coercing: arg ]
        ifTrue: [ (LargeInteger fromInteger: self) +
            (LargeInteger fromInteger: arg) ]
!

- arg
    "Subtract arg from the receiver and answer another Number"
    <primitive: 2>
    ^self generality == arg generality
        ifFalse: [ self retry: #- coercing: arg ]
        ifTrue: [ (LargeInteger fromInteger: self) -
           (LargeInteger fromInteger: arg) ]
!

< arg
    "Answer whether the receiver is less than arg"
    <primitive: 3>
    ^self retry: #< coercing: arg
!

> arg
    "Answer whether the receiver is greater than arg"
    <primitive: 4>
    ^self retry: #> coercing: arg
!

<= arg
    "Answer whether the receiver is less than or equal to arg"
    <primitive: 5>
    ^self retry: #<= coercing: arg
!

>= arg
    "Answer whether the receiver is greater than or equal to arg"
    <primitive: 6>
    ^self retry: #>= coercing: arg
!

= arg
    "Answer whether the receiver is equal to arg"
    <primitive: 7>
    ^(arg isKindOf: Number) and: [ self retry: #= coercing: arg ]
!

== arg
    "Answer whether the receiver is the same object as arg"
    <primitive: 7>
    "if they aren't = by the primitive, they're not =="
    ^false
!

~= arg
    "Answer whether the receiver is not equal to arg"
    <primitive: 8>
    ^(arg isKindOf: Number) not or: [ self retry: #~= coercing: arg ]
!

~~ arg
    "Answer whether the receiver is not the same object as arg"
    <primitive: 8>
    ^true			"see comment above for =="
!

* arg
    "Multiply the receiver and arg and answer another Number"
    <primitive: 9>
    ^self generality == arg generality
        ifFalse: [ self retry: #* coercing: arg ]
        ifTrue: [ (LargeInteger fromInteger: self) *
           (LargeInteger fromInteger: arg) ]
!

/ arg
    "Divide the receiver by arg and answer another Integer or Fraction"
    <primitive: 10>
    " Create a Fraction when it's appropriate "
    arg = 0 ifTrue: [ ^self error: 'cannot divide by zero' ].
    ^arg class == self class
	ifTrue: [ (Fraction numerator: self denominator: arg) reduce ]
        ifFalse: [ self retry: #/ coercing: arg ]
!

\\ arg
    "Calculate the remainder of dividing receiver by arg (with truncation
     towards -infinity) and answer it"
    <primitive: 11>
    arg = 0 ifTrue: [ ^self error: 'cannot divide by zero' ].
    ^self retry: #\\ coercing: arg
!

// arg
    "Dividing receiver by arg (with truncation towards -infinity) and answer
     the result"
    <primitive: 12>
    arg = 0 ifTrue: [ ^self error: 'cannot divide by zero' ].
    ^self retry: #// coercing: arg
!

quo: arg
    "Dividing receiver by arg (with truncation towards zero) and answer
     the result"
    <primitive: 13>
    arg = 0 ifTrue: [ ^self error: 'cannot divide by zero' ].
    ^self retry: #quo: coercing: arg
!

bitAnd: arg
    "Do a bitwise AND between the receiver and arg, answer the result"
    <primitive: 14>
    ^(arg isKindOf: Integer)
        ifFalse: [ ^self error: 'bit operations only valid with integers' ]
        ifTrue: [ (LargeInteger fromInteger: self) bitAnd: arg ]
!

bitOr: arg
    "Do a bitwise OR between the receiver and arg, answer the result"
    <primitive: 15>
    ^(arg isKindOf: Integer)
        ifFalse: [ ^self error: 'bit operations only valid with integers' ]
        ifTrue: [ (LargeInteger fromInteger: self) bitOr: arg ]
!

bitXor: arg
    "Do a bitwise XOR between the receiver and arg, answer the result"
    <primitive: 16>
    ^(arg isKindOf: Integer)
        ifFalse: [ ^self error: 'bit operations only valid with integers' ]
        ifTrue: [ (LargeInteger fromInteger: self) bitXor: arg ]
!

bitShift: arg
    "Shift the receiver by arg places to the left if arg > 0,
     by arg places to the right if arg < 0, answer another Number"

    <primitive: 17>
    ^arg class == Integer
        ifFalse: [ ^self error: 'bit operations only valid with small integers' ]
        ifTrue: [ (LargeInteger fromInteger: self) bitShift: arg ]
!

asFloat
    "Convert the receiver to a Float, answer the result"
    <primitive: 40>
    self primitiveFailed
!

asObject
    "Answer the object whose index is in the receiver, fail if no object found
     at that index"
    <primitive: 76>
    self primitiveFailed
!

asObjectNoFail
    "Answer the object whose index is in the receiver, or nil if no object is
     found at that index"
    <primitive: 76>
    ^nil
! !


!LargeInteger methodsFor: 'built-ins'!

hash
    "Answer an hash value for the receiver"
    <primitive: 104>
    ^0
!

size
    "Answer the number of indexed instance variable in the receiver"
    <primitive: 62>
!

digitLength
    "Answer the number of base-256 digits in the receiver"
    <primitive: 62>
!

at: anIndex
    "Answer the anIndex-th byte in the receiver's representation"
    <primitive: 60>
    ^self mostSignificantByte
!

at: anIndex put: aNumber
    "Answer the anIndex-th byte in the receiver's representation"
    <primitive: 61>
   self isReadOnly ifTrue: [ ^self error: 'object is read-only' ].
   self checkIndexableBounds: anIndex
!

primReplaceFrom: start to: stop with: replacementString 
    startingAt: replaceStart    
    "Private - Replace the characters from start to stop with new
     characters contained in replacementString (which, actually, can be
     any variable byte class, starting at the replaceStart location of
     replacementString"
    <primitive: 105>
    ^self primitiveFailed
!

digitAt: anIndex
    "Answer the anIndex-th base-256 digit in the receiver's representation"
    <primitive: 60>
    ^self mostSignificantByte
!

digitAt: anIndex put: aNumber
    "Answer the anIndex-th base-256 digit in the receiver's representation"
    <primitive: 61>
   self isReadOnly ifTrue: [ ^self error: 'object is read-only' ].
   self checkIndexableBounds: anIndex
! !


!LargeInteger methodsFor: 'disabled'!

asObject
    "This method always fails. The number of OOPs is far less than
    the minimum number represented with a LargeInteger."
    self primitiveFailed
!

asObjectNoFail
    ^nil
! !


!Float methodsFor: 'built ins'!

+ arg
    "Sum the receiver and arg and answer another Number"
    <primitive: 41>
    ^self retry: #+ coercing: arg
!

- arg
    "Subtract arg from the receiver and answer another Number"
    <primitive: 42>
    ^self retry: #- coercing: arg
!

< arg
    "Answer whether the receiver is less than arg"
    <primitive: 43>
    ^self retry: #< coercing: arg
!

> arg
    "Answer whether the receiver is greater than arg"
    <primitive: 44>
    ^self retry: #> coercing: arg
!

<= arg
    "Answer whether the receiver is less than or equal to arg"
    <primitive: 45>
    ^self retry: #<= coercing: arg
!

>= arg
    "Answer whether the receiver is greater than or equal to arg"
    <primitive: 46>
    ^self retry: #>= coercing: arg
!

= arg
    "Answer whether the receiver is equal to arg"
    <primitive: 47>
    ^(arg isKindOf: Number) and: [ self retry: #= coercing: arg ]
!

~= arg
    "Answer whether the receiver is not equal to arg"
    <primitive: 48>
    ^(arg isKindOf: Number) and: [ self retry: #~= coercing: arg ]
!

* arg
    "Multiply the receiver and arg and answer another Number"
    <primitive: 49>
    ^self retry: #* coercing: arg
!

/ arg
    "Divide the receiver by arg and answer another Float"
    <primitive: 50>
    self generality = arg generality
        ifTrue: [ ^self error: 'cannot divide by zero' ]
        ifFalse: [^self retry: #/ coercing: arg ]
!

truncated
    "Truncate the receiver towards zero and answer the result"

    "It's bad that a built-in has such a long code, but I had to put
     it somewhere..."

    | exponent bytes positive float |
    <primitive: 51>

    self checkCoercion.
    (positive := self > 0)
        ifTrue: [ float := self ]
        ifFalse: [ float := self negated ].

    exponent := float exponent.
    bytes := ByteArray new: exponent // 8 + 2.
    float := float timesTwoPower: (exponent bitClear: 7) negated.

    bytes size - 1 to: 1 by: -1 do: [ :i |
         bytes at: i put: float truncated.
         float := float fractionPart timesTwoPower: 8
    ].

    ^positive
        ifTrue: [ (LargeInteger from: bytes) ]
        ifFalse: [ (LargeInteger from: bytes) negated ]
!

fractionPart
    "Answer the fractional part of the receiver"
    <primitive: 52>
    self checkCoercion.
!

exponent
    "Answer the exponent of the receiver in mantissa*2^exponent
     representation ( |mantissa|<=1 ) "
    <primitive: 53>
!

timesTwoPower: arg
    "Answer the receiver multiplied by 2^arg"
    <primitive: 54>
!

exp
    "Answer 'e' (2.718281828459...) raised to the receiver"
    <primitive: 160>
    self primitiveFailed
!

ln
    "Answer the logarithm of the receiver in base 'e' (2.718281828459...)"
    <primitive: 161>
    ^self error: 'cannot extract logarithm of zero or a negative number'
!

raisedTo: aNumber
    "Answer the receiver raised to its aNumber power"
    <primitive: 164>
    aNumber isFloat ifTrue: [ ^self primitiveFailed ].
    ^self raisedTo: aNumber asFloat
!

sqrt
    "Answer the square root of the receiver"
    <primitive: 166>
    ^self error: 'cannot extract square root of a negative number'
!

ceiling
    "Answer the integer part of the receiver, truncated towards +infinity"
    <primitive: 168>
    self checkCoercion.
!

floor
    "Answer the integer part of the receiver, truncated towards -infinity"
    <primitive: 169>
    self checkCoercion.
!

sin
    "Answer the sine of the receiver"
    <primitive: 177>
    self primitiveFailed
!

cos
    "Answer the cosine of the receiver"
    <primitive: 178>
    self primitiveFailed
!

tan
    "Answer the tangent of the receiver"
    <primitive: 176>
!

arcSin
    "Answer the arc-sine of the receiver"
    <primitive: 180>
    ^self error: 'argument out of range'
!

arcCos
    "Answer the arc-cosine of the receiver"
    <primitive: 181>
    ^self error: 'argument out of range'
!

arcTan
    "Answer the arc-tangent of the receiver"
    <primitive: 179>
! !



!Object methodsFor: 'built ins'!

checkIndexableBounds: index
   "Private - Check the reason why an access to the given indexed
    instance variable failed"

   self class isFixed ifTrue: [ ^self error: 'object not indexable' ].

   index isInteger ifFalse: [ ^self error: 'index not an integer' ].
   (index < 1) ifTrue: [ ^self error: 'index out of bounds' ].
   (index > self basicSize) ifTrue: [ ^self error: 'index out of bounds' ].

   ^self error: 'invalid argument type'
!

specialBasicAt: index
    "Similar to basicAt: but without bounds checking.  This method is used
     to support instance mutation when an instance's class definition is
     changed. This method must not be overriddent"
    <primitive: 263>

!

at: anIndex
    "Answer the index-th indexed instance variable of the receiver"
    <primitive: 60>
   self checkIndexableBounds: anIndex
!

basicAt: anIndex
    "Answer the index-th indexed instance variable of the receiver.
     This method must not be overridden, override at: instead"
    <primitive: 60>
   self checkIndexableBounds: anIndex
!

at: anIndex put: value
    "Store value in the index-th indexed instance variable of the receiver"
    <primitive: 61>
   self isReadOnly ifTrue: [ ^self error: 'object is read-only' ].
   self checkIndexableBounds: anIndex
!

basicAt: anIndex put: value
    "Store value in the index-th indexed instance variable of the receiver
     This method must not be overridden, override at:put: instead"

    <primitive: 61>
   self isReadOnly ifTrue: [ ^self error: 'object is read-only' ].
   self checkIndexableBounds: anIndex
!

size
    "Answer the number of indexed instance variable in the receiver"
    <primitive: 62>
!

basicSize
    "Answer the number of indexed instance variable in the receiver"
    <primitive: 62>
!

become: otherObject
    "Change all references to the receiver into references to otherObject.
     Depending on the implementation, references to otherObject might or
     might not be transformed into the receiver (respectively,
     'two-way become' and 'one-way become').
     Implementations doing one-way become answer the receiver (so that it is
     not lost). Most implementations doing two-way become answer otherObject,
     but this is not assured - so do answer the receiver for consistency.
     GNU Smalltalk does two-way become and answers otherObject, but this
     might change in future versions: programs should not rely on the behavior
     and results of #become: ."
    <primitive: 72>
!

instVarAt: index
    "Answer the index-th instance variable of the receiver.
     This method must not be overridden."
    <primitive: 73>

    ^self error: 'index out of bounds'
!

instVarAt: index put: value
    "Store value in the index-th instance variable of the receiver.
     This method must not be overridden."
    <primitive: 74>

   (index < 1)
        ifTrue: [ ^self error: 'index out of bounds' ].

   (index > (self basicSize + self class instSize))
        ifTrue: [ ^self error: 'index out of bounds' ]
        ifFalse: [ ^self error: 'invalid argument type' ]
!

isReadOnly
    "Answer whether the object's indexed instance variables can be
     written"

    <primitive: 233>
!

makeReadOnly: aBoolean
    "Set whether the object's indexed instance variables can be written"

    <primitive: 234>
    ^self error: 'boolean object required'
!

makeWeak
    "Make the object a 'weak' one. When an object is only referenced by weak
     objects, it is collected and the slots in the weak objects are changed to
     nils by the VM"

    <primitive: 253>
!

addToBeFinalized
    "Add the object to the list of objects to be finalized when there
     are no more references to them"

    <primitive: 256>
!

removeToBeFinalized
    "Remove the object from the list of objects to be finalized when there
     are no more references to them"

    <primitive: 257>
!

asOop
    "Answer the object index associated to the receiver. The object
     index doesn't change when garbage collection is performed."
    <primitive: 75>
!

identityHash
    "Answer an hash value for the receiver. This method must not be
     overridden"
    <primitive: 75>
!

hash
    "Answer an hash value for the receiver. This hash value is ok
     for objects that do not redefine ==."
    <primitive: 75>
!

nextInstance
    "Private - answer another instance of the receiver's class, or
     nil if the entire object table has been walked"
    <primitive: 78>
    ^nil
!

perform: selectorOrMessage
    "Send the unary message named selectorOrMessage (if a Symbol) to the
     receiver, or the message and arguments it identifies (if a Message
     or DirectedMessage).
     This method should not be overridden"
    <primitive: 83>
    selectorOrMessage isSymbol
        ifTrue: [ self error: 'wrong number of arguments' ].
    ^selectorOrMessage sendTo: self
!

perform: selector with: arg1
    "Send the message named selector (a Symbol) to the receiver,
     passing arg1 to it. This method should not be overridden"
    <primitive: 83>
    self error: 'wrong number of arguments'
!

perform: selector with: arg1 with: arg2
    "Send the message named selector (a Symbol) to the receiver, passing
    arg1 and arg2 to it. This method should not be overridden"
    <primitive: 83>
    self error: 'wrong number of arguments'
!

perform: selector with: arg1 with: arg2 with: arg3
    "Send the message named selector (a Symbol) to the receiver, passing
    arg1, arg2 and arg3 to it. This method should not be overridden"
    <primitive: 83>
    self error: 'wrong number of arguments'
!

perform: selector withArguments: argumentsArray
    "Send the message named selector (a Symbol) to the receiver, passing
    every element of argumentsArray to it. This method should not be
    overridden"
    <primitive: 84>
    argumentsArray isArray 
        ifFalse: [ self error: 'arguments must be supplied in an Array' ].
    self error: 'wrong number of arguments'
!

== arg
    "Answer whether the receiver is the same object as arg. This is a
     very fast test and is called 'identity' "
    <primitive: 110>
!

= arg
    "Answer whether the receiver is equal to arg. The equality test is
     by default the same as that for equal objects. = must not fail;
     answer false if the receiver cannot be compared to arg"
    <primitive: 110>
!

class
    "Answer the class to which the receiver belongs"
    <primitive: 111>
    self primitiveFailed
!

error: message
    "Stop the execution and/or bring up a debugger. message is an error
     message to be shown"
    <primitive: 131>
!

basicPrint
    "Print a basic representation of the receiver"
    <primitive: 252>
!

"### look these messages up to be sure"

halt
    "Called to enter the debugger"
    ^self halt: 'halt encountered'
!

halt: aString
    "Called to enter the debugger"
    ^self error: aString
!

mark: aSymbol
    "Private - use this method to mark code which needs to be reworked,
     removed, etc. You can then find all senders of #mark: to find all marked
     methods or you can look for all senders of the symbol that you sent to
     #mark: to find a category of marked methods."
!

primitiveFailed
    "Called when a VM primitive fails"
    ^self error: 'primitive operation failed'
!

shouldNotImplement
    "Called when objects belonging to a class should not answer a
     selector defined by a superclass"
    ^self error: 'message should not be implemented'
!

subclassResponsibility
    "Called when a method defined by a class should be overridden
     in a subclass"
    ^self error: 'the method is the responsibility of the subclass'
!

notYetImplemented
    "Called when a method defined by a class is not yet implemented,
     but is going to be"
    ^self error: 'not yet implemented'
! !


!Object methodsFor: 'VM callbacks'!

doesNotUnderstand: message
    "Called by the system when a selector was not found. message is a
     Message containing information on the receiver"
    <primitive: 130>
!

badReturnError
    "Called back when a block performs a bad return"
    
    ^self error: 'block returning from non-existing method context'
!

mustBeBoolean
    "Called by the system when ifTrue:*, ifFalse:*, and: or or: are sent to
     anything but a boolean"
    self error: 'boolean instance required'.
    ^true
!

noRunnableProcess
    "Called back when all processes are suspended"
    self error: 'no runnable process'.
!

userInterrupt
    "Called back when the user presses Ctrl-Break"
    self error: 'interrupted!'.
! !



!SystemDictionary methodsFor: 'builtins'!

resetStatistics
    "Reset the statistics about the VM which #printStatistics can print."
    <primitive: 162>
    ^self primitiveFailed
!

printStatistics
    "Print statistics about what the VM did since #resetStatistics was last
     called.  Meaningful only if gst was made with `make profile' or
     `make profile_vm' "
    <primitive: 163>
    ^self primitiveFailed
!

enableGC: aBoolean
    "Enable GC of Smalltalk objects referenced in callins/callouts"
    <primitive: 264>
    ^self primitiveFailed
!

compact
    "Force a full garbage collection"
    <primitive: 260>
    ^self primitiveFailed
!

quitPrimitive
    "Quit the Smalltalk environment. Whether files are closed and
     other similar cleanup occurs depends on the platform"
    <primitive: 113>
    self primitiveFailed
!

quitPrimitive: exitStatus
    "Quit the Smalltalk environment, passing the exitStatus integer
     to the OS. Whether files are closed and other similar cleanup
     occurs depends on the platform"
    <primitive: 117>
    ^self error: 'Integer expected, not ', exitStatus printString
!

monitor: aBoolean
    "Start or stop profiling the VM's execution (if GNU Smalltalk was compiled
    with support for monitor(2), of course)."
    <primitive: 230>
    self primitiveFailed
!


backtrace
    "Prints the method invocation stack backtrace, as an aid to debugging"
    <primitive: 140>
    self primitiveFailed
!

getTraceFlag: anIndex
    "Private - Returns a boolean value which is one of the interpreter's
     tracing flags"
    <primitive: 141>
    self primitiveFailed
!

setTraceFlag: anIndex to: aBoolean
    "Private - Sets the value of one of the interpreter's tracing flags
    (indicated by 'anIndex') to the value aBoolean."
    <primitive: 142>
    self primitiveFailed
!

spaceGrowRate
    "Answer the rate with which the amount of memory used by the system grows"
    <primitive: 153>
    ^self primitiveFailed
!

spaceGrowRate: rate
    "Set the rate with which the amount of memory used by the system grows"
    <primitive: 154>
    rate class == Float
        ifTrue: [ ^self error: 'Parameter must be > 0 and <= 500' ]
        ifFalse: [ ^self spaceGrowRate: rate asFloat ]
!

growThresholdPercent
    "Answer the percentage of the amount of memory used by the system grows
     which has to be full for the system to allocate more memory"
    <primitive: 155>
    ^self primitiveFailed
!

growThresholdPercent: growPercent
    "Set the percentage of the amount of memory used by the system grows
     which has to be full for the system to allocate more memory"
    <primitive: 156>
    growPercent class == Float
        ifTrue: [ ^self error: 'Parameter must be > 0 and < 100' ]
        ifFalse: [ ^self growThresholdPercent: growPercent asFloat ]
!


growTo: numBytes
    "Grow the amount of memory used by the system grows to numBytes."
    <primitive: 157>
    ^self primitiveFailed
!

byteCodeCounter
    "Answer the number of bytecodes executed by the VM"
    <primitive: 231>
!

debug				"for DBX.  Set breakpoint in debug() and 
				 invoke this primitive near where you want
				 to stop"
    "This methods provides a way to break in the VM code. It should not
     be used in user programs, because it is ineffective when the VM is not
     being debugged"
    <primitive: 232>
!

snapshot: aString
    "Save an image on the aString file"
    <primitive: 251>
!


"==========================================================================
 These are so useful throughout the loading of the kernel methods that I
 make an exception here and put in real methods instead of just primitives.
 "

executionTrace
    "Answer whether executed bytecodes are printed on stdout"
    ^self getTraceFlag: 1
!

executionTrace: aBoolean
    "Set whether executed bytecodes are printed on stdout"
    ^self setTraceFlag: 1 to: aBoolean
!

declarationTrace
    "Answer whether compiled bytecodes are printed on stdout"
    ^self getTraceFlag: 0
!

declarationTrace: aBoolean
    "Set whether compiled bytecodes are printed on stdout"
    ^self setTraceFlag: 0 to: aBoolean
!

verboseTrace
    "Answer whether execution tracing prints the object on the stack top"
    ^self getTraceFlag: 2
!

verboseTrace: aBoolean
    "Set whether execution tracing prints the object on the stack top"
    ^self setTraceFlag: 2 to: aBoolean
!

gcMessage
    "Answer whether messages indicating that garbage collection is taking
     place are printed on stdout"
    ^self getTraceFlag: 3
!

gcMessage: aBoolean 
    "Set whether messages indicating that garbage collection is taking
     place are printed on stdout"
    ^self setTraceFlag: 3 to: aBoolean
! !



!Behavior methodsFor: 'built ins'!

flushCache
    "Invalidate the method cache kept by the virtual machine. This
     message should not need to be called by user programs."
    <primitive: 89>
    ^self primitiveFailed
!


new
    "Create a new instance of a class with no indexed instance variables"
    <primitive: 70>
    self isFixed ifTrue: [ ^self primitiveFailed ].
    ^self error: 'Cannot send #new to an indexable class; try using #new: '
!

basicNew
    "Create a new instance of a class with no indexed instance variables"
    <primitive: 70>
    self isFixed ifTrue: [ ^self primitiveFailed ].
    ^self error: 'Cannot send #basicNw to an indexable class; try using #basicNew: '
!

new: numInstanceVariables
    "Create a new instance of a class with indexed instance variables. The
     instance has numInstanceVariables indexed instance variables."
    <primitive: 71>
    self isFixed ifTrue: [
        ^self error: 'cannot send #new: to a non-indexalbe class;',
            ' try using #new instead '
    ].
    numInstanceVariables isSmallInteger ifTrue: [ ^self primitiveFailed ].

    ^self error: 'number of indexed instance variables must be integer'
!

basicNew: numInstanceVariables
    "Create a new instance of a class with indexed instance variables. The
     instance has numInstanceVariables indexed instance variables."
    <primitive: 71>
    self isFixed ifTrue: [
        ^self error: 'cannot send #new: to a non-indexalbe class;',
            ' try using #new instead '
    ]
    numInstanceVariables isSmallInteger ifTrue: [ ^self primitiveFailed ].

    ^self error: 'number of indexed instance variables must be integer'
!

someInstance
    "Private - Answer the first instance of the receiver in the object
     table"
    <primitive: 77>
    ^nil			"return nil on failure"
!

methodsFor: category ifTrue: condition
    "Compile the following code inside the receiver, with the given
     category, if condition is true; else ignore it"
    <primitive: 151>
    ^self primitiveFailed
!

makeDescriptorFor: funcNameString 
  returning: returnTypeSymbol 
  withArgs: argsArray
    "Private - Answer a CFunctionDescriptor"

    <primitive: 249>
    ^self primitiveFailed
!

compileString: aString
    "Compile the code in aString, with no category. Fail if the code
     does not obey Smalltalk syntax. Answer the generated CompiledMethod
     if it does"
    <primitive: 235>
    ^self primitiveFailed
!

compileString: aString ifError: aBlock
    "Compile the code in aString, with no category. Evaluate aBlock
     (passing the file name, line number and description of the error) if
     the code does not obey Smalltalk syntax. Answer the generated
     CompiledMethod if it does"
    <primitive: 236>
    ^self primitiveFailed
! !



!Array methodsFor: 'built ins'!

at: anIndex
    "Answer the index-th indexed instance variable of the receiver."
    <primitive: 60>
   self checkIndexableBounds: anIndex
!

basicAt: anIndex
    "Answer the index-th indexed instance variable of the receiver.
     This method must not be overridden, override at: instead"
    <primitive: 60>
   self checkIndexableBounds: anIndex
!

at: anIndex put: value
    "Store value in the index-th indexed instance variable of the receiver"
    <primitive: 61>
   self isReadOnly ifTrue: [ ^self error: 'object is read-only' ].
   self checkIndexableBounds: anIndex
!

basicAt: anIndex put: value
    "Store value in the index-th indexed instance variable of the receiver
     This method must not be overridden, override at:put: instead"

    <primitive: 61>
   self isReadOnly ifTrue: [ ^self error: 'object is read-only' ].
   self checkIndexableBounds: anIndex
! !




!ContextPart methodsFor: 'built ins'!

blockCopy: numArgs temporaries: numTemps
    "Generate a BlockClosure starting execution two bytecodes after
     the end of the 'send special message #blockCopy:' bytecode"
    <primitive: 80>
    ^self primitiveFailed
! !


!BlockClosure methodsFor: 'built ins'!

value
    "Evaluate the receiver passing no parameters"
    <primitive: 81>
    self error: 'wrong number of arguments'
!

value: arg1
    "Evaluate the receiver passing arg1 as the only parameter"
    <primitive: 81>
    self error: 'wrong number of arguments'
!

value: arg1 value: arg2
    "Evaluate the receiver passing arg1 and arg2 as the parameters"
    <primitive: 81>
    self error: 'wrong number of arguments'
!

value: arg1 value: arg2 value: arg3
    "Evaluate the receiver passing arg1, arg2 and arg3 as the parameters"
    <primitive: 81>
    self error: 'wrong number of arguments'
!

valueWithArguments: argumentsArray
    "Evaluate the receiver passing argArray's elements as the parameters"
    <primitive: 82>
    argumentsArray isArray 
        ifFalse: [ self error: 'arguments must be supplied in an Array' ].
    self error: 'wrong number of arguments'
! !



!ArrayedCollection methodsFor: 'built ins'!

size
    "Answer the size of the receiver"
    <primitive: 62>

! !



!String class methodsFor: 'instance creation'!

fromCData: aCObject size: anInteger
    "Answer a String containing anInteger bytes starting at the location pointed
     to by aCObject"
    <primitive: 201>
    ^self primitiveFailed
! !



!String methodsFor: 'built ins'!

hash
    "Answer an hash value for the receiver"
    <primitive: 104>
    ^super hash
!

size
    "Answer the size of the receiver"
    <primitive: 62>
    ^self primitiveFailed
!

at: index
    "Answer the index-th character of the receiver."
    <primitive: 63>
    ^self error: 'index out of bounds'.
!

basicAt: index
    "Answer the index-th character of the receiver. This method must
     not be overridden; override at: instead. String overrides it so
     that it looks like it contains character objects even though it
     contains bytes"
    <primitive: 63>
    ^self error: 'index out of bounds'.
!

at: index put: value
    "Change the index-th character of the receiver."
    <primitive: 64>

   self isReadOnly
       ifTrue: [ ^self error: 'cannot write on a read-only object' ].

   (index < 1)
        ifTrue: [ ^self error: 'index out of bounds' ].

   (index > self basicSize)
        ifTrue: [ ^self error: 'index out of bounds' ]
        ifFalse: [ ^self error: 'invalid argument type' ]
!

basicAt: index put: value
    "Change the index-th character of the receiver. This method must
     not be overridden; override at: instead. String overrides it so
     that it looks like it contains character objects even though it
     contains bytes"
    <primitive: 64>

    self isReadOnly
        ifTrue: [ ^self error: 'cannot write on a read-only object' ].

    (index < 1)
        ifTrue: [ ^self error: 'index out of bounds' ].

    (index > self basicSize)
        ifTrue: [ ^self error: 'index out of bounds' ]
        ifFalse: [ ^self error: 'invalid argument type' ]
!

replaceFrom: start to: stop withByteArray: byteArray startingAt: replaceStart
    "Replace the characters from start to stop with new characters whose
     ASCII codes are contained in byteArray, starting at the replaceStart
     location of byteArray"
    <primitive: 105>
    ^super replaceFrom: start to: stop with: byteArray asString startingAt: replaceStart
!

replaceFrom: start to: stop with: aString startingAt: replaceStart
    "Replace the characters from start to stop with new characters whose
     ASCII codes are contained in aString, starting at the replaceStart
     location of aString"
    <primitive: 105>
    ^super replaceFrom: start to: stop with: aString startingAt: replaceStart
!

valueAt: index
    "Answer the ascii value of index-th character variable of the receiver"
    <primitive: 60>
    ^self error: 'index out of bounds'
!

valueAt: index put: value
    "Store (Character value: value) in the index-th indexed instance variable
     of the receiver"
    <primitive: 61>

   self isReadOnly
       ifTrue: [ ^self error: 'cannot write on a read-only object' ].

   (index < 1)
       ifTrue: [ ^self error: 'index out of bounds' ].

   (index > self basicSize)
       ifTrue: [ ^self error: 'index out of bounds' ]
       ifFalse: [ ^self error: 'invalid argument type' ]
!

primReplaceFrom: start to: stop with: replacementString 
    startingAt: replaceStart    
    "Private - Replace the characters from start to stop with new
     characters contained in replacementString (which, actually, can be
     any variable byte class, starting at the replaceStart location of
     replacementString"
    <primitive: 105>
    ^self primitiveFailed
!

asCData: aCType
    "Convert the receiver to a CObject with the given type"
    <primitive: 203>
    ^self primitiveFailed
! !



!Symbol class methodsFor: 'built ins'!

intern: aString
    "Private - Same as 'aString asSymbol'"
    <primitive: 134>
    ^self error: 'Attempted to intern non-string'
! !



!Symbol methodsFor: 'built ins'!

= aSymbol
    "Answer whether the receiver and aSymbol are the same object"
    <primitive: 110>
    ^false
!

hash
    "Answer an hash value for the receiver. Symbols are optimized
     for speed"
    <primitive: 75>

! !



!Character class methodsFor: 'built ins'!

asciiValue: anInteger
    "Returns the character object corresponding to anInteger.  Error if
     anInteger is not an integer, or not in 0..255."
     <primitive: 132>

    anInteger isInteger
        ifFalse: [ ^self error: 'invalid argument type' ]
        ifTrue: [ ^self error: 'integer out of range' ]
!

value: anInteger
    "Returns the character object corresponding to anInteger.  Error if
     anInteger is not an integer, or not in 0..255."
     <primitive: 132>

    anInteger isInteger
        ifFalse: [ ^self error: 'invalid argument type' ]
        ifTrue: [ ^self error: 'integer out of range' ]
! !



!Character methodsFor: 'built ins'!

= char
    "Boolean return value; true if the characters are equal"
    <primitive: 110>
!

asciiValue
    "Returns the integer value corresponding to self"
    <primitive: 133>
!

asInteger
    "Returns the integer value corresponding to self"
    <primitive: 133>
!

value
    "Returns the integer value corresponding to self"
    <primitive: 133>
! !



!Dictionary class methodsFor: 'built ins'!

new
    "Answer a new Dictionary. This method, actually, won't last long -
     until Dictionary.st is loaded"
    <primitive: 135>
    ^self primitiveFailed
! !



!Dictionary methodsFor: 'built ins'!

at: key
    "Answer the value associated with the given key in the receiver.
     This method, actually, won't last long - until Dictionary.st is loaded"
    <primitive: 128>
    ^self primitiveFailed
!

at: key put: value
    "Change the value associated with the given key in the receiver to value.
     This method, actually, won't last long - until Dictionary.st is loaded"
    <primitive: 129>
    ^self primitiveFailed

! !


!CompiledMethod class methodsFor: 'instance creation'!

literals: lits numArgs: numArg numTemps: numTemp
    primitive: primIndex bytecodes: bytecodes depth: depth

    "Answer a full fledged CompiledMethod. Construct the method
     header from the parameters, and set the literals and bytecodes
     to the provided ones."

    <primitive: 79>
    self primitiveFailed
! !


!ByteArray methodsFor: 'built ins'!

byteAt: index
    "Answer the index-th indexed instance variable of the receiver"
    <primitive: 60>
    ^self error: 'index out of bounds'
!

byteAt: index put: value
    "Store the `value' byte in the index-th indexed instance variable
     of the receiver"
    <primitive: 61>

   self isReadOnly
       ifTrue: [ ^self error: 'cannot write on a read-only object' ].

   (index < 1)
       ifTrue: [ ^self error: 'index out of bounds' ].

   (index > self basicSize)
       ifTrue: [ ^self error: 'index out of bounds' ]
       ifFalse: [ ^self error: 'invalid argument type' ]
!


hash
    "Answer an hash value for the receiver"
    <primitive: 104>
!

replaceFrom: start to: stop withString: aString startingAt: srcIndex
    "Replace the characters from start to stop with the
     ASCII codes contained in aString (which, actually, can be
     any variable byte class), starting at the srcIndex location of
     aString"
    <primitive: 105>
    ^self primitiveFailed
!

primReplaceFrom: start to: stop with: aByteArray startingAt: srcIndex
    "Private - Replace the characters from start to stop with the
     ASCII codes contained in aString (which, actually, can be
     any variable byte class), starting at the srcIndex location of
     aString"
    <primitive: 105>
    ^self primitiveFailed
!

asCData: aCType
    "Convert the receiver to a CObject with the given type"
    <primitive: 204>
    ^self primitiveFailed
! !




Smalltalk at: #File put: nil!


!FileStream methodsFor: 'built ins'!

fileOp: ioFuncIndex
    "Private - Used to limit the number of primitives used by FileStreams"
    <primitive: 254>
    file isNil ifTrue: [ self error: 'file already closed' ].
    File checkError.
    ^nil
!

fileOp: ioFuncIndex ifFail: aBlock
    "Private - Used to limit the number of primitives used by FileStreams."
    <primitive: 254>
    ^aBlock value
!

fileOp: ioFuncIndex with: arg1
    "Private - Used to limit the number of primitives used by FileStreams"
    <primitive: 254>
    file isNil ifTrue: [ self error: 'file already closed' ].
    File checkError.
    ^nil
!

fileOp: ioFuncIndex with: arg1 ifFail: aBlock
    "Private - Used to limit the number of primitives used by FileStreams."
    <primitive: 254>
    ^aBlock value
!

fileOp: ioFuncIndex with: arg1 with: arg2
    "Private - Used to limit the number of primitives used by FileStreams"
    <primitive: 254>
    file isNil ifTrue: [ self error: 'file already closed' ].
    File checkError.
    ^nil
!

fileOp: ioFuncIndex with: arg1 with: arg2 ifFail: aBlock
    "Private - Used to limit the number of primitives used by FileStreams."
    <primitive: 254>
    ^aBlock value
!

fileIn
    "File in the contents of the receiver"
    <primitive: 247>
    file isNil ifTrue: [ self error: 'file already closed' ].
    File checkError.
    ^nil
!

fileInLine: lineNum fileName: aString at: charPosInt
    "Private - Much like a preprocessor #line directive"
    <primitive: 248>
    file isNil ifTrue: [ self error: 'file already closed' ].
    File checkError.
    ^nil
! !



!Memory class methodsFor: 'basic'!

addressOfOOP: anObject
    "Returns the address of the OOP (object table slot) for anObject.
     The result is still valid after a garbage collection occurs."
    <primitive: 138>
!

addressOf: anObject
    "Returns the address of the actual object that anObject references.
     The result might be invalidated after a garbage collection occurs."
    <primitive: 139>
!

type: aType at: anAddress
    "Returns a particular type object from memory at anAddress"
    <primitive: 145>
    ^self primitiveFailed
!

type: aType at: anAddress put: aValue
    "Sets the memory location anAddress to aValue"
    <primitive: 146>
    ^self primitiveFailed
! !



!ByteMemory class methodsFor: 'basic'!

at: address
    "Returns the byte at address as an integer"
    <primitive: 136>
!

at: address put: value
    "Sets the byte at ADDRESS (an integer) to be VALUE (INTEGER 0..255)"
    <primitive: 137>

! !



!Time class methodsFor: 'builtins'!

secondClock
    "Returns the number of seconds to/since 1/1/2000. This method won't
     last long - until Time.st restores correct Blue Book semantics"
    <primitive: 98>
    ^self primitiveFailed
!

primSecondClock
    "Returns the number of seconds to/from 1/1/2000."
    <primitive: 98>
    ^self primitiveFailed
!

millisecondClock
    "Returns the number of milliseconds since midnight. This method won't
     last long - until Time.st restores correct Blue Book semantics"
    <primitive: 99>
   ^self primitiveFailed
!

primMillisecondClock
    "Returns the number of milliseconds since midnight."
    <primitive: 99>
   ^self primitiveFailed
! !



!Process methodsFor: 'builtins'!

resume
    "Resume the receiver's execution"
    <primitive: 87>
   ^self primitiveFailed
!

yield
    "Yield control from the receiver to other processes"
    <primitive: 88>
    "If we are not executing the primitive fails - but that's not our problem"
    ^nil
! !


!ProcessorScheduler methodsFor: 'timed invocation'!

isTimeoutProgrammed
    "Private - Answer whether there is a pending call to
    #signal:atMilliseconds:"
    <primitive: 101>
!

signal: aSemaphore atMilliseconds: millis
    "Private - signal 'aSemaphore' after 'millis' milliseconds have elapsed"
    <primitive: 100>
    ^self primitiveFailed
!

signal: aSemaphore onInterrupt: anIntegerSignalNumber
    "Private - signal 'aSemaphore' when the given C signal occurs"
    <primitive: 152>
    ^self primitiveFailed
! !



!Semaphore methodsFor: 'builtins'!

"synchronization"

signal
    "Signal the receiver, resuming a waiting process' if there is one"
    <primitive: 85>
    ^self primitiveFailed
!

wait
    "Wait for the receiver to be signalled, suspending the executing process
     if it is not yet"
    <primitive: 86>
    ^self primitiveFailed
! !



!ClassDescription methodsFor: 'builtins'!

category: aString
    "Define a category for the receiver"

    category := aString
!

comment: aString
    "Define a comment for the receiver"

    comment := aString
    "This method is present so that comment declarations can always work, even
     before the real method is defined."
! !



!CObject class methodsFor: 'instance creation'!

alloc: nBytes
    "Allocate nBytes bytes and return an instance of the receiver"
    <primitive: 144>
    ^self error: 'invalid argument'
!

new: nBytes
    "Allocate nBytes bytes and return an instance of the receiver"
    <primitive: 144>
    ^self error: 'invalid argument'
!

alloc: nBytes type: cTypeObject
    "Allocate nBytes bytes and return a CObject of the given type"
    <primitive: 158>
    ^self error: 'invalid argument'
! !



!CObject methodsFor: 'C data access'!

"These used to be 147 and 148"

at: byteOffset type: aType
    "Answer some data of the given type from byteOffset bytes after
     the pointer stored in the receiver"
    <primitive: 183>
    ^self error: 'invalid argument(s)'
!
    
at: byteOffset put: aValue type: aType
    "Store aValue as data of the given type from byteOffset bytes after
     the pointer stored in the receiver"
    <primitive: 185>

    "Attempt to store something meaningful from another CObject"
    ^self at: byteOffset noCObjectsPut: aValue cObjStoredValue type: aType
!

free
    "Free the receiver's pointer and set it to null. Big trouble hits
     you if the receiver doesn't point to the base of a malloc-ed area."
    <primitive: 165>
    ^self primitiveFailed
! !



!CObject methodsFor: 'private'!

adjPtrBy: byteOffset
    <primitive: 187>
    ^self primitiveFailed
!

at: byteOffset noCObjectsPut: aValue type: aType
    "Private - Store aValue as data of the given type from byteOffset bytes
     after the pointer stored in the receiver. This version refuses CObjects
     for `aValue'. "
    <primitive: 185>
    ^self error: 'invalid argument(s)'
!

derefAt: byteOffset type: aType
    <primitive: 182>
    ^self primitiveFailed
!

derefAt: byteOffset put: aValue type: aType
    <primitive: 184>

    "Attempt to store something meaningful from another CObject"
    ^self derefAt: byteOffset noCObjectsPut: aValue cObjStoredValue type: aType
!

derefAt: byteOffset noCObjectsPut: aValue type: aType
    <primitive: 184>
    ^self error: 'invalid argument(s)'
!

ptrDiff: subtractedPtr elementSize: anInteger derefFirst: derefFirst
    "Private - Basically, this method subtracts subtractedPtr's memory address
     from the receiver's memory address, and divides by anInteger.  If
     derefFirst is false, the receiver is a CArray, so the process is
     exactly as above.  If it is true, however, the receiver and subtractedPtr
     are either CPtr's or CString's, so the VM dereferences the address they
     contain to a `char *' to obtain the memory addresses to subtract."

    <primitive: 186>
    ^self primitiveFailed
! !


"These are stubs...they will be replaced with the appropriate class
from Class.st and UndefObject.st.  These allow for Smalltalk type class
declarations of the built-in classes, so that they may be edited and
modified.  This mostly present to allow for future enhancement in which
the Smalltalk source files take a more active role in the definition of
the system, and the C definition of the classes diminishes in importance."

!Class methodsFor: 'builtins'!

subclass: classNameString
  instanceVariableNames: stringInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryNameString
    (Smalltalk at: classNameString) category: categoryNameString
!

variableSubclass: classNameString
  instanceVariableNames: stringInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryNameString
    (Smalltalk at: classNameString) category: categoryNameString
!

variableWordSubclass: classNameString
  instanceVariableNames: stringInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryNameString
    (Smalltalk at: classNameString) category: categoryNameString
!

variableByteSubclass: classNameString
  instanceVariableNames: stringInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryNameString
    (Smalltalk at: classNameString) category: categoryNameString
! !



!UndefinedObject methodsFor: 'builtins'!

subclass: classNameString
  instanceVariableNames: stringInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryNameString
    (Smalltalk at: classNameString) category: categoryNameString
!

variableSubclass: classNameString
  instanceVariableNames: stringInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryNameString
    (Smalltalk at: classNameString) category: categoryNameString
!

variableWordSubclass: classNameString
  instanceVariableNames: stringInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryNameString
    (Smalltalk at: classNameString) category: categoryNameString
!

variableByteSubclass: classNameString
  instanceVariableNames: stringInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryNameString
    (Smalltalk at: classNameString) category: categoryNameString
! !

