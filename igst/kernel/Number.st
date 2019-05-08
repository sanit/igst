"======================================================================
|
|   Number Method Definitions
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


Magnitude subclass: #Number
	  instanceVariableNames: ''
	  classVariableNames: ''
	  poolDictionaries: ''
	  category: 'Language-Data types'
!

Number comment: 
'I am an abstract class that provides operations on numbers, both floating
point and integer.  I provide some generic predicates, and supply the 
implicit type coercing code for binary operations.' !


!Number class methodsFor: 'converting'!

coerce: aNumber
    "Answer aNumber - whatever class it belongs to, it is good"
    ^aNumber
!

readFrom: aStream
    "Answer the number read from the rest of aStream, converted to an
     instance of the receiver. If the receiver is number, the class of the
     result is undefined -- but the result is good."
    ^self coerce: (aStream contents asString asNumber)
! !


!Number class methodsFor: 'testing'!

isImmediate
    "Answer whether, if x is an instance of the receiver, x copy == x"
    ^true
! !


!Number methodsFor: 'copying'!

shallowCopy
    "Return the receiver - it's an immediate (immutable) object"
    ^self
!

deepCopy
    "Return the receiver - it's an immediate (immutable) object"
    ^self
!!


!Number methodsFor: 'converting'!

degreesToRadians
    "Convert the receiver to radians"
    ^self asFloat / 57.295779513082320876846364344191
!

radiansToDegrees
    "Convert the receiver from radians to degrees"
    ^self asFloat * 57.295779513082320876846364344191
!

coerce: aNumber
    "Answer aNumber - whatever class it belongs to, it is good"
    ^aNumber
!

zero
    "Coerce 0 to the receiver's class. The default implementation works,
     but is inefficient"
    ^self coerce: 0
!

unity
    "Coerce 1 to the receiver's class. The default implementation works,
     but is inefficient"
    ^self coerce: 1
!

generality
    "Answer the receiver's generality"
    self subclassResponsibility
!

retry: aSymbol coercing: aNumber
    "Coerce the number (between the receiver and aNumber) which has the lowest
     generality to the other's number class, and retry calling aSymbol."

    | selfGen aNumGen |
    (aSymbol == #= or: [ aSymbol == #~= ])
    	ifTrue: [ (aNumber isKindOf: Number) 
		      "Return false for =, true for ~="
		      ifFalse: [ ^aSymbol ~~ #= ] ].
    selfGen := self generality.
    aNumGen := aNumber generality.
    selfGen > aNumGen
    	ifTrue: [ ^self perform: aSymbol with: (self coerce: aNumber) ].
    selfGen < aNumGen
    	ifTrue: [ ^(aNumber coerce: self) perform: aSymbol with: aNumber ].

    ^self error: 'retry:coercing: called with arguments of the same generality' 
!!



!Number methodsFor: 'arithmetic'!

+ aNumber
    "Sum the receiver and aNumber, answer the result"
    self subclassResponsibility
!

- aNumber
    "Subtract aNumber from the receiver, answer the result"
    self subclassResponsibility
!

* aNumber
    "Subtract the receiver and aNumber, answer the result"
    self subclassResponsibility
!

/ aNumber
    "Divide the receiver by aNumber, answer the result (no loss of
     precision)"
    self subclassResponsibility
!

// aNumber
    "Divide the receiver by aNumber, answer the result truncated towards
     -infinity"
    self subclassResponsibility
!

\\ aNumber
    "Divide the receiver by aNumber truncating towards -infinity, answer
     the remainder"
    self subclassResponsibility
!

quo: aNumber
    "Return the integer quotient of dividing the receiver by aNumber with
    truncation towards zero."
    ^(self / aNumber) truncated
!

rem: aNumber
    "Return the remainder of dividing the receiver by aNumber with
    truncation towards zero."
    ^self - ((self quo: aNumber) * aNumber)
!!



!Number methodsFor: 'truncation and round off'!

asInteger
    "Answer the receiver, truncated towards zero"
    ^self truncated
!

truncated
    "Answer the receiver, truncated towards zero"
    ^self subclassResponsibility
!

truncateTo: aNumber
    "Answer the receiver, truncated towards zero to a multiple
     of aNumber"
    ^(self / aNumber) truncated * aNumber
!

rounded
    "Returns the integer nearest the receiver"
    ^(self + 0.5) truncated
!

roundTo: aNumber
    "Answer the receiver, truncated to the nearest multiple
     of aNumber"
    ^(self / aNumber) rounded * aNumber
!!



!Number methodsFor: 'testing'!

isRational
    "Answer whether the receiver is rational - false by default"
    ^false
!

isNumber
    ^true
!

negative
    "Answer whether the receiver is < 0"
    ^self < self zero
!

positive
    "Answer whether the receiver is >= 0"
    ^self >= self zero
!

strictlyPositive
    "Answer whether the receiver is > 0"
    ^self > self zero
!

sign
    "Returns the sign of the receiver."
    self < self zero ifTrue: [ ^-1 ].
    self > self zero ifTrue: [ ^1 ].
    ^0
!

even
    "Returns true if self is divisible by 2"
    ^self truncated even
!

odd
    "Returns true if self is not divisible by 2"
    ^self truncated odd
!!



!Number methodsFor: 'misc math'!

squared
    "Answer the square of the receiver"
    ^self * self
!

abs
    "Answer the absolute value of the receiver"
    ^self > self zero
        ifTrue: [ self ]
        ifFalse: [ self negated ]
!

negated
    "Answer the negated of the receiver"
    ^self zero - self
!

sin
    "return the sine of the receiver"
    ^self asFloat sin
!

cos
    "return the cosine of the receiver"
    ^self asFloat cos
!

tan
    "return the tangent of the receiver"
    ^self asFloat tan
!

arcSin
    "return the arc sine of the receiver"
    ^self asFloat arcSin
!

arcCos
    "return the arc cosine of the receiver"
    ^self asFloat arcCos
!

arcTan
    "return the arc tangent of the receiver"
    ^self asFloat arcTan
!

sqrt
    "return the square root of the receiver"
    ^self asFloat sqrt
!

exp
    "return e raised to the receiver"
    ^self asFloat exp
!

ln
    "return log base e of the receiver"
    ^self asFloat ln
!

estimatedLog
    "Answer an estimate of (self abs floorLog: 10). This method
     should be overridden by subclasses, but Number's implementation
     does not raise errors - simply, it gives a correct result, so
     it is slow."
    ^self abs floorLog: 10
!

log: aNumber
    "return log base aNumber of the receiver"
    ^self asFloat ln / aNumber asFloat ln
!

floorLog: radix
    "return (self log: radix) floor. Optimized to answer an integer."

    | me answer |

    self < self zero ifTrue: [
       ^self error: 'cannot extract logarithm of a negative number'
    ].
    radix <= radix unity ifTrue: [
        (radix <= radix zero) ifTrue: [ ^self error: 'bad radix' ].
        (radix = radix unity) ifTrue: [ ^self error: 'bad radix' ].
        ^(self floorLog: radix reciprocal) negated
    ].

    me := self.
    self < self unity
        ifTrue: [
            answer := -1.
            [   me := me * radix. me < me unity ] whileTrue: [
                answer := answer - 1
            ]
        ]
        ifFalse: [
            answer := 0.
            [   me > radix ] whileTrue: [
                me := me / radix.
                answer := answer + 1
            ]
        ].

   ^answer
!

raisedTo: aNumber
    "Return self raised to aNumber power"

    ^aNumber isInteger
        ifTrue: [ self raisedToInteger: aNumber ]
        ifFalse: [ self asFloat raisedTo: aNumber asFloat ]
!

raisedToInteger: anInteger
    "Return self raised to the anInteger-th power"
    | result y n |

    "Some special cases first"
    anInteger isInteger ifFalse: [ ^self error: 'bad argument type' ].
    anInteger < 0 ifTrue: [ ^self reciprocal raisedToInteger: 0 - anInteger ].
    anInteger = 0 ifTrue: [ ^1 ].
    anInteger = 1 ifTrue: [ ^self ].

    result := self unity.
    y := self.
    n := anInteger.
    [   (n bitAnd: 1) == 1 ifTrue: [ result := result * y ].
        n := n bitShift: -1. n > 0
    ]   whileTrue: [ y := y * y ].

    ^result
!!



!Number methodsFor: 'truncation and round off'!

floor
    "Return the integer nearest the receiver toward negative infinity."

    | selfTruncated |
    selfTruncated := self truncated.
    "If positive, truncation to zero is what we want."
    self >= self zero ifTrue: [^selfTruncated].

    "Must be negative."
    self = (self coerce: selfTruncated)
	ifTrue: [^selfTruncated]
	ifFalse: [^selfTruncated - 1]
!!


!Number methodsFor: 'arithmetic'!

reciprocal
    "Return the reciprocal of the receiver"
    self = self zero
	ifTrue: [self error: 'can not return the reciprocal of zero']
	ifFalse: [^self unity / self]
!!



!Number methodsFor: 'Intervals & iterators'!

to: stop
    "Return an interval going from the receiver to stop by 1"
    ^Interval from: self to: stop
!

to: stop by: step
    "Return an interval going from the receiver to stop with the given step"
    ^Interval from: self to: stop by: step
!

to: stop by: step do: aBlock
    "Evaluate aBlock for each value in the interval going from the receiver
     to stop with the given step. Compiled in-line for integer literal steps,
     and for one-argument aBlocks without temporaries, and therefore
     not overridable."
    | i |
    i := self.
    step > step zero
	ifTrue: [
    	    [ i <= stop ]
	    	whileTrue: [ aBlock value: i.
		    	     i := i + step ]
    	]
	ifFalse: [
    	    [ i >= stop ]
	    	whileTrue: [ aBlock value: i.
		    	     i := i + step ]
	].
    ^stop
!

to: stop do: aBlock
    "Evaluate aBlock for each value in the interval going from the receiver
     to stop by 1. Compiled in-line for one-argument aBlocks without
     temporaries, and therefore not overridable."
    | i |
    i := self.
    [ i <= stop ]
    	whileTrue: [ aBlock value: i.
	    	     i := i + self unity ]
!!
