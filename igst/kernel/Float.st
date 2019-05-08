"======================================================================
|
|   Float Method Definitions
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


Number variableByteSubclass: #Float
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Data types'
!

Float comment: 
'My instances represent floating point numbers that have 64 bits of 
precision (well, less than that in precision; they are precisely the same
as C''s "double" datatype).  Besides the standard numerical operations,
I provide transcendental operations too.'  !


!Float class methodsFor: 'converting'!

coerce: aNumber
    "Answer aNumber converted to a Float"
    ^aNumber asFloat
! !


!Float class methodsFor: 'byte-order dependancies'!

leastSignificantMantissaByte
    ^Bigendian ifTrue: [ 2 ] ifFalse: [ 7 ]
!

exponentByte
    ^Bigendian ifTrue: [ 1 ] ifFalse: [ 8 ]
! !


!Float class methodsFor: 'basic'!

e
    "Returns the value of e. Hope is that it is precise enough"
    ^2.7182818284590452353602874713527
!

mantissaBits
    ^53
!

smallestAbs
    "Return the smallest normalized Float that is > 0"
    ^CDoubleMin
!

largest
    "Return the largest normalized Float that is not infinite."
    ^CDoubleMax
!

smallest
    "Return the smallest normalized Float that is not infinite."
    ^CDoubleMax negated
!

epsilon
    "Return the smallest Float x for which is 1 + x ~= 1"
    ^2.0 timesTwoPower: self mantissaBits negated
!

log10Base2
    "Returns the value of log2 10. Hope is that it is precise enough"
    ^3.3219280948873623478703194294894
!

infinity
    "Return a Float that represents positive infinity. I hope that it is big
    enough, IEEE 8 byte floating point values (C doubles) overflow at 1e308."
    ^1.0e10000
!

negativeInfinity
    "Return a Float that represents negative infinity. I hope that it is big
    enough, IEEE 8 byte floating point values (C doubles) overflow at -1e308."

    ^-1.0e10000
!

nan
    "Return a Float that represents a mathematically indeterminate value
     (e.g. Inf - Inf, Inf / Inf) "
    ^1.0e10000 - 1.0e10000
!

pi
    "Returns the value of pi. Hope is that it is precise enough"
    ^3.14159265358979323846264338327950288
!!



!Float methodsFor: 'arithmetic'!

// aNumber
    "Return the integer quotient of dividing the receiver by aNumber with
    truncation towards negative infinity."
    ^(self / aNumber) floor
!

\\ aNumber
    "Return the remainder of dividing the receiver by aNumber with
    truncation towards negative infinity."
    ^(self - (self // aNumber * aNumber)) floor
!

integerPart
    "Return the receiver's integer part"
    ^self - self fractionPart
!!


!Float methodsFor: 'testing'!

hash
    "Answer an hash value for the receiver"
    ^((self at: 3) bitShift: 20) +
     ((self at: 4) bitShift: 12) +
     ((self at: 5) bitShift: 4)  + (self at: 6)
!

isNaN
    "Answer whether the receiver represents a NaN"
    ^self ~= self
!

isInfinite
    "Answer whether the receiver represents positive or negative infinity"
    ^self = Float infinity or: [ self = Float negativeInfinity ]
!

negative
    "Answer whether the receiver is negative"
    ^(self at: self class exponentByte) > 127
!

strictlyPositive
    "Answer whether the receiver is > 0"
    ^(self at: self class exponentByte) < 128 and: [ self ~= 0.0 ]
!

positive
    "Answer whether the receiver is positive"
    ^(self at: self class exponentByte) < 128
!

sign
    "Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0.
     Handle IEEE-754 negative-zero by reporting a sign of -1"
    self negative ifTrue: [^-1].
    self > 0 ifTrue: [^ 1].
    ^ 0
!!



!Float methodsFor: 'coercing'!

zero
    "Coerce 0 to the receiver's class"
    ^0.0
!

unity
    "Coerce 1 to the receiver's class"
    ^1.0
!

coerce: aNumber
    "Coerce aNumber to the receiver's class"
    ^aNumber asFloat
!

generality
    "Answer the receiver's generality"
    ^400
!

asExactFraction
    "Convert the receiver into a fraction with optimal approximation,
     but with usually huge terms."

    self checkCoercion.

    ^(self   timesTwoPower:  self exponent + Float mantissaBits) truncated /
     (1      bitShift:       self exponent + Float mantissaBits)
!

asFraction
    "Convert the receiver into a fraction with a good (but undefined)
     approximation"

    | a x n2 d2 n1 d1 n0 d0 eps |

    self checkCoercion.

    "This uses an algorithm based on the theory of continued fractions!!!
      n2/d2 = numerator and denominator of the fraction two steps ago
      n1/d1 = numerator and denominator of the fraction a steps ago
      n0/d0 = numerator and denominator of the fraction at the current step"

    n1  := d0 := 0.
    n0  := d1 := 1.
    x   := self abs.
    eps := Float epsilon * 1024.0 .

    [   a := x truncated.
        n2 := n1. d2 := d1.
        n1 := n0. d1 := d0.
        n0 := n1 * a + n2.
        d0 := d1 * a + d2.
        (n0 asFloat / d0 asFloat - self) abs < eps        "Number out of a hat"
    ]   whileFalse: [
        x := 1.0 / x fractionPart
    ].

    ^Fraction
        numerator: (self < 0 ifTrue: [ n0 negated ] ifFalse: [ n0 ])
        denominator: d0
!


estimatedLog
    "Answer an estimate of (self abs floorLog: 10)"
    ^(self exponent + 1) asFloat / Float log10Base2
!

asFloat
    "Just defined for completeness.  Return the receiver."
    ^self
!

hash
    "Returns the hash value for the receiver (a Float).  If it's representable
    as an integer, we use that value, since it's likely to be much more random,
    but if not, we scale the number so that it's within the integer range and
    then return the integer part"
    | expt |
    expt := self exponent.
    expt >= 31
        ifTrue: [ ^(self timesTwoPower: 30 - expt) truncated ].

    expt < 0
        ifTrue: [ ^(self timesTwoPower: 30 + expt) truncated ].

    ^self
!!



!Float methodsFor: 'printing'!

printOn: aStream
    "Print a representation of the receiver on aStream"
    self
	printOn: aStream
	special: #('Inf' '-Inf' 'NaN')
!!



!Float methodsFor: 'storing'!

storeOn: aStream
    "Print a representation of the receiver on aStream"
    self
	printOn: aStream
	special: #('Float infinity' 'Float negativeInfinty' 'Float nan')
!!



!Float methodsFor: 'private'!

checkCoercion
    "Private - Fail if the receiver is only representable as a Float"

    self isInfinite ifTrue: [self error: 'Infinity can only be a Float'].
    self isNaN ifTrue: [self error: 'Not-a-Number can only be a Float'].
!

printOn: aStream special: whatToPrintArray
    "Private - Print a decimal representation of the receiver on aStream,
     printing one of the three elements of whatToPrintArray if it is
     infinity, negative infinity, or a NaN"
    self > Float largest
        ifTrue: [ ^aStream nextPutAll: (whatToPrintArray at: 1) ].
    self < Float smallest
        ifTrue: [ ^aStream nextPutAll: (whatToPrintArray at: 2) ].
    self isNaN
        ifTrue: [ ^aStream nextPutAll: (whatToPrintArray at: 3) ].

    self printFloatOn: aStream
!

printFloatOn: aStream
    "Private - Print a decimal representation of the receiver on aStream"
    | num str intDigits exp |
    num := self.
    exp := 0.
    (self exponent between: -50 and: 49) ifFalse: [
	num abs < 1.0
	    ifTrue: [
	        [ num abs < 1.0 ] whileTrue: [ num := num * 10.0. exp := exp - 1 ]
	    ]
	    ifFalse: [
		[num abs >= 10.0] whileTrue: [ num := num / 10.0. exp := exp + 1 ]
	    ]
    ].
    num negative ifTrue:
    	[ aStream nextPut: $-.
	  num := num negated. ].
    
    intDigits := num printIntegerPartOn: aStream.
    aStream nextPut: $..

    " produce the digits, up to a maximum of 15 = log10 (2^52) "
    num printFracPartOn: aStream decimals: 15 - intDigits.

    exp = 0 ifFalse: [
	aStream nextPut: $e.
	exp printOn: aStream ]
!

printIntegerPartOn: aStream
    "Private - Print a representation of the receiver's integer part
     on aStream"
    | num str twoDigits |
    self < 1.0
    	ifTrue: [ aStream nextPut: $0. ^0 ].

    str := WriteStream on: (String new: 10).

    num := self.
    [ num = 0.0 ] whileFalse: [
        "Get two digits instead of one to avoid rounding problems"
    	twoDigits := ((num / 100.0) fractionPart * 100.0) rounded.
	str nextPut: (Character digitValue: twoDigits \\ 10).
	num := (num / 10.0) integerPart.
    ].
    str contents reverseDo: [ :each | aStream nextPut: each ].
    ^str size
!

printFracPartOn: aStream decimals: digitLimit
    "Private - Print a representation of the receiver's fractional part
     on aStream"

    | num count zeroCount digit |
    num := self fractionPart.
    num = 0.0 ifTrue: [ ^aStream nextPut: $0 ].
    zeroCount := count := 0.

    [ (num ~= 0.0) & (count < digitLimit) ] whileTrue:
    	[ num := num * 10.0.
	  digit := num floor.
	  digit = 0		"for trailing zero suppression"
	    ifTrue: [ zeroCount := zeroCount + 1 ]
	    ifFalse: [ aStream next: zeroCount put: $0.
    	    	       aStream nextPut: (Character digitValue: digit).
	    	       zeroCount := 0 ].
	  num := num fractionPart.
	  count := count + 1 ].

    zeroCount = digitLimit ifTrue: [ aStream nextPut: $0 ]
! !


!Float methodsFor: 'testing functionality'!

isFloat
    ^true
!!

