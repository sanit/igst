"======================================================================
|
|   Class Fraction Definitions
|
|   $Revision: 1.6.2$
|   $Date: 1999/08/31 11:23:18$
|   $Author: pb$
|
 ======================================================================"


"======================================================================
|
| Copyright 1990, 91, 92, 94, 95, 99 Free Software Foundation, Inc.
| Written by David Duke.
| Slightly modified by Steve Byrne and Paolo Bonzini.
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


Number subclass: #Fraction
	instanceVariableNames: 'numerator denominator'
	classVariableNames: 'Zero One'
	poolDictionaries: ''
	category: 'Language-Data types'!

Fraction comment:
' I represent rational numbers in the form (p/q) where p and q are integers.
The arithmetic operations  *, +, -, /,  on fractions, all return a reduced 
fraction.'!



!Fraction class methodsFor: 'converting'!

coerce: aNumber
    "Answer aNumber converted to a Fraction"
    ^aNumber asFraction
! !


!Fraction class methodsFor: 'instance creation'!

initialize
    "Initialize the receiver's class variables"
    Zero := self numerator: 0 denominator: 1.
    One := self numerator: 1 denominator: 1.
!

numerator: nInteger denominator: dInteger 
    " Answer a new instance of fraction (nInteger/dInteger)"
 
    ^self new setNumerator: nInteger setDenominator: dInteger 
! !


!Fraction methodsFor: 'accessing'!

denominator
    "Answer the receiver's denominator"
    ^denominator!

numerator
    "Answer the receiver's numerator"
    ^numerator 

! !



!Fraction methodsFor: 'arithmetic'!

* aNumber 
     "Multiply two numbers and answer the result."

     (aNumber generality = self generality)
	 ifTrue: [^(Fraction numerator: numerator * aNumber numerator
			     denominator: denominator * aNumber denominator) reduce]
	 ifFalse: [^self retry: #* coercing: aNumber]
!

+ aNumber 
     "Sum two numbers and answer the result."

     (aNumber generality = self generality)
	 ifTrue: [^(Fraction numerator: (numerator * aNumber denominator)
			                + (aNumber numerator * denominator) 
			     denominator: denominator * aNumber denominator) reduce]
	 ifFalse: [^self retry: #+ coercing: aNumber]
!

- aNumber
     "Subtract aNumber from the receiver and answer the result."

     (aNumber generality = self generality)
	 ifTrue: [^self + (aNumber negated)]
	 ifFalse: [^self retry: #- coercing: aNumber]
!

/ aNumber 
     "Divide the receiver by aNumber and answer the result."

     (aNumber generality = self generality)
	 ifTrue: [^(Fraction numerator: numerator * aNumber denominator
			     denominator: denominator * aNumber numerator) reduce]
	 ifFalse: [^self retry: #/ coercing: aNumber]
!

// aNumber
     "Return the integer quotient of dividing the receiver by aNumber with
      truncation towards negative infinity."
     ^(self / aNumber) floor 
!

\\ aNumber
    "Return the remainder from dividing the receiver by aNumber, (using //)."
    ^(self - (self // aNumber * aNumber))
!

estimatedLog
    "Answer an estimate of (self abs floorLog: 10)"
    ^numerator estimateLog - denominator estimateLog
!

negated
    "Return the negated of the receiver"
    ^Fraction numerator: numerator negated denominator: denominator
!

reciprocal
    "Return the reciprocal of the receiver"
    denominator < 0
	ifTrue: [^Fraction numerator: denominator negated 
			   denominator: numerator negated ]
	ifFalse: [^Fraction numerator: denominator denominator: numerator]

! !



!Fraction methodsFor: 'coercing'!

zero
    "Coerce 0 to the receiver's class"
    ^Zero
!

unity
    "Coerce 1 to the receiver's class"
    ^One
!

coerce: aNumber 
    "Coerce aNumber to the receiver's class"
    ^aNumber asFraction
!

generality
    "Return the receiver's generality"
    ^300
!

truncated
    "Truncate the receiver and return the truncated result"
    ^numerator quo: denominator
! !



!Fraction methodsFor: 'comparing'!

< arg 
     "Test if the receiver is less than arg."

     (arg generality = self generality)
	 ifTrue: [^arg denominator * numerator < (denominator * arg numerator)]
	 ifFalse: [^self retry: #< coercing: arg]
!

<= arg 
     "Test if the receiver is less than or equal to arg."

     (arg generality = self generality)
	 ifTrue: [^arg denominator * numerator <= (denominator * arg numerator)]
	 ifFalse: [^self retry: #<= coercing: arg]
!

> arg 
     "Test if the receiver is more than arg."

     (arg generality = self generality)
	ifTrue: [^arg denominator * numerator > (denominator * arg numerator)]
	ifFalse: [^self retry: #> coercing: arg]
!

>= arg 
     "Test if the receiver is greater than or equal to arg."

     (arg generality = self generality)
	 ifTrue: [^arg denominator * numerator >= (denominator * arg numerator)]
	 ifFalse: [^self retry: #>= coercing: arg]
!

= arg 
     "Test if the receiver equals arg."

     (arg isKindOf: Number)
         ifFalse: [^false].

     (arg generality = self generality)
	 ifTrue: [^arg denominator * numerator = (denominator * arg numerator)]
	 ifFalse: [^self retry: #= coercing: arg]!

hash
    "Answer an hash value for the receiver"
    ^numerator + denominator + denominator
! !


!Fraction methodsFor: 'testing'!

isRational
    "Answer whether the receiver is rational - true"
    ^true
! !



!Fraction methodsFor: 'converting'!

asFloat
    "Answer the receiver converted to a Float"
    | n d shift sign |
    n := numerator.
    d := denominator.
    sign := n sign * d sign.

    "Avoid answering NaNs and infinite values.
     1e1800 asFloat / (1e1799 + 1) asFloat = NaN, but
     (1e1800 / (1e1799 + 1)) asFloat must be 10."
    shift := (Float largest exponent - numerator highBit).
    shift := shift min: (Float largest exponent - denominator highBit).
    shift < 0 ifTrue: [
        "Lose some more precision, but we MUST avoid infinites and NaNs!"
        shift := shift - 10. n := n bitShift: shift. d := d bitShift: shift ].

    d = 0 ifTrue: [
        ^sign > 0 ifTrue: [ Float infinity ] ifFalse: [ Float negativeInfinity ] ].
    n = 0 ifTrue: [
        ^sign > 0 ifTrue: [ 0.0 ] ifFalse: [ Float negativeInfinity reciprocal ] ].
    ^n asFloat / d asFloat
!

asFraction
    "Answer the receiver converted to a Fraction"
    ^self 
! !



!Fraction methodsFor: 'printing'!

printOn: aStream
    "Print a representation of the receiver on aStream"
    " Fractions print as (numerator/denominator) eg (3/4) ."

    aStream nextPut: $(; 
	print: numerator;
	nextPut: $/;
	print: denominator;
	nextPut: $) 
!

storeOn: aStream
    "Store Smalltalk code compiling to the receiver on aStream"
    aStream nextPutAll: '(Fraction numerator: ';
	store: numerator;
	nextPutAll: ' denominator: ';
	store: numerator ;
	nextPut: $)
! !



!Fraction methodsFor: 'private'!

reduce
    "Reduce the fraction"
    | gcd |
    numerator = 1 ifTrue: [^self].
    denominator = 1 ifTrue: [^numerator].
    numerator = 0 ifTrue: [^0].
    numerator = denominator ifTrue: [^1].
    gcd := numerator gcd: denominator.
    gcd = 1 ifTrue: [^self].
    denominator = gcd ifTrue: [^numerator quo: gcd].
    numerator := numerator quo: gcd.
    denominator := denominator quo: gcd.
    ^self
!

setNumerator: numInteger setDenominator: denInteger 
    "Set the fraction's numerator and denominator"
    denInteger = 0
	ifTrue: [^self error: 'The denominator can not be zero'].
    denInteger < 0
	ifTrue: [numerator := numInteger negated.
		 denominator := denInteger negated ]
	ifFalse: [numerator := numInteger.
		  denominator := denInteger]
! !


!Fraction methodsFor: 'optimized cases'!

negated
    "Return the receiver, with its sign changed."
    ^Fraction
	numerator: 0 - numerator
	denominator: denominator
!

raisedToInteger: anInteger 
    "Return self raised to the anInteger-th power."
    "No need to reduce"
    ^Fraction
	numerator: (numerator raisedToInteger: anInteger)
	denominator: (denominator raisedToInteger: anInteger)
!

reciprocal
    "Return the reciprocal of the receiver."
    ^Fraction
	numerator: denominator
	denominator: numerator
!

squared
    "Return the square of the receiver."
    ^Fraction
	numerator: numerator squared
	denominator: denominator squared
! !
