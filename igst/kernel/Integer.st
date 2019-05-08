"======================================================================
|
|   Integer Method Definitions
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


Number variableWordSubclass: #Integer "### Not really a variable word subclass"
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Data types'
!

Integer comment:
'I am the integer class of the GNU Smalltalk system.  My instances can 
represent signed 30 bit integers and are as efficient as possible.' !


!Integer class methodsFor: 'converting'!

coerce: aNumber
    "Answer aNumber converted to a kind of Integer"
    ^aNumber truncated
! !


!Integer class methodsFor: 'testing'!

isIdentity
    "Answer whether x = y implies x == y for instances of the receiver"
    ^true
! !


!Integer class methodsFor: 'getting limits'!

bits
    "Answer the number of bits (excluding the sign) that can be represented
    directly in an object pointer"
    ^CLongSize * 8 - 3
!

largest
    "Answer the largest integer represented directly in an object pointer"
    | maxBit |
    maxBit := 1 bitShift: CLongSize * 8 - 3.
    ^(maxBit - 1) + maxBit
!

smallest
    "Answer the smallest integer represented directly in an object pointer"
    | maxBit |
    maxBit := 1 bitShift: CLongSize * 8 - 3.
    ^maxBit negated - maxBit
!!


!Integer methodsFor: 'Misc math operators'!

hash
    "Answer an hash value for the receiver"
    ^self
!!



!Integer methodsFor: 'Other iterators'!

timesRepeat: aBlock
    "Evaluate aBlock a number of times equal to the receiver's value.
     Compiled in-line for no argument aBlocks without temporaries, and
     therefore not overridable."
    | i |
    i := 1.
    [ i <= self ] whileTrue: [ aBlock value.
    	    	    	       i := i + 1 ]
!!



!Integer methodsFor: 'bit operators'!

bitAt: index
    "Answer the index-th bit of the receiver (LSB: index = 1"
    ^(self bitShift: (index - 1) negated) bitAnd: 1
!

bitInvert
    "Return the 1's complement of the bits of the receiver"
    ^self bitXor: -1
!

bitClear: aMask 
    "Answer an Integer equal to the receiver, except that all the bits
     that are set in aMask are cleared."

    ^(self bitOr: aMask) bitXor: aMask
!

allMask: anInteger
    "True if all 1 bits in anInteger are 1 in the receiver"
    ^(self bitAnd: anInteger) = anInteger
!

anyMask: anInteger
    "True if any 1 bits in anInteger are 1 in the receiver"
    ^(self bitAnd: anInteger) ~= 0
!

clearBit: index
    "Clear the index-th bit of the receiver and answer a new Integer"
    | bit |
    bit := 1 bitShift: (index - 1).
    ^(self bitOr: bit) bitXor: bit
!

noMask: anInteger
    "True if no 1 bits in anInteger are 1 in the receiver"
    ^(self bitAnd: anInteger) = 0
!

highBit
    "Return the index of the highest order 1 bit of the receiver"
    self = 0 ifTrue: [ ^0 ].

    "??? I don't know what the right value is"
    self < 0 ifTrue: [ ^self negated highBit + 1 ].
    30 to: 0 by: -1 do:
    	[ :i | (self bitAnd: (1 bitShift: i)) ~= 0 ifTrue: [ ^i ] ]
!

isBitSet: index
    "Answer whether the index-th bit of the receiver is set"
    ^((self bitShift: (index - 1) negated) bitAnd: 1) == 1
!

setBit: index
    "Set the index-th bit of the receiver and answer a new Integer"
    ^self bitOr: (1 bitShift: (index - 1))
!!



!Integer methodsFor: 'Math methods'!

factorial
    "Return the receiver's factorial"
    | result |

    self < 0 ifTrue: [ ^self error: 'factorial of a negative number' ].

    result := 1.
    2 to: self do: [ :i | result := result * i ].
    ^result
!

estimatedLog
    "Answer an estimate of (self abs floorLog: 10)"
    ^(self highBit asFloat / Float log10Base2) ceiling
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
    radix isInteger ifFalse: [
        ^(self coerce: radix) floorLog: radix
    ].

    me := self.
    answer := 0.
    [   me >= radix ] whileTrue: [
        me := me // radix.
        answer := answer + 1
    ].
    ^answer
!

gcd: anInteger
    "Return the greatest common divisor (Euclid's algorithm) between the
     receiver and anInteger"
    | a b remainder |
    self negative | anInteger negative ifTrue: [ ^self abs gcd: anInteger abs ].

    self < anInteger
        ifTrue: [ a := anInteger. b := self ]
        ifFalse: [ a := self. b := anInteger ].
    [ b = 0 ] whileFalse: [
        remainder := a \\ b.
	a := b.
	b := remainder. ].
    ^a
!

lcm: anInteger
    "Return the least common multiple between the receiver and anInteger"
    ^((self // (self gcd: anInteger)) * anInteger) abs
!

even
    "Return whether the receiver is even"
    ^(self bitAt: 1) = 0
!

odd
    "Return whether the receiver is odd"
    ^(self bitAt: 1) ~= 0
!!



!Integer methodsFor: 'Coercion methods (heh heh heh)'!

asCharacter
    "Return self as an ascii character"
    ^Character value: self
!

zero
    "Coerce 0 to the receiver's class"
    ^0
!

unity
    "Coerce 1 to the receiver's class"
    ^1
!

coerce: aNumber
    "Coerce aNumber to the receiver's class"
    ^aNumber truncated
!

generality
    "Return the receiver's generality"
    ^100
!

ceiling
    "Return the receiver - it's already truncated"
    ^self
!

floor
    "Return the receiver - it's already truncated"
    ^self
!

truncated
    "Return the receiver - it's already truncated"
    ^self
!

rounded
    "Return the receiver - it's already truncated"
    ^self
!!



!Integer methodsFor: 'converting'!

asFraction
    "Return the receiver converted to a fraction"
    ^Fraction numerator: self denominator: 1
!!



!Integer methodsFor: 'printing'!

printOn: aStream base: b
    "Print on aStream the base b representation of the receiver"
    self signedStringBase: b showRadix: false on: aStream
!

storeOn: aStream base: b
    "Print on aStream Smalltalk code compiling to the receiver, 
     represented in base b"
    self signedStringBase: b showRadix: true on: aStream
!

radix: baseInteger
    "Return the base b representation of the receiver, with BBr in
     front of it"
    | str |
    str := WriteStream on: (String new: 10).
    self signedStringBase: baseInteger showRadix: true on: str.
    ^str contents
!

printString: baseInteger
    "Return the base b representation of the receiver"
    | str |
    str := WriteStream on: (String new: 10).
    self signedStringBase: baseInteger showRadix: false on: str.
    ^str contents
!

printOn: aStream
    "Print on aStream the base 10 representation of the receiver"
    self signedStringBase: 10 showRadix: false on: aStream
!!



!Integer methodsFor: 'storing'!

storeOn: aStream
    "Print on aStream the base 10 representation of the receiver"
    self printOn: aStream		"they print and store the same"
!!



!Integer methodsFor: 'private'!

signedStringBase: radix showRadix: showRadix on: str
    "Append to str the base baseInteger representation of the receiver"
    | sign num revStr |
    sign := (self sign = -1).
    num := sign ifTrue: [ self negated ] ifFalse: [ self ].

    showRadix ifTrue:
    	[ radix printOn: str.
	  str nextPut: $r ].
    sign ifTrue: [ str nextPut: $- ].

    revStr := WriteStream on: (String new: 8).
    num reverseStringBase: radix on: revStr.
    revStr contents reverseDo: [ :each | str nextPut: each ].
!

reverseStringBase: b on: revStr
    "Print on str the reversed base b representation of the receiver
     (which is > 0)"

    | num |
    self = 0 ifTrue: [ revStr nextPut: $0. ^1 ].

    num := self.
    [ num = 0 ] whileFalse:
    	[ revStr nextPut: (Character digitValue: (num \\ b)).
	  num := num // b ].

    ^revStr size
!!


!Integer methodsFor: 'testing functionality'!

isRational
    "Answer whether the receiver is rational - true"
    ^true
!

isSmallInteger
    ^true
!

isInteger
    ^true
!!



!Integer methodsFor: 'accessing'!

numerator
    ^self
!

denominator
    ^1
!!