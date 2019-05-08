"======================================================================
|
|   Random number Method Definitions
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


Stream subclass: #Random
       instanceVariableNames: 'seed'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Streams'
!

Random comment: "Here's a random comment :-)"
'My instances are generator streams that produce random numbers, which are 
floating point values between 0 and 1.'!


!Random class methodsFor: 'instance creation'!

seed: aFloat
    "Create a new random number generator whose seed is aFloat"
    ^self basicNew seed: aFloat; yourself
!

new
    "Create a new random number generator whose seed is given by the
    current time on the millisecond clock"
    ^self basicNew setSeed
!!



!Random methodsFor: 'testing'!

chiSquare
    "returns under Pentium II, NT 4.0, 93.0"
    "the previous algorithm's chi-square was 93.4"
    ^self chiSquare: 1000 range: 100
!

chiSquare: n range: r
    "Return the chi-square deduced from calculating n random
     numbers in the 0..r range"

    | f t seed |
    seed := 1234567.
    f := Array new: r + 1.
    1 to: r + 1 do: [ :i | f at: i put: 0 ].
    n timesRepeat: [
	seed := (seed + Float pi) squared squared fractionPart.
	t := (seed * r) truncated.
	f at: t + 1 put: (f at: t + 1) + 1
    ].
    t := 0.
    1 to: r do: [ :i | t := t + (f at: i) squared ].
    ^r asFloat * t / n - n

!!



!Random methodsFor: 'basic'!

atEnd
    "This stream never ends. Always answer false"
    ^false
!

next
    "Return the next random number in the sequence"
    "Found on an obscure Japanese SHARP manual. Turns out to be good!"
    ^seed := (seed + Float pi) squared squared fractionPart
!

nextPut: value
    self shouldNotImplement
!!



!Random methodsFor: 'private'!

seed: aFloat
    "Private - Set the random number seed to aFloat.
    Ensure decent results even when integers are passed"
    seed := (aFloat / 100000.0 + aFloat) fractionPart.
    self next.
    self next
!

setSeed
    "Private - Set a random number seed."
    seed := Time primSecondClock - Time primMillisecondClock.
    seed := (seed / 16r3FFFFF.0) fractionPart.
    self next.
    self next
!!
