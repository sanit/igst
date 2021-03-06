"======================================================================
|
|   Character Method Definitions
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



Magnitude variableWordSubclass: #Character 
	  instanceVariableNames: ''
	  classVariableNames: 'Table'
	  poolDictionaries: ''
	  category: 'Language-Data types'
!

Character comment: 
'My instances represent the 256 characters of the character set.  I provide
messages to translate between integers and character objects, and provide
names for some of the common unprintable characters.' !


!Character class methodsFor: 'testing'!

isIdentity
    "Answer whether x = y implies x == y for instances of the receiver"
    ^true
!

isImmediate
    "Answer whether, if x is an instance of the receiver, x copy == x"
    ^true
! !


!Character class methodsFor: 'initializing lookup tables'!

initialize
    "Initialize the lookup table which is used to make case and digit-to-char
     conversions faster.
     Indices in Table are ASCII values incremented by one. Indices 1-256
     classify chars (0 = nothing special, 2 = separator, 48 = digit,
     55 = uppercase, 3 = lowercase), indices 257-512 map to lowercase chars,
     indices 513-768 map to uppercase chars."

    "Smalltalk executionTrace: true; verboseTrace: true."
    Table := ByteArray new: 768.

    1 to: 256 do: [ :value |
        Table
            at: value + 256 put: value - 1;
            at: value + 512 put: value - 1.

        (value between: 49 and: 58)
            ifTrue: [ Table at: value put: 48 ].

        (value between: 66 and: 91)
            ifTrue: [ Table at: value put: 55; at: value + 512 put: value + 31 ].

        (value between: 98 and: 123)
            ifTrue: [ Table at: value put: 3; at: value + 256 put: value - 33 ]
    ].
    Table
        at: Character space	value + 1 put: 2;
        at: Character cr	value + 1 put: 2;
        at: Character tab	value + 1 put: 2;
        at: Character nl	value + 1 put: 2;
        at: Character newPage	value + 1 put: 2.
! !


!Character class methodsFor: 'constants'!

backspace
    "Returns the character 'backspace'"
    ^Character value: 8
!

cr
    "Returns the character 'cr'"
    ^Character value: 13
!

eof
    "Returns the character 'eof', aka 'sub' "
    ^Character value: 26
!

esc
    "Returns the character 'esc'"
    ^Character value: 27
!

nul
    "Returns the character 'nul'"
    ^Character value: 0
!

tab
    "Returns the character 'tab'"
    ^Character value: 9
!

nl
    "Returns the character 'nl'"
    ^Character value: 10
!

newPage
    "Returns the character 'newPage', aka 'ff'"
    ^Character value: 12
!

space
    "Returns the character 'space'"
    ^($ )
!!



!Character class methodsFor: 'Instance creation'!

digitValue: anInteger
    "Returns a character that corresponds to anInteger.  0-9 map to $0-$9,
    10-35 map to $A-$Z"
    ^'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ' at: anInteger + 1
!!



!Character methodsFor: 'converting'!

digitValue
    "Returns the value of self interpreted as a digit.  Here, 'digit' means
    either 0-9, or A-Z, which maps to 10-35."
    | result |
    result := Table at: 1 + self asciiValue.
    ^result > 32
        ifTrue: [ self asciiValue - result ]
        ifFalse: [ self error: 'Invalid digit character' ]
!!



!Character methodsFor: 'comparing'!

< aCharacter
    "Compare the character's ASCII value. Answer whether the receiver's
     is the least."
    ^self asciiValue < aCharacter asciiValue
!

<= aCharacter
    "Compare the character's ASCII value. Answer whether the receiver's
     is the least or their equal."
    ^self asciiValue <= aCharacter asciiValue
!

> aCharacter
    "Compare the character's ASCII value. Answer whether the receiver's
     is the greatest."
    ^self asciiValue > aCharacter asciiValue
!

>= aCharacter
    "Compare the character's ASCII value. Answer whether the receiver's
     is the greatest or their equal."
    ^self asciiValue >= aCharacter asciiValue
!!



!Character methodsFor: 'testing'!

isDigit
    "True if self is a 0-9 digit"
    ^(Table at: 1 + self asciiValue) = 48
!

isLetter
    "True if self is an upper- or lowercase letter"
    ^((Table at: 1 + self asciiValue) bitAnd: 1) = 1
!

isAlphaNumeric
    "True if self is a letter or a digit"
    ^(Table at: 1 + self asciiValue) > 2
!

isLowercase
    "True if self is a lowercase letter"
    ^(Table at: 1 + self asciiValue) = 3
!

isUppercase
    "True if self is uppercase"
    ^(Table at: 1 + self asciiValue) = 55
!

isSeparator
    "Returns true if self is a space, cr, tab, nl, or newPage"
    ^(Table at: 1 + self asciiValue) = 2
!

isVowel
    "Returns true if self is a, e, i, o, or u; case insensitive"
    | char |
    "So rare it isn't worth optimization"
    char := self asLowercase.
    char = $a ifTrue: [ ^true ].    
    char = $e ifTrue: [ ^true ].    
    char = $i ifTrue: [ ^true ].    
    char = $o ifTrue: [ ^true ].    
    char = $u ifTrue: [ ^true ].    
    ^false
!!



!Character methodsFor: 'Coercion methods'!

asLowercase
    "Returns self as a lowercase character if it's an uppercase letter,
     otherwise returns the character unchanged."
    ^Character value: (Table at: 513 + self asciiValue)
!

asUppercase
    "Returns self as a uppercase character if it's an lowercase letter,
     otherwise returns the character unchanged."
    ^Character value: (Table at: 257 + self asciiValue)
!

asString
    "Returns the character self as a string."
    ^String with: self
!

asSymbol
    "Returns the character self as a symbol."
    ^Symbol internCharacter: self
!!



!Character methodsFor: 'printing'!

printOn: aStream
    "Store a representation of the receiver on aStream"
    self storeOn: aStream
!!



!Character methodsFor: 'storing'!

storeOn: aStream
    "Store Smalltalk code compiling to the receiver on aStream"
    aStream nextPut: $$.
    aStream nextPut: self
!!


!Character methodsFor: 'private'!

asLowercaseValue
    "Returns the ASCII value of the receiver converted to a lowercase character
     if it's an uppercase letter, otherwise returns the ASCII value unchanged."
    ^Table at: 513 + self asciiValue
!!


!Character methodsFor: 'testing functionality'!

isCharacter
    "Answer True. We're definitely characters"
    ^true
!!

Character initialize!