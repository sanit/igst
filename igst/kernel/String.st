"======================================================================
|
|   String Method Definitions
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


ArrayedCollection variableByteSubclass: #String
		  instanceVariableNames: ''
		  classVariableNames: ''
		  poolDictionaries: ''
		  category: 'Language-Data types'
!

String comment: 
'My instances represent string data types.  I provide accessing and
manipulation methods for string data types.' !


!String class methodsFor: 'basic'!

fromString: aString
    "But we are already strings! So just answer a fresh copy of aString."
    ^aString copyFrom: 1 to: aString size
!

lineDelimiter
    "Answer a String which one can use as a line delimiter."
    ^self with: Character nl
! !


!String methodsFor: 'comparing'!

< aString
    "Return true if the receiver is less than aString, ignoring case
    differences."
    | c1 c2 |
    1 to: (self size min: aString size) do:
    	[ :i | c1 := (self at: i) asLowercaseValue.
	       c2 := (aString at: i) asLowercaseValue.
	       c1 > c2 ifTrue: [ ^false ].
	       c1 < c2 ifTrue: [ ^true ] ].
    ^self size < aString size
!

> aString
    "Return true if the receiver is greater than aString, ignoring case
    differences."
    | c1 c2 |
    " Scan self and aString until a character is clearly greater or lesser
      (All preceding characters must have been equal).  If the end is reached,
      one of the strings is a possibly improper initial substring of the other,
      and for the receiver to be less than aString, it must be the initial
      substring."
    1 to: (self size min: aString size) do:
    	[ :i | c1 := (self at: i) asLowercaseValue.
	       c2 := (aString at: i) asLowercaseValue.
	       c1 > c2 ifTrue: [ ^true ].
	       c1 < c2 ifTrue: [ ^false ] ].
    ^self size > aString size
!

<= aString
    "Returns true if the receiver is less than or equal to aString,
    ignoring case differences.  If is receiver is an initial substring of
    aString, it is considered to be less than aString."
    | c1 c2 |
    " Scan self and aString until a character is clearly greater or lesser
      (All preceding characters must have been equal).  If the end is reached,
      one of the strings is a possibly improper initial substring of the other,
      and for the receiver to be less than aString, it must be the initial
      substring."
    1 to: (self size min: aString size) do:
    	[ :i | c1 := (self at: i) asLowercaseValue.
	       c2 := (aString at: i) asLowercaseValue.
	       c1 < c2 ifTrue: [ ^true ].
	       c1 > c2 ifTrue: [ ^false ] ].
    ^self size <= aString size
!

>= aString
    "Returns true if the receiver is greater than or equal to aString,
    ignoring case differences.  If is aString is an initial substring of
    the receiver, it is considered to be less than the receiver."
    | c1 c2 |
    1 to: (self size min: aString size) do:
    	[ :i | c1 := (self at: i) asLowercaseValue.
	       c2 := (aString at: i) asLowercaseValue.
	       c1 < c2 ifTrue: [ ^false ].
	       c1 > c2 ifTrue: [ ^true ] ].
    ^self size >= aString size
!

sameAs: aString
    "Returns true if the receiver is the same string as aString, ignoring
    case differences."
    self size ~= aString size ifTrue: [ ^false ].
    1 to: self size do: [ :i |
        (self at: i) asLowercaseValue ~= (aString at: i) asLowercaseValue
	    ifTrue: [ ^false ]
    ].
    ^true
!

match: aString
    "Answer whether the receiver matches the aString pattern. # in aString
     means 'match any character', * in aString means 'match any sequence of
     characters' "
    | result |
    result := self asLowercase
        matchSubstring: 1
        in: aString asLowercase
        at: 1.

    ^result = aString size
!

indexOf: aString matchCase: aBoolean startingAt: anIndex
    "Answer an Interval of indices in the receiver which match the aString
     pattern. # in aString means 'match any character', * in aString means
     'match any sequence of characters'. The first item of the returned in-
     terval is >= anIndex. If aBoolean is false, the search is case-insen-
     sitive, else it is case-sensitive.
     If no Interval matches the pattern, answer nil."

    | result |

    aBoolean ifFalse: [
        ^self asLowercase
           indexOf: aString asLowercase
           matchCase: true
           startingAt: anIndex ].

    anIndex to: self size do: [ :i |
        result := aString matchSubstring: 1 in: self at: i.
        result notNil ifTrue: [^i to: result]
    ].
    ^nil
!!


!String methodsFor: 'string processing'!

substrings
    "Answer an OrderedCollection of substrings of the receiver. A new substring
     start at the start of the receiver, or after every sequence of white space
     characters"
    | oc last |

    last := 1.
    oc := OrderedCollection new.
    1 to: self size do: [:i |
        (self at: i) isSeparator ifTrue: [
            last = i ifFalse: [
                oc addLast: (self copyFrom: last to: i - 1)
            ].
            last := i + 1.
        ]
    ].
    last <= self size ifTrue: [
        oc addLast: (self copyFrom: last to: self size)
    ].
    ^oc
!

substrings: aCharacter
    "Answer an OrderedCollection of substrings of the receiver. A new substring
     start at the start of the receiver, or after every sequence of characters
     matching aCharacter"
    | oc last |

    last := 1.
    oc := OrderedCollection new.
    1 to: self size do: [:i |
        (self at: i) == aCharacter ifTrue: [
            last = i ifFalse: [
                oc addLast: (self copyFrom: last to: i - 1)
            ].
            last := i + 1.
        ]
    ].
    last <= self size ifTrue: [
        oc addLast: (self copyFrom: last to: self size)
    ].
    ^oc
!

bindWith: s1
    "Answer the receiver with every %1 replaced by s1"
    ^self bindWithArguments: (Array with: s1)
!

bindWith: s1 with: s2
    "Answer the receiver with every %1 or %2 replaced by s1 or s2,
     respectively"
    ^self bindWithArguments: (Array with: s1 with: s2)
!

bindWith: s1 with: s2 with: s3
    "Answer the receiver with every %1, %2 or %3 replaced by s1, s2 or s3,
     respectively"

    ^self bindWithArguments: (Array with: s1 with: s2 with: s3)
!

bindWith: s1 with: s2 with: s3 with: s4
    "Answer the receiver with every %1, %2, %3 or %4 replaced by s1, s2, s3
     or s4, respectively"

    ^self bindWithArguments: (Array with: s1 with: s2 with: s3 with: s4)
!

bindWithArguments: anArray
    "Answer the receiver with every %n (1<=n<=9) replaced by the n-th element
     of anArray"

    | result wasPercent |
    result := WriteStream on: (self copyEmpty: self size + 20).
    wasPercent := false.
    
    self do: [:char |
        wasPercent
            ifTrue: [
                char = $%
                    ifTrue: [ result nextPut: char ]
                    ifFalse: [ result nextPutAll: (anArray at: char digitValue) ].
                wasPercent := false
            ]
            ifFalse: [
                (wasPercent := (char = $%))
                    ifFalse: [ result nextPut: char ]
            ]
    ].

    ^result contents
! !


!String methodsFor: 'converting'!

asNumber
    "Parse a Number from the receiver until the input character is invalid
     and answer the result at this point"

    | stream value ch scale |
    value := 0.
    stream := ReadStream on: self.
    scale := (stream peekFor: $-) ifTrue: [ -1 ] ifFalse: [ 1 ].

    [ stream atEnd ifTrue: [^value].
      (ch := stream next) isDigit ] whileTrue: [
         value := ch digitValue * scale + (value * 10).
    ].
    ch = $. ifFalse: [^value].

    [ stream atEnd ifTrue: [^value].
      (ch := stream next) isDigit ] whileTrue: [
         scale := scale / 10.
         value := ch digitValue * scale + value.
    ].

    ^value
!

asUppercase
    "Returns a copy of self as an uppercase string"
    | newStr |
    newStr := self copyEmpty: self size.
    1 to: self size do:
    	[ :i | newStr at: i put: (self at: i) asUppercase ].
    ^newStr
!

asLowercase
    "Returns a copy of self as a lowercase string"
    | newStr |
    newStr := self copyEmpty: self size.
    1 to: self size do:
    	[ :i | newStr at: i put: (self at: i) asLowercase ].
    ^newStr
!

asString
    "But I already am a string!  Really!"
    ^self
!

asGlobalKey
    "Return the receiver, ready to be put in the Smalltalk dictionary"
    ^self asSymbol
!

asPoolKey
    "Return the receiver, ready to be put in a pool dictionary"
    ^self asSymbol
!

asClassPoolKey
    "Return the receiver, ready to be put in a class pool dictionary"
    ^self asSymbol
!

asByteArray
    "Return the receiver, converted to a ByteArray of ASCII values"
    | byteArray size |
    size := self size.
    byteArray := ByteArray new: size.
    byteArray replaceFrom: 1 to: size withString: self startingAt: 1.
    ^byteArray
!

asInteger
    "Parse an Integer number from the receiver until the input character
     is invalid and answer the result at this point"
    | result i sign |
    result := 0.
    sign := (self at: 1) = $-
        ifTrue: [ i := 2. -1 ]
        ifFalse: [ i := 1. 1 ].
    self from: i to: self size do: 
	[ :ch | ch isDigit ifFalse: [ ^result ].
		result := result * 10 + (ch digitValue * sign) ].
    ^result
!


fileName
    "But I don't HAVE a file name!"
    ^nil
!

filePos
    "But I don't HAVE a file position!"
    ^nil
!

isNumeric
    "Answer whether the receiver denotes a number"

    | stream ch |
    stream := ReadStream on: self.
    [ stream atEnd ifTrue: [^true].
      (ch := stream next) isDigit ] whileTrue: [
    ].
    ch = $. ifFalse: [^false].

    [   ch := stream next.
        ch isDigit ifFalse: [ ^false ]
        stream atEnd] whileFalse: [ ].
    ^true
!

asSymbol
    "Returns the symbol corresponding to the string"
    ^Symbol intern: self
! !



!String methodsFor: 'copying'!

shallowCopy
    "Returns a shallow copy of the receiver"
    | newStr |
    newStr := self copyEmpty: self size.
    newStr replaceFrom: 1 to: self size with: self.
    " Off the top of my head, I can't think why we need to use the code below
      instead of the faster code above
	  1 to: self size do:
	      [ :i | newStr at: i put: (self at: i) ].
    "
    ^newStr
!

deepCopy
    "Returns a deep copy of the receiver. This is the same thing as a
     shallow copy for strings"
    ^self shallowCopy
!

, aString
    "Answer a new instance of an ArrayedCollection containing all the elements
     in the receiver, followed by all the elements in aSequenceableCollection"
    | newString mySize |
    (aString isKindOf: String) ifFalse: [ ^super , aString ].

    newString := self copyEmpty: ((mySize := self size) + aString size).
    newString primReplaceFrom: 1 to: mySize with: self startingAt: 1.
    newString primReplaceFrom: mySize + 1 to: newString size
	      with: aString startingAt: 1.
    ^newString
!

replaceFrom: start to: stop with: replacementCollection
    "Replace the items from start to stop with replacementCollection's items
     from repStart to repStart+stop-start"
    self replaceFrom: start to: stop with: replacementCollection 
	 startingAt: 1
! !




!String methodsFor: 'printing'!

printOn: aStream
    "Print a representation of the receiver on aStream"
    self storeOn: aStream
! !



!String methodsFor: 'storing'!

storeOn: aStream
    "Print Smalltalk code compiling to the receiver on aStream"
    aStream nextPut: $'.
    self do:
    	[ :char | char == $' ifTrue: [ aStream nextPut: char ].
	    	  aStream nextPut: char ].
    aStream nextPut: $'
! !



!String methodsFor: 'private'!

matchSubstring: pp in: aString at: i
    "Private - Match the pattern from the pp-th character of the receiver
     to the characters from the i-th in aString. Answer nil if they
     don't match, else answer the last character making up the pattern"

    | result s |
    s := i.
    self from: pp to: self size doWithIndex: [ :pc :p |

        pc = $*
            ifTrue: [
                (aString size + 1) to: s by: -1 do: [:ss |
                    result := self matchSubstring: p + 1
                        in: aString
                        at: ss.

                    result notNil ifTrue: [ ^result ].
                ].
                ^nil ].

        s > aString size ifTrue: [ ^nil ].

        pc = $# ifFalse: [
            pc = (aString at: s) ifFalse: [ ^nil ]
        ].
        s := s + 1
    ].
    ^s - 1
!!


!String methodsFor: 'testing functionality'!

isString
    ^true
!!

