"======================================================================
|
|   ByteStream Method Definitions
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


ReadWriteStream subclass: #ByteStream
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Streams-Collections'
!

ByteStream comment: 
'My instances are read/write streams specially crafted for ByteArrays.
They are able to write binary data to them.'
!


!ByteStream methodsFor: 'basic'!

next
    "Return the next *character* in the ByteArray"

    ^Character value: super next
!

nextByte
    "Return the next byte in the byte array"
    ^super next
!

nextByteArray: numBytes
    "Return the next numBytes bytes in the byte array"
    ^(self next: numBytes) asByteArray
!

nextSignedByte
    "Return the next byte in the byte array, interpreted as a 8 bit signed number"
    ^self nextBytes: 1 signed: true
!

nextUlong
    "Return the next 4 bytes in the byte array, interpreted as a 32 bit unsigned int"
    ^self nextBytes: 4 signed: false
!

nextLong
    "Return the next 4 bytes in the byte array, interpreted as a 32 bit signed int"
    ^self nextBytes: 4 signed: true
!

nextUshort
    "Return the next 2 bytes in the byte array, interpreted as a 16 bit unsigned int"
    ^self nextBytes: 2 signed: false
!

nextShort
    "Return the next 2 bytes in the byte array, interpreted as a 16 bit signed int"
    ^self nextBytes: 2 signed: true
!

nextPut: aChar
    "Store aChar on the byte array"
    aChar isCharacter
        ifTrue: [ super nextPut: aChar value ]
        ifFalse: [ ^self error: 'invalid argument type' ]
!

nextPutByte: anInteger
    "Store anInteger (range: -128..255) on the byte array"
    | int |
    int := anInteger.
    int:= (anInteger < 0)
        ifTrue: [ (anInteger negated - 1) bitXor: 255 ]
        ifFalse: [ anInteger ].

    ^super nextPut: (int bitAnd: 255)
!

nextPutAll: aCollection
    "Write all the objects in aCollection to the receiver"
    | collEnd relative lastCopied |
    aCollection isEmpty ifTrue: [ ^self ].

    collEnd := ptr + aCollection size - 1.
    relative := ptr - 1.

    [ 
        lastCopied := collEnd min: collection size.
        collection
            replaceFrom: ptr
            to: lastCopied
	    with: aCollection
	    startingAt: ptr - relative.
	
	(ptr := lastCopied + 1) > collEnd
    ]   whileFalse: [
        ptr > endPtr ifTrue: [ endPtr := ptr ].
        self growCollection
    ].
!

nextPutByteArray: aByteArray
    "Store aByteArray on the byte array"
    ^self nextPutAll: aByteArray
!

nextPutLong: anInteger
    "Store anInteger (range: -2^31..2^32-1) on the byte array as 4 bytes"
    self nextPutBytes: 4 of: anInteger
!

nextPutShort: anInteger
    "Store anInteger (range: -32768..65535) on the byte array as 2 bytes"
    self nextPutBytes: 2 of: anInteger
!!


!ByteStream methodsFor: 'private'!

basicNextByte
    "Private - Return the next byte in the stream"
    ^super next
!

basicNextPutByte: anInteger
    "Private - Store a byte in the stream"
    | int |
    int := anInteger.
    int:= (anInteger < 0)
        ifTrue: [ (anInteger negated - 1) bitXor: 255 ]
        ifFalse: [ anInteger ].

    ^super nextPut: (int bitAnd: 255)
!

nextBytes: n signed: signed
    "Private - Get an integer out of the next anInteger bytes in the stream"

    | int |
    int := 0.
    0 to: n * 8 - 8 by: 8 do: [ :i |
        int := int bitOr: (self nextByte bitShift: i)
    ].
    signed ifFalse: [ ^int ].

    ^int > (1 bitShift: 8 * n - 1)
        ifTrue: [ int - (1 bitShift: 8 * n) ]
        ifFalse: [ int ]
!

nextPutBytes: n of: anInteger
    "Private - Store the n least significant bytes of int in little-endian format"
    | int |

    int := (anInteger < 0)
        ifTrue: [ anInteger + (1 bitShift: 8 * n) ]
        ifFalse: [ anInteger ].

    n timesRepeat: [
        self nextPutByte: (int bitAnd: 255).
        int := int bitShift: -8
    ]
!!


