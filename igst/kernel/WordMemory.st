"======================================================================
|
|   Word oriented memory definitions
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
| This file is part of GNU-Smalltalk.
|
| GNU-Smalltalk is free software; you can redistribute it and/or modify it
| under the terms of the GNU General Public License as published by the Free
| Software Foundation; either version 2, or (at your option) any later version.
| 
| GNU-Smalltalk is distributed in the hope that it will be useful, but WITHOUT
| ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
| FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
| details.
| 
| You should have received a copy of the GNU General Public License along with
| GNU-Smalltalk; see the file COPYING.  If not, write to the Free Software
| Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
|
 ======================================================================"


Memory variableWordSubclass: #WordMemory
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Implementation'
!

WordMemory comment: 
'I have no instances.  I provide messages to my class that access real memory 
as 32-bit words.  An alternative implementation would be to have a single
instance of word memory that represented all memory, and at: and at:put:
accessor methods, but since you''d typically refer to that instance via a
global variable, and since the global variable would probably be named
WordMemory, the actual method invocations are exactly the same in either case.'
!



!WordMemory class methodsFor: 'accessing'!

at: address
    "Access a 32-bit word at the given address"
    | value |
    ^Bigendian 
	ifTrue: [
	    value := ByteMemory at: address.
	    "value >= 64 
		ifTrue: [ ^self error: 'Word at ' , (address radix: 16),
				' is not representable yet' ]."

	    address + 1 to: address + CLongSize - 1 do: [ :addr |
		value := value bitShift: 8.
		value := value bitOr: (ByteMemory at: addr)
	    ].
	    value
	]
	ifFalse: [
	    value := ByteMemory at: address + CLongSize - 1.
	    "value >= 64 
		ifTrue: [ ^self error: 'Word at ' , (address radix: 16),
				' is not representable yet' ]."

	    address + CLongSize - 2 to: address by: -1 do: [ :addr |
		value := value bitShift: 8.
		value := value bitOr: (ByteMemory at: addr)
	    ].
	    value
	]
!

at: address put: value
    "Store value as a 32-bit word at the given address"
    | tempValue |
    tempValue := value.
    ^Bigendian
	ifTrue: [
	    address + CLongSize - 1 to: address + 1 by: -1 do: [ :addr |
		ByteMemory
		    at: addr
		    put: (tempValue bitAnd: 255).

		tempValue := tempValue bitShift: -8
	    ].
	    tempValue < 0
		ifTrue: [ tempValue := 256 + tempValue ].

	    ByteMemory at: address put: tempValue.
            value
        ]
	ifFalse: [
	    address to: address + CLongSize - 2 do: [ :addr |
		ByteMemory
		    at: addr
		    put: (tempValue bitAnd: 255).

		tempValue := tempValue bitShift: -8
	    ].
	    tempValue < 0
		ifTrue: [ tempValue := 256 + tempValue ].

	    ByteMemory at: address + CLongSize - 1 put: tempValue
            value
	]
! !
