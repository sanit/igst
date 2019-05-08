"======================================================================
|
|   C object basic data type definitions.
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


Object variableWordSubclass:  #CObject
	    instanceVariableNames: 'type '
	    classVariableNames: ''
	    poolDictionaries: ''
	    category: 'Language-C interface'!


CObject comment:
'I am not part of the standard Smalltalk kernel class hierarchy.
My instances contain values that are not interpreted by the Smalltalk 
system; they frequently hold "pointers" to data outside of the Smalltalk
environment.  The C callout mechanism allows my instances to be transformed
into their corresponding C values for use in external routines.'!


!CObject class methodsFor: 'instance creation'!

address: anInteger
    "Answer a new object pointing to the passed address, anInteger"
    ^(self basicNew: 1) address: anInteger
! !


!CObject class methodsFor: 'conversion'!

scalarIndex
    "Nothing special in the default case - answer a CType for the receiver"
    ^CType cObjectType: self
!

type
    "Nothing special in the default case - answer a CType for the receiver"
    ^CType cObjectType: self
! !


!CObject methodsFor: 'finalization'!

finalize
    "To make the VM call this, use #addToBeFinalized. It frees
     automatically any memory pointed to by the CObject. It is not
     automatically enabled because big trouble hits you if you use
     #free and the receiver doesn't point to the base of a malloc-ed
     area."
    self free
! !


!CObject methodsFor: 'conversion'!

castTo: aType
    "Answer another CObject, pointing to the same address as the receiver,
     but belonging to the aType CType."
    ^self at: 0 type: aType
!

scalarIndex
    "Nothing special in the default case - answer the receiver's CType"
    ^type
!

type
    "Answer a CType for the receiver"
    ^type
! !


!CObject methodsFor: 'accessing'!

address
    "Answer the address the receiver is pointing to."
    ^self basicAt: self basicSize
!

address: anInteger
    "Set the receiver to point to the passed address, anInteger"
    self basicAt: self basicSize put: anInteger
!

printOn: aStream
    "Print a representation of the receiver"
    aStream
        print: self class;
        nextPut: $(;
        nextPutAll: (self address radix: 16);
        nextPut: $)
!

type: aCType
    "Set the receiver's type to aCType."
    type := aCType
!

value
    "What can I return? So fail"
    self shouldNotImplement
!

value: anObject
    "What can I set? So fail"
    self shouldNotImplement
! !


!CObject methodsFor: 'private'!

cObjStoredValue
    "Private - Provide a conversion from a CObject to a Smalltalk object
     to be stored by #at:put:"
    ^self value
!!


CObject variableWordSubclass: #CScalar
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Language-C interface'
!

!CScalar class methodsFor: 'instance creation'!

value: anObject
    "Answer a newly allocated CObject containing the passed value,
     anObject. Remember to call #addToBeFinalized if you want the
     CObject to be automatically freed"
    | cObject |
    cObject := self type new.
    cObject value: anObject.
    ^cObject
!

type
    "Answer a CType for the receiver - for example, CByteType if
     the receiver is CByte."
    ^Smalltalk at: (self name, 'Type') asSymbol
! !

!CScalar methodsFor: 'accessing'!

value
    "Answer the value the receiver is pointing to. The exact returned
     value depends on the receiver's class"
    ^super at: 0 type: self scalarIndex
!

value: aValue
    "Set the receiver to point to the value, aValue. The exact meaning
     of aValue depends on the receiver's class"
    super at: 0 put: aValue type: self scalarIndex
! !



CScalar variableWordSubclass: #CSmalltalk
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Language-C interface'!

!CSmalltalk class methodsFor: 'accessing'!

sizeof
    "Answer the receiver's instances size"
    ^CPtrSize
!

alignof
    "Answer the receiver's instances required aligment"
    ^CPtrSize
!

scalarIndex
    "Private - Answer an index referring to the receiver's instances scalar type"
    ^9
! !
    

!CSmalltalk methodsFor: 'accessing'!

sizeof
    "Answer the receiver's size"
    ^CPtrSize
!

alignof
    "Answer the receiver's required aligment"
    ^CPtrSize
!

scalarIndex
    "Private - Answer an index referring to the receiver's scalar type"
    ^9
! !



CScalar variableWordSubclass: #CLong
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Language-C interface'!


!CLong class methodsFor: 'accessing'!

sizeof
    "Answer the receiver's instances size"
    ^CLongSize
!

alignof
    "Answer the receiver's instances required aligment"
    ^CLongSize
!

scalarIndex
    "Private - Answer an index referring to the receiver's instances scalar type"
    ^4
! !
    

!CLong methodsFor: 'accessing'!

sizeof
    "Answer the receiver's size"
    ^CLongSize
!

alignof
    "Answer the receiver's required aligment"
    ^CLongSize
!

scalarIndex
    "Private - Answer an index referring to the receiver's scalar type"
    ^4
! !



CScalar variableWordSubclass: #CULong
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Language-C interface'!

!CULong class methodsFor: 'accessing'!

sizeof
    "Answer the receiver's instances size"
    ^CLongSize
!

alignof
    "Answer the receiver's instances required aligment"
    ^CLongSize
!

scalarIndex
    "Private - Answer an index referring to the receiver's instances scalar type"
    ^5
! !
    


!CULong methodsFor: 'accessing'!

sizeof
    "Answer the receiver's size"
    ^CLongSize
!

alignof
    "Answer the receiver's required aligment"
    ^CLongSize
!

scalarIndex
    "Private - Answer an index referring to the receiver's scalar type"
    ^5
! !


CScalar variableWordSubclass: #CInt
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Language-C interface'!

!CInt class methodsFor: 'accessing'!

sizeof
    "Answer the receiver's size"
    ^CIntSize
!

alignof
    "Answer the receiver's required aligment"
    ^CIntSize
!

scalarIndex
    "Private - Answer an index referring to the receiver's instances scalar type"
    ^10
! !
    

!CInt methodsFor: 'accessing'!

sizeof
    "Answer the receiver's instances size"
    ^CIntSize
!

alignof
    "Answer the receiver's instances required aligment"
    ^CIntSize
!

scalarIndex
    "Private - Answer an index referring to the receiver's scalar type"
    ^10
! !



CScalar variableWordSubclass: #CUInt
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Language-C interface'!

!CUInt class methodsFor: 'accessing'!

sizeof
    "Answer the receiver's instances size"
    ^CIntSize
!

alignof
    "Answer the receiver's instances required aligment"
    ^CIntSize
!

scalarIndex
    "Private - Answer an index referring to the receiver's instances scalar type"
    ^11
! !
    


!CUInt methodsFor: 'accessing'!

sizeof
    "Answer the receiver's size"
    ^CIntSize
!

alignof
    "Answer the receiver's required aligment"
    ^CIntSize
!

scalarIndex
    "Private - Answer an index referring to the receiver's scalar type"
    ^11
! !




CScalar variableWordSubclass: #CShort
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Language-C interface'!

!CShort class methodsFor: 'accessing'!

sizeof
    "Answer the receiver's instances size"
    ^CShortSize
!

alignof
    "Answer the receiver's instances required aligment"
    ^CShortSize
!

scalarIndex
    "Private - Answer an index referring to the receiver's instances scalar type"
    ^2
! !
    


!CShort methodsFor: 'accessing'!

sizeof
    "Answer the receiver's size"
    ^CShortSize
!

alignof
    "Answer the receiver's required aligment"
    ^CShortSize
!

scalarIndex
    "Private - Answer an index referring to the receiver's scalar type"
    ^2
! !



CScalar variableWordSubclass: #CUShort
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Language-C interface'!

!CUShort class methodsFor: 'accessing'!

sizeof
    "Answer the receiver's instances size"
    ^CShortSize
!

alignof
    "Answer the receiver's instances required aligment"
    ^CShortSize
!

scalarIndex
    "Private - Answer an index referring to the receiver's instances scalar type"
    ^3
! !
    

!CUShort methodsFor: 'accessing'!

sizeof
    "Answer the receiver's size"
    ^CShortSize
!

alignof
    "Answer the receiver's required aligment"
    ^CShortSize
!

scalarIndex
    "Private - Answer an index referring to the receiver's scalar type"
    ^3
! !




CScalar variableWordSubclass: #CChar
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Language-C interface'!

!CChar class methodsFor: 'accessing'!

sizeof
    "Answer the receiver's instances size"
    ^1
!

alignof
    "Answer the receiver's instances required aligment"
    ^1
!

scalarIndex
    "Private - Answer an index referring to the receiver's instances scalar type"
    ^0
! !
    

!CChar methodsFor: 'accessing'!

sizeof
    "Answer the receiver's size"
    ^1
!

alignof
    "Answer the receiver's required aligment"
    ^1
!

scalarIndex
    "Private - Answer an index referring to the receiver's scalar type"
    ^0
! !





CScalar variableWordSubclass: #CUChar
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Language-C interface'!

!CUChar class methodsFor: 'getting info'!

sizeof
    "Answer the receiver's instances size"
    ^1
!

alignof
    "Answer the receiver's instances required aligment"
    ^1
!

scalarIndex
    "Private - Answer an index referring to the receiver's instances scalar type"
    ^1
! !

!CUChar methodsFor: 'accessing'!

sizeof
    "Answer the receiver's size"
    ^1
!

alignof
    "Answer the receiver's required aligment"
    ^1
!

scalarIndex
    "Private - Answer an index referring to the receiver's scalar type"
    ^1
! !




CScalar variableWordSubclass: #CFloat
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Language-C interface'!

!CFloat class methodsFor: 'accessing'!

sizeof
    "Answer the receiver's instances size"
    ^CFloatSize
!

alignof
    "Answer the receiver's instances required aligment"
    ^CFloatSize
!

scalarIndex
    "Private - Answer an index referring to the receiver's instances scalar type"
    ^6
! !
    
!CFloat methodsFor: 'accessing'!

sizeof
    "Answer the receiver's size"
    ^CFloatSize
!

alignof
    "Answer the receiver's required aligment"
    ^CFloatSize
!

scalarIndex
    "Private - Answer an index referring to the receiver's scalar type"
    ^6
! !




CScalar variableWordSubclass: #CDouble
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Language-C interface'!

!CDouble class methodsFor: 'accessing'!

sizeof
    "Answer the receiver's instances size"
    ^CDoubleSize
!

alignof
    "Answer the receiver's instances required aligment"
    ^CDoubleAlignment
!

scalarIndex
    "Private - Answer an index referring to the receiver's instances scalar type"
    ^7
! !
    
!CDouble methodsFor: 'accessing'!

sizeof
    "Answer the receiver's size"
    ^CDoubleSize
!

alignof
    "Answer the receiver's required aligment"
    ^CDoubleAlignment
!

scalarIndex
    "Private - Answer an index referring to the receiver's scalar type"
    ^7
! !


"Forward define CType instances"
Smalltalk at: #CCharType put: nil!
Smalltalk at: #CStringType put: nil!

CScalar variableWordSubclass: #CString
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Language-C interface'!

CString comment:
'Technically, CString is really a pointer to type char.  However, it''s
so darn useful as a distinct datatype, and it is a separate datatype
in Smalltalk, so we allow developers to express their semantics more precisely
by using a more descriptive type.

In general, I behave like a cross between an array of characters and a pointer
to a character.  I provide the protocol for both data types.  My #value
method returns a Smalltalk String, as you would expect for a scalar datatype.
'!


!CString class methodsFor: 'getting info'!

sizeof
    "Answer the receiver's size"
    ^CPtrSize
!

alignof
    "Answer the receiver's instances required aligment"
    ^CPtrSize
!

scalarIndex
    "Private - Answer an index referring to the receiver's instances scalar type"
    ^8
! !



!CString methodsFor: 'accessing'!

sizeof
    "Answer the receiver's size"
    ^CPtrSize
!

alignof
    "Answer the receiver's required aligment"
    ^CPtrSize
!

scalarIndex
    "Private - Answer an index referring to the receiver's scalar type"
    ^8
! !



!CString methodsFor: 'pointer like behavior'!

at: anIndex
    "Access the string, returning the Smalltalk Character corresponding
     to the given indexed element of the string.
     anIndex is zero-based, just like with all other C-style accessing."
    self derefAt: anIndex type: 0	"char"
!

addressAt: anIndex
    "Access the string, returning a Smalltalk CChar corresponding
     to the given indexed element of the string.
     anIndex is zero-based, just like with all other C-style accessing."
    ^self at: anIndex type: CCharType
!

at: anIndex put: aCharacter
    "Store in the string a Smalltalk Character, at the given indexed
     element of the string.
     anIndex is zero-based, just like with all other C-style accessing."
    self derefAt: anIndex put: aCharacter type: 0
!

deref
    "Access the string, returning the Smalltalk CChar corresponding
     to the first element of the string. This may not make much
     sense, but it resembles what `*string' does in C."
    ^self derefAt: 0 type: CCharType
!

deref: aCChar
    "Access the string, setting the first element of the string to
     the value of the passed CChar. This may not make much
     sense, but it resembles what we get in C if we do *string = 's'."
    self derefAt: 0 put: aCChar value type: 0 "char"
!

+ anInteger
    "Return another CString pointing at &receiver[anInteger] (or, if you
     prefer, what `receiver + anInteger' does in C)."
    (anInteger isMemberOf: Integer)
	ifFalse: [ ^self error: 'Integer datatype required as right operand of +' ].
    ^self derefAt: anInteger type: CStringType
!

- intOrPtr
    "If intOrPtr is an integer, return another CString pointing at
     &receiver[-anInteger] (or, if you prefer, what `receiver - anInteger'
     does in C).
     If it is a CString, return the difference in chars, i.e. in bytes,
     between the two pointed addresses (or, if you prefer, what
     `receiver - anotherCharPtr' does in C)"

    (intOrPtr isMemberOf: Integer)
        ifTrue: [ ^self derefAt: intOrPtr negated type: CStringType ].
    (intOrPtr isKindrOf: CString ) 
	ifTrue: [ ^self ptrDiff: intOrPtr elementSize: 1 derefFirst: true ].
									    
    ^self error: 'Integer or String required a second operand of "-"'.
!								   

incr
    "Adjust the pointer by one byte up (i.e. ++receiver)"
    self adjPtrBy: 1
!

decr
    "Adjust the pointer by one byte down (i.e. --receiver)"
    self adjPtrBy: -1
!

incrBy: anInteger
    "Adjust the pointer by anInteger bytes up (i.e. receiver += anInteger)"
    self adjPtrBy: anInteger
!

decrBy: anInteger
    "Adjust the pointer by anInteger bytes down (i.e. receiver -= anInteger).
     Note that, unlike #-, #decrBy: does not support passing another CString
     as its parameter, since neither C supports something like
     `charPtr -= anotherCharPtr'"
    self adjPtrBy: anInteger negated
!

replaceWith: aString
    "Overwrite memory starting at the receiver's address, with the contents
    of the Smalltalk String aString, null-terminating it. Ensure there is
    free space enough, or big trouble will hit you!"
    <primitive: 188>
    ^self replaceWith: aString asString
! !


!CString methodsFor: 'private'!

cObjStoredValue
    "Private - Provide a conversion from a CObject to a Smalltalk object
     to be stored by #at:put:"

    "If they want to store the receiver with #at:put:, they store the
     address (of the first character) without dereferencing the pointer."
    ^self address
!!


CObject variableWordSubclass: #CAggregate
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Language-C interface'!


!CAggregate class methodsFor: 'accessing'!

sizeof
    "Answer the receiver's instances size"
    "This is the closest possible guess for CArrays"
    ^CPtrSize
!

alignof
    "Answer the receiver's instances required aligment"
    "This is the closest possible guess for CArrays"
    ^CPtrSize
! !


!CAggregate methodsFor: 'accessing'!

deref
    "Access the object, returning a new Smalltalk object of the element type,
     corresponding to the dereferenced pointer or to the first element of the
     array."
    ^self at: 0
!
					       
deref: aValue
    "Modify the object, storing the object of the element type
     into the pointed address or to the first element of the
     array."
    ^self at: 0 put: aValue
!
					       
at: anIndex
    "Access the array, returning a new Smalltalk object of the element type,
     corresponding to the given indexed element of the array.
     anIndex is zero-based, just like with all other C-style accessing."
    | elementType offset |
    offset := anIndex * elementType sizeof.
    elementType := self type elementType.
    ^elementType valueType isInteger
        ifTrue: [ self derefAt: offset type: elementType valueType ]
        ifFalse: [ (self derefAt: offset type: elementType) value ]
!

addressAt: anIndex
    "Access the array, returning a new Smalltalk CObject of the element type,
     corresponding to the given indexed element of the array.
     anIndex is zero-based, just like with all other C-style accessing."
    | elementType |
    elementType := self type elementType.
    ^self derefAt: (anIndex * elementType sizeof) type: elementType
!

at: anIndex put: aValue
    "Store in the array the passed Smalltalk object `aValue', which should
     be of the element type, corresponding to the given indexed element.
     anIndex is zero-based, just like with all other C-style accessing."
    | elementType |
    elementType := self type elementType.
    self derefAt: (anIndex * elementType sizeof) put: aValue
	 type: elementType valueType
!

value
    "Access the array, returning a new Smalltalk object of the element type,
     corresponding to the first element of the array."
    ^self at: 0
!

value: aValue
    "Modify the array, storing the object of the element type
     into the first element of the array."
    self at: 0 put: aValue
!

incr
    "Adjust the pointer by sizeof(elementType) bytes up (i.e. ++receiver)"
    self adjPtrBy: self type elementType sizeof
!

decr
    "Adjust the pointer by sizeof(elementType) bytes down (i.e. --receiver)"
    self adjPtrBy: self type elementType sizeof negated
!

incrBy: anInteger
    "Adjust the pointer by anInteger elements up (i.e. receiver += anInteger)"
    self adjPtrBy: self type elementType sizeof * anInteger
!

decrBy: anInteger
    "Adjust the pointer by anInteger elements down (i.e. receiver -= anInteger)"
    self adjPtrBy: self type elementType sizeof * anInteger negated
!

+ anInteger
    "Return another CString pointing at &receiver[anInteger] (or, if you
     prefer, what `receiver + anInteger' does in C)."

    anInteger isSmallInteger
	ifFalse: [ ^self error: 'Integer datatype required as right operand of +' ].

    ^self copy adjPtrBy: anInteger * self type elementType sizeof; yourself
!

- intOrPtr
    "If intOrPtr is an integer, return another CString pointing at
     &receiver[-anInteger] (or, if you prefer, what `receiver - anInteger'
     does in C).
     If it is a CString, return the difference in chars, i.e. in bytes,
     between the two pointed addresses (or, if you prefer, what
     `receiver - anotherCharPtr' does in C)"
    | elementType |
    elementType := self type elementType.
    intOrPtr isSmallInteger ifTrue: [
        ^self copy
            adjPtrBy: intOrPtr negated * self type elementType sizeof;
            yourself ].

    (intOrPtr isKindOf: self class)
	ifTrue: [ ^self ptrDiff: intOrPtr 
			elementSize: elementType sizeof  
			derefFirst: true ].
									    
    ^self error:
        'Integer or ', self class printString, '  required as a second operand of "-"'.
!!								   


!CAggregate methodsFor: 'private'!

cObjStoredValue
    "Private - Provide a conversion from a CObject to a Smalltalk object
     to be stored by #at:put:"

    "If they want to store the receiver with #at:put:, they
     store the address without dereferencing the pointer."
    ^self address
!!


CAggregate variableWordSubclass: #CArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Language-C interface'!

!CArray methodsFor: 'accessing'!

sizeof
    "Answer the receiver's size"
    | type |
    type := self type.
    ^type numElements * type elementType sizeof
!

alignof
    "Answer the receiver's required aligment"
    ^self type elementType alignof
! !



CAggregate variableWordSubclass: #CPtr
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Language-C interface'!

!CPtr methodsFor: 'accessing'!

sizeof
    "Answer the receiver's size"
    ^CPtrSize
!

alignof
    "Answer the receiver's required aligment"
    ^CPtrSize
! !



CUChar variableWordSubclass: #CByte
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-C interface'!

CByte comment: 'You''re a marine.
You adapt -- you improvise -- you overcome

    	    	    	- Gunnery Sgt. Thomas Highway
    	    	    	  Heartbreak Ridge'!

!CByte class methodsFor: 'conversion'!

scalarIndex
    "Nothing special in the default case - answer a CType for the receiver"
    ^CType cObjectType: self
!

type
    "Nothing special in the default case - answer a CType for the receiver"
    ^CType cObjectType: self
! !


!CByte methodsFor: 'accessing'!

scalarIndex
    "Nothing special in the default case - answer the receiver's CType"
    ^type
!

type
    "Answer a CType for the receiver"
    ^type
!

value
    "Get the receiver's value - it's the same as for CUChars, but we
     convert the result to ASCII"
    ^super value asciiValue
!

value: anInteger
    "Set the receiver's value - it's the same as for CUChars, but we
     get an Integer, not a Character"
    ^super value: (Character value: anInteger)
! !



CByte variableWordSubclass: #CBoolean
      instanceVariableNames: ''
      classVariableNames: ''
      poolDictionaries: ''
      category: 'Language-C interface'!

CBoolean comment: 'I return true if a byte is not zero, false otherwise.'!

!CBoolean methodsFor: 'accessing'!

value
    "Get the receiver's value - answer true if it is != 0, false if it is 0."
    ^super value > 0
!

value: aBoolean
    "Set the receiver's value - it's the same as for CBytes, but we
     get a Boolean, not a Character"
    ^super value: aBoolean asCBooleanValue
! !


