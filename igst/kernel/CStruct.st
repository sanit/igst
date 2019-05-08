"======================================================================
|
|   C struct definition support classes.
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


CObject variableWordSubclass: #CStruct
      instanceVariableNames: ''
      classVariableNames: 'TypeMap'
      poolDictionaries: ''
      category: 'Language-C interface'
!



!Integer methodsFor: 'extension'!

alignTo: anInteger
    "Answer the receiver, truncated to the first higher or equal
     multiple of anInteger (which must be a power of two)"
    ^(self + anInteger - 1) bitClear: (anInteger - 1)
! !




!CStruct class methodsFor: 'instance creation'!

new
    "Allocate a new instance of the receiver. To free the memory after
     GC, remember to call #addToBeFinalized."
    ^self alloc: self sizeof
!

type
    "Answer a CType for the receiver"
    ^CType cObjectType: self
!!


!CStruct class methodsFor: 'subclass creation'!

initialize
    "Initialize the receiver's TypeMap"
    (TypeMap := Dictionary new)
	at: #long put: CLongType; 
	at: #uLong put: CULongType;
	at: #byte put: CByteType;
	at: #char put: CCharType;
	at: #uChar put: CUCharType;
	at: #uchar put: CUCharType;
	at: #short put: CShortType;
	at: #uShort put: CUShortType;
	at: #ushort put: CUShortType;
	at: #int put: CIntType;
	at: #uInt put: CUIntType;
	at: #uint put: CUIntType;
	at: #float put: CFloatType;
	at: #double put: CDoubleType;
	at: #string put: CStringType
!

newStruct: structName declaration: array
    "Create a new class with the given name that contains code
     to implement the given C struct - see documentation for more
     information"

    | type name newClass offset maxAlignment str inspStr |
    newClass := CStruct variableWordSubclass: structName asSymbol
		       instanceVariableNames: ''
		       classVariableNames: ''
		       poolDictionaries: ''
		       category: 'Synthetic Class'.

    offset := 0.
    maxAlignment := 1.
    inspStr := WriteStream on: (String new: 8).
    inspStr nextPutAll: 'inspectSelectorList'; nl; nextPutAll: '    ^#('.

    "Iterate through each member, doing alignment, size calculations,
     and creating accessor methods"
    array do: [ :dcl |
        name := dcl at: 1.
	type := dcl at: 2.
	self emitInspectTo: inspStr for: name.

	self computeTypeString: type block: [ :size :alignment :typeString | 
	    offset := offset alignTo: alignment.

	    maxAlignment := alignment max: maxAlignment.

	    str := WriteStream on: (String new: 20).
	    str nextPutAll: name;
	        nl;
	        nextPutAll: '    ^self at: ';
		print: offset;
		nextPutAll: ' type: ';
		nextPutAll: typeString.
	    newClass compile: str classified: 'accessing'.

	    str := WriteStream on: (String new: 20).
	    str nextPutAll: name;
	        nextPutAll: ': value';
	        nl;
	        nextPutAll: '    ^self at: ';
		print: offset;
	        nextPutAll: ' put: value type: ';
		nextPutAll: typeString.
	    newClass compile: str classified: 'accessing'.
            offset := offset + size
        ]
    ].

    newClass compile: inspStr contents, ')' classified: 'debugging'.
    self compileSize: offset align: maxAlignment for: newClass.
!
			
computeAggregateType: type block: aBlock
    "Private - Called by computeTypeString:block: for pointers/arrays.
    Format of type:
    	(array int 3) or
    	(ptr FooStruct)
    "
    | structureType |
    " ### Should check for 2 or 3 elts only "
    structureType := type at: 1.
    structureType == #array 
	ifTrue: [ ^self computeArrayType: type block: aBlock ].
    structureType == #ptr
	ifTrue: [ ^self computePtrType: type block: aBlock ].
!

computeTypeString: type block: aBlock
    "Private - Pass the size, alignment, and description of CType for aBlock,
    given the field description in `type' (the second element of each pair)."
    | typeInfo |
    type class == Array
	ifTrue: [ ^self computeAggregateType: type block: aBlock ].

    "must be a type name, either built in or struct"

    typeInfo := TypeMap at: type ifAbsent: [ nil ].

    typeInfo notNil
	ifTrue: [ aBlock value: typeInfo sizeof
			 value: typeInfo alignof
			 value: typeInfo storeString
        ]
	ifFalse: [ typeInfo := Smalltalk at: type.
		   aBlock value: typeInfo sizeof
			  value: typeInfo alignof
			  value: type, ' type'
        ]
!

	
computeArrayType: type block: aBlock
    "Private - Called by computeAggregateType:block: for arrays"
    | numElts elementType |
    elementType := type at: 2.
    numElts := type at: 3.
    self computeTypeString: elementType
         block: [ :size :alignment :typeString | 
	     aBlock value: size * numElts
		    value: alignment
		    value: '(CArrayCType elementType: ', typeString,
	                   ' numberOfElements: ', (numElts printString), ')'
         ]
!

computePtrType: type block: aBlock
    "Private - Called by computeAggregateType:block: for pointers"
    | subType |
    subType := type at: 2.
    self computeTypeString: subType
	 block: [ :size :alignment :typeString | 
	     aBlock value: CPtr sizeof
		    value: CPtr alignof
		    value: '(CPtrCType elementType: ', typeString, ')'
    ]
!


compileSize: size align: alignment for: aClass
    "Private - Compile sizeof and alignof methods"
    | sizeofMethod alignofMethod |

    sizeofMethod :=  'sizeof
    ^', (size alignTo: alignment) printString.
    alignofMethod := 'alignof
    ^', (alignment printString).

    aClass compile: sizeofMethod classified: 'accessing'.
    aClass class compile: sizeofMethod classified: 'accessing'.

    aClass compile: alignofMethod classified: 'accessing'.
    aClass class compile: alignofMethod classified: 'accessing'.
!

emitInspectTo: str for: name
    "Private - Emit onto the given stream the code for adding the
     given selector to the CStruct's inspector."

    str nl;
        next: 8 put: Character space;
        nextPutAll: name
! !


!CStruct methodsFor: 'instance creation'!


inspectSelectorList
    "Answer a list of selectors whose return values should be inspected
     by #inspect."

    "We can't call subclassResponsibility because #inspect should never
     fail - it would lead to recursive walkbacks. So answer an empty
     array.
     For subclasses, it will answer an Array of the selectors whose
     values are to be shown in the inspector."
    ^#()
!
inspect
    "Inspect the contents of the receiver"
    "This inspect method applies to every instance of the receiver
     and their subclasses, which only override #inspectSelectorList."
    self printNl.
    self inspectSelectorList do: [ :each |
        Transcript
           nextPutAll: '    ';
           nextPutAll: each;
           nextPutAll: ': ';
	   print: (self perform: each) value;
	   nl
    ].
! !

CStruct initialize!

