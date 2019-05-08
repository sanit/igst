"======================================================================
|
|   CompiledMethod Method Definitions
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



ArrayedCollection variableByteSubclass: #CompiledMethod
       instanceVariableNames: 'descriptor methodHeader literals'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Implementation'
!

CompiledMethod comment:
'I represent methods that have been compiled.  I can recompile
methods from their source code, I can invoke Emacs to edit the source code
for one of my instances, and I know how to access components of my
instances.' !


!CompiledMethod class methodsFor: 'lean images'!

stripSourceCode
    "Remove all the references to method source code from the system"
    self allInstancesDo: [ :each | each stripSourceCode ]
! !


!CompiledMethod class methodsFor: 'instance creation'!

newMethod: numBytecodes header: anInteger numLiterals: numLiterals
    "Answer a new CompiledMethod with room for the given bytes and the
     given header"
    ^(self new: numBytecodes)
        header: anInteger
        literals: (Array new: numLiterals)
!

newMethod: numBytecodes header: anInteger literals: literals
    "Answer a new CompiledMethod with room for the given bytes and the
     given header"
    ^(self new: numBytecodes)
        header: anInteger
        literals: literals
! !
   

!CompiledMethod methodsFor: 'basic'!

methodCategory
    "Answer the method category"
    ^descriptor category
!

methodCategory: aCategory
    "Set the method category to the given string"
    descriptor category: aCategory
!

methodSourceCode
    "Answer the method source code (a FileSegment or String or nil)"
    ^descriptor sourceFile = 'stdin'
        ifTrue: [ nil]
        ifFalse: [ descriptor sourceCode ]
!

methodSourceString
    "Answer the method source code as a string"
    ^descriptor sourceFile = 'stdin'
        ifTrue: [ nil]
        ifFalse: [ descriptor sourceString ]
!

methodSourceFile
    "Answer the file where the method source code is stored"
    ^descriptor sourceFile = 'stdin'
        ifTrue: [ nil]
        ifFalse: [ descriptor sourceFile ]
!

methodSourcePos
    "Answer the location where the method source code is stored in
     the methodSourceFile"
    ^descriptor  sourceFile = 'stdin'
        ifTrue: [ nil]
        ifFalse: [ descriptor sourcePos ]
!

= aMethod
    "Answer whether the receiver and aMethod are equal"

    self == aMethod ifTrue: [ ^true ].
    self class == aMethod class ifFalse: [ ^false ].
    descriptor = aMethod getDescriptor ifFalse: [ ^false ].
    methodHeader = aMethod getHeader ifFalse: [ ^false ].
    1 to: self numLiterals do:
    	[ :i |  (self literalAt: i) = (aMethod literalAt: i)
	    	    ifFalse: [ ^false ] ].
    1 to: self numBytecodes do:
    	[ :i | (self bytecodeAt: i) = (aMethod bytecodeAt: i)
	    	    ifFalse: [ ^false ] ].
    ^true
!

hash
    "Answer an hash value for the receiver"

    | hashValue |
    hashValue :=
    	(descriptor hash bitAnd: 16r1FFFFFFF) +
    	(methodHeader hash bitAnd: 16r1FFFFFFF) +
    	(literals hash bitAnd: 16r1FFFFFFF).

    1 to: self basicSize do:
    	[ :i | hashValue := ((hashValue bitShift: 1) bitAnd: 16r1FFFFFFF) +
    	    	    	    (self basicAt: i) ].
    ^hashValue
!!



!CompiledMethod methodsFor: 'accessing'!

literals
    ^self literals
!

literalAt: anIndex
    "Answer the anIndex-th literal"
    ^literals at: anIndex
!

literalAt: anInteger put: aValue
    "Store aValue as the anIndex-th literal"
    ^literals at: anInteger put: aValue
!

bytecodeAt: anIndex
    "Answer the anIndex-th bytecode"
    ^self basicAt: (anIndex + self bytecodeStart)
!

bytecodeAt: anIndex put: aValue
    "Store aValue as the anIndex-th bytecode"
    ^self basicAt: (anIndex + self bytecodeStart) put: aValue
!

flags
    "Private - Answer the optimization flags for the receiver"
    ^((methodHeader bitShift: -5) bitAnd: 16r3)
!

primitive
    "Answer the primitive called by the receiver"
    ^(methodHeader bitShift: -19) bitAnd: 16r3FF
!

numArgs
    "Answer the number of arguments for the receiver"
    ^methodHeader bitAnd: 16r1F
!

numTemps
    "Answer the number of temporaries for the receiver"
    ^(methodHeader bitShift: -13) bitAnd: 16r3F
!

stackDepth
    "Answer the number of stack slots needed for the receiver"
    ^((methodHeader bitShift: -7) bitAnd: 16r3F) * 4
!

numLiterals
    "Answer the number of literals for the receiver"
    ^literals size
!!



!CompiledMethod methodsFor: 'copying'!

copy
    "Answer a shallow copy of the receiver"
    ^(CompiledMethod newMethod: self basicSize
		     header: methodHeader) shallowCopyMethodContents: self
!

shallowCopy
    "Answer a shallow copy of the receiver"
    ^(CompiledMethod newMethod: self basicSize
		     header: methodHeader) shallowCopyMethodContents: self
!

deepCopy
    "Answer a deep copy of the receiver"
    ^(CompiledMethod newMethod: self basicSize
		     header: methodHeader) deepCopyMethodContents: self
!!



!CompiledMethod methodsFor: 'debugging'!

inspect
    "Print the contents of the receiver in a verbose way."

    | class instVars lit |
    class := self class.
    instVars := class instVarNames.
    Transcript
        nextPutAll: 'An instance of ';
        print: class.

    1 to: (instVars size - 1) do: "assumes literals is last inst var"
    	[ :i | Transcript
    	           nextPutAll: '  ';
	           nextPutAll: (instVars at: i);
	           nextPutAll: ': ';
	           print: (self instVarAt: i);
	           nl ].
    Transcript
        nextPutAll: '  Header Flags: ';
        nl;
        nextPutAll: '    flags: ';
        print:      self flags;
        nl;
        nextPutAll: '    primitive index: ';
        print:      self primitive;
        nl;
        nextPutAll: '    number of arguments: ';
        print:      self numArgs;
        nl;
        nextPutAll: '    number of temporaries: ';
        print:      self numTemps;
        nl;
        nextPutAll: '    number of literals: ';
        print:      self numLiterals;
        nl;
        nextPutAll: '    needed stack slots: ';
        print:      self stackDepth;
        nl.

    self numLiterals > 0
    	ifTrue: [ Transcript nextPutAll: '  literals: ['; nl.
                  1 to: self numLiterals do:
                      [ :i | Transcript
                                 nextPutAll: '    [';
                                 print: (i - 1);
                                 nextPutAll: ']: '.

                             lit := self literalAt: i.
                             "Avoid an infinite loop"
                             (lit class == Association)
                                 ifTrue: [
                                     Transcript nextPutAll: 
                                         '(Reference to global/pool variable ', lit key, ')'; nl ]
                                 ifFalse: [ lit storeNl ].
                      ].
                  Transcript nextPutAll: '  ]'; nl ].
    Transcript nextPutAll: '  byte codes: ['; nl.
    self printByteCodesOn: Transcript.
    Transcript nextPutAll: '  ]'; nl.
!!



!CompiledMethod methodsFor: 'debugging'!

breakpointAt: byteIndex
    "Put a break-point at the given bytecode"

    Debugger recordOldByte: (self bytecodeAt: byteIndex)
	     atIndex: byteIndex
	     forMethod: self.
    self bytecodeAt: byteIndex put: Debugger debugByte
!

breakAtLine: lineNumber
    self notYetImplemented
!

removeBreakpointAt: byteIndex
    "Remove the break-point at the given bytecode (don't fail if none
     was set"

    | oldByte |
    oldByte := Debugger origByteAt: byteIndex forMethod: self.
    oldByte notNil 
	ifTrue: [ self bytecodeAt: byteIndex put: oldByte ]
!!



!CompiledMethod methodsFor: 'printing'!

printOn: aStream
    "Print the receiver's source code on aStream"

    | source |
    source := self methodSourceString.
    source isNil ifTrue: [ source := '" *** No source code available *** "' ].
    aStream nextPutAll: source.
!

storeOn: aStream
    "Print code to create the receiver on aStream"
    aStream
        nextPutAll: '((';
        print: self class;
        nextPutAll: ' literals: ';
        store: ((1 to: self numLiterals) collect: [ :i | self literalAt: i ]);
        nextPutAll: ' numArgs: ';
        store: self numArgs;
        nextPutAll: ' numTemps: ';
        store: self numTemps;
        nextPutAll: ' primitive: ';
        store: self primitive;
        nextPutAll: ' bytecodes: ';
        store: self asByteArray;
        nextPutAll: ' source: ';
        store: self methodSourceCode;
        nextPutAll: ') makeLiteralsReadOnly; yourself)'
!!


!CompiledMethod methodsFor: 'testing accesses'!

containsLiteral: anObject
    "Answer if the receiver contains a literal which is equal to anObject."
    | lit |
    1 to: self numLiterals do: [ :i |
        lit := self literalAt: i.
        lit = anObject ifTrue: [^true ].
    ].
    ^false
!

refersTo: anObject
    "Answer whether the receiver refers to the given object"
    | byte |
    (self containsLiteral: anObject) ifTrue: [ ^true ].

    "Look for symbols referenced to by special message sends"
    byte := #(+	    	-   	    <	    	>
             <=     	>=  	    =	    	~=
             *	    	/   	    \\	    	@
             bitShift:	//  	    bitAnd: 	bitOr:
             at:    	at:put:	    size    	next
             nextPut:	atEnd	    ==	    	class
             blockCopy:temporaries:
	     value	value:      do:
             new        new:        isNil       notNil)

        indexOf: anObject ifAbsent: [ ^false ].

    byte := byte + 175.
    self allByteCodeIndicesDo: [ :i :bytecode |
        byte = bytecode ifTrue: [^true].
    ].
    ^false
!

accesses: instVarIndex
    "Answer whether the receiver access the instance variable with the given
     index"

    | byte |
    self flags = 2
        ifTrue: [ ^((methodHeader bitShift: -6) bitAnd: 16r1F) = instVarIndex].

    self allByteCodeIndicesDo: [ :i :byte |
        (byte < 16) & (byte = instVarIndex) ifTrue: [^true].   "push"

        (byte >= 96) & (byte < 104) & (byte - 96 = instVarIndex)
            ifTrue: [^true].    "pop"
            
        (#(128 129 130 142) includes: byte) ifTrue: [ "2 byte stack operation"
            "This deserves an explanation. The correct test would be
             (nextByte < 64) & ((nextByte bitAnd: 63) = instVarIndex,
             but: a) if the next byte is < 64 the bitwise and has no effect;
             b) instVarIndex must be < 64, so the next byte must be < 64 too
             for it to be equal to instVarIndex... OUCH!!"

            (self bytecodeAt: i + 1) = instVarIndex ifTrue: [^true]
        ]
    ].
    ^false
!!


!CompiledMethod methodsFor: 'private-printing'!

printByteCodesOn: aStream
    "Private - Disassemble the bytecode instructions to aStream"

    self allByteCodeIndicesDo: [ :i :byte |
        self printByte: byte at: i on: aStream
    ]
!

printByte: byte at: anIndex on: aStream
    "Private - Print the byte bytecode (starting at anIndex) on aStream"

    aStream
        nextPutAll: '    [';
        print: anIndex;
        nextPutAll: ']: '.

    byte < 95 ifTrue:
	[ self printIndexedAt: anIndex on: aStream ].
    (byte between: 96 and: 111) ifTrue:
	[ self emitSimplePop: byte on: aStream ].
    (byte between: 112 and: 125) ifTrue: 
	[ self emitBuiltin: byte on: aStream ].
    byte == 126 ifTrue: 
	[ self print3ByteOp: anIndex + 1 on: aStream ].
    "127 is the debugger breakpoint and we don't get it here"
    byte == 128 ifTrue: 
	[ self print2ByteStackOp: 'push' at: anIndex on: aStream ].
    byte == 129 ifTrue: 
	[ self print2ByteStoreOp: 'store' at: anIndex on: aStream ].
    byte == 130 ifTrue: 
	[ self print2ByteStoreOp: 'pop and store' at: anIndex on: aStream ].
    (byte between: 131 and: 134) ifTrue: 
	[ self emitIndexedSend: anIndex on: aStream ].
    byte == 135 ifTrue:
	[ aStream nextPutAll: 'pop stack top ' ].
    byte == 136 ifTrue:
	[ aStream nextPutAll: 'duplicate stack top' ].
    byte == 137 ifTrue: 
	[ aStream nextPutAll: 'push current context' ].
    byte == 138 ifTrue: 
	[ self emitOuterStackOp: anIndex + 1 on: aStream ].
    byte == 139 ifTrue: 
	[ aStream nextPutAll: 'no operation' ].
    byte == 140 ifTrue: 
	[ aStream nextPutAll: 'set stack top to self' ].
    byte == 141 ifTrue: 
	[ aStream nextPutAll: 'set stack top to 1' ].
    byte == 142 ifTrue: 
	[ self print2ByteStackOp: 'set stack top to' at: anIndex on: aStream ].
    byte == 143 ifTrue:
	[ aStream nextPutAll: 'INVALID bytecode 143' ].
    (byte between: 144 and: 175) ifTrue:
	[ self printJump: anIndex on: aStream ].
    (byte between: 176 and: 191) ifTrue:
	[ aStream nextPutAll: 'send arithmetic message '.
	  (#(+	    	-   	    <	    	>
	     <=     	>=  	    =	    	~=
	     *	    	/   	    \\	    	@
	     bitShift:	//  	    bitAnd: 	bitOr:)
	       at: byte - 175) printOn: aStream.
	].
    (byte between: 192 and: 207) ifTrue:
	[ aStream nextPutAll: 'send special message '.
	  (#(at:    	at:put:	    size    	next
    	    nextPut:	atEnd	    ==	    	class
            blockCopy:temporaries:
            value	value:      do:
            new        new:        isNil       notNil)
	       at: byte - 191) printOn: aStream.
	].
    (byte between: 208 and: 255) ifTrue:
	[ self printSmallArgSend: byte on: aStream ].
    aStream nextPut: (Character nl).
!	

printIndexedAt: anIndex on: aStream
    "Private - Print the push bytecode starting at anIndex on aStream,
     byte<=95"

    | byte index |
    byte := self bytecodeAt: anIndex.
    byte <= 15 ifTrue:
    	[ ^self pushIndexed: 'Instance Variable' 
		withIndex: (byte bitAnd: 15)
		on: aStream ].
    byte <= 31 ifTrue:
	[ ^self pushIndexed: 'Temporary'
		withIndex: (byte bitAnd: 15)
		on: aStream ].
    byte <= 63 ifTrue:
	[ ^self pushIndexed: 'Literal'
		withIndex: (byte bitAnd: 31)
		on: aStream ].

    " >= 64 case here "
    aStream nextPutAll: 'push Global Variable['.
    (byte bitAnd: 31) printOn: aStream.
    aStream nextPutAll: '] = '.
    self printAssociationKeyFor: (byte bitAnd: 31) on: aStream
!

pushIndexed: indexLabel withIndex: anIndex on: aStream
    "Private - Print a push bytecode on aStream"
    aStream nextPutAll: 'push '.
    aStream nextPutAll: indexLabel.
    aStream nextPut: $[.
    aStream print: anIndex.
    aStream nextPut: $]
!

emitSimplePop: byte on: aStream
    "Private - Print the byte bytecode (a pop) on aStream, 96<=byte<=111"
    (byte between: 96 and: 103) ifTrue:
	[ aStream nextPutAll: 'pop and store instance variable['.
	  (byte bitAnd: 7) printOn: aStream.
	  aStream nextPut: $] ].
    (byte between: 104 and: 111) ifTrue:
	[ aStream nextPutAll: 'pop and store Temporary['.
	  (byte bitAnd: 7) printOn: aStream.
	  aStream nextPut: $] ].
!

emitBuiltin: byte on: aStream
    "Private - Print the byte bytecode (a special push or a return) on
     aStream, 112<=byte<=125"
    byte == 112 ifTrue: [ aStream nextPutAll: 'push self' ].
    byte == 113 ifTrue: [ aStream nextPutAll: 'push true' ].
    byte == 114 ifTrue: [ aStream nextPutAll: 'push false' ].
    byte == 115 ifTrue: [ aStream nextPutAll: 'push nil' ].
    byte == 116 ifTrue: [ aStream nextPutAll: 'push -1' ].
    byte == 117 ifTrue: [ aStream nextPutAll: 'push 0' ].
    byte == 118 ifTrue: [ aStream nextPutAll: 'push 1' ].
    byte == 119 ifTrue: [ aStream nextPutAll: 'push 2' ].
    byte == 120 ifTrue: [ aStream nextPutAll: 'return self' ].
    byte == 121 ifTrue: [ aStream nextPutAll: 'return true' ].
    byte == 122 ifTrue: [ aStream nextPutAll: 'return false' ].
    byte == 123 ifTrue: [ aStream nextPutAll: 'return nil' ].
    byte == 124 ifTrue: [ aStream nextPutAll: 'return Message stack top' ].
    byte == 125 ifTrue: [ aStream nextPutAll: 'return Block stack top' ].
!

print3ByteOp: index on: aStream 
    "Private - Decode the 3-byte push literal bytecode (126) onto aStream"
    | what num |
    what := self bytecodeAt: index.
    num := self bytecodeAt: index + 1.
    num := what * 256 + num.
    what := what // 64.
    what = 0 ifTrue: [ aStream nextPutAll: 'push Literal[' ].
    what = 1 ifTrue: [ aStream nextPutAll: 'push Global Variable[' ].
    what = 2 ifTrue: [ aStream nextPutAll: 'store Global Variable[' ].
    what = 3 ifTrue: [ aStream nextPutAll: 'pop and store Global Variable[' ].
    aStream
        print: num;
        nextPut: $].

    what = 0 ifTrue: [ ^self ]
    aStream nextPutAll: ' = '.
    self printAssociationKeyFor: num on: aStream
!

print2ByteStackOp: opName at: anIndex on: aStream
    | nextByte locationName locIndex |
    nextByte := self bytecodeAt: anIndex + 1.
    locIndex := nextByte bitShift: -6.
    locationName := self indexedLocationName: locIndex.
    aStream nextPutAll: opName;
            space;
	    nextPutAll: locationName;
	    nextPutAll:'['.
    (nextByte bitAnd: 63) printOn: aStream.
    aStream nextPut: $].
    locIndex == 3 ifTrue:
	[ aStream nextPutAll: ' = '.
	  self printAssociationKeyFor: (nextByte bitAnd: 63) on: aStream ]
!

indexedLocationName: locIndex
    "Private - Answer the location name for the given index"
    ^#('Instance Variable' 'Temporary' 'Literal' 'Global Variable')
	at: locIndex + 1
!

print2ByteStoreOp: opName at: anIndex on: aStream
    | nextByte locationName locIndex |
    nextByte := self bytecodeAt: anIndex + 1.
    locIndex := nextByte bitShift: -6.
    locationName := self indexedLocationName: locIndex.
    locIndex == 2 ifTrue: [ aStream nextPutAll: 'INVALID ' ].
    aStream nextPutAll: opName;
            space;
	    nextPutAll: locationName;
	    nextPutAll:'['.
    (nextByte bitAnd: 63) printOn: aStream.
    aStream nextPut: $].
    locIndex == 3 ifTrue:
	[ aStream nextPutAll: ' = '.
	  self printAssociationKeyFor: (nextByte bitAnd: 63) on: aStream ]
!

emitOuterStackOp: stackOp at: anIndex on: aStream
    "Private - Decode the 3-byte outer-temporary bytecode (138) onto aStream"
    | what num |
    what := self bytecodeAt: anIndex.
    what := what // 64.
    what = 0 ifTrue: [ aStream nextPutAll: '(invalid)' ].
    what = 1 ifTrue: [ aStream nextPutAll: 'push' ].
    what = 2 ifTrue: [ aStream nextPutAll: 'store' ].
    what = 3 ifTrue: [ aStream nextPutAll: 'pop and store' ].

    aStream
        nextPutAll: ' outer Temporary[';
        print: ((self bytecodeAt: anIndex) bitAnd: 63);
        nextPutAll: '] scopes = ';
        print: (self bytecodeAt: anIndex + 1)
!

emitIndexedSend: anIndex on: aStream
    | byte byte1 byte2 toSuper |
    byte := self bytecodeAt: anIndex.
    byte := byte - 131.		"transform to 0..3"
    byte <= 1 ifTrue: [ toSuper := '' ]
	      ifFalse: [ toSuper := 'to Super ' ].
    (byte == 0) | (byte == 2)
	ifTrue:
	    [ byte1 := self bytecodeAt: anIndex + 1.
	      self emitGenericSend: toSuper index: (byte1 bitAnd: 31)
		   args: (byte1 bitShift: -5) on: aStream.
	      ^2 ]
	ifFalse:
	    [ byte1 := self bytecodeAt: anIndex + 1.
	      byte2 := self bytecodeAt: anIndex + 2.
	      self emitGenericSend: toSuper index: byte2
		   args: byte1 on: aStream.
	      ^3]
!


emitGenericSend: toSuper index: anIndex args: numArgs on: aStream
    aStream nextPutAll: 'send ';
	nextPutAll: toSuper;
	nextPutAll: 'selector '.
    anIndex printOn: aStream.
    aStream nextPutAll: ', '.
    numArgs printOn: aStream.
    aStream nextPutAll: ' args = '.
    self printLiteralSymbolAt: anIndex on: aStream
!

printJump: anIndex on: aStream
    | byte |
    byte := self bytecodeAt: anIndex.
    byte <= 151 ifTrue:
	[ aStream nextPutAll: 'jump to '.
	  ((byte bitAnd: 7) + anIndex + 1 + 1 ) printOn: aStream.
	  ^1 ].
    byte <= 159 ifTrue:
	[ aStream nextPutAll: 'jump to '.
	  ((byte bitAnd: 7) + anIndex + 1 + 1 ) printOn: aStream.
	  aStream nextPutAll: ' if false'.
	  ^1 ].
    byte <= 167 ifTrue:
	[ aStream nextPutAll: 'jump to '.
	  (((byte bitAnd: 7) - 4) * 256 + (self bytecodeAt: anIndex + 1)
	      + anIndex + 2) printOn: aStream.
	  ^2 ].
    byte <= 171 ifTrue:
	[ aStream nextPutAll: 'pop and jump to '.
	  ((byte bitAnd: 3) * 256 + (self bytecodeAt: anIndex + 1)
	      + anIndex + 2) printOn: aStream.
	  aStream nextPutAll: ' if true'.
	  ^2 ].
    byte <= 175 ifTrue:
	[ aStream nextPutAll: 'pop and jump to '.
	  ((byte bitAnd: 3) * 256 + (self bytecodeAt: anIndex + 1)
	      + anIndex + 2) printOn: aStream.
	  aStream nextPutAll: ' if false'.
	  ^2 ]
!

printSmallArgSend: byte on: aStream
    | numArgs index |
    index := (byte - 208) bitAnd: 15.
    numArgs := (byte - 208) // 16.
    aStream nextPutAll: 'send selector '.
    index printOn: aStream.
    aStream nextPutAll: ', '.
    numArgs printOn: aStream.
    numArgs == 1 
	ifTrue: [ aStream nextPutAll: ' arg' ]
	ifFalse: [ aStream nextPutAll: ' args' ].
    aStream nextPutAll: ' = '.
    self printLiteralSymbolAt: index on: aStream
!

printAssociationKeyFor: anIndex on: aStream
    | assoc |
    assoc := self literalAt: anIndex + 1.
    aStream nextPutAll: assoc key
!

printLiteralSymbolAt: anIndex on: aStream
    (self literalAt: anIndex + 1) printOn: aStream
! !


!CompiledMethod methodsFor: 'private'!

shallowCopyMethodContents: aMethod 
    "Private - copy the descriptor, literals and bytecodes
    from aMethod. Don't need to copy the method header; it's already done"

    descriptor := aMethod getDescriptor.
    literals := aMethod literals.
    1 to: aMethod numBytecodes do:
        [ :i | self bytecodeAt: i put: (aMethod bytecodeAt: i) ]
!

deepCopyMethodContents: aMethod
    "Private - Deep copy the descriptor, literals and bytecodes 
     from aMethod. Don't need to copy the method header; it's already done"

    descriptor := aMethod getDescriptor copy.
    literals := aMethod literals deepCopy.
    1 to: aMethod numBytecodes do:
        [ :i | self bytecodeAt: i put: (aMethod bytecodeAt: i) ]
!

allByteCodeIndicesDo: aBlock
    "Private - Evaluate aBlock passing each of the index where a
     new bytecode instruction starts"

    | numBytes i byte |
    i := 1.
    numBytes := self numBytecodes.
    [ i <= numBytes ] whileTrue: [
        byte := self bytecodeAt: i.
        byte == 127 ifTrue: [
            byte := Debugger origByteAt: i forMethod: self
        ].
        aBlock value: i value: byte.

        i := i + (self bytecodeSizeAt: i)
    ]
!

bytecodeSizeAt: anIndex
    "Private - Answer the size of the bytecode instruction starting at anIndex"

    | byte |
    byte := self bytecodeAt: anIndex.
    byte == 127
	ifTrue: [ byte := Debugger origByteAt: anIndex forMethod: self ].

    byte == 126 ifTrue: [ ^3 ].
    byte < 128 ifTrue: [^1].
    byte > 175 ifTrue: [^1].
    byte == 128 ifTrue: [ ^2].
    byte == 129 ifTrue: [ ^2].
    byte == 130 ifTrue: [ ^2].
    byte < 135 ifTrue: [^(byte - 131) bitOr: 2].   "2 or 3"
    byte == 138 ifTrue: [^3].
    byte == 142 ifTrue: [^2].
    byte < 160 ifTrue: [^1].
    ^2
!

makeLiteralsReadOnly
    ^self makeLiteralsReadOnly: literals
!

makeLiteralsReadOnly: array
    array do: [ :each |
        each class == Array ifTrue: [ self makeLiteralsReadOnly: each ].
        each makeReadOnly: true
    ]
!

initialize
    descriptor := MethodInfo new
!

getDescriptor
    ^descriptor
!

stripSourceCode
    descriptor setSourceCode
!

header: hdr literals: lits
    methodHeader := hdr.
    literals := lits.
!

getHeader
     "The structure of a method header is as follows (from interp.h)

        3                   2                   1 
      1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     |.|.| prim index        | #temps    | depth / 4 |flg| #args   |1|
     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

     stackdepth 6 bits 8..13
     temporarycount 6 bits 14..19
     argscount 5 bits 1..5
     primitiveIndex 10 bits 20..29
     flags 2 bits 6..7
     flags 0 -- use arguments as they are, ignore prim index
     flags 1 -- return self
     flags 2 -- return instance variable
     flags 3 -- call the primitive indexed by primIndex"

    ^methodHeader
!

numBytecodes
    "Answer the number of bytecodes in the receiver"
    ^(self basicSize) - (self bytecodeStart)
!

bytecodeStart
    "Answer the index where the bytecodes start"
    ^0
!!
