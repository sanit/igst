"======================================================================
|
|   FileStream Method Definitions
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


ReadWriteStream subclass: #FileStream
		instanceVariableNames: 'file name buffer isPipe'
		classVariableNames: 'Verbose Record Includes'
		poolDictionaries: ''
		category: 'Streams-Files'
!

FileStream comment: 
'My instances are what conventional programmers think of as files.
My instance creation methods accept the name of a disk file (or any named
file object, such as /dev/rmt0 on UNIX or MTA0: on VMS).'
!



!FileStream class methodsFor: 'basic'!

new
    self shouldNotImplement
!

append
    "Open for writing.  The file is created if it does not exist.  The stream is
    positioned at the end of the file."

    ^'a'
!

create
    "Open for reading and writing.  The file is created if it does not exist,
    otherwise it is truncated.  The stream is positioned at the beginning of
    the file."

    ^'w+'
!

readWrite
    "Open for reading and writing.  The stream is positioned at the beginning of
    the file."

    ^'r+'
!

open: fileName
    "Open fileName in read-write mode - fail if the file cannot be opened.
     Else answer a new FileStream.
     The file will be automatically closed upon GC if the object is not
     referenced anymore, but you should close it with #close anyway.
     To keep a file open, send it #removeToBeFinalized"
    ^self open: fileName mode: FileStream readWrite
!

open: fileName mode: fileMode
    "Open fileName in the required mode - answered by #append, #create,
     #readWrite, #read or #write - and fail if the file cannot be opened.
     Else answer a new FileStream. For mode anyway you can use any
     standard C non-binary fopen mode.
     The file will be automatically closed upon GC if the object is not
     referenced anymore, but it is better to close it as soon as you're
     finished with it anyway, using #close. To keep a file open even when
     no references exist anymore, send it #removeToBeFinalized"

    ^self basicNew
        initialize;
        fileOp: 0 with: fileName with: fileMode ifFail: [ ^self error: 
            'could not open ', fileName ];
        addToBeFinalized;
        yourself
!

open: fileName mode: fileMode ifFail: aBlock
    "Open fileName in the required mode - answered by #append, #create,
     #readWrite, #read or #write - and evaluate aBlock if the file cannot
     be opened. Else answer a new FileStream. For mode anyway you can use any
     The file will be automatically closed upon GC if the object is not
     referenced anymore, but it is better to close it as soon as you're
     finished with it anyway, using #close. To keep a file open even when
     no references exist anymore, send it #removeToBeFinalized"

    ^self basicNew
        initialize;
        fileOp: 0 with: fileName with: fileMode ifFail: [ ^aBlock value ];
        addToBeFinalized;
        yourself
!

popen: commandName dir: direction
    "Open a pipe on the given command and fail if the file cannot be opened.
     Else answer a new FileStream.
     The pipe will not be automatically closed upon GC, even if the object
     is not referenced anymore, because when you close a pipe you have to wait
     for the associated process to terminate. To enforce automatic closing of
     the pipe, send it #addToBeFinalized.
     direction is returned by #read or #write ('r' or 'w') and is interpreted
     from the point of view of Smalltalk: reading means Smalltalk reads the
     standard output of the command, writing means Smalltalk writes the standard input of the command. The other channel
     (stdin when reading, stdout when writing) is the same as GST's, unless
     commandName alters it."

    ^self basicNew
        initialize;
        fileOp: 7 with: commandName with: direction ifFail: [ ^self error: 
            'could not open pipe on ', commandName ];
        yourself
!

popen: commandName dir: direction ifFail: aBlock
    "Open a pipe on the given command and evaluate aBlock
     file cannot be opened. Else answer a new FileStream.
     The pipe will not be automatically closed upon GC, even if the object
     is not referenced anymore, because when you close a pipe you have to wait
     for the associated process to terminate. To enforce automatic closing of
     the pipe, send it #addToBeFinalized.
     direction is interpreted from the point of view of Smalltalk: reading
     means that Smalltalk reads the standard output of the command, writing
     means that Smalltalk writes the standard input of the command"

    ^self basicNew
        initialize;
        fileOp: 7 with: commandName with: direction ifFail: [ ^aBlock value ];
        yourself
!

read
    "Open text file for reading.  The stream is positioned at the beginning of
    the file."
    ^'r'
!

write
    "Truncate file to zero length or create text file for writing.  The stream
    is positioned at the beginning of the file."
    ^'w'
! !


!FileStream class methodsFor: 'file-in'!

fileIn: aFileName
    "File in the aFileName file"

    | oldIncludes newCollection file |
    Verbose ifTrue: [ Transcript nextPutAll: 'Loading ', aFileName; nl ].

    Record ifTrue: [
        newCollection := OrderedCollection new.
	Includes add: aFileName -> newCollection.
	oldIncludes := Includes.
	Includes := newCollection
    ].

    (self open: aFileName mode: FileStream read)
        fileIn;
        close.

    Record ifTrue: [ Includes := oldIncludes ].
!

fileIn: aFileName line: lineInteger from: realFileName at: aCharPos
    "File in the aFileName file giving errors such as if it was loaded
     from the given line, file name and starting position (instead of 1)"

    (self open: aFileName mode: FileStream read)
        fileInLine: lineInteger fileName: realFileName at: aCharPos;
        close.
!

fileIn: aFileName ifMissing: aSymbol
    "Conditionally do a file in, only if the key (often a class) specified
     by 'aSymbol' is not present in the Smalltalk system dictionary already"

    Smalltalk at: aSymbol
	      ifAbsent: [ self fileIn: aFileName ]
!

fileIn: aFileName ifTrue: aBoolean
    "Conditionally do a file in, only if the supplied boolean is true"
							       
    aBoolean 
        ifTrue: [ self fileIn: aFileName ]
!

require: assoc
    "Conditionally do a file in from the value of assoc, only if the
     key of assoc is not present in the Smalltalk system dictionary already"
    Smalltalk at: assoc key
	      ifAbsent: [ self fileIn: assoc value ]
!

verbose: verboseFlag
    "Set whether Smalltalk should output debugging messages when filing in"
    | oldVerbose |
    oldVerbose := Verbose.
    Verbose := verboseFlag.
    ^oldVerbose
!

record: recordFlag
    "Set whether Smalltalk should record information aboutnested file-ins.
     When recording is enabled, use #generateMakefileOnto: to automatically
     generate a valid makefile for the intervening file-ins."
    | oldRecord |
    oldRecord := Record.
    Record := recordFlag.
    Includes := Record 
	ifTrue: [ OrderedCollection new ]
	ifFalse: [ nil ].
    ^oldRecord
!

generateMakefileOnto: aStream
    "Generate a make file for the file-ins since record was last set to true.
     Store it on aStream"
    aStream nextPutAll: 
'
#
# Automatically generated Smalltalk dependencies
#

'.
    self recursiveGenerateOnto: aStream with: Includes.
    aStream nextPutAll: 
'#
# End automatically generated Smalltalk dependencies
#

'.
!!


!FileStream class methodsFor: 'private'!

recursiveGenerateOnto: aStream with: includeCollection
    "Private - Generate a make file for the file in information in
     includeCollection. Store it on aStream"

    includeCollection isNil ifTrue: [ ^self ].
    includeCollection do: [ :include |
        include value size > 0 ifTrue: [
            aStream
                nextPutAll: include key;
		nextPutAll: ': '.
	
	    include value do: [ :subinclude |
	        aStream nextPutAll: subinclude key; space
	    ].
	    aStream nl; nl.
	    self recursiveGenerateOnto: aStream with: include value
	]
    ]
! !



!FileStream class methodsFor: 'initialization'!

initialize
    "Initialize the receiver's class variables"
    Record := Verbose := false.
! !



!FileStream methodsFor: 'basic'!

close
    "Close the file"
    self fileOp: 1.
    self removeToBeFinalized.
    file := nil.
!

finalize
    "Close the file if it is still open by the time the object becomes
    garbage."
    file isNil ifFalse: [ self close ].
!

flush
    "Flush the output buffer"
    self fileOp: 11
!

next
    "Return the next character in the file, or nil at eof"

    | ch |
    ^buffer isNil
	ifTrue: [ self fileOp: 2 ]
	ifFalse: [ ch := buffer. buffer := nil. ch ]
!

nextByte
    "Return the next byte in the file, or nil at eof"

    | ch |
    ^buffer isNil
	ifTrue: [ self fileOp: 12 ]
	ifFalse: [ ch := buffer. buffer := nil. ch asciiValue ]
!

nextPut: aChar
    "Store aChar on the file"
    buffer isNil ifFalse: [ self position: self primPosition - 1 ].
    self fileOp: 3 with: aChar ifFail: [
        File checkError ifFalse: [
            self error: 'invalid argument type'
        ]
    ]
!

nextPutByte: anInteger
    "Store aChar on the file"
    buffer isNil ifFalse: [ self position: self primPosition - 1 ].
    self fileOp: 3 with: anInteger ifFail: [
        File checkError ifFalse: [
            self error: 'invalid argument type'
        ]
    ]
!

nextPutByteArray: aByteArray
    "Store aByteArray on the file"
    ^self nextPutAll: aByteArray asString
!

peek
    "Peek a character in the file, don't change the position"
    buffer isNil
        ifTrue: [ buffer := self next ].
    ^buffer
!

peekFor: aCharacter
    "Peek a character in the file, don't change the position"
    buffer isNil
        ifTrue: [ buffer := self next ].
    
    ^buffer = aCharacter
        ifTrue: [ buffer := nil. true ]
        ifFalse: [ false ]
    
!

reset
    "Reset the stream to its beginning"
    self position: 0
!

position: bytePosition
    "Set the position of the file pointer in the receiver"
    buffer := nil.
    self fileOp: 4 with: bytePosition
! 

position
    "Return the position of the file pointer in the receiver"
    ^buffer isNil
        ifTrue: [ self primPosition ]
        ifFalse: [ self primPosition - 1 ]
!

contents
    "Answer the whole contents of the file"
    | size contents |
    size := self size.
    ^size == 0
	ifTrue: [
	    contents := WriteStream on: (String new: 1).
	    [ self atEnd ] whileFalse: [ contents nextPut: self next ].
	    contents contents
        ]
	ifFalse: [ ^self next: size ]
!

copyFrom: start to: end
    "Answer the contents of the file between the two given positions"
    | pos str |
    pos := self position.
    self position: start.

    str := self species new: end - start + 1.
    1 to: end - start + 1 do:
	[ :i | str at: i put: (self next) ].

    self position: pos.
    ^str
!

size
    "Return the current size of the file, in bytes"
    ^self fileOp: 8

! !


!FileStream methodsFor: 'accessing'!

isPipe
    "Return whether the file is a pipe or an actual disk file"
    ^isPipe
!

name
    "Return the name of the file"
    ^name
! !


!FileStream methodsFor: 'printing'!

printOn: aStream
    "Print a representation of the receiver on aStream"
    aStream
        nextPut: $(;
        nextPutAll: (self isPipe ifTrue: ['Pipe on '] ifFalse: ['File ']);
        nextPutAll: name;
        nextPut: $)
! !
    

!FileStream methodsFor: 'overriding inherited methods'!

setToEnd
    "Reset the file pointer to the end of the file"
    self position: self size
!

skip: anInteger
    "Skip anInteger bytes in the file"
    | pos |
    pos := ((self position + anInteger) max: 0) min: self size - 1.
    self position: pos
!

reverseContents
    "Return the contents of the file from the last byte to the first"
    ^self contents reverse
!

isEmpty
    "Answer whether the receiver is empty"
    ^self size == 0
!

nextPutAll: aCollection
    "Put all the characters in aCollection in the file"
    | string |
    buffer isNil ifFalse: [ self position: self primPosition - 1 ].

    "The primitive only works for strings."
    (aCollection isKindOf: String)
        ifTrue: [ ^self fileOp: 9 with: aCollection ].

    string := String new: aCollection size.
    string replaceFrom: 1 to: string size with: aCollection startingAt: 1.
    self fileOp: 9 with: string
!

next: anInteger
    "return the next 'anInteger' characters from the stream, as a String."
    | data buf |
    buffer isNil ifTrue: [ ^self fileOp: 10 with: anInteger ].

    data := self fileOp: 10 with: anInteger - 1.
    buf := buffer.
    buffer := nil.
    ^(String new: anInteger)
        at: 1 put: buf;
        replaceFrom: 2 to: anInteger with: data startingAt: 1
!!




!FileStream methodsFor: 'testing'!

atEnd
    "Answer whether data has come to an end"
    ^self fileOp: 6

!!


!FileStream methodsFor: 'private'!

basicNextByte
    "Private - Return the next byte in the stream, or nil at eof"

    | ch |
    ^buffer isNil
	ifTrue: [ self fileOp: 12 ]
	ifFalse: [ ch := buffer. buffer := nil. ch asciiValue ]
!

basicNextPutByte: anInteger
    "Private - Store anInteger in the file"
    self fileOp: 3 with: anInteger ifFail: [
        File checkError ifFalse: [
            self error: 'invalid argument type'
        ]
    ]
!

initialize
    collection := ''
!

primPosition
    ^self fileOp: 5
!!


!FileStream methodsFor: 'class type methods'!

isBinary
    "We answer characters, so answer false"
    ^false
!

isText
    "We answer characters, so answer true"
    ^true
!

species
    ^String
!!

FileStream initialize!
