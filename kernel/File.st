"======================================================================
|
|   File Method Definitions
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


Object subclass: #File
       instanceVariableNames: 'name stat'
       classVariableNames: 'Epoch'
       poolDictionaries: ''
       category: 'Streams-Files'
! 


CStruct newStruct: #CStatStruct declaration: #( 
	(stMode uShort)          " protection "
	(stSize long)            " total size, in bytes "
	(stAtime long)		" time of last access "
	(stMtime long)		" time of last modification "
	(stCtime long)		" time of crea "
).

File
	defineCFunc: 'stat'
	withSelectorArgs: 'statOn: fileName into: statStruct'
	returning: #int
	args: #(string cObject).

"opendir and closedir needed to test for directories"

File
	defineCFunc: 'opendir'
	withSelectorArgs: 'openDir: dirName'
	returning: #cObject
	args: #(string).

File
	defineCFunc: 'closedir'
	withSelectorArgs: 'closeDir: dirObject'
	returning: #int
	args: #(cObject).

File class
	defineCFunc: 'unlink'
	withSelectorArgs: 'primRemove: fileName'
	returning: #void
	args: #(string).

File class
	defineCFunc: 'rename'
	withSelectorArgs: 'primRename: oldFileName to: newFileName'
	returning: #void
	args: #(string string).

File class
	defineCFunc: 'errno'
	withSelectorArgs: 'errno'
	returning: #long
	args: #()
!



!File class methodsFor: 'initialization'!

initialize
    "Initialize the receiver's class variables"
    Epoch := Date newDay: 2 month: #Jan year: 2000.
! !


!File class methodsFor: 'file operations'!

checkError
    "Return whether an error had been reported or not.
     If there had been one, raise an exception too"

    ^self checkError: self errno
!

checkError: errno
    "The error with the C code `errno' has been reported.
     If errno >= 1, raise an exception"

    | errors |
    errno < 1 ifTrue: [ ^false ].
    errors := #(
       'Not super-user'
       'No such file or directory'
       'No such process'
       'Interrupted system call'
       'I/O error'
       'No such device or address'
       'Arg list too long'
       'Exec format error'
       'Bad file number'
       'No children'
       'No more processes'
       'Not enough core'
       'Permission denied'
       'Bad address'
       'Block device required'
       'Mount device busy'
       'File exists'
       'Cross-device link'
       'No such device'
       'Not a directory'
       'Is a directory').

    ^errno > errors size
        ifTrue: [ self error: 'error #', errno printString. true ]
        ifFalse: [ self error: (errors at: errno). true ]
!


remove: fileName
    "Remove the file with the given path name"
    self primRemove: fileName.
    self checkError
!

rename: oldFileName to: newFileName
    "Rename the file with the given path name oldFileName to newFileName"
    self primRename: oldFileName to: newFileName.
    self checkError
!!


!File class methodsFor: 'instance creation'!

name: aName
    "Answer a new file with the given path. The path is not validated until
    some of the fields of the newly created objects are accessed"
    ^self basicNew init: aName
! !


!File class methodsFor: 'testing'!

exists: fileName
    "Answer whether a file with the given name exists"
    ^(File name: fileName) exists
! !


!File class methodsFor: 'reading system defaults'!

image
    "Answer the full path to the image being used."
    ^ImageFileName
! !


!File methodsFor: 'accessing'!

name
    "Answer the name of the file identified by the receiver"
    ^name
!

size
    "Answer the size of the file identified by the receiver"
    ^self stat stSize value
!

lastAccessTime
    "Answer the last access time of the file identified by the receiver"
    ^self getDateAndTime: self stat stAtime value
!

lastChangeTime
    "Answer the last change time of the file identified by the receiver
    (the `last change time' has to do with permissions, ownership and the
    like). On some operating systems, this could actually be the
    file creation time."
    ^self getDateAndTime: self stat stCtime value
!

creationTime
    "Answer the creation time of the file identified by the receiver.
    On some operating systems, this could actually be the last change time
    (the `last change time' has to do with permissions, ownership and the
    like)."
    ^self getDateAndTime: self stat stCtime value
!

lastModifyTime
    "Answer the last modify time of the file identified by the receiver
    (the `last modify time' has to do with the actual file contents)."
    ^self getDateAndTime: self stat stMtime value
!

refresh
    "Refresh the statistics for the receiver"
    stat isNil ifTrue: [
        stat := CStatStruct new.
        stat addToBeFinalized
    ].
    self statOn: name into: stat.
    self class checkError
! !



!File methodsFor: 'testing'!

exists
    "Answer whether a file with the name contained in the receiver does exist."
    stat isNil ifTrue: [
        stat := CStatStruct new.
        stat addToBeFinalized.
    ].
    self statOn: name into: stat.
    ^self class errno == 0
!

isFile
    "Answer whether a file with the name contained in the receiver does exist
    and does not identify a directory."
    ^self exists and: [ self isDirectory not ]
!

isDirectory
    "Answer whether a file with the name contained in the receiver does exist
    and identifies a directory."
    | dir errno |
    self exists ifFalse: [ ^false ].
    dir := self openDir: name.
    errno := self class errno.
    (errno = 0) ifTrue: [
        self closeDir: dir.
        ^true
    ].
    errno = 20 ifTrue: [ ^false ].
    self class checkError: errno
! !


!File methodsFor: 'file operations'!

contents
    "Open a read-only FileStream on the receiver, read its contents,
    close the stream and answer the contents"
    | stream contents |
    stream := self readStream.
    contents := stream contents.
    stream close.
    ^contents
!

open: mode
    "Open the receiver in the given mode (as answered by FileStream's
    class constant methods)"
    ^FileStream open: self name mode: mode
!

readStream
    "Open a read-only FileStream on the receiver"
    ^FileStream open: self name mode: FileStream read
!

writeStream
    "Open a write-only FileStream on the receiver"
    ^FileStream open: self name mode: FileStream write
!

remove
    "Remove the file identified by the receiver"
    self class remove: name.
!

renameTo: newName
    "Remove the file identified by the receiver"
    self class rename: name to: newName.
    name := newName
! !


!File methodsFor: 'private'!

getDateAndTime: time
    "Private - Convert a time expressed in seconds from 1/1/2000 to
     an array of two Smalltalk Date and Time objects"

    ^Array
        with: (Epoch addDays: time // 86400 - 1)
        with: (Time fromSeconds: time \\ 86400)
!

stat
    "Private - Answer the receiver's statistics' C struct"
    stat isNil ifTrue: [ self refresh ].
    ^stat
!

init: aName
    "Private - Initialize the receiver's instance variables"
    name := aName
! !
    

File initialize!