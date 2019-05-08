"======================================================================
|
|   FileSegment Method Definitions
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


Object subclass: #FileSegment
       instanceVariableNames: 'fileName startPos length'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Implementation'
!

FileSegment comment: 
'My instances represent sections of files.  I am primarily used by the
compiler to record source code locations.  I am not a part of the normal
Smalltalk-80 kernel; I am specific to the GNU Smalltalk implementation.' !



!FileSegment class methodsFor: 'basic'!

on: aFile startingAt: startPos for: lengthInteger
    "Create a new FileSegment referring to the contents of the given file,
    from the startPos-th byte and for lengthInteger bytes"
    ^self new setFile: aFile start: startPos length: lengthInteger
!!



!FileSegment methodsFor: 'basic'!

asString
    "Answer a String containing the required segment of the file"
    | str file |
    str := String new: length.
    file := FileStream open: fileName mode: FileStream read.
    file position: startPos.
    1 to: length do: [ :i | str at: i put: (file next) ].
    file close.
    ^str
!

fileName
    "Answer the name of the file containing the segment"
    ^fileName
!

filePos
    "Answer the position in the file where the segment starts"
    ^startPos
!!


!FileSegment methodsFor: 'equality'!

= aFileSegment
    "Answer whether the receiver and aFileSegment are equal."

    self class == aFileSegment class ifFalse: [ ^false ].
    self == aFileSegment ifTrue: [ ^true ].

    ^fileName = aFileSegment fileName and: [
        startPos = aFileSegment filePos and: [
            length = aFileSegment length ] ]
!

hash
    "Answer an hash value for the receiver."

    ^fileName hash + startPos + length
!!


!FileSegment methodsFor: 'private'!

length
    ^length
!

setFile: aFileName start: startingPos length: lengthInteger
    fileName := aFileName.
    startPos := startingPos.
    length := lengthInteger
!!
