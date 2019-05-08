"======================================================================
|
|   ReadWriteStream Method Definitions
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


WriteStream subclass: #ReadWriteStream
	    instanceVariableNames: ''
	    classVariableNames: ''
	    poolDictionaries: ''
	    category: 'Streams-Collections'
!

ReadWriteStream comment: 
'I am the class of streams that may be read and written from simultaneously.
In some sense, I am the best of both ReadStream and WriteStream.' !



!ReadWriteStream class methodsFor: 'instance creation'!

on: aCollection
    "Answer a new stream working on aCollection from its end. You can
     use #skip: or #position: to move back."
    | stream |
    stream := (super on: aCollection) access: 3.
    stream useWholeStream.
    ^stream
!

onEmpty: aCollection
    "Extension: do not require the use of the entire stream -- pretend
     that the stream is empty, regardless of the size of the collection
     that it operates over.
     Answer a new stream working on aCollection from its start."
    | stream |
    stream := (super on: aCollection) access: 3.
    ^stream
!!


!ReadWriteStream methodsFor: 'positioning'!

position: anInteger
    "Unlike WriteStreams, ReadWriteStreams don't truncate the stream"
    self basicPosition: anInteger
!

skip: anInteger
    "Unlike WriteStreams, ReadWriteStreams don't truncate the stream"
    self basicPosition: ((ptr + anInteger max: 1) min: endPtr).
!!


!ReadWriteStream methodsFor: 'private'!

useWholeStream
    "Private - Makes sure that the contents message to this object
     returns the whole contents, not just that which has been written
     so far."
    endPtr := collection size
!!
     
