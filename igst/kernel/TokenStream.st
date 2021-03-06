"======================================================================
|
|   Token stream Method Definitions
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


Stream subclass: #TokenStream
       instanceVariableNames: 'charStream'
       classVariableNames: ''
       poolDictionaries:''
       category: 'Streams-Collections'
!

TokenStream comment:
'I am not a typical part of the Smalltalk kernel class hierarchy.
I operate on a stream of characters and return distinct whitespace-delimited
groups of characters; I am used to parse the parameters of class-creation
methods.

Basically, I parse off whitespace separated tokens as substrings and
return them (next).  If the entire contents of the string are requested,
I return them as an Array containing the individual strings.'!



!TokenStream class methodsFor: 'instance creation'!

on: aString
    "Answer a TokenStream working on aString"
    ^self onStream: (ReadStream on: aString)
!

onStream: aStream
    "Answer a TokenStream working on the collection on which aStream
    is in turn streaming."
    ^self new setStream: aStream
!!



!TokenStream methodsFor: 'basic'!

next
    "Answer a new whitespace-separated token from the input stream"
    | char tokStream |
    self atEnd ifTrue: [ ^nil ]. "has the nice side effect of skipping
    	    	    	    	  leading white space."
    tokStream := WriteStream on: (String new: 1).
    [ char := charStream peek.
      (char notNil) and: [ (char isSeparator) not ] ]
        whileTrue: [ tokStream nextPut: (charStream next) ].
    ^tokStream contents
!

atEnd
    "Answer whether the input stream has no more tokens."
    | char |
    [ char := charStream peek.
      char isNil ] whileFalse:
    	[ (char isSeparator) ifFalse: [ ^false ].
	 charStream next ].
    ^true
!!



!TokenStream methodsFor: 'write methods'!

nextPut: anObject
    self shouldNotImplement
!!



!TokenStream methodsFor: 'private'!

setStream: aStream
    charStream := aStream.
!!
