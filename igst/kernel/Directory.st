"======================================================================
|
|   Directory Method Definitions
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


File subclass: #Directory
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Streams-Files'
!    


Directory class
	 defineCFunc: 'rmdir'
	 withSelectorArgs: 'primRemove: fileName'
	 returning: #void
	 args: #(string).

Directory class
	 defineCFunc: 'chdir'
	 withSelectorArgs: 'primWorking: dirName'
	 returning: #void
	 args: #(string).

Directory class
	 defineCFunc: 'getCurDirName'
	 withSelectorArgs: 'working'
	 returning: #stringOut
	 args: #().

Directory
	 defineCFunc: 'extractDirentName'
	 withSelectorArgs: 'extractDirentName: dirent'
	 returning: #string
	 args: #(cObject).

Directory
	 defineCFunc: 'readdir'
	 withSelectorArgs: 'readDir: dirObject'
	 returning: #cObject
	 args: #(cObject).

Directory
	 defineCFunc: 'rewinddir'
	 withSelectorArgs: 'rewindDir: dirObject'
	 returning: #void
	 args: #(cObject)!


!Directory class methodsFor: 'reading system defaults'!

home
    "Answer the path to the user's home directory"
    ^Smalltalk getenv: 'HOME'
!

image
    "Answer the path to GNU Smalltalk's image file"
    ^ImageFilePath
!

kernel
    "Answer the path to the GNU Smalltalk kernel's Smalltalk source files"
    ^KernelFilePath
!!


!Directory class methodsFor: 'file operations'!

working: dirName
    "Change the current working directory to dirName."
    self primWorking: dirName.
    self checkError
!!


!Directory methodsFor: 'accessing'!

contents
    "Answer an Array with the names of the files in the directory
    represented by the receiver."
    | ws |
    ws := WriteStream on: (Array new: 50).
    self do: [ :each | ws nextPut: each ].
    ^ws contents
!

do: aBlock
    "Evaluate aBlock once for each file in the directory represented by the
    receiver, passing its name."
    | dir entry |
    dir := self openDir: self name.
    self class checkError.

    [ entry := self readDir: dir.
      self class checkError.
      entry notNil ] whileTrue:
	  [ aBlock value: (self extractDirentName: entry) ].
    self closeDir: dir.
! !


!Directory methodsFor: 'private'!

init: aString
    "Private - Initialize the receiver"
    name := aString
! !


"

| d file |
     d := Directory name: '..'.
     d do: [ :name | file := File name: '../', name.
                       Transcript nextPutAll: file name ;
                            nextPutAll: ' size: ';
                            print: file stat stSize value;
                            nl.
	            
            ].
!
     
"
