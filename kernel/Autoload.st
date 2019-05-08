"======================================================================
|
|   File autoloading mechanism
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

nil subclass: #Autoload
       instanceVariableNames: 'className fileName'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Examples-Useful tools'
!

Autoload comment:
'I am not a part of the normal Smalltalk kernel class system.  I provide the
ability to do late-loading or "on demand loading" of class definitions.
Through me, you can define any class to be loaded when any message is sent to
the class itself (such as to create an instance).' !


!Autoload class methodsFor: 'instance creation'!

class: classNameString from: fileNameString
    "Make Smalltalk automatically load the class named classNameString
     from fileNameString when needed"

    ^Autoload new autoload__InitClass: classNameString 
		  initFile: fileNameString
!!


!Autoload methodsFor: 'accessing'!

doesNotUnderstand: aMessage
    | s |
    Smalltalk removeKey: className.
    FileStream fileIn: fileName.
    ^aMessage reinvokeFor: (Smalltalk at: className
				      ifAbsent: [ ^Autoload error: 
'Autoloaded file should have defined class "', className, '" but didn''t' ])
!!



!Autoload methodsFor: 'private'!

autoload__InitClass: aClassName initFile: aFileName
    className := aClassName asSymbol.
    Smalltalk at: className put: self.
    fileName := aFileName.
!!
