"======================================================================
|
|   IdentitySet Method Definitions
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


Set variableSubclass: #IdentitySet
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Collections-Unordered'
!

IdentitySet comment:
'I am the typical set object; I can store any objects uniquely.  I
use the == operator to determine duplication of objects.' !


!IdentitySet methodsFor: 'private methods'!


hashFor: anElement

    "Answer the hash value for anElement"

    ^anElement identityHash
!

is: anElement sameAs: searchedObject

    "Answer whether findIndex: should stop scanning the receiver: anElement has
     been found and findIndex:'s parameter was searchedObject"

    ^anElement == searchedObject
!!
