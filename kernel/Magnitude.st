"======================================================================
|
|   Magnitude Method Definitions
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


Object subclass: #Magnitude
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Language-Data types'
!

Magnitude comment: 
'I am an abstract class.  My objects represent things that are discrete and 
map to a number line.  My instances can be compared with < and >.' !


!Magnitude methodsFor: 'basic'!

"Relational operators.  '==', '~=', '~~' are inherited from Object"

= aMagnitude
    "Answer whether the receiver is equal to aMagnitude"
    self subclassResponsibility
!

< aMagnitude
    "Answer whether the receiver is less than aMagnitude"
    self subclassResponsibility
!

> aMagnitude
    "Answer whether the receiver is greater than aMagnitude"
    ^aMagnitude < self
!

<= aMagnitude
    "Answer whether the receiver is less than or equal to aMagnitude"
    ^(aMagnitude < self) not
!

>= aMagnitude
    "Answer whether the receiver is greater than or equal to aMagnitude"
    ^(self < aMagnitude) not
!!



!Magnitude methodsFor: 'misc methods'!

between: min and: max
    "Returns true if object is inclusively between min and max."
    ^(self >= min) and: [ self <= max ]
!

min: aMagnitude
    "Returns the least object between the receiver and aMagnitude"
    ^self < aMagnitude ifTrue: [ self ]
    	    	      ifFalse: [ aMagnitude ]
!

max: aMagnitude
    "Returns the greatest object between the receiver and aMagnitude"
    ^self > aMagnitude ifTrue: [ self ]
    	    	      ifFalse: [ aMagnitude ]
!!

