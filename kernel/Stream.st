"======================================================================
|
|   Stream Method Definitions
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


Object subclass: #Stream
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Streams'
!

Stream comment: 
'I am an abstract class that provides interruptable sequential access to
objects.  I can return successive objects from a source, or accept
successive objects and store them sequentially on a sink.  I provide
some simple iteration over the contents of one of my instances, and 
provide for writing collections sequentially.' !


!Stream methodsFor: 'accessing-reading'!

next
    "Return the next object in the receiver"
    self subclassResponsibility
!

next: anInteger
    "Return the next anInteger objects in the receiver"
    | stream |
    stream := WriteStream on: (self species new: anInteger).
    anInteger timesRepeat: [ stream nextPut: self next ].
    ^stream contents
!

nextMatchFor: anObject
    "Answer whether the next object is equal to anObject. Even if it does
     not, anObject is lost"
    ^anObject = self next
!

contents
    "Answer the whole contents of the receiver, from the next object to
     the last"
    | stream |

    stream := WriteStream on: (self species new: 10).
    self do: [ :each | stream nextPut: each ].
    ^stream contents
!!



!Stream methodsFor: 'accessing-writing'!

nextPut: anObject
    "Write anObject to the receiver"
    self subclassResponsibility
!

nextPutAll: aCollection
    "Write all the objects in aCollection to the receiver"
    aCollection do: [ :element | self nextPut: element ].
    ^aCollection
!

next: anInteger put: anObject
    "Write anInteger copies of anObject to the receiver"
    anInteger timesRepeat: [ self nextPut: anObject ].
    ^anObject
! !



!Stream methodsFor: 'testing'!

atEnd
    "Answer whether the stream has got to an end"
    self subclassResponsibility
! !



!Stream methodsFor: 'enumerating'!

do: aBlock
    "Evaluate aBlock once for every object in the receiver"
    [self atEnd] whileFalse:
    	[aBlock value: self next ]

! !



!Stream methodsFor: 'basic'!

species
    ^Array
!!


!Stream methodsFor: 'character writing'!

cr
    "Store a cr on the receiver"
    self nextPut: Character cr
!

nl
    "Store a new line on the receiver"
    self nextPut: Character nl
!

crTab
    "Store a cr and a tab on the receiver"
    self cr.
    self tab
!

nlTab
    "Store a new line and a tab on the receiver"
    self nl.
    self tab
!

space
    "Store a space on the receiver"
    self nextPut: Character space
!

tab
    "Store a tab on the receiver"
    self nextPut: Character tab
!!


!Stream methodsFor: 'providing consistent protocols'!

close
    "Do nothing. This is provided for consistency with file streams"
!!


!Stream methodsFor: 'printing'!

print: anObject
    "Print anObject on the receiver by sending printOn: to anObject. This
     method is provided so that you can use cascading and obtain
     better-looking code"
    anObject printOn: self
! !



!Stream methodsFor: 'storing'!

store: anObject
    "Print Smalltalk code compiling to anObject on the receiver, by sending
     storeOn: to anObject. This method is provided so that you can use
     cascading and obtain better-looking code"
    anObject storeOn: self
! !


!Stream methodsFor: 'filing out'!

fileOut: aClass
    "File out aClass on the receiver. If aClass is not a metaclass, file out
     class and instance methods; if aClass is a metaclass, file out only the
     class methods"

    aClass fileOutOn: self
!!
