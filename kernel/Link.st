"======================================================================
|
|   Link Method Definitions
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


Object subclass: #Link
       instanceVariableNames: 'nextLink'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Collections-Sequenceable'
!

Link comment: 
'I represent simple linked lists.  Generally, I am not used by myself, but
rather a subclass adds other instance variables that hold the information
for each node, and I hold the glue that keeps them together.' !


!Link class methodsFor: 'instance creation'!

nextLink: aLink
    "Create an instance with the given next link"
    ^self new nextLink: aLink

!!



!Link methodsFor: 'basic'!

nextLink
    "Answer the next item in the list"
    ^nextLink
!

nextLink: aLink
    "Set the next item in the list"
    nextLink := aLink.
    ^aLink
!!

