"======================================================================
|
|   Time Method Definitions
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


Magnitude subclass: #Time
	  instanceVariableNames: 'seconds'
	  classVariableNames: 'SecondClockAdjustment SecondClockOnStartup'
	  poolDictionaries: ''
	  category: 'Language-Data types'
!

Time comment: 
'My instances represent times of the day.  I provide methods for instance 
creation, methods that access components (hours, minutes, and seconds) of a 
time value, and a block execution timing facility.' !


!Time class methodsFor: 'basic'!

initialize
     "(99 * 365 + 25) * 86400 secs/day."
     SecondClockAdjustment := 86400 * 36161.
!

onStartup
     "Private - Called on startup"
     SecondClockOnStartup := Time primSecondClock.
!

now
    "Answer a time representing the current time of day"

    "\\ rounds towards -infinity, so it is good for negative numbers too"
    ^self new setSeconds: self primSecondClock \\ 86400
!

new
    "Answer a Time representing midnight"
    ^self basicNew setSeconds: 0
!

fromSeconds: secondCount
    "Answer a Time representing secondCount seconds past midnight"
    ^self new setSeconds: secondCount \\ 86400
!

millisecondClockValue
    "Answer the number of milliseconds since startup"

    ^self primMillisecondClock \\ 1000 +
     (self primSecondClock - SecondClockOnStartup * 1000)
!

millisecondClock
    "Answer the number of milliseconds since startup"

    ^self primMillisecondClock \\ 1000 +
     (self primSecondClock - SecondClockOnStartup * 1000)
!

secondClock
    "Answer the number of seconds since the midnight of 1/1/1901"
    ^self primSecondClock + SecondClockAdjustment
!

millisecondsPerDay
    "Answer the number of milliseconds in a day"
    ^86400000
!

millisecondsToRun: timedBlock
    "Answer the number of milliseconds which timedBlock took to run"
    | startTime|
    startTime := self millisecondClock.
    timedBlock value.
    ^self millisecondClock - startTime
!!


!Time methodsFor: 'accessing'!

asSeconds
    ^seconds
!

hours
    "Answer the number of hours in the receiver"
    ^(seconds // 3600) \\ 24
!

minutes
    "Answer the number of minutes in the receiver"
    ^(seconds // 60) \\ 60
!

seconds
    "Answer the number of seconds in the receiver"
    ^seconds \\ 60
!!



!Time methodsFor: 'comparing'!

= aTime
    "Answer whether the receiver is equal to aTime"
    ^self class == aTime class and: [ seconds = aTime asSeconds ]
!

< aTime
    "Answer whether the receiver is less than aTime"
    ^seconds < aTime asSeconds
!

hash
    "Answer an hash value for the receiver"
    ^seconds
!!





!Time methodsFor: 'arithmetic'!

addTime: timeAmount
    "Answer a new Time that is timeAmount seconds after the receiver"
    ^Time new setSeconds: seconds + timeAmount asSeconds
!

subtractTime: timeAmount
    "Answer a new Time that is timeAmount seconds before the receiver"
    ^Time new setSeconds: seconds - timeAmount asSeconds
!

printOn: aStream
    "Print a representation of the receiver on aStream"
    self hours printOn: aStream.
    aStream nextPut: $:.
    self minutes < 10 ifTrue: [ aStream nextPut: $0 ].
    self minutes printOn: aStream.
    aStream nextPut: $:.
    self seconds < 10 ifTrue: [ aStream nextPut: $0 ].
    self seconds printOn: aStream.
!!



!Time methodsFor: 'private'!

setSeconds: secs
    seconds := secs
!!
