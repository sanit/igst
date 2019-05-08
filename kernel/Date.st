"======================================================================
|
|   Date Method Definitions
|
|   $Revision: 1.6.2$
|   $Date: 1999/08/31 11:23:18$
|   $Author: pb$
|
 ======================================================================"


"======================================================================
|
| Copyright 1990, 91, 92, 94, 95, 99 Free Software Foundation, Inc.
| Written by Steve Byrne, Paolo Bonzini and Jeff Rosenwald.
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


Magnitude subclass: #Date
	  instanceVariableNames: 'days day month year'
	  classVariableNames: 'DayNameDict MonthNameDict'
	  poolDictionaries: ''
	  category: 'Language-Data types'!

Date comment: 
'My instances represent dates.  My base date is defined to be Jan 1, 1901.
I provide methods for instance creation (including via "symbolic" dates, 
such as "Date newDay: 14 month: #Feb year: 1990".

PLEASE BE WARNED -- use this class only for dates after 1582 AD; that''s the
beginning of the epoch. Dates before 1582 will not be correctly printed. In
addition, since ten days were lost from October 5 through October 15,
operations between a Gregorian date (after 15-Oct-1582) and a Julian date
(before 5-Oct-1582) will give incorrect results; or, 4-Oct-1582 + 2 days will
yield 6-Oct-1582 (a non-existent day!), not 16-Oct-1582.

In fact, if you pass a year < 1582 to a method like #newDay:month:year:
it will assume that it is a two-digit year (e.g. 90=1990, 1000=2900). The only
way to create Julian calendar dates is with the #fromDays: instance creation
method.' !


!Date class methodsFor: 'basic'!

initialize
    "Initialize the receiver"

    "28 = 7 days*2 keys each day, multiplied by 2 to make hashing effective.
     48 = 12 months*2 keys each month, multiplied by 2 for the same reason."
    DayNameDict := Dictionary new: 28.
    MonthNameDict := Dictionary new: 48.
    self initDayNameDict.
    self initMonthNameDict
!

initDayNameDict
    "Initialize the DayNameDict to the names of the days"
    | dayNames |
    dayNames := #(
    	    	 (monday mon)	"1"
    	    	 (tuesday tue)	"2"
    	    	 (wednesday wed) "3"
    	    	 (thursday thu) "4"
    	    	 (friday fri)	"5"
    	    	 (saturday sat) "6"
    	    	 (sunday sun)	"7"
    	    	).
    dayNames doWithIndex: [ :names :dayIndex |
        names do:
	    [ :name | DayNameDict at: name put: dayIndex ]
    ].
!

initMonthNameDict
    "Initialize the MonthNameDict to the names of the months"
    | monthNames |
    monthNames := #(
    	    	 (january   jan)	"1"
    	    	 (february  feb)	"2"
    	    	 (march	    mar)    	"3"
    	    	 (april	    apr)    	"4"
    	    	 (may)	    	    	"5"
    	    	 (june	    jun)    	"6"
    	    	 (july	    jul)	"7"
    	    	 (august    aug)	"8"
    	    	 (september sep)	"9"
    	    	 (october   oct)	"10"
    	    	 (november  nov)	"11"
    	    	 (december  dec)	"12"
    	    	).
    monthNames doWithIndex: [ :names :dayIndex |
        names do:
	    [ :name | MonthNameDict at: name put: dayIndex ]
    ].
!


dayOfWeek: dayName
    "Answer the index of the day of week corresponding to the given name"
    ^DayNameDict at: dayName asLowercase asSymbol
!

nameOfDay: dayIndex
    "Answer the name of the day of week corresponding to the given index"
    ^#(Monday Tuesday Wednesday Thursday Friday Saturday Sunday) at: dayIndex
!

indexOfMonth: monthName
    "Answer the index of the month corresponding to the given name"
    ^MonthNameDict
        at: monthName asLowercase asSymbol
!

shortNameOfMonth: monthIndex
    "Answer the name of the month corresponding to the given index"
    ^#(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec) at: monthIndex
!

nameOfMonth: monthIndex
    "Answer the name of the month corresponding to the given index"
    ^#(January February  March
       April   May       June
       July    August    September
       October November  December) at: monthIndex
!

daysInMonth: monthName forYear: yearInteger
    "Answer the number of days in the given (named) month for the given year"
    | monthIndex |
    monthIndex := self indexOfMonth: monthName.
    ^self daysInMonthIndex: monthIndex forYear: yearInteger
!

daysInYear: yearInteger
    "Answer the number of days in the given year"
    | i |
    i := self fixYear: yearInteger.
    ^((i + 4800) * 1461 // 4) - ((i + 4900) // 100 * 3 // 4)
    -((i + 4799) * 1461 // 4) + ((i + 4899) // 100 * 3 // 4)
!

dateAndTimeNow
    "Answer an array containing the current date and time"
    ^Array with: (Date today) with: (Time now)
!!



!Date class methodsFor: 'instance creation'!

today
    "Answer a Date denoting the current date"
    | days |
    days := Time primSecondClock // (24 * 60 * 60).
    ^self new setDays: days + 36159 "(99 * 365 + 24)"
!

fromSeconds: time
    "Answer a Date denoting the date time seconds past Jan 1st, 2000"
    | days |
    days := time // (24 * 60 * 60).
    ^self new setDays: days + 36159 "(99 * 365 + 24)"
!


fromJulian: jd
    "Answer a Date denoting the jd-th day in the astronomical Julian calendar."
    ^self new setDays: jd - 2415386
!

fromDays: dayCount
    "Answer a Date denoting dayCount days past 1/1/1901"
    ^self new setDays: dayCount
!

newDay: dayCount year: yearInteger
    "Answer a Date denoting the dayCount day of the yearInteger year"
    ^self new setDays:
        dayCount + (self yearAsDays: yearInteger)
!

newDay: day monthIndex: monthIndex year: yearInteger
    "Answer a Date denoting the dayCount day of the given (as a number)
     month and year"
    ^self new
        setDay: day
        monthIndex: monthIndex
        year: yearInteger
!

newDay: day month: monthName year: yearInteger
    "Answer a Date denoting the dayCount day of the given (named)
     month and year"
    ^self new
        setDay: day
        monthIndex: (self indexOfMonth: monthName)
        year: yearInteger
!!



!Date class methodsFor: 'private methods'!

yearAsDays: yearInteger
    "Private - Returns the number of days between Jan 1, 1901
     and Jan 0th of the given year yearInteger"
    | i |
    i := self fixYear: yearInteger.
        
    "Calculate Julian day via Fliegal & Van Flandern method
     -- ACM Algorithm 289. I have to subtract 2415387 to correct for Smalltalk
     epoch, and I merged that constant in the one (32074) in their formula."
    ^((i + 4799) * 1461 // 4)
         - ((i + 4899) // 100 * 3 // 4)
         - 2447125!

fixYear: yy
    "Private - Convert 2-digits year to 4-digits if needed"
    ^(yy < 1583)
        ifTrue: [ yy + 1900 ]
        ifFalse: [ yy].
!

daysUntilMonth: monthIndex year: yearInteger
    "Private - Answer the number of days between Jan 1, 1901, and the
     hypotetical 0th day in the given month, for the given year"
    | i j |
    j := (monthIndex - 14) quo: 12.
    i := j + (self fixYear: yearInteger).
        
    "Calculate Julian day via Fliegal & Van Flandern method
     -- ACM Algorithm 289. I have to subtract 2415386 to correct for Smalltalk
     epoch, and I merged that constant in the one (32074) in their formula."
    ^((i + 4800) * 1461 // 4)
         + ((monthIndex - 2 - (j * 12)) * 367 // 12)
         - ((i + 4900) // 100 * 3 // 4)
         - 2447461
!


daysInMonthIndex: monthIndex forYear: yearInteger
    "Private - Answer the number of days in the given (indexed) month,
     for the given year"

    monthIndex = 2
    	ifTrue: [ ^(self daysInYear: yearInteger) - 337 ].

    ^#(31 x 31		"Jan Feb Mar"
    	    30 31 30		"Apr May Jun"
	    31 31 30		"Jul Aug Sep"
	    31 30 31		"Oct Nov Dec"
	    ) at: monthIndex
!!


!Date methodsFor: 'testing'!

< aDate
    "Answer whether the receiver indicates a date preceding aDate"
    ^aDate days < self days
!

= aDate
    "Answer whether the receiver indicates the same date as aDate"
    ^(aDate class == self class) and: [ aDate days = self days ]
!

hash
    "Answer an hash value for the receievr"
    ^self days
!!


!Date methodsFor: 'basic'!

addDays: dayCount
    "Answer a new Date pointing dayCount past the receiver"
    ^Date fromDays: self days + dayCount
!

subtractDays: dayCount
    "Answer a new Date pointing dayCount before the receiver"
    ^Date fromDays: self days - dayCount
!

subtractDate: aDate
    "Answer the number of days between aDate and the receiver (negative
     if the receiver is before aDate)"
    ^self days - aDate days
!!


!Date methodsFor: 'date computations'!

asSeconds
    "Answer the date as the number of seconds from 1/1/1901."
    ^self days * 86400
!

dayOfWeek
    "Answer the day of week of the receiver. 1 = Monday, 7 = Sunday"
    ^(self days + 1) \\ 7 + 1
!

dayName
    "Answer the day of week of the receiver as a Symbol"
    ^Date nameOfDay: ((self days + 1) \\ 7 + 1)
!

day
    "Answer the day represented by the receiver"
    ^day
!

dayOfYear
    "Answer the days passed since 31/12 of last year; e.g. New Year's Day is 1"
    ^self days - (Date yearAsDays: self year)
!

daysFromBaseDay
    "Answer the days passed since 1/1/1901"
    ^days
!

daysInMonth
    "Answer the days in the month represented by the receiver"
    ^Date daysInMonthIndex: month forYear: year
!

daysInYear
    "Answer the days in the year represented by the receiver"
    ^Date daysInYear: self year
!

daysLeftInMonth
    "Answer the days to the end of the month represented by the receiver"
    ^(Date daysInMonthIndex: month forYear: year) - day
!

daysLeftInYear
    "Answer the days to the end of the year represented by the receiver"
    ^(Date yearAsDays: self year + 1) - self days
!

firstDayOfMonth
    "Answer a Date representing the first day of the month represented by the
     receiver"
    ^self subtractDays: self dayOfMonth - 1
!

lastDayOfMonth
    "Answer a Date representing the last day of the month represented by the
     receiver"
    ^self addDays: self daysLeftInMonth
!

dayOfMonth

    ^day
!

month
    "Answer the month represented by the receiver"
    ^month
!

monthName
    "Answer the name of the month represented by the receiver"
    ^Date nameOfMonth: self monthIndex
!

year
    "Answer the year represented by the receiver"
    ^year
!!


!Date methodsFor: 'printing'!

printOn: aStream
    "Print a representation for the receiver on aStream"
    aStream
        print: self day;
        nextPut: $-;
        nextPutAll: (Date shortNameOfMonth: self month);
        nextPut: $-;
        print: self year
!!



!Date methodsFor: 'storing'!

storeOn: aStream
    "Store on aStream Smalltalk code compiling to the receiver"

    aStream
        nextPut: $(;
        nextPutAll: self classNameString;
        nextPutAll: ' newDay: ';
        store: self day;
        nextPutAll: ' monthIndex: ';
        store: self month;
        nextPutAll: ' year: ';
        store: self year;
        nextPut: $)
!!



!Date methodsFor: 'private methods'!

days
    "Private - Same as daysFromBaseDay"
    ^days
!

setDay: dayOfMonth monthIndex: monthIndex year: yearInteger
    "Private - Set the receiver to the given date parts"

    days := dayOfMonth + (Date daysUntilMonth: monthIndex year: yearInteger).
    dayOfMonth < 1
        ifTrue: [ ^self setDays: days ].
    dayOfMonth > (Date daysInMonthIndex: monthIndex forYear: yearInteger)
        ifTrue: [ ^self setDays: days ].

    day := dayOfMonth.
    month := monthIndex.
    year := yearInteger.
!

setDays: dayCount
    "Private - Compute the date parts from the given dayCount and initialize
     the receiver"

    "Fliegal and Van Flandern's methods for computing y/m/d from Julian.
     The original algorithm starts by summing 68569 to the Julian day --
     I sum 2483955 to adjust for smalltalk epoch."

    | julian n |
    days := dayCount.
    julian := days + 2483955.
    n := (4 * julian) // 146097.
    julian := julian - ((146097 * n + 3) // 4).
    year := (4000 * (julian + 1)) // 1461001.
    julian := julian - ((1461 * year) // 4) + 31.
    month := (80 * julian) // 2447.
    day := julian - ((2447 * month) // 80).
    julian := month // 11.
    month := month + 2 - (12 * julian).
    year := (100 * (n - 49)) + year + julian.
!!
