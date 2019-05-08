"======================================================================
|
|   Initialize various Smalltalk classes
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


"This file is loaded after all the kernel Smalltalk classes have been loaded.
It generally performs any initializations that might depend on the full
Smalltalk class hierarchy being defined. After this is file is loaded, 
some 'advanced' class definitions are loaded (CFuncs, Autoload, exception
handling, ...) and then the binary image is saved."

"This enables GC of objects sent to C call-outs. User programs can avoid
 this behavior by using some class protocol which makes referenced objects
 stay alive, or by using (self addDependent: self) and (self release)."

Smalltalk addInit: [ Smalltalk enableGC: true ].
Smalltalk addInit: [ Time onStartup ].

"Example init block follows"
"Smalltalk addInit: [
    stdout
        nextPutAll: 'VM started at ';
        print: Date today;
        nextPut: $ ;
        print: Time now;
        nl
]"!

Fraction initialize!
LargeInteger initialize!
Date initialize!
Time initialize!
Smalltalk initialize!
