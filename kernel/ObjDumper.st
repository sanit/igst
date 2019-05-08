"======================================================================
|
|   ObjectDumper Method Definitions
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


Stream subclass: #ObjectDumper
    instanceVariableNames: 'toObjects fromObjects stream'
    classVariableNames: 'SpecialCaseDump SpecialCaseLoad Proxies'
    poolDictionaries: ''
    category: 'Streams-Files'!

Object subclass: #DumperProxy
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Streams-Files'!

Object subclass: #NullProxy
    instanceVariableNames: 'object'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Streams-Files'!

ObjectDumper comment: 'I''m not part of a normal Smalltalk system, but most
Smalltalks provide a similar feature: that is, support for storing objects
in a binary format; there are many advantages in using me instead of #storeOn:
and the Smalltalk compiler
   - My instances store data in a very compact format
   - Loading is much faster when compared with compiling the Smalltalk code
     prepared by #storeOn:
   - Almost all Smalltalks don''t allow programmers to ship the compiler as
     part of the run-time image
   - My instances support circular references between objects, while #storeOn:
     supports it only if you know of such references at design time and you
     override #storeOn: to deal with them'.

DumperProxy comment: 'I am an helper class for ObjectDumper. When an
object cannot be saved in the standard way, you can register a subclass of me
to provide special means to save that object.'.

NullProxy comment: 'I am a proxy that does no special processing on the object
to be saved -- it uses about the same format as a #dump: without a proxy. It
can be used to disable proxies for particular subclasses.'!


!ObjectDumper class methodsFor: 'testing'!

example
    "This is a real torture test: it outputs recursive
     objects, identical objects multiple times,
     classes, metaclasses, integers and characters!"

    | f a d |
    Transcript nextPutAll: 'Must print true six times.'; nl.
    f := FileStream open: 'dumptest' mode: FileStream write.
    a := Array new: 1. a at: 1 put: a.
    (ObjectDumper on: f)
        dump: 'asdf';
        dump: #('asdf' 1 2 $a);
        dump: Array;
        dump: 'asdf';
        dump: Array class;
        dump: a.
    f close.

    f := FileStream open: 'dumptest' mode: FileStream read.
    d := ObjectDumper on: f.
    ((a := d load) = 'asdf') printNl.
    (d load = #('asdf' 1 2 $a)) printNl.
    (d load = Array) printNl.
    (d load == a) printNl.
    (d load = Array class) printNl.
    a := d load.
    (a == (a at: 1)) printNl.
    f close
! !


!ObjectDumper class methodsFor: 'establishing proxy classes'!

hasProxyFor: aClass
    "Answer whether a proxy class has been registered for instances
     of aClass."
    Proxies keysDo: [ :any |
        (aClass inheritsFrom: any) ifTrue: [ ^true ]
    ].
    ^false
!

disableProxyFor: aClass
    "Disable proxies for instances of aClass and its descendants"
    self registerProxyClass: NullProxy for: aClass
!

registerProxyClass: aProxyClass for: aClass
    "Register the proxy class aProxyClass - descendent of DumperProxy -
     to be used for instances of aClass and its descendants"
    Proxies at: aClass put: aProxyClass
!

proxyFor: anObject
    "Answer a valid proxy for an object, or nil if none could be found"
    Proxies keysAndValuesDo: [ :any |
        (anObject isKindOf: any key) ifTrue: [ ^any value on: anObject ]
    ].
    ^anObject
! !


!ObjectDumper class methodsFor: 'private - initialization'!

specialCaseIf: aBlock dump: dumpBlock load: loadBlock
    "Private - This method establishes a condition on which a particular
     method must be used to save an object.
     An application should not use this method, since it might cause
     failure to load file that set the special-case blocks differently;
     instead, you should use ObjectDumper's higher level proxy feature,
     i.e. its #registerProxyClass:for: method - which builds on the
     low-level feature enabled by this method but without its inherent
     problems."

    SpecialCaseDump addLast: aBlock -> dumpBlock.
    SpecialCaseLoad addLast: loadBlock
!

initialize
    "Initialize the ObjectDumper class"
    Proxies := IdentityDictionary new.
    SpecialCaseDump := OrderedCollection new.
    SpecialCaseLoad := OrderedCollection new.

    self
        specialCaseIf: [ :object | object isNil ]
        dump: [ :client :object | ]
        load: [ :client | nil ];

        specialCaseIf: [ :object | object == true ]
        dump: [ :client :object | ]
        load: [ :client | true ];

        specialCaseIf: [ :object | object == false ]
        dump: [ :client :object | ]
        load: [ :client | false ];

        specialCaseIf: [ :object | self isSmallInteger: object ]
        dump: [ :client :object | client stream nextPutLong: object ]
        load: [ :client | client stream nextLong ];

        specialCaseIf: [ :object | object isCharacter ]
        dump: [ :client :object | client stream nextPut: object ]
        load: [ :client | client stream next ];

        specialCaseIf: [ :object | object isClass | object isMetaclass ]
        dump: [ :client :object | client storeClass: object ]
        load: [ :client | client loadClass ];

        specialCaseIf: [ :object | object isSymbol ]
        dump: [ :client :object | client stream nextPutAll: object; nextPutByte: 0 ]
        load: [ :client | client nextAsciiz asSymbol ];

        specialCaseIf: [ :object | object == Processor ]
        dump: [ :client :object | ]
        load: [ :client | Processor ];

        specialCaseIf: [ :object | object class == NullProxy ]
        dump: [ :client :object | client primDump: object object ]
        load: [ :client | client primLoad: client stream nextLong ];

        specialCaseIf: [ :object | self hasProxyFor: object class ]
        dump: [ :client :object | client dump: (self proxyFor: object) ]
        load: [ :client | client load object ].
!!


!ObjectDumper class methodsFor: 'private - portability'!

isSmallInteger: anObject
    "Private - Answer true if the receiver is a small integer (<2^30)"

    ^anObject isSmallInteger        "GNU-Smalltalk, Squeak"
    "^anObject class == SmallInteger  Most other Smalltalks"
!!


!ObjectDumper class methodsFor: 'instance creation'!

on: aFileStream
    "Answer an ObjectDumper working on aFileStream."

    ^self basicNew initializeStream: aFileStream
!

new
    self shouldNotImplement
!!


!ObjectDumper class methodsFor: 'shortcuts'!

dump: anObject to: aFileStream
    "Dump anObject to aFileStream. Answer anObject"

    ^(self on: aFileStream) dump: anObject
!

loadFrom: aFileStream
    "Load an object from aFileStream and answer it"

    ^(self on: aFileStream) load
!!


!ObjectDumper methodsFor: 'stream interface'!

atEnd
    "Answer whether the underlying stream is at EOF"
    ^stream atEnd
!

next
    "Load an object from the underlying stream"
    ^self load
!

nextPut: anObject
    "Store an object on the underlying stream"
    self dump: anObject
! !


!ObjectDumper methodsFor: 'loading/dumping objects'!

dump: anObject
    "Dump anObject on the stream associated with the receiver. Answer
     anObject"

    (self specialCaseDump: anObject) ifFalse: [ self primDump: anObject ].
    ^anObject
!

load
    "Load an object from the stream associated with the receiver and answer
     it"

    | index |

    "Special-case metaclasses and other objects"
    index := stream nextLong.
    ^index < 0
        ifTrue: [ self specialCaseLoad: index ]
        ifFalse: [ self primLoad: index ]
!!


!ObjectDumper methodsFor: 'accessing'!

stream
    ^stream
!!


!ObjectDumper methodsFor: 'private - handling maps'!

lookup: anObject
    "Private - Lookup anObject into the fromObjects map. If it is present, put
     its index in stream and answer true; else put four zeros in the stream
     and answer false"

    | index |

    index := fromObjects at: anObject ifAbsent: [
        stream nextPutLong: 0.
        ^false
    ].

    stream nextPutLong: index.
    ^true
!

lookupIndex: index ifPresent: aBlock
    "Private - If index is a valid index into the toObjects map, evaluate
     aBlock passing the object associated to it"

    index > 0
        ifTrue: [ aBlock value: (toObjects at: index) ]
!

register: anObject
    "Private - Register the anObject in the fromObjects and toObjects maps.
     Assumes that anObject is absent in these maps"

    "(fromObject includesKey: anObject) ifTrue: [
        ^self error: 'Huh?!? Assertion failed' ].   "

    toObjects addLast: anObject.
    fromObjects at: anObject put: toObjects size
!!


!ObjectDumper methodsFor: 'private'!

initializeStream: aStream
    "Private - Initialize the receiver's instance variables"

    stream := aStream.
    toObjects := OrderedCollection new.
    fromObjects := IdentityDictionary new.
    ^self
!

loadClass
    "Private - Load the next object's class from stream"

    | isMeta class |

    isMeta := stream nextByte = 0.

    self lookupIndex: stream nextLong ifPresent: [ :loadedClass |
        loadedClass isClass ifFalse: [ ^self error: 'Bad class' ].
        ^isMeta
             ifTrue: [ loadedClass class ]
             ifFalse: [ loadedClass ]
    ].

    Smalltalk at: self nextAsciiz asGlobalKey ifPresent: [ :loadedClass |
        loadedClass isClass ifFalse: [ ^self error: 'Bad class' ].

        "We don't register metaclasses; instead we register their instance
         (the class) and use a byte to distinguish between the two cases."

        ^isMeta
            ifTrue: [ loadedClass class ]
            ifFalse: [ self register: loadedClass. loadedClass ]
    ].

    ^self error: 'Bad class name'
!

load: anObject sending: selector to: receiver
    "Private - Fill anObject's indexed instance variables from the stream.
     To get a variable, send selector to receiver. Answer anObject"

    1 to: anObject basicSize do: [ :i |
        anObject
            basicAt: i
            put: (receiver perform: selector)
    ].
    ^anObject
!

loadFixedPart: class
    "Private - Load the fixed instance variables of a new instance of class"

    | object |
    object := class isVariable
        ifTrue: [ class basicNew: stream nextLong ]
        ifFalse: [ class basicNew ].

    self register: object.

    1 to: class instSize do: [ :i |
        object instVarAt: i put: self load
    ].
    ^object
!

nextAsciiz
    "Private - Get a Null-terminated string from stream and answer it"

    | ch answer |
    answer := WriteStream on: (String new: 30). "Hopefully large enough"

    [
        ch := stream next. ch asciiValue = 0 ] whileFalse: [
        answer nextPut: ch
    ].
    ^answer contents
!

primDump: anObject
    "Private - Basic code to dump anObject on the stream associated with the
     receiver, without using proxies and the like."

    | class |
    (self lookup: anObject) ifTrue: [ ^anObject ].

    self storeClass: (class := anObject class).
    anObject isClass ifTrue: [ ^anObject ].

    class isVariable ifTrue: [ stream nextPutLong: anObject basicSize ].

    1 to: class instSize do: [ :i |
        self dump: (anObject instVarAt: i)
    ].

    class isVariable ifFalse: [ ^self ].

    class isPointers
        ifTrue: [ ^self store: anObject sending: #dump: to: self ].

    class isBytes
        ifFalse: [ ^self store: anObject sending: #nextPutLong: to: stream ].

    ^anObject isString
        ifTrue: [ self store: anObject sending: #nextPut: to: stream ]
        ifFalse: [ self store: anObject sending: #nextPutByte: to: stream ]
!

primLoad: index
    "Private - Basic code to load an object from the stream associated with the
     receiver, assuming it doesn't use proxies and the like.  The first four
     bytes of the encoding are in index"

    | object class |
    self lookupIndex: index ifPresent: [ :object | ^object ].

    class := self loadClass.
    class isMetaclass ifTrue: [ ^class instanceClass ].

    object := self loadFixedPart: class.

    class isVariable ifFalse: [ ^object ].

    class isPointers
        ifTrue: [ ^self load: object sending: #load to: self ].

    class isBytes
        ifFalse: [ ^self load: object sending: #nextLong to: stream ].

    ^class == String
        ifTrue: [ self load: object sending: #next to: stream ]
        ifFalse: [ self load: object sending: #nextByte to: stream ]
!

specialCaseDump: anObject
    "Private - Store special-cased objects. These include booleans, integers,
     nils, characters, classes and Processor. Answer true if object belongs
     to one of these categories, else do nothing and answer false"

    SpecialCaseDump doWithIndex: [ :each :index |
        ((each key) value: anObject) ifTrue: [
            stream nextPutLong: index negated.
            (each value) value: self value: anObject.
            ^true
        ]
    ].
    ^false
!

specialCaseLoad: index

    "Private - The first 4 bytes in the file were less than 0.
     Load the remaining info about the object and answer it."

    ^index <= SpecialCaseLoad size
        ifTrue: [ (SpecialCaseLoad at: index negated) value: self ]
        ifFalse: [ ^self error: 'error in file or special case not registered' ]
!

storeClass: aClass
    "Private - Store the aClass class in stream. The format is:
        - for a metaclass, a 0 followed by the asciiz name of its instance
        - for a class, a 1 followed by its asciiz name"

    "We don't register metaclasses; instead we register their instance
     (the class) and use a byte to distinguish between the two cases."

    | primaryClass |
    aClass isMetaclass
        ifTrue: [ stream nextPutByte: 0. primaryClass := aClass instanceClass ]
        ifFalse: [ stream nextPutByte: 1. primaryClass := aClass ].

    (self lookup: primaryClass) ifFalse: [

        self register: primaryClass.
        stream
            nextPutAll: primaryClass name;
            nextPutByte: 0
    ]
!

store: anObject sending: selector to: receiver
    "Private - Store anObject's indexed instance variables into the stream.
     To store a variable, send selector to receiver passing it the variable.
     Answer anObject"

    self register: anObject.

    1 to: anObject basicSize do: [ :i |
        receiver
            perform: selector
            with: (anObject basicAt: i)
    ].
    ^anObject
!!


!DumperProxy class methodsFor: 'instance creation'!

on: anObject
    "Answer a proxy to be used to save anObject. IMPORTANT: the newly
    constructed DumperProxy must NOT have anObject in one of its in-
    stance variable, because that would result in an infinite loop!"
    self subclassResponsibility
! !

!DumperProxy methodsFor: 'reconstructing'!

object
    "Reconstruct the object stored in the proxy and answer it"
    self subclassResponsibility
! !


!NullProxy class methodsFor: 'instance creation'!

on: anObject
    "Answer a proxy to be used to save anObject.  IMPORTANT: the newly
    constructed DumperProxy must NOT have anObject in one of its in-
    stance variable, because that would result in an infinite loop!"
    ^self new object: anObject
! !

!NullProxy methodsFor: 'accessing'!

object
    "Reconstruct the object stored in the proxy and answer it"
    ^object
!

object: theObject
    "Private - Set the object to be dumped to theObject.  This is set
     by the #on: class method; note that NullProxies don't conform to
     the rule that proxies must not have their object in an instance
     variable, because they are treated different from all other
     proxy objects in the ObjectDumper."
    object := theObject
! !


ObjectDumper initialize!

