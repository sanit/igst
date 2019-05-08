"======================================================================
|
|   PackageLoader Method Definitions
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
| This file is part of GNU-Smalltalk.
|
| GNU-Smalltalk is free software; you can redistribute it and/or modify it
| under the terms of the GNU General Public License as published by the Free
| Software Foundation; either version 2, or (at your option) any later version.
| 
| GNU-Smalltalk is distributed in the hope that it will be useful, but WITHOUT
| ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
| FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
| details.
| 
| You should have received a copy of the GNU General Public License along with
| GNU-Smalltalk; see the file COPYING.  If not, write to the Free Software
| Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
|
 ======================================================================"


Object subclass: #PackageLoader
	instanceVariableNames: ''
	classVariableNames: 'PackagePrerequisites FileIns Directories LoadDate'
	poolDictionaries: ''
	category: 'Language-Data types'
!

PackageLoader comment: 
'I am not part of a standard Smalltalk system. I provide methods for loading
packages into a Smalltalk image, correctly handling dependencies.'!


!PackageLoader class methodsFor: 'accessing'!

directoryFor: package
    "Answer a complete path to the given package's file-in"
    | dir |
    self refreshDependencies.
    dir := Directories at: package asString.
    dir size >= 2 ifTrue: [ 
        ((dir at: 1) == $/) | ((dir at: 1) == $\) | (dir includes: $:)
            ifTrue: [ ^dir ]
    ].

    ^Directory kernel, '/', dir
!

fileInsFor: package
    "Answer a Set of Strings containing the filenames of the given package's
     file-ins (relative to the directory answered by #directoryFor:)"
    self refreshDependencies.
    ^FileIns at: package asString
!

prerequisitesFor: package
    "Answer a Set of Strings containing the prerequisites for the given package"
    self refreshDependencies.
    ^PackagePrerequisites at: package asString
!

addPackage: package directory: dir fileIn: fileIns needs: prerequisites
    "Add the given package to the `packages' file, with the given
     directory (if relative, it is relative to the kernel directory),
     fileIns and prerequisites. fileIns and prerequisites should be
     two Collections of Strings.
     Note that none of this fields are optional. If there are no prere-
     quisites, just use #('Kernel') as the prerequisites."
    | file |
    fileIns isEmpty | prerequisites isEmpty | dir isEmpty ifTrue: [
        ^self error: 'parameter not optional'
    ].

    PackagePrerequisites isNil ifFalse: [
        PackagePrerequisites at: package asString put: prerequisites asSet.
        FileIns at: package asString put: fileIns.
        Directories at: package asString put: dir.
    ].

    file := FileStream
        open: Directory image, '/packages'
        mode: FileStream append.
    file
        nl;
        nl;
        nextPutAll: '# Added on';
        print: Date dateAndTimeNow; nl;
        nextPutAll: package; nl;
        nextPutAll: '   '.

    prerequisites do: [ :each | file space; nextPutAll: each ].

    file
        nl;
        nextPutAll: '   '.

    fileIns do: [ :each | file space; nextPutAll: each ].

    file
        nl;
        nextPutAll: '    ';
        nextPutAll: dir;
        nl;
        close.
!

refreshDependencies
    "Reload the `packages' file in the image directory"
    | state |
    LoadDate isNil ifFalse: [
        self stillValid ifTrue: [ ^self ]
    ].

    LoadDate := Date dateAndTimeNow.
    FileIns := Dictionary new.
    Directories := Dictionary new.
    PackagePrerequisites := Dictionary new.
    state := nil -> 1.
    self packageFileLinesDo: [ :each |
        self line: each state: state
    ].
    state value = 1 ifFalse: [ 
        ^self error: 'bad packages file'
    ].
!!


!PackageLoader class methodsFor: 'loading'!

extractDependenciesFor: packagesList onError: aBlock
    "Answer an OrderedCollection containing all the packages which you
     have to load to enable the packages in packagesList, in an appropriate
     order. For example

     PackageLoader extractDependenciesFor: #('BloxTestSuite' 'Blox' 'Browser')

     on a newly built image will evaluate to an OrderedCollection containing
     'Kernel', 'C:tclInit', 'Blox', 'BloxTestSuite' and 'Browser'. Note that
     Blox has been moved before BloxTestSuite.
     Pass an error message to aBlock if any of the packages needs C call-outs which
     are not defined."

    | toBeLoaded oldDep newDep |
    toBeLoaded := packagesList asOrderedCollection.
    oldDep := packagesList.
    [   newDep := Set new.
        oldDep do: [ :each |
            (self hasFeature: each) ifFalse: [
                (self isCallout: each)
                    ifTrue: [ ^aBlock value: 'C callout not available: ', each ].
                (self isLoadable: each)
                    ifFalse: [ ^aBlock value: 'package not available: ', each ].
                newDep addAll: (self prerequisitesFor: each)
            ]
        ].

        "I don't think there will never be lots of packages in newDep (say
         (more than 5), so I think it is acceptable to remove duplicates
         this naive way.
         Note that we remove duplicates from toBeLoaded so that prerequisites
         are always loaded *before*."
        toBeLoaded removeAll: newDep ifAbsent: [ :doesNotMatter | ].
        newDep isEmpty
    ]   whileFalse: [
        toBeLoaded addAllFirst: newDep.
        
        "Proceed recursively with the prerequisites for newDep"
        oldDep := newDep.
    ].
    ^toBeLoaded
!

fileInPackage: package
    "File in the given package into GNU Smalltalk."
    self fileInPackages: (Array with: package)
!

fileInPackages: packagesList
    "File in all the packages in packagesList into GNU Smalltalk."
    | toBeLoaded |
    toBeLoaded := self
        extractDependenciesFor: packagesList
        onError: [ :errorMessage | ^self error: errorMessage ].
        
    toBeLoaded do: [ :each | self primFileInPackage: each ]
! !


!PackageLoader class methodsFor: 'testing'!

canLoad: package
    "Answer whether all the needed C call-outs are registered within
     GNU Smalltalk"
    self
        extractDependenciesFor: (Array with: package)
        onError: [ ^false ].
    ^true
! !


!PackageLoader class methodsFor: 'private'!

hasFeature: feature
    "Private - Answer whether the given `feature' (either a C callout in the
     form C:funcName or a package name) is present in GNU Smalltalk"
    | descriptor |
    (self isCallout: feature)
	ifFalse: [ ^Smalltalk hasFeatures: feature asSymbol ].

    ^CFunctionDescriptor isFunction:
	(feature copyFrom: 3 to: feature size)
!

isLoadable: feature
    "Private - Answer whether the packages file includes an entry for `feature'"
    self refreshDependencies.
    ^FileIns includesKey: feature asString
!

isCallout: feature
    "Private - Answer whether the given `feature' represents a C callout in the
     form C:funcName"
    ^(feature size > 2) and: [
        ((feature at: 1) = $C) & ((feature at: 2) = $:)
    ]
!

primFileInPackage: package
    "Private - File in the given package without paying attention at
     dependencies and C callout availability"
    | dir |
    (self hasFeature: package) ifTrue: [ ^self ].

    Transcript
        nextPutAll: 'Loading package ', package;
        nl.

    dir := Directory working.
    Directory working: (self directoryFor: package).
    (self fileInsFor: package) do: [ :each | FileStream fileIn: each ].
    Directory working: dir.

    Smalltalk addFeature: package asSymbol
! !


!PackageLoader class methodsFor: 'private - loading packages file'!

packageFileName
    ^Directory image, '/packages'
!

packageFileLinesDo: aBlock
    "Private - Evaluate aBlock once for each non empty line in the packages
     file, passing it a TokenStream containing the line's contents.
     Every character in line, after a #, is discarded as a comment. "
    | file line discard ch tokens |
    file := FileStream open: self packageFileName mode: FileStream read.
    line := WriteStream on: (String new: 50).
    discard := false.
 
    [ file atEnd ] whileFalse: [
        ch := file next.
        discard ifFalse: [
            (discard := (ch = $#))
                ifFalse: [ line nextPut: ch ].
        ].
        ch = Character cr ifTrue: [ file peekFor: (ch := Character nl) ].
        ch = Character nl ifTrue: [
            tokens := TokenStream on: line contents.
            tokens atEnd ifFalse: [
                aBlock value: tokens.
                line emptyStream
            ].
            discard := false
        ].
    ].
    
    tokens := TokenStream on: line contents.
    tokens atEnd ifFalse: [
        aBlock value: tokens.
        line emptyStream
    ].
    file close
!

line: line state: state
    "Private - Parse the given line (a TokenStream) of the `packages' file,
     expecting the file as a sequence of:
     - line with package name
     - line with package prerequisites (either C callouts in the form
       C:funcName or package names)
     - line with file-ins
     - line with the file-ins' directory, relative to the kernel directory.
     
     Blank lines are discarded, like everything coming after a # field."

    | coll |
    state value = 1 ifTrue: [
        ^state key: line next value: 2
    ].
    state value = 2 ifTrue: [
        coll := PackagePrerequisites at: state key put: Set new.
        [ line atEnd ] whileFalse: [ coll add: line next ].
        ^state value: 3
    ].
    state value = 3 ifTrue: [
        coll := FileIns at: state key put: OrderedCollection new.
        [ line atEnd ] whileFalse: [ coll add: line next ].
        ^state value: 4.
    ].
    state value = 4 ifTrue: [
        Directories at: state key put: line next.
        ^state value: 1.
    ]
!

stillValid
    | date |
    date := (File name: self packageFileName) lastModifyTime.
    (date at: 1) > (LoadDate at: 1) ifTrue: [ ^false ].
    (date at: 1) < (LoadDate at: 1) ifTrue: [ ^true ].
    ^(date at: 2) < (LoadDate at: 2).
!!

