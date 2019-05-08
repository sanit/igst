/***********************************************************************
 *
 *	Main (library) Module
 *
 *	$Revision: 1.6.2$
 *	$Date: 1999/08/31 11:23:18$
 *	$Author: pb$
 *
 ***********************************************************************/

/***********************************************************************
 *
 * Copyright 1990, 91, 92, 94, 95, 99 Free Software Foundation, Inc.
 * Written by Steve Byrne.
 *
 * This file is part of GNU Smalltalk.
 *
 * GNU Smalltalk is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2, or (at your option) any later 
 * version.
 * 
 * GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * GNU Smalltalk; see the file COPYING.  If not, write to the Free Software
 * Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
 *
 ***********************************************************************/

#include "gst.h"
#include "alloc.h"
#include "comp.h"
#include "save.h"
#include "lib.h"
#include "cint.h"
#include "callin.h"
#include "dld_gst.h" 
#include "lex.h"
#include "dict.h"
#include "str.h"
#include "sysdep.h" 
#include <stdio.h>
#include <sys/types.h>
#ifdef HAVE_IO_H
#include <io.h>
#else
#include <sys/file.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#ifndef MAXPATHLEN
#define MAXPATHLEN		1024 /* max length of a file and path */
#endif

#ifdef NAMES83
#define MAP_FILE(name83, name14)	name83
#else
#define MAP_FILE(name83, name14)	name14
#endif


#ifdef atarist
#define INIT_FILE_NAME		".gstinit"
#define PRE_IMAGE_FILE_NAME	".gstpre" /* hope this is ok */
#else
#ifdef MSDOS
#define INIT_FILE_NAME		"_stinit"
#define PRE_IMAGE_FILE_NAME	"_stpre"
#else
#define INIT_FILE_NAME		".stinit"
#define PRE_IMAGE_FILE_NAME	".stpre"
#endif
#endif


extern int		yydebug;
extern char		*getenv();



static char		helpText[] =
   "GNU Smalltalk usage:"
   "\n"
   "\n    gst [ flag ... ] [ file ...]"
   "\n"
   "\nShort flags can appear either as -xyz or as -x -y -z.  If an option is"
   "\nmandatory for a long option, it is also mandatory for a short one. The"
   "\ncurrently defined set of flags is:"
   "\n   -a --smalltalk\t\t Pass the remaining arguments to Smalltalk"
   "\n   -c --core-dump\t\t Dump core on fatal signal"
   "\n   -d --declaration-trace-user\t Trace compilation of files on the command line"
   "\n   -D --declaration-trace-all\t Trace compilation of all loaded files"
   "\n   -e --execution-trace-user\t Trace execution of files on the command line"
   "\n   -E --execution-trace-all\t Trace execution of all loaded files"
   "\n   -g --no-gc-message\t\t Do not print garbage collection messages"
   "\n   -H -h -? --help\t\t Print this message and exit"
   "\n   -i --rebuild-image\t\t Ignore the image file; rebuild it from scratch"
   "\n   -I --image-file file\t\t Instead of `gst.im', use `file' as the image\n\t\t\t\t file, and ignore the kernel files' timestamps\n"
   "\n   -l --log-changes\t\t Log changes in `change-log.st'"
   "\n   -L --log-file file\t\t Log changes in file `file'"
   "\n   -p --emacs-mode\t\t Execute as a `process' (from within Emacs)"
   "\n   -q --quiet --silent\t\t Do not print execution information"
   "\n   -Q --no-messages\t\t Run Smalltalk with -q -g and no startup banner"
   "\n   -r --regression-test\t\t Run in regression test mode, i.e. make\n\t\t\t\t printed messages constant\n"
   "\n   -s --store-no-source\t\t Don't store source code as Strings for\n\t\t\t\t methods outside the kernel directory\n"
   "\n   -S --snapshot\t\t Save a snapshot just before exiting"
   "\n   -v --version\t\t\t Print the Smalltalk version number and exit"
   "\n   -V --verbose\t\t\t Print names of loaded files"
   "\n   -y --yacc-debug\t\t Turn on debugging in the parser"
   "\n   -  --\t\t\t Read input from standard input explicitly"
   "\n"
   "\nFiles are loaded one after the other.  After the last one is loaded,"
   "\nSmalltalk will exit.  If no files are specified, Smalltalk reads from"
   "\nthe terminal, with prompts."
   "\n"
   "\nReport bugs to bug-gnu-smalltalk@gnu.ai.mit.edu,"
   "\nwith a Cc: to bonzini@gnu.org";

static char		copyrightAndLegalStuffText[] =
   "Copyright 1990, 91, 92, 94, 95, 99 Free Software Foundation, Inc."
   "\n"
   "\nGNU Smalltalk comes with NO WARRANTY, to the extent permitted by law."
   "\nYou may redistribute copies of GNU Smalltalk under the terms of the"
   "\nGNU General Public License.  For more information, see the file named COPYING."
   "\n"
   "\n";

static char		*longOptions[] = {
   "-a\0--smalltalk",			"-c\0--core-dump",
   "-d\0--declaration-trace-user",	"-D\0--declaration-trace-all",
   "-e\0--execution-trace-user",	"-E\0--execution-trace-all",
   "-g\0--no-gc-message",		"-H\0--help",
   "-i\0--rebuild-image",		"-I\0--image-file",
   "-l\0--log-changes",			"-L\0--log-file",
   "-p\0--emacs-mode",			"-q\0--quiet",
   "-Q\0--no-messages",			"-r\0--regression-test",
   "-q\0--silent",			"-s\0--store-no-source",
   "-S\0--snapshot",			"-v\0--version",
   "-V\0--verbose",			"-y\0--yacc-debug",
   nil};



#ifdef unused
/**/extern int		adds, reused, reprobes, hitsOn[];
#endif /* unused */

/* When true, this flag suppresses the printing of execution-related
 * messages, such as the number of byte codes executed by the
 * last expression, etc.
 */
mst_Boolean		quietExecution;


char			*kernelFileDefaultPath, *imageFileDefaultPath;

/* This string contains the printed representation of the Smalltalk version
 * number
 */
char			versionString[50];

/* This is the name of the binary image to load.  If it is not nil after the
 * command line is parsed, the checking of the dates of the kernel source files
 * against the image file date is overridden.  If it is nil, it is set to
 * defaultImageName.
 */
char			*binaryImageName = nil;

/* This is used by the callin functions to auto-initialize Smalltalk.
 * When it's not true, initialization needs to be performed.  It's set
 * to true by initSmalltalk().
 */
mst_Boolean			smalltalkInitialized = false;

/* Set by command line flag.  When true, Smalltalk creates FileSegment objects
 * even for files outside the kernel directory.
 */

mst_Boolean			storeNoSource = false;



/***********************************************************************
 *
 *	Private declarations
 *
 ***********************************************************************/

static char *		parseLongOption();
static mst_Boolean 	processFile(), okToLoadBinary(), findKernelFile();
static void		loadStandardFiles(), loadUserInitFile(), initPaths(),
			parseArgs(), loadUserPreImageFile(), makeVersionString();

/* Set by command line flag.  When true, Smalltalk saves a snapshot after
 * loading the files on the command line, before exiting.
 */

static mst_Boolean		snapshotAfterLoad = false;

/* Whether SMALLTALK_IMAGE is set.  Only if it is set, .stpre is loaded. */
static mst_Boolean		isLocalImage;

/* The imageFileDefaultPath followed by /gst.im */
static char			*defaultImageName;

/* Set by command line flag.  When true, Smalltalk does not print any sort
 * of version banner at the startup of the interpreter.  This makes Smalltalk
 * usable as a filter, or as a pure producer of content (such as the
 * main program in a cgi-bin script).
 */

static mst_Boolean		noBanner = false;

/* Set by cmd line flag.  If true, Smalltalk is more verbose about what it's
 * doing.
 */
static mst_Boolean		verbose = false;

/* If true, even both kernel and user method definitions are shown as
 * they are compiled.
 */
static mst_Boolean		traceKernelDeclarations;

/* If true, execution tracing is performed when loading kernel method
 * definitions
 */
static mst_Boolean		traceKernelExecution;

/* If true, skip date checking of kernel files vs. binary image; pretend
 * that binary image does not exist
 */
static mst_Boolean		ignoreImage;

/* If true, make printed messages constant.
 */
mst_Boolean			regressionTesting;

/* Set by command line flag.  When this is true, the interpreter 
 * does not print out things like "execution begins" or information
 * about the number of byte codes executed.
 */
static mst_Boolean		runQuietly = false;


/* The complete list of "kernel" class and method definitions.  Each of
 * these files is loaded, in the order given below.  Their last modification
 * dates are compared against that of the image file; if any are newer,
 * the image file is ignored, these files are loaded, and a new image file
 * is created.
 */
static char		*standardFiles[] = {
  "Builtins.st", 
  "Object.st",
  "Message.st",
  "Boolean.st",
  "False.st",
  "True.st",
  MAP_FILE("Magnitud.st", "Magnitude.st"),
  "Integer.st",
  "Date.st",
  "Time.st",
  "Number.st",
  "Float.st",
  "Fraction.st",
  "LargeInt.st",
  MAP_FILE("Charactr.st", "Character.st"),
  MAP_FILE("Assoc.st", "Association.st"),
  "Link.st",
  "Process.st",
  MAP_FILE("Collectn.st", "Collection.st"),
  MAP_FILE("SeqColl.st", "SeqCollect.st"),
  MAP_FILE("LnkList.st", "LinkedList.st"),
  MAP_FILE("Semaphor.st", "Semaphore.st"),
  MAP_FILE("ArrdColl.st", "ArrayColl.st"),
  "Array.st",
  "String.st",
  "Symbol.st",
  MAP_FILE("ByteArr.st", "ByteArray.st"),
  MAP_FILE("CompMeth.st", "CompildMeth.st"),
  "Interval.st",
  MAP_FILE("OrdColl.st", "OrderColl.st"),
  MAP_FILE("SortColl.st", "SortCollect.st"),
  "Bag.st",
  MAP_FILE("MapColl.st", "MappedColl.st"),
  "Set.st",
  MAP_FILE("IdentSet.st", "IdentitySet.st"),
  MAP_FILE("Dict.st", "Dictionary.st"),
  MAP_FILE("IdDict.st", "IdentDict.st"),
  MAP_FILE("SysDict.st", "SysDict.st"),
  "Stream.st",
  MAP_FILE("PosStrm.st", "PosStream.st"),
  MAP_FILE("RdStream.st", "ReadStream.st"),
  MAP_FILE("WrStream.st", "WriteStream.st"),
  "RWStream.st",
  MAP_FILE("ByteStrm.st", "ByteStream.st"),
  MAP_FILE("FStream.st", "FileStream.st"),
  MAP_FILE("TokStrm.st", "TokenStream.st"),
  "Random.st",
  MAP_FILE("UndefObj.st", "UndefObject.st"),
  MAP_FILE("ProcSch.st", "ProcSched.st"),
  "Delay.st",
  MAP_FILE("ShQueue.st", "SharedQueue.st"),
  "Behavior.st",
  MAP_FILE("ClassDsc.st", "ClassDesc.st"),
  "Class.st",
  MAP_FILE("Metaclas.st", "Metaclass.st"),
  MAP_FILE("Context.st", "ContextPart.st"),
  MAP_FILE("MthCtxt.st", "MthContext.st"),
  MAP_FILE("BlkCtxt.st", "BlkContext.st"),
  MAP_FILE("BlkClsr.st", "BlkClosure.st"),
  "Memory.st",
  MAP_FILE("WordMem.st", "WordMemory.st"),
  MAP_FILE("ByteMem.st", "ByteMemory.st"),
  MAP_FILE("MthInfo.st", "MethodInfo.st"),
  MAP_FILE("FileSeg.st", "FileSegment.st"),
  "SymLink.st",
  MAP_FILE("init.st", "initialize.st"),
  "CObject.st",
  "CType.st",
  "CFuncs.st",
  "CStruct.st",
  "Autoload.st",
  MAP_FILE("Trnscrpt.st", "Transcript.st"),	/* yuck! no vocals! */
  "File.st",
  MAP_FILE("Dir.st", "Directory.st"),
  "Browser.st",
  "RunArray.st",
  MAP_FILE("DirMsg.st", "DirMessage.st"),
  MAP_FILE("ExcHandl.st", "ExcHandling.st"),
  MAP_FILE("Adaptors.st", "ValueAdapt.st"),
  MAP_FILE("ObjDump.st", "ObjDumper.st"),
  "Point.st",
  MAP_FILE("Rect.st", "Rectangle.st"),
  MAP_FILE("PkgLoad.st", "PkgLoader.st"),
  "DLD.st",
  nil
};

static char	*smalltalkArgVec[] = { "gst", nil };
static char	**smalltalkArgv = smalltalkArgVec ;
static int	smalltalkArgc = 0;

char		**smalltalkPassedArgv = nil ;
int		smalltalkPassedArgc = 0;



void
smalltalkArgs(argc, argv)
     int	argc;
     char	**argv;
{
  smalltalkArgc = argc;
  smalltalkArgv = argv;
}


void
initSmalltalk()
{
  mst_Boolean	loadBinary, traceUserDeclarations, traceUserExecution;

#ifdef USE_MONCONTROL
  moncontrol(0);		/* don't monitor the initial stuff */
#endif /* USE_MONCONTROL */

  gst_dld_init(smalltalkArgv[0]);

  yydebug = 0;
  traceKernelDeclarations = declareTracing = false;
  traceKernelExecution = executionTracing = false;
  regressionTesting = false;
  ignoreImage = false;
  verbose = false;

  /*
   * Even though we're nowhere near through initialization, we set this
   * to make sure we don't invoke a callin function which would recursively
   * invoke us.
   */
  smalltalkInitialized = true;

  initPaths();
  makeVersionString();
#ifdef HAVE_READLINE
  initializeReadline();
#endif /* HAVE_READLINE */

  parseArgs(smalltalkArgc, smalltalkArgv);
  initSysdep();
  initSignals();
  initMem();
  initCFuncs();
  initOOPRegistry();

  if (binaryImageName) {
    loadBinary = true;
  } else {
    binaryImageName = defaultImageName;
    loadBinary = !ignoreImage && okToLoadBinary();
  }

  
  if (loadBinary && loadFromFile(binaryImageName)) {
    initChangesStream();
    initDefaultCompilationEnvironment();
    initRuntimeObjects();
    initInterpreter();
    gcOn();
  } else {
    initOOPTable();
    initDictionary();
    initSymbols();
    initInterpreter();

    resetChangesFile();
    installInitialMethods();
    initChangesStream();

    traceUserDeclarations = declareTracing;
    traceUserExecution = executionTracing;
    if (!traceKernelDeclarations) {
      declareTracing = false;
    }
    if (!traceKernelExecution) {
      executionTracing = false;
    }

    gcOn();
    loadStandardFiles();

    declareTracing = traceUserDeclarations;
    executionTracing = traceUserExecution;

    if (isLocalImage) {
      loadUserPreImageFile();
    }
    if (!saveToFile(defaultImageName)) {
      errorf("Couldn't open file %s", defaultImageName);
    }
  }

#ifdef USE_MONCONTROL
  moncontrol(1);
#endif /* USE_MONCONTROL */

  invokeInitBlocks();
  loadUserInitFile();

#ifdef unused
/**/#ifdef symbol_table_profiling
/**/  {
/**/    int i;
/**/    printf("%d adds, %d reused %d reprobes\n", adds, reused, reprobes);
/**/    for (i = 0; i < OOP_TABLE_SIZE; i++) {
/**/      if (hitsOn[i]) {
/**/	    printf("hitsOn[%d] = %4d\t%d\t%4x", i, hitsOn[i], i, i);
/**/	    printObject(oopAt(i));
/**/	    printf("\n");
/**/      }
/**/    }
/**/  }
/**/#endif /* symbol_table_profiling */
#endif /* unused */

}

void
topLevelLoop()
{
  int		filesProcessed;

  if (!noBanner) {
    printf("GNU Smalltalk Ready\n\n");
  }
  runQuietly = true ; // sanit add
  quietExecution = runQuietly || emacsProcess;

  for (filesProcessed = 0; *++smalltalkArgv; ) {
    if (smalltalkArgv[0][0] != '-') {
      processFile(smalltalkArgv[0], quietExecution);
      filesProcessed++;
    } else if (smalltalkArgv[0][1] == '-' || smalltalkArgv[0][1] == '\0') {
      /* either - by itself or -- indicates standard input */
      nonInteractive = false;
#ifdef HAVE_READLINE
      pushReadlineString();
#else
      pushUNIXFile(stdin, "stdin");
#endif /* HAVE_READLINE */
      parseStream();
      popStream(true);
      nonInteractive = true;
    }
  }

  if (filesProcessed == 0) {	/* didn't do any from cmd line, so read stdin*/
    nonInteractive = false;
#ifdef HAVE_READLINE
    pushReadlineString();
#else
    pushUNIXFile(stdin, "stdin");
#endif /* HAVE_READLINE */
    parseStream();
  } else if (snapshotAfterLoad) {
    saveToFile(binaryImageName);
  }

#ifdef USE_MONCONTROL
   moncontrol(0);
#endif /* USE_MONCONTROL */

}

/*
 *	static void initPaths()
 *
 * Description
 *
 *	Sets up the paths for the kernel source directory and for where the
 *	saved Smalltalk binary image lives.  Uses environment variables
 *	SMALLTALK_KERNEL and SMALLTALK_IMAGE if they are set, otherwise uses
 *	the paths assigned in mstpaths.h.
 *
 */
void
initPaths()
{
  char *currentDirectory = getCurDirName();

  if ((kernelFileDefaultPath = (char *)getenv("SMALLTALK_KERNEL")) == nil) {
    kernelFileDefaultPath = KERNEL_PATH;
  }
  if (!fileIsReadable(kernelFileDefaultPath)) {
    kernelFileDefaultPath = xmalloc(1024);
#ifndef ios_device
    strcpy(kernelFileDefaultPath, currentDirectory);
    strcat(kernelFileDefaultPath, "/kernel");
#else
    char *homeDirectory = (char *)getenv("HOME") ;
      
    strcpy(kernelFileDefaultPath,homeDirectory);//sanit add 1 May 2018
    strcat(kernelFileDefaultPath,"/Documents") ; //sanit add 1 May 2018
#endif
    resizeString(&kernelFileDefaultPath);
  }
#ifndef ios_device
  imageFileDefaultPath = (char *)getenv("SMALLTALK_IMAGE");
#endif
    
#ifdef ios_device
  imageFileDefaultPath = (char *)getenv("HOME");//sanit add 1 May 2018
  strcat(imageFileDefaultPath,"/Documents") ; //sanit add 1 May 2018
#endif
  isLocalImage = true;
  
  if (imageFileDefaultPath == nil || !fileIsReadable(imageFileDefaultPath)) {
    if (fileIsReadable(basicImageName)) {
      imageFileDefaultPath = xmalloc(1024);
      strcpy(imageFileDefaultPath, currentDirectory);
      resizeString(&imageFileDefaultPath);
    } else {
      imageFileDefaultPath = IMAGE_PATH;

      /* This is a kludge so that OSes such as Windows and MS-DOS which have
       * no concept of home directories always load the .stpre file.
       */
      isLocalImage = ( ((char *)getenv("HOME")) == nil);
    }
  }

  defaultImageName = xmalloc(1024);
#ifndef ios_device
  imageFileDefaultPath = "/Users/Sanit/Library/Containers/th.ac.kmutt.sit.gst/Data" ;
//  imageFileDefaultPath = "/var/mobile/Containers/Data/Application/3FA4972D-BDC4-42E9-B3D2-215237E3ABFB/Documents" ;
#endif
  strcpy(defaultImageName, imageFileDefaultPath);
  strcat(defaultImageName, "/" basicImageName);
  resizeString(&defaultImageName);

  free(currentDirectory);
}

mst_Boolean
okToLoadBinary()
{
  long		imageFileTime;
  char		**fileNames, fullFileName[MAXPATHLEN],
  		*home, preImageFileName[MAXPATHLEN];

  imageFileTime = getFileModifyTime(binaryImageName);

  if (imageFileTime == 0) { /* not found */
    return (false);
  }

  for (fileNames = standardFiles; *fileNames; fileNames++) {
    if (findKernelFile(*fileNames, fullFileName) && !isLocalImage) {
      /* file lives locally but the image doesn't -- bad semantics.
         Note that if SOME of the files are local and the image file
         is local, it is good. */
      return (false);
    }
    if (imageFileTime < getFileModifyTime(fullFileName)) {
      return (false);
    }
  }

  if (isLocalImage) {
    if ((home = (char *)getenv("HOME")) != nil) {
      sprintf(preImageFileName, "%s/%s", home, PRE_IMAGE_FILE_NAME);
    } else {
      strcpy(preImageFileName, PRE_IMAGE_FILE_NAME);
    }
    if(imageFileTime < getFileModifyTime(preImageFileName)) {
      return (false);
    }
  }

  return (true);
}

/*
 *	static void loadStandardFiles()
 *
 * Description
 *
 *	Loads the kernel Smalltalk files.  It uses a vector of file names, and
 *	loads each file individually.  To provide for greater flexibility, if a
 *	one of the files exists in the current directory, that is used in
 *	preference to one in the default location.  The default location can be
 *	overridden at runtime by setting the SMALLTALK_KERNEL environment
 *	variable. 
 *
 */
void
loadStandardFiles()
{
  char		**fileNames, fullFileName[MAXPATHLEN], willStoreNoSource;
  
  willStoreNoSource = storeNoSource;
  storeNoSource = true;

  for (fileNames = standardFiles; *fileNames; fileNames++) {
    findKernelFile(*fileNames, fullFileName);
    if (!processFile(fullFileName, true)) {
      fprintf(stderr,
	      "Can't find system file '%s', proceeding without it.\n",
	      fullFileName);
    }
  }

  storeNoSource = willStoreNoSource;
}


/*
 *	static mst_Boolean findKernelFile(fileName, fullFileName)
 *
 * Description
 *
 *	Attempts to find a viable kernel Smalltalk file (.st file).  First
 *	tries the current directory to allow for overriding installed kernel
 *	files.  If that isn't found, the full path name of the installed kernel
 *	file is stored in fullFileName.  Note that the directory part of the
 *	kernel file name in this second case can be overridden by defining the
 *	SMALLTALK_KERNEL environment variable to be the directory that should
 *	serve as the kernel directory instead of the installed one.
 *
 * Inputs
 *
 *	fileName: 
 *		A simple file name, sans directory.
 *	fullFileName: 
 *		The file name to use for the particular kernel file is returned
 *		in this variable (which must be a string large enough for any
 *		file name).  If there is a file in the current directory with
 *		name "fileName", that is returned; otherwise the kernel path is
 *		prepended to fileName (separated by a slash, of course) and
 *		that is stored in the string pointed to by "fullFileName".
 *
 * Outputs
 *
 *	Returns true if the kernel file is found in the local directory
 *	or its `kernel' subdirector, and false if the file was found using
 *	the default path.
 */
mst_Boolean
findKernelFile(fileName, fullFileName)
     char *fileName, *fullFileName;
{
  if (fileIsReadable(fileName)) {
    strcpy(fullFileName, fileName);
    return (true);
  }

  strcpy(fullFileName, "kernel/");
  strcat(fullFileName, fileName);
  if (fileIsReadable(fullFileName)) {
    return (true);
  }

  sprintf(fullFileName, "%s/%s", kernelFileDefaultPath, fileName);
  return (false);
}


void
loadUserPreImageFile()
{
  char		fileName[MAXPATHLEN], *home;

  if ((home = (char *)getenv("HOME")) != nil) {
    sprintf(fileName, "%s/%s", home, PRE_IMAGE_FILE_NAME);
  } else {
    strcpy(fileName, PRE_IMAGE_FILE_NAME);
  }
  processFile(fileName, quietExecution);
}

void
loadUserInitFile()
{
  char		fileName[MAXPATHLEN], *home;

  if ((home = (char *)getenv("HOME")) != nil) {
    sprintf(fileName, "%s/%s", home, INIT_FILE_NAME);
  } else {
    strcpy(fileName, INIT_FILE_NAME);
  }
  processFile(fileName, quietExecution);
}

mst_Boolean
processFile(fileName, quiet)
     char	*fileName;
     mst_Boolean	quiet;
{
  FILE		*file;

  file = openFile(fileName, "r");
  if (file == NULL) {
    return (false);
  }

  if (verbose) {
    printf("Processing %s\n", fileName);
  }
  quietExecution = quiet;
  pushUNIXFile(file, fileName);
  parseStream();
  popStream(true);
  return (true);
}


/*
 *	static char *parseLongOption(opt, arg, defaultValue)
 *
 * Description
 *
 *	Tries to match the long option contained in arg to one of those
 *	in the opt table. If none matched, print an error message and
 *	return defaultValue.
 *	If more than one option begins with the string contained in arg,
 *	also print an error message and return defaultValue.
 *
 * Inputs
 *
 *	opt: 
 *		A NULL-terminated table of strings representing the
 *		different long options. The format of each entry is
 *		"<equivalent parameter using short options>\0<long option>"
 *
 *	arg: 
 *		The element of argv to be matched, including --
 *
 *	defaultValue:
 *		Typically, an option which shows an help screen.
 *
 * Outputs
 *
 *	Returns a string that can be parsed just like a short option
 *	argument.
 */
char *
parseLongOption(opt, arg, defaultValue)
     char **opt;
     char *arg, *defaultValue;
{
  char *i, *j;
  char *possibleAnswer = nil;
  for (; *opt; opt++) {
    i = *opt;
    j = arg;

    /* First get past the \0 - i.e. past the equivalent short option */
    while (*i++) ;
    do {
      if (*j != '\0') {
        continue;
      }
      /* End of argv option: even if there are more characters, it
         could be ok - as long as only one option matches. Note that
         if the option ends in the table (*i == '\0') and not
         in argv (*j != '\0'), it is an error. */
      if (possibleAnswer) {
	printf("Ambiguous option %s.\n", arg);
	return (defaultValue);
      } else {
	/* Found a matching option, remember the equivalent short option. */
	possibleAnswer = *opt;
	break;
      }
    } while (*i++ == *j++);
    /* If we get here, either this option did not match, or it did match but
       we have to go on to check for possible ambiguities. */
  }

  if (!possibleAnswer) {
    /* No good option found. Print error message. */
    printf("Unrecognized option %s.\n", arg);
    possibleAnswer = defaultValue;
  }
  return (possibleAnswer);
}

/*
 *	static void parseArgs(argc, argv)
 *
 * Description
 *
 *	This routine scans the command line arguments, accumulating information
 *	and setting flags.  This will probably be replaced by getopt in a
 *	future version of Smalltalk.
 *
 * Inputs
 *
 *	argc  : The number of arguments present
 *	argv  : Vector of strings that are the arguments.
 *
 */
void
parseArgs(argc, argv)
     int	argc;
     char	**argv;
{
  char		**av;
  char		*flags;

  smalltalkPassedArgc = 0;
  while (*++argv) {
    if (argv[0][0] != '-') {
      continue;
    }
    if (argv[0][1] == '-' && argv[0][2] != '\0') {
      /* Try to convert a long option to a short one */
      *argv = parseLongOption(longOptions, argv[0], "-h");
    }

    /* Parse short options */
    flags = &argv[0][1];

    for (; *flags; flags++) {
      switch (*flags) {
	case 'c': makeCoreFile = true;			break;
	case 'D': traceKernelDeclarations = true; 	/* fall thru */
	case 'd': declareTracing = true;		break;
	case 'E': traceKernelExecution = true;		/* fall thru */
	case 'e': executionTracing = true;		break;
	case 'g': gcMessage = false;			break;
	case 'i': ignoreImage = true;			break;
	case 'p': emacsProcess = true;			break;
	case 'Q': gcMessage = false; noBanner = true;   /* fall thru */
	case 'q': runQuietly = true;			break;
	case 'r': regressionTesting = true;		break;
	case 's': storeNoSource = true;			break;
	case 'S': snapshotAfterLoad = true;		break;
	case 'V': verbose = true;			break;
	case 'y': yydebug = 1;				break;

	/* ??? Is there some better place for this to be?  Canonical places
	 * seem to lose in that canonical places are often write-protected,
	 * which is ok for extant changes (like in a saved system), but not
	 * for new changes by the user.  Again, the notion of "the" user's
	 * home area is kind of random if there are multiple snapshots,
	 * since each snapshot should be associated with a particular changes
	 * file for which it is relevant. We use the current directory and
	 * allow to override with the -L option. */
	case 'l': changeFileName = "change-log.st";     break;

	case '-':
	  /* stdin is just another file to process, so it matters later */
	  break;

	case 'a':
	  /* "Officially", the C command line ends here.  The Smalltalk command
	      line, instead, starts right after the parameter containing -a */
	  smalltalkPassedArgv = argv + 1;
          *argv = nil;
          while (*++argv) {
            smalltalkPassedArgc++;
          }
          /* Get back to the last valid argv. ++argv in the while statement
             will get to the zero pointer. */
          argv--;
          break;

	case 'I':
	  binaryImageName = argv[1];
	  for (av = argv+1; *av; av++) {	/* remove this argument */
	    *av = *(av+1);
	  }
	  break;

	case 'L':
	  changeFileName = argv[1];
	  for (av = argv+1; *av; av++) {	/* remove this argument */
	    *av = *(av+1);
	  }
	  break;

	case 'v':
	  printf("GNU Smalltalk version %s\n", versionString);
	  printf("written by Steve Byrne (sbb@gnu.org) and Paolo Bonzini (bonzini@gnu.org)\n");
	  printf(copyrightAndLegalStuffText);
	  printf("Using default kernel path: %s\n", KERNEL_PATH);
	  printf("Using default image path : %s\n", IMAGE_PATH);
	  exit(0);

	default:
	  printf("Unrecognized option -%c.\n", *flags);
	  /* Fall through and show help message */

	case 'h': case 'H': case '?':
	  printf(helpText);
          exit(0);
      }
    }
  }

  if (regressionTesting) {
    traceKernelDeclarations = declareTracing = false;
    traceKernelExecution = executionTracing = false;
    verbose = false;
  }
}

void
makeVersionString()
{
  if (ST_EDIT_VERSION != 0) {
    sprintf(versionString, "%d.%d.%s%d", ST_MAJOR_VERSION, ST_MINOR_VERSION,
	    ST_EDIT_PREFIX, ST_EDIT_VERSION);
  } else {
    sprintf(versionString, "%d.%d", ST_MAJOR_VERSION, ST_MINOR_VERSION);
  }
}
