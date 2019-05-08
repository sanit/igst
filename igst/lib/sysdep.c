/***********************************************************************
 *
 * System specific implementation module.
 *
 * This module contains implementations of various operating system
 * specific routines.  This module should encapsulate most of these OS
 * specific calls so that the rest of the code is portable.
 *
 * $Revision: 1.6.2$
 * $Date: 1999/08/31 11:23:18$
 * $Author: pb$
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
 * GNU Smalltalk; see the file COPYING.	 If not, write to the Free Software
 * Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 ***********************************************************************/


#include "gst.h"
#include "sysdep.h"
#include "alloc.h"

#ifdef sgi
# define _BSD_SIGNALS
#endif

#include <errno.h>
#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/times.h>

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif 

#if defined(HAVE_SYS_TIME_H) || defined(TIME_WITH_SYS_TIME)
# include <sys/time.h>
#endif
#if !defined(HAVE_SYS_TIME_H) || defined(TIME_WITH_SYS_TIME)
# include <time.h>
#endif

#ifdef STDC_HEADERS
# include <string.h>
# include <stdlib.h>
#endif /* STDC_HEADERS */

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifndef WEXITSTATUS
# define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#endif
#ifndef WIFEXITED
# define WIFEXITED(stat_val) (((stat_val) & 255) == 0)
#endif

#if !defined(__OS2__) && !defined(_MSC_VER)
#  include <sys/param.h>
#endif
#ifdef _WIN32
#  include <windows.h>
#endif

#ifdef __OS2__
#  include <direct.h>
#endif

#ifdef HAVE_IO_H
# include <io.h>
#endif

#ifndef R_OK
# ifdef S_IREAD
#  define R_OK S_IREAD
# else
#  define R_OK 4
# endif
#endif

#ifndef PATH_MAX
#define PATH_MAX  1024 /* max length of a file and path */
#endif

#ifndef SIGALRM
#define 0		/* hmmm... */
#else
#define HAVE_SIGALRM
#endif

#if defined(HAVE_SIGHOLD) && !defined(HAVE_SIGSETMASK)
static unsigned long __cursigmask; /* keep track of signal mask status */
#endif

static int	tm_diff ();

/*
 * IntState disableInterrupts()
 *
 * Description
 *
 * Saves and returns the current state of the software interrupt system.
 * Disables all interrupts.
 *
 * Outputs
 *
 * The old state of the interrupt system (presumably for saving for a
 * later call to enableInterrupts).
 */
IntState
disableInterrupts()
{
#ifdef HAVE_SIGSETMASK
  return (sigsetmask(-1));
#else
#ifdef HAVE_SIGHOLD
  unsigned long oldmask = __cursigmask;
  register int i;

  __cursigmask = -1;
  for (i=1; i <= 32; i++) {
    sighold(i);	 /* want it blocked - ok if it already is */
  }
  return oldmask;
#else
  return 0;   /* no recognized interrupt mechanism */
#endif
#endif
}


/*
 * void enableInterrupts(mask)
 *
 * Description
 *
 * Restores the state of the interrupt system to that which it had when
 * "mask" was created.
 *
 * Inputs
 *
 * mask	 : An interrupt state...should have been returned at some point
 *  from a call to disableInterrupts.
 *
 */
void
enableInterrupts(mask)
     IntState mask;
{
#ifdef HAVE_SIGSETMASK
  sigsetmask(mask);

#else
#ifdef HAVE_SIGHOLD  /* !!! have config check for sigrelse too? */
  unsigned long oldmask = __cursigmask;
  register int i;

  __cursigmask = mask;
  for (i=1; mask != 0; i++, mask >>= 1) {
    if (oldmask & (0x1 << (i-1))) {
      sigrelse(i); /* want it unblocked and it is blocked */
    }
  }
#else
  ;    /* do nothing -- we don't know what to do */
#endif
#endif
}

void
setSignalHandler(signum, handlerFunc)
     int signum;
     RETSIGTYPE (*handlerFunc)(SIG_ARG_TYPE);
{
#ifdef _POSIX_VERSION
  /* If we are running on a posix-compliant system, then do
     things the Posix way. */
  struct sigaction act;

  act.sa_handler = handlerFunc;
#ifdef SA_ONESHOT
  act.sa_flags = SA_ONESHOT;
#else
#ifdef SA_RESETHAND
  act.sa_flags = SA_RESETHAND;
#else
  act.sa_flags = 0;
#endif
#endif

  sigemptyset (&act.sa_mask);
  sigaction (signum, &act, (struct sigaction *)NULL);
#else
  signal(signum, handlerFunc);
#endif
}




/*
 * unsigned long getMilliTime()
 *
 * Description
 *
 * Returns the local time in milliseconds
 *
 */
unsigned long
getMilliTime()
{
#ifdef _WIN32
  /* time() seems not to work... so we hack. */
  SYSTEMTIME st;
  GetLocalTime(&st);
  return (st.wMilliseconds +
	  st.wSecond * 1000 +
	  st.wMinute * 60000 +
	  st.wHour   * 3600000);
#else
#ifdef HAVE_GETTIMEOFDAY
  struct timeval t;

  gettimeofday(&t, nil);
  return (t.tv_sec * 1000 + t.tv_usec / 1000);
#else
#ifdef __OS2__
  time_t t;
  t = time(NULL);
  return (t * 1000 / CLOCKS_PER_SEC);
#else
  time_t t;
  struct tms dummy;

  t = times(&dummy);
  return (t * 1000 / HZ);
#endif
#endif
#endif
}

#define TM_YEAR_BASE 1900

/* Yield A - B, measured in seconds.
   This function is copied from the GNU C Library.  */
static int
tm_diff (a, b)
     struct tm *a, *b;
{
  /* Compute intervening leap days correctly even if year is negative.
     Take care to avoid int overflow in leap day calculations,
     but it's OK to assume that A and B are close to each other.  */
  int a4 = (a->tm_year >> 2) + (TM_YEAR_BASE >> 2) - ! (a->tm_year & 3);
  int b4 = (b->tm_year >> 2) + (TM_YEAR_BASE >> 2) - ! (b->tm_year & 3);
  int a100 = a4 / 25 - (a4 % 25 < 0);
  int b100 = b4 / 25 - (b4 % 25 < 0);
  int a400 = a100 >> 2;
  int b400 = b100 >> 2;
  int intervening_leap_days = (a4 - b4) - (a100 - b100) + (a400 - b400);
  int years = a->tm_year - b->tm_year;
  int days = (365 * years + intervening_leap_days
	      + (a->tm_yday - b->tm_yday));
  return (60 * (60 * (24 * days + (a->tm_hour - b->tm_hour))
		+ (a->tm_min - b->tm_min))
	  + (a->tm_sec - b->tm_sec));
}

/*
 * long adjustTimeZone(long)
 *
 * Description
 *
 * Converts the given time (expressed in seconds since midnight Jan 1, 1970,
 * and in Universal Coordinated Time) into a local time.
 *
 * Outputs
 *
 * The local time, also expressed in seconds since midnight Jan 1, 1970.
 */
long
adjustTimeZone(t)
     long t;
{
  struct tm save_tm, *decoded_time;
  long bias;

#ifdef LOCALTIME_CACHE
  tzset ();
#endif
  decoded_time = localtime(&t);
  save_tm = *decoded_time;
  decoded_time = gmtime (&t);
  bias = tm_diff (&save_tm, decoded_time);

  return(t + bias);
}

long
currentTimeZoneBias()
{
  long now;
  long bias;
  struct tm save_tm, *decoded_time;

  time((time_t *) &now);

#ifdef LOCALTIME_CACHE
  tzset ();
#endif

  decoded_time = localtime(&now);
  save_tm = *decoded_time;
  decoded_time = gmtime (&now);
  bias = tm_diff (&save_tm, decoded_time);
  return(bias);
}

/*
 * long getTime()
 *
 * Description
 *
 * Returns the time in seconds since midnight Jan 1, 1970 (standard UNIX
 * type time).
 *
 * Outputs
 *
 * As described above.
 */
long
getTime()
{
  time_t now;
  time(&now);

  return (adjustTimeZone((unsigned long) now));
}



/*
 * void signalAfter(deltaMilli, func)
 *
 * Description
 *
 * Set up func as a signal handler to be called after deltaMilli
 * milliseconds. The function should NOT expect anything about the
 * condition in which it is called (e.g. it should not expect to
 * be called from an interrupt handler, even though we pass SIGALRM
 * to it to fake this assumption some way).
 *
 * Inputs
 *
 * deltaMilli:
 * 	Time in milliseconds before the function is invoked.  Rounded
 * 	up to nearest second for machines without higher precision
 * 	timers.
 * func:
 *	Signal handling function invoked when interval expires
 *
 * Outputs
 *
 * None.
 */

#ifdef _WIN32
struct {
  HANDLE	hNewWaitEvent;
  HANDLE	hCritEvent;
  long		sleepTime;
  RETSIGTYPE	(*alarmFunc)();
} alarms;

/* thread for precise alarm callbacks */
void CALLBACK
alarmThread(unused)
     LPVOID unused;
{
  WaitForSingleObject(alarms.hNewWaitEvent, INFINITE);
  for(;;) {
    int sleepTime;
    RETSIGTYPE	(*alarmFunc)();

    WaitForSingleObject(alarms.hCritEvent, INFINITE);
    sleepTime = alarms.sleepTime;
    alarmFunc = alarms.alarmFunc;
    SetEvent(alarms.hCritEvent);

    if (sleepTime > 0) {
      if (WaitForSingleObject(alarms.hNewWaitEvent, sleepTime) != WAIT_TIMEOUT) {
	/* The old wait was canceled by a new one */
	continue;
      }
    }
    alarmFunc(SIGALRM);
    WaitForSingleObject(alarms.hNewWaitEvent, INFINITE);
  }
}

static void
initializeWin32Alarms()
{
  HANDLE hthread;
  DWORD tid;

  /* Starts as non-signaled, so alarmThread will wait */
  alarms.hNewWaitEvent = CreateEvent(NULL, FALSE, FALSE, NULL);

  /* Starts as signaled (i.e. the first wait won't block) */
  alarms.hCritEvent = CreateEvent(NULL, FALSE, TRUE, NULL);

  /* Start alarmThread with a 1024 bytes stack */
  hthread = CreateThread(NULL,
    1024,
    (LPTHREAD_START_ROUTINE) alarmThread,
    NULL, 0, &tid);

  /* This does not terminate the thread - it only releases our handle */
  CloseHandle(hthread);
}
#endif

void
signalAfter(deltaMilli, func)
     int deltaMilli;
     RETSIGTYPE (*func)();
{
/* Please feel free to make this more accurate for your operating system
 * and send me the changes.
 */
  if (deltaMilli <= 0) {
    func(SIGALRM);
  } else {

#ifdef _WIN32
    WaitForSingleObject(alarms.hCritEvent, INFINITE);
    alarms.sleepTime = deltaMilli;
    alarms.alarmFunc = func;
    SetEvent(alarms.hCritEvent);
    SetEvent(alarms.hNewWaitEvent);

#else
 #ifdef ITIMER_REAL
    struct itimerval value;
    value.it_interval.tv_sec = value.it_interval.tv_usec = 0;
    value.it_value.tv_sec = deltaMilli/1000;
    value.it_value.tv_usec = (deltaMilli%1000) * 1000;
    setSignalHandler(SIGALRM, func);
    setitimer(ITIMER_REAL, &value, (struct itimerval *)0);

 #else
  #ifdef HAVE_SIGALRM
    setSignalHandler(SIGALRM, func);
  #endif
  #ifdef HAVE_ALARM
    /* round up to nearest second */
    alarm((deltaMilli + 999)/ 1000);
  #else
    /* ugh: no support for alarms on this system!! */
    func(SIGALRM);
  #endif
 #endif
#endif
  }
}



/*
 * char *getCurDirName()
 *
 * Description
 *
 * Returns the path name for the current directory, without trailing
 * delimiter (?).
 *
 * Outputs
 *
 * Pointer to allocated string for current path name.  Caller has
 * responsibility for freeing the returned value when through.
 */
char *
getCurDirName()
{
#ifdef HAVE_UNISTD_H
  char *cwd;
  char *ret;
  unsigned path_max;
  int save_errno;

  path_max = (unsigned) PATH_MAX;
  path_max += 2;  /* The getcwd docs say to do this. */

  cwd = xmalloc (path_max);

  errno = 0;
  do {
    ret = getcwd (cwd, path_max);
    if (ret) {
      return (cwd);
    }
    if (errno != ERANGE) {
      break;
    }
    errno = 0;
    path_max += 128;
    cwd = xrealloc (cwd, path_max);
  } while (!errno);

  save_errno = errno;
  xfree (cwd);
  errno = save_errno;
  return (NULL);
#else
 #ifndef HAVE_GETWD
  #error "Must have either getcwd or getwd!!"
 #else
  char	name[PATH_MAX];
  
  getwd(name);
  return (strdup(name));
 #endif
#endif
}


/*
 * char *getFullFileName(fileName)
 *
 * Description
 *
 * Returns the full path name for a given file.
 *
 * Inputs
 *
 * fileName:
 *  Pointer to file name, can be relative or absolute
 *
 * Outputs
 *
 * Full path name string.  Caller has the responsibility for freeing the
 * returned string.
 */
char *
getFullFileName(fileName)
     char *fileName;
{
  char	*fullFileName;
  static char *fullPath = NULL;
  extern char *strdup();

  if (fileName[0] == '/') { /* absolute, so don't need to change */
    return (strdup(fileName));
  }

  if (fullPath == NULL) {
    /* Only need to do this once, then cache the result */
    fullPath = getCurDirName();
  }

  /*
   * ### canonicalize filename and full path here in the future (remove any
   * extraneous .. or . directories, etc.)
   */

  fullFileName = (char *)xmalloc(strlen(fullPath) + strlen(fileName)
    + 1 /* slash */
    + 1 /* trailing nul */);
  sprintf(fullFileName, "%s/%s", fullPath, fileName);
  return (fullFileName);
}


/*
 * long getFileModifyTime(fileName)
 *
 * Description
 *
 * Returns the time the file "fileName" was last modified.  On UNIX
 * machines, this is the number of seconds since midnight Jan 1 1970 GMT.
 * On other platforms/environments, it's probably not important exactly
 * what it returns as long as it's unites consistent with other accesses
 * that client code may do to the file system.
 *
 * Inputs
 *
 * fileName:
 *  Name of the file to be checked.
 *
 * Outputs
 *
 * Time the file was last modified, in some reasonable units for the local
 * operating system.
 */
long
getFileModifyTime(fileName)
     char *fileName;
{
  struct stat st;

  if (stat(fileName, &st) < 0) {
    return ((unsigned long)0);
  } else {
    return (adjustTimeZone((unsigned long) st.st_mtime));
  }
}


/*
 * mst_Boolean fileIsReadable(fileName)
 *
 * Description
 *
 * Returns true if the file named "fileName" exists and is readable by the
 * current process.  Returns false otherwise.
 *
 * Inputs
 *
 * fileName:
 *  The name of the file to check on.
 *
 * Outputs
 *
 * True if the file exists and is readable, false otherwise.
 */
mst_Boolean
fileIsReadable(fileName)
     char *fileName;
{
  return (access(fileName, R_OK) == 0);
}

OOP
getOpenFileSize(fd)
     int fd;
{
  struct stat statBuf;

  if (fstat(fd, &statBuf) < 0) {
    return nilOOP;
  } else {
    return fromInt(statBuf.st_size);
  }
}



FILE *
openPipe(command, mode)
     char *command, *mode;
{
#ifdef HAVE_POPEN
  return (popen(command, mode));
#else
#if defined(HAVE_PIPE) && defined(HAVE_WAITPID)

  int fildes[2];
  int parent_pipe_end, child_pipe_end;

  if (mode[1]) {
    return (NULL);	/* Must be a single character long */
  } else if (mode[0] == 'w') {
    parent_pipe_end = 1; child_pipe_end = 0;
  } else if (mode[0] == 'r') {
    parent_pipe_end = 0; child_pipe_end = 1;
  } else {
    return (NULL);
  }

  if (pipe(fildes) != 0) {
    /* Cannot create pipe */
    return (NULL);
  }

  switch (fork()) {
  case -1:
    /* Error */
    break;

  case 0:
    /* Child process */
    if ((close(fildes[parent_pipe_end]) != 0)
	|| (close(child_pipe_end) != 0)
	|| (dup(fildes[child_pipe_end]) != child_pipe_end)) {
      exit(1);
    }

    /* Write a single byte through the pipe to establish it */
    if (child_pipe_end == 1) {
      write(fildes[child_pipe_end],"z",1);
    } else {
      char buff[1];
      read(fildes[child_pipe_end],buff,1);
      if (*buff != 'z') {
        exit(1);
      }
    }

    exit (system(command) >= 0);
    /*NOTREACHED*/

  default:
    /* Parent process */
    if (close(fildes[child_pipe_end]) != 0) {
      break;
    }

    /* Write a single byte through the pipe to establish it */
    if (parent_pipe_end == 1) {
      write(fildes[parent_pipe_end],"z",1);
    } else {
      char buff[1];
      read(fildes[parent_pipe_end],buff,1);
      if (*buff != 'z') {
        break;
      }
    }

    return fdopen(fildes[parent_pipe_end], mode);
  }

  /* Error */
  close(fildes[0]);
  close(fildes[1]);
  return (NULL);
#endif
#endif
}

/*
 * int closePipe(file)
 *
 * Description
 *
 * Closes the given pipe (if it is a pipe).
 *
 * Inputs
 *
 * file	 : A stdio type FILE pointer.
 *
 * Outputs
 *
 * Returns -1 on error.
 */
int
closePipe(file)
     FILE *file;
{
#ifdef HAVE_POPEN
  return (pclose(file));
#else
  int statloc;
  close(fileno(file));
  waitpid(-1, &statloc, 0);

  /* need to call fclose() to deallocate FILE buffer */
  return (fclose(file));
#endif
}


FILE *
openFile(fileName, fileMode)
     char *fileName, *fileMode;
{
#if defined(_WIN32) || defined(__OS2__) || defined(MSDOS)
  char *fileModeBinary;
  fileModeBinary = alloca(strlen(fileMode) + 2);
  strcpy(fileModeBinary, fileMode);
  strcat(fileModeBinary, "b");
  return (fopen(fileName, fileModeBinary));
#else
  return (fopen(fileName, fileMode));
#endif
}


#ifndef HAVE_GETDTABLESIZE
int
getdtablesize()
{
  return (sizeof(short) * 8);	 /* a safe >>>hack<<< value */
}

#endif /* ! HAVE_GETDTABLESIZE */


/*
 * void initSysdep()
 *
 * Description
 *
 * Perform any system dependent initializations that are required.
 *
 */
void
initSysdep()
{
#ifdef _WIN32
  initializeWin32Alarms();
#endif
  tzset ();
}


#ifdef HAVE_STDARG_H
void debugf (const char *fmt, ...)
#else
void
debugf(va_alist)
     va_dcl
#endif
{
  char buf[1024];
  va_list args;
#ifndef HAVE_STDARG_H
  char  *fmt;
  va_start(args);
  fmt = va_arg(args, char *);
#else /* !HAVE_STDARG_H */
  va_start (args, fmt);
#endif /* !HAVE_STDARG_H */

  vsprintf (buf, fmt, args);

#if defined(_WIN32) && !defined(__CYGWIN__)&& !defined(__CYGWIN32__)
  /* VC++ has a Win32-friendly debugger, Cygwin has not */
  OutputDebugString (buf);
#else /* !_WIN32 */
  {
    static FILE *debFile = NULL;
    if (debFile == NULL) {
      debFile = fopen("mst.deb", "w");
    }
    fputs(buf, debFile);
    fflush(debFile);
  }
#endif /* !_WIN32 */

  va_end(args);
}
