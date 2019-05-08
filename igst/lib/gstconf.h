/* gstconf.h.  Generated automatically by configure.  */
/* gstconf.h.in.  Generated automatically from configure.in by autoheader.  */

/* Define if on AIX 3.
   System headers sometimes define this.
   We just want to avoid a redefinition error message.  */
#ifndef _ALL_SOURCE
/* #undef _ALL_SOURCE */
#endif

/* Define if using alloca.c.  */
/* #undef C_ALLOCA */

/* Define to one of _getb67, GETB67, getb67 for Cray-2 and Cray-YMP systems.
   This function is required for alloca.c support on those systems.  */
/* #undef CRAY_STACKSEG_END */

/* Define if you have alloca, as a function or macro.  */
#define HAVE_ALLOCA 1

/* Define if you have <alloca.h> and it should be used (not on Ultrix).  */
#define HAVE_ALLOCA_H 1

/* Define as __inline if that's what the C compiler calls it.  */
/* #undef inline */

/* Define if your system's dynamic loading method is supported.  */
#define HAVE_DLD 1

/* Define if your CC has the `&&' and `goto void *' GCC extensions. */
#define HAVE_GOTO_VOID_P 1

/* Define if your system's sockets provide access to the Internet.  */
#define HAVE_INET_SOCKETS 1

/* Define if your system's localtime(3) caches the timezone.  */
/* #undef LOCALTIME_CACHE */

/* Define if you need to in order for stat and other things to work.  */
/* #undef _POSIX_SOURCE */

/* Define as the return type of signal handlers (int or void).  */
#define RETSIGTYPE void

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at run-time.
	STACK_DIRECTION > 0 => grows toward higher addresses
	STACK_DIRECTION < 0 => grows toward lower addresses
	STACK_DIRECTION = 0 => direction of growth unknown
 */
/* #undef STACK_DIRECTION */

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS 1

/* Define if you can safely include both <sys/time.h> and <time.h>.  */
#define TIME_WITH_SYS_TIME 1

/* Define if your <sys/time.h> declares struct tm.  */
/* #undef TM_IN_SYS_TIME */

/* Define if your processor stores words with the most significant
   byte first (like Motorola and SPARC, unlike Intel and VAX).  */
/* #undef WORDS_BIGENDIAN */

/* This is anbled if you wnat the also ultra-winning GNU readline library
 * to be present in the command reader.
 */
 //#define HAVE_READLINE 1
#undef HAVE_READLINE

/* Define as __const if that's what the C compiler calls it.  */
/* #undef const */

/* Define this with the memory alignment required by type "double" */
#define DOUBLE_ALIGNMENT SIZEOF_CHAR_P

/* Define if your system is a loser with 8.3 filenames  */
/* #undef NAMES83 */

/* Define if the C compiler doesn't support void * and enums.  */
/* #undef OLDCC */

/* Define as empty if the C compiler doesn't support it.  */
/* #undef signed */

/* Define if you have unistd.h and it defines ioctl (like IRIX 5.3's does) */
/* #undef IOCTL_IN_UNISTD_H */

/* The set of builtin preprocessor symbols which are defined in the current 
 * environment.
 */
#define MACHINE_DEFINES  

/* Enabling this makes the the allocator run the GC with each allocation to
 * test for non-incubated objects being freed too soon.
 */
/* #undef GC_TORTURE */

/* Major version of GNU Smalltalk */
#define ST_MAJOR_VERSION 1

/* Minor version of GNU Smalltalk */
#define ST_MINOR_VERSION 6

/* Patch level version of GNU Smalltalk */
#define ST_EDIT_VERSION 2

/* Prefix (like "alpha" or "test" before the edit number) */
#define ST_EDIT_PREFIX ""

/* The current version of GNU Smalltalk */
#define VERSION 1.6.2

/* The number of bytes in a char *.  */
#define SIZEOF_CHAR_P 8

/* The number of bytes in a long.  */
#define SIZEOF_LONG 8

/* Define if you have the alarm function.  */
#define HAVE_ALARM 1

/* Define if you have the bcopy function.  */
#define HAVE_BCOPY 1

/* Define if you have the bzero function.  */
#define HAVE_BZERO 1

/* Define if you have the getdtablesize function.  */
#define HAVE_GETDTABLESIZE 1

/* Define if you have the gethostname function.  */
#define HAVE_GETHOSTNAME 1

/* Define if you have the gettimeofday function.  */
#define HAVE_GETTIMEOFDAY 1

/* Define if you have the getwd function.  */
#define HAVE_GETWD 1

/* Define if you have the ioctl function.  */
#define HAVE_IOCTL 1

/* Define if you have the ldexp function.  */
#define HAVE_LDEXP 1

/* Define if you have the memcpy function.  */
#define HAVE_MEMCPY 1

/* Define if you have the memset function.  */
#define HAVE_MEMSET 1

/* Define if you have the pipe function.  */
#define HAVE_PIPE 1

/* Define if you have the popen function.  */
#define HAVE_POPEN 1

/* Define if you have the strdup function.  */
#define HAVE_STRDUP 1

/* Define if you have the sighold function.  */
#define HAVE_SIGHOLD 1

/* Define if you have the sigsetmask function.  */
#define HAVE_SIGSETMASK 1

/* Define if you have the uname function.  */
#define HAVE_UNAME 1

/* Define if you have the waitpid function.  */
#define HAVE_WAITPID 1

/* Define if you have the <dirent.h> header file.  */
#define HAVE_DIRENT_H 1

/* Define if you have the <fcntl.h> header file.  */
#define HAVE_FCNTL_H 1

/* Define if you have the <io.h> header file.  */
/* #undef HAVE_IO_H */

/* Define if you have the <ndir.h> header file.  */
/* #undef HAVE_NDIR_H */

/* Define if you have the <stdarg.h> header file.  */
#define HAVE_STDARG_H 1

/* Define if you have the <stdlib.h> header file.  */
#define HAVE_STDLIB_H 1

/* Define if you have the <sys/dir.h> header file.  */
/* #undef HAVE_SYS_DIR_H */

/* Define if you have the <sys/ndir.h> header file.  */
/* #undef HAVE_SYS_NDIR_H */

/* Define if you have the <sys/time.h> header file.  */
#define HAVE_SYS_TIME_H 1

/* Define if you have the <sys/utsname.h> header file.  */
#define HAVE_SYS_UTSNAME_H 1

/* Define if you have the <sys/wait.h> header file.  */
#define HAVE_SYS_WAIT_H 1

/* Define if you have the <unistd.h> header file.  */
#define HAVE_UNISTD_H 1
