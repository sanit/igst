# Generated automatically from Makefile.in by configure.
# Generated automatically from Makefile.in by configure.
# Library Makefile for GNU Smalltalk
# Copyright 1990, 91, 92, 94, 95, 99 Free Software Foundation, Inc.
# Written by Steve Byrne

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

# $Revision: 1.6.2$

# OS/2 doesn't like this line
SHELL = /bin/sh
# .SUFFIXES : .obj .h .c   # OS/2 w/gnu make needs this

#### Start of system configuration section. ####

srcdir = /Users/Sanit/Temp/smalltalk-1.6.2/lib
VPATH = /Users/Sanit/Temp/smalltalk-1.6.2/lib

PROGS = gst

# Last Validated: 2/23/99
MAINTAINER = 'bonzini@gnu.org'

# Older makes don't set $(MAKE), so we set it for them.


# Programs - Use cp if you don't have install.
INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_INFO = install-info
MAKEINFO = makeinfo
CC = gcc
AR = ar
RANLIB = ranlib
BISON = bison


# Platform-dependent definitions
PLATFORM=unix
OBJ=o
LIB=a
RM = rm -f


# other definitions
ALLOCA = 
DLD_MODULE=dld_none
GST_TAB = gst.tab
DEFS = -DHAVE_CONFIG_H -DIMAGE_PATH="\"$(IMAGE_PATH)\"" -DKERNEL_PATH="\"$(KERNEL_PATH)\""
LIBS =  -lm -lreadline
MAKE_LIBRARY_COMMAND = cd lib; $(MAKE) $(TARGET) TARGET=$(TARGET); cd ..
USER_OBJECTS = 


# These control the distribution version
VERSION= 1.6.2
ST_MAJOR_VERSION = 1
ST_MINOR_VERSION = 6
ST_EDIT_VERSION  = 2
ST_EDIT_PREFIX   = 


# Program flags
OPT_FLAG = -O3
CDEBUG = -g
INCLUDES= -I$(srcdir)/.. -I$(srcdir) $(USER_INCLUDES)
CPPFLAGS = 
CFLAGS =  # $(INCLUDES)
LDFLAGS =  $(USER_LDFLAGS)
X_CFLAGS = 
TARGET = all
GCC_EXTRA_FLAGS = -Wall -Wno-parentheses -Wno-switch 
# ICC_EXTRA_FLAGS = /c /Fo$@ /D__EXTENDED__  # flags for ICC


# Paths given by autoconf
prefix = /usr/local
exec_prefix = ${prefix}
bindir = ${exec_prefix}/bin
libdir = ${exec_prefix}/lib
datadir = ${prefix}/share
mandir = ${prefix}/man
infodir = ${prefix}/info
includedir = ${prefix}/include
oldincludedir = /usr/include
X_LIBS = 
X_EXTRA_LIBS = 
X_PRE_LIBS = 
LDPATH =  


# Other paths we use
smlibdir = $(libdir)
stdir = $(datadir)/smalltalk
IMAGE_PATH=$(stdir)
KERNEL_PATH=$(IMAGE_PATH)/kernel

#### End of system configuration section. ####

MDEFINES = bindir='$(bindir)' libdir='$(libdir)' mandir='$(mandir)' \
       INSTALL='$(INSTALL)' INSTALLDATA='$(INSTALLDATA)' \
       LIBS='$(LIBS)' LDPATH='$(LDPATH)' LIBPROGS='$(LIBPROGS)' \
       MAKE='$(MAKE)' \
       AR='$(AR)' RANLIB='$(RANLIB)' \
       BISON='$(BISON)' CFLAGS='$(CFLAGS)' LDFLAGS='$(LDFLAGS)' \
       CC='$(CC)' INCLUDES='$(INCLUDES)' DEFS='$(DEFS)' CDEBUG='$(CDEBUG)' \
       GCC_EXTRA_FLAGS='$(GCC_EXTRA_FLAGS)' GST_TAB='$(GST_TAB)' RM='$(RM)'


GENERIC_CC = $(CC) -c $(CPPFLAGS) $(DEFS) $(INCLUDES) $(GCC_EXTRA_FLAGS) $(CFLAGS) $(CDEBUG) 
# here's the OS/2 definition
# GENERIC_CC = $(CC) -c $(CPPFLAGS) $(DEFS) $(INCLUDES) $(GCC_EXTRA_FLAGS) $(CFLAGS) $(ICC_EXTRA_FLAGS) $(CDEBUG) 

.c.$(OBJ): 
	$(GENERIC_CC) $<



.PHONY : mostlyclean clean distclean maintainer-clean all

all: libgst.$(LIB)


OBJECTS = lib.$(OBJ)	$(GST_TAB).$(OBJ)	lex.$(OBJ)	str.$(OBJ)    \
	tree.$(OBJ)	byte.$(OBJ)		interp.$(OBJ)   alloc.$(OBJ)  \
	comp.$(OBJ) 	sym.$(OBJ)		dict.$(OBJ)	oop.$(OBJ)    \
	save.$(OBJ)	cint.$(OBJ) 		sysdep.$(OBJ)	callin.$(OBJ) \
	cfuncs.$(OBJ)	$(LIBOBJS)		$(USER_OBJS) 	$(ALLOCA)     \
	dld_gst.$(OBJ)  $(DLD_MODULE).$(OBJ)


LIBOBJECTS = +lib + $(GST_TAB) + lex + str \
	tree + byte + interp + comp + \
	sym + dict + oop + save + cint + \
	sysdep + callin + cfuncs \
	+dld_gst       +$(DLD_MODULE)


INLINES = oop.inl dict.inl interp.inl prims.inl

SOURCES = lib.c	    $(GST_TAB).c  lex.c       str.c      alloc.c   \
	tree.c      byte.c        interp.c    comp.c 		   \
	sym.c       dict.c   	  oop.c       save.c     cint.c    \
	sysdep.c    callin.c 	  cfuncs.c    strdup.c   putenv.c  \
	dld_gst.c   dld_shlib.c	  dld_win32.c dld_hpux.c dld_gnu.c \
	dld_ltdl.c  dld_none.c

HEADERS = gst.h lib.h $(GST_TAB).h lex.h str.h \
	tree.h byte.h interp.h comp.h \
	sym.h dict.h oop.h save.h cint.h \
	sysdep.h callin.h gstpub.h alloc.h dld_gst.h

DISTFILES = $(SOURCES) $(HEADERS) $(INLINES) gst.y Makefile.in alloca.c


# deliberately not using $(LIB) here
libgst.a: $(OBJECTS)
	$(RM) $@
	$(AR) cr $@ $(OBJECTS)
	-$(RANLIB) $@

# OS/2 definition
libgst.lib: $(OBJECTS)
	-$(RM) $@
	lib $@ $(LIBOBJECTS);
	-$(RANLIB) $@


optimize:
	$(MAKE) CFLAGS=" $(OPT_FLAG) $(CFLAGS)" CDEBUG=


$(GST_TAB).h $(GST_TAB).c: gst.y
	$(BISON) -dv gst.y

# These depend on the oop table size 
save.$(OBJ) oop.$(OBJ) lib.$(OBJ) sym.$(OBJ): oop.h

gst.h: $(srcdir)/../gstconf.h
	touch gst.h

gstpub.h: gst.h
	touch gstpub.h

$(DLD_MODULE).$(OBJ): $(DLD_MODULE).c dld_gst.h

lib.$(OBJ): lib.c alloc.h gst.h comp.h \
 lex.h dict.h dict.inl save.h \
 lib.h cint.h callin.h sysdep.h dld_gst.h
 
$(GST_TAB).$(OBJ): $(GST_TAB).c gst.h \
 sym.h tree.h dict.h dict.inl interp.h \
 interp.inl comp.h oop.h oop.inl lex.h 

lex.$(OBJ): lex.c gst.h \
 lex.h $(GST_TAB).h str.h dict.h dict.inl \
 sym.h interp.h interp.inl comp.h oop.h oop.inl \
 tree.h sysdep.h

alloca.$(OBJ): alloca.c $(srcdir)/../gstconf.h

putenv.$(OBJ): putenv.c alloc.h

str.$(OBJ): str.c gst.h alloc.h
 
strdup.$(OBJ): str.c gst.h alloc.h
 
tree.$(OBJ): tree.c alloc.h gst.h \
 
byte.$(OBJ): byte.c alloc.h gst.h \
 interp.inl comp.h oop.h oop.inl 
 
interp.$(OBJ): interp.c alloc.h gst.h \
 lex.h dict.h dict.inl sym.h lib.h byte.h 
 
comp.$(OBJ): comp.c alloc.h gst.h \
 interp.h interp.inl oop.h oop.inl lex.h 
 
sym.$(OBJ): sym.c gst.h alloc.h \
 interp.h interp.inl comp.h 
 
dict.$(OBJ): dict.c alloc.h gst.h \
 comp.h oop.h oop.inl lex.h
 
oop.$(OBJ): oop.c gst.h alloc.h \
 interp.h interp.inl comp.h save.h callin.h
 
save.$(OBJ): save.c gst.h \
 oop.inl lex.h dict.h dict.inl sym.h
 
cint.$(OBJ): cint.c alloc.h gst.h \
 lex.h dict.h dict.inl sym.h dld_gst.h
 
sysdep.$(OBJ): sysdep.c alloc.h gst.h 
 
callin.$(OBJ): callin.c alloc.h gst.h \
 oop.inl lex.h dict.h dict.inl sym.h
 
cfuncs.$(OBJ): cfuncs.c gst.h

# ci_version should be set on the command line
ci_r:
	ci -l -mVersioning "-N$(ci_version)" RCS/*

profile:
	$(MAKE) CFLAGS='$(CFLAGS) -pg -Dprofiling'


TAGS: $(SOURCES)
	etags $(SOURCES)

tags: $(SOURCES)
	ctags $(SOURCES)

install: all 
	$(INSTALL_DATA) libgst.$(LIB) $(smlibdir)/libgst.$(LIB)
	$(INSTALL_DATA) gst.h $(includedir)/gst.h
	$(INSTALL_DATA) gstpub.h $(includedir)/gstpub.h
	$(INSTALL_DATA) gstconf.h $(includedir)/gstconf.h

uninstall:


ls:
	@echo $(DISTFILES)

mostlyclean:
	$(RM) *.$(OBJ) core gst.output libgst.$(LIB)

clean: mostlyclean


distclean: clean
	$(RM) Makefile tags TAGS

maintainer-clean:
	@echo 'This command is intended for maintainers to use; it'
	@echo 'deletes files that may need special tools to rebuild.'
	$(MAKE) do-maintainer-clean

do-maintainer-clean: distclean
	$(RM) $(GST_TAB).*

# Tell versions [3.59, 3.63) of GNU make not to export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
