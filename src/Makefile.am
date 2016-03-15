lib_LTLIBRARIES = libAntTweakBar.la

libAntTweakBar_la_SOURCES = TwColors.cpp TwFonts.cpp TwOpenGL.cpp TwBar.cpp \
 TwMgr.cpp TwOpenGLCore.cpp TwPrecomp.cpp LoadOGLCore.cpp LoadOGL.cpp TwEventGLFW.c TwEventGLUT.c \
 TwEventSDL.c TwEventSDL12.c TwEventSDL13.c

if FORTRAN_BINDINGS
libAntTweakBar_la_SOURCES += TwFortran.f90
endif

AM_CFLAGS = -D_UNIX -D__PLACEMENT_NEW_INLINE -I../include
AM_CXXFLAGS = -D_UNIX -D__PLACEMENT_NEW_INLINE -I../include
AM_FCFLAGS =
if DEBUG
AM_CFLAGS += -D_DEBUG
AM_CXXFLAGS += -D_DEBUG
AM_FCFLAGS += -D_DEBUG
endif

noinst_HEADERS = AntPerfTimer.h d3d10vs2003.h \
 LoadOGLCore.h LoadOGL.h MiniGLFW.h MiniGLUT.h \
 MiniSDL12.h MiniSDL13.h MiniSFML16.h resource.h \
 TwBar.h TwColors.h TwDirect3D9.h TwDirect3D10.h TwDirect3D11.h \
 TwFonts.h TwGraph.h TwMgr.h TwOpenGLCore.h TwOpenGL.h TwPrecomp.h

if FORTRAN_BINDINGS
nodist_include_HEADERS = anttweakbar.mod
libAntTweakBar_la_LIBADD = $(F03GL_LIBS) $(FCLIBS)
endif

# these are not required on Unix, keep them in package anyway
EXTRA_DIST = TwDirect3D9.cpp TwDirect3D10.cpp TwDirect3D11.cpp TwDirect3D11.hlsl \
 TwEventSFML.cpp TwEventWin.c \
 AntTweakBar.rc AntTweakBar.sln AntTweakBar.vcproj Readme.txt

# add to package files on subdir res/
EXTRA_DIST += res/cur00000.cur res/cur00001.cur res/cur00002.cur	\
 res/cur00003.cur res/cur00004.cur res/cur00005.cur res/cur00006.cur	\
 res/cur00007.cur res/cur00008.cur res/cur00009.cur res/cur00010.cur	\
 res/cur00011.cur res/cur00012.cur res/cur00013.cur res/curs00.pbm	\
 res/curs01.pbm res/curs02.pbm res/curs03.pbm res/curs04.pbm		\
 res/curs05.pbm res/curs06.pbm res/curs07.pbm res/curs08.pbm		\
 res/curs09.pbm res/curs10.pbm res/curs11.pbm res/curs12.pbm		\
 res/curs13.pbm res/FontChars.txt res/FontFixed1.pgm			\
 res/FontLargeAA.pgm res/FontNormalAA.pgm res/FontNormal.pgm		\
 res/FontSmall.pgm res/mask00.pbm res/mask01.pbm res/mask02.pbm		\
 res/mask03.pbm res/mask04.pbm res/mask05.pbm res/mask06.pbm		\
 res/mask07.pbm res/mask08.pbm res/mask09.pbm res/mask10.pbm		\
 res/mask11.pbm res/mask12.pbm res/mask13.pbm res/TwXCursors.h

# GLCore sources
EXTRA_DIST += LoadOGLCore.cpp TwOpenGLCore.cpp

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod


