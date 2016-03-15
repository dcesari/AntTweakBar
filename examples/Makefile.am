AM_CFLAGS = -I../include
AM_CXXFLAGS = -I../include
AM_FCFLAGS = -I../src
if DEBUG
AM_CFLAGS += -D_DEBUG
AM_CXXFLAGS += -D_DEBUG
AM_FCFLAGS += -D_DEBUG
endif

noinst_PROGRAMS =

if GLUT
noinst_PROGRAMS += TwSimpleGLUT TwDualGLUT

TwSimpleGLUT_SOURCES = TwSimpleGLUT.c
TwDualGLUT_SOURCES = TwDualGLUT.c
TwSimpleGLUT_LDADD = ../src/libAntTweakBar.la $(GLUT_LIBS) $(GLU_LIBS)
TwDualGLUT_LDADD = ../src/libAntTweakBar.la $(GLUT_LIBS) $(GLU_LIBS)

if FORTRAN_BINDINGS
noinst_PROGRAMS += TwPlotfunc

TwPlotfunc_SOURCES = TwPlotfunc.f90
TwPlotfunc_LDADD = ../src/libAntTweakBar.la $(F03GL_LIBS) $(GLUT_LIBS) $(GLU_LIBS)
endif
endif

if SDL
noinst_PROGRAMS += TwSimpleSDL

TwSimpleSDL_SOURCES = TwSimpleSDL.c
TwSimpleSDL_LDADD = ../src/libAntTweakBar.la $(SDL_LIBS) $(GLU_LIBS)
endif

if GLFW
noinst_PROGRAMS += TwSimpleGLFW TwAdvanced1 TwString

TwSimpleGLFW_SOURCES = TwSimpleGLFW.c
TwAdvanced1_SOURCES = TwAdvanced1.cpp
TwString_SOURCES = TwString.cpp
TwSimpleGLFW_LDADD = ../src/libAntTweakBar.la $(GLFW_LIBS) $(GLU_LIBS)
TwAdvanced1_LDADD = ../src/libAntTweakBar.la $(GLFW_LIBS) $(GLU_LIBS)
TwString_LDADD = ../src/libAntTweakBar.la $(GLFW_LIBS) $(GLU_LIBS)
endif

pkgdoc_DATA = TwSimpleGLUT.c TwPlotfunc.f90 TwSimpleSDL.c TwSimpleGLFW.c TwAdvanced1.cpp TwString.cpp

EXTRA_DIST = Readme.txt \
 TwSimpleDX9.cpp TwSimpleDX10.cpp TwSimpleDX11.cpp TwSimpleSFML.cpp \
 d3d10vs2003.h glfw.h TwSimpleDX11.hlsl \
 TwAdvanced1.vcproj TwSimpleDX10.vcproj TwSimpleGLUT.vcproj \
 TwCopyDLL.vcproj TwSimpleDX11.vcproj TwSimpleSDL.vcproj \
 TwDualGLUT.vcproj TwSimpleDX9.vcproj TwSimpleSFML.vcproj \
 TwSimpleGLFW.vcproj TwString.vcproj

# GLCore sources
EXTRA_DIST += TwGLCoreSDL.c TwGLCoreSDL.vcproj

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
