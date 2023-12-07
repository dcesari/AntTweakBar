dnl Process this file with autoconf to produce a configure script.

AC_INIT([AntTweakBar],[1.16],[])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([configure.ac])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_MACRO_DIR([m4])

AC_LANG([C])
dnl AC_C_BIGENDIAN
dnl AC_DEFINE_UNQUOTED(_UNIX
dnl AC_DEFINE_UNQUOTED(__PLACEMENT_NEW_INLINE

dnl Detect X Window system
AC_PATH_XTRA
LIBS="$LIBS $X_LIBS"
CFLAGS="$CFLAGS $X_CFLAGS"
CXXFLAGS="$CXXFLAGS $X_CFLAGS"

AC_CHECK_LIB([m],[sqrt],, AC_MSG_ERROR([C math library not found]),)
dnl Check for OpenGL headers and library
AC_CHECK_HEADERS([GL/gl.h OpenGL/gl.h])
AC_CHECK_LIB([GL],[glBegin],, AC_MSG_ERROR([OpenGL library not found]),)

AC_ARG_ENABLE(examples, AS_HELP_STRING([--disable-examples],[disable build of examples for AntTweakBar (examples require additional libraries to be installed)]),,
[enableval=yes])dnl default enable
enableexamples=$enableval

AC_ARG_ENABLE(fortran, AS_HELP_STRING([--enable-fortran],[enable build of Fortran 2003 bindings for AntTweakBar]),,
[enableval=no])dnl default disable
AM_CONDITIONAL(FORTRAN_BINDINGS, test "x$enableval" = "xyes")
enablefortran=$enableval

dnl Check for additional libraries for the examples and for Fortran
if test "x$enableexamples" = "xyes" -o "x$enablefortran" = "xyes"; then

AC_CHECK_LIB([glut], [glutInit],
 glut=yes
 GLUT_LIBS="-lglut"
 AC_SUBST(GLUT_LIBS)
 AC_DEFINE(HAVE_LIBGLUT, 1, [Enable glut]),
 AC_MSG_RESULT([glut library not found]), )

AC_CHECK_LIB([GLU], [gluPerspective],
 glu=yes
 GLU_LIBS="-lGLU"
 AC_SUBST(GLU_LIBS)
 AC_DEFINE(HAVE_LIBGLU, 1, [Enable GLU]),
 AC_MSG_RESULT([GLU library not found]), )

fi

if test "x$enableexamples" = "xyes"; then

AC_CHECK_LIB([SDL], [SDL_Init],
 sdl=yes
 SDL_LIBS="-lSDL"
 AC_SUBST(SDL_LIBS)
 AC_DEFINE(HAVE_LIBSDL, 1, [Enable SDL]),
 AC_MSG_RESULT([SDL library not found]), )

glfw=
AC_CHECK_LIB([glfw], [glfwInit],
 AC_CHECK_HEADER([glfw.h],glfw=yes,),,[-lXrandr])

if test "x$glfw" = "xyes"; then
 GLFW_LIBS="-lglfw -lXrandr"
 AC_SUBST(GLFW_LIBS)
 AC_DEFINE(HAVE_LIBGLFW, 1, [Enable glfw])
else
 AC_MSG_RESULT([glfw library and/or headers not found])
fi

fi

AM_CONDITIONAL(GLUT, test "x$glut" = "xyes" -a "x$glu" = "xyes" -a "x$enableexamples" = "xyes")
AM_CONDITIONAL(SDL, test "x$sdl" = "xyes" -a "x$glu" = "xyes" -a "x$enableexamples" = "xyes")
AM_CONDITIONAL(GLFW, test "x$glfw" = "xyes" -a "x$enableexamples" = "xyes")

dnl locate C++ compiler
AC_PROG_CXX


if test "x$enablefortran" = "xyes"; then
dnl Locate Fortran 90 compiler among these if not specified
AC_PROG_FC([gfortran g95 pgf90 ifort xlf90 f90])
F77=$FC
FFLAGS=$FCFLAGS

AC_LANG_PUSH(Fortran)dnl Do the following checks in Fortran
AC_FC_SRCEXT(f90)dnl and use .f90 extension

dnl check for Fortran 2003 ISO_C_BINDING intrinsic module
AC_COMPILE_IFELSE(
[AC_LANG_PROGRAM(,[[       USE, INTRINSIC :: iso_c_binding]])],
                          [AC_MSG_RESULT([Fortran 2003 C bindings found!])],
                          [AC_MSG_ERROR([Fortran 2003 C bindings not found!
Sorry, the Fortran interface to AntTweakBar requires a fortran compiler
with at least the ISO_C_BINDING module capabilities of Fortran 2003 standard]
)])

dnl check for F03GL Fortran interface to OpenGL to be installed
SAVELIBS=$LIBS
LIBS="-lf03gl $GLUT_LIBS $GLU_LIBS $LIBS"
AC_LINK_IFELSE(
[AC_LANG_PROGRAM(,[[       USE opengl_glut]])],
                          [AC_MSG_RESULT([F03GL found!])],
                          [AC_MSG_ERROR([F03GL not found!
Please check whether additional FCFLAGS or LDFLAGS options are needed
for finding F03GL modules and libraries]
)])
LIBS=$SAVELIBS
F03GL_LIBS=-lf03gl
AC_SUBST(F03GL_LIBS)
FCLIBS=-lgfortran
AC_SUBST(FCLIBS)

AC_LANG_POP()dnl back to C

else

dnl hack for libtool, better idea?
FC=$CC
FCFLAGS=$CFLAGS

fi

dnl F77=$FC
dnl FFLAGS=$FCFLAGS

dnl check for debug mode
AC_ARG_ENABLE(debug-mode, AS_HELP_STRING([--enable-debug-mode],[enable debug mode]), , [enableval=no])dnl default disable
if test "x$enableval" = "xyes"; then
CFLAGS="$CFLAGS -g"
CXXFLAGS="$CXXFLAGS -g"
FCFLAGS="$FCFLAGS -g"
AC_DEFINE(_DEBUG, 1, [Enable debugging mode])
else
CFLAGS="$CFLAGS -O"
CXXFLAGS="$CXXFLAGS -O"
FCFLAGS="$FCFLAGS -O"
fi
AM_CONDITIONAL(DEBUG, test "x$enableval" = "xyes")

LT_INIT

pkgdocdir='${datadir}/doc/${PACKAGE}'
AC_SUBST(pkgdocdir)

AC_CONFIG_FILES([
Makefile
src/Makefile
include/Makefile
examples/Makefile
])

AC_OUTPUT
