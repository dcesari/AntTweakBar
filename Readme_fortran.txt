--- AntTweakBar development library ---


This package includes also a Fortran interface to the AntTweakBar
library, to enable the build of the interface on Unix-like systems,
please use configure with the --with-fortran option. The F03GL library
(Fortran interface to OpenGL) must be installed in order to compile
the Fortran interface to AntTweakBar.

You can download f03gl with autotools support at:

https://github.com/dcesari/f03gl

or at the original site, without autotools support:

http://www-stone.ch.cam.ac.uk/pub/f03gl/index.xhtml

The AntTweakBar Fortran API is very similar to the C one, please refer
to examples/TwPlotfunc.f90 for an almost complete example of Fortran
application using AntTweakBar.

Davide Cesari - 2016/03/15
