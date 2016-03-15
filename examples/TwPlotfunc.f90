! This program plots a function of two variables.  The function, called
! func_to_plot, is an external procedure at the end of the file.  
! This begins with the same module used in the modview example, followed by
! another module for plotting the function, called function_plotter.
! You might want to change default initial settings in modules modview and
! function_plotter.

! William F. Mitchell
! william.mitchell@nist.gov
! Mathematical and Computational Sciences Division
! National Institute of Standards and Technology
! August, 1999

!---------------------------------------------------------------------------

module view_modifier

! This module provides facilities to modify the view in an OpenGL window.
! The mouse buttons and keyboard arrow keys can be used to zoom, pan,
! rotate and change the scale.  A menu or submenu can be used to select which
! buttons perform which function and to reset the view to the initial settings.
! This is limited to one window.

! William F. Mitchell
! william.mitchell@nist.gov
! Mathematical and Computational Sciences Division
! National Institute of Standards and Technology
! April, 1998

! To use this module:
!
! 1) put a USE view_modifier statement in any program unit that calls a
!    procedure in this module
!
! 2) set the initial operation assignments, view and scale below the
!    "Initial configuration" comment below
!
! 3) call view_modifier_init after glutCreateWindow
!    This is a function that returns integer(kind=glcint) menuid.  The menuid
!    is the ID returned by glutCreateMenu.  You can either use the view_modifier
!    menu as your menu by calling glutAttachMenu immediately after
!    view_modifier_init, as in
!       menuid = view_modifier_init()
!       call glutAttachMenu(GLUT_RIGHT_BUTTON)
!    or by using the menuid to attach a submenu to your own menu, as in
!       call glutAddSubMenu("View Modifier",menuid)
!
! 4) in any callback functions that update the display, put
!       call reset_view
!    as the first executable statement
!
! Note that view_modifier_init sets the callback functions for glutMouseFunc,
! glutMotionFunc and glutSpecialFunc, so don't call these yourself
!
! The menu allows you to select what operation is attached to the left and
! middle mouse buttons and arrow keys, reset to the initial view, and quit.
! The right mouse button should be used for the menu.

use opengl_gl
use opengl_glu
use opengl_glut
use anttweakbar
implicit none
private
public :: view_modifier_init, reset_view

integer(kind=glcint), parameter :: ZOOM = 1, PAN = 2, ROTATE = 3, SCALEX = 4, &
                      SCALEY = 5, SCALEZ = 6
INTEGER(kind=c_int), SAVE, target :: RESET_OP = 10, ABOVE_OP = 11, QUIT_OP = 12
real(kind=gldouble), parameter :: PI = 3.141592653589793_gldouble

TYPE(twbar),SAVE :: view_bar

type, private :: cart2D ! 2D cartesian coordinates
   real(kind=gldouble) :: x, y
end type cart2D

type, private :: cart3D ! 3D cartesian coordinates
   real(kind=gldouble) :: x, y, z
end type cart3D

type, private :: sphere3D ! 3D spherical coordinates
   real(kind=gldouble) :: theta, phi, rho
end type sphere3D

type(cart2D), save :: angle
type(cart3D), save :: shift
REAL(kind=gldouble), SAVE, TARGET :: xscale_factor, yscale_factor, zscale_factor
logical, save :: moving_left=.false., moving_middle=.false.
type(cart2D), save :: begin_left, begin_middle

interface operator(+)
   module procedure cart3D_plus_cart3D
end interface
interface operator(-)
   module procedure cart3D_minus_cart3D
end interface

! ------- Initial configuration -------

! Set the initial operation performed by each button and the arrow keys.
! The operations are ZOOM, PAN, ROTATE, SCALEX, SCALEY, and SCALEZ

INTEGER(kind=c_int), SAVE, target :: &
  left_button_func = ROTATE, middle_button_func = ZOOM, arrow_key_func = PAN

! Set the initial view as the point you are looking at, the point you are
! looking from, and the scale factors

type(cart3D), parameter :: &
     init_lookat = cart3D(0.5_gldouble, 0.5_gldouble, 0.0_gldouble), &
   init_lookfrom = cart3D(5.0_gldouble, 10.0_gldouble, 2.5_gldouble)

real(kind=gldouble), parameter :: &
   init_xscale_factor = 1.0_gldouble, &
   init_yscale_factor = 1.0_gldouble, &
   init_zscale_factor = 1.0_gldouble

! -------- end of Initial configuration ------

contains

!          -------------
subroutine reset_to_init
!          -------------

! This resets the view to the initial configuration

type(sphere3D) :: slookfrom

slookfrom = cart2sphere(init_lookfrom-init_lookat)
angle%x = -180.0_gldouble*slookfrom%theta/PI - 90.0_gldouble
angle%y = -180.0_gldouble*slookfrom%phi/PI
shift%x = 0.0_gldouble
shift%y = 0.0_gldouble
shift%z = -slookfrom%rho
xscale_factor = init_xscale_factor
yscale_factor = init_yscale_factor
zscale_factor = init_zscale_factor

call glutPostRedisplay

return
end subroutine reset_to_init

!          ---------------
subroutine view_from_above
!          ---------------

! This sets the view to be from straight above

type(sphere3D) :: slookfrom

slookfrom = cart2sphere(cart3D(0.0,0.0,1.0))
angle%x = -180.0_gldouble*slookfrom%theta/PI
angle%y = -180.0_gldouble*slookfrom%phi/PI

call glutPostRedisplay

return
end subroutine view_from_above

!          ----------
subroutine reset_view
!          ----------

! This routine resets the view to the current orientation and scale

call glMatrixMode(GL_MODELVIEW)
call glPopMatrix
call glPushMatrix
call glTranslated(shift%x, shift%y, shift%z)
call glRotated(angle%x, 0.0_gldouble, 0.0_gldouble, 1.0_gldouble)
call glRotated(angle%y, cos(PI*angle%x/180.0_gldouble), &
               -sin(PI*angle%x/180.0_gldouble), 0.0_gldouble)
call glTranslated(-init_lookat%x, -init_lookat%y, -init_lookat%z)
call glScaled(xscale_factor,yscale_factor,zscale_factor)

return
end subroutine reset_view

!          -----
subroutine mouse(button, state, x, y) bind(c,name="")
!          -----
integer(kind=glcint), value :: button, state, x, y

! This gets called when a mouse button changes

  ! event consumed by tw
  if (tweventmousebuttonglut(button, state, x, y) /= 0) then
    CALL glutPostRedisplay()
    return
  endif

  if (button == GLUT_LEFT_BUTTON .and. state == GLUT_DOWN) then
    moving_left = .true.
    begin_left = cart2D(x,y)
  endif
  if (button == GLUT_LEFT_BUTTON .and. state == GLUT_UP) then
    moving_left = .false.
  endif
  if (button == GLUT_MIDDLE_BUTTON .and. state == GLUT_DOWN) then
    moving_middle = .true.
    begin_middle = cart2D(x,y)
  endif
  if (button == GLUT_MIDDLE_BUTTON .and. state == GLUT_UP) then
    moving_middle = .false.
  endif
end subroutine mouse

!          ------
subroutine motion(x, y) bind(c,name="")
!          ------
integer(kind=glcint), value :: x, y

! This gets called when the mouse moves

integer :: button_function
type(cart2D) :: begin
real(kind=gldouble) :: factor

! Determine and apply the button function

if (moving_left) then
   button_function = left_button_func
   begin = begin_left
else if(moving_middle) then
   button_function = middle_button_func
   begin = begin_middle
else
! event passed to tw
  if (tweventmousemotionglut(x, y) /= 0) then ! check!
    CALL glutPostRedisplay()
    return
  endif
end if

select case(button_function)
case (ZOOM)
   if (y < begin%y) then
      factor = 1.0_gldouble/(1.0_gldouble + .002_gldouble*(begin%y-y))
   else if (y > begin%y) then
      factor = 1.0_gldouble + .002_gldouble*(y-begin%y)
   else
      factor = 1.0_gldouble
   end if
   shift%z = factor*shift%z
case (PAN)
   shift%x = shift%x + .01*(x - begin%x)
   shift%y = shift%y - .01*(y - begin%y)
case (ROTATE)
   angle%x = angle%x + (x - begin%x)
   angle%y = angle%y + (y - begin%y)
case (SCALEX)
   if (y < begin%y) then
      factor = 1.0_gldouble + .002_gldouble*(begin%y-y)
   else if (y > begin%y) then
      factor = 1.0_gldouble/(1.0_gldouble + .002_gldouble*(y-begin%y))
   else
      factor = 1.0_gldouble
   end if
   xscale_factor = xscale_factor * factor
case (SCALEY)
   if (y < begin%y) then
      factor = 1.0_gldouble + .002_gldouble*(begin%y-y)
   else if (y > begin%y) then
      factor = 1.0_gldouble/(1.0_gldouble + .002_gldouble*(y-begin%y))
   else
      factor = 1.0_gldouble
   end if
   yscale_factor = yscale_factor * factor
case (SCALEZ)
   if (y < begin%y) then
      factor = 1.0_gldouble + .002_gldouble*(begin%y-y)
   else if (y > begin%y) then
      factor = 1.0_gldouble/(1.0_gldouble + .002_gldouble*(y-begin%y))
   else
      factor = 1.0_gldouble
   end if
   zscale_factor = zscale_factor * factor
end select

! update private variables and redisplay

if (moving_left) then
   begin_left = cart2D(x,y)
else if(moving_middle) then
   begin_middle = cart2D(x,y)
endif

if (moving_left .or. moving_middle) then
   call glutPostRedisplay
endif

return
end subroutine motion

SUBROUTINE passivemotion(x, y) BIND(c,name="")
!          ------
integer(kind=glcint), value :: x, y

! This gets called when the mouse moves without a button being pressed

! event passed to tw
if (tweventmousemotionglut(x, y) /= 0) THEN ! check!
  CALL glutPostRedisplay()
end if

END SUBROUTINE passivemotion

!          ------
subroutine arrows(key, x, y) bind(c,name="")
!          ------
integer(glcint), value :: key, x, y

! This routine handles the arrow key operations

real(kind=gldouble) :: factor

! event consumed by tw
if (tweventspecialglut(key, x, y) /= 0) then
  CALL glutPostRedisplay()
  return
endif

select case(arrow_key_func)
case(ZOOM)
   select case(key)
   case(GLUT_KEY_DOWN)
      factor = 1.0_gldouble + .02_gldouble
   case(GLUT_KEY_UP)
      factor = 1.0_gldouble/(1.0_gldouble + .02_gldouble)
   case default
      factor = 1.0_gldouble
   end select
   shift%z = factor*shift%z
case(PAN)
   select case(key)
   case(GLUT_KEY_LEFT)
      shift%x = shift%x - .02
   case(GLUT_KEY_RIGHT)
      shift%x = shift%x + .02
   case(GLUT_KEY_DOWN)
      shift%y = shift%y - .02
   case(GLUT_KEY_UP)
      shift%y = shift%y + .02
   end select
case(ROTATE)
   select case(key)
   case(GLUT_KEY_LEFT)
      angle%x = angle%x - 1.0_gldouble
   case(GLUT_KEY_RIGHT)
      angle%x = angle%x + 1.0_gldouble
   case(GLUT_KEY_DOWN)
      angle%y = angle%y + 1.0_gldouble
   case(GLUT_KEY_UP)
      angle%y = angle%y - 1.0_gldouble
   end select
case(SCALEX)
   select case(key)
   case(GLUT_KEY_DOWN)
      factor = 1.0_gldouble/(1.0_gldouble + .02_gldouble)
   case(GLUT_KEY_UP)
      factor = 1.0_gldouble + .02_gldouble
   case default
      factor = 1.0_gldouble
   end select
   xscale_factor = xscale_factor * factor
case(SCALEY)
   select case(key)
   case(GLUT_KEY_DOWN)
      factor = 1.0_gldouble/(1.0_gldouble + .02_gldouble)
   case(GLUT_KEY_UP)
      factor = 1.0_gldouble + .02_gldouble
   case default
      factor = 1.0_gldouble
   end select
   yscale_factor = yscale_factor * factor
case(SCALEZ)
   select case(key)
   case(GLUT_KEY_DOWN)
      factor = 1.0_gldouble/(1.0_gldouble + .02_gldouble)
   case(GLUT_KEY_UP)
      factor = 1.0_gldouble + .02_gldouble
   case default
      factor = 1.0_gldouble
   end select
   zscale_factor = zscale_factor * factor

end select
   
call glutPostRedisplay

return
end subroutine arrows

!          ------------
subroutine keyboard(key, x, y) bind(c,name="")
!          ------------
integer(glbyte), value :: key
integer(glcint), value :: x, y

character(kind=c_char) :: ckey

ckey = TRANSFER(key, ckey)
if (tweventkeyboardglut(ckey, x, y) /= 0) then
  CALL glutPostRedisplay()
  return
endif

end subroutine keyboard

SUBROUTINE resize(x, y) bind(c,name="")
INTEGER(KIND=glcint), value :: x, y

INTEGER :: ier

CALL glviewport(0, 0, x, y)
ier = twwindowsize(x, y)
CALL glutPostRedisplay()

END SUBROUTINE resize

SUBROUTINE quitprogram() BIND(c,name="")
integer :: ier

ier = twterminate()
STOP

END SUBROUTINE quitprogram

!          ------------
subroutine do_action(operation) bind(c,name="")
!          ------------
type(c_ptr), value :: operation

integer,pointer :: foperation

CALL C_F_POINTER(operation, foperation)

! This routine handles the first level entries in the menu

if (foperation == RESET_OP) then
   call reset_to_init()
else if (foperation == ABOVE_OP) then
   call view_from_above()
else if (foperation == QUIT_OP) then
  call quitprogram()
end if

return
end subroutine do_action

!        ------------------
subroutine view_modifier_init()
!        ------------------

! This initializes the view modifier variables and sets initial view.
! It should be called immediately after glutCreateWindow

! set the callback functions

call glutMouseFunc(mouse)
call glutMotionFunc(motion)
call glutpassivemotionfunc(passivemotion)
call glutKeyboardFunc(keyboard)
call glutSpecialFunc(arrows)
call glutReshapeFunc(resize)
call glutCloseFunc(quitprogram)

! create the menu
call view_modifier_twmenu()

! set the perspective

call glMatrixMode(GL_PROJECTION)
call gluPerspective(10.0_gldouble, 1.0_gldouble, 0.1_gldouble, 200.0_gldouble)

! set the initial view

call glMatrixMode(GL_MODELVIEW)
call glPushMatrix
call reset_to_init

end subroutine view_modifier_init


SUBROUTINE view_modifier_twmenu()
INTEGER :: action_list, ier

view_bar = twnewbar("view")
ier = twdefine("view label='3D view' size='180 220' position='0 0'")

action_list = twdefineenum("action_list", &
 (/ROTATE, ZOOM, PAN, SCALEX, SCALEY, SCALEZ/), &
 (/"rotate ","zoom   ","pan    ","scale x","scale y","scale z"/))
ier = twaddvarrw(view_bar, "button_left", action_list, C_LOC(left_button_func), &
 "label='Left button action' group=Actions &
 &help='Select the action associated with mouse left button.'")
ier = twaddvarrw(view_bar, "button_middle", action_list, C_LOC(middle_button_func), &
 "label='Middle button action' group=Actions &
 &help='Select the action associated with mouse middle button.'")
ier = twaddvarrw(view_bar, "arrow_keys", action_list, C_LOC(arrow_key_func), &
 "label='Arrow keys action' group=Actions &
 &help='Select the action associated with arrow keys.'")

ier = twaddvarrw(view_bar, "xscale_factor", TW_TYPE_DOUBLE, C_LOC(xscale_factor), &
 "label='X scale factor' group=Settings min=0.1 max=10. step=0.1 &
 &help='Set the scale factor on X axis.'")
ier = twaddvarrw(view_bar, "yscale_factor", TW_TYPE_DOUBLE, C_LOC(yscale_factor), &
 "label='Y scale factor' group=Settings min=0.1 max=10. step=0.1 &
 &help='Set the scale factor on Y axis.'")

ier = twaddbutton(view_bar, "view_from_above", do_action, C_LOC(ABOVE_OP), &
 "label='View from above' group=View help='Set view from above.'")
ier = twaddbutton(view_bar, "reset_view", do_action, C_LOC(RESET_OP), &
 "label='Reset view' group=View help='Reset view to initial state.'")
ier = twaddbutton(view_bar, "quit", do_action, C_LOC(QUIT_OP), &
 "label='Quit program' help='Quit program.'")

END SUBROUTINE view_modifier_twmenu

!        -----------
function sphere2cart(spoint) result(cpoint)
!        -----------
type(sphere3D), intent(in) :: spoint
type(cart3D) :: cpoint

! This converts a 3D point from spherical to cartesean coordinates

real(kind=gldouble) :: t,p,r

t=spoint%theta
p=spoint%phi
r=spoint%rho

cpoint%x = r*cos(t)*sin(p)
cpoint%y = r*sin(t)*sin(p)
cpoint%z = r*cos(p)

return
end function sphere2cart

!        -----------
function cart2sphere(cpoint) result(spoint)
!        -----------
type(cart3D), intent(in) :: cpoint
type(sphere3D) :: spoint

! This converts a 3D point from cartesean to spherical coordinates

real(kind=gldouble) :: x,y,z

x=cpoint%x
y=cpoint%y
z=cpoint%z

spoint%rho = sqrt(x*x+y*y+z*z)
if (x==0.0_gldouble .and. y==0.0_gldouble) then
   spoint%theta = 0.0_gldouble
else
   spoint%theta = atan2(y,x)
end if
if (spoint%rho == 0.0_gldouble) then
   spoint%phi = 0.0_gldouble
else
   spoint%phi = acos(z/spoint%rho)
endif

return
end function cart2sphere

!        ------------------
function cart3D_plus_cart3D(cart1,cart2) result(cart3)
!        ------------------
type(cart3D), intent(in) :: cart1, cart2
type(cart3D) :: cart3

! Compute the sum of two 3D cartesean points

cart3%x = cart1%x + cart2%x
cart3%y = cart1%y + cart2%y
cart3%z = cart1%z + cart2%z

return
end function cart3D_plus_cart3D

!        -------------------
function cart3D_minus_cart3D(cart1,cart2) result(cart3)
!        -------------------
type(cart3D), intent(in) :: cart1, cart2
type(cart3D) :: cart3

! Compute the difference of two 3D cartesean points

cart3%x = cart1%x - cart2%x
cart3%y = cart1%y - cart2%y
cart3%z = cart1%z - cart2%z

return
end function cart3D_minus_cart3D

end module view_modifier

!---------------------------------------------------------------------------

module function_plotter
use opengl_gl
use opengl_glut
use anttweakbar
use view_modifier
implicit none
private
public :: display, draw_func, function_plotter_init

! symbolic constants

integer, parameter :: surfgrid_toggle = 1, &
                      surfsolid_toggle = 2, &
                      contour_toggle = 3, &
                      quit_selected = 4

integer, parameter :: set_nx = 1, &
                      set_ny = 2, &
                      set_ncontour = 3, &
                      set_contour_val = 4, &
                      set_xrange = 5, &
                      set_yrange = 6, &
                      reset_params = 7

integer, save, target :: uniform_color = 1, &
                         rainbow_color = 2

INTEGER, SAVE, TARGET :: CONTOUR_COLOR_OP = 1, CONTOUR_UNIFORM_COLOR_OP = 2, &
 SURFACE_COLOR_OP = 3, SURFACE_UNIFORM_COLOR_OP  = 4, &
 X_INTERVAL_OP = 11, Y_INTERVAL_OP = 12, N_CONTOUR_OP = 13, &
 X_MIN_OP = 14, X_MAX_OP = 15, Y_MIN_OP = 16, Y_MAX_OP = 17, &
 GRID_TOGGLE_OP = 21, SURFACE_TOGGLE_OP = 22, CONTOUR_TOGGLE_OP = 23, &
 RESET_OP = 31

! Default initial settings

integer, parameter :: init_ngridx = 40, &
                      init_ngridy = 40, &
                      init_num_contour = 20, &
                      init_contour_color = 1, &
                      init_surface_color = 2

real(GLDOUBLE), parameter :: init_minx = 0.0_GLDOUBLE, &
                             init_maxx = 1.0_GLDOUBLE, &
                             init_miny = 0.0_GLDOUBLE, &
                             init_maxy = 1.0_GLDOUBLE

logical, parameter :: init_draw_surface_grid = .false., &
                      init_draw_surface_solid = .true., &
                      init_draw_contour = .true.

REAL(GLFLOAT), PARAMETER :: init_contour_uniform_color(4) = (/0.0,0.0,0.0,1.0/), &
 init_surface_uniform_color(4) = (/1.0,0.0,0.0,1.0/)

! Current settings
INTEGER, SAVE, target :: ngridx, ngridy, num_contour, contour_color, surface_color
REAL(GLFLOAT), SAVE, TARGET :: contour_uniform_color(4), surface_uniform_color(4)
REAL(GLDOUBLE) :: minx, maxx, miny, maxy,minz, maxz
LOGICAL :: draw_surface_grid, draw_surface_solid, draw_contour

TYPE(twbar),save :: plotter_bar

contains

subroutine display() bind(c)
integer :: i
! This gets called when the display needs to be redrawn

call reset_view

call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
call glCallList(1)
i = twdraw()
call glutSwapBuffers

return
end subroutine display

subroutine draw_func
real(GLDOUBLE) :: gridx(0:ngridx),gridy(0:ngridy),zval(0:ngridy,2)
integer :: i,j,k,cont
real(GLDOUBLE) :: x1,x2,x3,xt,y1,y2,y3,yt,z1,z2,z3,zt
real(GLDOUBLE) :: frac,xcross1,xcross2,ycross1,ycross2
real(GLDOUBLE) :: contour_value(num_contour)
real(GLFLOAT) :: color(4), normal(3), &
                 black(4) = (/0.0,0.0,0.0,1.0/)
real(GLDOUBLE), external :: func_to_plot

! prepare to make a new display list

call reset_view
call glDeleteLists(1_gluint, 1_glsizei)
call glNewList(1_gluint, gl_compile_and_execute)

! set the grid points

gridx = (/ ((minx + i*(maxx-minx)/ngridx),i=0,ngridx) /)
gridy = (/ ((miny + i*(maxy-miny)/ngridy),i=0,ngridy) /)

! if this is the first call and either rainbow coloring of a solid surface
! or contours are being drawn, minz and maxz need to be set

if (minz == 0.0_GLDOUBLE .and. maxz == 0.0_GLDOUBLE .and. &
    ( (draw_surface_solid .and. surface_color == rainbow_color) .or. &
      draw_contour ) ) then
   do i=0,ngridx
      do j=0,ngridy
         z1 = func_to_plot(gridx(i),gridy(j))
         minz = min(z1,minz)
         maxz = max(z1,maxz)
      end do
   end do
endif

! draw the solid surface

if (draw_surface_solid) then

   call glPolygonMode(gl_front_and_back, gl_fill)
   call glBegin(gl_triangles)

! set the color for a red or white surface.  For white, set the lighting
! such that there is uniform brightness.

   if (surface_color == uniform_color) then
      call glMaterialfv(gl_front_and_back,gl_ambient_and_diffuse, &
       surface_uniform_color)
!   elseif (surface_color == white_surface) then
! these create problems to tw:
!      call glDisable(gl_light0)
!      call glLightModelfv(gl_light_model_ambient, (/1.,1.,1.,1./))
!      call glMaterialfv(gl_front_and_back,gl_ambient_and_diffuse,white)
   endif

! compute the function values for the first line of points

   do j=0,ngridy
      zval(j,2) = func_to_plot(gridx(0),gridy(j))
   end do

! for each x grid interval...

   do i=1,ngridx

! copy left side function values from the right side of the previous interval

      zval(:,1) = zval(:,2)

! compute the function values for the right side of the interval

      do j=0,ngridy
         zval(j,2) = func_to_plot(gridx(i),gridy(j))
      end do

      minz = min(minz,minval(zval))
      maxz = max(maxz,maxval(zval))

! for each y grid interval ...

      do j=1,ngridy

! add two triangles to the display list
! Before each triangle, set the normal.  Before each vertex, set the
! color if we're coloring by height

         normal = normcrossprod((/gridx(i-1),gridx(i),gridx(i)/), &
                                (/gridy(j-1),gridy(j-1),gridy(j)/), &
                                (/zval(j-1,1),zval(j-1,2),zval(j,2)/))
         call glNormal3fv(normal)
         if (surface_color == rainbow_color) then
            call get_rainbow(zval(j-1,1),minz,maxz,color)
            call glMaterialfv(gl_front_and_back,gl_ambient_and_diffuse,color)
         endif
         call glvertex3d(gridx(i-1),gridy(j-1),zval(j-1,1))
         if (surface_color == rainbow_color) then
            call get_rainbow(zval(j-1,2),minz,maxz,color)
            call glMaterialfv(gl_front_and_back,gl_ambient_and_diffuse,color)
         endif
         call glvertex3d(gridx(i  ),gridy(j-1),zval(j-1,2))
         if (surface_color == rainbow_color) then
            call get_rainbow(zval(j,2),minz,maxz,color)
            call glMaterialfv(gl_front_and_back,gl_ambient_and_diffuse,color)
         endif
         call glvertex3d(gridx(i  ),gridy(j  ),zval(j  ,2))
         normal = normcrossprod((/gridx(i-1),gridx(i-1),gridx(i)/), &
                                (/gridy(j),gridy(j-1),gridy(j)/), &
                                (/zval(j,1),zval(j-1,1),zval(j,2)/))
         call glNormal3fv(normal)
         if (surface_color == rainbow_color) then
            call get_rainbow(zval(j,2),minz,maxz,color)
            call glMaterialfv(gl_front_and_back,gl_ambient_and_diffuse,color)
         endif
         call glvertex3d(gridx(i  ),gridy(j  ),zval(j  ,2))
         if (surface_color == rainbow_color) then
            call get_rainbow(zval(j-1,1),minz,maxz,color)
            call glMaterialfv(gl_front_and_back,gl_ambient_and_diffuse,color)
         endif
         call glvertex3d(gridx(i-1),gridy(j-1),zval(j-1,1))
         if (surface_color == rainbow_color) then
            call get_rainbow(zval(j,1),minz,maxz,color)
            call glMaterialfv(gl_front_and_back,gl_ambient_and_diffuse,color)
         endif
         call glvertex3d(gridx(i-1),gridy(j  ),zval(j  ,1))

      end do
   end do

! if the surface is white, reset the lighting conditions

   call glEnd

!   if (surface_color == white_surface) then
! these create problems to tw:
!      call glEnable(gl_light0)
!      call glLightModelfv(gl_light_model_ambient, (/.5,.5,.5,1./))
!   endif

   

endif ! draw_surface_solid

! draw the surface grid

if (draw_surface_grid) then

   call glPolygonMode(gl_front_and_back, gl_line)
   call glBegin(gl_triangles)

! draw surface grid in black

   call glMaterialfv(gl_front_and_back,gl_ambient_and_diffuse,black)

! compute the function values for the first line of points

   do j=0,ngridy
      zval(j,2) = func_to_plot(gridx(0),gridy(j))
   end do

! for each x grid interval...

   do i=1,ngridx

! copy left side function values from the right side of the previous interval

      zval(:,1) = zval(:,2)

! compute the function values for the right side of the interval

      do j=0,ngridy
         zval(j,2) = func_to_plot(gridx(i),gridy(j))
      end do

      minz = min(minz,minval(zval))
      maxz = max(maxz,maxval(zval))

! for each y grid interval ...

      do j=1,ngridy

! add two triangles to the display list

         call glvertex3d(gridx(i-1),gridy(j-1),zval(j-1,1))
         call glvertex3d(gridx(i  ),gridy(j-1),zval(j-1,2))
         call glvertex3d(gridx(i  ),gridy(j  ),zval(j  ,2))
         call glvertex3d(gridx(i  ),gridy(j  ),zval(j  ,2))
         call glvertex3d(gridx(i-1),gridy(j-1),zval(j-1,1))
         call glvertex3d(gridx(i-1),gridy(j  ),zval(j  ,1))

      end do
   end do

   call glEnd

endif ! draw_surface_grid

! draw the contour plot

if (draw_contour) then

   call glPolygonMode(gl_front_and_back, gl_line)
   call glBegin(gl_lines)
   call glNormal3fv((/0.0_glfloat, 0.0_glfloat, 1.0_glfloat/))

! draw the domain in black, also sets color to black for black_contour

   call glMaterialfv(gl_front_and_back,gl_ambient_and_diffuse,black)
   call glVertex3d(minx,miny,0.0_GLDOUBLE)
   call glVertex3d(maxx,miny,0.0_GLDOUBLE)
   call glVertex3d(maxx,miny,0.0_GLDOUBLE)
   call glVertex3d(maxx,maxy,0.0_GLDOUBLE)
   call glVertex3d(maxx,maxy,0.0_GLDOUBLE)
   call glVertex3d(minx,maxy,0.0_GLDOUBLE)
   call glVertex3d(minx,maxy,0.0_GLDOUBLE)
   call glVertex3d(minx,miny,0.0_GLDOUBLE)

! set the contour values

   do i=1,num_contour
      contour_value(i) = minz+(maxz-minz)*(i-1)/real(num_contour-1,GLDOUBLE)
   end do

! compute the function values for the first line of points

   do j=0,ngridy
      zval(j,2) = func_to_plot(gridx(0),gridy(j))
   end do

   IF (contour_color /= rainbow_color) THEN
     CALL glMaterialfv(gl_front_and_back,gl_ambient_and_diffuse, &
      contour_uniform_color)
   ENDIF

! for each x grid interval...

   do i=1,ngridx

! copy left side function values from the right side of the previous interval

      zval(:,1) = zval(:,2)

! compute the function values for the right side of the interval

      do j=0,ngridy
         zval(j,2) = func_to_plot(gridx(i),gridy(j))
      end do

      minz = min(minz,minval(zval))
      maxz = max(maxz,maxval(zval))

! for each y grid interval ...

      do j=1,ngridy

! for two triangles

         do k=1,2

! set the vertices

            if (k==1) then
               x1 = gridx(i-1); y1 = gridy(j-1); z1 = zval(j-1,1)
               x2 = gridx(i  ); y2 = gridy(j-1); z2 = zval(j-1,2)
               x3 = gridx(i  ); y3 = gridy(j  ); z3 = zval(j  ,2)
            else
               x1 = gridx(i-1); y1 = gridy(j-1); z1 = zval(j-1,1)
               x2 = gridx(i-1); y2 = gridy(j  ); z2 = zval(j  ,1)
               x3 = gridx(i  ); y3 = gridy(j  ); z3 = zval(j  ,2)
            endif

! order the vertices by z value

            xt = x1; yt = y1; zt = z1
            if (z2 < z1) then
               xt = x1; yt = y1; zt = z1
               if (z3 < z1) then
                  if (z2 < z3) then
                     x1 = x2; y1 = y2; z1 = z2
                     x2 = x3; y2 = y3; z2 = z3
                  else
                     x1 = x3; y1 = y3; z1 = z3
                  endif
                  x3 = xt; y3 = yt; z3 = zt
               else
                  x1 = x2; y1 = y2; z1 = z2
                  x2 = xt; y2 = yt; z2 = zt
               endif
            elseif (z3 < z1) then
               x1 = x3; y1 = y3; z1 = z3
               x3 = x2; y3 = y2; z3 = z2
               x2 = xt; y2 = yt; z2 = zt
            elseif (z3 < z2) then
               xt = x2; yt = y2; zt = z2
               x2 = x3; y2 = y3; z2 = z3
               x3 = xt; y3 = yt; z3 = zt
            endif

! if z1==z3, the triangle is horizontal and no contours pass through it

            if (z1==z3) cycle

! for each contour value

            do cont = 1,num_contour

! see if it passes through this triangle

               if (contour_value(cont) < z1) cycle
               if (contour_value(cont) > z3) exit

! set the color for contours colored by solution

               if (contour_color == rainbow_color) then
                  call get_rainbow(contour_value(cont),minz,maxz,color)
                  call glMaterialfv(gl_front_and_back,gl_ambient_and_diffuse, &
                                    color)
               endif

! see where it crosses the 1-3 edge

               frac = (contour_value(cont)-z1)/(z3-z1)
               xcross1 = (1.0_GLDOUBLE - frac)*x1 + frac*x3
               ycross1 = (1.0_GLDOUBLE - frac)*y1 + frac*y3

! see where it crosses one of the other edges

               if (contour_value(cont) == z2) then
                  xcross2 = x2
                  ycross2 = y2
               elseif (contour_value(cont) < z2) then
                  frac = (contour_value(cont)-z1)/(z2-z1)
                  xcross2 = (1.0_GLDOUBLE - frac)*x1 + frac*x2
                  ycross2 = (1.0_GLDOUBLE - frac)*y1 + frac*y2
               else
                  frac = (contour_value(cont)-z2)/(z3-z2)
                  xcross2 = (1.0_GLDOUBLE - frac)*x2 + frac*x3
                  ycross2 = (1.0_GLDOUBLE - frac)*y2 + frac*y3
               endif

! add the line segment to the display list

               call glVertex3d(xcross1,ycross1,0.0_GLDOUBLE)
               call glVertex3d(xcross2,ycross2,0.0_GLDOUBLE)

            end do
         end do
      end do
   end do

   call glEnd

endif ! draw_contour

! finish off display list

call glEndList
call glutPostRedisplay

end subroutine draw_func

subroutine get_rainbow(val,minval,maxval,c)
real(GLDOUBLE), intent(in) :: val,maxval,minval
real(GLFLOAT), intent(out) :: c(4)

real(GLFLOAT) :: f

if (maxval > minval) then
   f = (val-minval)/(maxval-minval)
else ! probably maxval==minval
   f = 0.5_glfloat
endif

if (f < .25) then
   c(1) = 0.0_glfloat
   c(2) = 4.0_glfloat * f
   c(3) = 1.0_glfloat
   c(4) = 1.0_glfloat
elseif (f < .5) then
   c(1) = 0.0_glfloat
   c(2) = 1.0_glfloat
   c(3) = 2.0_glfloat - 4.0_glfloat*f
   c(4) = 1.0_glfloat
elseif (f < .75) then
   c(1) = 4.0_glfloat * f - 2.0_glfloat
   c(2) = 1.0_glfloat
   c(3) = 0.0_glfloat
   c(4) = 1.0_glfloat
else
   c(1) = 1.0_glfloat
   c(2) = 4.0_glfloat - 4.0_glfloat*f
   c(3) = 0.0_glfloat
   c(4) = 1.0_glfloat
endif

end subroutine get_rainbow

function normcrossprod(x,y,z)
real(glfloat), dimension(3) :: normcrossprod
real(gldouble), dimension(3), intent(in) :: x,y,z
real(glfloat) :: t1(3),t2(3),norm
t1(1) = x(2) - x(1)
t1(2) = y(2) - y(1)
t1(3) = z(2) - z(1)
t2(1) = x(3) - x(1)
t2(2) = y(3) - y(1)
t2(3) = z(3) - z(1)
normcrossprod(1) = t1(2)*t2(3) - t1(3)*t2(2)
normcrossprod(2) = t1(3)*t2(1) - t1(1)*t2(3)
normcrossprod(3) = t1(1)*t2(2) - t1(2)*t2(1)
norm = sqrt(dot_product(normcrossprod,normcrossprod))
if (norm /= 0._glfloat) normcrossprod = normcrossprod/norm
end function normcrossprod

SUBROUTINE value_set(selection, operation) BIND(c,name="")
TYPE(c_ptr), VALUE :: selection, operation

INTEGER(kind=c_int),POINTER :: fselection, foperation
REAL(kind=c_double),POINTER :: fdselection
REAL(kind=c_float),POINTER :: ffselection(:)
LOGICAL,POINTER :: flselection

CALL C_F_POINTER(operation, foperation)

IF (foperation == SURFACE_COLOR_OP) THEN
  CALL C_F_POINTER(selection, fselection)
  surface_color = fselection
ELSE IF (foperation == SURFACE_UNIFORM_COLOR_OP) THEN
  CALL C_F_POINTER(selection, ffselection, SHAPE(surface_uniform_color))
  surface_uniform_color(:) = ffselection(:)
ELSE IF (foperation == CONTOUR_COLOR_OP) THEN
  CALL C_F_POINTER(selection, fselection)
  contour_color = fselection
ELSE IF (foperation == CONTOUR_UNIFORM_COLOR_OP) THEN
  CALL C_F_POINTER(selection, ffselection, SHAPE(contour_uniform_color))
  contour_uniform_color(:) = ffselection(:)
ELSE IF (foperation == X_INTERVAL_OP) THEN
  CALL C_F_POINTER(selection, fselection)
  ngridx = fselection
ELSE IF (foperation == Y_INTERVAL_OP) THEN
  CALL C_F_POINTER(selection, fselection)
  ngridy = fselection
ELSE IF (foperation == N_CONTOUR_OP) THEN
  CALL C_F_POINTER(selection, fselection)
  num_contour = fselection
ELSE IF (foperation == X_MIN_OP) THEN
  CALL C_F_POINTER(selection, fdselection)
  minx = MIN(fdselection, maxx)
ELSE IF (foperation == X_MAX_OP) THEN
  CALL C_F_POINTER(selection, fdselection)
  maxx = MAX(fdselection, minx)
ELSE IF (foperation == Y_MIN_OP) THEN
  CALL C_F_POINTER(selection, fdselection)
  miny = MIN(fdselection, maxy)
ELSE IF (foperation == Y_MAX_OP) THEN
  CALL C_F_POINTER(selection, fdselection)
  maxy = MAX(fdselection, miny)
ELSE IF (foperation == GRID_TOGGLE_OP) THEN
  CALL C_F_POINTER(selection, flselection)
  draw_surface_grid = flselection
ELSE IF (foperation == SURFACE_TOGGLE_OP) THEN
  CALL C_F_POINTER(selection, flselection)
  draw_surface_solid = flselection
ELSE IF (foperation == CONTOUR_TOGGLE_OP) THEN
  CALL C_F_POINTER(selection, flselection)
  draw_contour = flselection
ENDIF

call draw_func()

end subroutine value_set

SUBROUTINE value_get(selection, operation) BIND(c,name="")
TYPE(c_ptr), VALUE :: selection, operation

INTEGER(kind=c_int),POINTER :: fselection, foperation
REAL(kind=c_double),POINTER :: fdselection
REAL(kind=c_float),POINTER :: ffselection(:)
LOGICAL,POINTER :: flselection

CALL C_F_POINTER(operation, foperation)

IF (foperation == SURFACE_COLOR_OP) THEN
  CALL C_F_POINTER(selection, fselection)
  fselection = surface_color
ELSE IF (foperation == SURFACE_UNIFORM_COLOR_OP) THEN
  CALL C_F_POINTER(selection, ffselection, SHAPE(surface_uniform_color))
  ffselection(:) = surface_uniform_color(:)
ELSE IF (foperation == CONTOUR_COLOR_OP) THEN
  CALL C_F_POINTER(selection, fselection)
  fselection = contour_color
ELSE IF (foperation == CONTOUR_UNIFORM_COLOR_OP) THEN
  CALL C_F_POINTER(selection, ffselection, SHAPE(contour_uniform_color))
  ffselection(:) = contour_uniform_color(:)
ELSE IF (foperation == X_INTERVAL_OP) THEN
  CALL C_F_POINTER(selection, fselection)
  fselection = ngridx
ELSE IF (foperation == Y_INTERVAL_OP) THEN
  CALL C_F_POINTER(selection, fselection)
  fselection = ngridy
ELSE IF (foperation == N_CONTOUR_OP) THEN
  CALL C_F_POINTER(selection, fselection)
  fselection = num_contour
ELSE IF (foperation == X_MIN_OP) THEN
  CALL C_F_POINTER(selection, fdselection)
  fdselection = minx
ELSE IF (foperation == X_MAX_OP) THEN
  CALL C_F_POINTER(selection, fdselection)
  fdselection = maxx
ELSE IF (foperation == Y_MIN_OP) THEN
  CALL C_F_POINTER(selection, fdselection)
  fdselection = miny
ELSE IF (foperation == Y_MAX_OP) THEN
  CALL C_F_POINTER(selection, fdselection)
  fdselection = maxy
ELSE IF (foperation == GRID_TOGGLE_OP) THEN
  CALL C_F_POINTER(selection, flselection)
  flselection = draw_surface_grid
ELSE IF (foperation == SURFACE_TOGGLE_OP) THEN
  CALL C_F_POINTER(selection, flselection)
  flselection = draw_surface_solid
ELSE IF (foperation == CONTOUR_TOGGLE_OP) THEN
  CALL C_F_POINTER(selection, flselection)
  flselection = draw_contour
ENDIF

end subroutine value_get

SUBROUTINE do_action(operation) BIND(c,name="")
TYPE(c_ptr), VALUE :: operation

INTEGER(kind=c_int),POINTER :: foperation

CALL C_F_POINTER(operation, foperation)

IF (foperation == RESET_OP) THEN
  CALL value_reset()
  CALL draw_func()
ENDIF

END SUBROUTINE do_action

SUBROUTINE value_reset()
ngridx = init_ngridx
ngridy = init_ngridy
num_contour = init_num_contour
contour_color = init_contour_color
surface_color = init_surface_color
contour_uniform_color(:) = init_contour_uniform_color(:)
surface_uniform_color(:) = init_surface_uniform_color(:)
minx = init_minx
maxx = init_maxx
miny = init_miny
maxy = init_maxy
minz = 0.0_GLDOUBLE
maxz = 0.0_GLDOUBLE
draw_surface_grid = init_draw_surface_grid
draw_surface_solid = init_draw_surface_solid
draw_contour = init_draw_contour
END SUBROUTINE value_reset

SUBROUTINE function_plotter_init()
INTEGER :: color_list, ier

IF (twinit(TW_OPENGL, C_NULL_PTR) /= 1) THEN
  PRINT*,'Error in TwInit:',TRIM(twgetlasterror())
  STOP
ENDIF

ier = twdefine(" GLOBAL help='&
 &TwPlotfunc: AntTweakBar version of GLUT plotfunc program by Davide Cesari."//CHAR(10)//&
 "AntTweakBar: a graphical user interface for OpenGL and DirectX by Philippe Decaudin."//CHAR(10)//&
 "Plotfunc: a Fortran example for OpenGL/GLU/GLUT F90GL by William F. Mitchell."//CHAR(10)//&
 "F03GL: a Fortran 2003 interface to OpenGL/GLU/GLUT by A. Stone and A. Donev.'")

call value_reset()

plotter_bar = twnewbar("plotter")
ier = twdefine("plotter label='Function plot' size='180 300' position='0 220'")

color_list = twdefineenum("color_list", &
 (/uniform_color,rainbow_color/), (/"uniform","rainbow"/))
ier = twaddvarcb(plotter_bar, "contour_color", color_list, &
 value_set, value_get, C_LOC(CONTOUR_COLOR_OP), &
 "label='Contour color' group=Color help='Set contour color mode.'")
ier = twaddvarcb(plotter_bar, "contour_uniform_color", TW_TYPE_COLOR4F, &
 value_set, value_get, C_LOC(CONTOUR_UNIFORM_COLOR_OP), &
 "label='Contour color' group=Color help='Set contour color for uniform color.'")
ier = twaddvarcb(plotter_bar, "surface_color", color_list, &
 value_set, value_get, C_LOC(SURFACE_COLOR_OP), &
 "label='Surface color' group=Color help='Set surface color mode.'")
ier = twaddvarcb(plotter_bar, "surface_uniform_color", TW_TYPE_COLOR4F, &
 value_set, value_get, C_LOC(SURFACE_UNIFORM_COLOR_OP), &
 "label='Surface color' group=Color help='Set surface color for uniform color.'")

ier = twaddvarcb(plotter_bar, "x_interval", TW_TYPE_INT32, &
 value_set, value_get, C_LOC(X_INTERVAL_OP), &
 "label='X grid N' Group=Size min=10 max=1000 &
 &help='Set the number of grid intervals on X axis.'")
ier = twaddvarcb(plotter_bar, "y_interval", TW_TYPE_INT32, &
 value_set, value_get, C_LOC(Y_INTERVAL_OP), &
 "label='Y grid N' Group=Size min=10 max=1000 &
 &help='Set the number of grid intervals on Y axis.'")
ier = twaddvarcb(plotter_bar, "x_min", TW_TYPE_DOUBLE, &
 value_set, value_get, C_LOC(X_MIN_OP), &
 "label='X min' Group=Size min=-1000. max=1000. step=0.1 &
 &help='Set the minimum value on X axis.'")
ier = twaddvarcb(plotter_bar, "x_max", TW_TYPE_DOUBLE, &
 value_set, value_get, C_LOC(X_MAX_OP), &
 "label='X max' Group=Size min=-1000. max=1000. step=0.1 &
 &help='Set the maximum value on X axis.'")
ier = twaddvarcb(plotter_bar, "y_min", TW_TYPE_DOUBLE, &
 value_set, value_get, C_LOC(Y_MIN_OP), &
 "label='Y min' Group=Size min=-1000. max=1000. step=0.1 &
 &help='Set the minimum value on Y axis.'")
ier = twaddvarcb(plotter_bar, "y_max", TW_TYPE_DOUBLE, &
 value_set, value_get, C_LOC(Y_MAX_OP), &
 "label='Y max' Group=Size min=-1000. max=1000. step=0.1 &
 &help='Set the maximum value on Y axis.'")
ier = twaddvarcb(plotter_bar, "n_contour", TW_TYPE_INT32, &
 value_set, value_get, C_LOC(N_CONTOUR_OP), &
 "label='Contour N' Group=Size min=2 max=100 &
 &help='Set the number of contour levels.'")

ier = twaddvarcb(plotter_bar, "grid_toggle", TW_TYPE_BOOL32, &
 value_set, value_get, C_LOC(GRID_TOGGLE_OP), &
 "label='Surface grid' Group='Plot elements' &
 &help='Enable or disable plot of surface grid.'")
ier = twaddvarcb(plotter_bar, "surface_toggle", TW_TYPE_BOOL32, &
 value_set, value_get, C_LOC(SURFACE_TOGGLE_OP), &
 "label='Solid surface' Group='Plot elements' &
 &help='Enable or disable plot of solid surface.'")
ier = twaddvarcb(plotter_bar, "contour_toggle", TW_TYPE_BOOL32, &
 value_set, value_get, C_LOC(CONTOUR_TOGGLE_OP), &
 "label='Contour levels' Group='Plot elements' &
 &help='Enable or disable plot of contour levels.'")

ier = twaddbutton(plotter_bar, "reset_plot", do_action, C_LOC(RESET_OP), &
 "label='Reset plot' help='Reset plotting parameters to initial state.'")

END SUBROUTINE function_plotter_init

end module function_plotter

!---------------------------------------------------------------------------

program plot_func

use opengl_gl
use opengl_glut
!use anttweakbar
use view_modifier
use function_plotter
implicit none

integer :: winid

! Initializations

call glutInit
call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
call glutInitWindowSize(600_glcint,600_glcint)

! Create a window

winid = glutCreateWindow("Function plotter"//CHAR(0))

! initialize function_plotter and create function_plotter menu

call function_plotter_init()

! initialize view_modifier and create view_modifier menu

call view_modifier_init()

! Set the display callback

call glutDisplayFunc(display)

! set the lighting conditions

call glClearColor(0.9_glclampf, 0.9_glclampf, 0.9_glclampf, 1.0_glclampf)
call glLightfv(gl_light0, gl_diffuse, (/1.,1.,1.,1./))
call glLightfv(gl_light0, gl_position, (/1.5,-.5,2.,0.0/))
call glEnable(gl_lighting)
call glEnable(gl_light0)
call glLightModelfv(gl_light_model_ambient, (/.5,.5,.5,1./))
call glDepthFunc(gl_lequal)
call glEnable(gl_depth_test)

! Create the image

call draw_func()

! Let glut take over
! this is needed for anttweakbar to work at its best
CALL glutReshapeWindow(glutGet(GLUT_WINDOW_WIDTH), glutGet(GLUT_WINDOW_HEIGHT))
call glutMainLoop()

end program plot_func

!---------------------------------------------------------------------------

! The function to plot

function func_to_plot(x,y)
use opengl_gl
real(GLDOUBLE) :: func_to_plot
real(GLDOUBLE), intent(in) :: x,y

func_to_plot = 0.5_GLDOUBLE * ( &
       cos(0.3_GLDOUBLE*sqrt(80.0_GLDOUBLE*x)-16.0_GLDOUBLE*y/3.0_GLDOUBLE)* &
       cos(16.0_GLDOUBLE*x/3.0_GLDOUBLE) + x-y )

end function func_to_plot
