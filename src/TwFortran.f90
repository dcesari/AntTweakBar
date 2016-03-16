! This module gives access to most of AntTweakBar API from Fortran 2003
MODULE anttweakbar
USE, INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE

INTEGER, PARAMETER :: &
 TW_TYPE_UNDEF   = 0, &
 TW_TYPE_BOOL8   = 2, &
 TW_TYPE_BOOL16  = 3, &
 TW_TYPE_BOOL32  = 4, &
 TW_TYPE_CHAR    = 5, &
 TW_TYPE_INT8    = 6, &
! TW_TYPE_UINT8   = 7, &
 TW_TYPE_INT16   = 8, &
! TW_TYPE_UINT16  = 9, &
 TW_TYPE_INT32   =10, &
! TW_TYPE_UINT32  =11, &
 TW_TYPE_FLOAT   =12, &
 TW_TYPE_DOUBLE  =13, &
 TW_TYPE_COLOR32 =14, & !32 bits color. Order is RGBA IF API is OpenGL or Direct3D10, and inversed IF API is Direct3D9 (can be modified by defining 'colorOrder=...', see doc)
 TW_TYPE_COLOR3F =15, & ! 3 floats color. Order is RGB.
 TW_TYPE_COLOR4F =16, & ! 4 floats color. Order is RGBA.
! TW_TYPE_CDSTRING=17, & ! Null-terminated C DYNAMIC String (POINTER to an array of char dynamically allocated WITH malloc/realloc/strdup)
 TW_TYPE_QUAT4F  =19, & ! 4 floats encoding a quaternion {qx,qy,qz,qs}
 TW_TYPE_QUAT4D  =20, & ! 4 doubles encoding a quaternion {qx,qy,qz,qs}
 TW_TYPE_DIR3F   =21, & ! direction vector represented by 3 floats
 TW_TYPE_DIR3D   =22    ! direction vector represented by 3 doubles

! For twinit
INTEGER, PARAMETER :: &
 TW_OPENGL           = 1, &
 TW_DIRECT3D9        = 2, &
 TW_DIRECT3D10       = 3

! For twsetparam
INTEGER, PARAMETER :: &
 TW_PARAM_INT32      = 0, &
 TW_PARAM_FLOAT      = 1, &
 TW_PARAM_DOUBLE     = 2, &
 TW_PARAM_CSTRING    = 3

TYPE, BIND(C) :: TwBar
  TYPE(c_ptr) :: ptr
END TYPE TwBar

TYPE, BIND(C) :: CTwEnumVal
  INTEGER(kind=c_int) :: value
  TYPE(c_ptr) :: label
END TYPE CTwEnumVal


INTERFACE
  FUNCTION twaddbutton_c(bar, name, callback, clientdata, def) &
   BIND(C,NAME="TwAddButton")
  IMPORT
  TYPE(twbar),VALUE :: bar
  CHARACTER(kind=c_char),INTENT(IN) :: name(*)
  TYPE(c_funptr),VALUE :: callback
  TYPE(c_ptr),VALUE :: clientdata
  CHARACTER(kind=c_char),INTENT(IN) :: def(*)
  INTEGER(kind=c_int) :: twaddbutton_c
  END FUNCTION twaddbutton_c
END INTERFACE


INTERFACE
  FUNCTION twaddseparator_c(bar, name, def) BIND(C,NAME="TwAddSeparator")
  IMPORT
  TYPE(twbar),VALUE :: bar
  CHARACTER(kind=c_char),INTENT(IN) :: name(*)
  CHARACTER(kind=c_char),INTENT(IN) :: def(*)
  INTEGER(kind=c_int) :: twaddseparator_c
  END FUNCTION twaddseparator_c
END INTERFACE


INTERFACE
  FUNCTION twaddvarcb_c(bar, name, type, setcallback, getcallback, &
   clientdata, def) BIND(C,NAME="TwAddVarCB")
  IMPORT
  TYPE(twbar),VALUE :: bar
  CHARACTER(kind=c_char),INTENT(IN) :: name(*)
  INTEGER(kind=c_int),VALUE :: type
  TYPE(c_funptr),VALUE :: setcallback
  TYPE(c_funptr),VALUE :: getcallback
  TYPE(c_ptr),VALUE :: clientdata
  CHARACTER(kind=c_char),INTENT(IN) :: def(*)
  INTEGER(kind=c_int) :: twaddvarcb_c
  END FUNCTION twaddvarcb_c
END INTERFACE


INTERFACE
  FUNCTION twaddvarro_c(bar, name, type, var, def) BIND(C,NAME="TwAddVarRO")
  IMPORT
  TYPE(twbar),VALUE :: bar
  CHARACTER(kind=c_char),INTENT(IN) :: name(*)
  INTEGER(kind=c_int),VALUE :: type
  TYPE(c_ptr),VALUE :: var
  CHARACTER(kind=c_char),INTENT(IN) :: def(*)
  INTEGER(kind=c_int) :: twaddvarro_c
  END FUNCTION twaddvarro_c
END INTERFACE


INTERFACE
  FUNCTION twaddvarrw_c(bar, name, type, var, def) BIND(C,NAME="TwAddVarRW")
  IMPORT
  TYPE(twbar),VALUE :: bar
  CHARACTER(kind=c_char),INTENT(IN) :: name(*)
  INTEGER(kind=c_int),VALUE :: type
  TYPE(c_ptr),VALUE :: var
  CHARACTER(kind=c_char),INTENT(IN) :: def(*)
  INTEGER(kind=c_int) :: twaddvarrw_c
  END FUNCTION twaddvarrw_c
END INTERFACE


INTERFACE
  FUNCTION twdefine_c(def) BIND(C,NAME="TwDefine")
  IMPORT
  CHARACTER(kind=c_char),INTENT(IN) :: def(*)
  INTEGER(kind=c_int) :: twdefine_c
  END FUNCTION twdefine_c
END INTERFACE


INTERFACE
  FUNCTION twdefineenum_c(def, enumvalues, nvalues) &
   BIND(C,NAME="TwDefineEnum")
  IMPORT
  CHARACTER(kind=c_char),INTENT(IN) :: def(*)
  TYPE(CTWENUMVAL) :: enumvalues(*)
  INTEGER(kind=c_int),VALUE :: nvalues
  INTEGER(kind=c_int) :: twdefineenum_c
  END FUNCTION twdefineenum_c
END INTERFACE


INTERFACE
  FUNCTION twdeleteallbars() BIND(C,NAME='TwDeleteAllBars')
  IMPORT
  INTEGER(kind=c_int) :: twdeleteallbars
  END FUNCTION twdeleteallbars
END INTERFACE


INTERFACE
  FUNCTION twdeletebar(bar) BIND(C,NAME='TwDeleteBar')
  IMPORT
  TYPE(twbar),VALUE :: bar
  INTEGER(kind=c_int) :: twdeletebar
  END FUNCTION twdeletebar
END INTERFACE


INTERFACE
  FUNCTION twdraw() BIND(C,NAME='TwDraw')
  IMPORT
  INTEGER(kind=c_int) :: TwDraw
  END FUNCTION twdraw
END INTERFACE


INTERFACE
  FUNCTION twgetbarbyname_c(barname) BIND(C,NAME='TwGetBarByName')
  IMPORT
  CHARACTER(kind=c_char),INTENT(IN) :: barname(*)
  TYPE(twbar) :: twgetbarbyname_c
  END FUNCTION twgetbarbyname_c
END INTERFACE


INTERFACE
  FUNCTION twgetbarbyindex(barindex) BIND(C,NAME='TwGetBarByIndex')
  IMPORT
  INTEGER(kind=c_int),VALUE :: barindex
  TYPE(twbar) :: twgetbarbyindex
  END FUNCTION twgetbarbyindex
END INTERFACE


INTERFACE
  FUNCTION twgetbarcount() BIND(C,NAME='TwGetBarCount')
  IMPORT
  INTEGER(kind=c_int) :: twgetbarcount
  END FUNCTION twgetbarcount
END INTERFACE


INTERFACE
  FUNCTION twgetbarname_c(bar) BIND(C,NAME='TwGetBarName')
  IMPORT
  TYPE(twbar),VALUE :: bar
  TYPE(c_ptr) :: twgetbarname_c
  END FUNCTION twgetbarname_c
END INTERFACE


INTERFACE
  FUNCTION twgetbottombar() BIND(C,NAME='TwGetBottomBar')
  IMPORT
  TYPE(twbar) :: twgetbottombar
  END FUNCTION twgetbottombar
END INTERFACE


INTERFACE
  FUNCTION twgetlasterror_c() BIND(C,NAME='TwGetLastError')
  IMPORT
  TYPE(c_ptr) :: twgetlasterror_c
  END FUNCTION twgetlasterror_c
END INTERFACE


INTERFACE
  FUNCTION twgetcurrentwindow() BIND(C,NAME='TwGetCurrentWindow')
  IMPORT
  INTEGER(kind=c_int) :: twgetcurrentwindow
  END FUNCTION twgetcurrentwindow
END INTERFACE


INTERFACE
  FUNCTION twgettopbar() BIND(C,NAME='TwGetTopBar')
  IMPORT
  TYPE(twbar) :: twgettopbar
  END FUNCTION twgettopbar
END INTERFACE


INTERFACE
  SUBROUTINE twhandleerrors(errorhandler) BIND(C,NAME='TwHandleErrors')
  IMPORT
  INTERFACE 
    SUBROUTINE errorhandler(errormessage) BIND(C)
    IMPORT
    CHARACTER(kind=c_char), INTENT(in) :: errormessage(*)
    END SUBROUTINE errorhandler
  END INTERFACE
  END SUBROUTINE twhandleerrors
END INTERFACE


INTERFACE
  FUNCTION twinit(graphapi, device) BIND(C,NAME='TwInit')
  IMPORT
  INTEGER(kind=c_int),VALUE :: graphapi
  TYPE(c_ptr),VALUE :: device
  INTEGER(kind=c_int) :: twinit
!  consider to wrap it as return TwInit(TW_OPENGL, NULL);
  END FUNCTION twinit
END INTERFACE


INTERFACE
  FUNCTION twnewbar_c(name) RESULT(bar) BIND(C,NAME='TwNewBar')
  IMPORT
  CHARACTER(kind=c_char), INTENT(in) :: name(*)
  TYPE(c_ptr) :: bar
  END FUNCTION twnewbar_c
END INTERFACE


INTERFACE
  FUNCTION twrefreshbar(bar) BIND(C,NAME='TwRefreshBar')
  IMPORT
  TYPE(twbar),VALUE :: bar
  INTEGER(kind=c_int) :: twrefreshbar
  END FUNCTION twrefreshbar
END INTERFACE


INTERFACE
  FUNCTION twremoveallvars(bar) BIND(C,NAME='TwRemoveAllvars')
  IMPORT
  TYPE(twbar),VALUE :: bar
  INTEGER(kind=c_int) :: twremoveallvars
  END FUNCTION twremoveallvars
END INTERFACE


INTERFACE
  FUNCTION twremovevar_c(bar, name) BIND(C,NAME='TwRemoveVar')
  IMPORT
  TYPE(twbar),VALUE :: bar  
  CHARACTER(kind=c_char), INTENT(in) :: name(*)
  INTEGER(kind=c_int) :: twremovevar_c
  END FUNCTION twremovevar_c
END INTERFACE


INTERFACE
  FUNCTION twsetbottombar(bar) BIND(C,NAME='TwSetBottomBar')
  IMPORT
  TYPE(twbar),VALUE :: bar  
  INTEGER(kind=c_int) :: twsetbottombar
  END FUNCTION twsetbottombar
END INTERFACE


INTERFACE
  FUNCTION twsetcurrentwindow(wndid)  BIND(C,NAME='TwSetCurrentWindow')
  IMPORT
  INTEGER(kind=c_int),VALUE :: wndid
  INTEGER(kind=c_int) :: twsetcurrentwindow
  END FUNCTION twsetcurrentwindow
END INTERFACE


INTERFACE
  FUNCTION twsetparam_c(bar, varname, paramname, paramvaluetype, &
   invaluecount, invalues) BIND(C,NAME='TwSetParam')
  IMPORT
  TYPE(twbar),VALUE :: bar
  CHARACTER(kind=c_char),INTENT(in) :: varname(*), paramname(*)
  INTEGER(kind=c_int),VALUE :: paramvaluetype, invaluecount
  TYPE(c_ptr),VALUE :: invalues
  INTEGER(kind=c_int) :: twsetparam_c
  END FUNCTION twsetparam_c
END INTERFACE


INTERFACE
  FUNCTION twsettopbar(bar) BIND(C,NAME='TwSetTopBar')
  IMPORT
  TYPE(twbar),VALUE :: bar  
  INTEGER(kind=c_int) :: twsettopbar
  END FUNCTION twsettopbar
END INTERFACE


INTERFACE
  FUNCTION twterminate() BIND(C,NAME='TwTerminate')
  IMPORT
  INTEGER(kind=c_int) :: twterminate
  END FUNCTION twterminate
END INTERFACE


INTERFACE
  FUNCTION twwindowexists(wndid)  BIND(C,NAME='TwWindowExists')
  IMPORT
  INTEGER(kind=c_int),VALUE :: wndid
  INTEGER(kind=c_int) :: twwindowexists
  END FUNCTION twwindowexists
END INTERFACE


INTERFACE
  FUNCTION twwindowsize(x, y) BIND(C,NAME='TwWindowSize')
  IMPORT
  INTEGER(kind=c_int),VALUE :: x, y
  INTEGER(kind=c_int) :: twwindowsize
  END FUNCTION twwindowsize
END INTERFACE


! GLUT event handlers
INTERFACE
  FUNCTION tweventkeyboardglut(glutkey, mousex, mousey) &
   BIND(C,NAME='TwEventKeyboardGLUT')
  IMPORT
  CHARACTER(kind=c_char),VALUE :: glutkey
  INTEGER(kind=c_int),VALUE :: mousex, mousey
  INTEGER(kind=c_int) :: tweventkeyboardglut
  END FUNCTION tweventkeyboardglut
END INTERFACE


INTERFACE
  FUNCTION tweventspecialglut(glutkey, mousex, mousey) &
   BIND(C,NAME='TwEventSpecialGLUT')
  IMPORT
  INTEGER(kind=c_int),VALUE :: glutkey, mousex, mousey
  INTEGER(kind=c_int) :: tweventspecialglut
  END FUNCTION tweventspecialglut
END INTERFACE


INTERFACE
  FUNCTION tweventmousebuttonglut(glutbutton, glutstate, mousex, mousey) &
   BIND(C,NAME='TwEventMouseButtonGLUT')
  IMPORT
  INTEGER(kind=c_int),VALUE :: glutbutton, glutstate, mousex, mousey
  INTEGER(kind=c_int) :: tweventmousebuttonglut
  END FUNCTION tweventmousebuttonglut
END INTERFACE


INTERFACE
  FUNCTION tweventmousemotionglut(mousex, mousey) &
   BIND(C,NAME='TwEventMouseMotionGLUT')
  IMPORT
  INTEGER(kind=c_int),VALUE :: mousex, mousey
  INTEGER(kind=c_int) :: tweventmousemotionglut
  END FUNCTION tweventmousemotionglut
END INTERFACE

INTERFACE
  SUBROUTINE twglutmodifiersfunc(twglutgetmodifiersfunc) &
   BIND(C,NAME='TwGLUTModifiersFunc')
  IMPORT
  INTERFACE
    FUNCTION twglutgetmodifiersfunc() BIND(C)
    IMPORT
    INTEGER(kind=c_int) :: twglutgetmodifiersfunc
    END FUNCTION twglutgetmodifiersfunc
  END INTERFACE
  END SUBROUTINE twglutmodifiersfunc
END INTERFACE


! generic Fortran interfaces
INTERFACE twaddbutton
  MODULE PROCEDURE twaddbutton_regular, twaddbutton_null
END INTERFACE

INTERFACE twsetparam
  MODULE PROCEDURE twsetparam_int32v, twsetparam_int32, &
   twsetparam_floatv, twsetparam_float, &
   twsetparam_doublev, twsetparam_double, twsetparam_cstring
END INTERFACE

INTERFACE twnullify
  MODULE PROCEDURE twnullify_f
END INTERFACE

INTERFACE twassociated
  MODULE PROCEDURE twassociated_f
END INTERFACE

PRIVATE twsetparam_int32v, twsetparam_int32, &
 twsetparam_floatv, twsetparam_float, &
 twsetparam_doublev, twsetparam_double, twsetparam_cstring, &
 twnullify_f, twassociated_f

CONTAINS

! Replace the TW_TYPE_CSSTRING C macro
FUNCTION tw_type_csstring(n)
INTEGER, INTENT(in) :: n
INTEGER :: tw_type_csstring

tw_type_csstring = INT(Z'30000000')+IAND(n,INT(Z'0fffffff'))
! 0x30000000+((n)&0xfffffff)
! Null-terminated C Static String of size n (defined as char[n], with n<2^28)
END FUNCTION tw_type_csstring


! Do the oppposite job as tw_type_csstring
FUNCTION tw_len_csstring(csstring)
INTEGER(kind=c_int),VALUE :: csstring
INTEGER :: tw_len_csstring

tw_len_csstring = csstring - INT(Z'30000000')

END FUNCTION tw_len_csstring


! Terminate a variable if it is a string
SUBROUTINE tw_terminate_string(type, string)
INTEGER(kind=c_int),VALUE :: type
TYPE(c_ptr),VALUE :: string

INTEGER :: lenchar, n
CHARACTER(len=1),POINTER :: fchar(:)

lenchar = tw_len_csstring(type)
IF (lenchar > 0) THEN ! it is a string
  CALL C_F_POINTER(string, fchar, (/lenchar/))
  IF (ALL(fchar(:) /= CHAR(0))) THEN ! String not zero-terminated yet
    n = lenchar
    DO n = lenchar, 1, -1
      IF (fchar(n) /= ' ') EXIT
    ENDDO
    fchar(MIN(lenchar+1, n+1)) = CHAR(0) ! may be truncated if not long enough
  ENDIF
ENDIF

END SUBROUTINE tw_terminate_string


! this is the interface for the full button tool
FUNCTION twaddbutton_regular(bar, name, callback, clientdata, def) RESULT(twaddbutton)
TYPE(twbar) :: bar
CHARACTER(kind=c_char,len=*),INTENT(IN) :: name
INTERFACE
  SUBROUTINE callback(clientdata) BIND(C)
  IMPORT
  TYPE(c_ptr),VALUE :: clientdata
  END SUBROUTINE callback
END INTERFACE
TYPE(c_ptr),VALUE :: clientdata
CHARACTER(kind=c_char,len=*),INTENT(IN) :: def
INTEGER(kind=c_int) :: twaddbutton

twaddbutton = twaddbutton_c(bar, TRIM(name)//CHAR(0), C_FUNLOC(callback), &
 clientdata, TRIM(def)//CHAR(0))

END FUNCTION twaddbutton_regular


! this is the interface for the no-callback button tool, with just a line of text
FUNCTION twaddbutton_null(bar, name, callback, clientdata, def) RESULT(twaddbutton)
TYPE(twbar) :: bar
CHARACTER(kind=c_char,len=*),INTENT(IN) :: name
TYPE(c_funptr),VALUE :: callback
TYPE(c_ptr),VALUE :: clientdata
CHARACTER(kind=c_char,len=*),INTENT(IN) :: def
INTEGER(kind=c_int) :: twaddbutton

twaddbutton = twaddbutton_c(bar, TRIM(name)//CHAR(0), callback, &
 clientdata, TRIM(def)//CHAR(0))

END FUNCTION twaddbutton_null


FUNCTION twaddseparator(bar, name, def)
TYPE(twbar),VALUE :: bar
CHARACTER(kind=c_char,len=*),INTENT(IN) :: name
CHARACTER(kind=c_char,len=*),INTENT(IN) :: def
INTEGER(kind=c_int) :: twaddseparator

twaddseparator = twaddseparator_c(bar, TRIM(name)//CHAR(0), TRIM(def)//CHAR(0))

END FUNCTION twaddseparator


FUNCTION twaddvarcb(bar, name, type, setcallback, getcallback, clientdata, def)
TYPE(twbar) :: bar
CHARACTER(kind=c_char,len=*),INTENT(IN) :: name
INTEGER(kind=c_int),VALUE :: type
INTERFACE
  SUBROUTINE setcallback(value, clientdata) BIND(C)
  IMPORT
  TYPE(c_ptr),VALUE :: value
  TYPE(c_ptr),VALUE :: clientdata
  END SUBROUTINE setcallback
END INTERFACE
INTERFACE
  SUBROUTINE getcallback(value, clientdata) BIND(C)
  IMPORT
  TYPE(c_ptr),VALUE :: value
  TYPE(c_ptr),VALUE :: clientdata
  END SUBROUTINE getcallback
END INTERFACE
TYPE(c_ptr),VALUE :: clientdata
CHARACTER(kind=c_char,len=*),INTENT(IN) :: def
INTEGER(kind=c_int) :: twaddvarcb

twaddvarcb = twaddvarcb_c(bar, TRIM(name)//CHAR(0), type, &
 C_FUNLOC(setcallback),  C_FUNLOC(getcallback), clientdata, TRIM(def)//CHAR(0))

END FUNCTION twaddvarcb


FUNCTION twaddvarro(bar, name, type, var, def)
TYPE(twbar),VALUE :: bar
CHARACTER(kind=c_char,len=*),INTENT(IN) :: name
TYPE(c_ptr),VALUE :: var
INTEGER(kind=c_int),VALUE,INTENT(in) :: type
CHARACTER(kind=c_char,len=*),INTENT(IN) :: def
INTEGER(kind=c_int) :: twaddvarro

CALL tw_terminate_string(type, var)
twaddvarro = twaddvarro_c(bar, TRIM(name)//CHAR(0), type, var, &
 TRIM(def)//CHAR(0))

END FUNCTION twaddvarro


FUNCTION twaddvarrw(bar, name, type, var, def)
TYPE(twbar),VALUE :: bar
CHARACTER(kind=c_char,len=*),INTENT(IN) :: name
INTEGER(kind=c_int),VALUE :: type
TYPE(c_ptr),VALUE :: var
CHARACTER(kind=c_char,len=*),INTENT(IN) :: def
INTEGER(kind=c_int) :: twaddvarrw

CALL tw_terminate_string(type, var)
twaddvarrw = twaddvarrw_c(bar, TRIM(name)//CHAR(0), type, var, &
 TRIM(def)//CHAR(0))

END FUNCTION twaddvarrw


FUNCTION twdefine(def)
CHARACTER(kind=c_char,len=*),INTENT(IN) :: def
INTEGER(kind=c_int) :: twdefine

twdefine = twdefine_c(TRIM(def)//CHAR(0))

END FUNCTION twdefine


INTEGER FUNCTION twdefineenum(name, values, labels)
CHARACTER(kind=c_char,len=*),INTENT(IN) :: name
INTEGER(kind=c_int) :: values(:)
CHARACTER(kind=c_char,len=*) :: labels(:)

CHARACTER(kind=c_char,len=LEN(labels)+1),TARGET :: clabels(SIZE(labels))
TYPE(CTWENUMVAL) :: enumvalues(SIZE(labels))
INTEGER :: i

DO i = 1, SIZE(labels)
  clabels(i) = TRIM(labels(i))//CHAR(0)
  enumvalues(i)%value = values(i)
  enumvalues(i)%label = C_LOC(clabels(i))
ENDDO

twdefineenum = twdefineenum_c(TRIM(name)//CHAR(0), enumvalues, &
 MIN(SIZE(values),SIZE(labels)))

END FUNCTION twdefineenum


FUNCTION twgetbarbyname(barname)
CHARACTER(len=*), INTENT(in) :: barname
TYPE(twbar) :: twgetbarbyname

twgetbarbyname = twgetbarbyname_c(TRIM(barname)//CHAR(0))

END FUNCTION twgetbarbyname


FUNCTION twgetbarname(bar)
TYPE(twbar),VALUE :: bar
CHARACTER(len=128) :: twgetbarname

CALL cptrtofchar(twgetbarname_c(bar), twgetbarname)

END FUNCTION twgetbarname


FUNCTION twgetlasterror()
CHARACTER(len=512) :: twgetlasterror

CALL cptrtofchar(twgetlasterror_c(), twgetlasterror)

END FUNCTION twgetlasterror


FUNCTION twnewbar(name) RESULT(bar)
CHARACTER(len=*), INTENT(in) :: name
TYPE(twbar) :: bar  

bar%ptr = twnewbar_c(TRIM(name)//CHAR(0))

END FUNCTION twnewbar


FUNCTION twremovevar(bar, name)
TYPE(twbar),VALUE :: bar  
CHARACTER(len=*), INTENT(in) :: name
INTEGER(kind=c_int) :: twremovevar

twremovevar = twremovevar_c(bar, TRIM(name)//CHAR(0))

END FUNCTION twremovevar


FUNCTION twsetparam_int32v(bar, varname, paramname, invalues) RESULT(param)
TYPE(twbar),VALUE :: bar
CHARACTER(len=*) :: varname, paramname
INTEGER(kind=c_int),TARGET :: invalues(:)
INTEGER :: param

INTEGER(kind=c_int),POINTER :: cinvalues

cinvalues => invalues(1)

param = twsetparam_c(bar, TRIM(varname)//CHAR(0), TRIM(paramname)//CHAR(0), &
 TW_PARAM_INT32, SIZE(invalues), C_LOC(cinvalues))

END FUNCTION twsetparam_int32v


FUNCTION twsetparam_int32(bar, varname, paramname, invalues) RESULT(param)
TYPE(twbar),VALUE :: bar
CHARACTER(len=*) :: varname, paramname
INTEGER(kind=c_int),TARGET :: invalues
INTEGER :: param

param = twsetparam_c(bar, TRIM(varname)//CHAR(0), TRIM(paramname)//CHAR(0), &
 TW_PARAM_INT32, 1, C_LOC(invalues))

END FUNCTION twsetparam_int32


FUNCTION twsetparam_floatv(bar, varname, paramname, invalues) RESULT(param)
TYPE(twbar),VALUE :: bar
CHARACTER(len=*) :: varname, paramname
REAL(kind=c_float),TARGET :: invalues(:)
INTEGER :: param

REAL(kind=c_float),POINTER :: cinvalues

cinvalues => invalues(1)

param = twsetparam_c(bar, TRIM(varname)//CHAR(0), TRIM(paramname)//CHAR(0), &
 TW_PARAM_FLOAT, SIZE(invalues), C_LOC(cinvalues))

END FUNCTION twsetparam_floatv


FUNCTION twsetparam_float(bar, varname, paramname, invalues) RESULT(param)
TYPE(twbar),VALUE :: bar
CHARACTER(len=*) :: varname, paramname
REAL(kind=c_float),TARGET :: invalues
INTEGER :: param

param = twsetparam_c(bar, TRIM(varname)//CHAR(0), TRIM(paramname)//CHAR(0), &
 TW_PARAM_FLOAT, 1, C_LOC(invalues))

END FUNCTION twsetparam_float


FUNCTION twsetparam_doublev(bar, varname, paramname, invalues) RESULT(param)
TYPE(twbar),VALUE :: bar
CHARACTER(len=*), INTENT(in) :: varname, paramname
REAL(kind=c_double),TARGET :: invalues(:)
INTEGER :: param

REAL(kind=c_double),POINTER :: cinvalues

cinvalues => invalues(1)

param = twsetparam_c(bar, TRIM(varname)//CHAR(0), TRIM(paramname)//CHAR(0), &
 TW_PARAM_DOUBLE, SIZE(invalues), C_LOC(cinvalues))

END FUNCTION twsetparam_doublev


FUNCTION twsetparam_double(bar, varname, paramname, invalues) RESULT(param)
TYPE(twbar),VALUE :: bar
CHARACTER(len=*), INTENT(in) :: varname, paramname
REAL(kind=c_double),TARGET :: invalues
INTEGER :: param

param = twsetparam_c(bar, TRIM(varname)//CHAR(0), TRIM(paramname)//CHAR(0), &
 TW_PARAM_DOUBLE, 1, C_LOC(invalues))

END FUNCTION twsetparam_double


FUNCTION twsetparam_cstring(bar, varname, paramname, invalues) RESULT(param)
TYPE(twbar),VALUE :: bar
CHARACTER(len=*), INTENT(in) :: varname, paramname
CHARACTER(len=*), INTENT(in) :: invalues
INTEGER :: param

CHARACTER(len=LEN(invalues)+1,kind=c_char),TARGET :: cinvalues

cinvalues = TRIM(invalues)//CHAR(0) ! trim or not trim?

param = twsetparam_c(bar, TRIM(varname)//CHAR(0), TRIM(paramname)//CHAR(0), &
 TW_PARAM_CSTRING, 1, C_LOC(cinvalues(1:1)))

END FUNCTION twsetparam_cstring


SUBROUTINE cptrtofchar(cptr, fchar)
TYPE(c_ptr) :: cptr
CHARACTER(len=*),INTENT(out) :: fchar

INTEGER :: i
CHARACTER(kind=c_char),POINTER :: tmpchar(:)

CALL C_F_POINTER(cptr, tmpchar, (/LEN(fchar)/))
DO i = 1, LEN(fchar)
  IF (tmpchar(i) == CHAR(0)) THEN
    fchar(i:) = ' '
    EXIT
  ENDIF
  fchar(i:i) = tmpchar(i)
ENDDO

END SUBROUTINE cptrtofchar


! This has to be used before using a CHARACTER var output from a bar
FUNCTION twcstrtofchar(cstr) RESULT(fchar)
CHARACTER(len=*), INTENT(in) :: cstr
CHARACTER(len=LEN(cstr)-1) :: fchar

INTEGER :: i

fchar = cstr
DO i = 1, LEN(fchar)
  IF (fchar(i:i) == CHAR(0)) THEN !if null-terminated pad with blanks
    fchar(i:) = ' '
    EXIT
  ENDIF

ENDDO

END FUNCTION twcstrtofchar

! This has to be used after modifying from Fortran a CHARACTER var used in a bar
SUBROUTINE twfchartocstr(fchar)
CHARACTER(len=*), INTENT(inout) :: fchar

INTEGER :: i

i = MIN(LEN_TRIM(fchar)+1, LEN(fchar))
fchar(i:i) = CHAR(0)

END SUBROUTINE twfchartocstr


SUBROUTINE twnullify_f(bar)
TYPE(twbar), INTENT(inout) :: bar
bar%ptr = C_NULL_PTR
END SUBROUTINE twnullify_f


FUNCTION twassociated_f(bar1, bar2) RESULT(associated_)
TYPE(twbar),INTENT(in) :: bar1
TYPE(twbar),INTENT(in),OPTIONAL :: bar2
LOGICAL :: associated_
IF(PRESENT(bar2)) THEN
  associated_ = C_ASSOCIATED(bar1%ptr, bar2%ptr)
ELSE
  associated_ = C_ASSOCIATED(bar1%ptr)
ENDIF
END FUNCTION twassociated_f


END MODULE anttweakbar
