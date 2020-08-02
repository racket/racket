cd %1
set BUILD_CONFIG=%2
set BUILD_LEVEL=%3
set UNDERSCORE_BC_SUFFIX=%4

set PLT_SETUP_OPTIONS=--no-foreign-libs
:suloop
if "%5"=="" goto sudone
set PLT_SETUP_OPTIONS=%PLT_SETUP_OPTIONS% %5
shift
goto suloop
:sudone


build
