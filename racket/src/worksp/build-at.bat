cd %1
set BUILD_CONFIG=%2
set BUILD_LEVEL=%3

set PLT_SETUP_OPTIONS=--no-foreign-libs
:suloop
if "%4"=="" goto sudone
set PLT_SETUP_OPTIONS=%PLT_SETUP_OPTIONS% %4
shift
goto suloop
:sudone


build
