cd %1
set BUILD_CONFIG=%2

set PLT_SETUP_OPTIONS=--no-foreign-libs
:suloop
if "%3"=="" goto sudone
set PLT_SETUP_OPTIONS=%PLT_SETUP_OPTIONS% %3
shift
goto suloop
:sudone


build
