set BUILD_CONFIG=%1
set PLATFORM_CONFIG=%2

if "%BUILD_CONFIG%"=="SDebug" goto asdebug
if "%BUILD_CONFIG%"=="BDebug" goto asdebug

cl /MT -O2 -I../include -I../../worksp /c mzdyn.c
goto finish

:asdebug
cl /Od /D"_DEBUG" /RTC1 /MTd /GS /W3 -I../include -I../../worksp /c mzdyn.c

:finish

set MACHTYPE=x64
if "%PLATFORM_CONFIG%"=="Win32" goto machready
set MACHTYPE=X86
:machready

lib -MACHINE:%MACHTYPE% -def:mzdyn.def -out:mzdyn.lib

mkdir ..\..\..\lib
mkdir ..\..\..\lib\msvc
copy mzdyn.exp ..\..\..\lib\msvc
copy mzdyn.obj ..\..\..\lib\msvc
