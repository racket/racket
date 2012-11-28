setlocal

cl rbuildmode.c
rbuildmode.exe
if errorlevel 1 (set BUILDMODE=win32) else (set BUILDMODE=x64)

set DEVENV=devenv
for %%X in (vcexpress.exe) do (set VCEXP=%%~$PATH:X)
if defined VCEXP set DEVENV=%VCEXP%

cd racket
"%DEVENV%" racket.sln /Build "Release|%BUILDMODE%"
if errorlevel 1 exit /B 1
cd ..\gracket
"%DEVENV%" gracket.sln /Build "Release|%BUILDMODE%"
if errorlevel 1 exit /B 1
cd ..

cd gc2
..\..\..\racketcgc -cu make.rkt
if errorlevel 1 exit /B 1
cd ..

..\..\racket -cu ..\get-libs.rkt core ..\..\lib
if errorlevel 1 exit /B 1
..\..\racket -cu ..\get-libs.rkt gui  ..\..\lib
if errorlevel 1 exit /B 1
..\..\racket -cu ..\get-libs.rkt db  ..\..\lib
if errorlevel 1 exit /B 1
..\..\racket -cu ..\get-libs.rkt com  ..\..\lib
if errorlevel 1 exit /B 1
..\..\racket -cu ..\get-libs.rkt math  ..\..\lib
if errorlevel 1 exit /B 1

cd mzstart
"%DEVENV%" mzstart.sln /Build "Release|%BUILDMODE%"
if errorlevel 1 exit /B 1
cd ..\mrstart
"%DEVENV%" mrstart.sln /Build "Release|%BUILDMODE%"
if errorlevel 1 exit /B 1
cd ..

cd mzcom
"%DEVENV%" mzcom.sln /Build "Release|%BUILDMODE%"
if errorlevel 1 exit /B 1
cd ..

cd mzcom
..\..\..\racket -cu xform.rkt
if errorlevel 1 exit /B 1
cd ..

cd mzcom
"%DEVENV%" mzcom.sln /Build "3m|%BUILDMODE%"
if errorlevel 1 exit /B 1
cd ..

..\..\racket -N "raco setup" -l- setup %PLT_SETUP_OPTIONS%
if errorlevel 1 exit /B 1
