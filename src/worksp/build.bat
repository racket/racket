setlocal

cl rbuildmode.c
rbuildmode.exe
if errorlevel 1 (set BUILDMODE=win32) else (set BUILDMODE=x64)

cd racket
devenv racket.sln /Build "Release|%BUILDMODE%"
if errorlevel 1 exit /B 1
cd ..\gracket
devenv gracket.sln /Build "Release|%BUILDMODE%"
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

cd mzstart
devenv mzstart.sln /Build "Release|%BUILDMODE%"
if errorlevel 1 exit /B 1
cd ..\mrstart
devenv mrstart.sln /Build "Release|%BUILDMODE%"
if errorlevel 1 exit /B 1
cd ..

cd mzcom
devenv mzcom.sln /Build "Release|%BUILDMODE%"
if errorlevel 1 exit /B 1
cd ..\libmysterx
devenv libmysterx.sln /Build "Release|%BUILDMODE%"
if errorlevel 1 exit /B 1
cd ..

cd libmysterx
..\..\..\racket -cu xform.rkt
if errorlevel 1 exit /B 1
cd ..

cd mzcom
..\..\..\racket -cu xform.rkt
if errorlevel 1 exit /B 1
cd ..

cd mzcom
devenv mzcom.sln /Build "3m|%BUILDMODE%"
if errorlevel 1 exit /B 1
cd ..\libmysterx
devenv libmysterx.sln /Build "3m|%BUILDMODE%"
if errorlevel 1 exit /B 1
cd ..

..\..\racket -l setup -N "raco setup"
if errorlevel 1 exit /B 1
