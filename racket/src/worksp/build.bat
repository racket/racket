setlocal

cl rbuildmode.c
rbuildmode.exe
if errorlevel 1 (set BUILDMODE=win32) else (set BUILDMODE=x64)

set DEVENV=devenv
for %%X in (vcexpress.exe) do (set VCEXP=%%~$PATH:X)
if defined VCEXP set DEVENV=%VCEXP%

if not exist ..\..\etc  mkdir ..\..\etc
if not exist ..\..\doc  mkdir ..\..\doc
if not exist ..\..\share  mkdir ..\..\share

if not defined BUILD_CONFIG set BUILD_CONFIG=..\..\etc

cd racket
"%DEVENV%" racket.sln /Build "Release|%BUILDMODE%"
if errorlevel 1 exit /B 1
cd ..\gracket
"%DEVENV%" gracket.sln /Build "Release|%BUILDMODE%"
if errorlevel 1 exit /B 1
cd ..

cd gc2
..\..\..\racketcgc -G ..\%BUILD_CONFIG% -cu make.rkt
if errorlevel 1 exit /B 1
cd ..

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
..\..\..\racket -G ..\%BUILD_CONFIG -cu xform.rkt
if errorlevel 1 exit /B 1
cd ..

cd mzcom
"%DEVENV%" mzcom.sln /Build "3m|%BUILDMODE%"
if errorlevel 1 exit /B 1
cd ..

copy ..\COPYING-libscheme.txt ..\..\share\
if errorlevel 1 exit /B 1
copy ..\COPYING_LESSER.txt ..\..\share\
if errorlevel 1 exit /B 1
copy ..\COPYING.txt ..\..\share\
if errorlevel 1 exit /B 1

..\..\racket -G %BUILD_CONFIG% -N "raco" %SELF_RACKET_FLAGS% -l- setup %PLT_SETUP_OPTIONS%
if errorlevel 1 exit /B 1
