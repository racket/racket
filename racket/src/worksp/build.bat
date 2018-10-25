setlocal

cl rbuildmode.c
rbuildmode.exe
if errorlevel 1 (set BUILDMODE=win32) else (set BUILDMODE=x64)

cl checkvs9.c
checkvs9.exe
if errorlevel 1 (set PLTSLNVER=9)

cl genvsx.c
genvsx.exe
if errorlevel 1 (set PLTSLNVER=X)

if not exist ..\..\etc  mkdir ..\..\etc
if not exist ..\..\doc  mkdir ..\..\doc
if not exist ..\..\share  mkdir ..\..\share

if not defined BUILD_CONFIG set BUILD_CONFIG=..\..\etc

cl cstartup.c
cstartup.exe ..\racket\src\startup.inc libracket\startup.inc
if errorlevel 1 exit /B 1
if not exist libracket\cstartup.inc echo #include "startup.inc" > libracket\cstartup.inc

cd racket
msbuild racket%PLTSLNVER%.sln /p:Configuration=Release /p:Platform=%BUILDMODE%
if errorlevel 1 exit /B 1
..\..\..\racketcgc -cu ..\..\racket\src\compile-startup.rkt ..\libracket\cstartup.inc ..\libracket\cstartup.zo ..\..\racket\src\startup.inc ..\..\racket\src\schvers.h
if errorlevel 1 exit /B 1
msbuild racket%PLTSLNVER%.sln /p:Configuration=Release /p:Platform=%BUILDMODE%

if not defined BUILD_LEVEL set BUILD_LEVEL="3m"
if "%BUILD_LEVEL%"=="cgc" goto doneBuilding

cd ..\gracket
msbuild gracket%PLTSLNVER%.sln /p:Configuration=Release /p:Platform=%BUILDMODE%
if errorlevel 1 exit /B 1
cd ..

REM  Assumes that Racket is started in a subdirectory of here:
set BOOT_SETUP=-W "info@compiler/cm error" -l- setup --boot ../../setup-go.rkt ../compiled

cd gc2
..\..\..\racketcgc -G ..\%BUILD_CONFIG% %BOOT_SETUP% make.none ../compiled/make.dep make.rkt
if errorlevel 1 exit /B 1
cd ..

cd mzstart
msbuild mzstart%PLTSLNVER%.sln /p:Configuration=Release /p:Platform=%BUILDMODE%
if errorlevel 1 exit /B 1
cd ..\mrstart
msbuild mrstart%PLTSLNVER%.sln /p:Configuration=Release /p:Platform=%BUILDMODE%
if errorlevel 1 exit /B 1
cd ..

cd mzcom
msbuild mzcom%PLTSLNVER%.sln /p:Configuration=Release /p:Platform=%BUILDMODE%
if errorlevel 1 exit /B 1
cd ..

cd mzcom
..\..\..\racket -G ..\%BUILD_CONFIG% %BOOT_SETUP% mzcom.none ../compiled/mzcom.dep xform.rkt
if errorlevel 1 exit /B 1
cd ..

cd mzcom
msbuild mzcom%PLTSLNVER%.sln /p:Configuration=3m /p:Platform=%BUILDMODE%
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

..\..\racket -G ..\%BUILD_CONFIG% -u gendef.rkt
if errorlevel 1 exit /B 1

:doneBuilding
