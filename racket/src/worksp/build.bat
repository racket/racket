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

REM Default is to add a BC suffix
if "%UNDERSCORE_BC_SUFFIX%"=="" set UNDERSCORE_BC_SUFFIX=_bc

if "%UNDERSCORE_BC_SUFFIX%"=="_bc" set BC_SUFFIX=BC
if "%UNDERSCORE_BC_SUFFIX%"=="_" set BC_SUFFIX=

echo #define COMPILED_PATH_AS_%BC_SUFFIX% > bc_suffix_new.h
fc bc_suffix.h bc_suffix_new.h
if errorlevel 1 (copy bc_suffix_new.h bc_suffix.h)
del bc_suffix_new.h

if not exist ..\..\etc  mkdir ..\..\etc
if not exist ..\..\doc  mkdir ..\..\doc
if not exist ..\..\share  mkdir ..\..\share

if not defined BUILD_CONFIG set BUILD_CONFIG=..\..\etc

cl cstartup.c
cstartup.exe ..\bc\src\startup.inc libracket\startup.inc
if errorlevel 1 exit /B 1
if not exist libracket\cstartup.inc echo #include "startup.inc" > libracket\cstartup.inc

cd racket
msbuild racket%PLTSLNVER%.sln /p:Configuration=Release /p:Platform=%BUILDMODE%
if errorlevel 1 exit /B 1
..\..\..\racketcgc -cu ..\..\bc\src\compile-startup.rkt ..\libracket\cstartup.inc ..\libracket\cstartup.zo ..\..\bc\src\startup.inc ..\..\version\racket_version.h
if errorlevel 1 exit /B 1
msbuild racket%PLTSLNVER%.sln /p:Configuration=Release /p:Platform=%BUILDMODE%

if not defined BUILD_LEVEL set BUILD_LEVEL="all"
if "%BUILD_LEVEL%"=="cgc" goto doneBuilding

cd ..\gracket
msbuild gracket%PLTSLNVER%.sln /p:Configuration=Release /p:Platform=%BUILDMODE%
if errorlevel 1 exit /B 1
cd ..

REM  Assumes that Racket is started in a subdirectory of here:
set BOOT_SETUP=-W "info@compiler/cm error" -l- setup --boot ../../setup-go.rkt ../compiled

REM Set after BC_SUFFIX is used to determine the "compiled" subdirectory
if "%BUILD_LEVEL%"=="bc" set BC_SUFFIX=BC

cd gc2
..\..\..\racketcgc -G ..\%BUILD_CONFIG% %BOOT_SETUP% make.none ../compiled/make.dep make.rkt --suffix "%BC_SUFFIX%"
if errorlevel 1 exit /B 1
cd ..

if "%BUILD_LEVEL%"=="3m" goto doneBuilding
if "%BUILD_LEVEL%"=="bc" goto doneBuilding

..\..\racketcgc -cu ..\bc\mkincludes.rkt ..\..\include ..\bc .

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
..\..\..\racket%BC_SUFFIX% -G ..\%BUILD_CONFIG% %BOOT_SETUP% mzcom.none ../compiled/mzcom.dep xform.rkt
if errorlevel 1 exit /B 1
cd ..

cd mzcom
msbuild mzcom%PLTSLNVER%.sln /p:Configuration=3m /p:Platform=%BUILDMODE%
if errorlevel 1 exit /B 1
cd ..

copy ..\LICENSE-libscheme.txt ..\..\share\
if errorlevel 1 exit /B 1
copy ..\LICENSE-MIT.txt ..\..\share\
if errorlevel 1 exit /B 1
copy ..\LICENSE-APACHE.txt ..\..\share\
if errorlevel 1 exit /B 1
copy ..\LICENSE-LGPL.txt ..\..\share\
if errorlevel 1 exit /B 1
copy ..\LICENSE-GPL.txt ..\..\share\
if errorlevel 1 exit /B 1

set PLT_REPLACE_INDEPENDENT_LAUNCHERS=yes
..\..\racket%BC_SUFFIX% -G %BUILD_CONFIG% -N "raco" %SELF_RACKET_FLAGS% -l- setup %PLT_SETUP_OPTIONS%
if errorlevel 1 exit /B 1

..\..\racket%BC_SUFFIX% -G ..\%BUILD_CONFIG% -u gendef.rkt
if errorlevel 1 exit /B 1

:doneBuilding
