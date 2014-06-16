setlocal

cl rbuildmode.c
rbuildmode.exe
if errorlevel 1 (set BUILDMODE=win32) else (set BUILDMODE=x64)

cl checkvs9.c
checkvs9.exe
if errorlevel 1 (set PLTSLNVER=9)

if not exist ..\..\etc  mkdir ..\..\etc
if not exist ..\..\doc  mkdir ..\..\doc
if not exist ..\..\share  mkdir ..\..\share

if not defined BUILD_CONFIG set BUILD_CONFIG=..\..\etc

cd racket
msbuild racket%PLTSLNVER%.sln /p:Configuration=Release /p:Platform=%BUILDMODE%
if errorlevel 1 exit /B 1
cd ..\gracket
msbuild gracket%PLTSLNVER%.sln /p:Configuration=Release /p:Platform=%BUILDMODE%
if errorlevel 1 exit /B 1
cd ..

cd gc2
..\..\..\racketcgc -G ..\%BUILD_CONFIG% -cu make.rkt
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
..\..\..\racket -G ..\%BUILD_CONFIG% -cu xform.rkt
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
