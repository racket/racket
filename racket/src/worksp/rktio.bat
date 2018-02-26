setlocal

cl rbuildmode.c
rbuildmode.exe
if errorlevel 1 (set BUILDMODE=win32) else (set BUILDMODE=x64)

cl checkvs9.c
checkvs9.exe
if errorlevel 1 (set PLTSLNVER=9)

set VCPROJ=vcproj

cl genvsx.c
genvsx.exe
if errorlevel 1 (set PLTSLNVER=X)
if errorlevel 1 (set VCPROJ=vcxproj)

cd librktio
msbuild librktio%PLTSLNVER%.%VCPROJ% /p:Configuration=Release /p:Platform=%BUILDMODE%
if errorlevel 1 exit /B 1

copy %BUILDMODE%\Release\librktio.lib ..\..\build\librktio.lib
cd ..
