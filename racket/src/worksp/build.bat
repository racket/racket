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
..\..\..\racketcgc %SELF_RACKET_FLAGS% -cu make.rkt
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
..\..\..\racket %SELF_RACKET_FLAGS% -cu xform.rkt
if errorlevel 1 exit /B 1
cd ..

cd mzcom
"%DEVENV%" mzcom.sln /Build "3m|%BUILDMODE%"
if errorlevel 1 exit /B 1
cd ..

..\..\racket -l racket/kernel/init -e "(if (directory-exists? \"../../etc\") (void) (make-directory \"../../etc\"))"
if errorlevel 1 exit /B 1

..\..\racket -N "raco setup" %SELF_RACKET_FLAGS% -l- setup %PLT_SETUP_OPTIONS%
if errorlevel 1 exit /B 1
