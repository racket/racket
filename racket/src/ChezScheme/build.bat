@echo off
setlocal

set M=%1
set WORKAREA=%M%
set LINKAS=dll
set RUNTIMEAS=static
set SRCDIR=%~dp0

if "%WORKAREA%"=="" goto needargument

:argloop
shift
set ARG=%1
if defined ARG (
  if "%ARG%"=="/exe" set LINKAS=exe && goto argloop
  if "%ARG%"=="/MD" set RUNTIMEAS=dll && goto argloop
  if "%ARG%"=="/MT" set RUNTIMEAS=static && goto argloop
  echo Unrecognized argument %ARG%
  exit /B 1
)  

if not exist %WORKAREA% mkdir %WORKAREA%

echo srcdir=%SRCDIR% > %WORKAREA%\Mf-config
echo m=%M% >> %WORKAREA%\Mf-config
echo linkAs=%LINKAS% >> %WORKAREA%\Mf-config
echo runtimeAs=%RUNTIMEAS% >> %WORKAREA%\Mf-config

echo workarea=%WORKAREA% > Makefile
echo !include %WORKAREA%\Mf-config >> Makefile
type "%SRCDIR%\makefiles\Makefile.nt" >> Makefile

xcopy /-i /y "%SRCDIR%\makefiles\buildmain.zuo" main.zuo > NUL
xcopy /-i /y "%SRCDIR%\makefiles\workmain.zuo" %WORKAREA%\main.zuo > NUL

echo Configured for %M%

goto donebuilding

:needargument

echo Please supply the machine name as an argument
exit /B 1

:donebuilding
