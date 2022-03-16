@echo off
setlocal

set M=%1
set WORKAREA=%M%

if "%WORKAREA%"=="" goto needargument

if not exist %WORKAREA% mkdir %WORKAREA%

echo srcdir=%~dp0 > %WORKAREA%\Mf-config
echo m=%M% >> %WORKAREA%\Mf-config
echo forWindows=y >> %WORKAREA%\Mf-config

echo workarea=%WORKAREA% > Makefile
echo !include %WORKAREA%\Mf-config >> Makefile
type "%~dp0\makefiles\Makefile.nt" >> Makefile

xcopy /-i /y "%~dp0\makefiles\buildmain.zuo" main.zuo > NUL
xcopy /-i /y "%~dp0\makefiles\workmain.zuo" %WORKAREA%\main.zuo > NUL

echo Configured for %M%

goto donebuilding

:needargument

echo Please supply the machine name as an argument
exit /B 1

:donebuilding
