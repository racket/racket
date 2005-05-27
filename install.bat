@echo off

set PLTDIR=

if not "%OS%"=="Windows_NT" goto NoDPHack
rem  On Windows NT %~dp0 is expanded dir+path of %0
set PLTDIR=%~dp0
if not "%PLTDIR%"=="" goto FoundPLTDIR
:NoDPHack

rem  %~dp0 didn't work -- try to hack into our own directory
cd "%0\.."
if not exist "install" goto NoPathFound
set PLTDIR=.
goto FoundPLTDIR

:NoPathFound
echo Cannot guess where this batch file is running from,
echo Try to run it again from its own directory.
pause
goto done

:FoundPLTDir
cd %PLTDIR%

rem  Look for MrEd.exe
if exist "%PLTDIR%\MrEd.exe" goto MrFound
if exist "%PLTDIR%\MzScheme.exe" goto MzFound
echo Could not find %PLTDIR%\MzScheme.exe or %PLTDIR%\MrEd.exe, abort.
pause
goto done
:MrFound
set MZMR=MrEd.exe
goto exeFound
:MzFound
set MZMR=MzScheme.exe
goto exeFound

:exeFound
rem  look for install
if exist "%PLTDIR%\install" goto installFound
echo %PLTDIR%\install not found, abort.
pause
goto done

:installFound
echo Running %PLTDIR%\%MZMR%
"%PLTDIR%\%MZMR%" -qC "%PLTDIR%\install" -i

:done
