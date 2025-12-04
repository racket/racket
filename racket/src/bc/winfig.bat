@echo off
setlocal

set SRCDIR=%~dp0

REM This script can be run directly, but it is normally run when
REM `nmake` is used in the root of a checkout of the Git repository for
REM Racket, or by "../../build.zuo" after "../../winfig.bat" is used

set SLSP_SUFFIX=
set ENABLE_CIFY=auto

:argloop
shift
set ARG=%0
if defined ARG (
  if "%ARG%"=="/sofind" set SLSP_SUFFIX=-%1 && shift && goto argloop
  if "%ARG%"=="/cify" set ENABLE_CIFY=yes && goto argloop
  if "%ARG%"=="/nocify" set ENABLE_CIFY=no && goto argloop
  echo Unrecognized argument %ARG%
  exit /B 1
)

copy /y "%SRCDIR%\buildmain.zuo" main.zuo > NUL
echo srcdir=%SRCDIR% > Makefile
echo CPPFLAGS=/DWIN32 >> Makefile
echo CFLAGS=/Ox /GS- >> Makefile
echo INSTALL_MISSING_PKGS=dist >> Makefile
echo INSTALL_SETUP_FLAGS=--no-user >> Makefile
echo SPLS_SUFFIX=%SLSP_SUFFIX% >> Makefile
echo ENABLE_CIFY=%ENABLE_CIFY% >> Makefile

cl.exe /nologo /Fe: winfig.exe "%SRCDIR%\..\worksp\winfig.c"
winfig.exe >> Makefile
