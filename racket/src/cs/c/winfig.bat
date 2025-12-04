@echo off
setlocal

set SRCDIR=%~dp0

REM This script can be run directly, but it is normally run when
REM `nmake` is used in the root of a checkout of the Git repository for
REM Racket, or by "../../build.zuo" after "../../winfig.bat" is used

set PLT_CS_SLSP_SUFFIX=

:argloop
shift
set ARG=%0
if defined ARG (
  if "%ARG%"=="/sofind" set PLT_CS_SLSP_SUFFIX=-%1 && shift && goto argloop
  echo Unrecognized argument %ARG%
  exit /B 1
)

copy /y "%SRCDIR%\buildmain.zuo" main.zuo > NUL
echo srcdir=%SRCDIR% > Makefile
echo CFLAGS=/DWIN32 /Ox >> Makefile

REM Keep these defaults consistent with "configure"
echo BOOT_COMPRESS_COMP=--compress >> Makefile
echo INSTALL_MISSING_PKGS=dist >> Makefile
echo INSTALL_SETUP_FLAGS=--no-user >> Makefile
echo PLT_CS_SLSP_SUFFIX=%PLT_CS_SLSP_SUFFIX% >> Makefile
echo CONFIGURE_RACKET_SO_COMPILE=PLT_CS_SLSP_SUFFIX=%PLT_CS_SLSP_SUFFIX% >> Makefile

cl.exe /nologo /Fe: winfig.exe "%SRCDIR%\..\..\worksp\winfig.c"
winfig.exe >> Makefile
