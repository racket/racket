@echo off
setlocal

set SRCDIR=%~dp0

copy /y "%SRCDIR%\buildmain.zuo" main.zuo > NUL
echo srcdir=%SRCDIR% > Makefile
echo CFLAGS=/DWIN32 /Ox >> Makefile

REM Keep this default consistent with "configure"
echo BOOT_COMPRESS_COMP=--compress >> Makefile
echo INSTALL_MISSING_PKGS=dist >> Makefile

cl.exe /nologo /Fe: winfig.exe "%SRCDIR%\..\..\worksp\winfig.c"
winfig.exe >> Makefile
