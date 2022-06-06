@echo off
setlocal

set SRCDIR=%~dp0

copy /y "%SRCDIR%\buildmain.zuo" main.zuo > NUL
echo srcdir=%SRCDIR% > Makefile
echo CPPFLAGS=/DWIN32 >> Makefile
echo CFLAGS=/Ox /GS- >> Makefile

cl.exe /nologo /Fe: winfig.exe "%SRCDIR%\..\worksp\winfig.c"
winfig.exe >> Makefile
