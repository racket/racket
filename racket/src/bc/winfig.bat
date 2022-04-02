@echo off
setlocal

set SRCDIR=%~dp0

copy /y "%SRCDIR%\buildmain.zuo" main.zuo > NUL
echo srcdir=%SRCDIR% > Makefile
echo CFLAGS=/DWIN32 /Ox /Zi /Gy >> Makefile

cl.exe /nologo /Fe: winfig.exe "%SRCDIR%\..\win\winfig.c"
winfig.exe >> Makefile
