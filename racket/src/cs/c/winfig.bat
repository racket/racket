@echo off
setlocal

set SRCDIR=%~dp0

copy /y "%SRCDIR%\buildmain.zuo" main.zuo > NUL
echo srcdir=%SRCDIR% > Mf-config
echo CFLAGS=/DWIN32 /Ox /Zi >> Mf-config

cl.exe /nologo /Fe: winfig.exe "%SRCDIR%\..\..\win\winfig.c"
winfig.exe >> Mf-config

