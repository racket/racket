@echo off
setlocal

set SRCDIR=%~dp0

copy /y "%SRCDIR%\buildmain.zuo" main.zuo > NUL
echo srcdir=%SRCDIR% > Mf-config
echo PREFLAGS=/DWIN32 >> Mf-config
echo CFLAGS=/Ox /GS- >> Mf-config

cl.exe /nologo /Fe: winfig.exe "%SRCDIR%\..\worksp\winfig.c"
winfig.exe >> Mf-config
