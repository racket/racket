@REM   Debug version of mkmzdyn
cl /Od /D"_DEBUG" /RTC1 /MTd /GS /W3 -I../include -I../../worksp /c mzdyn.c
lib -machine:X86 -def:mzdyn.def -out:mzdyn.lib
mkdir ..\..\..\lib
mkdir ..\..\..\lib\msvc
copy mzdyn.exp ..\..\..\lib\msvc
copy mzdyn.obj ..\..\..\lib\msvc
copy ..\..\worksp\libmzsch\debug\libmzschxxxxxxx.lib ..\..\..\lib\msvc
copy ..\..\worksp\libmzgc\debug\libmzgcxxxxxxx.lib ..\..\..\lib\msvc
