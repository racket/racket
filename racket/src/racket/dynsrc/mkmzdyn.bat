cl /MT -O2 -I../include -I../../worksp /c mzdyn.c
lib -machine:X86 -def:mzdyn.def -out:mzdyn.lib
mkdir ..\..\..\lib
mkdir ..\..\..\lib\msvc
copy mzdyn.exp ..\..\..\lib\msvc
copy mzdyn.obj ..\..\..\lib\msvc
