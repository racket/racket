bcc32 -I../include -omzdynb.obj -c mzdyn.c
mkdir ..\..\..\lib
mkdir ..\..\..\lib\bcc
copy mzdynb.obj ..\..\..\lib\bcc
copy mzdynb.def ..\..\..\lib\bcc

