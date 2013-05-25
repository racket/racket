# myssink.mak

CPP=cl.exe
CPP_FLAGS=/I"../../../include" /I"../mysc" /MT /W3 /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "_WINDLL" /D "MYSSINK_EXPORTS" /D "_ATL_STATIC_REGISTRY" /c

MTL=midl.exe
MTL_SWITCHES=/tlb myssink.tlb /h myssink.h /iid myssink_i.c /Oicf
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"myssink.res"

LINK32=link.exe
LINK32_FLAGS=..\..\..\lib\msvc\mzdyn.obj ..\..\..\lib\msvc\libmzgcxxxxxxx.lib ..\..\..\lib\msvc\libmzschxxxxxxx.lib \
kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib \
..\mysc\mysc.lib \
/nologo /subsystem:windows /dll /incremental:no /machine:I386 /def:myssink.def /out:myssink.dll
DEF_FILE=myssink.def
LINK32_OBJS= myssink.obj sink.obj comtypes.obj stdafx.obj myssink.res

all : myssink.tlb myssink.dll myssink.h myssink_i.c

myssink.dll : $(DEF_FILE) $(LINK32_OBJS) ..\mysc\mysc.lib
	$(LINK32) $(LINK32_FLAGS) $(LINK32_OBJS)
	copy myssink.dll ..\..\..\collects\mysterx\private\compiled\native\win32\i386
	$(REGSVR32) /s ..\..\..\collects\mysterx\private\compiled\native\win32\i386\myssink.dll

clean :
	-@erase comtypes.obj
	-@erase myssink.obj
	-@erase sink.obj
	-@erase myssink.res
	-@erase stdafx.obj
	-@erase myssink.dll
	-@erase myssink.h
	-@erase myssink.tlb
	-@erase myssink_i.c
	-@erase myssink_p.c

.cxx.obj::
   $(CPP) $(CPP_FLAGS) $<

myssink.obj : myssink.cxx myssink.h sink.h comtypes.h stdafx.h

sink.obj : sink.cxx myssink.h sink.h comtypes.h stdafx.h

comtypes.obj : comtypes.cxx comtypes.h stdafx.h

stdafx.obj : stdafx.cxx stdafx.h

myssink.tlb myssink.h myssink_i.c : myssink.idl
	$(MTL) $(MTL_SWITCHES) myssink.idl

myssink.res : myssink.rc myssink.tlb
	$(RSC) $(RSC_PROJ) myssink.rc


