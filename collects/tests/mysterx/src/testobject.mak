# mysterx.mak

all : testobject.dll

clean :
        -@erase testcontrol.obj
	-@erase testobject.obj
	-@erase testobject.dll

CPP=cl.exe
CPP_FLAGS=/MT /W3 /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "_ATL_STATIC_REGISTRY" /c

MTL=midl.exe
MTL_SWITCHES=/tlb testobject.tlb /h testobject.h /iid testobject_i.c /Oicf 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"testobject.res"
REGSVR32=regsvr32	

.cxx.obj::
   $(CPP) $(CPP_FLAGS) $< 

LINK32=link.exe
LINK32_FLAGS= \
	kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib \
	advapi32.lib ole32.lib oleaut32.lib \
	uuid.lib odbc32.lib odbccp32.lib \
	/nologo /dll /subsystem:windows /incremental:no /machine:I386 \
	/def:testobject.def /out:testobject.dll
DEF_FILE=testobject.def
LINK32_OBJS= \
        testobject.obj testcontrol.obj testobject.res 

testobject.dll : $(DEF_FILE) $(LINK32_OBJS)
	$(LINK32) $(LINK32_FLAGS) $(LINK32_OBJS)
	$(REGSVR32) /s testobject.dll

testcontrol.obj : testcontrol.cxx testobject.tlb stdafx.h

testobject.obj : testobject.cxx stdafx.h

testobject.tlb : testobject.idl
	$(MTL) $(MTL_SWITCHES) testobject.idl

testcontrol.res : testcontrol.rc testcontrol.tlb
	$(RSC) $(RSC_PROJ) testcontrol.rc

