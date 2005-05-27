# mysc.mak

all : mysc.lib

clean :
        -@erase bstr.obj

HTMLHELP=C:\Program Files\HTML Help Workshop
SHELL32=F:\SBN

CPP=cl.exe
CPP_FLAGS=/I"../../../include" /I"$(SHELL32)\Include" \
	/I"$(HTMLHELP)\include" /MT /W3 /EHsc /O2 /D "WIN32" /D "NDEBUG" /D "_LIB" /D "_MBCS" /c

.cxx.obj::
   $(CPP) $(CPP_FLAGS) $<

LINK32=$(MZC)
LINK32_LIBS= \
	kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib \
	advapi32.lib $(SHELL32)\LIB\shell32.lib ole32.lib oleaut32.lib \
	uuid.lib "$(HTMLHELP)\lib/htmlhelp.lib"

LINK32_OBJS= \
        bstr.obj

mysc.lib : $(LINK32_OBJS)
	LIB /OUT:mysc.lib $(LINK32_OBJS)

bstr.obj : bstr.cxx

