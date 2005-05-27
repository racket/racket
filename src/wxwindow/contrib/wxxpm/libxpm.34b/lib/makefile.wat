WXDIR   = ..\..\..\..

!include $(WXDIR)\src\makewat.env

CPPFLAGS    = /zw /w2 /zq /d2 /DFOR_MSW $(MODEL) /dwx_msw

LIBTARGET = ..\..\wxxpm.lib

OBJECTS = crbuffri.obj &
 create.obj &
 crpfrdat.obj &
 crbuffrp.obj &
 crifrbuf.obj &
 crdatfri.obj &
 crifrdat.obj &
 crdatfrp.obj &
 crpfrbuf.obj &
 data.obj &
 hashtab.obj &
 misc.obj &
 parse.obj &
 rdftop.obj &
 rdftoi.obj &
 rgb.obj &
 rdftodat.obj &
 scan.obj &
 simx.obj &
 wrffrdat.obj &
 wrffrp.obj &
 wrffri.obj &

all: $(LIBTARGET)

$(LIBTARGET): $(OBJECTS)
    %create tmp.lbc
    @for %i in ( $(OBJECTS) ) do @%append tmp.lbc +%i
    wlib /b /c /n /p=512 $^@ @tmp.lbc

clean:   .SYMBOLIC
    -erase *.obj *.bak *.err *.pch *.lib *.lnk *.res *.exe *.rex $(LIBTARGET)
