
INCS = /I.. /I..\..\rktio /I..\librktio /I$(SCHEME_DIR)\$(MACHINE)\boot\$(MACHINE)

RKTIO_LIB = ..\..\build\librktio.lib
BASE_WIN32_LIBS = WS2_32.lib Shell32.lib User32.lib
WIN32_LIBS = $(BASE_WIN32_LIBS) RpCrt4.lib Ole32.lib Advapi32.lib

LIBS = $(RKTIO_LIB) \
       $(SCHEME_DIR)\$(MACHINE)\boot\$(MACHINE)\$(SCHEME_LIB) \
       $(WIN32_LIBS)

DEST_DLL = ..\..\build\raw_libracketcs.dll
DEST = ..\..\build\raw_racketcs.exe
GDEST = ..\..\build\raw_gracketcs.exe
CSDIR = ..\..\cs\c

FLAGS = /DWIN32
COMP_SUBDIR = /DCS_COMPILED_SUBDIR=1

all: $(DEST) $(GDEST)

$(DEST_DLL): $(CSDIR)\boot.c libracket.res $(RKTIO_LIB)
	cl /LD /DLL /Fe$(DEST_DLL) /Ox /MT $(FLAGS) $(INCS) $(CSDIR)\boot.c libracket.res $(LIBS)

libracket.res: libracket.rc
	rc /l 0x409 /folibracket.res libracket.rc

$(DEST): $(CSDIR)\main.c $(DEST_DLL) racket.res
	cl /Fe$(DEST) /Ox /MT $(COMP_SUBDIR) $(FLAGS) $(INCS) $(CSDIR)\main.c racket.res $(WIN32_LIBS)

racket.res: ../racket/racket.rc ../racket/racket.ico
	rc /l 0x409 /foracket.res ../racket/racket.rc

$(GDEST): $(CSDIR)\grmain.c $(DEST_DLL) gracket.res
	cl /Fe$(GDEST) /Ox /MT $(COMP_SUBDIR) $(FLAGS) $(INCS) $(CSDIR)\grmain.c gracket.res $(WIN32_LIBS) /subsystem:windows

gracket.res: ../gracket/gracket.rc ../gracket/gracket.ico
	rc /l 0x409 /fogracket.res ../gracket/gracket.rc

# Useful for debugging:

RKTIO_DLL = ..\..\lib\librktio.dll

rktio-dll:
	cl /Fe$(RKTIO_DLL) /LD $(RKTIO_LIB) $(BASE_WIN32_LIBS) ..\..\rktio\rktio.def /link /machine:x64
