
INCS = /I.. /I..\..\rktio /I..\librktio /I$(SCHEME_DIR)\$(MACHINE)\boot\$(MACHINE)

RKTIO_LIB = ..\..\build\librktio.lib
BASE_WIN32_LIBS = WS2_32.lib Shell32.lib User32.lib
WIN32_LIBS = $(BASE_WIN32_LIBS) RpCrt4.lib Ole32.lib Advapi32.lib

LIBS = $(RKTIO_LIB) \
       $(SCHEME_DIR)\$(MACHINE)\boot\$(MACHINE)\$(SCHEME_LIB) \
       $(WIN32_LIBS)

DEST = ..\..\build\raw_racketcs.exe
CSDIR = ..\..\cs\c

COMP_SUBDIR = /DCS_COMPILED_SUBDIR=1

$(DEST): $(CSDIR).\main.c $(CSDIR)\boot.c racket.res $(RKTIO_LIB)
	cl /Fe$(DEST) /Ox /MT $(COMP_SUBDIR) $(INCS) $(CSDIR)\main.c $(CSDIR)\boot.c racket.res $(LIBS)

racket.res: ../racket/racket.rc ../racket/racket.ico
	rc /l 0x409 /foracket.res ../racket/racket.rc

# Useful for debugging:

RKTIO_DLL = ..\..\lib\librktio.dll

rktio-dll:
	cl /Fe$(RKTIO_DLL) /LD $(RKTIO_LIB) $(BASE_WIN32_LIBS) ..\..\rktio\rktio.def /link /machine:x64
