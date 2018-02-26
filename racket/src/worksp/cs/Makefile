
INCS = /I.. /I..\..\rktio /I..\librktio /I$(SCHEME_DIR)\$(MACHINE)\boot\$(MACHINE)

RKTIO_LIB = ..\..\build\librktio.lib
BASE_WIN32_LIBS = WS2_32.lib Shell32.lib User32.lib
WIN32_LIBS = $(BASE_WIN32_LIBS) RpCrt4.lib Ole32.lib

LIBS = $(RKTIO_LIB) \
       $(SCHEME_DIR)\$(MACHINE)\boot\$(MACHINE)\$(SCHEME_LIB) \
       $(WIN32_LIBS)

DEST = ..\..\build\raw_racketcs.exe
CSDIR = ..\..\cs\c

$(DEST): $(CSDIR).\main.c $(CSDIR)\boot.c $(RKTIO_LIB)
	cl /Fe$(DEST) /Ox /MT $(INCS) $(CSDIR)\main.c $(CSDIR)\boot.c $(LIBS)


# Useful for debugging:

RKTIO_DLL = ..\..\lib\librktio.dll

rktio-dll:
	cl /Fe$(RKTIO_DLL) /LD $(RKTIO_LIB) $(BASE_WIN32_LIBS) ..\..\rktio\rktio.def /link /machine:x64
