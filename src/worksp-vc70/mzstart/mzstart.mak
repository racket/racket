# Microsoft Developer Studio Generated NMAKE File, Based on mzstart.dsp
!IF "$(CFG)" == ""
CFG=MzStart - Win32 Release
!MESSAGE No configuration specified. Defaulting to MzStart - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "MzStart - Win32 Release"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "mzstart.mak" CFG="MzStart - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "MzStart - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
RSC=rc.exe
OUTDIR=.\..\..\..\collects\launcher
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\..\..\..\collects\launcher
# End Custom Macros

ALL : "$(OUTDIR)\mzstart.exe"


CLEAN :
	-@erase "$(INTDIR)\start.obj"
	-@erase "$(INTDIR)\start.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\mzstart.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mzstart.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\mzstart.pdb" /machine:I386 /out:"$(OUTDIR)\mzstart.exe" 
LINK32_OBJS= \
	"$(INTDIR)\start.obj" \
	"$(INTDIR)\start.res"

"$(OUTDIR)\mzstart.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

CPP_PROJ=/nologo /ML /W3 /GX /O1 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "MZSTART" /Fp"$(INTDIR)\mzstart.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

RSC_PROJ=/l 0x409 /fo"$(INTDIR)\start.res" /d "NDEBUG" /d "MZSTART" 

!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("mzstart.dep")
!INCLUDE "mzstart.dep"
!ELSE 
!MESSAGE Warning: cannot find "mzstart.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "MzStart - Win32 Release"
SOURCE=..\..\mzscheme\dynsrc\start.c

"$(INTDIR)\start.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\..\starters\start.rc

"$(INTDIR)\start.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) /l 0x409 /fo"$(INTDIR)\start.res" /i "..\starters" /d "NDEBUG" /d "MZSTART" $(SOURCE)



!ENDIF 

