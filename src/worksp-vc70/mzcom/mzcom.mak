# Microsoft Developer Studio Generated NMAKE File, Based on MzCOM.dsp
!IF "$(CFG)" == ""
CFG=MzCOM - Win32 Debug
!MESSAGE No configuration specified. Defaulting to MzCOM - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "MzCOM - Win32 Debug" && "$(CFG)" != "MzCOM - Win32 Release"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "MzCOM.mak" CFG="MzCOM - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "MzCOM - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE "MzCOM - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "MzCOM - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "..\..\..\collects\mzcom\mzcom.exe" ".\mzcom.tlb" ".\Debug\regsvr32.trg"


CLEAN :
	-@erase "$(INTDIR)\mzcom.obj"
	-@erase "$(INTDIR)\mzcom.res"
	-@erase "$(INTDIR)\mzobj.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\mzcom.pdb"
	-@erase "..\..\..\collects\mzcom\mzcom.exe"
	-@erase "..\..\..\collects\mzcom\mzcom.ilk"
	-@erase ".\mzcom.tlb"
	-@erase ".\Debug\regsvr32.trg"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /ZI /Od /I "..\..\..\collects\mzscheme\include" /I "..\..\mzcom" /I "." /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_ATL_STATIC_REGISTRY" /Fp"$(INTDIR)\MzCOM.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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

MTL=midl.exe
MTL_PROJ=
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\mzcom.res" /i ".\..\mzcom" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\MzCOM.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=libcmtd.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib mzsrc.lib gc.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\mzcom.pdb" /debug /machine:I386 /nodefaultlib:"libcmt.lib" /out:"../../../collects/mzcom/mzcom.exe" /pdbtype:sept /libpath:"..\mzsrc\Debug" /libpath:"..\gc\Debug" 
LINK32_OBJS= \
	"$(INTDIR)\mzcom.obj" \
	"$(INTDIR)\mzobj.obj" \
	"$(INTDIR)\mzcom.res"

"..\..\..\collects\mzcom\mzcom.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

OutDir=.\Debug
TargetPath=\plt\collects\mzcom\mzcom.exe
InputPath=\plt\collects\mzcom\mzcom.exe
SOURCE="$(InputPath)"

"$(OUTDIR)\regsvr32.trg" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	<<tempfile.bat 
	@echo off 
	"$(TargetPath)" /RegServer 
	echo regsvr32 exec. time > "$(OutDir)\regsvr32.trg" 
	echo Server registration done! 
<< 
	

!ELSEIF  "$(CFG)" == "MzCOM - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release

ALL : "..\..\..\collects\mzcom\mzcom.exe" ".\Release\regsvr32.trg"


CLEAN :
	-@erase "$(INTDIR)\mzcom.obj"
	-@erase "$(INTDIR)\mzcom.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "..\..\..\collects\mzcom\mzcom.exe"
	-@erase ".\mzcom.tlb"
	-@erase ".\Release\mzobj.obj"
	-@erase ".\Release\regsvr32.trg"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /O1 /I "..\..\mzcom" /I "." /I "..\..\..\collects\mzscheme\include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_ATL_STATIC_REGISTRY" /Fp"$(INTDIR)\MzCOM.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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

MTL=midl.exe
MTL_PROJ=
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\mzcom.res" /i "..\..\mzcom" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\MzCOM.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib mzsrc.lib gc.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\mzcom.pdb" /machine:I386 /out:"../../../collects/mzcom/mzcom.exe" /libpath:"..\mzsrc\Release" /libpath:"..\gc\Release" 
LINK32_OBJS= \
	"$(INTDIR)\mzcom.obj" \
	".\Release\mzobj.obj" \
	"$(INTDIR)\mzcom.res"

"..\..\..\collects\mzcom\mzcom.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

OutDir=.\Release
TargetPath=\plt\collects\mzcom\mzcom.exe
InputPath=\plt\collects\mzcom\mzcom.exe
SOURCE="$(InputPath)"

"$(OUTDIR)\regsvr32.trg" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	<<tempfile.bat 
	@echo off 
	"$(TargetPath)" /RegServer 
	echo regsvr32 exec. time > "$(OutDir)\regsvr32.trg" 
	echo Server registration done! 
<< 
	

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("MzCOM.dep")
!INCLUDE "MzCOM.dep"
!ELSE 
!MESSAGE Warning: cannot find "MzCOM.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "MzCOM - Win32 Debug" || "$(CFG)" == "MzCOM - Win32 Release"
SOURCE=..\..\mzcom\mzcom.cxx

!IF  "$(CFG)" == "MzCOM - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /ZI /Od /I "..\..\..\collects\mzscheme\include" /I "..\..\mzcom" /I "." /I "../../mzscheme/include ../worksp/mzcom" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_ATL_STATIC_REGISTRY" /Fp"$(INTDIR)\MzCOM.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

"$(INTDIR)\mzcom.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "MzCOM - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /O1 /I "..\..\mzcom" /I "." /I "..\..\..\collects\mzscheme\include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_ATL_STATIC_REGISTRY" /Fp"$(INTDIR)\MzCOM.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\mzcom.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\mzcom\mzcom.idl

!IF  "$(CFG)" == "MzCOM - Win32 Debug"

MTL_SWITCHES=/tlb "./mzcom.tlb" 

".\mzcom.tlb" : $(SOURCE) "$(INTDIR)"
	$(MTL) @<<
  $(MTL_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "MzCOM - Win32 Release"

MTL_SWITCHES=/tlb "./mzcom.tlb" 

".\mzcom.tlb" : $(SOURCE)
	$(MTL) @<<
  $(MTL_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\mzcom\mzobj.cxx

!IF  "$(CFG)" == "MzCOM - Win32 Debug"


"$(INTDIR)\mzobj.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "MzCOM - Win32 Release"


".\Release\mzobj.obj" : $(SOURCE)
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\mzcom.rc

"$(INTDIR)\mzcom.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)



!ENDIF 

