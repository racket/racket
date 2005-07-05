# Microsoft Developer Studio Project File - Name="MzCOM" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=MzCOM - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "MzCOM.mak".
!MESSAGE 
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

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "MzCOM - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /Yu"stdafx.h" /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /ZI /Od /I "..\..\..\include" /I "..\..\mzcom" /I "." /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_ATL_STATIC_REGISTRY" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /i ".\..\mzcom" /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 libcmtd.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib libmzschxxxxxxx.lib libmzgcxxxxxxx.lib /nologo /subsystem:windows /debug /machine:I386 /nodefaultlib:"libcmt.lib" /out:"../../../collects/mzcom/mzcom.exe" /pdbtype:sept /libpath:"..\libmzsch\Debug" /libpath:"..\libmzgc\Debug"
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "MzCOM - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "MzCOM___Win32_Release"
# PROP BASE Intermediate_Dir "MzCOM___Win32_Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /O1 /I "../../mzscheme/include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_ATL_STATIC_REGISTRY" /D "_ATL_MIN_CRT" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MT /W3 /O1 /I "..\..\mzcom" /I "." /I "..\..\..\include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_ATL_STATIC_REGISTRY" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /i "..\..\mzcom" /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib libmzschxxxxxxx.lib libmzgcxxxxxxx.lib /nologo /subsystem:windows /machine:I386 /out:"../../../collects/mzcom/mzcom.exe" /libpath:"..\libmzsch\Release" /libpath:"..\libmzgc\Release"
# SUBTRACT LINK32 /pdb:none

!ENDIF 

# Begin Target

# Name "MzCOM - Win32 Debug"
# Name "MzCOM - Win32 Release"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\..\mzcom\mzcom.cxx

!IF  "$(CFG)" == "MzCOM - Win32 Debug"

# ADD CPP /I "../../mzscheme/include ../worksp/mzcom"

!ELSEIF  "$(CFG)" == "MzCOM - Win32 Release"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\mzcom\mzcom.idl

!IF  "$(CFG)" == "MzCOM - Win32 Debug"

# ADD MTL /tlb "./mzcom.tlb"
# SUBTRACT MTL /Oicf

!ELSEIF  "$(CFG)" == "MzCOM - Win32 Release"

# PROP Intermediate_Dir "Release"
# ADD MTL /tlb "./mzcom.tlb"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\mzcom\mzobj.cxx

!IF  "$(CFG)" == "MzCOM - Win32 Debug"

!ELSEIF  "$(CFG)" == "MzCOM - Win32 Release"

# PROP Intermediate_Dir "Release"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\mzcom\stdafx.h
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\mzcom.h
# End Source File
# Begin Source File

SOURCE=.\MzCOMCP.h
# End Source File
# Begin Source File

SOURCE=..\..\mzcom\mzobj.h
# End Source File
# Begin Source File

SOURCE=..\..\mzcom\resource.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\mzcom.ico
# End Source File
# Begin Source File

SOURCE=.\mzcom.rc
# End Source File
# Begin Source File

SOURCE=.\MzCOM.rgs
# End Source File
# Begin Source File

SOURCE=.\MzObj.rgs
# End Source File
# End Group
# End Target
# End Project
