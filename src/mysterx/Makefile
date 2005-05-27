# Makefile for mysterx.dll, myspage.dll, myssink.dll

# See README for compilation instructions

MZC="..\..\mzc"
HTMLHELP="C:\Program Files\HTML Help Workshop"
SHELL32="C:\Program Files\Microsoft Visual Studio .NET\VC7\PlatformSDK"
REGSVR32="$(WINDIR)\System32\REGSVR32"

all : 
	@ -255 mkdir ..\..\collects\mysterx\private\compiled\native\win32\i386
	cd myspage
	nmake MZC=$(MZC) HTMLHELP=$(HTMLHELP) SHELL32=$(SHELL32) \
	 REGSVR32=$(REGSVR32) /f myspage.mak 
	cd ..\mysc
	nmake MZC=$(MZC) HTMLHELP=$(HTMLHELP) SHELL32=$(SHELL32) \
	 REGSVR32=$(REGSVR32) /f mysc.mak
	cd ..\myssink
	nmake MZC=$(MZC) HTMLHELP=$(HTMLHELP) SHELL32=$(SHELL32) \
	 REGSVR32=$(REGSVR32) /f myssink.mak
	cd ..
	nmake MZC=$(MZC) HTMLHELP=$(HTMLHELP) SHELL32=$(SHELL32) \
	 REGSVR32=$(REGSVR32) /f mysterx.mak

clean :
	cd myspage
	nmake clean /f myspage.mak
	cd ..\mysc
	nmake clean /f mysc.mak
	cd ..\myssink
	nmake clean /f myssink.mak
	cd ..
	nmake clean /f mysterx.mak
