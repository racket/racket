This directory contains extra DLLs that are needed for running PLT
Scheme.  The DLL files are moved into the PLT folder by
 plt/src/mzscheme/dynsrc/mkmzdyn.bat
which is, in turn, called by the MSVC projects.

----------------------------------------------------------------------
uniplt_xxxxxxx.dll

This DLL is really Microsoft's UnicoWS.dll.  Microsoft allows
redistribution of this DLL, but discourages installing it into the
system folder.  So, we rename it (and version-mangle the name) so that
it can be installed into the system folder without conflicts.

This DLL is only loaded by MzScheme/MrEd under Windows 95/98/Me. It's
actually not needed at all under Windows NT/2000/XP.

----------------------------------------------------------------------
