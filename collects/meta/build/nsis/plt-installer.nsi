!include "MUI2.nsh"
!include "WinVer.nsh"
!include "nsDialogs.nsh"

;; ==================== Configuration

;; The following should define:
;;   PLTVersion, PLTVersionLong, PLTHumanName,
;;   PLTDirName, PLTRegName

!include plt-defs.nsh

Name "${PLTHumanName}"
OutFile "installer.exe"

BrandingText "${PLTHumanName}"
BGGradient 4040A0 101020

SetCompressor /SOLID "LZMA"

InstallDir "$PROGRAMFILES\${PLTDirName}"
!ifndef SimpleInstaller
  InstallDirRegKey HKLM "Software\${PLTRegName}" ""
!endif
!define MUI_STARTMENUPAGE_DEFAULTFOLDER "${PLTStartName}"
!define MUI_ICON   "plt-installer.ico"
!define MUI_UNICON "plt-uninstaller.ico"
!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_BITMAP     "plt-header.bmp"
!define MUI_HEADERIMAGE_BITMAP_RTL "plt-header-r.bmp"
!define MUI_HEADERIMAGE_RIGHT

!define MUI_WELCOMEFINISHPAGE_BITMAP "plt-welcome.bmp"
!define MUI_UNWELCOMEFINISHPAGE_BITMAP "plt-welcome.bmp"

!define MUI_WELCOMEPAGE_TITLE "${PLTHumanName} Setup"
!define MUI_UNWELCOMEPAGE_TITLE "${PLTHumanName} Uninstall"
!ifdef SimpleInstaller
  !define MUI_WELCOMEPAGE_TEXT "This is a simple installer for ${PLTHumanName}.$\r$\n$\r$\nIt will only create the PLT folder.  To uninstall, simply remove the folder.$\r$\n$\r$\n$_CLICK"
!else
  !define MUI_WELCOMEPAGE_TEXT "This wizard will guide you through the installation of ${PLTHumanName}.$\r$\n$\r$\nPlease close other PLT applications (DrScheme, MrEd, MzScheme) so the installer can update relevant system files.$\r$\n$\r$\n$_CLICK"
!endif
!define MUI_UNWELCOMEPAGE_TEXT "This wizard will guide you through the removal of ${PLTHumanName}.$\r$\n$\r$\nBefore starting, make sure PLT applications (DrScheme, MrEd, MzScheme) are not running.$\r$\n$\r$\n$_CLICK"

!define MUI_FINISHPAGE_TITLE "${PLTHumanName}"
!ifdef SimpleInstaller
  !define MUI_FINISHPAGE_RUN
  !define MUI_FINISHPAGE_RUN_FUNCTION OpenInstDir
  Function OpenInstDir
    ExecShell "" "$INSTDIR"
  FunctionEnd
  !define MUI_FINISHPAGE_RUN_TEXT "Open the installation folder"
!else
  !define MUI_FINISHPAGE_RUN "$INSTDIR\DrScheme.exe"
  !define MUI_FINISHPAGE_RUN_TEXT "Run DrScheme"
!endif
!define MUI_FINISHPAGE_LINK "Visit the Racket web site"
!define MUI_FINISHPAGE_LINK_LOCATION "http://racket-lang.org/"

; !define MUI_UNFINISHPAGE_NOAUTOCLOSE ; to allow users see what was erased

!define MUI_STARTMENUPAGE_REGISTRY_ROOT "HKLM"
!define MUI_STARTMENUPAGE_REGISTRY_KEY "Software\${PLTRegName}"
!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"

; Doesn't work on some non-xp machines
; !define MUI_INSTFILESPAGE_PROGRESSBAR colored

VIProductVersion "${PLTVersionLong}"
VIAddVersionKey "ProductName" "PLT Scheme"
VIAddVersionKey "Comments" "This is PLT Scheme, including DrScheme which is based on MrEd and MzScheme."
VIAddVersionKey "CompanyName" "PLT"
VIAddVersionKey "LegalCopyright" "© PLT"
VIAddVersionKey "FileDescription" "PLT Scheme Installer"
VIAddVersionKey "FileVersion" "${PLTVersion}"

;; ==================== Variables

!ifndef SimpleInstaller
  Var MUI_TEMP
  Var STARTMENU_FOLDER
!endif

;; ==================== Interface

!define MUI_ABORTWARNING

; Install
!insertmacro MUI_PAGE_WELCOME
!define MUI_PAGE_CUSTOMFUNCTION_LEAVE myTestInstDir
!insertmacro MUI_PAGE_DIRECTORY
!ifndef SimpleInstaller
  !insertmacro MUI_PAGE_STARTMENU Application $STARTMENU_FOLDER
!endif
!insertmacro MUI_PAGE_INSTFILES

; Uncheck and hide the "run" checkbox on vista, since it will run with
; elevated permissions (see also ../nsis-vista-note.txt)
!define MUI_PAGE_CUSTOMFUNCTION_SHOW DisableRunCheckBoxIfOnVista
!insertmacro MUI_PAGE_FINISH
Function DisableRunCheckBoxIfOnVista
  ${If} ${AtLeastWinVista}
    ; use EnableWindow instead of ShowWindow to just disable it
    ShowWindow $mui.FinishPage.Run 0
    ${NSD_Uncheck} $mui.FinishPage.Run
  ${EndIf}
FunctionEnd

!ifndef SimpleInstaller
  ; Uninstall
  !define MUI_WELCOMEPAGE_TITLE "${MUI_UNWELCOMEPAGE_TITLE}"
  !define MUI_WELCOMEPAGE_TEXT "${MUI_UNWELCOMEPAGE_TEXT}"
  ; !insertmacro MUI_UNPAGE_WELCOME
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  ; !insertmacro MUI_UNPAGE_FINISH
!endif

!ifndef SimpleInstaller
  !define MUI_CUSTOMFUNCTION_UNGUIINIT un.myGUIInit
!endif

!insertmacro MUI_LANGUAGE "English"

!ifndef SimpleInstaller
  !define UNINSTEXE "$INSTDIR\Uninstall.exe"
!endif

;; ==================== Installer

!ifdef SimpleInstaller
Function myTestInstDir
  IfFileExists "$INSTDIR\*.*" +1 inst_dir_exists
    MessageBox MB_YESNO "The directory '$INSTDIR' already exists, continue?" /SD IDYES IDYES inst_dir_exists
    Abort
  inst_dir_exists:
FunctionEnd
!else
Function myTestInstDir
  ; The assumption is that users might have all kinds of ways to get a PLT
  ; tree, plus, they might have an old wise-based installation, so it is better
  ; to rely on files rather than test registry keys.  Note: no version check.
  ; if any of these exist, then we assume it's an old installation
  IfFileExists "$INSTDIR\MzScheme.exe" plt_is_installed
  IfFileExists "$INSTDIR\MrEd.exe"     plt_is_installed
  IfFileExists "$INSTDIR\DrScheme.exe" plt_is_installed
  IfFileExists "$INSTDIR\collects"     plt_is_installed
  Goto plt_is_not_installed
  plt_is_installed:
    IfFileExists "${UNINSTEXE}" we_have_uninstall
      MessageBox MB_YESNO "It appears that there is an existing PLT Scheme installation in '$INSTDIR', but no Uninstaller was found.$\r$\nContinue anyway (not recommended)?" /SD IDYES IDYES maybe_remove_tree
      Abort
    we_have_uninstall:
      MessageBox MB_YESNO "It appears that there is an existing PLT Scheme installation in '$INSTDIR'.$\r$\nDo you want to uninstall it first (recommended)?" /SD IDNO IDNO maybe_remove_tree
        HideWindow
        ClearErrors
        ExecWait '"${UNINSTEXE}" _?=$INSTDIR'
        IfErrors uninstaller_problematic
        IfFileExists "$INSTDIR\MzScheme.exe" uninstaller_problematic
        IfFileExists "$INSTDIR\MrEd.exe" uninstaller_problematic
          BringToFront
          Goto plt_is_not_installed
        uninstaller_problematic:
          MessageBox MB_YESNO "Errors in uninstallation!$\r$\nDo you want to quit and sort things out now (highly recommended)?" /SD IDNO IDNO maybe_remove_tree
          Quit
    maybe_remove_tree:
      MessageBox MB_YESNO "Since you insist, do you want to simply remove the previous directory now?$\r$\n(It is really better if you sort this out manually.)" /SD IDYES IDNO plt_is_not_installed
        RMDir /r $INSTDIR
  plt_is_not_installed:
FunctionEnd
!endif

Section ""
  SetShellVarContext all

  SetDetailsPrint both
  DetailPrint "Installing PLT Scheme..."
  SetDetailsPrint listonly
  SetOutPath "$INSTDIR"
  File /a /r "plt\*.*"
  !ifndef SimpleInstaller
    WriteUninstaller "${UNINSTEXE}" ; Create uninstaller
  !endif

  !ifndef SimpleInstaller
    SetDetailsPrint both
    DetailPrint "Creating Shortcuts..."
    SetDetailsPrint listonly
    !insertmacro MUI_STARTMENU_WRITE_BEGIN Application
      SetOutPath "$INSTDIR" ; Make installed links run in INSTDIR
      CreateDirectory "$SMPROGRAMS\$STARTMENU_FOLDER"
      CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\DrScheme.lnk" "$INSTDIR\DrScheme.exe"
      CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\PLT Documentation.lnk" "$INSTDIR\plt-help.exe"
      CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\MrEd.lnk" "$INSTDIR\MrEd.exe"
      CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\MzScheme.lnk" "$INSTDIR\MzScheme.exe"
      CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\PLT Folder.lnk" "$INSTDIR"
      CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Uninstall.lnk" "${UNINSTEXE}"
    !insertmacro MUI_STARTMENU_WRITE_END

    SetDetailsPrint both
    DetailPrint "Setting Registry Keys..."
    SetDetailsPrint listonly
      WriteRegStr HKLM "Software\${PLTRegName}" "" "$INSTDIR" ; Save folder location
      WriteRegStr HKCR ".ss" "" "Scheme.Document"
      WriteRegStr HKCR ".scm" "" "Scheme.Document"
      WriteRegStr HKCR ".scrbl" "" "Scheme.Document"
      WriteRegStr HKCR "Scheme.Document" "" "PLT Scheme Document"
      WriteRegStr HKCR "Scheme.Document\DefaultIcon" "" "$INSTDIR\collects\icons\schemedoc.ico"
      WriteRegStr HKCR "Scheme.Document\shell\open\command" "" '"$INSTDIR\DrScheme.exe" "%1"'
      ; Example, in case we want some things like this in the future
      ; WriteRegStr HKCR "Scheme.Document\shell\mzscheme" "" "Run with MzScheme"
      ; WriteRegStr HKCR "Scheme.Document\shell\mzscheme\command" "" '"$INSTDIR\MzScheme.exe" "-r" "%1"'
      WriteRegStr HKCR ".plt" "" "Setup PLT.Document"
      WriteRegStr HKCR "Setup PLT.Document" "" "PLT Scheme Package"
      WriteRegStr HKCR "Setup PLT.Document\DefaultIcon" "" "$INSTDIR\collects\icons\schemedoc.ico"
      WriteRegStr HKCR "Setup PLT.Document\shell\open\command" "" '"$INSTDIR\Setup PLT.exe" -p "%1"'

      WriteRegExpandStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PLTRegName}" "UninstallString" '"${UNINSTEXE}"'
      WriteRegExpandStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PLTRegName}" "InstallLocation" "$INSTDIR"
      WriteRegStr       HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PLTRegName}" "DisplayName" "${PLTHumanName}"
      WriteRegStr       HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PLTRegName}" "DisplayIcon" "$INSTDIR\DrScheme.exe,0"
      WriteRegStr       HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PLTRegName}" "DisplayVersion" "${PLTVersion}"
      ; used to also have "VersionMajor" & "VersionMinor" but looks like it's not needed
      WriteRegStr       HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PLTRegName}" "HelpLink" "http://racket-lang.org/"
      WriteRegStr       HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PLTRegName}" "URLInfoAbout" "http://racket-lang.org/"
      WriteRegStr       HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PLTRegName}" "Publisher" "PLT Scheme Inc."
      WriteRegDWORD     HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PLTRegName}" "NoModify" "1"
      WriteRegDWORD     HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PLTRegName}" "NoRepair" "1"
  !endif

  SetDetailsPrint both
  DetailPrint "Installation complete."
SectionEnd

;; ==================== Uninstaller

!ifndef SimpleInstaller

Function un.myGUIInit
  ; if any of these exist, then we're fine
  IfFileExists "$INSTDIR\MzScheme.exe" plt_is_installed_un
  IfFileExists "$INSTDIR\MrEd.exe"     plt_is_installed_un
  IfFileExists "$INSTDIR\DrScheme.exe" plt_is_installed_un
  IfFileExists "$INSTDIR\collects"     plt_is_installed_un
    MessageBox MB_YESNO "It does not appear that PLT Scheme is installed in '$INSTDIR'.$\r$\nContinue anyway (not recommended)?" /SD IDYES IDYES plt_is_installed_un
    Abort "Uninstall aborted by user"
  plt_is_installed_un:
FunctionEnd

Section "Uninstall"
  SetShellVarContext all

  SetDetailsPrint both
  DetailPrint "Removing the PLT Scheme installation..."
  SetDetailsPrint listonly
    Delete "$INSTDIR\*.exe"
    Delete "$INSTDIR\README*.*"
    RMDir /r "$INSTDIR\collects"
    RMDir /r "$INSTDIR\include"
    RMDir /r "$INSTDIR\lib"
    RMDir /r "$INSTDIR\doc"
    ;; these exist in PLT-Full installations
    RMDir /r "$INSTDIR\man"
    RMDir /r "$INSTDIR\src"
    Delete "${UNINSTEXE}"
    RMDir "$INSTDIR"
    ;; if the directory is opened, it will take some time to remove
    Sleep 1000
    IfErrors +1 uninstall_inst_dir_ok
      MessageBox MB_YESNO "The PLT Scheme installation at '$INSTDIR' was not completely removed.$\r$\nForce deletion?$\r$\n(Make sure no PLT applications are running.)" /SD IDYES IDNO uninstall_inst_dir_ok
        RMDir /r "$INSTDIR"
        IfErrors +1 uninstall_inst_dir_ok
          MessageBox MB_OK "Forced deletion did not work either, you will need to clean up '$INSTDIR' manually." /SD IDOK
    uninstall_inst_dir_ok:

  SetDetailsPrint both
  DetailPrint "Removing Shortcuts..."
  SetDetailsPrint listonly
    !insertmacro MUI_STARTMENU_GETFOLDER Application $MUI_TEMP
    Delete "$SMPROGRAMS\$MUI_TEMP\*.lnk"
    ;; Delete empty start menu parent diretories
    StrCpy $MUI_TEMP "$SMPROGRAMS\$MUI_TEMP"
    startMenuDeleteLoop:
      RMDir $MUI_TEMP
      GetFullPathName $MUI_TEMP "$MUI_TEMP\.."
      IfErrors startMenuDeleteLoopDone
      StrCmp $MUI_TEMP $SMPROGRAMS startMenuDeleteLoopDone startMenuDeleteLoop
    startMenuDeleteLoopDone:

  SetDetailsPrint both
  DetailPrint "Removing Registry Keys..."
  SetDetailsPrint listonly
    DeleteRegKey /ifempty HKLM "Software\${PLTRegName}\Start Menu Folder"
    DeleteRegKey /ifempty HKLM "Software\${PLTRegName}"
    DeleteRegKey HKCR ".ss"
    DeleteRegKey HKCR ".scm"
    DeleteRegKey HKCR ".scrbl"
    DeleteRegKey HKCR "Scheme.Document"
    DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PLTRegName}"

  SetDetailsPrint both
  DetailPrint "Uninstallation complete."
SectionEnd

!endif
