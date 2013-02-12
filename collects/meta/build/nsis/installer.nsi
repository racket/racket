!include "MUI2.nsh"
!include "WinVer.nsh"
!include "nsDialogs.nsh"

;; ==================== Configuration

;; The following should define:
;;   RKTVersion, RKTVersionLong, RKTHumanName, RKTShortName,
;;   RKTStartName, RKTDirName, RKTRegName, RKTProgFiles

!include racket-defs.nsh

Name "${RKTHumanName}"
OutFile "installer.exe"

BrandingText "${RKTHumanName}"
BGGradient 4040A0 101020

SetCompressor /SOLID "LZMA"

InstallDir "${RKTProgFiles}\${RKTDirName}"
!ifndef SimpleInstaller
  InstallDirRegKey HKLM "Software\${RKTRegName}" ""
!endif
!define MUI_STARTMENUPAGE_DEFAULTFOLDER "${RKTStartName}"
!define MUI_ICON   "installer.ico"
!define MUI_UNICON "uninstaller.ico"
!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_BITMAP     "header.bmp"
!define MUI_HEADERIMAGE_BITMAP_RTL "header-r.bmp"
!define MUI_HEADERIMAGE_RIGHT

!define MUI_WELCOMEFINISHPAGE_BITMAP "welcome.bmp"
!define MUI_UNWELCOMEFINISHPAGE_BITMAP "welcome.bmp"

!define MUI_WELCOMEPAGE_TITLE "${RKTHumanName} Setup"
!define MUI_UNWELCOMEPAGE_TITLE "${RKTHumanName} Uninstall"
!ifdef SimpleInstaller
  !define MUI_WELCOMEPAGE_TEXT "This is a simple installer for ${RKTShortName}.$\r$\n$\r$\nIt will only create the Racket folder.  To uninstall, simply remove the folder.$\r$\n$\r$\n$_CLICK"
!else
  !define MUI_WELCOMEPAGE_TEXT "This wizard will guide you through the installation of ${RKTShortName}.$\r$\n$\r$\nPlease close any running Racket applications so the installer can update the relevant system files.$\r$\n$\r$\n$_CLICK"
!endif
!define MUI_UNWELCOMEPAGE_TEXT "This wizard will guide you through the removal of ${RKTShortName}.$\r$\n$\r$\nBefore starting, make sure no Racket applications are running.$\r$\n$\r$\n$_CLICK"

!define MUI_FINISHPAGE_TITLE "${RKTHumanName}"
!ifdef SimpleInstaller
  !define MUI_FINISHPAGE_RUN
  !define MUI_FINISHPAGE_RUN_FUNCTION OpenInstDir
  Function OpenInstDir
    ExecShell "" "$INSTDIR"
  FunctionEnd
  !define MUI_FINISHPAGE_RUN_TEXT "Open the installation folder"
!else
  !define MUI_FINISHPAGE_RUN "$INSTDIR\DrRacket.exe"
  !define MUI_FINISHPAGE_RUN_TEXT "Run DrRacket"
!endif
!define MUI_FINISHPAGE_LINK "Visit the Racket web site"
!define MUI_FINISHPAGE_LINK_LOCATION "http://racket-lang.org/"

; !define MUI_UNFINISHPAGE_NOAUTOCLOSE ; to allow users see what was erased

!define MUI_STARTMENUPAGE_REGISTRY_ROOT "HKLM"
!define MUI_STARTMENUPAGE_REGISTRY_KEY "Software\${RKTRegName}"
!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"

; Doesn't work on some non-xp machines
; !define MUI_INSTFILESPAGE_PROGRESSBAR colored

VIProductVersion "${RKTVersionLong}"
VIAddVersionKey "ProductName" "Racket"
VIAddVersionKey "Comments" "This is the Racket language, see http://racket-lang.org/."
VIAddVersionKey "CompanyName" "PLT"
VIAddVersionKey "LegalCopyright" "© PLT"
VIAddVersionKey "FileDescription" "Racket Installer"
VIAddVersionKey "FileVersion" "${RKTVersion}"

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
  ; The assumption is that users might have all kinds of ways to get a Racket
  ; tree, plus, they might have an old wise-based installation, so it is better
  ; to rely on files rather than test registry keys.  Note: no version check.
  ; if any of these exist, then we assume it's an old installation
  IfFileExists "$INSTDIR\Racket.exe"   racket_is_installed
  IfFileExists "$INSTDIR\GRacket.exe"  racket_is_installed
  IfFileExists "$INSTDIR\DrRacket.exe" racket_is_installed
  IfFileExists "$INSTDIR\collects"     racket_is_installed
  Goto racket_is_not_installed
  racket_is_installed:
    IfFileExists "${UNINSTEXE}" we_have_uninstall
      MessageBox MB_YESNO "It appears that there is an existing Racket installation in '$INSTDIR', but no Uninstaller was found.$\r$\nContinue anyway (not recommended)?" /SD IDYES IDYES maybe_remove_tree
      Abort
    we_have_uninstall:
      MessageBox MB_YESNO "It appears that there is an existing Racket installation in '$INSTDIR'.$\r$\nDo you want to uninstall it first (recommended)?" /SD IDNO IDNO maybe_remove_tree
        HideWindow
        ClearErrors
        ExecWait '"${UNINSTEXE}" _?=$INSTDIR'
        IfErrors uninstaller_problematic
        IfFileExists "$INSTDIR\Racket.exe" uninstaller_problematic
        IfFileExists "$INSTDIR\GRacket.exe" uninstaller_problematic
          BringToFront
          Goto racket_is_not_installed
        uninstaller_problematic:
          MessageBox MB_YESNO "Errors in uninstallation!$\r$\nDo you want to quit and sort things out now (highly recommended)?" /SD IDNO IDNO maybe_remove_tree
          Quit
    maybe_remove_tree:
      MessageBox MB_YESNO "Since you insist, do you want to simply remove the previous directory now?$\r$\n(It is really better if you sort this out manually.)" /SD IDYES IDNO racket_is_not_installed
        RMDir /r $INSTDIR
  racket_is_not_installed:
FunctionEnd
!endif

Section ""
  SetShellVarContext all

  SetDetailsPrint both
  DetailPrint "Installing Racket..."
  SetDetailsPrint listonly
  SetOutPath "$INSTDIR"
  File /a /r "racket\*.*"
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
      CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\DrRacket.lnk" "$INSTDIR\DrRacket.exe"
      CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Racket Documentation.lnk" "$INSTDIR\Racket Documentation.exe"
      CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\GRacket.lnk" "$INSTDIR\GRacket.exe"
      CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Racket.lnk" "$INSTDIR\Racket.exe"
      CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Racket Folder.lnk" "$INSTDIR"
      CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Uninstall.lnk" "${UNINSTEXE}"
    !insertmacro MUI_STARTMENU_WRITE_END

    SetDetailsPrint both
    DetailPrint "Setting Registry Keys..."
    SetDetailsPrint listonly
      WriteRegStr HKLM "Software\${RKTRegName}" "" "$INSTDIR" ; Save folder location
      WriteRegStr HKCR ".rkt"   "" "Racket.Document"
      WriteRegStr HKCR ".rktl"  "" "Racket.Document"
      WriteRegStr HKCR ".rktd"  "" "Racket.Document"
      WriteRegStr HKCR ".ss"    "" "Racket.Document"
      WriteRegStr HKCR ".scm"   "" "Racket.Document"
      WriteRegStr HKCR ".scrbl" "" "Racket.Document"
      WriteRegStr HKCR "Racket.Document" "" "Racket Document"
      WriteRegStr HKCR "Racket.Document\DefaultIcon" "" "$INSTDIR\collects\icons\schemedoc.ico"
      WriteRegStr HKCR "Racket.Document\shell\open\command" "" '"$INSTDIR\DrRacket.exe" "%1"'
      ; Example, in case we want some things like this in the future
      ; WriteRegStr HKCR "Racket.Document\shell\racket" "" "Run with Racket"
      ; WriteRegStr HKCR "Racket.Document\shell\racket\command" "" '"$INSTDIR\Racket.exe" "-r" "%1"'
      WriteRegStr HKCR ".plt" "" "Racket Setup.Document"
      WriteRegStr HKCR "Racket Setup.Document" "" "Racket Package"
      WriteRegStr HKCR "Racket Setup.Document\DefaultIcon" "" "$INSTDIR\collects\icons\schemedoc.ico"
      WriteRegStr HKCR "Racket Setup.Document\shell\open\command" "" '"$INSTDIR\raco.exe" setup -p "%1"'

      WriteRegExpandStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${RKTRegName}" "UninstallString" '"${UNINSTEXE}"'
      WriteRegExpandStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${RKTRegName}" "InstallLocation" "$INSTDIR"
      WriteRegStr       HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${RKTRegName}" "DisplayName" "${RKTHumanName}"
      WriteRegStr       HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${RKTRegName}" "DisplayIcon" "$INSTDIR\DrRacket.exe,0"
      WriteRegStr       HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${RKTRegName}" "DisplayVersion" "${RKTVersion}"
      ; used to also have "VersionMajor" & "VersionMinor" but looks like it's not needed
      WriteRegStr       HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${RKTRegName}" "HelpLink" "http://racket-lang.org/"
      WriteRegStr       HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${RKTRegName}" "URLInfoAbout" "http://racket-lang.org/"
      WriteRegStr       HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${RKTRegName}" "Publisher" "PLT Design Inc."
      WriteRegDWORD     HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${RKTRegName}" "NoModify" "1"
      WriteRegDWORD     HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${RKTRegName}" "NoRepair" "1"
  !endif

  SetDetailsPrint both
  DetailPrint "Installation complete."
SectionEnd

;; ==================== Uninstaller

!ifndef SimpleInstaller

Function un.myGUIInit
  ; if any of these exist, then we're fine
  IfFileExists "$INSTDIR\Racket.exe"   racket_is_installed_un
  IfFileExists "$INSTDIR\GRacket.exe"  racket_is_installed_un
  IfFileExists "$INSTDIR\DrRacket.exe" racket_is_installed_un
  IfFileExists "$INSTDIR\collects"     racket_is_installed_un
    MessageBox MB_YESNO "It does not appear that Racket is installed in '$INSTDIR'.$\r$\nContinue anyway (not recommended)?" /SD IDYES IDYES racket_is_installed_un
    Abort "Uninstall aborted by user"
  racket_is_installed_un:
FunctionEnd

Section "Uninstall"
  SetShellVarContext all

  SetDetailsPrint both
  DetailPrint "Removing the Racket installation..."
  SetDetailsPrint listonly
    Delete "$INSTDIR\*.exe"
    Delete "$INSTDIR\README*.*"
    RMDir /r "$INSTDIR\collects"
    RMDir /r "$INSTDIR\include"
    RMDir /r "$INSTDIR\lib"
    RMDir /r "$INSTDIR\doc"
    ;; these exist in Racket-Full installations
    RMDir /r "$INSTDIR\man"
    RMDir /r "$INSTDIR\src"
    Delete "${UNINSTEXE}"
    RMDir "$INSTDIR"
    ;; if the directory is opened, it will take some time to remove
    Sleep 1000
    IfErrors +1 uninstall_inst_dir_ok
      MessageBox MB_YESNO "The Racket installation at '$INSTDIR' was not completely removed.$\r$\nForce deletion?$\r$\n(Make sure no Racket applications are running.)" /SD IDYES IDNO uninstall_inst_dir_ok
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
    DeleteRegKey /ifempty HKLM "Software\${RKTRegName}\Start Menu Folder"
    DeleteRegKey /ifempty HKLM "Software\${RKTRegName}"
    DeleteRegKey HKCR ".rkt"
    DeleteRegKey HKCR ".rktl"
    DeleteRegKey HKCR ".rktd"
    DeleteRegKey HKCR ".ss"
    DeleteRegKey HKCR ".scm"
    DeleteRegKey HKCR ".scrbl"
    DeleteRegKey HKCR "Racket.Document"
    DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${RKTRegName}"

  SetDetailsPrint both
  DetailPrint "Uninstallation complete."
SectionEnd

!endif
