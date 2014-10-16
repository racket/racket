#lang at-exp racket/base
(require racket/format
         racket/list
         racket/system
	 racket/path
         racket/runtime-path
         setup/getinfo)

(provide installer-exe)

(define-runtime-path installer-dir "windows-installer")

(define (get-exe-actions src-dir filename combine)
  (define f (build-path src-dir "lib" filename))
  (for/list ([(k v) (if (file-exists? f)
                        (call-with-input-file* f read)
                        (hash))])
    (combine k v)))

(define (get-extreg src-dir)
  (apply
   append
   (get-exe-actions src-dir "extreg.rktd"
                    (lambda (k v) 
                      (for/list ([v (in-list v)])
                        (append v (list k)))))))

(define (get-startmenu src-dir)
  (get-exe-actions src-dir "startmenu.rktd"
                   (lambda (k v) k)))

(define (get-auto-launch src-dir)
  (define l
    (filter (lambda (p) (real? (cdr p)))
            (get-exe-actions src-dir "startmenu.rktd"
                             cons)))
  (if (null? l)
      #f
      (path-replace-suffix (caar (sort l < #:key cdr)) #"")))

(define (try-exe f)
  (and (file-exists? f) f))

(define (nsis-generate dest distname version winplatform 
                       makensis
                       #:extension-registers [extregs null]
                       #:start-menus [startmenus null]
                       #:versionless [versionless? #t]
                       #:simple? [simple? #f]
                       #:auto-launch [auto-launch #f])
  (define distdir (regexp-replace* #rx" " distname "-"))
  (define destfilename (file-name-from-path dest))
  (define-values (version1 version2 version3 version4)
    (apply
     values
     (take (cdr (regexp-match #rx"^([0-9]*)[.]([0-9]*)[.]([0-9]*)[.]([0-9]*)"
                              (string-append version ".0.0.0")))
           4)))
  (define got-files (make-hash))
  (define (get-file s)
    (unless (hash-ref got-files s #f)
      (define dest (build-path "bundle" s))
      (unless (file-exists? dest)
	(hash-set! got-files s #t)
        (copy-file (build-path installer-dir s) dest)))
    s)
  (define script
  @~a{
!include "MUI2.nsh"
!include "WinVer.nsh"
!include "nsDialogs.nsh"

;; ==================== Configuration

!define RKTVersion "@|version|"
!define RKTVersionLong "@|version1|.@|version2|.@|version3|.@|version4|"
;; Full name for the package, and a short name for installer texts
!define RKTHumanName "@|distname| v@|version| (@|winplatform|)"
!define RKTShortName "@|distname|"
!define RKTStartName "@|distname|@(if versionless? "" @~a{ v@|version|})"
!define RKTDirName "@|distdir|@(if versionless? "" @~a{-@|version|})"
!define RKTRegName "@|distdir|-@|winplatform|-@|version|"
!define RKTProgFiles "$PROGRAMFILES@(if (equal? winplatform "x86_64") "64" "")"
@(if simple? @~a{!define SimpleInstaller} "")
@(if auto-launch @~a{!define RKTLaunchProgram "@|auto-launch|"} "")

Name "${RKTHumanName}"
OutFile "@|destfilename|"

BrandingText "${RKTHumanName}"
BGGradient 4040A0 101020

SetCompressor /SOLID "LZMA"

InstallDir "${RKTProgFiles}\${RKTDirName}"
!ifndef SimpleInstaller
  InstallDirRegKey HKLM "Software\${RKTRegName}" ""
!endif
!define MUI_STARTMENUPAGE_DEFAULTFOLDER "${RKTStartName}"
!define MUI_ICON   "@(get-file "installer.ico")"
!define MUI_UNICON "@(get-file "uninstaller.ico")"
!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_BITMAP     "@(get-file "header.bmp")"
!define MUI_HEADERIMAGE_BITMAP_RTL "@(get-file "header-r.bmp")"
!define MUI_HEADERIMAGE_RIGHT

!define MUI_WELCOMEFINISHPAGE_BITMAP "@(get-file "welcome.bmp")"
!define MUI_UNWELCOMEFINISHPAGE_BITMAP "@(get-file "welcome.bmp")"

!define MUI_WELCOMEPAGE_TITLE "${RKTHumanName} Setup"
!define MUI_UNWELCOMEPAGE_TITLE "${RKTHumanName} Uninstall"
!ifdef SimpleInstaller
  !define MUI_WELCOMEPAGE_TEXT "This is a simple installer for ${RKTShortName}.$\r$\n$\r$\nIt will only create the @|distname| folder.  To uninstall, simply remove the folder.$\r$\n$\r$\n$_CLICK"
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
@(if auto-launch
     @~a{
         !else
           !define MUI_FINISHPAGE_RUN "$INSTDIR\${RKTLaunchProgram}.exe"
           !define MUI_FINISHPAGE_RUN_TEXT "Run ${RKTLaunchProgram}"}
     "")
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
VIAddVersionKey "CompanyName" "PLT Design Inc."
VIAddVersionKey "LegalCopyright" "© PLT Design Inc."
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
  IfFileExists "$INSTDIR\Racket.exe" racket_is_installed
  @(if auto-launch @~a{IfFileExists "$INSTDIR\${RKTLaunchProgram}.exe" racket_is_installed} "")
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
      @apply[~a
             #:separator "\n"
             (for/list ([exe-str (in-list startmenus)])
               (define exe exe-str)
               (define lnk (path->string (path-replace-suffix exe-str #".lnk")))
               @~a{      CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\@|lnk|" "$INSTDIR\@|exe|"})]
      CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Racket.lnk" "$INSTDIR\Racket.exe"
      CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Racket Folder.lnk" "$INSTDIR"
      CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Uninstall.lnk" "${UNINSTEXE}"
    !insertmacro MUI_STARTMENU_WRITE_END

    SetDetailsPrint both
    DetailPrint "Setting Registry Keys..."
    SetDetailsPrint listonly
      WriteRegStr HKLM "Software\${RKTRegName}" "" "$INSTDIR" ; Save folder location
      @apply[~a 
             #:separator "\n"
             (apply
              append
              (for/list ([extreg (in-list extregs)])
                (define kind (list-ref extreg 1))
                (define icon (list-ref extreg 3))
                (define cmd (list-ref extreg 4))
                (define exe-name (list-ref extreg 5))
                (append
                 (for/list ([ext (in-list (list-ref extreg 2))])
                   @~a{    WriteRegStr HKCR ".@|ext|"   "" "@|kind|"})
                 (list
                  @~a{    WriteRegStr HKCR "@|kind|" "" "@(list-ref extreg 0)"}
                  @~a{    WriteRegStr HKCR "@|kind|\DefaultIcon" "" "$INSTDIR\lib\@|icon|"})
                 (if cmd
                     (list
                      @~a{    WriteRegStr HKCR "@|kind|\shell\open\command" "" '"$INSTDIR\@|exe-name|" @|cmd|'})
                     null))))]
      ; Example, in case we want some things like this in the future
      ; WriteRegStr HKCR "Racket.Document\shell\racket" "" "Run with Racket"
      ; WriteRegStr HKCR "Racket.Document\shell\racket\command" "" '"$INSTDIR\Racket.exe" "-r" "%1"'

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
  IfFileExists "$INSTDIR\lib\GRacket.exe"  racket_is_installed_un
  @(if auto-launch @~a{IfFileExists "$INSTDIR\${RKTLaunchProgram}.exe" racket_is_installed_un} "")
  IfFileExists "$INSTDIR\collects" racket_is_installed_un
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
    RMDir /r "$INSTDIR\include"
    RMDir /r "$INSTDIR\collects"
    RMDir /r "$INSTDIR\lib"
    RMDir /r "$INSTDIR\share"
    RMDir /r "$INSTDIR\etc"
    RMDir /r "$INSTDIR\doc"
    ;; these exist in Racket-Full installations
    RMDir /r "$INSTDIR\man"
    ; RMDir /r "$INSTDIR\src"
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
    @apply[~a
           #:separator "\n"
           (append
            (for*/list ([extreg (in-list extregs)]
                        [ext (in-list (list-ref extreg 2))])
              @~a{    DeleteRegKey HKCR ".@|ext|"})
            (for/list ([extreg (in-list extregs)])
              @~a{    DeleteRegKey HKCR ".@(list-ref extreg 1)"}))]
    DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${RKTRegName}"

  SetDetailsPrint both
  DetailPrint "Uninstallation complete."
SectionEnd

!endif
})
  (call-with-output-file*
   "bundle/installer.nsi"
   #:mode 'text
   #:exists 'truncate
   (lambda (o)
     (display script o)
     (newline o)))
  (parameterize ([current-directory "bundle"])
    (system* makensis "/V3" "installer.nsi")))

(define (installer-exe human-name base-name versionless? dist-suffix readme)
  (define makensis (or (find-executable-path "makensis.exe")
                       (try-exe "c:\\Program Files\\NSIS\\makensis.exe")
                       (try-exe "c:\\Program Files (x86)\\NSIS\\makensis.exe")
                       (error 'installer-exe "cannot find \"makensis.exe\"")))
  (define platform (let-values ([(base name dir?) (split-path (system-library-subpath #f))])
                     (path->string name)))
  (define exe-path (format "bundle/~a-~a-win32~a.exe" base-name platform dist-suffix))
  (when readme
    (call-with-output-file*
     #:exists 'truncate
     #:mode 'text
     (build-path "bundle" "racket" "README.txt")
     (lambda (o)
       (display readme o))))
  (nsis-generate exe-path
                 human-name
                 (version)
                 platform
                 makensis
                 #:versionless versionless?
                 #:extension-registers (get-extreg "bundle/racket")
                 #:start-menus (get-startmenu "bundle/racket")
                 #:auto-launch (get-auto-launch "bundle/racket"))
  exe-path)
