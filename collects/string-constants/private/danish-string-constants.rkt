#|

When modifying the string constants files,
please adhere to these guidelines:

- All the entries in english-string-constants.rkt have the same format
  (name string).  If the purpose of an entry you are adding to the
  file is not clear from just the name and string, put a comment next
  to the entry explaining what the string is going to be used for and
  in what context.
  That's especially true for strings that contain things like "~a".
  Systematically describe what the "~a" is going to be replaced with.
  When we have to translate strings like "deleting ~a", we translators
  need to know what "~a" is going to be (in particular, in some
  languages like French, we need to know whether the "~a" is going to
  be a masculine or feminine word, or whether it's going to be
  singular or plural, etc).

- When adding a bunch of new entries, put together in a section the
  entries that logically belong together.  Give a title to your
  section, so we have an idea of what the strings are for.  Don't mix
  in the same section strings that have nothing to do with each other,
  that's only going to confuse us.  Do not start a new section if
  there's already one that deals with the same thing.  Dumping all the
  new entries in random order at the end of the file is not a good way
  to have your new entries translated in a timely manner...

- Before adding strings for your new pet tool, check whether you can't
  re-use strings that already exist.  There's no need for yet another
  "Ok" string...

- If you modify an existing string in english-string-constants.rkt, go
  through all the *-string-constants.rkt files for the other languages,
  comment out the old version of the modified string in each of these
  files, and put a short comment there telling us the English string
  has changed and needs to be re-translated.  Do not erase the old
  version, it might help us translate the new one.  Do not move it
  either.  Just comment it out and add the short comment.  After the
  next git update DrRacket will automatically tell us translators that
  a new string needs to be translated, we will find your comment in
  the file, and know what to do.
	Some evil evil people might think that, since DrRacket automatically
  informs us of new strings to be translated, an easier thing to do
  when modifying an existing string would be to simply rename it at
  the same time.  This works, except that if you do that, we
  translators will get two warnings from DrRacket:
		language english had but french does not:
		(new-name "New String")
		language french had but english does not:
		(old-name "Old String")
  then we translators will be left to wonder whether the two things
  are related or not, and whether we can safely base our translation
  of "New String" on the translation of "Old String" (since the two
  strings are likely to be close in meaning).  Worse, we might not
  even realize the two strings are related and translate "New String"
  from scratch, just to realize later that it's only a variation of
  "Old String".  I can tell you that nothing pisses off a translator
  more than having to translate pretty much the same string twice
  just because *you* were too lazy to inform us that it was just a
  slight modification to an existing string rather than an entirely
  new one.  Conclusion: do not change the name of a string unless you
  have some really good reason to do so.

- Please think hard before choosing a string and make sure it means
  what you want it to mean.  That way you won't have to change it
  later, and we won't have to retranslate it.

- Please think hard before choosing the name for a string.  Use
  meaningful names.  "error" or "ok" are not meaningful names.  Prefix
  all related names with a common prefix (the name of your tool or
  module).  String names are not the right place to save on typing.

- If, for some reason, you really have to change the name of a string
  (like, because the original name you gave it really sucked...),
  change the name of the string in *ALL* the *-string-constants.rkt
  files.  That's a modification you can do without the help of a
  translator, so do it yourself.  It's not the job of the translators
  to clean up your naming mess for you.  Besides, you are the one who
  knows what you changed, so leaving the translators having to guess
  what you did is Not Nice(tm).

- If, for some reason, you need to remove a string (i.e. you changed
  your code and don't need the string anymore), remove the string in
  *ALL* the *-string-constants.rkt files.  Again, you don't need the
  help of a translator to do that.  If you're not sure whether you
  might need the string in the future or not, just comment it out in
  *ALL* the files.

|#

(module danish-string-constants "string-constant-lang.rkt"
  ;;; when translating this constant, substitute name of actual language for `English'
  (is-this-your-native-language "Foretrækker du dansk?")
  
  (are-you-sure-you-want-to-switch-languages
      "Dette ændrer sproget i den grafiske brugerflade. Er du sikker?")
  
  (interact-with-drscheme-in-language "Arbejd med DrRacket på Dansk")
  
  ;; these two should probably be the same in all languages except English.
  ;; they are the button labels (under macos and windows, respectively)
  ;; that go the with the string above.
  (accept-and-quit "Accepter og afslut")
  (accept-and-exit "Accepter og afslut")
  
  ;;; general purpose (DrRacket is hereby a word in every language, by decree of Robby :)
  (plt "PLT")
  (drscheme "DrRacket")
  (ok "Ok")
  (cancel "Fortryd")
  (abort "Afbryd")
  (untitled "Uden navn")
  (untitled-n "Uden navn ~a")
  (warning "Advarsel")
  (error "Fejl")
  (close "Luk") ;; as in, close an open window
  (stop "Stop")
  (&stop "&Stop") ;; for use in button and menu item labels, with short cut.
  (are-you-sure-delete? "Er du sikker på, du vil slette ~a?") ;; ~a is a filename or directory name
  (ignore "Ignorer")
  (revert "Gendan")
  
  ;; label for a generic check box, often supported on dialogs
  ;; that ask a binary choice of the user. If checked, the
  ;; dialog isn't going to be shown again.
  ;; One version for always using the current choice:
  (dont-ask-again-always-current "Spørg ikke igen (brugt altid nuværende valg)")
  ;; One generic version (ie, on the Quit DrRacket dialog)
  (dont-ask-again                "Spørg ikke igen")
  
  ;;; important urls
  (web-materials "Relaterede websites") ;; menu item title
  (tool-web-sites "Tool Web Sites")   ;; menu item title
  (plt-homepage "Racket")
  (pbd-homepage "Program by Design")
  
  ;;; bug report form
  (cancel-bug-report? "Fortryd afsendelse af fejlrapport?")
  (are-you-sure-cancel-bug-report?
   "Er du sikker, du ikke vil afsende denne fejlrapport?")
  (bug-report-form "Fejlrapportering")
  (bug-report-field-name "Navn")
  (bug-report-field-email "E-mail")
  (bug-report-field-summary "Resumé")
  (bug-report-field-severity "Alvorlighedsgrad")
  (bug-report-field-class "Klasse")
  (bug-report-field-description "Beskrivelse")
  (bug-report-field-reproduce1 "Skridt for at")
  (bug-report-field-reproduce2 "reproducere fejlen")
  (bug-report-field-environment "Omgivelse")
  (bug-report-field-docs-installed "Installeret dokumentation")
  (bug-report-field-collections "Samlinger (Collections)")
  (bug-report-field-human-language "Modersmål")
  (bug-report-field-memory-use "Hukommelsesforbrug")
  (bug-report-field-version "Version")
  (bug-report-synthesized-information "Indsamlet information")  ;; dialog title
  (bug-report-show-synthesized-info "Vis indsamlet information")
  (bug-report-submit "Send")
  (bug-report-submit-menu-item "Send fejlrapport...") ;; in Help Menu (drs & help desk)
  (error-sending-bug-report "Fejl under afsendelse af fejlrapport")
  (error-sending-bug-report-expln "Der opstod en fejl ved afsendelse af fejlrapporten. Hvis din internetforbindelse ellers er velfungerende, besøg venligst:\n\n    http://bugs.racket-lang.org/\n\nog send fejlrapporten ved hjælp af vores online fejlrapporteringsside. Vi er kede af besværet.\n\nFejlmeddelelsen er:\n~a")
  (illegal-bug-report "Ugyldig fejlrapport")
  (pls-fill-in-field "Udfyld venligst feltet \"~a\" ")
  (malformed-email-address "Ugyldig e-mail-adresse")
  (pls-fill-in-either-description-or-reproduce "Udfyld venligst enten beskrivelsesfeltet eller feltet til reproduktion af fejlen.")
  
  ;;; check syntax
  (check-syntax "Syntakstjek")
  (cs-italic "Kursiv")
  (cs-bold "Fed")
  (cs-underline "Understreget")
  (cs-change-color "Skift farve")
  (cs-tack/untack-arrow "Pile til/fra")
  (cs-jump-to-next-bound-occurrence "Hop til næste bundne forekomst")
  (cs-jump-to-binding "Hop til den bindende forekomst")
  (cs-jump-to-definition "Hop til definition")
  (cs-error-message "Fejlmeddelelse")
  (cs-open-file "Åbn ~a")
  (cs-rename-var "Omdøb ~a")
  (cs-rename-id "Omdøb navn")
  (cs-rename-var-to "Omdøb ~a til:")
  (cs-name-duplication-error "Det nye navn, du har valgt, ~s, er i konflikt med et allerede eksisterende navn i dette virkefelt.")
  (cs-rename-anyway "Rename ~a to:")
  (cs-status-init "Syntakstjek: Initialiserer omgivelser for brugerkode")
  (cs-status-coloring-program "Syntakstjek: farver udtryk")
  (cs-status-eval-compile-time "Syntakstjek: evaluering på oversættelsestid")
  (cs-status-expanding-expression "Syntakstjek: Udvider (expanding) udtryk")
  (cs-mouse-over-import "bindingen ~a er importeret fra ~s")
  ;(cs-mouse-over-variable-import "variabel ~s importeret fra ~s")
  ;(cs-mouse-over-syntax-import "syntaks ~s importeret fra ~s")
  
  (cs-lexical-variable "leksikalsk variabel")
  (cs-imported-variable "importeret variabel")
  
  ;;; info bar at botttom of drscheme frame
  (collect-button-label "GC")
  (read-only "Skrivebeskyttet")
  (auto-extend-selection "Auto-udvid")
  (overwrite "Overskriv")
  (running "kører")
  (not-running "kører ikke")
  
  ;;; misc
  (welcome-to-something "Velkommen til ~a")
  
  ; this appears in the drscheme about box.
  (welcome-to-drscheme-version/language "Velkommen til DrRacket, version ~a, ~a")
  
  ; these appear on subsequent lines in the `Help|Welcome to DrRacket' dialog.
  (welcome-to-drscheme "Velkommen til DrRacket")
  
  (goto-line "Gå til linje")
  (goto-line-invalid-number
   "~a er ikke et gyldigt linjenummer. Det skal være et heltal mellem 1 og ~a")
  (goto-position "Gå til position")
  (no-full-name-since-not-saved
   "Filen har ikke et fuldt navn, for den er endnu ikke gemt.")
  (cannot-open-because-dne "Kan ikke åbne ~a, for den findes ikke")
  
  (needs-execute-language-changed
   "ADVARSEL: Sproget er skiftet. Klik Kør.")
  (needs-execute-teachpack-changed
   "ADVARSEL: Undervisningspakkerne er ændrede. Klik Kør.")
  (needs-execute-defns-edited
   "ADVARSEL: Definitionsvinduet er ændret. Klik Kør.")
  
  (file-is-not-saved "Filen \"~a\" er endnu ikke gemt.")
  (save "Gem")
  (close-anyway "Luk alligevel")
  (clear-anyway "Rens Anyway")
  
  ;; menu item title
  (log-definitions-and-interactions "Log definitioner og interaktioner...")
  (stop-logging "Stop logføring")
  (please-choose-a-log-directory "Vælg en mappe til loggen")
  (logging-to "Log til: ")
  (erase-log-directory-contents "Slet indholdet af log-mappen: ~a?")
  (error-erasing-log-directory "Fejl under sletning ad log-mappens indhold.\n\n~a\n")
  
  ;; modes
  (mode-submenu-label "Tilstande")
  (scheme-mode "Racket-tilstand")
  (text-mode "Tekst-tilstand")
  
  (scheme-mode-color-symbol "Symbol")
  (scheme-mode-color-keyword "Nøgleord")
  (scheme-mode-color-comment "Kommentar")
  (scheme-mode-color-string "Strenf")
  (scheme-mode-color-constant "Konstant")
  (scheme-mode-color-parenthesis "Parentes")
  (scheme-mode-color-error "Fejl")
  (scheme-mode-color-other "Andet")
  ;; the ~a is filled in with one of the above (scheme-mode-*)
  (syntax-coloring-choose-color "Vælg en farve for ~a")
  (preferences-colors "Farver") ;; used in the preferences dialog
  
  (url: "URL:")
  (open-url... "Åbn URL...")
  (open-url "Åbn URL")
  (browse... "Gennemse...")
  (bad-url "Ugyldig URL")
  (bad-url:this "Ugyldig URL: ~a")
  
  ;; Help Desk
  (help "Hjælp")
  (help-desk "Hjælpebord")
  (plt:hd:search "Søg")
  (plt:hd:feeling-lucky "Jeg føler mig heldig")
  (plt:hd:home "Hjælpebordets start") 
  ; next 3 are popup menu choices in help desk search frame
  (plt:hd:search-for-keyword "Nøgleord")
  (plt:hd:search-for-keyword-or-index "Nøgleord eller indekseret opslag")
  (plt:hd:search-for-keyword-or-index-or-text "Nøgleord, indekseret opslag eller tekst")
  (plt:hd:exact-match "præcis match")
  (plt:hd:containing-match "indeholdende match")
  (plt:hd:regexp-match "regulært udtryk")
  (plt:hd:find-docs-for "Find dokumentation om:")
  (plt:hd:search-stopped-too-many-matches "[Søgning afbrudt: for mange resultater")
  (plt:hd:nothing-found-for "Intet fundet om ~a")
  (plt:hd:and "og")
  (plt:hd:refresh "opdater")
  (plt:hd:refresh-all-manuals "opdater alle manualer")
  (plt:hd:manual-installed-date "(~a installeret)")
  ; Help Desk configuration
  ;; refreshing manuals
  (plt:hd:refresh-clearing-indices "Renser forgemte indekser")
  ;; should not mention `SVN' (plt:hd:refresh-done "Færdig med at opdatere SVN-manualer")
  (plt:hd:refreshing-manuals "Genhenter manualer")
  (plt:hd:refresh-downloading... "Henter ~a...")
  (plt:hd:refresh-deleting... "Sletter gammel version af ~a...")
  (plt:hd:refresh-installing... "Installerer ny version af ~a...")
  (plt:hd:refreshing-manuals-finished "Færdig.")
  (plt:hd:about-help-desk "Om hjælpebord")
  (plt:hd:help-desk-about-string
   "Hjælpebordet er en fuldstændig kilde af information om PLT-software, inklusive DrRacket, MzScheme og MrEd.\n\nVersion ~a\nCopyright (c) ~a-~a PLT")
  (plt:hd:help-on-help "Hjælp om hjælp")
  (plt:hd:help-on-help-details "For hjælp om Hjælpebord, følg linket `How to use Help Desk'-linket på hjælpebordets startside.")
  (reload "Opdater") ;; refresh the page in a web browser
  (plt:hd:ask-about-separate-browser
   "Du har valgt et link til indhold på internettet. Vil du se det i Hjælpebordsbrowseren eller i et separat browserprogram?")
  (plt:hd:homebrew-browser "Hjælpebordsbrowser") ;; choice for the above string (in a button)
  (plt:hd:separate-browser "Separat browser") ;; other choice for the above string (also in a button)
  (plt:hd:external-link-in-help "Eksterne URL'er i hjælp")
  (plt:hd:use-homebrew-browser "Brug hjælpebordsbrowseren til eksterne URL'er")
  (plt:hd:new-help-desk "Nyt hjælpebord")
  
  ;; in the Help Desk language dialog, title on the right.
  (plt:hd:manual-search-ordering "Søgeorden")
  
  ;; in the help-desk standalone font preference dialog, on a check box
  (use-drscheme-font-size "Brug DrRacket's fontstørrelse")
  
  ;; in the preferences dialog in drscheme there is example text for help desk font size.
  ;; clicking the links in that text produces a dialog with this message
  (help-desk-this-is-just-example-text
   "Dette er bare en eksempeltekst til indstilling af fontstørrelsen. Åben det rigtige hjælpebord (fra Hjælp-menuen) for at følge disse links.")
  
  ; help desk htty proxy
  (http-proxy "HTTP-proxy")
  (proxy-direct-connection "Direkte forbindelse")
  (proxy-use-proxy "Benyt proxyen:")
  (proxy-host "Host")
  (proxy-port "Port")
  (proxy-bad-host "Ugyldig proxyvært")
  
  ;; browser
  (rewind-in-browser-history "Tilbage")
  (forward-in-browser-history "Fremad")
  (home "Hjem")
  (browser "Browser")
  (external-browser-choice-title "Ekstern browser") ; title for radio-button set
  (browser-command-line-label "Kommandolinje:") ; label for radio button that is followed by text boxes
  (choose-browser "Vælg en browser")
  (no-browser "Spørg senere") ; English changed from "None" to "Ask Later"
  (browser-cmdline-expl-line-1 "(Kommandolinjen konstrueret ved at sætte den forudgående tekst, URL'en,") ; explanatory text for dialog, line 1
  (browser-cmdline-expl-line-2 "og den efterfølgende tekst sammen uden brug af mellemrum mellem dem.)") ; ... line 2. (Anyone need more lines?)
  (install? "Installér?")  ;; if a .plt file is found (title of dialog)
  (you-have-selected-an-installable-package "Du har valgt en pakke, som kan installeres.")
  (do-you-want-to-install-it? "Vil du installere den?")
  (paren-file-size "(Filen fylder ~a bytes)")
  (download-and-install "Download og installér") ;; button label
  (download "Download") ;; button label
  (save-downloaded-file/size "Gem den downloadede fil (~a bytes) som ") ;; label for get-file dialog
  (save-downloaded-file "Gem den downloadede file som")  ;; label for get-file dialog
  (downloading "Downloader") ;; dialog title
  (downloading-file... "Downloader fil...")
  (package-was-installed "Pakken blev installeret.")
  (download-was-saved "Den downloadede fil blev gemt.")
  
  (install-plt-file-menu-item... "Installer .plt-fil...")
  (install-plt-file-dialog-title "Installer .plt-fil")
  (install-plt-web-tab "Web")
  (install-plt-file-tab "Fil")
  (install-plt-filename "Filnavn:")
  (install-plt-url "URL:")
  
  ;; install plt file when opened in drscheme strings
  (install-plt-file "Installer ~a eller åbn for at redigere?")
  (install-plt-file/yes "Installer")
  (install-plt-file/no "Rediger")
  
  (plt-installer-progress-window-title "Installeringsfremgang")
  (plt-installer-abort-installation "Afbryd installation") ;; button label
  (plt-installer-aborted "Afbrudt.") ;; msg that appears in the installation window when installation is aborted
  
  
  ;;; about box
  (about-drscheme-frame-title "Om DrRacket")
  
  ;;; save file in particular format prompting.
  (save-as-plain-text "Gem denne fil som tekt?")
  (save-in-drs-format "Gem denne fil i det DrRacket-specifikke ikke-tekst format?")
  (yes "Ja")
  (no "Nej")
  
  ;;; preferences
  (preferences "Indstillinger")
  (error-saving-preferences "Fejl under lagring af indstillinger: ~a")
  (error-reading-preferences "Fejl ved indlæsning af indstillinger")
  (scheme-prefs-panel-label "Racket")
  (warnings-prefs-panel-label "Advarsler")
  (editor-prefs-panel-label "Redigering")
  (general-prefs-panel-label "Generelt")
  (highlight-parens "Farv mellem samhørende parenteser")
  (fixup-open-brackets "Korriger automatisk [ -parentereser")
  (fixup-close-parens  "Korriger automatisk afsluttende parenteser")
  (flash-paren-match "Blink ved samhørende parenteser")
  (auto-save-files "Auto-save filer")
  (backup-files "Backup filer")
  (map-delete-to-backspace "Bind delete til backspace")
  (verify-exit "Spørg ved nedlukning")
  (ask-before-changing-format "Spørg før ændring af lagringsformat")
  (wrap-words-in-editor-buffers "Ombryd ord i redigeringsbufferne")
  (show-status-line "Vis statuslinjen")
  (count-columns-from-one "Tæl søjlenumger fra en.")
  (display-line-numbers "Vis linjenumre i bufferen; ikke tegn-offsets")
  (show-line-and-column-numbers "Vis linje og søjlenumre") ; used for popup menu; right click on line/column box in bottom of drs window
  (show-character-offsets "Vis tegnnummerering") ; used for popup menu; right click on line/column box in bottom of drs window
  (enable-keybindings-in-menus "Slå tastaturgenveje i menuer til")
  (option-as-meta "Brugtion option-tasten som meta") ;; macos/macos x only  
  (reuse-existing-frames "Genbrug gamle vinduer, når nye filer åbnes")
  (default-fonts "Standardskrifttyper")
  (paren-match-color "Parentesfremhævningsfarve") ; in prefs dialog
  (online-coloring-active "Interaktiv syntaksfarvelægning")
  (open-files-in-tabs "Åbn filer i separate faneblade (ikke separate vinduer)")
  (show-interactions-on-execute "Vis automatisk interaktionsvinduet ved kørsel")
  (limit-interactions-size "Afgræns interaktionernes størrelse")
  (background-color "Baggrundsfarve")
  (default-text-color "Default-tekst") ;; used for configuring colors, but doesn't need the word "color"
  (choose-a-background-color "Vælg en baggrundsfarve")
  
  
  ; title of the color choosing dialog
  
  ; should have entire alphabet
  (font-example-string "Høj bly gom vandt fræk sexquiz på wc.")
  
  (change-font-button-label "Skift")
  (fonts "Fonte")
  
  ; filled with type of font, eg modern, swiss, etc.
  (choose-a-new-font "Vælg en ny \"~a\" font")
  
  (font-size-slider-label "Størrelse")
  (restart-to-see-font-changes "Genstart for at se fontændringer")
  
  (font-prefs-panel-title "Font")
  (font-name "Fontnavn")
  (font-size "Fontstørrelse")
  (set-font "Anvend font...")
  (font-smoothing-label  "Skriftypeudglatning")
  (font-smoothing-none "Ingen")
  (font-smoothing-some "Nogen")
  (font-smoothing-all "Fuld")
  (font-smoothing-default "Brug systemindstillinger")
  (select-font-name "Vælg skrifttypenavn")
  (example-text "Eksempeltekst:")
  (only-warn-once "Advar kun én gang, når kørsler og interaktioner er ude af trit")
  
  ; warning message when lockfile is around
  (waiting-for-pref-lock "Venter på indstillingernes låsefil...")
  (pref-lock-not-gone
   "Indstillingernes låsefil:\n\n   ~a\n\nforhindrer indstillingerne i at blive gemt. Sørg for, at du ikke kører Racket-programmer og slet denne fil.")
  (still-locked-exit-anyway? "Indstillingerne blev ikke gemt rigtigt. Afslut alligevel?")
  
  ;;; indenting preferences panel
  (indenting-prefs-panel-label "Indrykning")
  (indenting-prefs-extra-regexp "Ekstra regexp")
  
  (square-bracket-prefs-panel-label "Firkantet parentes")
  
  ; filled with define, lambda, or begin
  (enter-new-keyword "Indtast et nyt ~a-lignende nøgleord:")
  (x-keyword "~a nøgleord")
  (x-like-keywords "~a-lignende nøgleord")
  
  ; used in Square bracket panel
  (skip-subexpressions "Antal under-udtryk, der skal skippes")
  
  (expected-a-symbol "forventede et symbol, fandt: ~a")
  (already-used-keyword "\"~a\" er allerede et nøgleord med speciel indrykning")
  (add-keyword "Tilføj")
  (remove-keyword "Fjern")
  
  ;;; find/replace
  (find-and-replace "Søg og erstat")
  (find "Søg")
  (replace "Erstat")
  (dock "Minimer")
  (undock "Gendan")
  (replace&find-again "Erstat og Søg igen") ;;; need double & to get a single &
  (forward "Frem")
  (backward "Tilbage")
  (hide "Skjul")
  (find-case-sensitive "Forskel på store og små bogstaver")  ;; the check box in both the docked & undocked search
  
  ;;; multi-file-search
  (mfs-multi-file-search-menu-item "Søg i filer...")
  (mfs-string-match/graphics "Streng match (klarer filer med grafik)")
  (mfs-regexp-match/no-graphics "Regulært udtryk (kun rå tekstfiler)")
  (mfs-searching... "Søger...")
  (mfs-configure-search "Søgeindstillinger") ;; dialog title
  (mfs-files-section "Filer")   ;; section in config dialog
  (mfs-search-section "Søg") ;; section in config dialog
  (mfs-dir "Mappe")
  (mfs-recur-over-subdirectories "Rekursivt i undermapper")
  (mfs-regexp-filename-filter "Regexp filnavnsfilter")
  (mfs-search-string "Søgestreng")
  (mfs-drscheme-multi-file-search "Søgning i flere filer - DrRacket") ;; results window and error message title
  (mfs-not-a-dir "\"~a\" er ikke en mappe")
  (mfs-open-file "Åbn fil")
  (mfs-stop-search "Stop søgning")
  (mfs-case-sensitive-label "Forskel på store og små bogstaver")
  (mfs-no-matches-found "Intet passende fundet.")
  (mfs-search-interrupted "Søgning afbrudt.")
  
  ;;; reverting a file
  (are-you-sure-revert
   "Er du sikker på, at du vil gendanne denne fil? En gendannen kan ikke fortrydes.")
  (are-you-sure-revert-title "Gendan?")
  
  ;;; saving a file
  ; ~a is filled with the filename
  (error-saving "Fejl under lagring") ;; title of error message dialog
  (error-saving-file/name "Der var en fejl ved lagring af ~a.")
  (error-loading "Indlæsefejl")
  (error-loading-file/name "Fejl ved læsning af ~a.")
  (unknown-filename "<< ukendt >>")
  
  ;;; finder dialog
  (must-specify-a-filename "Du skal angive et filnavn")
  (file-does-not-exist "Filen \"~a\" findes ikke.")
  (ask-because-file-exists "Filen \"~a\" findes allerede. Erstat den?")
  (dne-or-cycle "Filen \"~a\" indeholder en ikke-eksisterende mappe eller en cykel.")
  (get-file "Hent fil")
  (put-file "Gem fil")
  (full-pathname "Fuldt navn med sti")
  (show-dot-files "Vis filer og mapper, som begynder med punktum.")
  (up-directory-button-label "Op")
  (add-button-label "Tilføj") ;;; for multi-file selection
  (add-all-button-label "Tilføj alle") ;;; for multi-file selection
  (remove-button-label "Fjern") ;;; for multi-file selection
  (file-wrong-form "Det filnavn har ikke den rigtige form.")
  (select-files "Vælg filer")
  (select-file "Vælg en fil")
  (dir-dne "Den mappe findes ikke.")
  (file-dne "Den fil findes ikke.")
  (empty-filename "Filnavnet skal indeholde et tegn.")
  (that-is-dir-name "Det er et mappenavn.")
  
  ;;; raw menu names -- these must match the
  ;;; versions below, once the &s have been stripped.
  ;;; if they don't, DrRacket's menus will appear
  ;;; in the wrong order.
  (file-menu "Filer")
  (edit-menu "Rediger")
  (help-menu "Hjælp")
  (windows-menu "Vinduer")
  
  ;;; menus
  ;;; - in menu labels, the & indicates a alt-key based shortcut.
  ;;; - sometimes, things are stuck in the middle of
  ;;; menu item labels. For instance, in the case of
  ;;; the "Save As" menu, you might see: "Save Definitions As".
  ;;; be careful of spacing, follow the English, if possible.
  ;;; - the ellipses in the `after' strings indicates that
  ;;; more information is required from the user before completing
  ;;; the command.
  
  (file-menu-label "&Filer")
  
  (new-info  "Opret en ny fil")
  (new-menu-item "&Ny")
  (new-...-menu-item "&Ny...")
  
  (open-info "Åbn en fil fra disk")
  (open-menu-item "&Åbn...")
  
  (open-recent-info "En liste af filer brugt for nylig")
  (open-recent-menu-item "Åbn gammel")
  
  (revert-info "Gendan diskkopien af denne fil")
  (revert-menu-item "Gendan")
  
  (save-info "Gem filen på disk")
  (save-menu-item "&Gem")
  
  (save-as-info "Gemmer filen med et nyt filnavn")
  (save-as-menu-item "Gem &Som...")
  
  (print-info "Udskriv filen på printer")
  (print-menu-item "&Print...")
  
  (close-info "Luk denne fil")
  (close-menu-item "&Luk")
  
  (quit-info "Luk alle vinduer")
  (quit-menu-item-windows "E&xit")  ; TODO
  (quit-menu-item-others "&Quit")
  
  (edit-menu-label "&Rediger")
  
  (undo-info "Fortryd sidste handling")
  (undo-menu-item "&Fortryd")
  
  (redo-info "Fortryd det seneste fortryd")
  (redo-menu-item "&Omgør")
  
  (cut-info "Flyt det sidst valgte til klippebordet til senere indsættelse")
  (cut-menu-item "K&lip")
  
  (copy-info "Kopier det sidst valgte til klippebordet til senere indsættelse")
  (copy-menu-item "&Kopier")
  
  (paste-info "Erstat det valgte med det senest kopierede eller klippede")
  (paste-menu-item "&Indsæt")
  
  (clear-info "Slet de valgte elementer uden at påvirke klippebordet eller indsætning")
  (clear-menu-item-windows "&Rens")
  
  (select-all-info "Marker alt")
  (select-all-menu-item "Marker &alt")
  
  (find-info "Søg efter streng")
  (find-menu-item "Søg...")
  
  (find-again-info "Søg efter samme streng som før")
  (find-again-menu-item "Søg igen")
  
  (replace-and-find-again-info "Erstat den nuværende tekst og gentag søgningen")
  (replace-and-find-again-menu-item "Erstat og søg igen")
  
  (preferences-info "Rediger dine indstillinger")
  (preferences-menu-item "Indstillinger...")
  
  (keybindings-info "Vis de gældende, aktive tastebindinger")
  (keybindings-menu-item "Tastebindinger")
  (keybindings-show-active "Vis aktive tastebindinger")
  (keybindings-frame-title "Tastebindinger")
  (keybindings-sort-by-name "Sorter efter navn")
  (keybindings-sort-by-key "Sorter efter taster")
  (keybindings-add-user-defined-keybindings "Tilføj en brugerdefineret tastebindinger...")
  (keybindings-add-user-defined-keybindings/planet "Tilføj brugerdefinerede tastebindinger fra PLaneT...")
  (keybindings-menu-remove "Fjern ~a")
  (keybindings-choose-user-defined-file "Vælg en fil med tastebindinger.")
  (keybindings-planet-malformed-spec "PLaneT-specifikationen er ukorrekt: ~a") ; the string will be what the user typed in
  (keybindings-type-planet-spec "Indtast en PLaneT require-specifikation (uden `require')")
  
  ; first ~a will be a string naming the file or planet package where the keybindings come from;
  ; second ~a will be an error message
  (keybindings-error-installing-file "Error when installing the keybindings ~a:\n\n~a")
  
  (user-defined-keybinding-error "Fejl ved kørsel af brugerdefineret tastebinding ~a\n\n~a")
  (user-defined-keybinding-malformed-file "Filen ~a indeholder ikke et modul skrevet i sproget framework/keybinding-lang.")  
  
  ;; menu items in the "special" menu
  (insert-text-box-item "Indsæt tekstkasse")
  (insert-image-item    "Indsæt billede...")
  (insert-comment-box-menu-item-label "Indsæt kommentarkasse")
  (insert-lambda "Indsæt &Lambda")
  
  (wrap-text-item       "Ombryd tekst")
  
  (windows-menu-label "&Vinduer")
  (bring-frame-to-front "Skift til andet vindue")       ;;; title of dialog
  (bring-frame-to-front... "Skift til andet vindue...") ;;; corresponding title of menu item
  (most-recent-window "Sidst besøgte vindue")
  
  (view-menu-label "&Vis")
  (show-overview "Vis programkontur")
  (hide-overview "Skjul programkontur")
  (show-module-browser "Vis moduloversigt")
  (hide-module-browser "Skjul moduloversigt")
  
  (help-menu-label "&Hjælp")
  (about-info "Akkrediteringer og detaljer om dette program")
  (about-menu-item "Om...")
  
  ;; open here's new menu item
  (create-new-window-or-clear-current
   "Vil du have et nyt vindue, eller rense det gamle?")
  (clear-current "Rens nuværende")
  (new-window "Nyt vindue")
  
  ;;; exiting and quitting are you sure dialog
  ;;; (exit is used on windows, quit on macos. go figure)
  (exit "Afslut")
  (quit "Afslut")
  ;;; in are-you-sure-format, either exit or quit is filled in (from above)
  ;;; based on the platform drscheme is running on.
  (are-you-sure-exit "Er du sikker, du vil afslutte?")
  (are-you-sure-quit "Er du sikker, du vil afslutte?")
  ; these next two are only used in the quit/exit dialog
  ; on the button whose semantics is "dismiss this dialog".
  ; they are there to provide more flexibility for translations
  ; in English, they are just cancel.
  (dont-exit "Fortryd") 
  (dont-quit "Fortryd")
  
  ;;; autosaving
  (error-autosaving "Fejl under autosaving \"~a\".")
  (autosaving-turned-off "Autosaving er slået fra \nindtil filen filen gemmes.")
  (recover-autosave-files-frame-title "Gendan backupfiler")
  (autosave-details "Detaljer")
  (autosave-recover "Gendan")
  (autosave-unknown-filename "<<ukendt>>")
  
  ;; these are labels in a dialog that drscheme displays
  ;; if you have leftover autosave files. to see the dialog,
  ;; start up drscheme and modify (but don't save) a file
  ;; (also, do this with an unsaved file). Wait for the autosave
  ;; files to appear (typically 5 minutes). Kill DrRacket
  ;; and restart it. You'll see the dialog
  (autosave-autosave-label: "Autosave-fil:")
  (autosave-original-label: "Original fil:")
  (autosave-autosave-label "Autosave-fil")
  (autosave-original-label "Original fil")
  (autosave-compare-files "Sammenlign autosavede filer")
  
  (autosave-show-autosave "Autosave-fil") ;; title of a window showing the autosave file
  
  (autosave-explanation "DrRacket fandt autosavede filer, som måske indeholde ugemt arbejde.")
  
  (autosave-recovered! "Gendannet!") ;; status of an autosave file
  (autosave-deleted "Slettet")       ;; status of an autosave file
  
  (autosave-error-deleting "Fejl under sletning ~a\n\n~a") ;; first is a filename, second is an error message from mz.
  (autosave-delete-button "Slet")
  (autosave-delete-title "Slet")  ;; title of a dialog asking for deletion confirmation
  (autosave-done "Færdig")
  
  ;; appears in the file dialog
  (autosave-restore-to-where? "Vælg et sted til at gemme autosave-filen.")
  
  
  ;;; file modified warning
  (file-has-been-modified
   "Der er rettet i filen, siden den sidst blev gemt. Overskriv ændringerne?")
  (overwrite-file-button-label "Overskriv")
  
  (definitions-modified
    "Definitionsteksten er blevet ændret i filsystemet; gem venligst eller brug 'vend tilbage' for at bruge den gamle version")
  (drscheme-internal-error "Intern fejl i DrRacket")
  
  ;;; tools
  (invalid-tool-spec "Værkstøjsspecifikationen i collection ~a's info.rkt filen er ugyldig. Forventede enten en streng eller en ikke-tom liste af strenge, fik: ~e")
  (error-invoking-tool-title "Fejl ved kørsel af værktøj ~s;~s")
  (tool-tool-names-same-length "forventede `tool-names' og `tools' var to lister af samme længde i info.rkt for ~s, fik ~e og ~e")
  (tool-tool-icons-same-length  "forventede `tool-icons' og `tools' var to lister af samme længde i info.rkt  for ~s, fik ~e and ~e")
  (tool-tool-urls-same-length
   "forventede `tool-urls' og `tools' var lister af samme længde  i info.rkt-filen for ~s, fik ~e og ~e")
  (error-getting-info-tool  "fejl ved hentning af info.rkt file for ~s")
  (tool-error-phase1 "Fejl i fase 1 for værktøjet ~s; ~s")
  (tool-error-phase2 "Fejl i fase 2 for værktøjet ~s; ~s")
  
  
  ;;; define popup menu
  (end-of-buffer-define "<< slutning af buffer >>")
  (sort-by-name "Sorter efter navn")
  (sort-by-position "Sorter efter rækkefølge i programteksten")
  (no-definitions-found "<< ingen definitioner fundet >>")
  (jump-to-defn "Hop til definitionen af ~a")
  
  (recent-items-sort-by-age "Sorter efter Alder")
  (recent-items-sort-by-name "Sorter efter Navn")
  
  
  ;;; view menu
  (hide-definitions-menu-item-label "Skjul &Definitioner")
  (show-definitions-menu-item-label "Vis &Definitioner")
  (definitions-menu-item-help-string "Vis/Skjul definitionsvinduet")
  (show-interactions-menu-item-label "Vis &Interaktioner")
  (hide-interactions-menu-item-label "Skjul &Interaktioner")
  (interactions-menu-item-help-string "Vis/Skjul interaktionsvinduet")
  (show-toolbar "Vis &værktøjslinjen")
  (hide-toolbar "Skjul &værktøjslinjen")
  
  ;;; file menu
  (save-definitions-as "Gem definitioner som...")
  (save-definitions "Gem definitioner")
  (print-definitions "Udskriv definitioner...")
  (about-drscheme "Om DrRacket")
  (save-other "Gem andet")
  (save-definitions-as-text "Gem definitioner som tekst...")
  (save-interactions "Gem interaktioner")
  (save-interactions-as "Gem interaktioner som...")
  (save-interactions-as-text "Gem interaktioner som tekst...")
  (print-interactions "Udskriv interaktioner...")
  (new-tab "Nyt faneblad")
  (close-tab "Luk faneblad")
  
  ;;; edit-menu
  (split-menu-item-label "&Split")
  (collapse-menu-item-label "K&ollaps")
  
  ;;; language menu
  (language-menu-name "&Sprog")
  
  ;;; scheme-menu
  (scheme-menu-name "Ra&cket")
  (execute-menu-item-label "Kør")
  (execute-menu-item-help-string "Genstart programmet i definitionsvinduet")
  (break-menu-item-label "Afbryd")
  (break-menu-item-help-string "Afbryd den nuværende evaluering")
  (kill-menu-item-label "Slå ihjel")
  (kill-menu-item-help-string "Slå den nuværende evaluering ihjel")
  (clear-error-highlight-menu-item-label "Fjern fejlfarvelægningen")
  (clear-error-highlight-item-help-string "Fjerne den pinke farvelægning af fejlene")
  (reindent-menu-item-label "&Indryk igen")
  (reindent-all-menu-item-label "Indryk &alt igen")
  (semicolon-comment-out-menu-item-label "&Udkommenter med semikolonner")
  (box-comment-out-menu-item-label "&Udkommenter med en kasse")
  (uncomment-menu-item-label "&Afkommenter")
  
  (convert-to-semicolon-comment "Konverter til en semikolon-kommentar")
  
  
  ;;; executables
  (create-executable-menu-item-label "Lav binær kørselfil...")
  (create-executable-title "Lav binær kørselsfil")
  (must-save-before-executable "Du skal gemme dit program, før du laver en binær fil")
  (save-a-mred-launcher "Gem en GRacket-starter")
  (save-a-mzscheme-launcher "Gem en Racket-starter")
  (save-a-mred-stand-alone-executable "Gem en selvstændig GRacket-kørselsfil")
  (save-a-mzscheme-stand-alone-executable "Gen en selvstændig Racket kørselsfil")
  (save-a-mred-distribution "Gem en GRacket-distribution")
  (save-a-mzscheme-distribution "Gem en Racket-distribution")
  
  (definitions-not-saved "Definitionsvinduet har ikke været gemt. Den binære kørselsfil vil bruge den senest gemte version af definitionsvinduet. Fortsæt?")
  (launcher "Starter")
  ;; The "-explanatory-label" variants are the labels used for the radio buttons in
  ;;  the "Create Executable..." dialog for the "(module ...)" language.
  (launcher "Starter")
  (launcher-explanatory-label "Starter (kun til denne maskine, kører fra programtekst)")
  (stand-alone "Selvstændig")
  (stand-alone-explanatory-label "Selvstændig (kun til denne maskine, kører en oversat kopi)")
  (distribution "Distribution")
  (distribution-explanatory-label "Distribution (til at installere på andre maskiner)")
  (executable-type "Type")
  (executable-base "Base")
  (filename "Filnavn: ")
  (create "Opret")
  (please-specify-a-filename "Angive et filnavn til opretningen.")
  (~a-must-end-with-~a
      "Filnavnet ~a\n\n  ~a\n\ner ikke tilladt. Filnavnet må ende med \".~a\".")
  (macosx-executables-must-end-with-app
      "Filnavnet\n\n  ~a\n\ner ikke gyldigt. Under MacOS X skal en kørbar fil være en mappe, hvis navn slutter med .app.")
  (warning-directory-will-be-replaced
   "ADVARSEL: mappen:\n\n  ~a\n\nvil blive erstattet. Fortsæt?")
  
  (distribution-progress-window-title "Distributionsfremgang")
  (creating-executable-progress-status "Opretter kørbar fil til denne distribution...")
  (assembling-distribution-files-progress-status "Samler filer til distributionen...")
  (packing-distribution-progress-status "Pakker distributionen...")
  
  (create-servlet "Opret en Servlet...")
  
  ; the ~a is a language such as "module" or "algol60"
  (create-servlet-unsupported-language
   "Opret Servlet virker ikke med sproget ~a .")
  
  ;;; buttons
  (execute-button-label "Kør")
  (save-button-label "Gem")
  (break-button-label "Afbryd")
  
  ;;; search help desk popup menu
  (search-help-desk-for "Søg på hjælpebordet efter \"~a\"")
  (exact-lucky-search-help-desk-for "Præcis, heldig søgning på hjælpebordet efter \"~a\"")
  
  ;; collapse and expand popup menu items
  (collapse-sexp "Kollaps s-udtryk")
  (expand-sexp "Ekspander s-udtryk")
  
  ;;; fraction dialog
  (enter-fraction "Indtast brøk")
  (whole-part "Hele del")
  (numerator "Tæller")
  (denominator "Nævner")
  (invalid-number "Ugyldigt tal: skal være en være et eksakt, reelt, ikke-helt tal.")
  (insert-fraction-menu-item-label "Indsæt brøk...")
  
  ;; number snip popup menu
  (show-decimal-expansion "Vis som decimaltal")
  (show-mixed-fraction-view "Vis som blandet tal")
  (show-improper-fraction-view "Vis som uægte brøk")
  (show-more-decimal-places "Vis flere decimaler")
  
  ;;; TeachPack messages
  (select-a-teachpack "Vælg undervisningspakke")
  (clear-teachpack "Fjern undervisningspakken ~a")
  (teachpack-error-label "DrRacket - Undervisningspakkefejl")
  (teachpack-didnt-load "Undervisningspakkefilen ~a blev ikke hentet rigtigt.")
  (add-teachpack-menu-item-label "Tilføj Undervisningspakke")
  (clear-all-teachpacks-menu-item-label "Fjern alle undervisningspakker")
  (drscheme-teachpack-message-title "DrRacket Undervisningspakke")
  (already-added-teachpack "Undervisningspakken ~a er allerede tilføjet")
  
  ;;; Language dialog
  (introduction-to-language-dialog
   "Vælg venligst et sprog. Elever i de fleste begynderkurser bør vælge det foreslåede sprog.")
  (language-dialog-title "Vælg sprog")
  (case-sensitive-label "Forskel på store og små bogstaver")
  (output-style-label "Output-stil")
  (constructor-printing-style "Konstruktør")
  (quasiquote-printing-style "Kvasicitering")
  (write-printing-style "write")
  (print-printing-style "print")
  (sharing-printing-label "Vis deling i værdier")
  (use-pretty-printer-label "Indsæt linjeskift i printede værdier")
  (input-syntax "Input-syntaks")
  (dynamic-properties "Dynamiske egenskaber")
  (output-syntax "Output-syntaks")
  (no-debugging-or-profiling "Ingen debugning eller profilering")
  (debugging "Debugging")
  (debugging-and-profiling "Debugning og profilering")
  (test-coverage "Syntaktisk dækning af testsuiten")
  (show-details-button-label "Vis detaljer")
  (hide-details-button-label "Skjul detaljer")
  (choose-language-menu-item-label "Vælg sprog...")
  (revert-to-language-defaults "Vend tilbage til standardsproget")
  (fraction-style "Brøkvisning")
  (use-mixed-fractions "Uægte brøker")
  (use-repeating-decimals "Periodeiske decimalbrøker")
  (decimal-notation-for-rationals "Brug decimaltalsnotation for brøker")
  
  
  ;; startup wizard screen language selection section
  (please-select-a-language "Vælg et sprog")
  
  ;;; languages
  (beginning-student "Begynder")
  (beginning-one-line-summary "define, cond, strukturer, konstanter og primitiver")
  (beginning-student/abbrev "Begynder med listeforkortelser")
  (beginning/abbrev-one-line-summary "Begynder, men udskrivning anvender listenotation i REPL")
  (intermediate-student "Øvet")
  (intermediate-one-line-summary "Begynder med leksikalske virkefelter")
  (intermediate-student/lambda "Øvet med lambda")
  (intermediate/lambda-one-line-summary "Øvet med funktioner af højere orden")
  (advanced-student "Rutineret")
  (advanced-one-line-summary "Øvet med lambda og mutation")
  (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
  (pretty-big-scheme "Temmelig omfattende Racket")
  (pretty-big-scheme-one-line-summary "Grafisk, med mange standardbiblioteker")
  (r5rs-language-name "R5RS")
  (r5rs-one-line-summary "R5RS, uden dikkedarer")
  (expander "Ekspanderen")
  (expander-one-line-summary "Ekspanderer, snarere end evaluerer udtryk")
  (professional-languages "Professionelle sprog")
  (teaching-languages "Undervisningssprog")
  (experimental-languages "Eksperimentelle sprog")
  (initial-language-category "Startsprog")
  (no-language-chosen "Intet sprog valgt")
  
  ;(module-language-one-line-summary "En kørsel åbner en REPL i i modulets sammenhæng, inkluderer modulets deklarede sprog")
  
  ;;; from the `not a language language' used initially in drscheme.
  (must-choose-language "DrRacket kan ikke køre programmer før du vælger et programmeringssprog.")
  
  ; next two appear before and after the name of a text book (which will be in italics)
  (using-a-textbook-before "Bruger du ")
  (using-a-textbook-after "?")
  
  ; next two are before and after a language
  (start-with-before "Begynd med ")
  
  (seasoned-plt-schemer? "Rutineret PLT-Schemer?")
  (looking-for-standard-scheme? "Leder efter standard-Scheme?")
  
  ; the three string constants are concatenated together and the middle
  ; one is hyperlinked to the dialog that suggests various languages
  (get-guidance-before "Vælg enten “Vælg sprog...”-punktet i “Sprog”-menuen, eller ")
  (get-guidance-during "følg guiden")
  (get-guidance-after ".")
  
  ;;; debug language
  (unknown-debug-frame "[ukendt]")
  (backtrace-window-title "Tilbagesporing - DrRacket")
  (files-interactions "~a's interaktioner") ;; filled with a filename
  (current-interactions "interaktioner")
  (current-definitions "definitioner")
  ; (stack-frame-in-current-interactions "interaktioner")
  ; (stack-frame-in-current-definitions "definitioner")
  (mzscheme-w/debug "Tekstuel (MzScheme, inkluderer R5RS)")
  (mzscheme-one-line-summary "PLT Scheme uden GUI ")
  (mred-w/debug "Grafisk (MrEd, inkluderer MzScheme)")
  (mred-one-line-summary "PLT Scheme med GUI")
  
  ;; profiling
  (profiling-low-color "Lav")
  (profiling-high-color "Høj")
  (profiling-choose-low-color "Vælg en farve til lav")
  (profiling-choose-high-color "Vælg en farve til høj")
  (profiling "Profilering")
  (profiling-example-text "(define (foo) (foo))")
  (profiling-color-config "Farveområde for profilering")
  (profiling-scale "Farveskala for profilering")
  (profiling-sqrt "Kvadratrod")
  (profiling-linear "Lineær")
  (profiling-square "Kvadratisk")
  (profiling-number "Antal funktionskald")
  (profiling-time "Kumuleret tid")
  ;(profiling-clear "Rens profil")
  (profiling-update "Opdater profil")
  (profiling-col-percent-time "% tid")
  (profiling-col-function "Funktion")
  (profiling-col-time-in-msec "millisekunder")
  (profiling-col-calls "Kald")
  (profiling-show-profile "Vis profil")
  (profiling-hide-profile "Skjul profil")
  (profiling-unknown-src "<< ukendt >>")
  (profiling-no-information-available "Der er ingen profileringsinformation tilgængelig. Er du sikker på, at profilering er sat til i dit sprog, og at du har kørt dit program?")
  (profiling-clear? "Ændringer i definitionsvinduet gør profileringsinformationen ugyldig. Fortsæt?")
  
  ;; test coverage
  (test-coverage-clear? "Ændringer i definitionsvinduet ugyldiggør testdækningsinformationen. Fortsæt?")
  (test-coverage-clear-and-do-not-ask-again "Ja og spørg ikke igen")
  (test-coverage-ask? "Spørg om at rense dækningen af testen")
  
  ;; tracing
  (tracing-enable-tracing "Slå sporing til")
  (tracing-show-tracing-window "Vis sporing")
  (tracing-hide-tracing-window "Skjul sporing")
  (tracing-tracing-nothing-to-show "Ingen sporingsresultater er tilstede endnu. (Sørg for at dit sprog understøtter sporing og at sporing er slået til.)")
  
  
  ;;; repl stuff
  (evaluation-terminated "Evaluering termineret")
  (evaluation-terminated-explanation
   "Evalueringstråden kører ikke længere, så der kan ikke foretages yderligere evaluering inden næste kørsel.")
  (last-stack-frame "vis sidste stakramme")
  (last-stack-frames "vis de ~a sidste stakrammer")
  (next-stack-frames "vis de næste ~a stakrammer")
  
  ;;; welcoming message in repl
  (language "Sprog")
  (custom "speciel")
  (teachpack "Undervisningspakke")
  (welcome-to "Velkommen til")
  (version "version")
  
  ;;; kill evaluation dialog
  (kill-evaluation? "Vil du slå evalueringen ihjel?")
  (just-break "Bare afbryd")
  (kill "Ihjel")
  (kill? "Ihjel?")
  
  ;;; version checker
 (version:update-menu-item   "Undersøger om der er opdateringer...")
 (version:update-check       "Opdateringstjek") ; dialog title, with the next line
 (version:connecting-server  "Forbinder til Rackets versionsserver")
 (version:results-title      "Racket Versionstjek")
 (version:do-periodic-checks "Tjek med mellemrum om der er kommet nye Racket-versioner")
 (version:take-me-there      "Til download") ; ...to the download website
 ;; the next one can appear alone, or followed by a comma and the one after that
 (version:plt-up-to-date     "Din Racket version er up-to-date")
 (version:but-newer-alpha    "men læg mærke til, at der er en nyere alpha-udgave")
 ;; This is used in this context: "Racket vNNN <<<*>>> http://download..."
 (version:now-available-at   "er nu klar på")

  ;; special menu
  (special-menu "Speciel")
  
  ;; large semi colon letters
  (insert-large-letters... "Indsæt store bogstaver...")
  (large-semicolon-letters "Store semikolon-bogstaver")
  (text-to-insert "Tekst til indsættelse")
  
  (module-browser-filename-format "Fuldt filnavn: ~a (~a linjer)")
  (module-browser-root-filename "Rod-filnavn: ~a")
  (module-browser-font-size-gauge-label "Skriftstørrelse")
  (module-browser-progress-label "Fremgang for moduloverblik")
  (module-browser-adding-file "Tilføjer fil: ~a...")
  (module-browser-laying-out-graph-label "Beregner udseende for grafen")
  (module-browser-open-file-format "Åbn ~a")
  (module-browser "Moduloversigt") ;; frame title
  (module-browser... "Moduloversigt...") ;; menu item title
  (module-browser-error-expanding "Fejl under ekspansion af programmet:\n\n~a")
  (module-browser-show-lib-paths "Vis filer læst via (lib ..) stier")
  (module-browser-progress "Moduloversigt: ~a") ;; prefix in the status line
  (module-browser-compiling-defns "Moduloversigt: oversætter definitioner")
  (module-browser-show-lib-paths/short "Følg lib requires") ;; check box label in show module browser pane in drscheme window.
  (module-browser-show-planet-paths/short "Følg planet requires") ;; check box label in show module browser pane in drscheme window.
  (module-browser-refresh "Opdater") ;; button label in show module browser pane in drscheme window.
;  (module-browser-only-in-plt-and-module-langs
;   "Moduloversigten er kun tilgængelig for programmer i PLT-sprogene og i modul-sproget (og kun for de programmer, som benytter moduler).")
  (module-browser-name-length "Navnelængde")
  (module-browser-name-short "Kort")
  (module-browser-name-medium "Mellem")
  (module-browser-name-long "Lanf")
  (module-browser-open-all "Åbn alle filer vist her")
  
  (happy-birthday-matthias "Tillykke med fødselsdagen, Matthias!")
  (happy-birthday-matthew  "Tillykke med fødselsdagen, Matthew!")
  (happy-birthday-shriram  "Tillykke med fødselsdagen, Shriram!")
  
  (mrflow-using-default-language-title "Sprog, som bruges når andet ikke er valgt")
  (mrflow-using-default-language "Det sprog, som anvendes nu, har ikke en typetabel defineret for dets primitiver. R5RS Scheme bruges i stedet.")
  (mrflow-button-title "Analyser")
  ;(mrflow-unknown-style-delta-error-title "Unknown Box Style Delta")
  ;(mrflow-unknown-style-delta-error "Unknown box style delta: ~a")
  (mrflow-popup-menu-show-type "Vis type")
  (mrflow-popup-menu-hide-type "Skjul type")
  (mrflow-popup-menu-show-errors "Vis fejl")
  (mrflow-popup-menu-hide-errors "Skjul fejl")
  
  ;(mrflow-read-exception-title "Læseundtagelse (Read Exception)")
  ;(mrflow-read-exception "Læseundtagelse (Read exception): ~a")
  ;(mrflow-syntax-exception-title "Syntaksundtagelse")
  ;(mrflow-syntax-exception "Syntaksundtagelse: ~a")
  ;(mrflow-unknown-exception-title "Ukendt undtagelse")
  ;(mrflow-unknwon-exception "Ukendt undtagelse: ~a")
  ;(mrflow-language-primitives-error-title "Fejl i sprogprimitiver")
  ;(mrflow-language-primitives-error "Forkert filnavn for tabellen med typer for sprogets primitiver: ~a")
  
  (snips-and-arrows-popup-menu-tack-all-arrows "Vis alle pile")
  (snips-and-arrows-popup-menu-untack-all-arrows "Skjul alle pile")
  (snips-and-arrows-user-action-disallowed-title "Brugerændringer er i øjeblikket ikke tilladt")
  (snips-and-arrows-user-action-disallowed "Brugerændringer er ikke tilladt i editorer, som indeholder snips (f.eks. pile) indsat fra et tool. Skjul alle snips for at få lov til at ændre editorens indhold.")
  ;(snips-and-arrows-changing-terms-warning-title "Changing terms will be undoable")
  ;(snips-and-arrows-changing-terms-warning "Changing terms in an editor containing snips cannot be undone.  You can either cancel this action, remove the snips, and try the change again, or you can continue with the change, in which case the change will not be undoable (all others changes made before and afterward will still be undoable though).")
  (snips-and-arrows-hide-all-snips-in-editor "Skjul alle snips i editoren")
  
  (xml-tool-insert-xml-box "Indsæt XML-kasse")
  (xml-tool-insert-scheme-box "Indsæt Racket-kasse")
  (xml-tool-insert-scheme-splice-box "Indsæt Racket-splejningskasse (Splice Box)")
  (xml-tool-xml-box "XML-Kasse")
  (xml-tool-scheme-box "Racket-Kasse")
  (xml-tool-scheme-splice-box "Racket-splejsningskasse Racket Splice Box")
  (xml-tool-switch-to-scheme "Skift til Racket-kasse")
  (xml-tool-switch-to-scheme-splice "Skift til Racket-splejsningskasse")
  (xml-tool-eliminate-whitespace-in-empty-tags "Fjern blanktegn i tomme tags")
  (xml-tool-leave-whitespace-alone "Bevar blanktegn")
  
  (show-recent-items-window-menu-item "Vis de senest åbnede filer i et separat vindue")
  (show-recent-items-window-label "Senest åbnede filer")
  (number-of-open-recent-items "Antal nye ting")
  (switch-anyway "Skift fil alligevel")
  
  (stepper-program-has-changed "ADVARSEL: Programmet er ændret.")
  (stepper-program-window-closed "ADVARSEL: Programvinduet er væk.")
  
  (stepper-name "Stepper")
  (stepper-language-level-message
   "Sprogniveauet er sat til \"~a\". Indtil videre virker stepperen kun for sprogniveauerne fra \"~a\" til \"~a\".")
  (stepper-button-label "Step")
  (stepper-previous-application "Funktionskald")
  (stepper-previous "Step")
  (stepper-next "Step")
  (stepper-next-application "Funktionskald")
  (stepper-jump-to-beginning "Hjem")
  
  (debug-tool-button-name "Debug")
  
  (dialog-back "Tilbage")
  
  
  ;; warnings about closing a drscheme frame when the program
  ;; might still be doing something interesting
  (program-is-still-running "Programmet i definitionsvinduet kører stadig. Luk alligevel?")
  (program-has-open-windows "Programmet i definitionsvinduet har åbne vinduer. Luk dette vindue alligevel?")
  
  ;; ml-command-line-arguments is for the command line arguments
  ;; label in the module language details in the language dialog.
  (ml-command-line-arguments "Kommandolinje argumenter som en vektor af strenge i read-syntaks.")
  
  ;; ml-cp names are all for the module language collection path
  ;; configuration. See the details portion of the language dialog
  ;; for the module language (at the bottom).
  (ml-cp-default-collection-path "<<standard collection-sti>>")
  
  ;; in std get-directory 
  (ml-cp-choose-a-collection-path "Vælg en collection-sti")
  
  ;; err msg when adding default twice
  (ml-cp-default-already-present
   "Standard collection-stien er allerede med")
  
  ;; title of this section of the dialog (possibly the word
  ;; `Collection' should not be translated)
  (ml-cp-collection-paths "Collection-stier")
  
  ;; button labels
  (ml-cp-add "Tilføj")
  (ml-cp-add-default "Tilføj standardindstilling")
  (ml-cp-remove "Fjern")
  (ml-cp-raise "Op")
  (ml-cp-lower "Ned")
  
  ;; Profj
  (profj-java "Java")
  (profj-java-mode "Java-tilstande")
  (profj-java-mode-color-keyword "nøgleord")
  (profj-java-mode-color-string "streng")
  (profj-java-mode-color-literal "bogstavelighed (literal)")
  (profj-java-mode-color-comment "kommentar")
  (profj-java-mode-color-error "fejl")
  (profj-java-mode-color-identifier "navn")
  (profj-java-mode-color-default "andet")
  
  (profj-insert-java-comment-box "Indsæt Java-kommentarkasse")
  (profj-insert-java-interactions-box "Indsæt Java-interaktionskasse")
  
  ;; The Test Suite Tool
  ;; Errors
  (test-case-empty-error "Tomt test")
  (test-case-too-many-expressions-error "For mange udtryk i testen.")
  ;; DrRacket window menu items
  (test-case-insert "Indsæt test")
  (test-case-disable-all "Slå alle tests fra")
  (test-case-enable-all "Slp alle tests til")
  
  ;; NOTE: The following string constants are labels of the test-case fields. The width
  ;;       of the field is determined by the length of the longest of the following three words.
  ;;       if the words are too long the test case will take up too much horizontal room and
  ;;       not look very good.
  ;; This string is the label of the expression that is being tested in a test case.
  (test-case-to-test "Test")
  ;; This string is the label of the expression that is the expected value of the to-test expression.
  (test-case-expected "Forventet")
  ;; This string is the label of the actual result of the to test expression.
  (test-case-actual "Faktisk")
  (test-case-predicate "Prædikat")
  (test-case-should-raise "Skulle smide")
  ;; The label of a field of the test-case that describes the expected error message of a test case
  (test-case-error-message "Fejlbeskeden")
  
  (test-case-menu-title "Test")
  (test-case-switch-to-error-box "Sktift til fejl-testkasse")
  (test-case-switch-to-nonerror-box "Skift til ikke-fejl-testkasse")
  (test-case-collapse "Skjul testen")
  (test-case-show-actual "Vis faktisk værdi")
  (test-case-enable "Slå testen til")
  (test-case-show-predicate "Vis prædikat")
  (test-case-show-error-message "Vis fejlbeskeden")
  (test-case-convert-to-text "Konverter til tekst")
  
  ;; Profj Boxes
  (profjBoxes-empty-error "Tom interaktion")
  (profjBoxes-too-many-expressions-error "For mange udtryk i en kasse")
  (profjBoxes-interactions-label "Interaktioner")
  (profjBoxes-bad-java-id-error "Ulovlig Java-ID")
  (profjBoxes-examples-label "Eksempler")
  (profjBoxes-add-new-example-button "Tilføj nyt eksempel")
  (profjBoxes-type "Type")
  ;; The Java identifier of an example of data
  (profjBoxes-name "Navn")
  (profjBoxes-value "Værdi")
  (profjBoxes-insert-java-examples "Indsæt Java-eksempler")
  (profjBoxes-insert-java-interactions "Indsæt Java-interaktioner")
  
  ;; Slideshow
  (slideshow-hide-picts "Vis indlejrede kasser")
  (slideshow-show-picts "Vis billeder")
  (slideshow-cannot-show-picts "Kan ikke vise billeder; kør programmet for at cache størrelserne først")
  (slideshow-insert-pict-box "Indsæt billed-kasse (Pict Box)") 
  
  ;; GUI Tool
  (gui-tool-heading "GUI-værktøj")
  (gui-tool-before-clicking-message "Før værktøjsikonerne bruges, vælg \"Indsæt GUI\" fra \"Speciel\"-menuen for at indsætte et rod-GUI-element, eller vælg et allerede indsat GUI.")
  (gui-tool-show-gui-toolbar "Vis GUI-værktøjslinjen")
  (gui-tool-hide-gui-toolbar "Skjul GUI-værktøjslinjen")
  (gui-tool-insert-gui "Indsæt GUI")
  
  )
   
