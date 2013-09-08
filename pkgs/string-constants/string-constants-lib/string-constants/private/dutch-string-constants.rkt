(module dutch-string-constants "string-constant-lang.rkt"

;;; -- Where are Undo, Redo, Select all, and friends from the right-click popup menu?

 ;;; when translating this constant, substitute name of actual language for `English'
 (is-this-your-native-language "Is uw moedertaal Nederlands?")

 (are-you-sure-you-want-to-switch-languages
  "Wisselen van taal vergt een herstart van DrRacket.  Weet u zeker dat u dit wilt?")

 (interact-with-drscheme-in-language "Werk in het Nederlands met DrRacket")

 ;; these two should probably be the same in all languages except English.
 ;; they are the button labels (under macos and windows, respectively)
 ;; that go the with the string above.
 (accept-and-quit "OK, sluit DrRacket maar af")
 (accept-and-exit "OK, sluit DrRacket maar af")
 
 ;;; general purpose (DrRacket is hereby a word in every language, by decree of Robby :)
 (plt "PLT")
 (drscheme "DrRacket")
 (drracket "DrRacket")
 (ok "OK")
 (cancel "Annuleren")
 (untitled "Naamloos")
 (untitled-n "Naamloos ~a")
 (warning "Waarschuwing")
 (error "Fout")
 (close "Sluiten") ;; as in, close an open window
 (stop "Stop")   
 (&stop "&Stop") ;; for use in button and menu item labels, with short cut.

 ;;; important urls
 (web-materials "Verwante Web Sites") ;; menu item title
 (plt-homepage "Racket")
 (pbd-homepage "Program by Design")

 ;;; bug report form
 (cancel-bug-report? "Melden defect afbreken?")
 (are-you-sure-cancel-bug-report?
  "Weet u zeker dat u deze defectmelding NIET wilt sturen?")
 (bug-report-form "Defectmeldingsformulier")
 (bug-report-field-name "Naam")
 (bug-report-field-email "Email")
 (bug-report-field-summary "Samenvatting")
 (bug-report-field-severity "Ernst")
 (bug-report-field-class "Aard")
 (bug-report-field-description "Omschrijving")
 (bug-report-field-reproduce1 "Hoe te")
 (bug-report-field-reproduce2 "veroorzaken")
 (bug-report-field-environment "Omgeving")
 (bug-report-field-docs-installed "Geïnstalleerde documentatie") ; if allowed, add \n
 (bug-report-field-collections "Collecties")
 (bug-report-field-human-language "Spreektaal")
 (bug-report-field-version "Versie")
 (bug-report-synthesized-information "Systeeminformatie")  ;; dialog title
 (bug-report-show-synthesized-info "Toon systeeminformatie")
 (bug-report-submit "Stuur")
 (error-sending-bug-report "Fout bij het versturen van de defectmelding")
 (error-sending-bug-report-expln "Het versturen van de defectmelding mislukte.  Mocht uw internetverbinding verder goed werken, ga dan naar:\n\n    http://bugs.racket-lang.org/\n\nen verstuur de melding via het webformulier.  Sorry voor het ongemak.\n\nDe foutmelding is:\n~a")
 (illegal-bug-report "Ongeldige defectmelding")
 (pls-fill-in-field "Gelieve het \"~a\"-veld in te vullen")
 (malformed-email-address "Onmogelijk emailadres")
 (pls-fill-in-either-description-or-reproduce "Gelieve hetzij het omschrijvings-, hetzij het \"hoe te veroorzaken\"-veld in te vullen.")

 ;;; check syntax
 (check-syntax "Controleer Syntaxis")
 (cs-italic "Cursief")
 (cs-bold "Vet")
 (cs-underline "Onderstreept")
 (cs-change-color "Andere kleur")
 (cs-tack/untack-arrow "Pijlen vast/los")
 (cs-jump "Spring") ; <**> -- "Jump" - where used?
 (cs-error-message "Foutmelding")
 (cs-open-file "Open ~a")
 (cs-rename-var "Hernoem ~a")
 (cs-rename-id "Hernoem Identifier") ;<**>
 (cs-rename-var-to "Hernoem ~a tot:")
 (cs-name-duplication-error "De nieuwgekozen naam ~s komt al voor in dit bereik")
 
 ;;; info bar at botttom of drscheme frame
 (collect-button-label "GC")
 (read-only "Alleen lezen")
 (auto-extend-selection "Auto-extend") ; <**> -- when does this appear?
 (overwrite "Vervang")
 (running "Bezig")
 (not-running "Klaar")
 
 ;;; misc
 (welcome-to-something "Welkom bij ~a")
 
 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "Welkom bij DrRacket, versie ~a, ~a")

 ; these appear on subsequent lines in the `Help|Welcome to DrRacket' dialog.
 (welcome-to-drscheme "Welkom bij DrRacket")

 (goto-line "Ga naar regel")
 (goto-line-invalid-number
  "~a is geen geheel getal tussen 1 en ~a, dus geen geldig regelnummer")
 (goto-position "Naar positie")
 (no-full-name-since-not-saved "Dit bestand is nog nooit opgeslagen, en heeft dus nog geen naam.")
 (cannot-open-because-dne "~a bestaat niet, en kan dus niet geopend worden.")
 (interactions-out-of-sync
  "WAARSCHUWING: Interactie- en definitievenster komen niet overeen.  Druk op Doen!.")
 (file-is-not-saved "Het bestand \"~a\" is niet opgeslagen.")
 (save "Opslaan")
 (close-anyway "Toch sluiten")
 (clear-anyway "Toch wissen") ; <**> -- check with actual usage

 (url: "URL:")
 (open-url... "Open URL...")
 (open-url "Open URL")
 (browse... "Surf...") ; -- Translated as browsing on the Internet
 (bad-url "Bad URL") ; <**>
 (bad-url:this "Bad URL: ~a") ;<**>
 
 ;; Help Desk
 (search-results "Zoekresultaten")
 (help-desk "Hulpbron")
 (help-desk-n "Hulpbron ~a")
 (about-help-desk "Omtrent de Hulpbron")
 (help-desk-about-string
  "De Hulpbron bevat complete informatie omtrent PLT programmatuur, waaronder DrRacket, MzScheme, en MrEd.\n\nVersie ~a\nAuteursrecht (c) 1995-2001 PLT")
 (help-on-help "Hulp voor hulp")
 (help-on-help-details "Voor hulp bij het gebruik van de Hulpbron, klik de link `Help Desk' op de startpagina van de Hulpbron. (Om op die startpagina te komen, klik op de Startknop bovenaan het hulpbronvenster.)")
 (find-docs-for "Zoektekst:") ; <**> - This whole part needs redoing.  The "search" button is at the wrong place for natural Dutch..
 (search "Zoek")
 ; next 3 are popup menu choices at bottom of help desk window
 (search-for-keyword "een trefwoord")
 (search-for-keyword-or-index "een trefwoord of ingang") ; <**>
 (search-for-keyword-or-index-or-text "als trefwoord, Index Entry of tekst") ; <**>
 (exact-match "precies")
 (containing-match "die dit bevat")
 (regexp-match "regexp match")
 (feeling-lucky "Het eerste het beste") ; -- Where does this occur?
 (nothing-found-for-search-key "Niets gevonden voor \"~a\".")
 (searching "Searching...")
 (search-stopped "(Zoeken gestopt.)")
 (search-stopped-too-many-matches "(Zoeken gestopt - te veel treffers.)")
 (reload "Herlaad")
 (help "Help")
 (searching... "Zoeken...")
 (nothing-found-for-empty-search "Gezocht naar niets -- niets gevonden")
 (nothing-found-for "Geen treffers voor ~a")
 (and "en")
 (error-finding-docs "Kan documentatie niet vinden.\n\n~a")
 (manual-installed-date "(geïnstalleerd op ~a)") ; -- assuming ~a is a date.

 ;; refreshing manuals
 (refreshing-manuals "Handleidingen opnieuw ophalen")
 (refresh-downloading... "~a ophalen...")
 (refresh-deleting... "Oude versie van ~a verwijderen...")
 (refresh-installing... "Nieuwe versie van ~a installeren...")

 ;; help desk htty proxy
 (http-proxy "HTTP Proxy")
 (proxy-direct-connection "Directe vebinding")
 (proxy-use-proxy "Gebruik proxy:")
 (proxy-host "Host")
 (proxy-port "Port")
 (proxy-bad-host "Verkeerde Proxy Host")

 ;; browser
 (rewind-in-browser-history "Terug")
 (forward-in-browser-history "Vooruit")
 (home "Start")
 (browser "Browser")
 (choose-browser "Kies een surfer") ; -- translated as Internet browser
 (no-browser "Geen")
 (install? "Installeren?")  ;; if a .plt file is found (title of dialog)
 (you-have-selected-an-installable-package "U hebt een installeerbaar pakket gekozen.")
 (do-you-want-to-install-it? "Wilt u het installeren?")
 (paren-file-size "(Het bestand bevat ~a bytes)")
 (download-and-install "Haal op && Installeer") ;; button label
 (download "Haal op") ;; button label
 (save-downloaded-file/size "Opgehaald bestand (~a bytes) opslaan als") ;; label for get-file dialog
 (save-downloaded-file "Opgehaald bestand opslaan als")  ;; label for get-file dialog
 (downloading "Ophalen") ;; dialog title
 (downloading-file... "Bestand aan het ophalen...")
 (package-was-installed "Het pakket is geïnstalleerd.")
 (download-was-saved "Het opgehaalde bestand is opgeslagen.")

 (install-plt-file-menu-item... "Installeer .plt-bestand...")
 (install-plt-file-dialog-title "Installeer .plt-bestand")
 (install-plt-web-tab "Web")
 (install-plt-file-tab "Bestand")
 (install-plt-filename "Bestandsnaam:")
 (install-plt-url "URL:")
 
 ;; install plt file when opened in drscheme strings
 (install-plt-file "~a installeren of openen ter bewerking?")
 (install-plt-file/yes "Installeer")
 (install-plt-file/no "Bewerk")
 
 ;;; about box
 (about-drscheme-frame-title "Omtrent DrRacket")
 
 ;;; save file in particular format prompting.
 (save-as-plain-text "Bestand opslaan als platte tekst?")
 (save-in-drs-format "Bestand opslaan in DrRacket (niet tekst) -vorm?")
 (yes "Ja")
 (no "Nee") ; -- I personally prefer 'Neen', but it seems most younger people don't..
 
 ;;; preferences
 (preferences "Voorkeuren")
 (error-saving-preferences "Fout bij opslaan voorkeuren: ~a")
 (error-reading-preferences "Fout bij lezen voorkeuren")
 (scheme-prefs-panel-label "Racket")
 (warnings-prefs-panel-label "Waarschuwingen")
 (editor-prefs-panel-label "Bewerken")
 (highlight-parens "Oplichten tussen bijeenhorende haken") ; -- ugly Dutch - should say something completely different, such as "highlight substructure"
 (fixup-parens "Haken corrigeren")
 (flash-paren-match "Knipper bijbehorende haak")
 (auto-save-files "Bestanden automatisch opslaan")
 (backup-files "Reservebestanden")
 (map-delete-to-backspace "Delete naar links") ; -- to be changed if RtoL languages are supported
 (verify-exit "Bevestiging bij afsluiten")
 (ask-before-changing-format "Bevestiging bij opslaan in ander formaat")
 (wrap-words-in-editor-buffers "Wrap words in editor buffers")
 (show-status-line "Toon statusregel")
 (count-from-one "Regels en kolommen tellen vanaf 1") 
 (display-line-numbers "Toon regelnummers (geen letternummers) in buffer") ; -- 'buffer' is not wrong, but unclear
 (enable-keybindings-in-menus "Gebruik sneltoetsen in menus")
 (reuse-existing-frames "Reuse existing frames when opening new files")
 (default-fonts "Verstek-lettertypen")
 
 ; should have entire alphabet
 (font-example-string "The quick brown fox jumps over lazy dogs.") 

 (change-font-button-label "Veranderen") ; or: 'aanpassen' (= adapt)
 (fonts "Lettertypen") ; -- Where is this used?  If context is clear, then just 'typen' is more natural.

 ; filled with type of font, eg modern, swiss, etc.
 (choose-a-new-font "Gelieve een nieuw \"~a\" type te kiezen")

 (font-size-slider-label "Grootte")
 (restart-to-see-font-changes "Herstart om veranderingen te zien")

 (font-prefs-panel-title "Lettertypen")
 (font-name "Naam")
 (font-size "Grootte")
 (set-font "Lettertype instellen...")
 (select-font-name "Kies naam")
 (example-text "Voorbeeldtekst:")
 (only-warn-once "Slechts eenmaal waarschuwen als interacties niet met definitievenster overeenkomen") ; <**> -- The original English seems wrong here.  I translated "interactions" / "definitions" rather than "executions".
 
 ; warning message when lockfile is around
 (waiting-for-pref-lock "Waiting for the preferences lockfile...") ; <**>
 (pref-lock-not-gone
  "The preferences lockfile:\n\n   ~a\n\nprevents the preferences from being saved. Ensure that no Racket software is running and delete this file.")
 (still-locked-exit-anyway? "Opslaan voorkeuren mislukt. Toch afsluiten?")
 
 ;;; indenting preferences panel
 (indenting-prefs-panel-label "Indenteren")

 ; filled with define, lambda, or begin
 (enter-new-keyword "Nieuw ~a-achtig trefwoord")
 (x-keyword "~a trefwoord") ; -- Funny in English too, isn't it?  "Begin Keyword"
 (x-like-keywords "~a-achtige trefwoorden")

 (expected-a-symbol "verwachtte een symbool, kreeg: ~a")
 (already-used-keyword "Het trefwoord \"~a\" heeft al een speciale indentatie")
 (add-keyword "Toevoegen")
 (remove-keyword "Verwijderen")
 
 ;;; find/replace
 (find-and-replace "Zoek en vervang")
 (find "Zoek")
 (replace "Vervang")
 (dock "Aanhaken")
 (undock "Zweven")
 (replace&find-again "Vervang && Zoek opnieuw") ;;; need double & to get a single &
 (forward "Voorwaarts")
 (backward "Terug")
 (hide "Sluiten")
 
 ;;; multi-file-search
 (mfs-multi-file-search-menu-item "Zoek in bestanden...")
 (mfs-string-match/graphics "String match (handles files with graphics)") ; <**>
 (mfs-regexp-match/no-graphics "Regular Expression (alleen platte tekstbestanden)")
 (mfs-searching... "Zoekt...")
 (mfs-configure-search "Zoekinstellingen") ;; dialog title
 (mfs-files-section "Bestanden")   ;; section in config dialog
 (mfs-search-section "Zoek") ;; section in config dialog
 (mfs-dir "Dir")
 (mfs-recur-over-subdirectories "Recursief in mappen")
 (mfs-regexp-filename-filter "Regexp filename filter")
 (mfs-search-string "Zoektekst")
 (mfs-drscheme-multi-file-search "Multi File Search - DrRacket") ;; results window and error message title
 (mfs-not-a-dir "\"~a\" is geen map")
 (mfs-open-file "Bestand openen")
 (mfs-stop-search "Stop met zoeken")
 (mfs-case-sensitive-label "Case sensitive") ; <**>
 (mfs-no-matches-found "Geen treffers.")
 (mfs-search-interrupted "Zoeken afgebroken.")
 
 ;;; reverting a file
 (are-you-sure-revert "Herladen kan niet ongedaan gemaakt worden.  Toch doen?")
 (are-you-sure-revert-title
  "Herladen?")
 
 ;;; saving a file
 ; ~a is filled with the filename
 (error-saving "Fout bij opslaan") ;; title of error message dialog
 (error-saving-file/name "Er trad een fout op bij het opslaan van ~a.")

 ;;; finder dialog
 (must-specify-a-filename "You must specify a file name")
 (file-does-not-exist "Bestand \"~a\" bestaat niet.")
 (ask-because-file-exists "Bestand \"~a\" bestaat al. Vervangen?")
 (dne-or-cycle "Bestand \"~a\" bevat een nietbestaande map, of een cykel.") ; -- weird English.  How can a file contain a directory?
 (get-file "Get file") ; <**>
 (put-file "Put file")
 (full-pathname "Volledige padnaam")
 (show-dot-files "Toon ook bestanden en mappen waarvan de naam met een punt begint.")
 (up-directory-button-label "Één map omhoog")
 (add-button-label "Toevoegen") ;;; for multi-file selection
 (add-all-button-label "Alles toevoegen") ;;; for multi-file selection
 (remove-button-label "Verwijderen") ;;; for multi-file selection
 (file-wrong-form "Bestandsnaam heeft niet de juiste vorm.")
 (select-files "Kies bestanden")
 (select-file "Kies bestand")
 (dir-dne "Map bestaat niet.")
 (file-dne "Bestand bestaat niet.")
 (empty-filename "Bestandsnaam moet minstens één letter bevatten.")
 (that-is-dir-name "Dat is de naam van een map.")
 
 ;;; raw menu names -- these must match the 
 ;;; versions below, once the &s have been stripped.
 ;;; if they don't, DrRacket's menus will appear
 ;;; in the wrong order.
 (file-menu "Bestand")
 (edit-menu "Bewerken")
 (help-menu "Hulp")
 (windows-menu "Vensters")
 
 ;;; menus
 ;;; - in menu labels, the & indicates a alt-key based shortcut.
 ;;; - sometimes, things are stuck in the middle of 
 ;;; menu item labels. For instance, in the case of
 ;;; the "Save As" menu, you might see: "Save Definitions As". 
 ;;; be careful of spacing, follow the English, if possible.
 ;;; - the ellipses in the `after' strings indicates that
 ;;; more information is required from the user before completing
 ;;; the command.

 (file-menu-label "&Bestand")

 (new-info  "Open nieuw bestand")
 (new-menu-item "&Nieuw")
 (new-...-menu-item "&Nieuw...")

 (open-info "Open bestand van schijf")
 (open-menu-item "&Open...")

 (open-recent-info "Onlangs geopende bestanden")
 (open-recent-menu-item "Open opnieuw")
 
 (revert-info "Negeer de gemaakte wijzigingen, en herlaad het bestand")
 (revert-menu-item "&Herlaad")

 (save-info "Sla dit bestand op een schijf op")
 (save-menu-item "O&pslaan")

 (save-as-info "Vraag om een naam en sla dit bestand onder die naam op")
 (save-as-menu-item "Ops&laan als...")

 (print-info "Stuur dit bestand naar een printer")
 (print-menu-item "Af&drukken...")

 (close-info "Sluit dit bestand")
 (close-menu-item "&Sluiten")

 (quit-info "Sluit alle vensters")
 (quit-menu-item-windows "&Afsluiten")
 (quit-menu-item-others "&Afsluiten")
 
 (edit-menu-label "Be&werken")
 
 (undo-info "Maak de laatste actie ongedaan")
 (undo-menu-item "&Ongedaan maken")

 (redo-info "Voer de als laatste ongedaangemaakte actie alsnog uit")
 (redo-menu-item "Opnieuw &uitvoeren") ;<**> - does the & have the correct position?

 (cut-info "Verplaats de selectie naar het klembord")
 (cut-menu-item "K&nippen")

 (copy-info "Kopieer de selectie naar het klembord")
 (copy-menu-item "&Kopiëren")

 (paste-info "Kopieer de inhoud van het klembord naar de plaats van de cursor")
 (paste-menu-item "&Plakken")

 (clear-info "Gooi de selectie weg")
 (clear-menu-item-windows "&Wissen")

 (select-all-info "Selecteer het gehele document")
 (select-all-menu-item "Selecteer a&lles")
 
 (find-info "Zoek tekst in het document")
 (find-menu-item "Zoek...")

 (find-again-info "Zoek verder naar dezelfde tekst")
 (find-again-menu-item "Zoek nogmaals")
 
 (replace-and-find-again-info "Vervang de huidige selectie, en zoek verder")
 (replace-and-find-again-menu-item "Vervang && Zoek nogmaals")

 (preferences-info "Stel uw voorkeuren in")
 (preferences-menu-item "Voorkeuren...")

 (keybindings-info "Toon de huidige toetsbetekenissen")
 (keybindings-menu-item "Toetsbetekenissen")
 (keybindings-frame-title "Toetsbetekenissen")
 (keybindings-sort-by-name "Op betekenis")
 (keybindings-sort-by-key "Op toets")

 (insert-text-box-item "Tekst")
 (insert-image-item "Plaatje...")
 (insert-comment-box-menu-item-label "Commentaarblok")
 (wrap-text-item "Wrap Text") ; <**>
 (wrap-text-item "Wrap Text")


 (windows-menu-label "&Vensters")
 (bring-frame-to-front "Kies venster")       ;;; title of dialog
 (bring-frame-to-front... "Kies venster...") ;;; corresponding title of menu item

 (view-menu-label "&Tonen")
 (show-overview "Overzicht") 
 (hide-overview "Geen Overzicht")

 (help-menu-label "&Hulp")
 (about-info "Credits and details for this application") ; <**>
 (about-menu-item "Info...")
 
 ;;; help-desk-specific menus
 (new-help-desk "Nieuwe Hulpbron")

 ;; open here's new menu item
 (create-new-window-or-clear-current
  "Wilt u een nieuw venster openen, of het huidige wissen?")
 (clear-current "Wis huidig")
 (new-window "Open nieuw")

 ;;; exiting and quitting ``are you sure'' dialog
 ;;; (exit is used on windows, quit on macos, in English. Other
 ;;; languages probably use the same word on both platforms.
 (exit "Afsluiten")
 (quit "Afsluiten")
 (are-you-sure-exit "Weet u zeker dat u wilt afsluiten?")
 (are-you-sure-quit "Weet u zeker dat u wilt afsluiten?")
 
 ;;; autosaving
 (error-autosaving "Fout bij automatisch opslaan van \"~a\".")
 (autosaving-turned-off "Automatisch opslaan uitgezet tot\nhet bestand handmatig is opgeslagen.")
 
 ;;; file modified warning
 (file-has-been-modified
  "Sinds de laatste opslag is dit bestand gewijzigd.  Veranderingen overschrijven?")
 (overwrite-file-button-label "Overschrijven")
 
 (definitions-modified
  "Het bestand met de definities is gewijzigd.  Gelieve op te slaan of te herladen.")
 (drscheme-internal-error "Interne Fout van DrRacket")
 
 ;;; tools <**>
 (invalid-tool-spec "The tool specification in collection ~a's info.rkt file is invalid. Expected either a string or a non-empty list of strings, got: ~e")
 (error-invoking-tool-title "Error invoking tool ~s;~s")
 (tool-tool-names-same-length
  "expected `tool-names' and `tools' to be lists of the same length, in info.rkt file for ~s, got ~e and ~e")
 (tool-tool-icons-same-length
  "expected `tool-icons' and `tools' to be lists of the same length, in info.rkt file for ~s, got ~e and ~e")
 (error-getting-info-tool
  "error loading info.rkt file for ~s")
 (tool-error-phase1 "Error in phase 1 for tool ~s; ~s")
 (tool-error-phase2 "Error in phase 2 for tool ~s; ~s")


 ;;; define popup menu
 (end-of-buffer-define "<< buffereinde >>")
 (sort-by-name "Sorteer op naam")
 (sort-by-position "Sorteer op positie in bestand")
 (no-definitions-found "<< geen definities gevonden >>")
 (jump-to-defn "Spring naar definitie van ~a")

 (recent-items-sort-by-age "Sorteer op leeftijd")
 (recent-items-sort-by-name "Sorteer op naam")
 
 ;;; show menu
 (hide-definitions-menu-item-label "Geen &Definities")
 (show-definitions-menu-item-label "&Definities")
 (definitions-menu-item-help-string "Toon het definitievenster al dan niet")
 (show-interactions-menu-item-label "&Interacties")
 (hide-interactions-menu-item-label "Geen &Interacties niet")
 (interactions-menu-item-help-string "Toon het interactievenster al dan niet")
 
 ;;; file menu
 (save-definitions-as "Definities ops&laan als...")
 (save-definitions "Definities opslaan")
 (print-definitions "Definitions af&drukken...")
 (about-drscheme "Omtrent DrRacket")
 (save-other "Anderszins opslaan")
 (save-definitions-as-text "Definities opslaan als tekst...")
 (save-interactions "Interacties opslaan")
 (save-interactions-as "Interacties opslaan als...")
 (save-interactions-as-text "Interacties opslaan als tekst...")
 (print-interactions "Interacties afdrukken...")
 
 ;;; edit-menu
 (split-menu-item-label "&Splits")
 (collapse-menu-item-label "V&oeg samen")
 
 ;;; language menu
 (language-menu-name "&Taal")
 
 ;;; scheme-menu
 (scheme-menu-name "Ra&cket")
 (execute-menu-item-label "Doen!")
 (execute-menu-item-help-string "Voer het programma in het definitievenster uit")
 (break-menu-item-label "Onderbreken")
 (break-menu-item-help-string "Onderbreek de huidige berekening")
 (kill-menu-item-label "Beëindigen")
 (kill-menu-item-help-string "Beëindig de huidige berekening")
 (clear-error-highlight-menu-item-label "Verwijder foutkleur")
 (clear-error-highlight-item-help-string "Verwijdert de roze kleur die de fout aangeeft")
 (reindent-menu-item-label "He&rindenteer")
 (reindent-all-menu-item-label "Herindenteer &Alles")
 (comment-out-menu-item-label "&Commentarieer weg")
 (uncomment-menu-item-label "&Verwijder commentaartekens") ; -- Sorry, couldn't keep the &U..
 
 ;;; executables
 (create-executable-menu-item-label "Maak Exe...")
 (create-executable-title "Maak Exe")
 (must-save-before-executable "You must save your program before creating an executable.")
 (definitions-not-saved "The definitions window has not been saved. The executable will use the latest saved version of the definitions window. Continue?")
 (launcher "Launcher")
 (stand-alone "Stand-alone")
 (executable-type "Type")
 (executable-base "Base")
 (filename "Bestandsnaam: ")
 (create "Maak")
 ;; "choose-an-executable" changed to "specify-a"
 ;(please-choose-an-executable-filename "Kies een bestandsnaam voor het programma.")
 
 (create-servlet "Maak Servlet...")
  
 ;;; buttons
 (execute-button-label "Doen!") 
 (save-button-label "Opslaan")
 (break-button-label "Onderbreken")
 
 ;;; search help desk popup menu
 (search-help-desk-for "Zoek hulp omtrent \"~a\"")
 (exact-lucky-search-help-desk-for "Geef hulp omtremt \"~a\"")

 ;; collapse and expand popup menu items
 (collapse-sexp "S-expressie samenvouwen")
 (expand-sexp "S-expressie ontvouwen")
 
 ;;; fraction dialog
 (enter-fraction "Geef breuk")
 (whole-part "Gehelen")
 (numerator "Teller")
 (denominator "Noemer")
 (invalid-number "Onjuist getal: moet exact, reëel, en niet geheel zijn.")
 (insert-fraction-menu-item-label "Breuk...")

 ;; number snip popup menu
 (show-decimal-expansion "Toon als decimaal getal")
 (show-more-decimal-places "Toon meer decimalen")
 
 ;;; Teachpack messages
 (select-a-teachpack "Kies een lespakket")
 (clear-teachpack "Verwijder lespakket ~a")
 (teachpack-error-label "DrRacket - fout in lespakket") ; -- translated as "error in Teachpack".
 (teachpack-didnt-load "Lespakket ~a niet correct geladen.")
 (add-teachpack-menu-item-label "Lespakket toevoegen...")
 (clear-all-teachpacks-menu-item-label "Alle lespakketten verwijderen")
 (teachpack-not-only-one-import "Lespakket unit/sig in ~a moet precies één 'import' hebben.") ; <**> -- ??
 (drscheme-teachpack-message-title "DrRacket-lespakket")
 (already-added-teachpack "Lespakket ~a al aanwezig")
 
 ;;; Language dialog
 (introduction-to-language-dialog
  "Kies een taal.  De verstekwaarde is normaliter de juiste voor beginnende cursisten.")
 (language-dialog-title "Taal instellen")
 (case-sensitive-label "Verschil tussen hoofd- en kleine letters") ; -- ugly
 (output-style-label "Uitvoerstijl")
 (constructor-printing-style "Constructor")
 (quasiquote-printing-style "Quasiquote")
 (write-printing-style "write")
 (sharing-printing-label "Geef weerkerende delen in uitvoer aan")
 (use-pretty-printer-label "Toon uitvoer netjes over meer regels") ; -- maybe rather: 'show indented'?
 (input-syntax "Invoersyntaxis")
 (dynamic-properties "Dynamische eigenschappen")
 (output-syntax "Uitvoersyntaxis")
 (no-debugging-or-profiling "Geen ontwikkelhulp")
 (debugging "Alleen foutvinden")
 (debugging-and-profiling "Foutvinden en klokken")
 (show-details-button-label "Toon details")
 (hide-details-button-label "Toon details niet")
 (choose-language-menu-item-label "Kies taal...")
 (revert-to-language-defaults "Herstel verstekwaarden")
 (fraction-style "Breukweergave")
 (use-mixed-fractions "Gemengde breuken")
 (use-repeating-decimals "Repeterende breuken")
 (decimal-notation-for-rationals "Schrijf breuken decimaal")
 (please-select-a-language "Kies een taal")

 
 ;;; languages
 (beginning-student "Beginner")
 (beginning-one-line-summary "define, cond, structs, constanten, en primitieven")
 (beginning-student/abbrev "Beginner, met lijstnotatie")
 (beginning/abbrev-one-line-summary "Beginner, maar lijsten worden met lijstnotatie afgedrukt")
 (intermediate-student "Middenmoot")
 (intermediate-one-line-summary "Beginner, plus lexikaal bereik")
 (intermediate-student/lambda "Middenmoot, plus lambda")
 (intermediate/lambda-one-line-summary "Middenmoot, plus hogere-ordefuncties")
 (advanced-student "Gevorderde")
 (advanced-one-line-summary "Middenmoot, plus lambda en mutatie")
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (pretty-big-scheme "Vrij groot")
 (pretty-big-scheme-one-line-summary "Inclusief syntaxis and functies van de HtDP-talen")
 (r5rs-language-name "R5RS")
 (r5rs-one-line-summary "R5RS, kaal")
 (unknown-debug-frame "[onbekend]")
 
 ;(module-language-one-line-summary "Execute creates a REPL in the context of the module, including the module's declared language")
  
 ;;; debug language
 (backtrace-window-title "Spoor - DrRacket")
 (files-interactions "~a's interacties") ;; filled with a filename
 (current-interactions "interacties")
 (current-definitions "definities")
 (mzscheme-w/debug "Enkel tekst (MzScheme, omvat R5RS)")
 (mzscheme-one-line-summary "PLTs onderliggende Scheme")
 (mred-w/debug "Grafisch (MrEd, bevat MzScheme)")
 (mred-one-line-summary "MzScheme met een grafische schil")

 ;; profiling
 (profiling-low-color "Laag")
 (profiling-high-color "Hoog")
 (profiling-choose-low-color "Kies een onderkleur")
 (profiling-choose-high-color "Kies een bovenkleur")
 (profiling "Klokken")
 (profiling-example-text "(define (jippie) (jippie))")
 (profiling-color-config "Kleurbereik")
 (profiling-scale "Kleurschaling")
 (profiling-sqrt "Vierkantswortel")
 (profiling-linear "Lineair")
 (profiling-square "Kwadratisch")
 (profiling-number "Aantal aanroepen")
 (profiling-time "Totaaltijd")
 (profiling-clear "Wis klokgegevens")
 (profiling-update "Werk klokgegevens bij")
 (profiling-col-percent-time "% Tijd")
 (profiling-col-function "Functie")
 (profiling-col-time-in-msec "Msec")
 (profiling-col-calls "Aanroepen")
 (profiling-show-profile "Klokgegevens")
 (profiling-hide-profile "Geen Klokgegevens")
 (profiling-unknown-src "<< onbekend >>")
 (profiling-no-information-available "Er zijn geen klokgegevens.  Deze zijn enkel beschikbaar als klokken in uw taal aanstaat, en u uw programma al gedraaid hebt.")

 (profiling-clear? "Veranderingen in het definitievenster maken de klokgegevens ongeldig.  Toch doorgaan?")
 
 ;;; repl stuff
 (evaluation-terminated "Berekening gestopt")
 (evaluation-terminated-explanation
  "De rekenthread loopt niet meer, dus tot de volgende uitvoering kan geen berekening plaatsvinden.")
 (last-stack-frame "toon top van de stapel")
 (last-stack-frames "toon top ~a van de stapel")
 (next-stack-frames "toon volgende ~a van de stapel")
 
 ;;; welcoming message in repl
 (language "Taal")
 (custom "aangepast")
 (teachpack "Lespakket")
 (welcome-to "Welkom bij")
 (version "versie")
 
 ;;; kill evaluation dialog
 (kill-evaluation? "Wilt u de berekeing beëindigen?")
 (just-break "Enkel onderbreken")
 (kill "Beëindigen")
 (kill? "Beëindigen?")

 ;;; version checker
 (version:update-menu-item "Recentere versies...")
 (version:update-check "Versie bijwerken")

 ;; special menu
 (special-menu "Invoegen")
 
 ;; large semi colon letters
 (insert-large-letters... "Krantenkop")
 (large-semicolon-letters "Puntkommaletters")
 (text-to-insert "Invoegtekst")

 (module-browser-filename-format "Complete bestandsnaam: ~a (~a lines)")
 (module-browser-root-filename "Root Filename: ~a")
 (module-browser-font-size-gauge-label "Lettergrootte")
 (module-browser-progress-label "Module overview progress")
 (module-browser-adding-file "Toevoegen bestand: ~a...")
 (module-browser-laying-out-graph-label "Opmaken graaf")
 (module-browser-open-file-format "Open ~a")
 (module-browser "Modulekiezer") ;; frame title
 (module-browser... "Vind module...") ;; menu item title
 (module-browser-error-expanding "Error expanding the program:\n\n~a")

 (happy-birthday-matthias "Lang zal Matthias leven!")

 (mrflow-using-default-language-title "Verstektaal gebruikt")
 (mrflow-using-default-language "Er bestaat geen typetabel voor de primitieven van uw huidige taal.  Gelieve R5RS te gebruiken.")
 (mrflow-button-title "Analyseer")
 ;(mrflow-unknown-style-delta-error-title "Unknown Box Style Delta")
 ;(mrflow-unknown-style-delta-error "Unknown box style delta: ~a")
 (mrflow-popup-menu-show-type "Toon type")
 (mrflow-popup-menu-hide-type "Toon type niet")
 (mrflow-popup-menu-show-errors "Toon fouten")
 (mrflow-popup-menu-hide-errors "Toon fouten niet")
 (mrflow-popup-menu-tack-all-arrows "Alle pijlen vast")
 (mrflow-popup-menu-untack-all-arrows "Alle pijlen los")
 ;(mrflow-read-exception-title "Read Exception")
 ;(mrflow-read-exception "Read exception: ~a")
 ;(mrflow-syntax-exception-title "Syntax Exception")
 ;(mrflow-syntax-exception "Syntax exception: ~a")
 ;(mrflow-unknown-exception-title "Unknown Exception")
 ;(mrflow-unknown-exception "Unknown exception: ~a")
 ;(mrflow-language-primitives-error-title "Fout in basisfuncties")
 ;(mrflow-language-primitives-error "Onjuiste bestandsnaam voor typentabel basisfuncties: ~a")

 (xml-tool-insert-xml-box "XMLblok")
 (xml-tool-insert-scheme-box "Racketblok")
 (xml-tool-insert-scheme-splice-box "Racket Splice Box")
 (xml-tool-xml-box "XML Box")
 (xml-tool-scheme-box "Racket Box")
 (xml-tool-scheme-splice-box "Racket Splice Box")
 (xml-tool-switch-to-scheme "Switch to Racket box")
 (xml-tool-switch-to-scheme-splice "Switch to Racket splice box")
 (xml-tool-eliminate-whitespace-in-empty-tags
  "Verwijder wit in lege tags") ; <**> - don't know the official Dutch for 'tag' here..
 (xml-tool-leave-whitespace-alone "Laat wit staan")
 
 (show-recent-items-window-menu-item "Toon onlangs geopende bestanden in een apart venster")
 (show-recent-items-window-label "Onlangs geopende bestanden")
 (number-of-open-recent-items "Aantal recente items")
 (switch-anyway "Toch van bestand wisselen")

 (stepper-program-has-changed "WAARSCHUWING: Programma is veranderd.")
 (stepper-program-window-closed "WAARSCHUWING: Programmavenster is weg.")
 )


