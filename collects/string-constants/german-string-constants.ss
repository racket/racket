(module german-string-constants "string-constant-lang.ss"

 (is-this-your-native-language
  "Ist Deutsch Ihre Muttersprache?")

 (are-you-sure-you-want-to-switch-languages
  "Dies wird die Sprache der DrScheme-Benutzeroberfläche ändern und erfordert einen Neustart von DrScheme.  Sind Sie sicher?")

 (interact-with-drscheme-in-language "Deutsche Benutzeroberfläche für DrScheme")

 (accept-and-quit "In Ordnung - Beenden")
 (accept-and-exit "In Ordnung - Beenden")
 ;;; general purpose (DrScheme is hereby a word in every language, by decree of Robby :)
 (plt "PLT")
 (drscheme "DrScheme")
 (ok "OK")
 (cancel "Absagen")
 (abort "Abbrechen")
 (untitled "Namenlos")
 (untitled-n "Namenlos ~a")
 (warning "Warnung")
 (error "Fehler")
 (close "Schließen") ;; as in, close an open window
 (stop "Stop")
 (&stop "&Stop") ;; for use in button and menu item labels, with short cut.
 (are-you-sure-delete? "Sind Sie sicher, dass Sie ~a löschen wollen?") ;; ~a is a filename or directory name
 (ignore "Ignorieren")
 (revert "Änderungen rückgängig machen")

 (dont-ask-again "Nicht wieder nachfragen (immer so wie jetzt)")

 (web-materials "Verwandte Web-Seiten")
 (tool-web-sites "Web-Seiten mit Tools")
 (drscheme-homepage "DrScheme")
 (plt-homepage "PLT")
 (how-to-use-scheme "How to Use Scheme")
 (teachscheme!-homepage "TeachScheme!")

 ;;; bug report form
 (cancel-bug-report? "Bug-Report verwerfen?")
 (are-you-sure-cancel-bug-report?
  "Sind Sie sicher, dass Sie diesen Bug-Report verwerfen wollen?")
 (bug-report-form "Formular für Bug-Report")
 (bug-report-field-name "Name")
 (bug-report-field-email "Email")
 (bug-report-field-summary "Zusammenfassung")
 (bug-report-field-severity "Wie schlimm?")
 (bug-report-field-class "Art")
 (bug-report-field-priority "Priorität")
 (bug-report-field-description "Beschreibung")
 (bug-report-field-reproduce1 "Schritte, um das Problem zu")
 (bug-report-field-reproduce2 "reproduzieren")
 (bug-report-field-environment "Umgebung")
 (bug-report-field-tools "Tools")
 (bug-report-field-docs-installed "Installierte Dokumentation")
 (bug-report-field-language "Sprachebene")
 (bug-report-field-teachpacks "Teachpacks")
 (bug-report-field-collections "Kollektionen")
 (bug-report-field-human-language "Interaktionssprache")	;
 (bug-report-field-version "Version")
 (bug-report-synthesized-information "Generierte Information")  ;; dialog title
 (bug-report-show-synthesized-info "Generierte Informationen anzeigen")	; (an)zeigen
 (bug-report-submit "Abschicken")	
 (bug-report-submit-menu-item "Bug-Report abschicken") ;; in Help Menu (drs & help desk)
 (sending-bug-report "Bug-Report abschicken")
 (error-sending-bug-report "Versendung des Bug-Reports fehlgeschlagen")
 (error-sending-bug-report-expln "Ein Fehler ist beim Versenden des Bug-Reports aufgetreten. Falls Ihre Internet-Verbindung eigentlich funktioniert, besuchen Sie bitte:\n\n    http://bugs.plt-scheme.org/ \n\nund teilen Sie uns den Bug mit unserem Online-Formular mit. Wir bitten um Ihr Verständnis.\n\nDie Fehlermeldung lautet:\n~a")
 (bug-report-sent "Bug-Report erfolgreich verschickt")
 (bug-report-sent-detail "Wir danken für Ihren Bug-Report. Sie sollten innerhalb der nächsten 30 Minuten eine Bestätigung per Email bekommen. Falls nicht, schicken Sie eine Email an folgende Adresse: scheme@plt-scheme.org.")
 (illegal-bug-report "Ungültiger Bug-Report")
 (pls-fill-in-field "Bitte auch das \"~a\" Feld ausfüllen")
 (malformed-email-address "Ungültige Email-Adresse")
 (pls-fill-in-either-description-or-reproduce "Bitte füllen Sie entweder das Feld \"Beschreibung\" oder das Feld \"Schritte, um das Problem zu reproduzieren\" aus.")

 ;;; check syntax
 (check-syntax "Syntaxprüfung")
 (cs-italic "Kursiv")
 (cs-bold "Fett")
 (cs-underline "Unterstrichen")
 (cs-change-color "Farbe ändern")
 (cs-tack/untack-arrow "Pfeil befestigen/lösen")
 (cs-jump-to-next-bound-occurrence "Zum nächsten gebundenen Vorkommen springen")
 (cs-jump-to-binding "Zu bindendem Vorkommen springen")
 (cs-jump-to-definition "Zu Definition springen")
 (cs-error-message "Fehlermeldung")
 (cs-open-file "~a öffnen")
 (cs-rename-var "~a umbenennen")
 (cs-rename-id "Bezeichner umbenennen")
 (cs-rename-var-to "~a umbenennen nach:")
 (cs-name-duplication-error "Der neugewählte Name, ~s, ist hier schon gebunden.")
 (cs-rename-anyway "Trotzdem umbenennen")
 (cs-status-init "Syntaxprüfung: Umgebung für den User-Code initialisieren")
 (cs-status-coloring-program "Syntaxprüfung: Ausdruck einfärben")
 (cs-status-eval-compile-time "Syntaxprüfung: Compile-Time-Code ausführen")
 (cs-status-expanding-expression "Syntaxprüfung: Ausdruck expandieren")
 (cs-mouse-over-import "Bindung ~s importiert aus ~s")

 (cs-lexical-variable "lexikalische Variable")
 (cs-lexical-syntax "lexikalische Syntax")
 (cs-imported-variable "importierte Variable")
 (cs-imported-syntax "importierte Syntax")

 ;;; info bar at botttom of drscheme frame
 (collect-button-label "GC")
 (read-only "Schreibgeschützt")
 (read/write "Lesen/Schreiben")
 (auto-extend-selection "Automatisch erweitern")
 (overwrite "Überschreiben")
 (running "Programm läuft")
 (not-running "Programm inaktiv")
 
 ;;; misc
 (welcome-to-something "Willkommen bei ~a")
 
 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "Willkommen bei DrScheme! (Version ~a, ~a)")

 ; these appear on subsequent lines in the `Help|Welcome to DrScheme' dialog.
 (welcome-to-drscheme "Willkommen bei DrScheme")
 (version/language "Version ~a, ~a")

 (goto-line "Zu Zeile springen")
 (goto-line-invalid-number
  "~a ist keine gültige Zeilennummer. Es muss eine ganze Zahl zwischen 1 und ~a sein.")
 (goto-position "Zu Position springen")
 (no-full-name-since-not-saved
  "Die Datei hat noch keinen Namen, weil sie noch nicht abgespeichert wurde.")
 (cannot-open-because-dne "Die Datei ~a kann nicht geöffnet werden, weil sie nicht existiert.")
 (needs-execute-language-changed
   "WARNUNG: Die Sprache hat sich geändert. \"Start\" drücken.")
 (needs-execute-teachpack-changed
  "WARNUNG: Die Teachpacks haben sich geändert. \"Start\" drücken.")
 (needs-execute-defns-edited
  "WARNUNG: Die Definitionen haben sich geändert. \"Start\" drücken.")

 (file-is-not-saved "Die Datei \"~a\" ist nicht gespeichert.")
 (save "Speichern")
 (please-choose-either "Bitte entweder \"~a\" oder \"~a\" wählen")
 (close-anyway "Trotzdem schließen")
 (clear-anyway "Trotzdem löschen")

 (log-definitions-and-interactions "Definitionen and Interaktionen protokollieren...")
 (stop-logging "Protokoll stoppen")
 (please-choose-a-log-directory "Bitte wählen Sie ein Verzeichnis f?r das Protokoll")
 (logging-to "Protokoll: ")
 (erase-log-directory-contents "Inhalt von Protokoll-Verzeichnisses ~a löschen?")
 (error-erasing-log-directory "Fehler beim Löschen des Protokoll-Verzeichnisses.\n\n~a\n")

 ;; modes
 (mode-submenu-label "Modi")
 (scheme-mode "Scheme-Modus")
 (text-mode "Text-Modus")

 (scheme-mode-color-symbol "Symbol")
 (scheme-mode-color-keyword "Schlüsselwort")
 (scheme-mode-color-comment "Kommentar")
 (scheme-mode-color-string "Zeichenkette")
 (scheme-mode-color-constant "Literal")
 (scheme-mode-color-parenthesis "Klammer")
 (scheme-mode-color-error "Fehler")
 (scheme-mode-color-other "Sonstiges")
 (syntax-coloring-choose-color "Wählen Sie eine Farbe für ~a")
 (preferences-colors "Farben")

 (url "URL")
 (url: "URL:")
 (open-url... "URL öffnen...")
 (open-url "URL öffnen")
 (browse... "Browsen...")
 (bad-url "Ungültige URL")
 (bad-url:this "Ungültige URL: ~a")
 
 ;; Help Desk
 (help "Hilfe")
 (help-desk "Hilfezentrum")
 (plt:hd:search-results "Suchergebnisse")
 (plt:hd:search "Suchen")
 (plt:hd:search-for "Suchen nach")
 (plt:hd:lucky "Glück gehabt!")
 (plt:hd:feeling-lucky "Auf gut Glück")
 (plt:hd:stop "Stop")   
 (plt:hd:options "Optionen") 
 (plt:hd:configure "Konfiguration")
 (plt:hd:home "Hilfezentrum-Homepage") 
 (plt:hd:show-manuals "Handbücher anzeigen") 
 (plt:hd:send-bug-report "Bug-Report")
 (plt:hd:query-bug-reports "Bug-Reports abfragen")
 ; next 3 are popup menu choices in help desk search frame
 (plt:hd:search-for-keyword "Stichworteintrag")
 (plt:hd:search-for-keyword-or-index "Stichwort- oder Index-Eintrag")
 (plt:hd:search-for-keyword-or-index-or-text "Stichwort- oder Index-Eintrag, oder Text")
 (plt:hd:exact-match "Exakte Treffer")
 (plt:hd:containing-match "Teilwort")
 (plt:hd:regexp-match "über regulären Ausdruck")
 (plt:hd:find-docs-for "Finde Dokumentation zu:")
 (plt:hd:nothing-found-for-search-key "Nichts zu \"~a\" gefunden.")
 (plt:hd:searching "Suche...")
 (plt:hd:search-stopped "[Suche gestopt.]")
 (plt:hd:search-stopped-too-many-matches "[Suche abgebrochen: zu viele Treffer]")
 (plt:hd:nothing-found-for "Nichts zu ~a gefunden")
 (plt:hd:error-finding-docs "Konnte die Dokumentation nicht finden.\n\n~a")
 (plt:hd:and "und")
 (plt:hd:refresh "aktualisieren")
 (plt:hd:refresh-all-manuals "alle Handbücher aktualisieren")
 (plt:hd:manual-installed-date "(installiert ~a)")
 ; Help Desk configuration
 (plt:hd:configuration "Konfiguration PLT-Hilfezentrum")
 (plt:hd:no-frames "Ohne Frames")
 (plt:hd:use-frames "Mit Frames")
 (plt:hd:use-html-frames "Mit HTML-frames")
 (plt:hd:search-pane-options "Optionen Such-Panel")
 (plt:hd:height "Höhe")
 (plt:hd:bg-color "Hintergrundfarbe")
 (plt:hd:pixels "Pixel")
 (plt:hd:text-color "Farbe Text")
 (plt:hd:link-color "Farbe Links")
 (plt:hd:text-sample "Text im Such-Panel erscheint in dieser Farbe")
 (plt:hd:link-sample "Links im Such-Panel erscheinen in dieser Farbe")
 (plt:hd:save-changes "Änderungen sichern")
 (plt:hd:reset "Zurücksetzen")
 (plt:hd:defaults "Werkseinstellungen")
 (plt:hd:javascript-note
    "Ihre Selektionen werden hier erscheinen, falls Sie Javascript eingeschaltet haben und einen aktuellen standardkompatiblen Browser benutzen.")
 ;; refreshing manuals
 (plt:hd:refresh-downloading "~a herunterladen")
 (plt:hd:refresh-installing "~a installieren")
 (plt:hd:refresh-progress "Fortschritt beim Herunterladen von PLT-Handbuch")
 (plt:hd:refresh-done "Aktualisierung der Handbücher abgeschlossen")
;; should not mention `SVN' (plt:hd:refresh-done "Aktualisierung der Handbücher aus SVN abgeschlossen")
 (plt:hd:refresh-installation-log "Installations-Protokoll")
 (plt:hd:refresh-stopped "PLT-Handbuch-Aktualisierung gestoppt")
 (plt:hd:refreshing-manuals "Handbücher aktualisieren")
 (plt:hd:refresh-downloading... "~a herunterladen...")
 (plt:hd:refresh-deleting... "Alte Version von ~a löschen...")
 (plt:hd:refresh-installing... "Neue Version von ~a installieren...")
 (plt:hd:refresh-clearing-indicies "Gecachte Indizes löschen")
 (plt:hd:refreshing-manuals-finished "Fertig.")
 (plt:hd:about-help-desk "Über das Hilfezentrum")
 (plt:hd:help-desk-about-string
  "Das Hilfezentrum ist die primäre Quelle für Information über die PLT-Software,insbesondere DrScheme, MzScheme und MrEd.\n\nVersion ~a\nCopyright (c) 1995-2003 PLT")
 (plt:hd:help-on-help "Hilfe zur Hilfe")
 (plt:hd:help-on-help-details "Hilfe zum Hilfezentrum finden Sie auf der Hilfezentrum-Hompage unter 'Help Desk'. (Um auf diese Homepage zu gelangen, drücken Sie den 'Home'-Knopf oben im Hilfezentrum.)")
  (reload "Aktualisieren") ;; refresh the page in a web browser
  (plt:hd:ask-about-separate-browser
   "Sie haben einen Link selektiert, der ins Web zeigt. Wollen Sie die Seite im Hilfe-Browser oder im externen Browser anzeigen?")
  (plt:hd:homebrew-browser "Hilfe-Browser") ;; choice for the above string (in a button)
  (plt:hd:separate-browser "Externer Browser") ;; other choice for the above string (also in a button)
  (plt:hd:external-link-in-help "Externe URLs im Hilfe-Browser")
  (plt:hd:use-homebrew-browser "Den Hilfe-Browser für externe URLs benutzen")
  (plt:hd:new-help-desk "Neues Hilfezentrum")
  (plt:hd:teaching-manuals "Handbücher für Lehrende und Lernende")
  (plt:hd:professional-manuals "Handbücher für Anwender")
  (plt:hd:all-manuals "Alle Handbücher")

  ;; in the Help Desk language dialog, title on the right.
  (plt:hd:manual-search-ordering "Suchreihenfolge Handbuch")


 ;; Help desk htty proxy
 (http-proxy "HTTP-Proxy")
 (proxy-direct-connection "Direkte Verbindung")
 (proxy-use-proxy "Proxy benutzen:")
 (proxy-host "Name")
 (proxy-port "Port")
 (proxy-bad-host "Unzulässiger Proxy")

 ;; browser
 (rewind-in-browser-history "Zurück")
 (forward-in-browser-history "Vor")
 (home "Home")
 (browser "Browser")
 (external-browser-choice-title "Externer Browser")
 (browser-command-line-label "Kommandzeile:")
 (choose-browser "Browser auswählen")
 (no-browser "Später")
 (use-internal-browser-for-help "Hilfe mit internem PLT-Browser lesen")
 (use-external-browser-for-help "Hilfe mit externam Browser lesen")
 (browser-cmdline-expl-line-1 "(Kommandozeile konstruiert durch Aneinanderhängen von Vor-Text, URL,")
 (browser-cmdline-expl-line-2 " und Nach-Text, ohne zusätzliche Leerzeichen dazwischen.")
 (cannot-display-url "Kann URL ~s nicht anzeigen: ~a")
 (install? "Installieren?")  ;; if a .plt file is found (title of dialog)
 (you-have-selected-an-installable-package "Sie haben eine installierbares Paket angewählt.")
 (do-you-want-to-install-it? "Wollen Sie es installieren?")
 (paren-file-size "(Die Datei hat ~a Bytes)")
 (download-and-install "Herunterladen && installieren") ;; button label
 (download "Herunterladen") ;; button label
 (save-downloaded-file/size "Datei (~a Bytes) speichern als") ;; label for get-file dialog
 (save-downloaded-file "Datei speichern als")  ;; label for get-file dialog
 (downloading "Herunterladen") ;; dialog title
 (downloading-file... "Datei herunterladen...")
 (package-was-installed "Das Paket wurde erfolgreich installiert.")
 (download-was-saved "Die Datei wurde erfolgreich gespeichert.")
 (getting-page "Seite laden") ;; dialog title

 (install-plt-file-menu-item... ".plt-Datei installieren...")
 (install-plt-file-dialog-title ".plt-Datei installieren")
 (install-plt-web-tab "Web")
 (install-plt-file-tab "Datei")
 (install-plt-filename "Dateiname:")
 (install-plt-url "URL:")
  
 (install-plt-file "~a installieren oder editieren?")
 (install-plt-file/yes "Installieren")
 (install-plt-file/no "Editieren")
 
 (plt-installer-progress-window-title "Fortschritt Installation")
 (plt-installer-abort-installation "Installation abbrechen")
 (plt-installer-aborted "Abgebrochen.")
  
 ;;; about box
 (about-drscheme-frame-title "Über DrScheme")
 (take-a-tour "Nehmen Sie die Führung!")
 (release-notes "Release-Notes")
 (parenthetical-last-version "(vorige Version ~a)")
 (parenthetical-last-language "(vorige Sprache ~a)")
 (parenthetical-last-version/language "(vorige Version ~a, Sprache ~a)")
 
 
 ;;; save file in particular format prompting.
 (save-as-plain-text "Diese Datei als Text speichern?")
 (save-in-drs-format "Diese Datei im DrScheme-Format (kein Text) speichern?")
 (yes "Ja")
 (no "Nein")
 
 ;;; preferences
 (preferences "Einstellungen")
 (saving-preferences "Einstellungen werden gesichert")
 (error-unmarshalling "Fehler beim Lesen der Einstellung für ~a")
 (error-saving-preferences "Fehler beim Speichern der Einstellungen für ~a")
 (error-reading-preferences "Fehler beim Lesen der Einstellungen")
 (expected-list-of-length2 "Eine Liste mit zwei Elementen erwartet")
 (scheme-prefs-panel-label "Scheme")
 (warnings-prefs-panel-label "Warnmeldungen")
 (editor-prefs-panel-label "Editieren")
 (general-prefs-panel-label "Allgemein")
 (highlight-parens "Geklammerten Text hervorheben")
 (fixup-parens "Klammern korrigieren")
 (flash-paren-match "Passende Klammer anblinken")
 (auto-save-files "Dateien automatisch abspeichern")
 (backup-files "Backup-Dateien")
 (map-delete-to-backspace "Entf löscht rückwärts")
 (verify-exit "Bei Verlassen nachfragen")
 (ask-before-changing-format "For Formatänderung beim Speichern nachfragen")
 (wrap-words-in-editor-buffers "Worte in Editor-Puffern umbrechen")
 (show-status-line "Status-Zeile anzeigen")
 (count-columns-from-one "Spaltennummern fangen mit 1 an")
 (display-line-numbers "Zeilennummern in Puffern anzeigen, keine Puffer-Indizes")
 (enable-keybindings-in-menus "Tastenbelegung für Menüs")
 (automatically-to-ps "Automatisch in PostScript-Datei drucken")
 (option-as-meta "Option-Taste als Mera behandeln") ;; macos/macos x only
 (use-mdi "MDI-Fenster verwenden") ;;; ms windows only -- use that window in a window thingy
 (separate-dialog-for-searching "Für Textsuche separaten Dialog verwenden")
 (reuse-existing-frames "Existierende Fenster für neu geöffnete Dateien wiederverwenden")
 (default-fonts "Standard-Fonts")
 (paren-match-color "Farbe für Klammern-Hervorhebung") ; in prefs dialog
 (choose-color "Farbe auswählen") ; in prefs dialog
 (online-coloring-active "Syntax interaktiv einfärben")
 (open-files-in-tabs "Dateien in separaten Tabs öffnen (nicht separaten Fenstern)")
 (show-interactions-on-execute "Interaktionen beim Programmstart automatisch öffnen")
 (limit-interactions-size "Umfang der Interaktionen einschränken")
 (background-color "Hintergrundfarbe")
 (default-text-color "Standard für Text") ;; used for configuring colors, but doesn't need the word "color"
 (choose-a-background-color "Hintergrundfarbe auswählen")

 ; title of the color choosing dialog
 (choose-paren-highlight-color "Farbe für Klammerhervorhebung wählen")

 ; should have entire alphabet
 (font-example-string "Zwölf Boxkämpfer jagen Victor quer über den großen Sylter Deich.") 

 (change-font-button-label "Ändern")
 (fonts "Schriften")

 ; filled with type of font, eg modern, swiss, etc.
 (choose-a-new-font "Neuen Font für \"~a\" wählen")

 (font-size-slider-label "Größe")
 (restart-to-see-font-changes "Neu starten, damit die Schriftänderung wirksam wird")

 (font-prefs-panel-title "Schriftart")
 (font-name "Name Schriftart")
 (font-size "Größe Schriftart")
 (set-font "Schriftart setzen...")
 (font-smoothing-label  "Weiche Kanten bei Schrift")
 (font-smoothing-none "Nicht")
 (font-smoothing-some "Bißchen")
 (font-smoothing-all "Total")
 (font-smoothing-default "System-Einstellung verwenden")
 (select-font-name "Schriftart-Name auswählen")
 (example-text "Beispieltext:")
 (only-warn-once "Nur einmal warnen, wenn Definitionen und Interaktionen nicht synchron sind")
 
 ; warning message when lockfile is around
 (waiting-for-pref-lock "Auf Lock-Datei für Einstellungen warten...")
 (pref-lock-not-gone
  "Die Lock-Datei für die Einstellungen:\n\n   ~a\n\nverhindert, dass die Einstellungen abgespeichert werden können. Bitte stellen Sie sicher, dass keine andere PLT-Software läuft und löschen Sie dann diese Datei.")
 (still-locked-exit-anyway? "Die Einstellungen wurden nicht korrekt gespeichert.  Trotzdem beenden?")
 
 ;;; indenting preferences panel
 (indenting-prefs-panel-label "Einrücken")
 (indenting-prefs-extra-regexp "Zusätzlicher Regexp")

 ; filled with define, lambda, or begin
 (enter-new-keyword "Bitte ein Schlüsselwort wie ~a eingeben:")
 (x-keyword "~a-Schlüsselwort")
 (x-like-keywords "Schlüsselwort wie ~a")

 (expected-a-symbol "Symbol erwartet, stattdessen bekommen: ~a")
 (already-used-keyword "\"~a\" ist bereits ein Schlüsselwort mit Spezial-Einrückung")
 (add-keyword "Hinzufügen")
 (remove-keyword "Entfernen")
 
 ;;; find/replace
 (find-and-replace "Suchen und Ersetzen")
 (find "Suchen")
 (replace "Ersetzen")
 (dock "Andocken")
 (undock "Ablegen")
 (use-separate-dialog-for-searching "Separaten Dialog für Suchen verwenden")
 (replace&find-again "Nochmals Suchen && Ersetzen") ;;; need double & to get a single &
 (replace-to-end "Ersetzen bis zum Ende")
 (forward "Vorwärts")
 (backward "Rückwärts")
 (hide "Ausblenden")
 
 ;;; multi-file-search
 (mfs-multi-file-search-menu-item "In Dateien suchen...")
 (mfs-string-match/graphics "per Text (auch in Dateien mit Grafik)")
 (mfs-regexp-match/no-graphics "per regulärem Ausdruck (nur reine Textdateien)")
 (mfs-searching... "Suche...")
 (mfs-configure-search "Einstellungen Suche") ;; dialog title
 (mfs-files-section "Dateien")   ;; section in config dialog
 (mfs-search-section "Suche") ;; section in config dialog
 (mfs-dir "Verzeichnis")
 (mfs-recur-over-subdirectories "In Unterverzeichnisse abtauchen")
 (mfs-regexp-filename-filter "Regulärer Ausdruck Dateinamen-Filter")
 (mfs-search-string "Zeichenkette suchen")
 (mfs-drscheme-multi-file-search "DrScheme - Suche in mehreren Dateien") ;; results window and error message title
 (mfs-not-a-dir "\"~a\" ist kein Verzeichnis")
 (mfs-open-file "Datei öffnen")
 (mfs-stop-search "Suche stoppen")
 (mfs-case-sensitive-label "Groß-/Kleinschreibung beachten")
 (mfs-no-matches-found "Keine Treffer gefunden.")
 (mfs-search-interrupted "Suche abgebrochen.")
 
 ;;; reverting a file
 (error-reverting "DrScheme - Fehler beim Wiederherstellen")
 (could-not-read "Konnte \"~a\" nicht lesen")
 (are-you-sure-revert
  "Sind Sie sicher, dass Sie diese Datei wiederherstellen wollen? Diese Operation kann nicht rückgängig gemacht werden.")
 (are-you-sure-revert-title
  "Wiederherstellen?")
 
 ;;; saving a file
 ; ~a is filled with the filename
 (error-saving "Fehler beim Speichern") ;; title of error message dialog
 (error-saving-file/name "Fehler beim Speichern von ~a")
 (error-loading "Fehler beim Laden")
 (error-loading-file/name "Fehler beim Laden von ~a.")
 (unknown-filename "<< unbekannt >>")

 ;;; finder dialog
 (must-specify-a-filename "Sie müssen einen Dateinamen angeben")
 (file-does-not-exist "Die Datei \"~a\" existiert nicht.")
 (ask-because-file-exists "Die Datei \"~a\" existiert schon. Ersetzen?")
 (dne-or-cycle "Der Dateiname \"~a\" enthält ein nicht existentes Verzeichnis oder einen Zyklus.")
 (get-file "Datei lesen")
 (put-file "Datei schreiben")
 (full-pathname "Gesamter Dateiname")
 (show-dot-files "Dateien und Verzeichnisse anzeigen, die mit einem Punkt anfangen.")
 (up-directory-button-label "Verzeichnis nach oben")
 (add-button-label "Hinzufügen") ;;; for multi-file selection
 (add-all-button-label "Alle hinzufügen") ;;; for multi-file selection
 (remove-button-label "Entfernen") ;;; for multi-file selection
 (file-wrong-form "Der Dateiname hat nicht die richtige Form.")
 (select-files "Dateien auswählen")
 (select-file "Datei auswählen")
 (dir-dne "Das Verzeichnis existiert nicht.")
 (file-dne "Die Datei existiert nicht.")
 (empty-filename "Der Dateiname muss Buchstaben enthalten.")
 (that-is-dir-name "Dieser Name gehört zu einem Verzeichnis.")
 
 ;;; raw menu names -- these must match the 
 ;;; versions below, once the &s have been stripped.
 ;;; if they don't, DrScheme's menus will appear
 ;;; in the wrong order.
 (file-menu "Datei")
 (edit-menu "Bearbeiten")
 (help-menu "Hilfe")
 (windows-menu "Fenster")
 
 ;;; menus
 ;;; - in menu labels, the & indicates a alt-key based shortcut.
 ;;; - sometimes, things are stuck in the middle of 
 ;;; menu item labels. For instance, in the case of
 ;;; the "Save As" menu, you might see: "Save Definitions As". 
 ;;; be careful of spacing, follow the English, if possible.
 ;;; - the ellipses in the `after' strings indicates that
 ;;; more information is required from the user before completing
 ;;; the command.

 (file-menu-label "&Datei")

 (new-info  "Neue Datei öffnen")
 (new-menu-item "&Neu")
 (new-...-menu-item "&Neu...")

 (open-info "Datei öffnen")
 (open-menu-item "&Öffnen...")
 (open-here-menu-item "Hier &öffnen...")

 (open-recent-info "Liste kürzlich bearbeiteter Dateien")
 (open-recent-menu-item "Öffne noch einmal")
 
 (revert-info "Stelle diese Datei wieder her wie zuletzt gespeichert")
 (revert-menu-item "&Wiederherstellen")

 (save-info "Diese Datei auf der Platte speichern")
 (save-menu-item "&Speichern")

 (save-as-info "Dateinamen abfragen und dann Datei auf der Platte speichern")
 (save-as-menu-item "Speichern &unter...")

 (print-info "Diese Datei zum Drucker schicken")
 (print-menu-item "&Drucken...")

 (close-info "Diese Datei schließen")
 (close-menu-item "&Schließen")

 (quit-info "Alle Fenster schließen")
 (quit-menu-item-windows "Be&enden")
 (quit-menu-item-others "&Beenden")
 
 (edit-menu-label "&Bearbeiten")
 
 (undo-info "Letzte Aktion rückgängig machen")
 (undo-menu-item "&Rückgängig")

 (redo-info "Letzte Rückgängig-Operation rückgängig machen")
 (redo-menu-item "&Nochmal")

 (cut-info "Verschiebe die Selektion ins Clipboard, um sie später wieder einfügen zu können")
 (cut-menu-item "&Ausschneiden")

 (copy-info "Kopiere die Selektion ins Clipboard, um sie später wieder einfügen zu könne")
 (copy-menu-item "&Kopieren")

 (paste-info "Ersetze die aktuelle Selektion durch die zuletzt kopierte oder ausgeschnittene Selektion")
 (paste-menu-item "&Einfügen")

 (clear-info "Lösche die Selektion, ohne das Clipboard dabei zu ändern oder etwas einzufügen")
 (clear-menu-item-others "Löschen")
 (clear-menu-item-windows "&Löschen")

 (select-all-info "Selektiere das gesamte Dokument")
 (select-all-menu-item "&Alles selektieren")
 
 (find-info "Suche eine Zeichenkette")
 (find-menu-item "Suche...")

 (find-again-info "Suche die gleiche Zeichenkette nochmal")
 (find-again-menu-item "Suche nochmal")
 
 (replace-and-find-again-info "Ersetze den aktuellen Text und suche dann das gleiche nochmal")
 (replace-and-find-again-menu-item "Ersetzen && nochmal suchen")

 (preferences-info "Konfiguriere die Einstellungen")
 (preferences-menu-item "Einstellungen...")

 (keybindings-info "Aktuelle Tastaturbelegung anzeigen")
 (keybindings-menu-item "Tastaturbelegung")
 (keybindings-show-active "Aktive Tastenbelegungen anzeigen")
 (keybindings-frame-title "Tastaturbelegung")
 (keybindings-sort-by-name "Nach Name sortieren")
 (keybindings-sort-by-key "Nach Taste sortieren")
 (keybindings-add-user-defined-keybindings "Benutzerdefinierte Tastenbelegungen hinzufügen...")
 (keybindings-menu-remove "~a entfernen")
 (keybindings-choose-user-defined-file "Bitte eine Datei mit den Tastenbelegungen auswählen.")

 (user-defined-keybinding-error "Fehler beim Ausführen der Tastenbelegung ~a\n\n~a")
 (user-defined-keybinding-malformed-file "Die Datei ~a enthält kein Modul, das in der Sprache (lib \"keybinding-lang.ss\" \"framework\") geschrieben ist.")  

 ;; menu items in the "special" menu
 (insert-text-box-item "Text-Kasten einfügen")
 (insert-pb-box-item "Pinwand-Kasten einfügen")
 (insert-image-item "Bild einfügen...")
 (insert-comment-box-menu-item-label "Kommentarkasten einfügen")
 (insert-lambda "&Lambda einfügen")
 (insert-delta "&Delta (define) einfügen")

 (wrap-text-item "Text umbrechen")

 (windows-menu-label "&Fenster")
 (bring-frame-to-front "Fenster nach vorn")       ;;; title of dialog
 (bring-frame-to-front... "Fenster nach vorn...") ;;; corresponding title of menu item
 (next-window "Nächstes Fenster")
 (previous-window "Voriges Fenster")
 (most-recent-window "Letztes Fenster")

 (view-menu-label "&Anzeigen")
 (show-overview "Programm-Umriss einblenden") 
 (hide-overview "Programm-Umriss ausblenden")
 (show-module-browser "Modul-Browser einblenden")
 (hide-module-browser "Modul-Browser ausblenden")

 (help-menu-label "&Hilfe")
 (about-info "Mehr über dieses Programm und seine Entstehung")
 (about-menu-item "Über...")
 (help-menu-check-for-updates "Nach Updates schauen...")
 
 ;; open here's new menu item
 (create-new-window-or-clear-current
  "Würden Sie gern ein neues Fenster aufmachen oder dieses hier löschen und wiederverwenden?")
 (clear-current "Dieses löschen")
 (new-window "Neues Fenster")

 ;;; exiting and quitting ``are you sure'' dialog
 ;;; exit is used on windows, quit on macos, in English. Other
 ;;; languages probably use the same word on both platforms.
 (exit "Beenden")
 (quit "Beenden")
 (are-you-sure-exit "Sind Sie sicher, dass Sie das Programm beenden wollen?")
 (are-you-sure-quit "Sind Sie sicher, dass Sie das Programm beenden wollen?")
 
 ;;; autosaving
 (error-autosaving "Fehler beim automatischen Speichern von \"~a\".") ;; ~a will be a filename
 (autosaving-turned-off "Automatisches Speichern abgeschaltet\nbis die Datei wieder gespeichert wird.")
 (recover-autosave-files-frame-title "Automatisch gespeicherte Dateien zurückholen")
 (autosave-details "Details")
 (autosave-recover "Zurückholen")
 (autosave-unknown-filename "<<unbekannt>>")
  
  ;; these are labels in a dialog that drscheme displays
  ;; if you have leftover autosave files. to see the dialog,
  ;; start up drscheme and modify (but don't save) a file
  ;; (also, do this with an unsaved file). Wait for the autosave
  ;; files to appear (typically 5 minutes). Kill DrScheme
  ;; and restart it. You'll see the dialog
  (autosave-autosave-label: "Automatisch gespeicherte Datei:")
  (autosave-original-label: "Ursprüngliche Datei:")
  (autosave-autosave-label "Automatisch gespeicherte Datei")
  (autosave-original-label "Ursprüngliche Datei")
  (autosave-compare-files "Automatisch gespeicherte Dateien vergleichen")

  (autosave-show-autosave "Automatisch gespeicherte Datei") ;; title of a window showing the autosave file

  (autosave-explanation "DrScheme hat automatisch gespeicherte Dateien gefunden, die nicht regulär gespeicherten Inhalt enthalten könnten.")

  (autosave-recovered! "Zurückgeholt!") ;; status of an autosave file
  (autosave-deleted "Gelöscht")       ;; status of an autosave file

  (autosave-error-deleting "Fehler beim Löschen von ~a\n\n~a") ;; first is a filename, second is an error message from mz.
  (autosave-delete-button "Löschen")
  (autosave-delete-title "Löschen")  ;; title of a dialog asking for deletion confirmation
  (autosave-done "Fertig")
  
  ;; appears in the file dialog
  (autosave-restore-to-where? "Bestimmen Sie, wo die automatisch gespeicherte Datei hin zurückgeholt werden soll")
  
  
 ;;; file modified warning
 (file-has-been-modified
  "Die Datei wurde verändert, seit sie das letzte Mal gespeichert wurde. Änderungen überschreiben?")
 (overwrite-file-button-label "Überschreiben")
 
 (definitions-modified 
  "Die Definitionen wurden auf der Platte geändert; bitte speichern sie die Definitionen oder holen Sie diese von der Platte zurück.")
 (drscheme-internal-error "Interner Fehler in DrScheme")
 
 ;;; tools
 (invalid-tool-spec "Die Tool-Spezifikation in der Datei info.ss der Kollektion ~a enthält Fehler. Da sollte eine Zeichenkette oder eine Liste von Zeichenketten stehen, tatsächlich steht dort aber: ~e")
 (error-loading-tool-title "DrScheme - Fehler beim Laden von ~s; ~s")
 (error-invoking-tool-title "Fehler beim Starten von Tool ~s;~s")
 (tool-tool-names-same-length
  "`tool-names' und `tools' in info.ss für ~s müssen Listen der gleichen Länge sein, tatsächlich stehen dort ~e und ~e")
 (tool-tool-icons-same-length
  "`tool-icons' und `tools' in info.ss für ~s müssen Listen der gleichen Länge sein, tatsächlich stehen dort ~e und ~e")
 (tool-tool-urls-same-length
  "`tool-urls' und `tools' in info.ss für ~s müssen Listen der gleichen Länge sein, tatsächlich stehen dort ~e und ~e")
 (error-getting-info-tool
  "Fehler beim Laden von info.ss file für ~s")
 (tool-error-phase1 "Fehler in Phase 1 von Tool ~s; ~s")
 (tool-error-phase2 "Fehler in Phase 2 von Tool ~s; ~s")


 ;;; define popup menu
 (end-of-buffer-define "<< Text-Ende >>")
 (sort-by-name "Nach Namen Sortieren")
 (sort-by-position "Nach Position in der Datei sortieren")
 (no-definitions-found "<< keine Definitionen gefunden>>")
 (jump-to-defn "Zur Definition von ~a springen")

 (recent-items-sort-by-age "Nach Alter sortieren")
 (recent-items-sort-by-name "Nach Name sortieren")
 
 ;;; view menu
 (hide-definitions-menu-item-label "&Definitionen ausblenden")
 (show-definitions-menu-item-label "&Definitionen einblenden")
 (definitions-menu-item-help-string "Definitionsfenster ein-/ausblenden")
 (show-interactions-menu-item-label "&Interaktionen einblenden")
 (hide-interactions-menu-item-label "&Interaktionen ausblenden")
 (interactions-menu-item-help-string "Interaktionsfenster ein-/ausblenden")
 (show-toolbar "&Toolbar einblenden")
 (hide-toolbar "&Toolbar ausblenden")

 ;;; file menu
 (save-definitions-as "Definitionen speichern unter...")
 (save-definitions "Definitionen speichern")
 (print-definitions "Definition drucken...")
 (about-drscheme "Über DrScheme")
 (save-other "Speichern unter")
 (save-definitions-as-text "Definitionen als Text speichern...")
 (save-interactions "Interaktionen speichern")
 (save-interactions-as "Interaktionen speichern unter...")
 (save-interactions-as-text "Interaktionen als Text speichern...")
 (print-interactions "Interaktionen drucken...")
 (new-tab "Neuer Tab")
 (close-tab "Tab schließen")
 
 ;;; edit-menu
 (split-menu-item-label "&Splitten")
 (collapse-menu-item-label "Einfalten")
 
 ;;; language menu
 (language-menu-name "&Sprache")
 
 ;;; scheme-menu
 (scheme-menu-name "S&cheme")
 (execute-menu-item-label "Start")
 (execute-menu-item-help-string "Das Programm im Definitionsfenster neu starten")
 (break-menu-item-label "Stop")
 (break-menu-item-help-string "Momentane Auswertung unterbrechen")
 (kill-menu-item-label "Abbrechen")
 (kill-menu-item-help-string "Momentante Auswertung abbrechen")
 (clear-error-highlight-menu-item-label "Fehlermarkierung entfernen")
 (clear-error-highlight-item-help-string "Entfernt die rosa Fehlermarkierung")
 (reindent-menu-item-label "&Einrücken")
 (reindent-all-menu-item-label "&Alles einrücken")
 (semicolon-comment-out-menu-item-label "Mit Semikolon auskommentieren")
 (box-comment-out-menu-item-label "Mit Kommentar-Kasten auskommentieren")
 (uncomment-menu-item-label "Einkommentieren")

 (convert-to-semicolon-comment "In Semikolon-Kommentar umwandeln")
 
 ;;; executables
 (create-executable-menu-item-label "Programmdatei generieren...")
 (create-executable-title "Programmdatei generieren")
 (must-save-before-executable "Sie müssen vor der Generierung einer Programmdatei speichern.")
 (save-an-executable "Programmdatei speichern")
 (save-a-mred-launcher "MrEd-Launcher speichern")
 (save-a-mzscheme-launcher "MzScheme-Launcher speichern")
 (save-a-mred-stand-alone-executable "MrEd-Stand-Alone-Programmdatei speichern")
 (save-a-mzscheme-stand-alone-executable "MzScheme-Stand-Alone-Programmdatei speichern")

 (definitions-not-saved "Die Definitionen sind nicht gespeichert. Die Programmdatei wird von der letzten gespeicherten Version gezogen. Weitermachen?")
 (inline-saved-program-in-executable?
  "Scheme-Code in das Programm einbinden? Dann könnten Sie die Programmdatei zu einem anderen ~a-Computer transferieren, aber die Programmdatei wird dann ziemlich groß. Falls nicht, können Sie die Programmdatei nicht transferieren, aber sie wird deutlich kleiner. Außerdem wird die Programmdatei dann die jeweils neueste Version des Scheme-Codes benutzen.")
 (use-mred-binary?
  "MrEd für diese Programmdatei verwenden?\n\nFalls ja, kann das Programm die Bibliothek (lib \"mred.ss\" \"mred\") verwenden. Falls nein, wird DrScheme MzScheme verwenden - dann kann das Programm die Bibliothek nicht verwenden.n\nFalls Sie nicht sicher sind, wählen Sie \"ja\".")
 (inline-saved-program-in-executable/windows/path
   "WARNUNG! Die generierte Programmdatei benötigt drei DLLs: libmred.dll, libmzsch.gll und libgc.dll, die sich in folgendem Verzeichnis befinden:\n\n~a\n\nDie Programmdatei findet DLLs entweder im selben Verzeichnis wie die Programmdatei selbst oder durch die Umgebungsvariable PATH.\n\nAls Sie DrScheme installierten, hat der Installer PATH derart verwändert, dass das DLL-Verzeichnis dabei ist. Diese Einstellung könnte seitdem geändert worden sein.\n\nFalls Sie die Programmdatei auf eine andere Maschine tranferieren, müssen Sie die DLLs ebenfalls transferieren - entweder in das gleiche Verzeichnis wie die Programmdatei oder in ein Verzeichnis im PATH der anderen Maschine.")
 (launcher "Launcher")
 (stand-alone "Stand-alone")
 (executable-type "Typ")
 (executable-base "Hauptteil")
 (filename "Dateiname: ")
 (create "Erzeugen")
 (please-choose-an-executable-filename "Bitte Dateinamen für Programm auswählen")
 (windows-executables-must-end-with-exe
  "Der Dateiname\n\n  ~a\n\nist unzulässig. Unter Windows müssen Programmdateien mit .exe enden.")
 (macosx-executables-must-end-with-app
  "Der Dateiname\n\n  ~a\n\nist unzulässig. Unter Mac OS X müssen Namen für Programme mit .app enden.")
 (warning-directory-will-be-replaced
  "WARNUNG: das Verzeichnis:\n\n  ~a\n\nwird überschrieben werden. Weitermachen?")
 
 (create-servlet "Servlet erzeugen...")

 ; the ~a is a language such as "module" or "algol60"
 (create-servlet-unsupported-language
  "Servlet lassen sich nicht aus einem Programm in der Sprache \"~a\" erzeugen.")
  
 ;;; buttons
 (execute-button-label "Start") 
 (save-button-label "Speichern")
 (break-button-label "Stop")
 
 ;;; search help desk popup menu
 (search-help-desk-for "Suche im Hilfezentrum nach \"~a\"")
 (exact-lucky-search-help-desk-for "Exakte Suche im Hilfezentrum auf gut Glück nach \"~a\"")

 ;; collapse and expand popup menu items
 (collapse-sexp "S-Expression einfalten")
 (expand-sexp "S-Expression wieder ausfalten")
 
 ;;; fraction dialog
 (enter-fraction "Bruch eingeben")
 (whole-part "Ganzzahliger Anteil")
 (numerator "Zähler")
 (denominator "Nenner")
 (invalid-number "Unzulässige Zahl: muss exakt, reell und nicht ganz sein.")
 (insert-fraction-menu-item-label "Bruch einfügen...")

 ;; number snip popup menu
 (show-decimal-expansion "Als Dezimalexpansion anzeigen")
 (show-fraction-view "Als Bruch anzeigen")
 (show-mixed-fraction-view "Als gemischten Bruch anzeigen")
 (show-improper-fraction-view "Als ungemischten Bruch anzeigenn")
 (show-more-decimal-places "Mehr Dezimalziffern anzeigen")
 
 ;;; Teachpack messages
 (select-a-teachpack "Teachpack auswählen")
 (clear-teachpack "Teachpack ~a herauswerfen")
 (teachpack-error-label "DrScheme - Teachpack-Fehler")
 (teachpack-dne/cant-read "Die Teachpack-Datei ~a existiert nicht oder ist nicht lesbar.")
 (teachpack-didnt-load "Die Teachpack-Datei ~a konnte nicht korrekt geladen werden.")
 (teachpack-error-invoke "Die Teachpack-Datei ~a hat beim Start ein Problem signalisiert.")
 (add-teachpack-menu-item-label "Teachpack hinzufügen...")
 (clear-all-teachpacks-menu-item-label "Alle Teachpacks herauswerfen")
 (drscheme-teachpack-message-title "DrScheme-Teachpack")
 (already-added-teachpack "Teachpack ~a ist schon dabei")
 
 ;;; Language dialog
 (introduction-to-language-dialog
  "Bitte eine Sprache auswählen. Für den Anfängerkurs ist wahrscheinlich die voreingestellte Sprache die richtige.")
 (language-dialog-title "Sprache auswählen")
 (case-sensitive-label "Groß-/Kleinschreibung unterscheiden")
 (output-style-label "Ausgabenotation")
 (constructor-printing-style "Konstruktor")
 (quasiquote-printing-style "Quasiquote")
 (write-printing-style "write")
 (print-printing-style "current-print")
 (sharing-printing-label "Zeige Sharing an")
 (use-pretty-printer-label "Zeilenumbrüche in Ausdruck einfügen")
 (input-syntax "Eingabesyntax")
 (dynamic-properties "Laufzeit")
 (output-syntax "Ausgabesyntax")
 (no-debugging-or-profiling "Kein Debugging oder Profiling")
 (debugging "Debugging")
 (debugging-and-profiling "Debugging und Profiling")
 (test-coverage "Syntaktische Test-Suiten-Abdeckung")
 (whole/fractional-exact-numbers-label "Zahlen als Brüche ausdrucken")
 (booleans-as-true/false-label "Booleans als \"true\" und \"false\" ausdrucken")
 (show-details-button-label "Details einblenden")
 (hide-details-button-label "Details ausblenden")
 (choose-language-menu-item-label "Sprache auswählen...")
 (revert-to-language-defaults "Standard-Spracheinstellungen wiederherstellen")
 (language-docs-button-label "Dokumentation für Sprache")
 (fraction-style "Bruch-Ausgabe")
 (use-mixed-fractions "gemischte Brüche")
 (use-repeating-decimals "Dezimalausgabe mit Perioden")
 (decimal-notation-for-rationals "Dezimalnotation für Brüche")
 (please-select-a-language "Bitte Sprache auswählen")

 ;; startup wizard screen language selection section
 (please-select-a-language "Sprache auswählen")
 (show-all-languages "Alle sprachen anzeigen")
 (show-drscheme-usage-questions "Fragen zur Benutzung von DrScheme anzeigen")
 (are-you...-kind-of-drscheme-user "Sind Sie ...")
 (use-with-htdp "... ein DrScheme-Benutzer, der mit \"How to Design Programs\" arbeitet?")
 (use-seasoned "... ein erfahrener PLT-Schemer?")
 (use-other "... ein DrScheme-Benutzer aus anderen Gründen?")
 (use-eopl "... ein DrScheme-Benutzer, der mit \"Essentials of Programming Languages\" arbeitet?")
 (pl-lang-choice-format "Sprache am Anfang: ~a")
 (choose-new-language-before-running "Bitte wählen Sie eine andere Sprache vor dem Start aus.")

 
 ;;; languages
 (beginning-student "Anfänger")
 (beginning-one-line-summary "define, cond, Strukturen, Konstanten und Primitiva")
 (beginning-student/abbrev "Anfänger mit Listen-Abkürzungen")
 (beginning/abbrev-one-line-summary "Anfänger, wobei Listen mit \"list\" in der REPL ausgedruckt werden")
 (intermediate-student "Zwischenstufe")
 (intermediate-one-line-summary "Anfänger plus lexikalische Bindung")
 (intermediate-student/lambda "Zwischenstufe mit lambda")
 (intermediate/lambda-one-line-summary "Zwischenstufe plus Prozeduren höherer Ordnung")
 (advanced-student "Fortgeschritten")
 (advanced-one-line-summary "Zwischenstufe plus lambda und Mutation")
 (full-language "Alles") ;; also in the HtDP languages section
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (r5rs-like-languages "R5RS-verwandet")
 (pretty-big-scheme "Kombo (enthält MrEd and Fortgeschritten)")
 (pretty-big-scheme-one-line-summary "Macht Syntax and Prozeduren der HtDP-Sprachen verfügbar")
 (r5rs-lang-name "Standard (R5RS)")
 (r5rs-one-line-summary "R5RS, ohne alles andere")
 (expander "Expander")
 (expander-one-line-summary "Expandiert Ausdrücke, statt sie auszuwerten")
 (professional-languages "Sprachen für Entwickler")
 (teaching-languages "Lehrsprachen")
 (experimental-languages "Experimentelle Sprachen")
 (initial-language-category "Sprache am Anfang")
 (no-language-chosen "Keine Sprache ausgewählt")

 (module-language-one-line-summary "Start erzeugt eine REPL im Kontext des Moduls inklusive der deklarierten Sprache des Moduls.")
  
  ;;; from the `not a language language' used initially in drscheme.
 (must-choose-language "DrScheme kann keine Programme verarbeiten, bis Sie eine Sprache auswählen.")
 
 ;; next two appear before and after the name of a text book (which will be in italics)
 (using-a-textbook-before "Benutzen Sie ")
 (using-a-textbook-after "?")
 
 ;; next two are before and after a language
 (start-with-before "Anfangen mit ")
 (start-with-after ".")

 (seasoned-plt-schemer? "Erfahrener PLT-Schemer?")
 (looking-for-standard-scheme? "Wollen Sie Standard-Scheme?")
 
 ;; the three string constants are concatenated together and the middle
 ;; one is hyperlinked to the dialog that suggests various languages
 (get-guidance-before "Wählen Sie “Sprache auswählen...” im “Sprache”-Menü oder ")
 (get-guidance-during "Hilfe anfordern")
 (get-guidance-after ".")

 ;;; debug language
 (unknown-debug-frame "[unbekannt]")
 (backtrace-window-title "Backtrace - DrScheme")
 (files-interactions "Interaktionen von ~a") ;; filled with a filename
 (current-interactions "Interaktionen")
 (current-definitions "Definitionen")
 (mzscheme-w/debug "Text (MzScheme, mit R5RS)")
 (mzscheme-one-line-summary "Die PLT-Version von Scheme")
 (mred-w/debug "Grafisch (MrEd, mit MzScheme)")
 (mred-one-line-summary "MzScheme + GUI-Bibliothek")

 ;; profiling
 (profiling-low-color "Wenig")
 (profiling-high-color "Viel")
 (profiling-choose-low-color "Bitte Farbe für \"wenig\" auswählen")
 (profiling-choose-high-color "Bitte Farbe für \"viel\" auswählen")
 (profiling "Profiling")
 (profiling-example-text "(define (whee) (whee))")
 (profiling-color-config "Farbbereich für Profiling") 
 (profiling-scale "Farbskala für Profiling")
 (profiling-sqrt "Wurzel")
 (profiling-linear "Linear")
 (profiling-square "Quadrat")
 (profiling-number "Aufrufanzahl")
 (profiling-time "Gesamtzeit")
 (profiling-update "Profile atkualisieren")
 (profiling-col-percent-time "% Zeit")
 (profiling-col-function "Prozedur")
 (profiling-col-name "Name")
 (profiling-col-time-in-msec "ms")
 (profiling-col-calls "Aufrufe")
 (profiling-show-profile "Profile einblenden")
 (profiling-hide-profile "Profile ausblenden")
 (profiling-unknown-src "<< unbekannt >>")
 (profiling-no-information-available "Es ist keine Profiling-Information verfügbar. Bitte stellen Sie sicher, dass Profiling eingeschaltet und Ihr Programm gelaufen ist.")
 (profiling-clear? "Änderungen im Definitionsfenster machen die Profiling-Informationen ungültig. Weitermachen?")
 
 ;; test coverage
 (test-coverage-clear? "Änderungen im Definitionsfenster machen die Information über Testabdeckung ungültig. Weitermachen?")
 (test-coverage-clear-and-do-not-ask-again "Ja, und nicht nicht wieder fragen")
 (test-coverage-ask? "Frage nach dem Löschen der Testabdeckungs-Information")
  
 ;; tracing
 (tracing-enable-tracing "Tracing einschalten")
 (tracing-show-tracing-window "Tracing einblenden")
 (tracing-hide-tracing-window "Tracing ausblenden")
 (tracing-tracing-nothing-to-show "Es liegen keine Tracing-Resultate vor. Stellen Sie sicher, dass die eingestellte Sprache Tracing unterstützt und dass Tracing eingeschaltet ist.")

 ;;; repl stuff
 (evaluation-terminated "Auswertung abgebrochen")
 (evaluation-terminated-explanation
  "Der Auswertungs-Thread läuft nicht mehr; es findet also keine Auswertung bis zum nächsten Programmlauf statt.")
 (last-stack-frame "letzten Stack-Frame zeigen")
 (last-stack-frames "die letzten ~a Stack-Frames zeigen")
 (next-stack-frames "die nächsten ~a Stack-Frames zeigen")
 
 ;;; welcoming message in repl
 (language "Sprache")
 (custom "angepasst")
 (teachpack "Teachpack")
 (welcome-to "Willkommen bei")
 (version "Version")
 
 ;;; kill evaluation dialog
 (kill-evaluation? "Auswertung abbrechen?")
 (just-break "Nur unterbrechen")
 (kill "Abbrechen")
 (kill? "Abbrechen?")

 ;;; version checker
 ;; the next two are used in the initial wizard dialog.
 ;; Note that vc-wizard-check-prompt can (should) have newlines so
 ;; it will not make the dialog too wide.
 (vc-wizard-check-note "Die Version, die Sie gerade installieren wollen, könnte veraltet\n sein. Wenn Sie wollen, kann DrScheme nachsehen.")
 (vc-wizard-check-button "Nach Updates schauen")
 (vc-update-check "Update-Prüfung")
 (vc-please-wait "Bitte warten")
 (vc-connecting-version-server "Mit PLT-Versions-Server verbinden")
 (vc-network-timeout "Netzwerk-Timeout") 
 (vc-cannot-connect  "Verbindungsversuch zum PLT-Versions-Server fehlgeschlagen")
 (vc-network-failure "Netzwerkproblem")
 (vc-old-binaries "Die installierten Programmdateien für DrScheme (oder MzScheme) sind veraltet")
 (vc-binary-information-format "Version installierte Programmdatei: ~a (Iteration ~a)")
 (vc-details-format "~a~nDetails:~n~a")
 (vc-details-text "Details:~n")
 (vc-error-format "Fehler: ~a") 
 (vc-current-format "~a v.~a (Iteration ~a) ist auf dem aktuellen Stand")
 (vc-update-format "~a v.~a (Iteration ~a) sollte aktualisiert werden auf v.~a (Iteration ~a)")
 (vc-binary-name "Programmdatei")
 (vc-updates-available "Updates sind verfügbar auf")
 (vc-latest-binary-information-format "Neuestes Release ist Version ~a (Iteration ~a)")
 (vc-update-dialog-title "PLT-Update-Status")
 (vc-need-update-string "Ein oder mehrere installierte PLT-Software-Pakete sind veraltet")
 (vc-no-update-string "Alle installierten PLT-Software-Pakte sind auf dem neuesten Stand")

 ;; special menu
 (special-menu "S&pezial")
 
 ;; large semi colon letters
 (insert-large-letters... "Große Buchstaben einfügen...")
 (large-semicolon-letters "Große Buchstaben aus Semikolons")
 (text-to-insert "Einzufügender Text")

 (module-browser-filename-format "Vollständiger Dateiname: ~a (~a Zeilen)")
 (module-browser-root-filename "Basis-Dateiname: ~a")
 (module-browser-font-size-gauge-label "Schriftgröße")
 (module-browser-progress-label "Fortschritt Modul-Übersicht")
 (module-browser-adding-file "Datei ~a hinzufügen...")
 (module-browser-laying-out-graph-label "Graph-Layout")
 (module-browser-open-file-format "~a öffnen")
 (module-browser "Modul-Browser") ;; frame title
 (module-browser... "Modul-Browser...") ;; menu item title
 (module-browser-error-expanding "Fehler beim Expandieren des Programms:\n\n~a")
 (module-browser-show-lib-paths "Dateien anzeigen, die über (lib ..)-Pfade eingebunden wurden")
 (module-browser-progress "Modul-Browser: ~a") ;; prefix in the status line
 (module-browser-compiling-defns "Modul-Browser: Definition compilieren")
 (module-browser-show-lib-paths/short "\"lib\"-requires folgen") ;; check box label in show module browser pane in drscheme window.
 (module-browser-refresh "Aktualisieren") ;; button label in show module browser pane in drscheme window.
 (module-browser-only-in-plt-and-module-langs
  "Der Modul-Browser ist nur für Programme in den PLT-Sprachen und in der Modul-Sprache verfügbar (und nur für Programme mit Modulen).")
 (module-browser-name-length "Länge der Namen")
 (module-browser-name-short "Kurz")
 (module-browser-name-medium "Mittel")
 (module-browser-name-long "Lang")
 (module-browser-open-all "Alle hier angezeigten Datein öffnen")

 (happy-birthday-matthias "Happy Birthday, Matthias!")
 (happy-birthday-matthew "Happy Birthday, Matthew!")
 (happy-birthday-shriram "Happy Birthday, Shriram!")

 (mrflow-using-default-language-title "Standard-Sprache verwendet")
 (mrflow-using-default-language "Die momentan verwendete Sprache hat keine Typ-Tabelle für ihre Primitiva.  Verwende stattdessen R5RS-Scheme.")
 (mrflow-button-title "Analyse")
 ;(mrflow-unknown-style-delta-error-title "Unknown Box Style Delta")
 ;(mrflow-unknown-style-delta-error "Unknown box style delta: ~a")
 (mrflow-coloring-error-title "Farbe für \"unbekannt\"")
 (mrflow-coloring-error "Kein Style für Farbe ~a verwendet")
 (mrflow-popup-menu-show-type "Typ einblenden")
 (mrflow-popup-menu-hide-type "Typ ausblenden")
 (mrflow-popup-menu-show-errors "Fehler einblenden")
 (mrflow-popup-menu-hide-errors "Fehler ausblenden")
 ;(mrflow-read-exception-title "Read Exception")
 ;(mrflow-read-exception "Read exception: ~a")
 ;(mrflow-syntax-exception-title "Syntax Exception")
 ;(mrflow-syntax-exception "Syntax exception: ~a")
 ;(mrflow-unknown-exception-title "Unknown Exception")
 ;(mrflow-unknown-exception "Unknown exception: ~a")
 ;(mrflow-language-primitives-error-title "Language Primitives Error")
 ;(mrflow-language-primitives-error "Wrong filename for language primitives types table: ~a")
  
 (snips-and-arrows-popup-menu-tack-all-arrows "Alle Pfeile befestigen")
 (snips-and-arrows-popup-menu-untack-all-arrows "Alle Pfeile lösen")
 (snips-and-arrows-user-action-disallowed-title "Änderungen durch den Benutzer momentan nicht möglich")
 (snips-and-arrows-user-action-disallowed "In Editoren, die von Tools erzeugte Snips enthalten, sind Änderungen durch den Benutzer nicht möglich. Blenden Sie alle Snips aus, bevor Sie den Inhalt des Editors ändern.")
 ;(snips-and-arrows-changing-terms-warning-title "Changing terms will be undoable")
 ;(snips-and-arrows-changing-terms-warning "Changing terms in an editor containing snips cannot be undone.  You can either cancel this action, remove the snips, and try the change again, or you can continue with the change, in which case the change will not be undoable (all others changes made before and afterward will still be undoable though).")
 (snips-and-arrows-hide-all-snips-in-editor "Alle Snips im Editor ausblenden")

 (xml-tool-menu "XML")
 (xml-tool-insert-xml-box "XML-Kasten einfügen")
 (xml-tool-insert-scheme-box "Scheme-Kasten einfügen")
 (xml-tool-insert-scheme-splice-box "Scheme-Spleiß-Kasten einfügen")
 (xml-tool-xml-box "XML-Kasten")
 (xml-tool-scheme-box "Scheme-Kasten")
 (xml-tool-scheme-splice-box "Scheme-Spleiß-Kasten")
 (xml-tool-switch-to-scheme "In Scheme-Kasten verwandeln")
 (xml-tool-switch-to-scheme-splice "In Scheme-Spleiß-Kasten verwandeln")
 (xml-tool-eliminate-whitespace-in-empty-tags
  "Überflüssigen Whitespace in leeren Tags entfernen")
 (xml-tool-leave-whitespace-alone
  "Whitespace unverändert lassen")
 
 (show-recent-items-window-menu-item "Kürzlich geöffnete Dateien in separatem Fenster anzeigen")
 (show-recent-items-window-label "Kürzlich geöffnete Dateien")
 (number-of-open-recent-items "Anzahl kürzlich geöffneter Dateien")
 (switch-anyway "Datei trotzdem wechseln")

 (stepper-program-has-changed "WARNUNG: Das Programm wurde geändert.")
 (stepper-program-window-closed "WARNUNG: Das Programm-Fenster ist nicht mehr da.")

 (stepper-home "Anfang")
 (stepper-name "Stepper")
 (stepper-language-level-message
  "Die aktuelle Sprachebene ist \"~a\". Der Stepper funktioniert bisher nur für die Ebenen\"~a\" bis \"~a\".")
 (stepper-button-label "Stepper")
 (stepper-previous-application "|< Applikation")
 (stepper-previous "< Schritt")
 (stepper-next "Schritt >")
 (stepper-next-application "Applikation >|")
 
 (wizard-next "Weiter")
 (wizard-back "Zurück")
 (wizard-finish "Fertigstellen")

 ;; warnings about closing a drscheme frame when the program
 ;; might still be doing something interesting
 (program-is-still-running "Das Programm im Definitionsfenster läuft noch.  Trotzdem schließen?")
  (program-has-open-windows "Das Programm im Definitionsfenster hat noch offene Fenster.  Trotzdem dieses Fenster schließen?")
 
  ;; ml-command-line-arguments is for the command line arguments
  ;; label in the module language details in the language dialog.
  (ml-command-line-arguments "Kommandozeilen-Argumente als Vektoren von Zeichenketten, in Read-Syntax")

  ;; ml-cp names are all for the module language collection path
  ;; configuration. See the details portion of the language dialog
  ;; for the module language (at the bottom).
  (ml-cp-default-collection-path "<<Standard-Pfade für Kollektionen>>")

  ;; in std get-directory 
  (ml-cp-choose-a-collection-path "Bitte Pfad für Kollektion auswählen")

  ;; err msg when adding default twice
  (ml-cp-default-already-present
   "Standard-Pfade für Kollektionen schon vorhanden")
  
  ;; title of this section of the dialog (possibly the word
  ;; `Collection' should not be translated)
  (ml-cp-collection-paths "Pfade für Kollektionen")

  ;; button labels
  (ml-cp-add "Hinzufügen")
  (ml-cp-add-default "Standard hinzufügen")
  (ml-cp-remove "Entfernen")
  (ml-cp-raise "Höher")
  (ml-cp-lower "Tiefer")

  ;; Profj
  (profj-java "Java")
  (profj-java-mode "Java-Modus")
  (profj-java-mode-color-keyword "Schlüsselwort")
  (profj-java-mode-color-string "Zeichenkette")
  (profj-java-mode-color-literal "Literal")
  (profj-java-mode-color-comment "Kommentar")
  (profj-java-mode-color-error "Fehler")
  (profj-java-mode-color-identifier "Bezeichner")
  (profj-java-mode-color-default "sonstiges")
  
  (profj-insert-java-comment-box "Java-Kommentarkasten einfügen")
  (profj-insert-java-interactions-box "Java-Interaktions-Kasten einfügen")

  ;; The Test Suite Tool
  ;; Errors
  (test-case-empty-error "Leerer Testfall")
  (test-case-too-many-expressions-error "Zu viele Ausdrücke in einem Testfall")
  (test-case-not-at-top-level "Testfall-Kasten nicht ganz am Top-Level")
  ;; Dr. Scheme window menu items
  (test-case-insert "Testfall einfügen")
  (test-case-disable-all "Alle Testfälle deaktivieren")
  (test-case-enable-all "Alle Testfälle aktivieren")
  ;; NOTE: The following three string constants are labels of the test-case fields. The width
  ;;       of the field is determined by the length of the longest of the following three words.
  ;;       if the words are too long the test case will take up too much horizontal room and
  ;;       not look very good.
  ;; This string is the label of the expression that is being tested in a test case.
  (test-case-to-test "Test")
  ;; This string is the label of the expression that is the expected value of the to-test expression.
  (test-case-expected "Sollte sein")
  ;; This string is the label of the actual result of the to test expression.
  (test-case-actual "Tatsächlich")
  (test-case-predicate "Prädikate")
  (test-case-should-raise "Sollte verursachen")
  ;; The label of a field of the test-case that describes the expected error message of a test case
  (test-case-error-message "Fehlermeldung")

  (test-case-menu-title "Testfall")
  (test-case-switch-to-error-box "Zu Fehler-Testbox machen")
  (test-case-switch-to-nonerror-box "Zu Nicht-Fehler-Testbox machen")
  (test-case-collapse "Testfall einfalten")
  (test-case-show-actual "Tatsächlichen Wert zeigen")
  (test-case-enable "Testfall aktivieren")
  (test-case-show-predicate "Prädikat anzeigen")
  (test-case-show-error-message "Fehlermeldung anzeigen")
  (test-case-convert-to-text "In Text umwandeln")

  ;; Profj Boxes
  (profjBoxes-empty-error "Leere Interaktion")
  (profjBoxes-too-many-expressions-error "Zu viele Ausdrücke in einem Kasten")
  (profjBoxes-interactions-label "Interaktionen")
  (profjBoxes-bad-java-id-error "Nicht-wohlgeformte Java-ID")
  (profjBoxes-examples-label "Beispiele")
  (profjBoxes-add-new-example-button "Neues Beispiel hinzufügen")
  (profjBoxes-type "Typ")
  ;; The Java identifier of an example of data
  (profjBoxes-name "Name")
  (profjBoxes-value "Wert")
  (profjBoxes-insert-java-examples "Java-Beispiele einfügen")
  (profjBoxes-insert-java-interactions "Java-Interactionen einfügen")

  ;; Slideshow
  (slideshow-show-slideshow-panel "Slideshow-Panel zeigen")
  (slideshow-hide-slideshow-panel "Slideshow Panel")
  (slideshow-freeze-picts "Diese Picts einfrieren")
  (slideshow-thaw-picts "Picts unter der Maus zeigen")
  (slideshow-hide-picts "Geschachtelte Kästen anzeigen")
  (slideshow-show-picts "Picts anzeigen")
  (slideshow-cannot-show-picts "Kann die Picts nicht anzeigen; Sie müssen erst das Programm zum Cachen der Größen laufen lassen")
  (slideshow-insert-pict-box "Pict-Kasten einfügen") 

  ;; GUI Tool
  (gui-tool-heading "GUI-Werkzeug")
  (gui-tool-before-clicking-message "Befor Sie auf ein Tool-Icon klicken, benutzen Sie \"GUI einfügen\" vom \"Spezial\"-Menü, um ein Wurzel-GUI-Element einzufügen, oder selektieren Sie eine schon vorher eingefügte GUI.")
  (gui-tool-show-gui-toolbar "GUI-Toolbar einblenden")
  (gui-tool-hide-gui-toolbar "GUI-Toolbar ausblenden")
  (gui-tool-insert-gui "GUI einfügen")
  )
