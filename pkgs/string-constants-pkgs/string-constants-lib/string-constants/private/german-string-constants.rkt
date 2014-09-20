(module german-string-constants "string-constant-lang.rkt"

 (is-this-your-native-language
  "Ist Deutsch Ihre Muttersprache?")

 (are-you-sure-you-want-to-switch-languages
  "Dies wird die Sprache der DrRacket-Benutzeroberfläche ändern und erfordert einen Neustart von DrRacket.  Sind Sie sicher?")

 (interact-with-drscheme-in-language "Deutsche Benutzeroberfläche für DrRacket")

 (accept-and-quit "In Ordnung - Beenden")
 (accept-and-exit "In Ordnung - Beenden")
 ;;; general purpose (DrRacket is hereby a word in every language, by decree of Robby :)
 (plt "PLT")
 (drscheme "DrRacket")
 (drracket "DrRacket")
 (ok "OK")
 ;; We can't use "Abbrechen" here because that's much closer in
 ;; meaning to "abort", and it appears in dialogs saying "Quit?" "OK"
 ;; "Cancel."
 (cancel "Abbrechen")
 (abort "Abbrechen")
 (untitled "Namenlos")
 (untitled-n "Namenlos ~a")
 (warning "Warnung")
 (error "Fehler")
 (close "Schließen") ;; as in, close an open window or tab. must match close-menu-item
                  ;; in the sense that, when the &s have been stripped from
                  ;; close-menu-item, it must be the same string as this.
 (close-window "Fenster schließen")
 (stop "Stop")
 (&stop "&Stop") ;; for use in button and menu item labels, with short cut.
 (are-you-sure-delete? "Sind Sie sicher, dass Sie ~a löschen wollen?") ;; ~a is a filename or directory name
 (are-you-sure-replace? "Sind Sie sicher, dass Sie ~a ersetzen wollen?") ;; ~a is a filename or directory name
 (ignore "Ignorieren")
 (revert "Änderungen rückgängig machen")

 (dont-ask-again-always-current "Nicht wieder nachfragen (immer so wie jetzt)")
 (dont-ask-again                "Nicht wieder nachfragen")

 (web-materials "Verwandte Web-Seiten")
 (tool-web-sites "Web-Seiten mit Tools")
 (plt-homepage "Racket")
 (pbd-homepage "Program by Design")

 ;;; bug report form
 (cancel-bug-report? "Bug-Report verwerfen?")
 (are-you-sure-cancel-bug-report?
  "Sind Sie sicher, dass Sie diesen Bug-Report verwerfen wollen?")
 (do-you-want-to-discard-or-save-this-bug-report
  "Wollen Sie den Bug-Report verwerfen oder speichern?")
 (discard "Verwerfen") ;; a button label for a dialog box with the above question
 (bug-report-form "Formular für Bug-Report")
 (bug-report-field-name "Name")
 (bug-report-field-email "Email")
 (bug-report-field-summary "Zusammenfassung")
 (bug-report-field-severity "Wie schlimm?")
 (bug-report-field-class "Art")
 (bug-report-field-description "Beschreibung")
 (bug-report-field-reproduce1 "Schritte, um das Problem zu")
 (bug-report-field-reproduce2 "reproduzieren")
 (bug-report-field-environment "Umgebung")
 (bug-report-field-docs-installed "Installierte Dokumentation")
 (bug-report-field-collections "Collections")
 (bug-report-field-links "Links")  ;; from 'raco link'
 (bug-report-field-human-language "Interaktionssprache") ;
 (bug-report-field-memory-use "Speicherverbrauch")
 (bug-report-field-version "Version")
 (bug-report-synthesized-information "Generierte Information")  ;; dialog title
 (bug-report-show-synthesized-info "Generierte Informationen anzeigen") ; (an)zeigen
 (bug-report-submit "Abschicken")
 (close-and-save-bug-report "Schließen && Speichern") ;; button in bug report dialog, next to cancel and bug-report-submit
 (bug-report-submit-menu-item "Bug-Report abschicken...") ;; in Help Menu (drs & help desk)
 (saved-bug-reports-menu-item "Gepeicherte Bug-Reports") ;; in Help Menu, submenu title
 (disacard-all-saved-bug-reports "Alle gespeicherten Bug-Reports verwerfen") ;; menu item: only shows up when there is more than one saved bug report
 (no-saved-bug-reports "Kein Bug-Report wurde gespeichert") ;; an info message that shows up as a disabled menu item when no saved bug reports are around
 (new-bug-report "Neuer Bug-Report") ;; button label the user sees when there are saved bug reports, but the user asks to save another one.
 (close-and-save "Schließen und Speichern") ;; button on the bottom of the bug report form
 (saved-unsubmitted-bug-reports "Gespeicherte, noch nicht abgeschickte Bug-Reports:") ;; next to previous line in same dialog, followed by list of bug report subjects (as buttons)
 (error-sending-bug-report "Versendung des Bug-Reports fehlgeschlagen")
 (error-sending-bug-report-expln "Ein Fehler ist beim Versenden des Bug-Reports aufgetreten. Falls Ihre Internet-Verbindung eigentlich funktioniert, besuchen Sie bitte:\n\n    http://bugs.racket-lang.org/ \n\nund teilen Sie uns den Bug mit unserem Online-Formular mit. Wir bitten um Ihr Verständnis.\n\nDie Fehlermeldung lautet:\n~a")
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
 (cs-foreground-color "Vordergrundfarbe")
 (cs-background-color "Hintergrundfarbe")
 (cs-tack/untack-arrow "Pfeil befestigen/lösen")
 (cs-jump-to-next-bound-occurrence "Zum nächsten gebundenen Vorkommen springen")
 (cs-jump-to-previous-bound-occurrence "Zum vorigen gebundenen Vorkommen springen")
 (cs-jump-to-binding "Zu bindendem Vorkommen springen")
 (cs-jump-to-definition "Zu Definition (in anderer Datei) springen")
 (cs-open-defining-file "Datei mit Definition öffnen")
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
 (cs-status-loading-docs-index "Syntaxprüfung: Dokumentations-Index laden")
 (cs-mouse-over-import "Bindung ~s importiert aus ~s")

 (cs-view-docs "Dokumentation für ~a anschauen")
 (cs-view-docs-from "~a aus ~a")  ;; a completed version of the line above (cs-view-docs) is put into the first ~a and a list of modules (separated by commas) is put into the second ~a. Use check syntax and right-click on a documented variable (eg, 'require') to see this in use
  
 (cs-lexical-variable "lexikalische Variable")
 (cs-set!d-variable "geset!zte Variable")
 (cs-imported-variable "importierte Variable")
 (cs-unused-require "unbenutztes require")
 (cs-free-variable "freie Variable")

 (cs-binder-count "~a bindende Vorkommen")
 (cs-zero-varrefs "keine gebundenen Vorkommen")
 (cs-one-varref "Ein gebundenes Vorkommen")
 (cs-n-varrefs "~a gebundene Vorkommen") ;; expected to have one ~a formatter that will accept a number

 (cs-contract-my-obligation "Vertrag: Obliation dieses Moduls")
 (cs-contract-their-obligation "Vertrag: Obligation des Klientenmoduls")
 (cs-contract-both-obligation "Vertrag: Obligation sowohl dieses Moduls als auch des Klientenmoduls")
 (cs-contract-unk-obligation "Vertrag: Oblikation unbekannt")

 ;; mode sub-menu in the "view" menu
 (cs-check-syntax-mode "Syntax-Check-Modus")
 (cs-mode-menu-show-my-obligations "Meine Vertragsobligationen")
 (cs-mode-menu-show-client-obligations "Vertragsobligationen des Klienten")
 (cs-mode-menu-show-syntax "Syntaktische Kategorien")

  ;; the documentation blue boxes in the upper-right corner of the drracket window
  (sc-read-more... "Mehr hier nachlesen ...")
  (sc-f2-to-un/lock "f2 um zu (ent)blockieren")

 ;; the online check syntax status messages (mouse over the bottom right of drracket's window to see the messages during online expansion's various phases)
 (online-expansion-running "Hintergrund-Expansion läuft")
 (online-expansion-only-raw-text-files-supported "Nur reine Text-Dateien sind unterstützt")
 (online-expansion-abnormal-termination "Hintergrund-Expansion unglücklich abgebrochen")
 (online-expansion-abnormal-termination-out-of-memory "Hintergrund-Expansion unglücklich abgebrochend (kein Speicher mehr)")
 (online-expansion-finished-successfully "Hintergrund-Expansion erfolgreich abgeschlossen")

 (jump-to-error "Zum Fehler springen")
 (online-expansion-is-disabled "Hintergrund-Expansion ist deaktiviert")
 ;; these next two show up in the bar along the bottom of the drracket window
 (online-expansion-pending "Hintergrund-Expansion läuft ...")
 (online-expansion-finished "Hintergrund-Expansion fertig") ;; note: there may still be errors in this case
 
 ; the next two show up in a menu when you click on the circle in the bottom right corner
 (disable-online-expansion "Hintergrund-Expansion deaktivieren")
 (enable-online-expansion "Hintergrund-Expansion aktivieren")
 ;; the online expansion preferences pane
 (online-expansion "Hintergrund-Expansion") ;; title of prefs pane
 ; the different kinds of errors
 (online-expansion-show-read-errors-as "Reader-Fehler anzeigen")
 (online-expansion-show-variable-errors-as "Ungebundene Bezeichner anzeigen")
 (online-expansion-show-other-errors-as "Andere Fehler anzeigen")
 ; locations the errors can be shown
 (online-expansion-error-gold-highlight "mit goldener Markierung")
 (online-expansion-error-margin "am Rand")
 ; the label of a preference in the (string-constant online-expansion) section
 (show-arrows-on-mouseover "Bindungen und Tail-Positionen unter Mauszeiger anzeigen")
 (show-blueboxes "Blaue Kästen  und blaue anzeigen")
 ;;; info bar at botttom of drscheme frame
 (collect-button-label "GC")
 (read-only "Nur Lesen")
 (auto-extend-selection "Automatisch erweitern")
 (overwrite "Überschreiben")
 (running "Programm läuft")
 (not-running "Programm inaktiv")
 
 (install-package-button "~a installieren") ;; button label: ~a is filled with the name of a pkg

 (update-catalog "Katalog aktualisieren") ;; button label; shown when there is a missing module, but no matching package
 (updating-catalog-from "Von ~a aktualisieren...") ;; message label; used as a status message when updating the pkg catalog

 ;;; misc
 (welcome-to-something "Willkommen bei ~a")
 
 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "Willkommen bei DrRacket! (Version ~a, ~a)")

 ; these appear on subsequent lines in the `Help|Welcome to DrRacket' dialog.
 (welcome-to-drscheme "Willkommen bei DrRacket")

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

 (editor-changed-since-srcloc-recorded
  "Dieser Editor wurde geändert seit die Quelltext-Stellen zugeordnet wurden: die markierte Region entspricht möglicherweise nicht mehr der korrekten Stelle im Quelltext.")

 (file-is-not-saved "Die Datei \"~a\" ist nicht gespeichert.")
 (save "Speichern")
 (close-anyway "Trotzdem schließen")
 (dont-save "Nicht speichern")
 (clear-anyway "Trotzdem löschen")

 (log-definitions-and-interactions "Definitionen and Interaktionen protokollieren...")
 (stop-logging "Protokoll stoppen")
 (please-choose-a-log-directory "Bitte wählen Sie ein Verzeichnis für das Protokoll")
 (logging-to "Protokoll: ")
 (erase-log-directory-contents "Inhalt von Protokoll-Verzeichnisses ~a löschen?")
 (error-erasing-log-directory "Fehler beim Löschen des Protokoll-Verzeichnisses.\n\n~a\n")

  ;; menu items connected to the logger -- also in a button in the planet status line in the drs frame
  (show-log "&Log einblenden")
  (hide-log "&Log ausblenden")
  (logger-scroll-on-output "Bei Ausgabe scrollen") ; a checkbox in the logger pane
  (log-messages "Log-Nachrichten") ;; label for the drracket logging gui panel

 ;; modes
 (mode-submenu-label "Modi")
 (scheme-mode "Scheme-Modus")
 (racket-mode "Racket-Modus")
 (text-mode "Text-Modus")

 (scheme-mode-color-symbol "Symbol")
 (scheme-mode-color-keyword "Schlüsselwort")
 (scheme-mode-color-comment "Kommentar")
 (scheme-mode-color-string "Zeichenkette")
 (scheme-mode-color-text "Text")
 (scheme-mode-color-constant "Literal")
 (scheme-mode-color-parenthesis "Klammer")
 (scheme-mode-color-hash-colon-keyword "#:Keyword")
 (scheme-mode-color-error "Fehler")
 (scheme-mode-color-other "Sonstiges")
 (syntax-coloring-choose-color "Wählen Sie eine Farbe für ~a")
 (preferences-colors "Farben")

 ;; parenthesis color scheme string constants
 (parenthesis-color-scheme "Farbschema für Klammern") ;; label for the choice% menu in the preferences dialog
 (paren-color-basic-grey "Grau Standard")
 (paren-color-shades-of-gray "Grauschattierungen")
 (paren-color-shades-of-blue "Blauschattierungen")
 (paren-color-spring "Frühling")
 (paren-color-fall "Herbst")
 (paren-color-winter "Winter")

 (url: "URL:")
 (open-url... "URL öffnen...")
 (open-url "URL öffnen")
 (browse... "Browsen...")
 (bad-url "Ungültige URL")
 (bad-url:this "Ungültige URL: ~a")
 
 ;; Help Desk
 (help "Hilfe")
 (racket-documentation "Dokumentation für Racket")
 (help-desk "Hilfezentrum")
 (plt:hd:search "Suchen")
 (plt:hd:feeling-lucky "Auf gut Glück")
 (plt:hd:home "Hilfezentrum-Homepage") 
 ; next 3 are popup menu choices in help desk search frame
 (plt:hd:search-for-keyword "Stichworteintrag")
 (plt:hd:search-for-keyword-or-index "Stichwort- oder Index-Eintrag")
 (plt:hd:search-for-keyword-or-index-or-text "Stichwort- oder Index-Eintrag, oder Text")
 (plt:hd:exact-match "Exakte Treffer")
 (plt:hd:containing-match "Teilwort")
 (plt:hd:regexp-match "über regulären Ausdruck")
 (plt:hd:find-docs-for "Finde Dokumentation zu:")
 (plt:hd:search-stopped-too-many-matches "[Suche abgebrochen: zu viele Treffer]")
 (plt:hd:nothing-found-for "Nichts zu ~a gefunden")
 (plt:hd:and "und")
 (plt:hd:refresh "aktualisieren")
 (plt:hd:refresh-all-manuals "alle Handbücher aktualisieren")
 (plt:hd:manual-installed-date "(installiert ~a)")
 ; Help Desk configuration
 ;; refreshing manuals
;; should not mention `SVN' (plt:hd:refresh-done "Aktualisierung der Handbücher aus SVN abgeschlossen")
 (plt:hd:refreshing-manuals "Handbücher aktualisieren")
 (plt:hd:refresh-downloading... "~a herunterladen...")
 (plt:hd:refresh-deleting... "Alte Version von ~a löschen...")
 (plt:hd:refresh-installing... "Neue Version von ~a installieren...")
 (plt:hd:refresh-clearing-indices "Gecachte Indizes löschen")
 (plt:hd:refreshing-manuals-finished "Fertig.")
 (plt:hd:about-help-desk "Über das Hilfezentrum")
 (plt:hd:help-desk-about-string
  "Das Hilfezentrum ist die primäre Quelle für Information über die PLT-Software,insbesondere DrRacket, MzScheme und MrEd.\n\nVersion ~a\nCopyright (c) ~a-~a PLT")
 (plt:hd:help-on-help "Hilfe zur Hilfe")
 (plt:hd:help-on-help-details "Hilfe zum Hilfezentrum befindet sich auf der Homepage des Hilfezentrums unter `Help Desk'.   (Die Homepage des Hilfezentrums ist über den `Home'-Knopf zu erreichen.)")
  (reload "Aktualisieren") ;; refresh the page in a web browser
  (plt:hd:ask-about-separate-browser
   "Sie haben einen Link selektiert, der ins Web zeigt. Wollen Sie die Seite im Hilfe-Browser oder im externen Browser anzeigen?")
  (plt:hd:homebrew-browser "Hilfe-Browser") ;; choice for the above string (in a button)
  (plt:hd:separate-browser "Externer Browser") ;; other choice for the above string (also in a button)
  (plt:hd:external-link-in-help "Externe URLs im Hilfe-Browser")
  (plt:hd:use-homebrew-browser "Den Hilfe-Browser für externe URLs benutzen")
  (plt:hd:new-help-desk "Neues Hilfezentrum")

  ;; in the Help Desk language dialog, title on the right.
  (plt:hd:manual-search-ordering "Suchreihenfolge Handbuch")

  (use-drscheme-font-size "DrRacket-Schriftgröße verwenden")

  (help-desk-this-is-just-example-text
   "Dies ist nur ein Beispieltext für das Setzen der Schriftgröße.  Öffnen sie das Hilfezentrum (im \"Hilfe\"-Menü), um diesen Links zu folgen.")

  ;; this appears in the bottom part of the frame the first time the user hits `f1' 
  ;; (assuming nothing else has loaded the documentation index first)
  ;; see also: cs-status-loading-docs-index
  (help-desk-loading-documentation-index "Hilfezentrum: Dokumentations-Index wird geladen")

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
 (browser-cmdline-expl-line-1 "(Kommandozeile konstruiert durch Aneinanderhängen von Vor-Text, URL,")
 (browser-cmdline-expl-line-2 " und Nach-Text, ohne zusätzliche Leerzeichen dazwischen.")
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

 (install-plt-file-menu-item... ".plt-Datei installieren...")
 (install-plt-file-dialog-title ".plt-Datei installieren")
 (install-plt-web-tab "Web")
 (install-plt-file-tab "Datei")
 (install-plt-filename "Dateiname:")
 (install-plt-url "URL:")
 ; an error message from a primitive operation is appended to the end of this message.
 (install-plt-error-downloading "Beim Herunterladen der"
                                " .plt-Datei ist ein Fehler aufgetreten.\n\nDetails:\n")
 (install-plt-error-header "Beim Überprüfen der heruntergeladenen .plt-Datei ist ein Fehler aufgetreten. Bitte überprüfen Sie die URL und versuchen Sie es noch einmal.")
  
 (install-plt-file "~a installieren oder editieren?")
 (install-plt-file/yes "Installieren")
 (install-plt-file/no "Editieren")
 
 (plt-installer-progress-window-title "Fortschritt Installation")
 (plt-installer-abort-installation "Installation abbrechen")
 (plt-installer-aborted "Abgebrochen.")
  
 ;;; about box
 (about-drscheme-frame-title "Über DrRacket")
 
 ;;; save file in particular format prompting.
 (save-as-plain-text "Diese Datei als Text speichern?")
 (save-in-drs-format "Diese Datei im DrRacket-Format (kein Text) speichern?")
 (yes "Ja")
 (no "Nein")
 
 ;; saving image (right click on an image to see the text)
 (save-image "Bild abspeichern...")

 ;;; preferences
 (preferences "Einstellungen")
 (error-saving-preferences "Fehler beim Speichern der Einstellungen für ~a")
 (error-saving-preferences-title "Fehler beim Speichern der Einstellungen")
 (steal-the-lock-and-retry "Lock an uns reißen && nochmal versuchen") ;; in the preferences error dialog; this happens when the lockfile exists (after 3 pref writes). 
 (error-reading-preferences "Fehler beim Lesen der Einstellungen")
 (error-reading-preferences-explanation "Die Datei mit den Einstellungen ist gesperrt und deshalb kann die  ~a-Einstellung nicht gelesen werden") ;; ~a is filled with the name of the preference (a symbol)
 (dont-ask-again-until-drracket-restarted "Nicht noch einmal fragen (bis DrRacket neu gestartet wird)")
 ; difference between the above and below is one comes with a question (steal the lock or not) and the other with just a notation saying "the file is locked"
 (dont-notify-again-until-drracket-restarted "Nicht noch einmal benachrichtigen (bis DrRacket neu gestartet wird)") 
 (prefs-file-locked "Die Datei mit den Einstellungen ist gesperrt (weil die Datei ~a existiert), weshalb die Änderungen an den Einstellungen nicht gespeichert werden konnten. Änderung an den Einstellungen rückgängig machen?")
 (try-again "Nochmal versuchen") ;; button label

 (give-up-and-use-the-default "Aufgeben und Standardeinstellung verwenden") ;; button label
  
 (prefs-file-still-locked "Die Datei mit den Einstellungen ist immer noch gesperrt (weil die Datei ~a existiert), weshalb die Änderungen an den Einstellungen nicht gespeichert werden konnten.")
 (prefs-file-locked-nothing-doing "Die Datei mit den Einstellungen ist gesperrt (durch ~s), so dass Änderungen an den Einstellungen nicht gespeichert werden können.") ;; the  ~s is filled with the lockfile; this string is (currently) used only on windows where lockfiles are less friendly (and there is no steal fallback)
 (scheme-prefs-panel-label "Racket")
 (warnings-prefs-panel-label "Warnmeldungen")
 (editor-prefs-panel-label "Editieren")
 (general-prefs-panel-label "Allgemein")
 (highlight-parens "Geklammerten Text hervorheben")
 (fixup-open-brackets "Öffnende eckige Klammern automatisch anpassen")
 (fixup-close-parens "Schließende Klammern automatisch anpassen")
 (flash-paren-match "Passende Klammer anblinken")
 (auto-save-files "Dateien automatisch abspeichern")
 (backup-files "Backup-Dateien")
 (map-delete-to-backspace "Entf löscht rückwärts")
 (verify-exit "Bei Verlassen nachfragen")
 (ask-before-changing-format "Vor Formatänderung beim Speichern nachfragen")
 (wrap-words-in-editor-buffers "Worte in Editor-Puffern umbrechen")
 (show-status-line "Status-Zeile anzeigen")
 (count-columns-from-one "Spaltennummern fangen mit 1 an")
 (display-line-numbers "Zeilennummern in Puffern anzeigen, keine Puffer-Indizes")
 (show-line-and-column-numbers "Zeilen- && Spaltennummern anzeigen") ; used for popup menu; right click on line/column box in bottom of drs window
 (show-character-offsets "Zeichen-Offsets anzeigen") ; used for popup menu; right click on line/column box in bottom of drs window
 (enable-keybindings-in-menus "Tastenbelegung für Menüs")
 (printing-mode "Druckmodus")
 (print-using-platform-specific-mode "Plattformspezifisches Drucken")
 (print-to-ps "Drucken in PostScript-Datei")
 (print-to-pdf "Drucken in PDF-Datei")

 (command-as-meta "Command-Taste als Meta behandeln") ;; macos/macos x only
 (alt-as-meta "Alt-Taste als Meta behandeln")
 (reuse-existing-frames "Existierende Fenster für neu geöffnete Dateien wiederverwenden")
 (default-fonts "Standard-Fonts")
 (basic-gray-paren-match-color "Farbe für Klammern-Hervorhebung \"Grau Standard\"") ; in prefs dialog

 (online-coloring-active "Syntax interaktiv einfärben")
 (open-files-in-tabs "Dateien in separaten Tabs öffnen (nicht separaten Fenstern)")
 (show-interactions-on-execute "Interaktionen beim Programmstart automatisch öffnen")
 (switch-to-module-language-automatically "Automatisch in die `module'-Sprache wechseln, wenn ein Modul geöffnet wird")
 (interactions-beside-definitions "Interaktionen neben den Definitionen anzeigen") ;; in preferences, below the checkbox one line above this one
 (show-line-numbers "Zeilennummern einblenden")
 (show-line-numbers/menu "Zeilen&nummern einblenden")
 (hide-line-numbers/menu "Zeilen&nummern ausblenden")

 (show-line-numbers-in-definitions "Alle Zeilennummern in Definitionen einblenden") ;; shows up in the popup menu item in the bottom of the drracket window; controls the line numbers on each line in the definitions; used in a checkable menu item
 (maximum-char-width-guide-pref-check-box "Richtschnur für maximale Zeichenbreite")
 (hide-column-width-guide "Richtschnur für Spaltenbreite für Dateien mit ~a Spalten einblenden")
 (show-column-width-guide "Richtschnur für Spaltenbreite für Dateien mit ~a Spalten ausblenden") ;; filled with a number > 2
 (limit-interactions-size "Umfang der Interaktionen einschränken")
 (background-color "Hintergrund")
 (default-text-color "Standard für Text") ;; used for configuring colors, but doesn't need the word "color"
 (choose-a-background-color "Hintergrundfarbe auswählen")

 (revert-to-defaults "Standardeinstellung wiederherstellen")
 (undo-changes "Änderungen rückgängig machen und schließen") ;; used in the preferences dialog to undo preference changes

 (color-schemes "Farbschemata") ;; the label in the preferences dialog for the color scheme panel
 (classic-color-scheme "Klassisch") ;; formerly called 'black on white'
 (modern-color-scheme "Modern")   ;; an attempt to be more color-blind friendly
 (white-on-black-color-scheme "Weiß auf Schwarz") ;; clicking the buttons changes the color schemes to some defaults that've been set up.
 ; drracket additions to the color scheme dialog; two buttons
 (design-your-own-color-schemes "Farbschemata selbst machen") ; pointer to (english-only) docs
 (style-and-color-names "Stil && Farbnamen")

 (add-spacing-between-lines "Ein Pixel Extra-Platz zwischen den Zeilen")

 ; title of the color choosing dialog

 ; should have entire alphabet
 (font-example-string "Zwölf Boxkämpfer jagen Victor quer über den großen Sylter Deich.") 

 (change-font-button-label "Ändern")
 (fonts "Schriften")
 (other... "Andere...") ;; used in the font choice menu item

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
 
 ; font size menu items in the 'view' menu; the ~a is filled with a number (font size)
 (increase-font-size "Schrift vergrößern (auf ~a)")
 (decrease-font-size "Schrift verkleinern (auf ~a)")

 ; warning message when lockfile is around
 (waiting-for-pref-lock "Auf Lock-Datei für Einstellungen warten...")
 (pref-lock-not-gone
  "Die Lock-Datei für die Einstellungen:\n\n   ~a\n\nverhindert, dass die Einstellungen abgespeichert werden können. Bitte stellen Sie sicher, dass keine andere Racket-Software läuft und löschen Sie dann diese Datei.")
 (still-locked-exit-anyway? "Die Einstellungen wurden nicht korrekt gespeichert.  Trotzdem beenden?")
 
 ;;; indenting preferences panel
 (indenting-prefs-panel-label "Einrücken")
 (indenting-prefs-extra-regexp "Zusätzlicher Regexp")

 (square-bracket-prefs-panel-label "eckige Klammern")

 ; filled with define, lambda, or begin
 (enter-new-keyword "Bitte ein Schlüsselwort wie ~a eingeben:")
 (x-keyword "~a-Schlüsselwort")
 (x-like-keywords "Schlüsselwort wie ~a")

 ; used in Square bracket panel
 (skip-subexpressions "Anzahl zu überspringender Unterausdrücke")

 (expected-a-symbol "Symbol erwartet, stattdessen bekommen: ~a")
 (already-used-keyword "\"~a\" ist bereits ein Schlüsselwort mit Spezial-Einrückung")
 (add-keyword "Hinzufügen")
 (remove-keyword "Entfernen")

 ;; repl color preferences
 (repl-colors "REPL")
 (repl-out-color "Ausgabe")
 (repl-value-color "Werte")
 (repl-error-color "Fehler")
 
  ;;; find/replace
 (search-next "Weiter")
 (search-previous "Zurück")
 (search-match "Fundort")  ;;; this one and the next one are singular/plural variants of each other
 (search-matches "Fundorte") 
 (search-replace "Ersetzen")
 (search-skip "Überspringen")
 (search-show-replace "Ersetzen einblenden")
 (search-hide-replace "Ersetzen ausblenden")
 (find-case-sensitive "Groß-/Kleinschreibung beachten")  ;; the check box in both the docked & undocked search
 (find-anchor-based "Suchen mit Ankern")
 
 ;; these string constants used to be used by searching,
 ;; but aren't anymore. They are still used by other tools, tho.
 (hide "Ausblenden")
 (dock "Andocken")
 (undock "Ablegen")
 
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
 (mfs-drscheme-multi-file-search "Suchen in mehreren Dateien - DrRacket") ;; results window and error message title
 (mfs-not-a-dir "\"~a\" ist kein Verzeichnis")
 (mfs-open-file "Datei öffnen")
 (mfs-stop-search "Suche stoppen")
 (mfs-case-sensitive-label "Groß-/Kleinschreibung beachten")
 (mfs-no-matches-found "Keine Treffer gefunden.")
 (mfs-search-interrupted "Suche abgebrochen.")
 (mfs-drscheme-multi-file-search-title "Suchen in mehreren Dateien nach \"~a\" - DrRacket") ;; the ~a format specifier is filled in with the search string
 
 ;;; reverting a file
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
 ;;; if they don't, DrRacket's menus will appear
 ;;; in the wrong order.
 (file-menu "Datei")
 (edit-menu "Bearbeiten")
 (help-menu "Hilfe")
 (windows-menu "Fenster")

 (tabs-menu "Tabs")
 
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

 (open-recent-info "Liste kürzlich bearbeiteter Dateien")
 (open-recent-menu-item "Noch einmal öffnen")
 
 (revert-info "Stelle diese Datei wieder her wie zuletzt gespeichert")
 (revert-menu-item "&Wiederherstellen")

 (save-info "Diese Datei auf der Platte speichern")
 (save-menu-item "&Speichern")

 (save-as-info "Dateinamen abfragen und dann Datei abspeichern")
 (save-as-menu-item "Speichern &unter...")

 (page-setup-info "Ausdruck-Einstellungen ändern")
 (page-setup-menu-item "Ausdruck-Einstellungen...")
 
 (print-info "Diese Datei zum Drucker schicken")
 (print-menu-item "&Drucken...")

 (close-info "Diese Datei schließen")
 (close-menu-item "&Schließen")
 (close-window-menu-item "Fenster &schließen")

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
 (clear-menu-item-windows "&Löschen")

 (select-all-info "Gesamtes Dokument selektieren")
 (select-all-menu-item "&Alles selektieren")
 
 (find-info "Zum nächsten Vorkommen der Zeichenkette aus dem Such-Fenster springen")
 (find-menu-item "Suchen")
 (find-from-selection-menu-item "Die Selektion suchen")

 (find-next-info "Zum nächsten Fundort der Zeichenkette im Suchfenster springen")
 (find-next-menu-item "Weitersuchen")

 (find-previous-info "Zum vorherigen Vorkommen der Zeichenkette aus dem Such-Fenster springen")
 (find-previous-menu-item "Rückwärts weitersuchen")

 (show-replace-menu-item "Ersetzen einblenden")
 (hide-replace-menu-item "Ersetzen ausblenden")
 (show/hide-replace-info "Wechselt die Sichtbarkeit des Ersetzen-Panels")
 
 (replace-menu-item "Ersetzen")
 (replace-info " Suchtext im dunklen Kreis ersetzen")

 (replace-all-info "Alle Vorkommen der Such-Zeichenkette ersetzen")
 (replace-all-menu-item "Alle ersetzen")
 
 (find-case-sensitive-info "Schaltet zwischen Groß-/Kleinschreibung berücksichtigendem und nicht berücksichtigendem Suchen um")
 (find-case-sensitive-menu-item "Suchen mit Groß-/Kleinschreibung")

 (complete-word "Wort vervollständigen") ; the complete word menu item in the edit menu
 (no-completions "... keine Vervollständigungen verfügbar") ; shows up in the completions menu when there are no completions (in italics)
  
 (overwrite-mode "Überschreib-Modus")
 (enable-overwrite-mode-keybindings "Tastenbelegungen für Überschreib-Modus aktivieren")

 (enable-automatic-parens "Automatische Klammerung einschalten") ; should "and square brackets and quotes" appear here?

 (preferences-info "Die Einstellungen konfigurieren")
 (preferences-menu-item "Einstellungen...")

 (keybindings-info "Aktuelle Tastaturbelegung anzeigen")
 (keybindings-menu-item "Tastaturbelegung")
 (keybindings-show-active "Aktive Tastenbelegungen anzeigen")
 (keybindings-frame-title "Tastaturbelegung")
 (keybindings-sort-by-name "Nach Name sortieren")
 (keybindings-sort-by-key "Nach Taste sortieren")
 (keybindings-add-user-defined-keybindings "Benutzerdefinierte Tastenbelegungen hinzufügen...")
 (keybindings-add-user-defined-keybindings/planet "Benutzerdefinierte Tastenbelegungen aus PLaneT hinzufügen...")
 (keybindings-menu-remove "~a entfernen")
 (keybindings-choose-user-defined-file "Bitte eine Datei mit den Tastenbelegungen auswählen.")

 (user-defined-keybinding-error "Fehler beim Ausführen der Tastenbelegung ~a\n\n~a")
 (user-defined-keybinding-malformed-file "Die Datei ~a enthält kein Modul, das in der Sprache framework/keybinding-lang geschrieben ist.")  
 (user-defined-keybinding-malformed-file/found-lang "Die Datei ~a enthält kein Modul, das in der Sprache framework/keybinding-lang geschrieben ist.  Stattdessen wurde Sprache ~s vorgefunden.")  
 (keybindings-planet-malformed-spec "Die PLaneT-Spezifikation ist fehlerhaft: ~a") ; the string will be what the user typed in
 (keybindings-type-planet-spec "Bitte PLaneT-require-Spezifikation eingeben (ohne das `require')")
  
 ; first ~a will be a string naming the file or planet package where the keybindings come from;
 ; second ~a will be an error message
 (keybindings-error-installing-file "Fehler beim Installieren der Tastenbelegungen ~a:\n\n~a")
  
 ;; menu items in the "special" menu
 (insert-text-box-item "Text-Kasten einfügen")
 (insert-image-item "Bild einfügen...")
 (insert-comment-box-menu-item-label "Kommentarkasten einfügen")
 (insert-lambda "&Lambda einfügen")

 (wrap-text-item "Text umbrechen")

 ;; windows menu
 (windows-menu-label "&Fenster")
 (tabs-menu-label "&Tabs")
 (minimize "Minimieren") ;; minimize and zoom are only used under mac os x
 (zoom "Zoomen")
 (bring-frame-to-front "Fenster nach vorn")       ;;; title of dialog
 (bring-frame-to-front... "Fenster nach vorn...") ;;; corresponding title of menu item
 (most-recent-window "Letztes Fenster")
 (next-tab "Nächster Tab")
 (prev-tab "Vorheriger Tab")
 (move-current-tab-right "Tab nach &rechts bewegen")
 (move-current-tab-left "Tab nach &links bewegen")
 (tab-i "Tab ~a: ~a") ;; menu item in the windows menu under mac os x. first ~a is filled with a number between 1 and 9; second one is the filename of the tab
 (tab-i/no-name "Tab ~a")
  
 (view-menu-label "&Anzeigen")
 (show-overview "&Programm-Umriss einblenden") 
 (hide-overview "&Programm-Umriss ausblenden")
 (show-module-browser "&Modul-Browser einblenden")
 (hide-module-browser "&Modul-Browser ausblenden")

 (help-menu-label "&Hilfe")
 (about-info "Mehr über dieses Programm und seine Entstehung")
 (about-menu-item "Über...")
 
 ;; open here's new menu item
 (create-new-window-or-clear-current
  "Würden Sie gern ein neues Fenster aufmachen oder dieses hier löschen und wiederverwenden?")
 (clear-current "Dieses löschen")
 (new-window "Neues Fenster")

 ;; popup menu when right-clicking in the gap between
 ;; the definitions and interactions window
 (change-to-vertical-alignment "Auf vertikal umschalten")
 (change-to-horizontal-alignment "Auf horizontal umschalten")

 ;;; exiting and quitting ``are you sure'' dialog
 ;;; exit is used on windows, quit on macos, in English. Other
 ;;; languages probably use the same word on both platforms.
 (exit "Beenden")
 (quit "Beenden")
 (are-you-sure-exit "Sind Sie sicher, dass Sie das Programm beenden wollen?")
 (are-you-sure-quit "Sind Sie sicher, dass Sie das Programm beenden wollen?")
 (dont-exit "Widerrufen")
 (dont-quit "Widerrufen")
 
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
  ;; files to appear (typically 5 minutes). Kill DrRacket
  ;; and restart it. You'll see the dialog
  (autosave-autosave-label: "Automatisch gespeicherte Datei:")
  (autosave-original-label: "Ursprüngliche Datei:")
  (autosave-autosave-label "Automatisch gespeicherte Datei")
  (autosave-original-label "Ursprüngliche Datei")
  (autosave-compare-files "Automatisch gespeicherte Dateien vergleichen")

  (autosave-show-autosave "Automatisch gespeicherte Datei") ;; title of a window showing the autosave file

  (autosave-explanation "DrRacket hat automatisch gespeicherte Dateien gefunden, die nicht regulär gespeicherten Inhalt enthalten könnten.")

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
 (drscheme-internal-error "Interner Fehler in DrRacket")
 
 ;;; tools
 (invalid-tool-spec "Die Tool-Spezifikation in der Datei info.rkt der Collection ~a enthält Fehler. Da sollte eine Zeichenkette oder eine Liste von Zeichenketten stehen, tatsächlich steht dort aber: ~e")
 (error-invoking-tool-title "Fehler beim Starten von Tool ~s;~s")
 (error-loading-tool-title "Fehler beim Laden von Tool ~s\n~a") ;; ~s filled with a path, ~a filled with an error message from an exn
 (tool-tool-names-same-length
  "`tool-names' und `tools' in info.rkt für ~s müssen Listen der gleichen Länge sein, tatsächlich stehen dort ~e und ~e")
 (tool-tool-icons-same-length
  "`tool-icons' und `tools' in info.rkt für ~s müssen Listen der gleichen Länge sein, tatsächlich stehen dort ~e und ~e")
 (tool-tool-urls-same-length
  "`tool-urls' und `tools' in info.rkt für ~s müssen Listen der gleichen Länge sein, tatsächlich stehen dort ~e und ~e")
 (error-getting-info-tool
  "Fehler beim Laden von info.rkt file für ~s")
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
 (use-horizontal-layout "Horizontal anordnen")
 (use-vertical-layout "Vertikal anordnen")
 (interactions-menu-item-help-string "Interaktionsfenster ein-/ausblenden")
 (toolbar "Toolbar")
 (toolbar-on-top "Toolbar oben")
 (toolbar-on-top-no-label "Toolbar oben mit kleinen Knöpfen")
 (toolbar-on-left "Toolbar links")
 (toolbar-on-right "Toolbar rechts")
 (toolbar-hidden "Toolbar ausblenden")

 ;;; file menu
 (save-definitions-as "Definitionen speichern unter...")
 (save-definitions "Definitionen speichern")
 (print-definitions "Definition drucken...")
 (about-drscheme "Über DrRacket")
 (save-other "Speichern unter")
 (save-definitions-as-text "Definitionen als Text speichern...")
 (save-interactions "Interaktionen speichern")
 (save-interactions-as "Interaktionen speichern unter...")
 (save-interactions-as-text "Interaktionen als Text speichern...")
 (print-interactions "Interaktionen drucken...")
 (new-tab "Neuer Tab")
 (close-tab "Tab schließen")

 (close-tab-amp "Tab &schließen") ;; like close-tab, but with an ampersand on the same letter as the one in close-menu-item
 
 ;;; edit menu
 (split-menu-item-label "&Splitten")
 (collapse-menu-item-label "Einfalten")
 (find-longest-line "Längste Zeile finden")
 
 ;;; language menu
 (language-menu-name "&Sprache")
 
 ;;; scheme-menu
 (scheme-menu-name "Ra&cket")
 (execute-menu-item-label "Start")
 (execute-menu-item-help-string "Das Programm im Definitionsfenster neu starten")

 (ask-quit-menu-item-label "Programm bitten aufzuhören")
 (ask-quit-menu-item-help-string "Benutzt break-thread, um den primären Thread der Auswertung zu stoppen")
 (force-quit-menu-item-label "Programm zwingen aufzuhören")
 (force-quit-menu-item-help-string "Benutzt custodian-shutdown-all, um die Auswertung abzubrechen")
 (limit-memory-menu-item-label "Speicherverbrauch einschränken...")
 (limit-memory-msg-1 "Das Limit wird beim nächsten Programmstart aktiv")
 (limit-memory-msg-2 "und muss mindestens acht Megabyte betragen.") ;; minimum memory limit is now 8 megabytes
 (limit-memory-unlimited "nicht einschränken")
 (limit-memory-limited "einschränken")
 (limit-memory-megabytes "Megabytes")

 ; the next two constants are used together in the limit memory dialog; they are inserted
 ; one after another. The first one is shown in a bold font and the second is not.
 ; (the first can be the empty string)
 (limit-memory-warning-prefix "Warning: ")
 (limit-memory-warning "Die Einstellung für uneingeschränkten Speicherverbrauch ist unsicher. Mit dieser Einstellung kann DrRacket sich nicht gegen Programme schützen, die zuviel Speicher allozieren - DrRacket könnte abstürzen.")
 
 (clear-error-highlight-menu-item-label "Fehlermarkierung entfernen")
 (clear-error-highlight-item-help-string "Entfernt die rosa Fehlermarkierung")
 (jump-to-next-error-highlight-menu-item-label "Zur nächsten Fehlermarkierung springen")
 (jump-to-prev-error-highlight-menu-item-label "Zur vorigen Fehlermarkierung springen")
 (reindent-menu-item-label "&Einrücken")
 (reindent-all-menu-item-label "&Alles einrücken")
 (semicolon-comment-out-menu-item-label "Mit &Semikolon auskommentieren")
 (box-comment-out-menu-item-label "Mit &Kommentar-Kasten auskommentieren")
 (uncomment-menu-item-label "Einkommentieren")

 (convert-to-semicolon-comment "In Semikolon-Kommentar umwandeln")
 
 ;;; executables
 (create-executable-menu-item-label "Programmdatei generieren...")
 (create-executable-title "Programmdatei generieren")
 (drracket-creates-executables-only-in-some-languages
  "DrRacket unterstützt die Erzeugung von Programmdateien nur,"
  " wenn eine Lehrsprache (DMdA oder HtDP) im Dialog “Sprache auswählen”"
  " ausgewählt ist, oder wenn dort “Die Sprache Racket” ausgewählt ist und"
  " eine #lang-Zeile am Anfang des Programms steht.\n\nZiehen"
  " Sie das Kommandozeilenprogramm \"raco exe\" in Betracht.")
 (must-save-before-executable "Sie müssen vor der Generierung einer Programmdatei speichern.")
 (save-a-mred-launcher "GRacket-Launcher speichern")
 (save-a-mzscheme-launcher "Racket-Launcher speichern")
 (save-a-mred-stand-alone-executable "GRacket-Stand-Alone-Programmdatei speichern")
 (save-a-mzscheme-stand-alone-executable "Racket-Stand-Alone-Programmdatei speichern")
 (save-a-mred-distribution "GRacket-Distribution speichern")
 (save-a-mzscheme-distribution "Racket-Distribution speichern")

 (error-creating-executable "Fehler beim Erzeugen der Stand-Alone-Programmdatei:") ;; this is suffixed with an error message ala error-display-handler

 (definitions-not-saved "Die Definitionen sind nicht gespeichert. Die Programmdatei wird von der letzten gespeicherten Version gezogen. Weitermachen?")
 (launcher "Launcher")
 (launcher-explanatory-label "Launcher (nur für diese Maschine, läuft vom Quelltext)")
 (stand-alone "Stand-alone")
 (stand-alone-explanatory-label "Stand-alone (nur für diese Maschine, startet compilierte Kopie)")
 (distribution "Distribution")
 (distribution-explanatory-label "Distribution (für die Installation auf anderen Maschinen)")
 (executable-type "Typ")
 (executable-base "Hauptteil")
 (filename "Dateiname: ")
 (create "Erzeugen")
 (files-for-icons-etc "Dateien für für Bildchen etc.")
 (please-specify-a-filename "Bitte einen Dateinamen angeben.")
 (~a-must-end-with-~a
  "Der Dateiname auf \".~a\"\n\n  ~a\n\nist nicht zulässig. Der Dateiname muss auf \".~a\" enden.")
 (macosx-executables-must-end-with-app
  "Der Dateiname auf \".~a\"\n\n  ~a\n\nist nicht zulässig. Unter Mac OS X muss der Dateiname auf \".app\" enden.")
 (warning-directory-will-be-replaced
  "WARNUNG: Das Verzeichnis:\n\n  ~a\n\nsoll überschrieben werden. Weitermachen?")
 
 (distribution-progress-window-title "Fortschritt bei der Erstellung der Distribution")
 (creating-executable-progress-status "Ausführbares Programm für Distribution erstellen...")
 (assembling-distribution-files-progress-status "Dateien für Distribution zusammenstellen...")
 (packing-distribution-progress-status "Distribution einpacken...")

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
 (insert-number/bad-whole-part "Der ganzzahlige Anteil muss eine ganze Zahl sein")
 (insert-number/bad-numerator "Der Zähler einer Zahl muss eine nichtnegative ganze Zahl sein")
 (insert-number/bad-denominator "Der Nenner einer Zahl muss eine nichtnegative ganze Zahl sein")

 (insert-fraction-menu-item-label "Bruch einfügen...")

 ;; number snip popup menu
 (show-decimal-expansion "Als Dezimalexpansion anzeigen")
 (show-mixed-fraction-view "Als gemischten Bruch anzeigen")
 (show-improper-fraction-view "Als ungemischten Bruch anzeigenn")
 (show-more-decimal-places "Mehr Dezimalziffern anzeigen")
 
 ;;; Teachpack messages
 (select-a-teachpack "Teachpack auswählen")
 (clear-teachpack "Teachpack ~a entfernen")
 (teachpack-error-label "DrRacket - Teachpack-Fehler")
 (teachpack-didnt-load "Die Teachpack-Datei ~a konnte nicht korrekt geladen werden.")
 (add-teachpack-menu-item-label "Teachpack hinzufügen...")
 (clear-all-teachpacks-menu-item-label "Alle Teachpacks entfernen")
 (drscheme-teachpack-message-title "DrRacket-Teachpack")
 (already-added-teachpack "Teachpack ~a ist schon dabei")

 ; ~a is filled with the teachpack's name; the message appears in the teachpack selection dialog when a user installs a new teachpack
 (compiling-teachpack "Teachpack ~a compilieren...")
 
  (teachpack-pre-installed "Vorinstallierte Teachpacks")
  (teachpack-pre-installed/htdp "Vorinstallierte HtDP-Teachpacks")
  (teachpack-pre-installed/2htdp "Vorinstallierte HtDP/2e-Teachpacks")
  (teachpack-user-installed "selbst installierte Teachpacks")
  (add-teachpack-to-list... "Teachpack zu Liste hinzufügen...")
  ; first and second ~a are teachpack names, third is a symbol identifing an export
  (teachpack-conflict
   "WARNUNG: Das schon installierte Teachpack ~a ist im Konflikt mit ~a (beide haben den Export ~a)")
   ;; a button label; the two ~a are filled with teachpack names
  (remove-and-add-teachpack "~a entfernen und ~a hinzufügen")
  (teachpack-already-installed "Ein Teachpack names '~a' ist schon installiert. Überschreiben?")
  ; ~a is filled with a list of language names. Each name is separated by a newline and is indented two spaces (no commas, no 'and')
  (teachpacks-only-in-languages "Die Teachpacks sind nur in diesen Sprachen verfügbar: ~a}\n\nIn anderen Sprachen ist `require' vorgesehen.")

 ;;; Language dialog
 (introduction-to-language-dialog
  "Bitte eine Sprache auswählen. Für den Anfängerkurs ist wahrscheinlich die voreingestellte Sprache die richtige.")
 (language-dialog-title "Sprache auswählen")
 (case-sensitive-label "Groß-/Kleinschreibung unterscheiden")
 (output-style-label "Ausgabenotation")
 (constructor-printing-style "Konstruktor")
 (quasiquote-printing-style "Quasiquote")
 (write-printing-style "write")
 (print-printing-style "print")
 (sharing-printing-label "Zeige Sharing an")
 (use-pretty-printer-label "Zeilenumbrüche in Ausdruck einfügen")
 (input-syntax "Eingabesyntax")
 (dynamic-properties "Laufzeit")
 (output-syntax "Ausgabesyntax")
 (teachpacks "Teachpacks") ;; label in the language dialog for the teaching languages
 (teachpacks-none "<< keine >>") ;; shows up under the previous string, when there are no teachpacks
 (no-debugging-or-profiling "Kein Debugging oder Profiling")
 (debugging "Debugging")
 (debugging-and-profiling "Debugging und Profiling")
 (test-coverage "Syntaktische Test-Suiten-Abdeckung")
 (show-details-button-label "Details einblenden")
 (hide-details-button-label "Details ausblenden")
 (choose-language-menu-item-label "Sprache auswählen...")
 (revert-to-language-defaults "Standard-Spracheinstellungen wiederherstellen")
 (fraction-style "Bruch-Ausgabe")
 (use-mixed-fractions "gemischte Brüche")
 (use-repeating-decimals "Dezimalausgabe mit Perioden")
 (decimal-notation-for-rationals "Dezimalnotation für Brüche")
 (enforce-primitives-group-box-label "Initiale Bindungen")
 (enforce-primitives-check-box-label "Änderungen von initialen Bindungen verbieten")

 (automatically-compile "\"compiled\"-Verzeichnisse bestücken (für schnelleres laden)")
 (preserve-stacktrace-information "Stack-Trace behalten (einige Optimierungen werden abgeschaltet)")
 (enforce-module-constants-checkbox-label "Konstantendefinitionen durchsetzen (sorgt für etwas Inlining)")
 (expression-level-stacktrace "Stack-Trace mit Ausdrücken")
 (function-level-stacktrace "Stack-Trace mit Funktionen")
 (submodules-to-run "Submodule zum Ausführen")
 (add-submodule "Submodul-Option hinzufügen ...") ;; menu item
 (add-submodule-title "Submodul hinzufügen") ;; title of dialog opened by above menu item

 ;; used in the bottom left of the drscheme frame as the label
 ;; above the programming language's name
 ;; used the popup menu from the just above; greyed out and only
 ;; visible when some languages are in the history
 (recent-languages "Kürzlich verwendete Sprachen:")
 ;; shows up in bottom-left programming language menu popup, when no langs are recorded
 (no-recently-chosen-languages "keine kürzlich verwendete Sprache") 

 ;; startup wizard screen language selection section
 (please-select-a-language "Sprache auswählen")
 
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
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (pretty-big-scheme "Kombo")
 (pretty-big-scheme-one-line-summary "Macht Syntax and Prozeduren der HtDP-Sprachen verfügbar")
 (r5rs-language-name "R5RS")
 (r5rs-one-line-summary "R5RS, ohne alles andere")
 (expander "Expander")
 (expander-one-line-summary "Expandiert Ausdrücke, statt sie auszuwerten")
 (legacy-languages "Altlast-Sprachen")
 (teaching-languages "Lehrsprachen")
 (experimental-languages "Experimentelle Sprachen")
 (initial-language-category "Sprache am Anfang")
 (no-language-chosen "Keine Sprache ausgewählt")

 (other-languages "Andere Sprachen")

 (module-language-name "Sprache aus Quelltext ermitteln")
 (module-language-one-line-summary "Die #lang-Zeile spezifiziert die tatsächliche Sprache.")
  
 (module-language-auto-text "Automatisch Zeile mit #lang") ;; shows up in the details section of the module language

 ;; for the upper portion of the language dialog
 (the-racket-language "Die Sprache Racket")
 (choose-a-language "Sprache auswählen")

 ;; the next two string constants appear in the
 ;; language dialog with a list
 ;; of example languages appearing between them
 (racket-language-discussion "Am Anfang des Programms spezifiziert #lang den gewünschten Dialekt. Zum Beispiel:\n\n")
 (racket-language-discussion-end "\n... und viele weitere")

 ;; the next three string constants are put into a message-box dialog
 ;; that appears when the user clicks on the example #lang languages
 ;; in the language dialog. The first one always appears and then either
 ;; the second or the third appears. The second one has the clicked
 ;; on #lang line placed into the ~a, and third one has the 
 ;; current #lang line in the first ~a and the clicked on in the second one.
 ;; The two comments are separated by a blank line.
 (racket-dialect-in-buffer-message "Racket-Dialekte werden normalerweise im Editor ausgewählt, nicht durch Auswahl eines Eintrags im Sprachendialog.")
 (racket-dialect-add-new-#lang-line "Soll ich also “~a” am Anfang der Definitionen einfügen?")
 (racket-dialect-replace-#lang-line "Ich sehe auch ein “~a” im Code; durch “~a” ersetzen?")
 (racket-dialect-already-same-#lang-line "Es ist allerdings schon “~a” im Code; es ist also alles bereit fürs Programmieren!")

 ;; in the dialog containing the above strings, one of these is a button that appears
 (add-#lang-line "#lang-Zeile hinzufügen")
 (replace-#lang-line "#lang-Zeile ersetzen")

 ;; for the 'new drracket user' dialog
 (use-language-in-source "Sprache, die im Code spezifiziert ist, benutzen")

  ;;; from the `not a language language' used initially in drscheme.
 (must-choose-language "DrRacket kann keine Programme verarbeiten, bis Sie eine Sprache auswählen.")
 
 ;; next two appear before and after the name of a text book (which will be in italics)
 (using-a-textbook-before "Benutzen Sie ")
 (using-a-textbook-after "?")
 
 ;; next two are before and after a language
 (start-with-before "Mit ")

 (start-with-after "anfangen?")

 (seasoned-plt-schemer? "Erfahrener PLT-Schemer?")
 (racketeer? "Sind Sie ein Racketeer?")
 (looking-for-standard-scheme? "Wollen Sie Standard-Scheme?")
 
 ;; the three string constants are concatenated together and the middle
 ;; one is hyperlinked to the dialog that suggests various languages
 (get-guidance-before "Wählen Sie \"Sprache auswählen...\" im \"Sprache\"-Menü oder ")
 (get-guidance-during "Hilfe anfordern")
 (get-guidance-after ".")

 ;;; debug language
 (unknown-debug-frame "[unbekannt]")
 (backtrace-window-title "Backtrace - DrRacket")
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
 (profiling-update "Profile aktualisieren")
 (profiling-col-percent-time "% Zeit")
 (profiling-col-function "Prozedur")
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
 (test-coverage-on "Durch Tests abgedeckt")
 (test-coverage-off "Durch Tests nicht abgedeckt")
  
 ;; tracing
 (tracing-enable-tracing "Tracing einschalten")
 (tracing-show-tracing-window "Tracing einblenden")
 (tracing-hide-tracing-window "Tracing ausblenden")
 (tracing-tracing-nothing-to-show "Es liegen keine Tracing-Resultate vor. Stellen Sie sicher, dass die eingestellte Sprache Tracing unterstützt und dass Tracing eingeschaltet ist.")

 ;;; repl stuff
 (evaluation-terminated "Auswertung abgebrochen")
 (evaluation-terminated-explanation
  "Der Auswertungs-Thread läuft nicht mehr; es findet also keine Auswertung bis zum nächsten Programmlauf statt.")

  ; The next three constants show up in the same dialog as the above evaluation-terminated string
  ; constants.
  ; The first two show up only when the user calls 'exit' (possibly with a status code).
  ; The third shows up when the program runs out of memory.
  (exited-successfully "Erfolgreich beendet.")
  (exited-with-error-code "Beendet mit Fehlercode ~a.") ;; ~a is filled in with a number between 1 and 255
  (program-ran-out-of-memory "Dem Programm ist der Speicher ausgegangen.")
  
  (show-evaluation-terminated-dialog "Den Dialog ‘Auswertung abgebrochen’ zeigen")
  (evaluation-terminated-ask "Diesen Dialog das nächste Mal anzeigen")
  
  (last-stack-frame "letzten Stack-Frame anzeigen")
  (last-stack-frames "die letzten ~a Stack-Frames anzeigen")
  (next-stack-frames "die nächsten ~a Stack-Frames anzeigen")
 
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
 (version:update-menu-item "Nach Updates schauen...")
 (version:update-check "Update-Prüfung")
 (version:connecting-server  "Mit Racket-Versions-Server verbinden")
 (version:results-title      "Racket-Versions-Check")
 (version:do-periodic-checks "Regelmäßig nach neueren Racket-Versionen schauen")
 (version:take-me-there      "Dorthin gehen") ; ...to the download website
 ;; the next one can appear alone, or followed by a comma and the one after that
 (version:plt-up-to-date     "Die Racket-Version ist aktuell")
 (version:but-newer-alpha    "aber es gibt eine neuere Alpha-Version")
 ;; This is used in this context: "Racket vNNN <<<*>>> http://download..."
 (version:now-available-at   "ist jetzt verfügbar bei")

 ;; insert menu
 (insert-menu "E&infügen")
 
 ;; large semi colon letters
 (insert-large-letters... "Große Buchstaben einfügen...")
 (large-semicolon-letters "Große Buchstaben aus Semikolons")
 (text-to-insert "Einzufügender Text")

 (module-browser-filename-format "Vollständiger Dateiname: ~a (~a Zeilen)")
 (module-browser-root-filename "Basis-Dateiname: ~a")
 (module-browser-font-size-gauge-label "Schriftgröße")
 (module-browser-progress-label "Fortschritt Modul-Browser")
 (module-browser-adding-file "Datei ~a hinzufügen...")
 (module-browser-laying-out-graph-label "Graph-Layout")
 (module-browser-open-file-format "~a öffnen")
 (module-browser "Modul-Browser") ;; frame title
 (module-browser... "&Modul-Browser...") ;; menu item title
 (module-browser-in-file "M&odul-Browser auf ~a") ;; menu item title; ~a is filled with a filename
 (module-browser-no-file "Modul-Browser auf dieser gespeicherten Datei") ;; menu item title for above menu item; used when there is no saved file
 (module-browser-error-expanding "Fehler beim Expandieren des Programms:\n\n~a")
 (module-browser-show-lib-paths "Dateien anzeigen, die über (lib ..)-Pfade eingebunden wurden")
 (module-browser-progress "Modul-Browser: ~a") ;; prefix in the status line
 (module-browser-compiling-defns "Modul-Browser: Definition compilieren")
 (module-browser-show-lib-paths/short "\"lib\"-requires folgen") ;; check box label in show module browser pane in drscheme window.
 (module-browser-show-planet-paths/short "\"PLaneT\"-requires folgen") ;; check box label in show module browser pane in drscheme window.
 (module-browser-refresh "Aktualisieren") ;; button label in show module browser pane in drscheme window.
 (module-browser-highlight "Hervorheben") ;; used to search in the graph; the label on a text-field% object
 (module-browser-only-in-plt-and-module-langs
  "Der Modul-Browser ist nur für modulbasierte Programme verfügbar.")
 (module-browser-name-length "Länge der Namen")
 (module-browser-name-short "Kurz")
 (module-browser-name-medium "Mittel")
 (module-browser-name-long "Lang")
 (module-browser-name-very-long "Lang mit Phasen")  ;; like 'Long' but shows the phases where this file is loaded
 (module-browser-open-all "Alle hier angezeigten Datein öffnen")

 (happy-birthday-matthias "Happy Birthday, Matthias!")
 (happy-birthday-matthew "Happy Birthday, Matthew!")
 (happy-birthday-shriram "Happy Birthday, Shriram!")

 (mrflow-using-default-language-title "Standard-Sprache verwendet")
 (mrflow-using-default-language "Die momentan verwendete Sprache hat keine Typ-Tabelle für ihre Primitiva.  Verwende stattdessen R5RS-Scheme.")
 (mrflow-button-title "Analyse")
 ;(mrflow-unknown-style-delta-error-title "Unknown Box Style Delta")
 ;(mrflow-unknown-style-delta-error "Unknown box style delta: ~a")
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

 (xml-tool-insert-xml-box "XML-Kasten einfügen")
 (xml-tool-insert-scheme-box "Racket-Kasten einfügen")
 (xml-tool-insert-scheme-splice-box "Racket-Spleiß-Kasten einfügen")
 (xml-tool-xml-box "XML-Kasten")
 (xml-tool-scheme-box "Racket-Kasten")
 (xml-tool-scheme-splice-box "Racket-Spleiß-Kasten")
 (xml-tool-switch-to-scheme "In Racket-Kasten verwandeln")
 (xml-tool-switch-to-scheme-splice "In Racket-Spleiß-Kasten verwandeln")
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

 (stepper-name "Stepper")
 (stepper-language-level-message
  "Der Stepper unterstützt die Sprachebene \"~a\" nicht.")
 (stepper-button-label "Stepper")

 (stepper-previous "Schritt")
 (stepper-next "Schritt")
 (stepper-jump "Springen...")
 (stepper-jump-to-beginning "an den Anfang")
 (stepper-jump-to-end "ans Ende")
 (stepper-jump-to-selected "an den Anfang der Selektion")
 (stepper-jump-to-previous-application "zur vorigen Applikation")
 (stepper-jump-to-next-application "zur nächsten Applikation")
 (stepper-out-of-steps "Ende der Auswertung erreicht, bevor ein angemessener Schritt gefunden werden konnte.")
 (stepper-no-such-step/title "Kein Schritt gefunden.")
 (stepper-no-such-step "Kein Schritt gefunden, der das Kriterium erfüllt.")
 (stepper-no-such-step/earlier "Kein früherer Schritt gefunden, der das Kriterium erfüllt.")
 
 (stepper-no-earlier-application-step "Keine vorherigen Applikationsschritte.")
 (stepper-no-later-application-step "Keine weiteren Applikationsschritte..")
 (stepper-complete "Alle Definitionen wurden erfolgreich ausgewertet.")
 
 (stepper-no-earlier-step "Keine vorherigen Schritte.")
 (stepper-no-later-step "Keine weiteren Schritte.")
  
 (stepper-no-selected-step "Keine Schritte im markierten Bereich. Vielleicht ist es auskommentiert?")
  
 (stepper-no-last-step "Der letzte Schritt ist nocht nicht verfügbar.")

 (debug-tool-button-name "Debugger")

 (dialog-back "Zurück")

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
  (ml-cp-default-collection-path "<<Standard-Pfade für Collections>>")

  ;; in std get-directory 
  (ml-cp-choose-a-collection-path "Bitte Pfad für Collection auswählen")

  ;; err msg when adding default twice
  (ml-cp-default-already-present
   "Standard-Pfade für Collections schon vorhanden")
  
  ;; title of this section of the dialog (possibly the word
  ;; `Collection' should not be translated)
  (ml-cp-collection-paths "Pfade für Collections")

  ;; button labels
  ;;  The package manager uses these, too
  (ml-cp-add "Hinzufügen")
  (ml-cp-add-default "Standard hinzufügen")
  (ml-cp-remove "Entfernen")
  (ml-cp-raise "Höher")
  (ml-cp-lower "Tiefer")

  (ml-always-show-#lang-line "#lang-Zeile in der `module'-Sprache immer anzeigen")

  ;; Profj
  (profj-java "Java")
  (profj-java-mode "Java-Modus")

  (profj-java-coverage "Java-Abdeckung") ;; shows up in the preferences dialog under 'Color'

  (profj-beginner-lang "Anfänger")
  (profj-beginner-lang-one-line-summary "Java-ähnliche Lehrsprache für Anfänger")
  (profj-full-lang "Voller Sprachumfang")
  (profj-full-lang-one-line-summary "Wie Java 1.0 (einige 1.1-Erweiterungen)")
  (profj-advanced-lang "Fortgeschritten")
  (profj-advanced-lang-one-line-summary "Java-ähnliche Lehrsprache für Fortgeschrittene")
  (profj-intermediate-lang "Zwischenstufe")
  (profj-intermediate-lang-one-line-summary "Java-ähnliche Lehrsprache, Zwischenstufe")
  (profj-intermediate-access-lang "Zwischenstufe + Zugriffskontrolle")
  (profj-intermediate-access-lang-one-line-summary "Java-ähnliche Lehrsprache, Zwischenstufe, mit Zugriffskontrolle")
  (profj-dynamic-lang "Java+dynamic")
  (profj-dynamic-lang-one-summary "Java mit dynamischen Typen")

  (profj-java-mode-color-heading "Farben ändern") ; Heading for preference to choose editing colors  
  (profj-java-mode-color-keyword "Schlüsselwort")
  (profj-java-mode-color-string "Zeichenkette")
  (profj-java-mode-color-literal "Literal")
  (profj-java-mode-color-comment "Kommentar")
  (profj-java-mode-color-error "Fehler")
  (profj-java-mode-color-identifier "Bezeichner")
  (profj-java-mode-color-prim-type "primitiver Typ") ; Example text for built-in Java types
  (profj-java-mode-color-default "sonstiges")
  
  (profj-coverage-color-heading "Farben für Abdeckung") ; Heading for preference to choose coverage colors
  (profj-coverage-color-covered "abgedeckte Ausdrücke") 

  (profj-language-config-display-preferences "Einstellungen Anzeige") ; Heading for preferences controlling printing
  (profj-language-config-display-style "Art der Anzeige")
  (profj-language-config-display-field "Klasse + Felder")
  (profj-language-config-class "Klasse")
  (profj-language-config-display-array "Gesamten Inhalt von Arrays ausdrucken?")
  (profj-language-config-testing-preferences "Einstellungen Testen") ; Heading for preferences controlling test behavior
  ;(profj-language-config-testing-enable "Testresultate bei Start anzeigen?") ; Run should be the word found on the Run button
  (profj-language-config-testing-coverage "Abdeckungsinformationen für Tests sammeln?")
  (profj-language-config-support-test-language "Spracherweiterung \"test\" unterstützen?")
  (profj-language-config-testing-check "Check-Ausdruck zulassen?") ; check should not be translated
  (profj-language-config-classpath "Klassenpfad")
  (profj-language-config-choose-classpath-directory "Verzeichnis für den Klassenpfad auswählren")
  (profj-language-config-classpath-display "Aktuellen Wert anzeigen") ; Button label to print the current classpath

  (profj-test-name-close-to-example "Der Name von Klasse ~a enhält etwas, das so ähnlich wie \"Example\" aussieht.")
  (profj-test-name-example-miscapitalized "Das \"example\" im Namen der Klasse ~a sollte \"Example\" geschrieben werden.")

   ;; Close testing window and do not run test cases any more
  ;(profj-test-results-close-and-disable "Schließen und Testen deaktivieren")
  ;; Hide docked testing window and do not run test cases any more
  ;(profj-test-results-hide-and-disable "Ausblenden und Testen deaktivieren")
  ;Renamed below
  ;(profj-test-results-window-title "Testresultate")

  (profj-unsupported "Nicht unterstützt")
  (profj-executables-unsupported "Programmdateien sind für Java bisher noch nicht unterstützt")

  (profj-convert-to-text-comment "Hier Textkommentar einfügen")
  (profj-convert-to-comment "Hier Kommentar einfügen")

  (profj-executing-main "main ausführen")

  (profj-insert-java-comment-box "Java-Kommentarkasten einfügen")
  (profj-insert-java-interactions-box "Java-Interaktions-Kasten einfügen")
  
  ;;The test engine tool
  ;;
  (test-engine-window-title "Testresultate")
  ;;Following two appear in View menu, attach and free test report window from DrRacket frame
  (test-engine-dock-report "Testresultate andocken")
 (test-engine-undock-report "Testresultate abdocken")
  ;;Following two appear in Racket (Java, etc) menu, cause Tests to be Run automatically or not
  (test-engine-enable-tests "Test aktivieren")
  (test-engine-disable-tests "Tests deaktivieren")
  
  (test-engine-ran-1-test "1 Test gelaufen.")
  (test-engine-ran-1-check "1 Check gelaufen.")
  ;; ditto, only plural
  (test-engine-ran-n-tests "~a Tests gelaufen.")
  (test-engine-ran-n-checks "~a Checks gelaufen.")
  (test-engine-1-test-passed "Der eine Test ist bestanden!")
  (test-engine-1-check-passed "Der eine Check ist bestanden!")
  (test-engine-both-tests-passed "Beide Tests bestanden!")
  (test-engine-both-checks-passed "Beide Checks bestanden!")
  (test-engine-all-tests-passed "Alle Tests bestanden!")
  (test-engine-all-checks-passed "Alle Checks bestanden!")
  (test-engine-all-n-tests-passed "Alle ~a Tests bestanden!")
  (test-engine-all-n-checks-passed "Alle ~a Checks bestanden!")
  (test-engine-0-tests-passed "0 Tests bestanden.")
  (test-engine-0-checks-passed "0 Checks bestanden.")
  (test-engine-m-of-n-tests-failed "~a der ~a Tests fehlgeschlagen.")
  (test-engine-m-of-n-checks-failed "~a der ~a Checks fehlgeschlagen.")
  (test-engine-must-be-tested "Dieses Programm muss noch getestet werden!")
  (test-engine-is-unchecked "Dieses Programm hat keine Checks!")
  (test-engine-tests-disabled "Tests deaktiviert.")
  (test-engine-both-tests-passed "Beide Tests waren erfolgreich.")
  ; ~a is replaced by count
  (test-engine-all-tests-passed "Alle ~a Tests waren erfolgreich!")
  (test-engine-should-be-tested "Dieses Programm sollte getestet werden.")
  (test-engine-at-line-column "in Zeile ~a, Spalte ~a")
  (test-engine-in-at-line-column "in ~a, Zeile ~a, Spalte ~a")
  ; as in "column (unknown)"
  (test-engine-unknown "(unbekannt)")
  (test-engine-trace-error "Trace-Fehler")

  (test-engine-check-encountered-error
   "check-expect bekam den folgenden Fehler statt des erwarteten Werts, ~F. ~n   :: ~a")
  (test-engine-actual-value-differs-error
   "Der tatsächliche Wert ~F ist nicht der erwartete Wert ~F.")
  (test-engine-actual-value-not-within-error
   "Der tatsächliche Wert ~F ist nicht innerhalb von ~v des erwarteten Werts ~F.")
  (test-engine-encountered-error-error
   "check-error bekam den folgenden Fehler anstatt des erwarteten ~a~n   :: ~a")
  (test-engine-expected-error-error
   "check-error erwartete den folgenden Fehler, bekam aber den Wert ~F.~n ~a")
  (test-engine-expected-an-error-error
   "check-error erwartete einen Fehler, bekam aber den Wert ~F.")
  (test-engine-not-mem-error  "Tatsächlicher Wert ~F ist keins der Elemente ")
  (test-engine-not-range-error "Tatsächlicher Wert ~F liegt nicht zwischen ~F und ~F (inklusive).")
  (test-engine-property-fail-error "Eigenschaft falsifizierbar mit")
  (test-engine-property-error-error "`check-property' bekam den folgenden Fehler~n:: ~a")

  (signature-enable-checks "Signaturüberprüfung aktivieren")
  (signature-disable-checks "Signaturüberprüfung deaktivieren")

  ; section header
  (test-engine-check-failures "Check-Fehler:")
  ; section header
  (test-engine-signature-violations "Signaturverletzungen:")

  ; part of one phrase "signature <at line ...> to blame: procedure <at line ...>
  (test-engine-signature "Signatur")
  (test-engine-to-blame "verantwortlich: Prozedur")

  (test-engine-no-signature-violations "Keine Signaturverletzungen.")
  (test-engine-1-signature-violation "1 Signaturverletzung.")
  (test-engine-n-signature-violations "~a Signaturverletzungen.")

  ; as in got <value>, signature <at ...>
  (test-engine-got "bekam")

  (profjWizward-insert-java-class "Java-Klasse einfügen")
  (profjWizard-insert-java-union "Java-Vereinigung einfügen")

  ;; The Test Suite Tool
  ;; Errors
  (test-case-empty-error "Leerer Testfall")
  (test-case-too-many-expressions-error "Zu viele Ausdrücke in einem Testfall")
  ;; DrRacket window menu items
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

  
  ;; contract violation tracking
  
  ; tooltip for new planet icon in drscheme window (must have a planet violation logged to see it)
  (show-planet-contract-violations "PLaneT-Vertragsverletzungen anzeigen")

  ; buttons in the dialog that lists the recorded bug reports
  (bug-track-report "Ticket einreichen")
  (bug-track-forget "Vergessen")
  (bug-track-forget-all "Alles vergessen")

  ;; planet status messages in the bottom of the drscheme window; the ~a is filled with the name of the package
  (planet-downloading "PLaneT: ~a herunterladen ...")
  (planet-installing "PLaneT: ~a installieren ...")
  (planet-finished "PLaneT: fertig mit ~a.")
  (planet-docs-building "PLaneT: Dokumentation bauen (ausgelöst durch ~a)...")
  (planet-no-status "PLaneT") ;; this can happen when there is status shown in a different and then the user switches to a tab where planet hasn't been used

  (bug-report-field-pkg "Info Package-System")
  

 ;; string normalization. To see this, paste some text with a ligature into DrRacket
 ;; the first three strings are in the dialog that appears. The last one is in the preferences dialog
 (normalize "Normalisieren")
 (leave-alone "Unverändert lassen")
 (normalize-string-info "Der Text, den Sie eingefügt haben, enthält Ligaturen oder andere nicht-normalisierte Zeichen. Normalisieren?")
 (normalize-string-preference "Eingefügten Text normalisieren")
 (ask-about-normalizing-strings "Bei Normalisierung nachfragen")
 
  (always-use-platform-specific-linefeed-convention "Immer die plattformspezifische Linefeed-Konvention verwenden")


  ;; optimization coach
  (hide-optimization-coach "Optimierungs-Coach ausblenden")
  (show-optimization-coach "Optimierungs-Coach einblenden")
  

  ;; labels used (in a big font) in the background of the definitions and interactions windows
  (definitions-window-label "Definitionen")
  (interactions-window-label "Interaktionen")
  (hide-defs/ints-label "Definitionen/Interaktionen-Beschriftung ausblenden") ;; popup menu
  (show-defs/ints-label "Definitionen/Interaktionen-Beschriftung einblenden") ;; preferences checkbox

  ;; menu item in the 'edit' menu; applies to editors with programs in them
  ;; (technically, editors that implement color:text<%>)
  (spell-check-string-constants "String-Konstanten korrekturlesen")
  (spell-check-scribble-text "Text korrekturlesen (zwischen {} in Scribble)")
  (spelling-dictionaries "Wörterbücher für die Rechtschreibprüfung") ; (sub)menu whose items are the different possible dictionaries
  (default-spelling-dictionary "Standard-Wörterbuch") ; first item in menu from previous line
  (misspelled-text-color "Rechtschreibfehler in Textfarbe") ;; in the preferences dialog  
  (cannot-find-ispell-or-aspell-path "aspell- bzw. ispell-Programm nicht gefunden")
  ; puts the path to the spell program in the ~a and then the error message
  ; is put following this string (with a blank line in between)
  (spell-program-wrote-to-stderr-on-startup "Der Rechtschreibchecker (~a) hat eine Fehlermeldung ausgegeben:")

  (spell-skip-to-next-misspelled-word "Zum nächsten falsch geschriebenen Wort") ;; menu item
  (spell-suggest-corrections "Rechtschreibkorrekturen vorschlagen...") ;; menu item
  (spell-correction-suggestions "Vorschläge für Rechtschreibkorrekturen") ;; dialog title
  (spell-choose-replacement-word "Austauschwort auswählen") ;; label in dialog 
  
  ;; GUI for installing a pkg package; available via File|Install Package...
  (install-pkg-install-by-source "Tu was ich meine") ; tab label
  (install-pkg-install-from-list "Im Katalog verfügbar") ; tab label
  (install-pkg-install-installed "Momentan installiert")         ; tab label
  (install-pkg-migrate-from "Kopieren von Version")           ; tab label
  (install-pkg-settings "Einstellungen")                        ; tab label
  (install-pkg-menu-item... "Paket installieren...")
  (install-pkg-dialog-title "Paket installieren")
  (install-pkg-source-label "Paket-Quelle")
  (install-pkg-package-name "Paket-Name")
  (install-pkg-package-source-desc "Eine Paket-Quelle ist ein Paket-Name, eine Datei, ein Verzeichnis, eine URL oder ein Verweis auf Github")
  (install-pkg-infer "Inferieren")
  (install-pkg-use "Benutzen") ; as opposed to "Infer", label for text box
  (install-pkg-type-label "Typ Paket-Quelle")
  (install-pkg-file "Datei")
  (install-pkg-dir "Verzeichnis")
  (install-pkg-dir-url "Verzeichnis woanders")
  (install-pkg-file-url "Datei woanders")
  (install-pkg-github "Github")
  (install-pkg-name "Name (frage Auflöser)")
  (install-pkg-inferred-as "Typ inferiert als ~a") ; ~a gets install-pkg-{file,dir,...}
  (install-pkg-link-dirs "Lokales Verzeichnis als Link")
  (install-pkg-file-or-dir? "Datei oder Verzeichnis auswählen")
  (install-pkg-force? "Konflikte ignorieren")
  (install-pkg-replace? "Existierende Installation überschreiben")
  (install-pkg-command-line "Äquivalenter Kommandozeilen-Aufruf:")
  (install-pkg-error-installing-title "Fehler beim Installieren von Paket")

  (install-pkg-action-label "Maßnahme")
  (install-pkg-install "Installieren")
  (install-pkg-update "Aktualisieren")
  (install-pkg-remove "Entfernen")
  (install-pkg-do-not-remove "Nicht entfernen")
  (install-pkg-action-inferred-to-be-update "Maßnahme als Aktualisierung inferiert")
  (install-pkg-action-inferred-to-be-install "Maßnahme als Installation inferiert")
  (install-pkg-default "Standard")
  (install-pkg-scope-label "Paket-Einzugsbereich")
  (install-pkg-default-scope-label "Standard-Paket-Einzugsbereich") ; for picking the scope to be default
  (install-pkg-installation "Bestimmte Racket-Installation")
  (install-pkg-user "Bestimmter Benutzer und Racket-Version")
  (install-pkg-set-as-default "Als Standard setzen")
  (install-pkg-scope-is "Paket-Einzugsbereich ist ~a") ; ~a gets install-pkg-{installation,user,shared}
  (install-pkg-select-package-directory "Paket-Verzeichnis auswählen")
  (install-pkg-select-package-file "Paket-Datei auswählen")
  (install-pkg-update-package-list "Paket-Liste aktualisieren")
  (install-pkg-stop-update "Aktualisierung anhalten")
  (install-pkg-filter "Filter")
  (install-pkg-update-catalogs? "Datenbank aktualisieren um mit dem konfigurierten Satz Kataloge übereinzustimmen?")
  (install-pkg-currently-configured-are "Die aktuell konfigurierten Kataloge sind")
  (install-pkg-database-recorded-are "Die Kataloge in der Datenbank sind")
  (install-pkg-update-catalogs "Aktualisieren")
  (install-pkg-do-not-update-catalogs "Nicht aktualisieren")
  (install-pkg-really-remove? "Sind Sie sicher, dass Sie die folgenden selektierten Pakete entfernen wollen?")
  (install-pkg-promote "Von \"automatisch installiert\" befördern")
  (install-pkg-demote "Nach \"automatisch installiert\" zurückstufen")
  (install-pkg-abort-install "Installation abbrechen")
  (install-pkg-abort-update "Aktualisierung abbrechen")
  (install-pkg-abort-remove "Entfernung abbrechen")
  (install-pkg-abort-demote "Zurückstufung abbrechen")
  (install-pkg-abort-promote "Beförderung abbrechen")
  (install-pkg-abort-migrate "Migration abbrechen")
  (install-pkg-abort-generic-action "Aktion abbrechen")
  (install-pkg-close-terminal-output "Anzeige schließen")
  (install-pkg-show-all-options "Alle Optionen anzeigen")
  (install-pkg-migrate-available-installations "Verfügbare Installationen")
  (pkg-manager-menu-item "Paket-Manager...")
  ;; where ~a gets an installation name:
  (install-pkg-packages-for "Pakete für ~a")
  (install-pkg-really-remove-installation "Sind Sie sicher, dass Sie alle installierten Pakete und Informationen für ~a löschen wollen?")

  (install-pkg-abort-set-scope "Änderung des Einzugsbereich widerrufen")

  (install-pkg-dependencies-fail "Fehlschlag: Installation/Aktualisierung widerrufen, falls Abhänigkeiten fehlen")
  (install-pkg-dependencies-force "Trotzdem: Installieren trotz fehlender Abhängigkeiten")
  (install-pkg-dependencies-search-ask "Fragen: bei jeder fehlenden Abhänigkeit fragen (nicht unterstützt in GUI)")
  (install-pkg-dependencies-search-auto "Auto: fehlende Abhänigkeiten automatisch installieren")
  (install-pkg-dependencies-search-auto+update "Auto + Aktualisieren: Abhänigkeiten aktualisieren wann immer möglich")

  (install-pkg-dependencies-mode "Modus Abhängigkeiten")

  (install-pkg-dependencies-search-ask-not-supported-in-gui
   "Der “Fragen“-Modus für Abhängigkeiten ist im GUI-Installierer nicht unterstützt.")
  ;; "~a" is pre-":" part of `install-pkg-dependencies-fail' or `install-pkg-dependencies-search-auto':
  (install-pkg-deps-is "Standard-Modus für Abhängigkeiten ist ~a")

  (install-pkg-package-catalogs "Paket-Kataloge") ; label for a list box
  (install-pkg-add-package-catalog "Paket-Katalog hinzufügen")

  (install-pkg-not-rentrant "Installation und Aktualisierung können nicht gleichzeitig laufen."
                            " Brechen Sie entweder den laufenden Prozess ab oder warten, bis er fertig ist.")

  ;; open a file via a collection path (new "Open" menu item in DrRacket)
  (open-require-path "Require-Pfad öffnen...")
  (enter-subcollection "Sub-Collection betreten") ; button in new dialog
  
  (path-to-racket-binary "Pfad zur Programmdatei")
  (use-a-different-racket "Anderes Racket benutzen")
  
  ; first ~a is filled with either the empty string or an error message from elsewhere
  ;  (bracketed by some newlines to separate it out)
  ; second ~a is filled with /etc/paths.d/racket (or some other path like it in the future)
  ; third ~a is filled with the path to the bin directory of the current drracket
  (adding-racket/bin-to-path-failed 
   "Versuch fehlgeschlagen, Racket-Unterstützung zur Kommandozeile hinzufügen.~aGenauer:"
   " konnte \"~a\" mit Inhalt \"~a\" nicht erzeugen.")
  ; first and third ~a are filled with /etc/paths.d/racket (or some other path like it in the future)
  ; and the second one is filled with the path to the bin directory that was put into that file.
  (added-racket/bin-to-path
   "Sie sollten nun Racket mit all seinen Programme von der"
   " Kommandozeile benutzen können.\n\nDer"
   " Standardpfad in PATH wurde für alle Benutzer konfiguriert, indem die Datei"
   " ~a hinzugefügt wurde und auf ~a zeigt. Sie können dies rückgängig machen, indem"
   " Sie ~a löschen.")
  (add-racket/bin-to-path "Kommandozeile für Racket konfigurieren...") ;; menu item label

  )

