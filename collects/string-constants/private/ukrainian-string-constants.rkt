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

(module ukrainian-string-constants "string-constant-lang.rkt"
 ;;; when translating this constant, substitute name of actual language for `English'
 (is-this-your-native-language "Українська - це Ваша рідна мова?")

 (are-you-sure-you-want-to-switch-languages
  "Для зміни мови інтерфейсу користувача необхідно перезапустити DrRacket. Ви впевнені, що дійсно хочете цього?")

 (interact-with-drscheme-in-language "Працювати з українським інтерфейсом DrRacket")

 ;; these two should probably be the same in all languages excepet English.
 ;; they are the button labels (under macos and windows, respectively)
 ;; that go the with the string above.
 (accept-and-quit "Застосувати й вийти")
 (accept-and-exit "Застосувати й вийти")
 
 ;;; general purpose (DrRacket is hereby a word in every language, by decree of Robby :)
 (plt "PLT")
 (drscheme "DrRacket")
 (ok "OK")
 (cancel "Скасувати")
 (abort "Скасувати")
 (untitled "Без імені")
 (untitled-n "Без імені ~a")
 (warning "Попередження")
 (error "Помилка")
 (close "Закрити") ;; as in, close an open window. must match close-menu-item
                 ;; in the sense that, when the &s have been stripped from
                 ;; close-menu-item, it must be the same string as this.
 (stop "Зупинити")   
 (&stop "&Зупинити") ;; for use in button and menu item labels, with short cut.
 (are-you-sure-delete? "Ви дійсно хочете видалити ~a?") ;; ~a is a filename or directory name
 (ignore "Ігнорувати")
 (revert "Завантажити заново")

 ;; label for a generic check box, often supported on dialogs
 ;; that ask a binary choice of the user. If checked, the
 ;; dialog isn't going to be shown again.
 ;; One version for always using the current choice:
 (dont-ask-again-always-current "Більше не питати (завжди використовувати поточний вибір)")
 ;; One generic version (ie, on the Quit DrRacket dialog)
 (dont-ask-again                "Більше не питати")

 ;;; important urls
 (web-materials "Пов'язані Web-сайти") ;; menu item title
 (tool-web-sites "Web-сайти встановлених інструментів")   ;; menu item title
 (plt-homepage "Racket")
 (pbd-homepage "Program by Design")

 ;;; bug report form
 (cancel-bug-report? "Скасувати відправлення звіту про помилки?")
 (are-you-sure-cancel-bug-report?
  "Ви дійсно хочете скасувати відправлення звіту про помилки?")
 (bug-report-form "Звіт про помилки")
 (bug-report-field-name "Ім'я")
 (bug-report-field-email "E-mail")
 (bug-report-field-summary "Резюме")
 (bug-report-field-severity "Серйозність")
 (bug-report-field-class "Клас")
 (bug-report-field-description "Опис")
 (bug-report-field-reproduce1 "Послідовність дій")
 (bug-report-field-reproduce2 "для відтворення помилки")
 (bug-report-field-environment "Середовище")
 (bug-report-field-docs-installed "Встановлена документація")
 (bug-report-field-collections "Колекція")
 (bug-report-field-human-language "Мова інтерфейсу")
  (bug-report-field-memory-use "Використовувана пам'ять")
 (bug-report-field-version "Версія")
 (bug-report-synthesized-information "Зібрані дані")  ;; dialog title
 (bug-report-show-synthesized-info "Показати зібрані дані")
 (bug-report-submit "Відправити")
 (bug-report-submit-menu-item "Відправити звіт про помилку...") ;; in Help Menu (drs & help desk)
 (error-sending-bug-report "Збій при відправці звіту про помилку")
 (error-sending-bug-report-expln "При відправці звіту про помилку відбувся збій. При наявності підключення до Internet відвідайте сайт:\n\n    http://bugs.racket-lang.org/\n\nі відправте звіт про помилку через Web-форму на ньому. Вибачте за незручності.\n\nПовідомлення про помилку:\n~a")
 (illegal-bug-report "Некоректний звіт про помилку")
 (pls-fill-in-field "Будь-ласка, заповніть поле \"~a\"")
 (malformed-email-address "Некоректна адреса електронної пошти")
 (pls-fill-in-either-description-or-reproduce "Будь-ласка, заповніть поле \"Опис\" або \"Послідовність дій для відтворення помилки\".")

 ;;; check syntax
 (check-syntax "Перевірити синтаксис")
 (cs-italic "Курсив")
 (cs-bold "Напівжирний")
 (cs-underline "Підкреслений")
 (cs-change-color "Змінити колір")
 (cs-tack/untack-arrow "З'єднувати/не з'єднувати стрілками")
 (cs-jump-to-next-bound-occurrence "Перейти до наступного входження")
 (cs-jump-to-binding "Перейти до визначення")
 (cs-jump-to-definition "Перейти до оголошення")
 (cs-error-message "Повідомлення про помилку")
 (cs-open-file "Відкрити ~a")
 (cs-rename-var "Перейменувати ~a")
 (cs-rename-id "Перейменувати ідентифікатор")
 (cs-rename-var-to "Перейменувати ~a в:")
 (cs-name-duplication-error "Обране ім'я ~s конфліктує з визначеним раніше.")
 (cs-rename-anyway "Перейменувати беззастережно")
 (cs-status-init "Перевірка синтаксису: ініціалізація оточення для коду користувача")
 (cs-status-coloring-program "Перевірка синтаксису: виокремлення виразів кольором")
 (cs-status-eval-compile-time "Перевірка синтаксису: обчислення часу компіляції")
 (cs-status-expanding-expression "Перевірка синтаксису: розгорнути вираз")
 (cs-status-loading-docs-index "Перевірка синтаксису: завантаження індексу документації")
 (cs-mouse-over-import "Прив'язка ~s імпортована з ~s")
 (cs-view-docs "Перегляд документації для ~a")
 (cs-view-docs-from "~a з ~a")  ;; a completed version of the line above (cs-view-docs) is put into the first ~a and a list of modules (separated by commas) is put into the second ~a. Use check syntax and right-click on a documented variable (eg, 'require') to see this in use  
  
 (cs-lexical-variable "лексична змінна")
 (cs-set!d-variable "перевизначена змінна")
 (cs-imported-variable "імпортована змінна")

 ;;; info bar at botttom of drscheme frame
 (collect-button-label "Збирання сміття")
  (read-only "Тільки для читання")
 (auto-extend-selection "Авторозширення")
 (overwrite "Заміна")
 (running "виконується")
 (not-running "не виконується")
 
 ;;; misc
 (welcome-to-something "Ласкаво просимо до ~a")
 
 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "Ласкаво просимо до DrRacket, версія ~a, ~a")

 ; these appear on subsequent lines in the `Help|Ласкаво просимо до DrRacket' dialog.
 (welcome-to-drscheme "Ласкаво просимо до DrRacket")

 (goto-line "Перейти до рядка")
 (goto-line-invalid-number
  "~a - некоректний номер рядка. Номер повинен бути цілим числом в діапазоні від 1 до ~a")
 (goto-position "Перейти до позиції")
 (no-full-name-since-not-saved
  "У файлу немає повного імені - він ще не був збережений.")
 (cannot-open-because-dne "Неможливо відкрити ~a, тому що він не існує.")

  (needs-execute-language-changed
   "Попередження: мова змінилась. Натисніть Виконати.")
  (needs-execute-teachpack-changed
   "Попередження: навчальний пакет змінився. Натисніть Виконати.")
  (needs-execute-defns-edited
   "Попередження: вікно визначень змінилось. Натисніть Виконати.") (file-is-not-saved "Файл \"~a\" не сохранен.")

  (editor-changed-since-srcloc-recorded
   "З моменту останньої фіксації місцезнаходження файлу було внесено зміни, тому виділена область може містити неправильне місцезнаходження файлу.")
 
 (file-is-not-saved "Файл \"~a\" не збережено.")
 (save "Зберегти")
 (close-anyway "Все одно закрити")
 (dont-save "Не зберігати")
 (clear-anyway "Все одно очистити")

 ;; menu item title
 (log-definitions-and-interactions "Зберегти вікна визначень та інтерпретатора...")
 (stop-logging "Зупинити протоколювання")
 (please-choose-a-log-directory "Будь-ласка, оберіть каталог")
 (logging-to "Протоколювати в: ")
 (erase-log-directory-contents "Видалити зміст каталогу: ~a?")
 (error-erasing-log-directory "Помилка видалення змісту каталогу.\n\n~a\n")

  ;; menu items connected to the logger -- also in a button in the planet status line in the drs frame
  (show-log "Показати &протокол")
  (hide-log "Сховати &протокол")
  (logging-all "Все") ;; in the logging window in drscheme, shows all logs simultaneously

 ;; modes
 (mode-submenu-label "Режими")
 (scheme-mode "Режим Racket")
 (text-mode "Режим тексту")

 (scheme-mode-color-symbol "Символ")
 (scheme-mode-color-keyword "Ключове слово")
 (scheme-mode-color-comment "Коментар")
 (scheme-mode-color-string "Рядок")
 (scheme-mode-color-constant "Константа")
 (scheme-mode-color-parenthesis "Круглі дужки")
 (scheme-mode-color-error "Помилка")
 (scheme-mode-color-other "Інші")
 ;; the ~a is filled in with one of the above (scheme-mode-*)
 (syntax-coloring-choose-color "Оберіть колір для ~a")
 (preferences-colors "Кольори") ;; used in the preferences dialog
 
  ;; parenthesis color scheme string constants
  (parenthesis-color-scheme "Виокремлення кольором круглих дужок") ;; label for the choice% menu in the preferences dialog
  (paren-color-basic-grey "Основний сірий")
  (paren-color-shades-of-gray "Відтінки сірого кольору")
  (paren-color-shades-of-blue "Відтінки блакитного кольору")
  (paren-color-spring "Теплі відтінки")
  (paren-color-fall "Осінні відтінки")
  (paren-color-winter "Холодні відтінки")

  
 (url: "URL:")
 (open-url... "Відкрити URL...")
 (open-url "Відкрити URL")
 (browse... "Браузер...")
 (bad-url "Невірний URL")
 (bad-url:this "Невірний URL: ~a")
 
 ;; Help Desk
 (help "Довідка")
 (help-desk "Допомога")
 (plt:hd:search "Пошук")
 (plt:hd:feeling-lucky "Мені пощастить")
 (plt:hd:home "Домашня сторінка допомоги") 
 ; next 3 are popup menu choices in help desk search frame
 (plt:hd:search-for-keyword "Шукати в ключових словах")
 (plt:hd:search-for-keyword-or-index "Шукати в ключових словах або в змісті")
 (plt:hd:search-for-keyword-or-index-or-text "Шукати в ключових словах, в змісті або в тексті")
 (plt:hd:exact-match "Точна відповідність")
 (plt:hd:containing-match "Міститься згадка")
 (plt:hd:regexp-match "Відповідність по регулярному виразу")
 (plt:hd:find-docs-for "Пошук документації з:")
 (plt:hd:search-stopped-too-many-matches "[Пошук перервано: занадто багато співпадань]")
 (plt:hd:nothing-found-for "Для ~a не знайдено")
 (plt:hd:and "і")
 (plt:hd:refresh "оновити")
 (plt:hd:refresh-all-manuals "оновити всі керівництва")
 (plt:hd:manual-installed-date "(інсталювати ~a)")
 ; Help Desk configuration
 ;; refreshing manuals
 (plt:hd:refreshing-manuals "Перезавантажити керівництва")
 (plt:hd:refresh-downloading... "Завантажити ~a...")
 (plt:hd:refresh-deleting... "Видалити стару версію ~a...")
 (plt:hd:refresh-installing... "Інсталювати нову версію ~a...")
 (plt:hd:refresh-clearing-indices "Очистити кешовані індекси")
 (plt:hd:refreshing-manuals-finished "Готово.")
 (plt:hd:about-help-desk "Про допомогу")
 (plt:hd:help-desk-about-string
  "Допомога - це повне джерело інформації про програмне забезпечення PLT, включаючи DrRacket, MzScheme та MrEd.\n\nВерсія ~a\nCopyright (c) ~a-~a PLT")
 (plt:hd:help-on-help "Довідка про довідку")
 (plt:hd:help-on-help-details "Для отримання довідки з використання допомоги, оберіть перше посилання `Допомога' на її домашній сторінці. (Для переходу на домашню сторінку натисніть кнопку `Додому' в верхній частині вікна допомоги.)")
  (reload "Оновити") ;; refresh the page in a web browser
  (plt:hd:ask-about-separate-browser
   "Ви обрали посилання на дані з WWW. Бажаєте переглянути їх в браузері допомоги чи в вікні зовнішнього браузера?")
  (plt:hd:homebrew-browser "Браузер допомоги") ;; choice for the above string (in a button)
  (plt:hd:separate-browser "Зовнішній браузер") ;; other choice for the above string (also in a button)
  (plt:hd:external-link-in-help "Зовнішні посилання в довідці")
  (plt:hd:use-homebrew-browser "Використовувати браузер допомоги для зовнішніх посилань")
  (plt:hd:new-help-desk "Нове вікно допомоги")

  ;; in the Help Desk language dialog, title on the right.
  (plt:hd:manual-search-ordering "Керівництво з пошуку")
  
  ;; in the help-desk standalone font preference dialog, on a check box
  (use-drscheme-font-size "Використовувати розмір шрифту DrRacket")
  
  ;; in the preferences dialog in drscheme there is example text for help desk font size.
  ;; clicking the links in that text produces a dialog with this message
  (help-desk-this-is-just-example-text
   "Це приклад для встановлення розміру шрифта. Відкрийте відповідну допомогу (з меню Довідка) для переходу за цими посиланнямим.")

  ;; this appears in the bottom part of the frame the first time the user hits `f1' 
  ;; (assuming nothing else has loaded the documentation index first)
  ;; see also: cs-status-loading-docs-index
  (help-desk-loading-documentation-index "Допомога: завантаження індексу документації")
  
 ;; Help desk htty proxy
 (http-proxy "HTTP-проксі")
 (proxy-direct-connection "Пряме підключення")
 (proxy-use-proxy "Використовувати проксі:")
 (proxy-host "Вузол")
 (proxy-port "Порт")
 (proxy-bad-host "Невірний проксі-вузол")

 ;; browser
 (rewind-in-browser-history "Назад")
 (forward-in-browser-history "Вперед")
 (home "На початок")
 (browser "Браузер")
 (external-browser-choice-title "Зовнішній браузер") ; title for radio-button set
 (browser-command-line-label "Командний рядок:") ; label for radio button that is followed by text boxes
 (choose-browser "Обрати браузер")
 (no-browser "Обрати пізніше")
 (browser-cmdline-expl-line-1 "(Командний рядок сформований з'єднанням попереднього тексту, URL,") ; explanatory text for dialog, line 1
 (browser-cmdline-expl-line-2 "та наступного тексту.)") ; ... line 2. (Anyone need more lines?)
 (install? "Інсталювати?")  ;; if a .plt file is found (title of dialog)
 (you-have-selected-an-installable-package "Ви обрали пакет для встановлення.")
 (do-you-want-to-install-it? "Ви хочете інсталювати його?")
 (paren-file-size "(Розмір файлу- ~a байт)")
 (download-and-install "Завантажити й інсталювати") ;; button label
 (download "Завантажити") ;; button label
 (save-downloaded-file/size "Зберегти завантажені файли (~a байт) як") ;; label for get-file dialog
 (save-downloaded-file "Зберегти завантажені файли як")  ;; label for get-file dialog
 (downloading "Завантаження") ;; dialog title
 (downloading-file... "Завантаження файлу...")
 (package-was-installed "Пакет інстальований.")
 (download-was-saved "Завантажені файли збережені.")

 (install-plt-file-menu-item... "Інсталювати .plt-файл...")
 (install-plt-file-dialog-title "Інсталювати .plt-файл")
 (install-plt-web-tab "Web")
 (install-plt-file-tab "Файл")
 (install-plt-filename "Ім'я файлу:")
 (install-plt-url "URL:")
 
 ;; install plt file when opened in drscheme strings
 (install-plt-file "Інсталювати ~a чи відкрити для редагування?")
 (install-plt-file/yes "Інсталювати")
 (install-plt-file/no "Редагувати")

 (plt-installer-progress-window-title "Інсталяція") ;; frame title
 (plt-installer-abort-installation "Процес інсталяції перервано") ;; button label
 (plt-installer-aborted "Перервано.") ;; msg that appears in the installation window when installation is aborted

 ;;; about box
 (about-drscheme-frame-title "Про DrRacket")
 
 ;;; save file in particular format prompting.
 (save-as-plain-text "Зберегти цей файл в звичайному текстовому форматі?")
 (save-in-drs-format "Зберегти цей файл в спеціальному нетекстовому форматі drscheme?")
 (yes "Так")     
 (no "Ні")
 
 ;; saving image (right click on an image to see the text)
  (save-image "Зберегти малюнок...")
  
 ;;; preferences
 (preferences "Налаштування користувача")
 (error-saving-preferences "Помилка збереження налаштувань користувача: ~a")
 (error-saving-preferences-title "Помилка збереження налаштувань користувача")
 (steal-the-lock-and-retry "Зняти блокування та повторити") ;; in the preferences error dialog; this happens when the lockfile exists (after 3 pref writes). 
 (error-reading-preferences "Помилка читання налаштувань користувача")
 (prefs-file-locked "Файл налаштувань користувача заблоковано (оскільки файл ~a існує), тому змінені налаштування не будуть збережені. Скасувати зміни?")
 (try-again "Спробувати знову") ;; button label
 (prefs-file-still-locked "Файл налаштувань користувача все ще заблоковано (оскільки файл ~a існує), тому змінені налаштування не будуть збережені..")
 (scheme-prefs-panel-label "Racket")
 (warnings-prefs-panel-label "Попередження")
 (editor-prefs-panel-label "Редагування")
 (general-prefs-panel-label "Загальне")
 (highlight-parens "Підсвічувати парні дужки")
 (fixup-open-brackets "Автокорекція квадратних дужок, які відкриваються")
 (fixup-close-parens "Автокорекція дужок, які закриваються")
 (flash-paren-match "Відображати відповідність дужок")
 (auto-save-files "Автозбереження файлів")
 (backup-files "Резервні копії файлів")
 (map-delete-to-backspace "Призначити Delete на Backspace")
 (verify-exit "Перевіряти завершення")
 (ask-before-changing-format "Запитувати перед зміною формату")
 (wrap-words-in-editor-buffers "Переносити слова в буферах редактора")
 (show-status-line "Показати рядок стану")
 (count-columns-from-one "Рахувати номери стовбців, починаючи з 1")
 (display-line-numbers "Відображати номери стовбців в буфері (немає зміщення символів)")
 (show-line-and-column-numbers "Показувати номери рядків і стовбців") ; used for popup menu; right click on line/column box in bottom of drs window
 (show-character-offsets "Показувати зміщення символів") ; used for popup menu; right click on line/column box in bottom of drs window
 (enable-keybindings-in-menus "Дозволити комбінації клавіш в меню")
 (command-as-meta "Вважати командний рядок мета-клавішою") ;; macos/macos x only
 (reuse-existing-frames "Використовувати існуючі вікна при відкритті нових файлів")
 (default-fonts "Шрифти за замовчуванням")
 (basic-gray-paren-match-color "Виділяти парні дужки сірим кольором") ; in prefs dialog
 (online-coloring-active "Інтерактивне підсвічування синтаксису")
 (open-files-in-tabs "відкривати файли в окремих вкладках (не нових вікнах)")
 (show-interactions-on-execute "Автоматично відкривати вікно інтерпретатора при виконанні програми")
  (switch-to-module-language-automatically "Автоматично перемикатись на мову модуля при відкритті модуля")
  (interactions-beside-definitions "Розміщувати вікно інтерпретатора поряд з вікном визначення") ;; in preferences, below the checkbox one line above this one
 (limit-interactions-size "Обмежити розмір програми")
 (background-color "Колір фону")
 (default-text-color "Колір тексту за замовчуванням") ;; used for configuring colors, but doesn't need the word "color"
 (choose-a-background-color "Будь-ласка, оберіть колір фону")
 (revert-to-defaults "Повернутись до значень за замовчуванням")
  
  (black-on-white-color-scheme "Чорний на білому") ;; these two appear in the color preferences dialog on butttons
  (white-on-black-color-scheme "Білий на чорному") ;; clicking the buttons changes teh color schemes to some defaults that've been set up.
  
 ; title of the color choosing dialog

 ; should have entire alphabet
 (font-example-string "Швидка руда лисиця перестрибнула через лінивих собак.") 

 (change-font-button-label "Змінити")
 (fonts "Шрифти")
 (other... "Інший...") ;; used in the font choice menu item

 ; filled with type of font, eg modern, swiss, etc.
 (choose-a-new-font "Будь-даска, оберіть новий \"~a\" шрифт")

 (font-size-slider-label "Розмір")
 (restart-to-see-font-changes "Перезапустіть, щоб побачити зміну типу шрифту")

 (font-prefs-panel-title "Шрифт")
 (font-name "Ім'я шрифту")
 (font-size "Розмір шрифту")
 (set-font "Встановити шрифт...")
 (font-smoothing-label  "Згладжування шрифтів")
 (font-smoothing-none "Відсутнє")
 (font-smoothing-some "Для деяких")
 (font-smoothing-all "Для всіх")
 (font-smoothing-default "Використати системні налаштування")
 (select-font-name "Вибір імені шрифту")
 (example-text "Зразок тексту:")
 (only-warn-once "Це попередження про те, що визначення і виклики не синхронізовані")
 
 ; warning message when lockfile is around
 (waiting-for-pref-lock "Очікування завершення блокування налаштувань користувача...")
 (pref-lock-not-gone
  "Файл блокування налаштувань користувача: \n\n   ~a\n\nне дозволяє зберегти зміни налаштувань. Переконайтесь у відсутності працюючого програмного забезпечення Racket та видаліть цей файл.")
 (still-locked-exit-anyway? "Налаштування не збережені. Все одно вийти?")
 
 ;;; indenting preferences panel
 (indenting-prefs-panel-label "Вирівнювання")
 (indenting-prefs-extra-regexp "Додаткові регулярні вирази")

 (square-bracket-prefs-panel-label "Квадратні дужки")
  
 ; filled with define, lambda, or begin
 (enter-new-keyword "Введіть нове ~a-подібне ключове слово:")
 (x-keyword "~a ключове слово")
 (x-like-keywords "~a-подібне ключове слово")

 ; used in Square bracket panel
 (skip-subexpressions "Кількість підвиразів, які буде пропущено")

 (expected-a-symbol "очікується символ,  знайдено: ~a")
 (already-used-keyword "\"~a\" -уже використовується як ключове слово")
 (add-keyword "Додати")
 (remove-keyword "Видалити")
 
  ; repl color preferences
  (repl-colors "REPL")
  (repl-out-color "Вивід")
  (repl-value-color "Значення")
  (repl-error-color "Помилки")
  
  ;;; find/replace
  (search-next "Далі")
  (search-previous "Назад")
  (search-match "Співпадання")  ;;; this one and the next one are singular/plural variants of each other
  (search-matches "Співпадання") 
  (search-replace "Замінити")
  (search-skip "Пропустити")
  (search-show-replace "Показати заміни")
  (search-hide-replace "Сховати заміни")
  (find-case-sensitive "З врахуванням регістру")  ;; the check box in both the docked & undocked search
  (find-anchor-based "Шукати, використовуючи прив'язки")

  ;; these string constants used to be used by searching,
  ;; but aren't anymore. They are still used by other tools, tho.
  (hide "Сховати")
  (dock "Прикріпити")
  (undock "Відкріпити")
  
 ;;; multi-file-search
 (mfs-multi-file-search-menu-item "Пошук в файлах...")
 (mfs-string-match/graphics "Відповідність рядків (опрацьовувати файли з графікою)")
 (mfs-regexp-match/no-graphics "Регулярні вирази (тільки для неформатованих текстових файлів)")
 (mfs-searching... "Пошук...")
 (mfs-configure-search "Налаштування пошуку") ;; dialog title
 (mfs-files-section "Файли")   ;; section in config dialog
 (mfs-search-section "Пошук") ;; section in config dialog
 (mfs-dir "Каталог")
 (mfs-recur-over-subdirectories "Шукати у вкладених каталогах")
 (mfs-regexp-filename-filter "Фільтрувати імена файлів за регулярними виразами")
 (mfs-search-string "Шукати рядки")
 (mfs-drscheme-multi-file-search "багатофайловый пошук - DrRacket") ;; results window and error message title
 (mfs-not-a-dir "\"~a\" не каталог")
 (mfs-open-file "Відкрити файл")
 (mfs-stop-search "Зупинити пошук")
 (mfs-case-sensitive-label "З врахуванням регістру")
 (mfs-no-matches-found "Співпадань не знайдено.")
 (mfs-search-interrupted "Пошук перервано.")
 
 ;;; reverting a file
 (are-you-sure-revert
  "Ви дійсно впевнені, що хочете повернутись до попередньої версії файлу? Внесені зміни буде неможливо відновити.")
 (are-you-sure-revert-title
  "Відкрити заново?")
 
 ;;; saving a file
 ; ~a is filled with the filename
 (error-saving "Помилка під час збереження") ;; title of error message dialog
 (error-saving-file/name "Під час збереження сталася помилка ~a.")
 (error-loading "Помилка відкриття")
 (error-loading-file/name "Під час відкриття сталася помилка ~a.")
 (unknown-filename "<< невідомий >>")

 ;;; finder dialog
 (must-specify-a-filename "Визначте ім'я файлу")
 (file-does-not-exist "Файл \"~a\" не існує.")
 (ask-because-file-exists "Файл \"~a\" уже існує. Замінити його?")
 (dne-or-cycle "Файл \"~a\" містить неіснуючий каталог або цикл.")
 (get-file "Отримати файл")
 (put-file "Помістити файл")
 (full-pathname "Повне ім'я")
 (show-dot-files "Показати файли й каталоги, які починаються з крапки.")
 (up-directory-button-label "В попередній каталог")
 (add-button-label "Додати") ;;; for multi-file selection
 (add-all-button-label "Додати все") ;;; for multi-file selection
 (remove-button-label "Видалити") ;;; for multi-file selection
 (file-wrong-form "Це ім'я файлу має неправильну форму.")
 (select-files "Вибір файлів")
 (select-file "Вибір файлу")
 (dir-dne "Каталог не існує.")
 (file-dne "Файл не існує.")
 (empty-filename "Ім'я файлу має містити символи.")
 (that-is-dir-name "Це ім'я каталогу.")
 
 ;;; raw menu names -- these must match the 
 ;;; versions below, once the &s have been stripped.
 ;;; if they don't, DrRacket's menus will appear
 ;;; in the wrong order.
 (file-menu "Файл")
 (edit-menu "Редагування")
 (help-menu "Довідка")
 (windows-menu "Вікна")
 
 ;;; menus
 ;;; - in menu labels, the & indicates a alt-key based shortcut.
 ;;; - sometimes, things are stuck in the middle of 
 ;;; menu item labels. For instance, in the case of
 ;;; the "Зберегти як" menu, you might see: "Зберегти визначення як". 
 ;;; be careful of spacing, follow the English, if possible.
 ;;; - the ellipses in the `after' strings indicates that
 ;;; more information is required from the user before completing
 ;;; the command.

 (file-menu-label "&Файл")

 (new-info  "Відкрити новий файл")
 (new-menu-item "&Новий")
 (new-...-menu-item "&Новий...")

 (open-info "Відкрити файл з диску")
 (open-menu-item "&Відкрити...")

 (open-recent-info "Список останніх відкритих файлів")
 (open-recent-menu-item "Відкрити о&станні")

 (revert-info "Замінити файл збереженим на диску")
 (revert-menu-item "Відкрити за&ново")

 (save-info "Зберегти цей файл на диск")
 (save-menu-item "&Зберегти")

 (save-as-info "Вказати ім'я файлу та зберегти цей файл на диск")
 (save-as-menu-item "Зберегти &як...")

 (print-info "Відправити файл на друк")
 (print-menu-item "&Друк...")

 (page-setup-info "Вибір конфігурації параметрів друку")
 (page-setup-menu-item "Параметри сторінки...")
 
 (close-info "Закрити файл")
 (close-menu-item "За&крити")

 (quit-info "Закрити всі вікна")
 (quit-menu-item-windows "&Вихід")
 (quit-menu-item-others "&Вихід")
 
 (edit-menu-label "&Редагування")
 
 (undo-info "Скасувати останню дію")
 (undo-menu-item "Скасувати")

 (redo-info "Повернути останню дію")
 (redo-menu-item "&Повернути")

 (cut-info "Перемістити обрані елементи в буфер обміну для подальшого вставлення")
 (cut-menu-item "Виріза&ти")

 (copy-info "Копіювати обрані елементи в буфер обміну для подальшого вставлення")
 (copy-menu-item "&Копіювати")

 (paste-info "Вставити останній скопійований або вирізаний елемент замість обраних елементів")
 (paste-menu-item "&Вставити")

 (clear-info "Видалити виокремлені елементи без зміни буферу або вставлення")
 (clear-menu-item-windows "Ви&далити")

 (select-all-info "Виокремити весь документ")
 (select-all-menu-item "Виокремити в&се")
 
  (find-menu-item "Знайти") ;; menu item
  (find-info "Перемикає клавіатурний фокус між вікном, в якому він здійснюється, і панеллю пошуку")
  
 (find-next-info "Знайти наступне входження")
 (find-next-menu-item "Знайти далі")
  
 (find-previous-info "Знайти попереднє входження")
 (find-previous-menu-item "Знайти попереднє")
  
  (show-replace-menu-item "Показати заміни")
  (hide-replace-menu-item "Сховати заміни")
  (show/hide-replace-info "Перемкнути видимість панелі замін")

  (replace-menu-item "Замінити")
  (replace-info "Замінити шукане")
  
  (replace-all-info "Замінити всі входження")
  (replace-all-menu-item "Замінити все")
  
  (find-case-sensitive-info "Перемкнутись між пошуком з врахуванням/без врахування регістру")
  (find-case-sensitive-menu-item "Здійснювати пошук з врахуванням регістру")
  
  (complete-word "Закінчення слів") ; the complete word menu item in the edit menu
  (no-completions "... закінчення недоступні") ; shows up in the completions menu when there are no completions (in italics)
  
  (overwrite-mode "Режим заміни")
  (enable-overwrite-mode-keybindings "Дозволити перемикання в режим заміни комбінацією клавіш")  

 (preferences-info "Конфігурація Ваших налаштувань")
 (preferences-menu-item "Налаштування користувача...")

 (keybindings-info "Показати активні комбінації клавіш")
 (keybindings-menu-item "Комбінації клавіш")
 (keybindings-show-active "Показати активні комбінації клавіш")
 (keybindings-frame-title "Комбінації клавіш")
 (keybindings-sort-by-name "Відсортувати за ім'ям")
 (keybindings-sort-by-key "Відсортувати за ключем")
 (keybindings-add-user-defined-keybindings "Додати визначені користувачем комбінації клавіш...")
 (keybindings-add-user-defined-keybindings/planet "Додати визначені користувачем комбінації клавіш з PLaneT...")
 (keybindings-menu-remove "Видалити ~a")
 (keybindings-choose-user-defined-file "Будь-ласка, оберіть файл, який містить комбінації клавіш.")
 (keybindings-planet-malformed-spec "Невірні налаштування PLaneT: ~a") ; the string will be what the user typed in
 (keybindings-type-planet-spec "Будь-ласка, введіть наобхідні налаштування PLaneT")
  
 ; first ~a will be a string naming the file or planet package where the keybindings come from;
 ; second ~a will be an error message
 (keybindings-error-installing-file "Помилка при інсталяції комбінації клавіш ~a:\n\n~a")
  
 (user-defined-keybinding-error "Помилка виконання комбінації клавіш ~a\n\n~a")
 (user-defined-keybinding-malformed-file "Файл ~a не містить модуля, написаного на мові оболонки/комбінацій клавіш.")  
 (user-defined-keybinding-malformed-file/found-lang "Файл ~a не містить модуля, написаного на мові оболонки/комбінацій клавіш. Знайдено мову ~s")     

 ;; menu items in the "special" menu
 (insert-text-box-item "Вставити текстовий блок")
 (insert-image-item "Вставити малюнок...")
 (insert-comment-box-menu-item-label "Вставити коментар")
 (insert-lambda "Вставити λ")

 (wrap-text-item "Переносити текст")

 ;; windows menu
 (windows-menu-label "&Вікна")
 (minimize "Згорнути") ;; minimize and zoom are only used under mac os x
 (zoom "Змінити розмір")
 (bring-frame-to-front "Показати поверх всіх вікон")       ;;; title of dialog
 (bring-frame-to-front... "Показати поверх всіх вікон...") ;;; corresponding title of menu item
 (most-recent-window "Останнє вікно")
  (next-tab "Наступна вкладка")
  (prev-tab "Попередня вкладка")

 (view-menu-label "&Вигляд")
 (show-overview "Показати контур програми") 
 (hide-overview "Сховати контур програми")
 (show-module-browser "Показати браузер модулів")
 (hide-module-browser "Сховати браузер модулів")

  (help-menu-label "&Довідка")
 (about-info "Відомості про авторів та подробицях застосування програми")
 (about-menu-item "Про програму...")
 
 ;; open here's new menu item
 (create-new-window-or-clear-current
  "Ви хочете створити нове вікно чи очистити поточне?")
 (clear-current "Очистити поточне")
 (new-window "Нове вікно")

 ;;; exiting and quitting ``are you sure'' dialog
 ;;; exit is used on windows, quit on macos, in English. Other
 ;;; languages probably use the same word on both platforms.
 (exit "Вихід")
 (quit "Вихід")
 (are-you-sure-exit "Ви дійсно впевнені, що хочете вийти?")
 (are-you-sure-quit "Ви дійсно впевнені, що хочете вийти?")
  ; these next two are only used in the quit/exit dialog
  ; on the button whose semantics is "dismiss this dialog".
  ; they are there to provide more flexibility for translations
  ; in English, they are just cancel.
 (dont-exit "Скасувати") 
 (dont-quit "Скасувати")
  
 ;;; autosaving
 (error-autosaving "Помилка автозбереження \"~a\".") ;; ~a will be a filename
 (autosaving-turned-off "Автозбереження відключене\nпід час запису файлу.")
 (recover-autosave-files-frame-title "Відновлення автозбережених файлів")
 (autosave-details "Подробиці")
 (autosave-recover "Відновлений")
 (autosave-unknown-filename "<<невідомий>>")
  
  ;; these are labels in a dialog that drscheme displays
  ;; if you have leftover autosave files. to see the dialog,
  ;; start up drscheme and modify (but don't save) a file
  ;; (also, do this with an unsaved file). Wait for the autosave
  ;; files to appear (typically 5 minutes). Kill DrRacket
  ;; and restart it. You'll see the dialog
  (autosave-autosave-label: "Автозбереження файлу:")
  (autosave-original-label: "Вихідний файл:")
  (autosave-autosave-label "Автозбереження файлу")
  (autosave-original-label "Вихідний файл")
  (autosave-compare-files "Порівняти автозбережені файли")

  (autosave-show-autosave "Автозбереження файлу") ;; title of a window showing the autosave file

  (autosave-explanation "DrRacket знайшов Ваші файли автозбереження, які можуть містити незбережену роботу.")

  (autosave-recovered! "Відновити!") ;; status of an autosave file
  (autosave-deleted "Видалити")       ;; status of an autosave file

  (autosave-error-deleting "Помилка видалення ~a\n\n~a") ;; first is a filename, second is an error message from mz.
  (autosave-delete-button "Видалити")
  (autosave-delete-title "Видалити")  ;; title of a dialog asking for deletion confirmation
  (autosave-done "Завершено")
  
  ;; appears in the file dialog
  (autosave-restore-to-where? "Оберіть місце для запису автозбереженого файлу.")
  
  
 ;;; file modified warning
 (file-has-been-modified
  "Файл був змінений після останнього автозбереження. Скасувати зміни?")
 (overwrite-file-button-label "Перезаписати")
 
 (definitions-modified 
  "Текст файлу визначень був змінений. Будь-ласка, збережіть або перевідкрийте визначення.")
 (drscheme-internal-error "Внутрішня помилка DrRacket")
 
 ;;; tools
 (invalid-tool-spec "Некоректний файл info.rkt, який містить опис інструменту в колекції ~a. Очікується рядок або непустий список рядків, отримано: ~e")
 (error-invoking-tool-title "Помилка виклику інструменту ~s;~s")
 (error-loading-tool-title "Помилка завантаження інструменту ~s\n~a") ;; ~s filled with a path, ~a filled with an error message from an exn
 (tool-tool-names-same-length
  "В файлі info.rkt для ~s очікується, що `tool-names' і `tools' - списки однакової довжини, отримано ~e і ~e")
 (tool-tool-icons-same-length
  "В файлі info.rkt для ~s очікується, що `tool-icons' і `tools' - списки однакової довжини, отримано ~e і ~e")
 (tool-tool-urls-same-length
  "В файлі info.rkt для ~s очікується, що `tool-urls' і `tools' - списки однакової довжини, отримано ~e і ~e")
 (error-getting-info-tool
  "помилка при завантаженні файлу info.rkt для ~s")
 (tool-error-phase1 "Помилка на 1 етапі для інструменту ~s; ~s")
 (tool-error-phase2 "Помилка на 2 етапі для інструменту ~s; ~s")


 ;;; define popup menu
 (end-of-buffer-define "<< кінець буферу >>")
 (sort-by-name "Упорядкувати за іменем")
 (sort-by-position "Упорядкувати за позицією в файлі")
 (no-definitions-found "<< визначення не знайдені >>")
 (jump-to-defn "Перейти до визначення ~a")

 (recent-items-sort-by-age "Упорядкувати за часом")
 (recent-items-sort-by-name "Упорядкувати за ім'ям")
 
 ;;; view menu
 (hide-definitions-menu-item-label "Сховати вікно визн&ачень")
 (show-definitions-menu-item-label "Показати вікно виз&начень")
 (definitions-menu-item-help-string "Показати/Сховати вікно визначень")
 (show-interactions-menu-item-label "Показати вікно &інтерпретатора")
 (hide-interactions-menu-item-label "Сховати вікно &інтерпретатора")
 (interactions-menu-item-help-string "Показати/Сховати вікно інтерпретатора")
 (toolbar "Панель інструментів")
 (toolbar-on-top "Панель інструментів зверху")
 (toolbar-on-left "Панель інструментів зліва")
 (toolbar-on-right "Панель інструментів справа")
 (toolbar-hidden "Сховати панель інструментів")

 ;;; file menu
 (save-definitions-as "Зберегти визначення &як...")
 (save-definitions "Зберегти визначення")
 (print-definitions "Друкувати визначення...")
 (about-drscheme "Про DrRacket")
 (save-other "Інші способи збереження")
 (save-definitions-as-text "Зберегти визначення як текст...")
 (save-interactions "Зберегти вікно інтерпретатора")
 (save-interactions-as "Зберегти вікно інтерпретатора як...")
 (save-interactions-as-text "Зберегти вікно інтерпретатора як текст...")
 (print-interactions "Друкувати вікно інтерпретатора...")
 (new-tab "Нова вкладка")
 (close-tab "Закрити вкладку") ;; must not have any &s in it.
 (close-tab-amp "&Закрити вкладку") ;; like close-tab, but with an ampersand on the same letter as the one in close-menu-item
  
 ;;; edit-menu
 (split-menu-item-label "&Розділити")
 (collapse-menu-item-label "&Об'єднати")
 
 ;;; language menu
 (language-menu-name "&Мова")
 
 ;;; scheme-menu
 (scheme-menu-name "&Racket")
 (execute-menu-item-label "Виконати")
 (execute-menu-item-help-string "Перезапустити програму в вікні визначень")
 (ask-quit-menu-item-label "Перервати програму")
 (ask-quit-menu-item-help-string "Перервати програму, яка виконується")
 (force-quit-menu-item-label "Примусово вийти з програми")
 (force-quit-menu-item-help-string "Перервати всі поточні обчислення")
 (limit-memory-menu-item-label "Обмеження по пам'яті...")
 ;(limit-memory-msg-1 "Обмеження стануть діяти після наступного виконання програми")
 ;(limit-memory-msg-2 "Для виконання необхідно, принаймні, 1 мегабайт.")  ;; minimum memory limit is now 8 megabytes
 (limit-memory-unlimited "Без обмежень")
 (limit-memory-limited "З обмеженнями")
 (limit-memory-megabytes "Мегабайт")
 (clear-error-highlight-menu-item-label "Очистити виокремлення помилок")
 (clear-error-highlight-item-help-string "Видалити виокремлення помилок")
 (reindent-menu-item-label "&Вирівняти")
 (reindent-all-menu-item-label "Вирівняти вс&е")
 (semicolon-comment-out-menu-item-label "&Закоментувати виокремлене крапкою з комою")
 (box-comment-out-menu-item-label "&Закоментувати весь блок")
 (uncomment-menu-item-label "&Розкоментувати")

 (convert-to-semicolon-comment "Перетворити в коментар з крапкою з комою")
 
 ;;; executables
 (create-executable-menu-item-label "Створити виконуваний файл...")
 (create-executable-title "Створити виконуваний файл")
 (must-save-before-executable "Ви повинні зберегти свою програму, перш ніж створити виконуваний файл.")
 (save-a-mred-launcher "Створити таким, який виконується в графічній оболонці")
 (save-a-mzscheme-launcher "Створити таким, який виконується в текстовій оболонці")
 (save-a-mred-stand-alone-executable "Створити автономний виконуваний файл з графічним інтерфейсом")
 (save-a-mzscheme-stand-alone-executable "Створити автономний виконуваний файл з текстовим інтерфейсом")
 (save-a-mred-distribution "Зберегти дистрибутив з графічним інтерфейсом")
 (save-a-mzscheme-distribution "Зберегти дистрибутив з текстовим інтерфейсом")

 (definitions-not-saved "Вікно визначень не було збережено. Програма, що виконується, буде використовувати останню збережену версію вікна визначень. Продовжити?")
 ;; The "-explanatory-label" variants are the labels used for the radio buttons in
 ;;  the "Create Executable..." dialog for the "(module ...)" language.
 (launcher "Виконувати в оболонці")
 (launcher-explanatory-label "Виконувати в оболонці (тільки для поточного комп'ютера, виконувати з вихідного файлу)")
 (stand-alone "Автономный")
 (stand-alone-explanatory-label "Автономний (тільки для поточного комп'ютера, виконання скомпільованої копії)")
 (distribution "Дистрибутив")
 (distribution-explanatory-label "Дистрибутив (для встановлення на інших машинах)")
 (executable-type "Тип")
 (executable-base "База")
 (filename "Ім'я файлу: ")
 (create "Створити")
 (please-specify-a-filename "Будь-ласка, визначте ім'я файлу, який створюється.")
 (~a-must-end-with-~a
  "Ім'я ~a файлу\n\n  ~a\n\nнеправильне. Ім'я файлу повинне закінчуватись \".~a\".")
 (macosx-executables-must-end-with-app
  "Ім'я файлу\n\n  ~a\n\nнеправильне. Для MacOS X виконувана програма повинна бути каталогом, ім'я якого закінчується .app.")
 (warning-directory-will-be-replaced
  "Попередження: каталог:\n\n  ~a\n\nбуде замінений. Продовжити?")
 
 (distribution-progress-window-title "Створення дистрибутиву")
 (creating-executable-progress-status "Створення виконуваних файлів для дистрибутиву...")
 (assembling-distribution-files-progress-status "Збирання файлів для дистрибутиву...")
 (packing-distribution-progress-status "Пакування дистрибутиву...")

 (create-servlet "Створити сервлет...")

 ; the ~a is a language such as "module" or "algol60"
 (create-servlet-unsupported-language
  "Створення сервлету не підтримується мовою ~a.")
  
 ;;; buttons
 (execute-button-label "Виконати") 
 (save-button-label "Зберегти")
 (break-button-label "Зупинити")
 
 ;;; search help desk popup menu
 (search-help-desk-for "Пошук в допомозі для \"~a\"")
 (exact-lucky-search-help-desk-for "Точний пошук в допомозі для \"~a\"")

 ;; collapse and expand popup menu items
 (collapse-sexp "Згорнути S-вираз")
 (expand-sexp "Розгорнути S-вираз")
 
 ;;; fraction dialog
 (enter-fraction "Введіть дріб")
 (whole-part "Ціла частина")
 (numerator "Чисельник")
 (denominator "Знаменник")
 (insert-number/bad-whole-part "Ціла частина числа повинна  бути цілим числом")
 (insert-number/bad-numerator "Чисельник повинен бути цілим невід'ємним числом")
 (insert-number/bad-denominator "Знаменник повинен бути цілим додатнім числом")
 (insert-fraction-menu-item-label "Вставити дріб...")

 ;; number snip popup menu
 (show-decimal-expansion "Показати десятковим дробом")
 (show-mixed-fraction-view "Показати змішаним дробом")
 (show-improper-fraction-view "Показати неправильним дробом")
 (show-more-decimal-places "Показати більше позицій після десяткової крапки")
 
 ;;; Teachpack messages
 (select-a-teachpack "Обрати навчальний пакет")
 (clear-teachpack "Очистити навчальний пакет ~a")
 (teachpack-error-label "DrRacket - помилка навчального пакету")
 (teachpack-didnt-load "Файл навчального пакету ~a не завантажується належним чином.")
 (add-teachpack-menu-item-label "Додати навчальний пакет...")
 (clear-all-teachpacks-menu-item-label "Очистити всі навчальні пакети")
 (drscheme-teachpack-message-title "Навчальний пакет DrRacket")
 (already-added-teachpack "Навчальний пакет ~a уже доданий")
  
  ; ~a is filled with the teachpack's name; the message appears in the teachpack selection dialog when a user installs a new teachpack
  (compiling-teachpack "Компіляція навчального пакету ~a ...")
  (teachpack-pre-installed "Попередньо інстальовані навчальні пакети")
  (teachpack-user-installed "Навчальні пакети користувача")
  (add-teachpack-to-list... "Додати  навчальний пакет до списку...")
  (teachpack-already-installed "Навчальний пакет з ім'ям '~a' уже був інстальований. Перезаписати?")
  ; ~a is filled with a list of language names. Each name is separated by a newline and is indented two spaces (no commas, no 'and')
  (teachpacks-only-in-languages "Навчальні пакети доступні лише на таких мовах: ~a")
  
  
 ;;; Language dialog
 (introduction-to-language-dialog
  "Будь-ласка, оберіть мову. Студентам у вступних курсах бажано використовувати мову за замовчуванням.")
 (language-dialog-title "Оберіть мову")
 (case-sensitive-label "Чутливість до регістру")
 (output-style-label "Стиль виведення")
 (constructor-printing-style "Конструктор")
 (quasiquote-printing-style "Псевдоапострофи")
 (write-printing-style "ввід")
 (print-printing-style "друк")
 (sharing-printing-label "Показувати сумісно використовуване в значеннях")
 (use-pretty-printer-label "Додавати перехід на новий рядок в друковані значення")
 (input-syntax "Синтакис введення")
 (dynamic-properties "Динамічні властивості")
 (output-syntax "Синтаксис виведення")
  (teachpacks "Навчальні пакети") ;; label in the language dialog for the teaching languages
  (teachpacks-none "<< відсутні >>") ;; shows up under the previous string, when there are no teachpacks
 (no-debugging-or-profiling "Не налагоджувати або профілювати")
 (debugging "Налагодити")
 (debugging-and-profiling "Налагодити й профілювати")
 (test-coverage "Включати набір синтаксичних тестів")
 (show-details-button-label "Показати подробиці")
 (hide-details-button-label "Сховати подробиці")
 (choose-language-menu-item-label "Обрати мову...")
 (revert-to-language-defaults "Повернути мову за замовчуванням")
 (fraction-style "Стиль дробу")
 (use-mixed-fractions "Змішані дроби")
 (use-repeating-decimals "Періодичні десяткові дроби")
 (decimal-notation-for-rationals "Використовувати десятковий запис для раціональних чисел")
 (enforce-primitives-group-box-label "Початкові прив'язки")
 (enforce-primitives-check-box-label "Заборонити перевизначення початкових прив'язок")
 (automatically-compile "Заповнити каталоги compiled/ (для більш швидкого завантаження)")
 (preserve-stacktrace-information "Зберегти трасування стеку (відключає деякі оптимізації")
 (expression-level-stacktrace "Вираз рівню трасування стеку")
 (function-level-stacktrace "Функція рівню трасування стеку")


  ; used in the bottom left of the drscheme frame 
  ; used the popup menu from the just above; greyed out and only
  ; visible when some languages are in the history
  (recent-languages "Останні мови:")
  ; shows up in bottom-left programming language menu popup, when no langs are recorded
  (no-recently-chosen-languages "немає обраних мов") 
  
 ;; startup wizard screen language selection section
 (please-select-a-language "Будь-ласка, оберіть мову")
  
  
 ;;; languages
 (beginning-student "Студент-початківець")
 (beginning-one-line-summary "define, cond, структури, константи та примітиви")
 (beginning-student/abbrev "Студент-початківець зі списковими скороченнями")
 (beginning/abbrev-one-line-summary "Студент-початківець зі списковим стилем друку в циклі \"читання-обчислення-друк\"")
 (intermediate-student "Середній студент")
 (intermediate-one-line-summary "Студент-початківець з лексичною видимістю")
 (intermediate-student/lambda "Середній студент з лямбда-виразами")
 (intermediate/lambda-one-line-summary "Середній студент з функціями вищих порядків")
 (advanced-student "Досвічений студент")
 (advanced-one-line-summary "Середній студент з лямбда-виразами й мутацією")
 (how-to-design-programs "Як проектувати програми") ;; should agree with MIT Press on this one...
 (pretty-big-scheme "Pretty Big")
 (pretty-big-scheme-one-line-summary "Додані синтаксис і функції мов з \"Як проектувати програми\" в mzscheme й mred")
 (r5rs-language-name "R5RS")
 (r5rs-one-line-summary " R5RS з несуттєвими скороченнями")
 (expander "Макрорасширитель")
 (expander-one-line-summary "Вираз частіше розкривається, ніж обчислюється")
 (legacy-languages "Успадковані мови")
 (teaching-languages "Навчальні мови")
 (experimental-languages "Експериментальні мови")
  (initial-language-category "Початкові мови")
  (no-language-chosen "Не обрана мова")
 
 (module-language-one-line-summary "Виконання створює цикл \"читання-обчислення-друк\" всередині модуля")
  (module-language-auto-text "Автоматичний рядок #lang") ;; shows up in the details section of the module language    

  ;;; from the `not a language language' used initially in drscheme.
  (must-choose-language "DrRacket не здатний виконувати програми, якщо не обрана мова програмування.")
  
  ; next two appear before and after the name of a text book (which will be in italics)
  (using-a-textbook-before "Використати ")
  (using-a-textbook-after "?")
  
  ; next two are before and after a language
  (start-with-before "Почати з ")
  (start-with-after "")
  
  (seasoned-plt-schemer? "Досвічений програміст на PLT Scheme?")
  (looking-for-standard-scheme? "Проглянути стандарт для Scheme?")

  ; the three string constants are  together and the middle
  ; one is hyperlinked to the dialog that suggests various languages
  (get-guidance-before "Або оберіть пункт \"Обрати мову...\" в меню \"Мова\", або ")
  (get-guidance-during "отримайте пораду")
  (get-guidance-after ".")
    
 ;;; debug language
 (unknown-debug-frame "[невідомий]")
 (backtrace-window-title "Обернене трасування DrRacket")
 (files-interactions "При виконанні ~a") ;; filled with a filename
 (current-interactions "викликів")
 (current-definitions "визначень")
 (mzscheme-w/debug "Текстовий (MzScheme, включаючи Стандарт 1998 року)")
 (mzscheme-one-line-summary "PLT-реалізація Scheme")
 (mred-w/debug "Графічний (MrEd, включаючи MzScheme)")
 (mred-one-line-summary "Додати підтримку графічного інтерфейсу користувача до MzScheme")

 ;; profiling
 (profiling-low-color "Нижній")
 (profiling-high-color "Верхній")
 (profiling-choose-low-color "Будь-ласка, оберіть нижній колір")
 (profiling-choose-high-color "Будь-ласка, оберіть верхній колір")
 (profiling "Профілювання")
 (profiling-example-text "(define (whee) (whee))")
 (profiling-color-config "Набір основних кольорів") 
 (profiling-scale "Шкала розподілу кольорів")
 (profiling-sqrt "Пропорційна кореню квадратному")
 (profiling-linear "Лінійна")
 (profiling-square "Квадратична")
 (profiling-number "Число викликів")
 (profiling-time "Загальний час")
 (profiling-update "Оновити профіль")
 (profiling-col-percent-time "% часу")
 (profiling-col-function "Функція")
 (profiling-col-time-in-msec "мілісекунд")
 (profiling-col-calls "Виклики")
 (profiling-show-profile "Показати профіль")
 (profiling-hide-profile "Сховати профіль")
 (profiling-unknown-src "<< невідомий >>")
 (profiling-no-information-available "Інформація про профілювання недоступна. Будь-ласка, переконайтеся, що профілювання допускається на обраній мові, а програма виконана.")
 (profiling-clear? "Зміни в вікні визначень позбавляють профілюючу інформацію актуальності. Продовжити?")
 
 ;; test coverage
 (test-coverage-clear? "Зміни в вікні визначень позбавляють інформацю про тестування актуальності. Продовжити?")
 (test-coverage-clear-and-do-not-ask-again "Так, більше не питати")
 (test-coverage-ask? "Питати про очищення інформації про тестування")
  
 ;; tracing
 (tracing-enable-tracing "Допустити трасування")
 (tracing-show-tracing-window "Показати трасування")
 (tracing-hide-tracing-window "Сховати трасування")
 (tracing-tracing-nothing-to-show "Результати трасування ще не доступні. (Переконайтеся, що обрана мова підтримує трасування і що трасування ввімкнене.)")

 ;;; repl stuff
 (evaluation-terminated "Перервати обчислення")
 (evaluation-terminated-explanation
  "Потік обчислень зупинений до наступного виконання.")
  
  ; The next three constants show up in the same dialog as the above evaluation-terminated string
  ; constants.
  ; The first two show up only when the user calls 'exit' (possibly with a status code).
  ; The third shows up when the program runs out of memory.
  (exited-successfully "Успішне завершення.")
  (exited-with-error-code "Завершення з кодом помилки ~a.") ;; ~a is filled in with a number between 1 and 255
  (program-ran-out-of-memory "Програмі не вистачило пам'яті для завершення виконання.")
 (last-stack-frame "Показати останнє значення стеку")
 (last-stack-frames "Показати останні ~a значень стеку")
 (next-stack-frames "Показати наступні ~a значень стеку")
 
 ;;; welcoming message in repl
 (language "Мова")
 (custom "обрана")
 (teachpack "Навчальний пакет")
 (welcome-to "Ласкаво просимо до")
 (version "версія")
 
 ;;; kill evaluation dialog
 (kill-evaluation? "Ви хочете перервати цикл обчислень?")
 (just-break "Тільки зупинити")
 (kill "Перервати")
 (kill? "Перервати?")

 ;;; version checker
 (version:update-menu-item   "Перевірка оновлень...")
 (version:update-check       "Перевірка оновлень") ; dialog title, with the next line
 (version:connecting-server  "З'єднання з сервером версії Racket")
 (version:results-title      "Перевірка версії Racket")
 (version:do-periodic-checks "Періодично перевіряти новіші версії Racket")
 (version:take-me-there      "Оновити") ; ...to the download website
 ;; the next one can appear alone, or followed by a comma and the one after that
 (version:plt-up-to-date     "У Вас найновіша версія Racket")
 (version:but-newer-alpha    "однак зауважте, що є новіша альфа-версія")
 ;; This is used in this context: "Racket vNNN <<<*>>> http://download..."
 (version:now-available-at   "тепер є в")

 ;; insert menu
 (insert-menu "&Вставлення")
 
 ;; large semi colon letters
 (insert-large-letters... "Вставити великі літери...")
 (large-semicolon-letters "Великі закоментовані літери")
 (text-to-insert "Вставити текст")

 (module-browser-filename-format "Повне ім'я: ~a (~a рядків)")
 (module-browser-root-filename "Основне ім'я файлу: ~a")
 (module-browser-font-size-gauge-label "Розмір шрифту")
 (module-browser-progress-label "Процес перегляду модулів")
 (module-browser-adding-file "Додати файл: ~a...")
 (module-browser-laying-out-graph-label "Граф розмітки")
 (module-browser-open-file-format "Відкрити ~a")
 (module-browser "Браузер модуля") ;; frame title
 (module-browser... "Браузер модуля...") ;; menu item title
 (module-browser-error-expanding "Помилка відкриття програми:\n\n~a")
 (module-browser-show-lib-paths "Показувати файли, які завантажуються з бібліотек")
 (module-browser-progress "Браузер модуля: ~a") ;; prefix in the status line
 (module-browser-compiling-defns "Браузер модуля: компіляція визначень")
 (module-browser-show-lib-paths/short "Показувати шляхи до бібліотек") ;; check box label in show module browser pane in drscheme window.
 (module-browser-show-planet-paths/short "Показувати шляхи до planet") ;; check box label in show module browser pane in drscheme window.
 (module-browser-refresh "Оновити") ;; button label in show module browser pane in drscheme window.
; (module-browser-only-in-plt-and-module-langs
;  "Браузер модуля доступний лише для програм на PLT-мовах і на мові модуля (і лише для програм, які містять модулі).")
 (module-browser-name-length "Довжина імені")
 (module-browser-name-short "Коротка")
 (module-browser-name-medium "Середня")
 (module-browser-name-long "Довга")
 (module-browser-name-very-long "Довга, з фазами")  ;; like 'Long' but shows the phases where this file is loaded
 (module-browser-open-all "Відкрити всі файли, які тут показані")

 (happy-birthday-matthias "З днем народження, Матіас!")
 (happy-birthday-matthew "З днем народження, Метью!")
 (happy-birthday-shriram "З днем народження, Шрірам!")

 (mrflow-using-default-language-title "Мова за замовчуванням")
 (mrflow-using-default-language "У мові, що використовується, не визначена таблиця типів для її примітивів. Зверніться до  стандарту Scheme.")
 (mrflow-button-title "Аналізувати")
 ;(mrflow-unknown-style-delta-error-title "Невідомий стиль блоку дельта")
 ;(mrflow-unknown-style-delta-error "Невідомий стиль блоку дельта: ~a")
 (mrflow-popup-menu-show-type "Показати тип")
 (mrflow-popup-menu-hide-type "Сховати тип")
 (mrflow-popup-menu-show-errors "Показати помилки")
 (mrflow-popup-menu-hide-errors "Сховати помилки")
 ;(mrflow-read-exception-title "Read Exception")
 ;(mrflow-read-exception "Read exception: ~a")
 ;(mrflow-syntax-exception-title "Syntax Exception")
 ;(mrflow-syntax-exception "Syntax Exception: ~a")
 ;(mrflow-unknown-exception-title "Unknown Exception")
 ;(mrflow-unknown-exception "Unknown Exception: ~a")
 ;(mrflow-language-primitives-error-title "Language Primitives Error")
 ;(mrflow-language-primitives-error "Wrong filename for language primitives types table: ~a")
  
 (snips-and-arrows-popup-menu-tack-all-arrows "З'єднати всі стрілки")
 (snips-and-arrows-popup-menu-untack-all-arrows "Видалити всі стрілки")
 (snips-and-arrows-user-action-disallowed-title "Внесення змін користувачем наразі заборонено")
 (snips-and-arrows-user-action-disallowed "Внесення змін користувачем заборонено в редакторах, які містять елементи з панелі інструментів. Сховайте всі інструменти перед редагуванням.")
 ;(snips-and-arrows-changing-terms-warning-title "Changing terms will be undoable")
 ;(snips-and-arrows-changing-terms-warning "Changing terms in an editor containing snips cannot be undone.  You can either cancel this action, remove the snips, and try the change again, or you can continue with the change, in which case the change will not be undoable (all others changes made before and afterward will still be undoable though).")
 (snips-and-arrows-hide-all-snips-in-editor "Сховати всі інструменти, що вставлені в редактор")

 (xml-tool-insert-xml-box "Вставити блок XML")
 (xml-tool-insert-scheme-box "Вставити блок Racket")
 (xml-tool-insert-scheme-splice-box "Приєднати блок Racket")
 (xml-tool-xml-box "XML блок")
 (xml-tool-scheme-box "Racket блок")
 (xml-tool-scheme-splice-box "Приєднаний блок Racket")
 (xml-tool-switch-to-scheme "Перемкнутися на блок Racket")
 (xml-tool-switch-to-scheme-splice "Перемкнутися на приєднаний блок Racket")
 (xml-tool-eliminate-whitespace-in-empty-tags
  "Видаляти пробіли в пустих тегах")
 (xml-tool-leave-whitespace-alone
  "Залишати по одному пробілу")
 
 (show-recent-items-window-menu-item "Показати нещодавно відкриті файли в окремому вікні")
 (show-recent-items-window-label "Нещодавно відкриті файли")
 (number-of-open-recent-items "Кількість нещодавніх елементів")
 (switch-anyway "Все одно перейти до файлу")

 (stepper-program-has-changed "Попередження: Програма була змінена.")
 (stepper-program-window-closed "Попередження: Вікно програми закрите.")

 (stepper-name "Покрокове виконання")
 (stepper-language-level-message "Покрокове виконання не працює для мови \"~a\".")
 (stepper-button-label "Крок")

 (stepper-previous-application "Програма")
 (stepper-previous "Крок")
 (stepper-next "Крок")
 (stepper-next-application "Програма")
 (stepper-jump "Перейти...") ;; this one is changed.  action?
 (stepper-out-of-steps "Обчислення завершено раніше, ніж досягнуто шуканий крок.")
 (stepper-no-such-step/title "Крок не знайдено")
 (stepper-no-such-step "Крок, що відповідає критерію, не знайдено.")
 (stepper-no-such-step/earlier "Попередній крок, що відповідає критерію, не знайдено.")
 (stepper-jump-to-beginning "в початок") ;; name changed from stepper-home to stepper-jump-to-beginning
 (stepper-jump-to-end "в кінець") ;; content changed
 (stepper-jump-to-selected "до початку обраного") ;; new
 
 (debug-tool-button-name "Налагодити")

 (dialog-back "Назад")

 ;; warnings about closing a drscheme frame when the program
 ;; might still be doing something interesting
 (program-is-still-running "Програма в вікні визначень все ще виконується. Все одно закрити?")
  (program-has-open-windows "Програма в вікні визначень має відкриті вікна. Все одно закрити?")
 
  ;; ml-command-line-arguments is for the command line arguments
  ;; label in the module language details in the language dialog.
  (ml-command-line-arguments "Аргументи командного рядка як вектор рядків")

  ;; ml-cp names are all for the module language collection path
  ;; configuration. See the details portion of the language dialog
  ;; for the module language (at the bottom).
  (ml-cp-default-collection-path "<<шляхи до колекції за замовчуванням>>")

  ;; in std get-directory 
  (ml-cp-choose-a-collection-path "Будь-ласка, оберіть шлях до колекції")

  ;; err msg when adding default twice
  (ml-cp-default-already-present
   "Задані за замовчуванням шляхи до колекції вже встановлені")
  
  ;; title of this section of the dialog (possibly the word
  ;; `Collection' should not be translated)
  (ml-cp-collection-paths "Шляхи до колекції")

  ;; button labels
  (ml-cp-add "Додати")
  (ml-cp-add-default "Додати за замовчуванням")
  (ml-cp-remove "Видалити")
  (ml-cp-raise "Вгору")
  (ml-cp-lower "Вниз")
  
  (ml-always-show-#lang-line "Завжди показувати рядок #lang модуля мови")

  ;; Profj
  (profj-java "Java")
  (profj-java-mode "Режим Java")
  (profj-java-coverage "Набір Java") ;; shows up in the preferences dialog under 'Color'

  (profj-beginner-lang "Початківець")
  (profj-beginner-lang-one-line-summary "Java-подібна мова для навчання початківців")
  (profj-full-lang "Достатній")
  (profj-full-lang-one-line-summary "Подібна Java 1.0 (наближено 1.1)")
  (profj-advanced-lang "Досвічений")
  (profj-advanced-lang-one-line-summary "Java-подібна мова для навчання досвічених користувачів")
  (profj-intermediate-lang "Середній")
  (profj-intermediate-lang-one-line-summary "Java-подібна мова для тих, хто продовжує навчання")
  (profj-intermediate-access-lang "Середній + доступ")
  (profj-intermediate-access-lang-one-line-summary "Java-подібна мова для тих, хто продовжує навчання, з доступом до модифікаторів")
  (profj-dynamic-lang "Java + динамічна типізація")
  (profj-dynamic-lang-one-summary "Java з можливістю динамічної типізації")

  (profj-java-mode-color-heading "Кольори редагування") ; Heading for preference to choose editing colors  
  (profj-java-mode-color-keyword "ключові слова")
  (profj-java-mode-color-string "рядок")
  (profj-java-mode-color-literal "літерал")
  (profj-java-mode-color-comment "коментар")
  (profj-java-mode-color-error "помилка")
  (profj-java-mode-color-identifier "ідентифікатор")
  (profj-java-mode-color-prim-type "примітивний тип") ; Example text for built-in Java types
  (profj-java-mode-color-default "за замовчуванням")

  (profj-coverage-color-heading "Кольори середовища") ; Heading for preference to choose coverage colors
  (profj-coverage-color-covered "вираз") 
  
  (profj-language-config-display-preferences "Персональні налаштування відображення") ; Heading for preferences controlling printing
  (profj-language-config-display-style "Стиль відображення")
  (profj-language-config-display-field "Клас + Поля")
  (profj-language-config-class "Клас")
  (profj-language-config-display-array "Виводити масиви повністю?")
  (profj-language-config-testing-preferences "Налаштування тестування") ; Heading for preferences controlling test behavior
  ;(profj-language-config-testing-enable "Відображати результати тестування при виконанні?") ; Run should be the word found on the Run button
  (profj-language-config-testing-coverage "Зібрати інформацію для тестування?")
  (profj-language-config-support-test-language "Підтримувати тестування мовних розширень?")
  (profj-language-config-testing-check "Дозволити перевірку виразів?") ; check should not be translated
  (profj-language-config-classpath "Шлях до класів")
  (profj-language-config-choose-classpath-directory "Обрати каталог для додавання до шляху до класів")
  (profj-language-config-classpath-display "Показати поточний") ; Button label to print the current classpath

  (profj-test-name-close-to-example "Ім'я класу ~a містить фразу, близьку до зразків.")
  (profj-test-name-example-miscapitalized "Ім'я класу ~a з точністю до регістру містить зразок.")
  
   ;; Close testing window and do not run test cases any more
  ;(profj-test-results-close-and-disable "Закрити й заборонити тестування")
  ;; Hide docked testing window and do not run test cases any more
  ;(profj-test-results-hide-and-disable "Сховати й заборонити тестування")
  ;Renamed below
  ;(profj-test-results-window-title "Результати тестування")
  
  (profj-unsupported "Не підтримується")
  (profj-executables-unsupported "Вибачте, наразі Java не підтримує виконувані файли")

  (profj-convert-to-text-comment "Перетворити в текстовий коментар")
  (profj-convert-to-comment "Перетворити в коментар")

  (profj-executing-main "виконання main")

  (profj-insert-java-comment-box "Вставити блок Java-коментарю")
  (profj-insert-java-interactions-box "Вставити блок Java-коду")

  ;;The Test engine tool
  ;;
  (test-engine-window-title "Результати тестування")
  ;;Following two appear in View menu, attach and free test report window from DrRacket frame
  (test-engine-dock-report "Прикріпити звіт про тестування")
  (test-engine-undock-report "Відкріпити звіт про тестування")
  ;;Following two appear in Racket (Java, etc) menu, cause Tests to be Run automatically or not
  (test-engine-enable-tests "Дозволити тестування")
  (test-engine-disable-tests "Заборонити тестування")

  (test-engine-ran-1-test "Виконаний 1 тест.")
  (test-engine-ran-1-check "Виконана 1 перевірка.")
  ;; ditto, only plural
  (test-engine-ran-n-tests "Виконано  ~a тестів.")
  (test-engine-ran-n-checks "Виконано ~a перевірок.")
  (test-engine-1-test-passed "Тест пройдений!")
  (test-engine-1-check-passed "Перевірка закінчена!")
  (test-engine-both-tests-passed "Обидва тести пройдені!")
  (test-engine-both-checks-passed "Обидві перевірки закінчені!")
  (test-engine-all-tests-passed "Всі тести пройдені!")
  (test-engine-all-checks-passed "Всі перевірки закінчені!")
  (test-engine-all-n-tests-passed "Всі ~a тести пройдені!")
  (test-engine-all-n-checks-passed "Всі ~a перевірки закінчені!")
  (test-engine-0-tests-passed "0 тестів пройдено.")
  (test-engine-0-checks-passed "0 перевірок закінчено.")
  (test-engine-m-of-n-tests-failed "~a з ~a тестів помилкові.")
  (test-engine-m-of-n-checks-failed "~a з ~a перевірок помилкові.")
  (test-engine-must-be-tested "Ця програма повинна бути протестована!")
  (test-engine-is-unchecked "Ця програма не перевірена!")
  (test-engine-tests-disabled "Тести відключені.")
  (test-engine-should-be-tested "Ця програма повинна бути перевірена.")
  (test-engine-at-line-column "в рядку ~a, стовбцю ~a")
  (test-engine-in-at-line-column "в ~a, рядку ~a, стовбцю ~a")
  ; as in "column (unknown)"
  (test-engine-unknown "(невідомо)")
  (test-engine-trace-error "Помилка трасування")

  ; The ~F is special marker for the offending values, which may be
  ; printed specially in DrRacket.
  (test-engine-check-encountered-error
   "помилка перевірки: замість очікуваного значення, ~F. ~n   :: ~a")
  (test-engine-actual-value-differs-error
   "Фактичне значення ~F відрізняється від очікуваного ~F.")
  (test-engine-actual-value-not-within-error
   "Фактичне значення ~F виходить за границі ~v очікуваного значення ~F.")
  (test-engine-encountered-error-error
   "помилка перевірки: наступна помилка замість очікуваного ~a~n   :: ~a")
  (test-engine-expected-error-error
   "помилка перевірки: очікувалась помилка, натомість отримано значення ~F.~n ~a")

  ; section header
  (test-engine-check-failures "Помилки тестування:")
  ; section header
  (test-engine-signature-violations "Порушення домовленості:")

  ; part of one phrase "signature <at line ...> to blame: procedure <...>
  (test-engine-signature "домовленість")
  (test-engine-to-blame "порушено: процедура")

  (test-engine-no-signature-violations "Немає порушень домовленості.")
  (test-engine-1-signature-violation "1 порушення домовленості.")
  (test-engine-n-signature-violations "~a порушень домовленості.")

  ; as in got <value>, signature <at ...>
  (test-engine-got "отримано")
  
  (profjWizward-insert-java-class "Вставити клас Java")
  (profjWizard-insert-java-union "Вставити об'єднання Java")
  
  ;; The Test Suite Tool
  ;; Errors
  (test-case-empty-error "Пустий набір тестів")
  (test-case-too-many-expressions-error "Забагато виразів у наборі тестів.")
  ;; DrRacket window menu items
  (test-case-insert "Вставити набір тестів")
  (test-case-disable-all "Відключити всі набори тестів")
  (test-case-enable-all "Ввімкнути всі набори тестів")
  
  ;; NOTE: The following string constants are labels of the test-case fields. The width
  ;;       of the field is determined by the length of the longest of the following three words.
  ;;       if the words are too long the test case will take up too much horizontal room and
  ;;       not look very good.
  ;; This string is the label of the expression that is being tested in a test case.
  (test-case-to-test "Виконується перевірка")
  ;; This string is the label of the expression that is the expected value of the to-test expression.
  (test-case-expected "Повинно бути")
  ;; This string is the label of the actual result of the to test expression.
  (test-case-actual "Фактично")
  (test-case-predicate "Предикат")
  (test-case-should-raise "Повинен бути")
  ;; The label of a field of the test-case that describes the expected error message of a test case
  (test-case-error-message "Повідомлення про помилку")

  (test-case-menu-title "Набір тестів")
  (test-case-switch-to-error-box "Перейти до вікна помилкових тестів")
  (test-case-switch-to-nonerror-box "Перейти до вікна безпомилкивих тестів")
  (test-case-collapse "Згорнути набір тестів")
  (test-case-show-actual "Показати фактичне значення")
  (test-case-enable "Дозволити набір тестів")
  (test-case-show-predicate "Показати предикат")
  (test-case-show-error-message "Показати повідомлення про помилку")
  (test-case-convert-to-text "Перетворити в текст")
  
  ;; Profj Boxes
  (profjBoxes-empty-error "Не задані дії")
  (profjBoxes-too-many-expressions-error "Забагато виразів")
  (profjBoxes-interactions-label "Дії")
  (profjBoxes-bad-java-id-error "Невірний Java ID")
  (profjBoxes-examples-label "Зразки")
  (profjBoxes-add-new-example-button "Додати новий зразок")
  (profjBoxes-type "Тип")
  ;; The Java identifier of an example of data
  (profjBoxes-name "Ім'я")
  (profjBoxes-value "Значення")
  (profjBoxes-insert-java-examples "Вставити зразки Java")
  (profjBoxes-insert-java-interactions "Вставити Java-код")

  ;; Slideshow
  (slideshow-hide-picts "Показати вкладені поля")
  (slideshow-show-picts "Показати малюнки")
  (slideshow-cannot-show-picts "Неможливо відобразити малюнки; запустіть програму для кешування розмірів")
  (slideshow-insert-pict-box "Вставити блок малюнку") 

  ;; GUI Tool
  (gui-tool-heading "Інструмент графічного інтерфейсу користувача")
  (gui-tool-before-clicking-message "Перед обранням іконки на панелі використайте \"Вставити елемент графічного інтерфейсу користувача\" з меню \"Спеціальне вставлення\" в кореневий елемент графічного інтерфейсу користувача або оберіть уже вставлений елемент.")
  (gui-tool-show-gui-toolbar "Показати панель інструментів графічного інтерфейсу користувача")
  (gui-tool-hide-gui-toolbar "Сховати панель інструментів графічного інтерфейсу користувача")
  (gui-tool-insert-gui "Вставити елемент графічного інтерфейсу користувача")

  ;; contract violation tracking
  
  ; tooltip for new planet icon in drscheme window (must have a planet violation logged to see it)
  (show-planet-contract-violations "Показати порушення від PLaneT")

  ; buttons in the dialog that lists the recorded bug reports
  (bug-track-report "Файл звіту")
  (bug-track-forget "Не опрацьовувати")
  (bug-track-forget-all "Все проігнорувати")
    
  ;; planet status messages in the bottom of the drscheme window; the ~a is filled with the name of the package
  (planet-downloading "PLaneT: завантаження ~a...")
  (planet-installing "PLaneT: інсталяція ~a...")
  (planet-finished "PLaneT: закінчена з ~a.")
  (planet-no-status "PLaneT") ;; this can happen when there is status shown in a different and then the user switches to a tab where planet hasn't been used
  
  ;; string normalization. To see this, paste some text with a ligature into DrRacket
  ;; the first three strings are in the dialog that appears. The last one is in the preferences dialog
  (normalize "Нормалізувати")
  (leave-alone "Залишити без змін")
  (normalize-string-info "Вставлений рядок, містить лігатури й інші ненормалізовані знаки. Нормалізувати їх?")
  (normalize-string-preference "Нормалізувати вставлені рядки")
  (ask-about-normalizing-strings "Запитувати про нормалізацію рядків")

  )
