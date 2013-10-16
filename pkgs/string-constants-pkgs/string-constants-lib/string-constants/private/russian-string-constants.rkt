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

(module russian-string-constants "string-constant-lang.rkt"
 ;;; when translating this constant, substitute name of actual language for `English'
 (is-this-your-native-language "Русский - это Ваш родной язык?")

 (are-you-sure-you-want-to-switch-languages
  "Смена языка интерфейса пользователя потребует перезапустить DrRacket. Вы уверены, что действительно хотите этого?")

 (interact-with-drscheme-in-language "Работать с русским интерфейсом DrRacket")

 ;; these two should probably be the same in all languages excepet English.
 ;; they are the button labels (under macos and windows, respectively)
 ;; that go the with the string above.
 (accept-and-quit "Применить и выйти")
 (accept-and-exit "Применить и выйти")
 
 ;;; general purpose (DrRacket is hereby a word in every language, by decree of Robby :)
 (plt "PLT")
 (drscheme "DrRacket")
 (drracket "DrRacket")
 (ok "OK")
 (cancel "Отмена")
 (abort "Отмена")
 (untitled "Безымянный")
 (untitled-n "Безымянный ~a")
 (warning "Предупреждение")
 (error "Ошибка")
 (close "Закрыть") ;; as in, close an open window. must match close-menu-item
                 ;; in the sense that, when the &s have been stripped from
                 ;; close-menu-item, it must be the same string as this.
 (stop "Остановить")   
 (&stop "&Остановить") ;; for use in button and menu item labels, with short cut.
 (are-you-sure-delete? "Вы действительно хотите удалить ~a?") ;; ~a is a filename or directory name
 (ignore "Игнорировать")
 (revert "Загрузить заново")

 ;; label for a generic check box, often supported on dialogs
 ;; that ask a binary choice of the user. If checked, the
 ;; dialog isn't going to be shown again.
 ;; One version for always using the current choice:
 (dont-ask-again-always-current "Не переспрашивать (всегда использовать текущий выбор)")
 ;; One generic version (ie, on the Quit DrRacket dialog)
 (dont-ask-again                "Не переспрашивать")

 ;;; important urls
 (web-materials "Связанные Web-сайты") ;; menu item title
 (tool-web-sites "Web-сайты установленных инструментов")   ;; menu item title
 (plt-homepage "Racket")
 (pbd-homepage "Program by Design")

 ;;; bug report form
 (cancel-bug-report? "Отменить отправку отчета об ошибках?")
 (are-you-sure-cancel-bug-report?
  "Вы действительно хотите отменить отправку отчета об ошибках?")
 (bug-report-form "Отчет об ошибках")
 (bug-report-field-name "Имя")
 (bug-report-field-email "E-mail")
 (bug-report-field-summary "Резюме")
 (bug-report-field-severity "Серьезность")
 (bug-report-field-class "Класс")
 (bug-report-field-description "Описание")
 (bug-report-field-reproduce1 "Последовательность действий")
 (bug-report-field-reproduce2 "для воспроизведения ошибки")
 (bug-report-field-environment "Среда")
 (bug-report-field-docs-installed "Установленная документация")
 (bug-report-field-collections "Коллекция")
 (bug-report-field-human-language "Язык интерфейса")
  (bug-report-field-memory-use "Используемая память")
 (bug-report-field-version "Версия")
 (bug-report-synthesized-information "Собранные данные")  ;; dialog title
 (bug-report-show-synthesized-info "Показать собранные данные")
 (bug-report-submit "Отправить")
 (bug-report-submit-menu-item "Отправить отчет об ошибке...") ;; in Help Menu (drs & help desk)
 (error-sending-bug-report "Сбой при отправке отчета об ошибке")
 (error-sending-bug-report-expln "При отправке отчета об ошибке произошел сбой. При наличии работающего подключения к Internet посетите сайт:\n\n    http://bugs.racket-lang.org/\n\nи отправьте отчет об ошибке через Web-форму на нем. Извините за неудобства.\n\nСообщение об ошибке:\n~a")
 (illegal-bug-report "Некорректный отчет об ошибке")
 (pls-fill-in-field "Пожалуйста, заполните поле \"~a\"")
 (malformed-email-address "Некорректный адрес электронной почты")
 (pls-fill-in-either-description-or-reproduce "Пожалуйста, заполните поле \"Описание\" либо \"Последовательность действий для воспроизведения ошибки\".")

 ;;; check syntax
 (check-syntax "Проверить синтаксис")
 (cs-italic "Курсив")
 (cs-bold "Полужирный")
 (cs-underline "Подчеркнутый")
 (cs-change-color "Изменить цвет")
 (cs-tack/untack-arrow "Соединять/не соединять стрелками")
 (cs-jump-to-next-bound-occurrence "Перейти к следующему вхождению")
 (cs-jump-to-binding "Перейти к определению")
 (cs-jump-to-definition "Перейти к объявлению")
 (cs-error-message "Сообщение об ошибке")
 (cs-open-file "Открыть ~a")
 (cs-rename-var "Переименовать ~a")
 (cs-rename-id "Переименовать идентификатор")
 (cs-rename-var-to "Переименовать ~a в:")
 (cs-name-duplication-error "Выбранное имя ~s конфликтует с ранее определенным.")
 (cs-rename-anyway "Переименовать безоговорочно")
 (cs-status-init "Проверка синтаксиса: инициализация окружения для пользовательского кода")
 (cs-status-coloring-program "Проверка синтаксиса: цветовыделение выражения")
 (cs-status-eval-compile-time "Проверка синтаксиса: вычисление времени компиляции")
 (cs-status-expanding-expression "Проверка синтаксиса: развернуть выражение")
 (cs-status-loading-docs-index "Проверка синтаксиса: загрузка индекса документации")
 (cs-mouse-over-import "Привязка ~s импортирована из ~s")
 (cs-view-docs "Просмотр документации для ~a")
 (cs-view-docs-from "~a из ~a")  ;; a completed version of the line above (cs-view-docs) is put into the first ~a and a list of modules (separated by commas) is put into the second ~a. Use check syntax and right-click on a documented variable (eg, 'require') to see this in use  

 (cs-lexical-variable "лексическая переменная")
 (cs-set!d-variable "переопределенная переменная")
 (cs-imported-variable "импортированная переменная")

 ;;; info bar at botttom of drscheme frame
 (collect-button-label "Cборка мусора")
  (read-only "Только для чтения")
 (auto-extend-selection "Авторасширение")
 (overwrite "Замена")
 (running "выполняется")
 (not-running "не выполняется")
 
 ;;; misc
 (welcome-to-something "Добро пожаловать в ~a")
 
 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "Добро пожаловать в DrRacket, версия ~a, ~a")

 ; these appear on subsequent lines in the `Help|Добро пожаловать в DrRacket' dialog.
 (welcome-to-drscheme "Добро пожаловать в DrRacket")

 (goto-line "Перейти к строке")
 (goto-line-invalid-number
  "~a - некорректный номер строки. Номер должен быть целым числом в диапазоне от 1 до ~a")
 (goto-position "Перейти к позиции")
 (no-full-name-since-not-saved
  "У файла нет полного имени - он еще не был сохранен.")
 (cannot-open-because-dne "Невозможно открыть ~a, потому что он не существует.")

  (needs-execute-language-changed
   "Предупреждение: язык изменился. Нажмите Выполнить.")
  (needs-execute-teachpack-changed
   "Предупреждение: учебный пакет изменился. Нажмите Выполнить.")
  (needs-execute-defns-edited
   "Предупреждение: окно определений изменилось. Нажмите Выполнить.")

  (editor-changed-since-srcloc-recorded
   "С момента последней фиксации местоположения файла были внесены изменения, поэтому выделенная область может содержать неверное местоположение файла.")
  
 (file-is-not-saved "Файл \"~a\" не сохранен.")
 (save "Сохранить")
 (close-anyway "Все равно закрыть")
 (dont-save "Не сохранять")
 (clear-anyway "Все равно очистить")

 ;; menu item title
 (log-definitions-and-interactions "Сохранить окна определения и интерпретатора...")
 (stop-logging "Остановить протоколирование")
 (please-choose-a-log-directory "Пожалуйста, выберите каталог")
 (logging-to "Протоколировать в: ")
 (erase-log-directory-contents "Удалить содержимое каталога: ~a?")
 (error-erasing-log-directory "Ошибка удаления содержимого каталога.\n\n~a\n")

  ;; menu items connected to the logger -- also in a button in the planet status line in the drs frame
  (show-log "Показать &протокол")
  (hide-log "Скрыть &протокол")
  (logging-all "Все") ;; in the logging window in drscheme, shows all logs simultaneously

 ;; modes
 (mode-submenu-label "Режимы")
 (scheme-mode "Режим Scheme")
 (racket-mode "Режим Racket")
 (text-mode "Режим текста")

 (scheme-mode-color-symbol "Символ")
 (scheme-mode-color-keyword "Ключевое слово")
 (scheme-mode-color-comment "Комментарий")
 (scheme-mode-color-string "Строка")
 (scheme-mode-color-constant "Константа")
 (scheme-mode-color-parenthesis "Круглые скобки")
 (scheme-mode-color-error "Ошибка")
 (scheme-mode-color-other "Другие")
 ;; the ~a is filled in with one of the above (scheme-mode-*)
 (syntax-coloring-choose-color "Выберите цвет для ~a")
 (preferences-colors "Цвета") ;; used in the preferences dialog
 
  ;; parenthesis color scheme string constants
  (parenthesis-color-scheme "Выделение цветом круглых скобок") ;; label for the choice% menu in the preferences dialog
  (paren-color-basic-grey "Основной серый")
  (paren-color-shades-of-gray "Оттенки серого цвета")
  (paren-color-shades-of-blue "Оттенки голубого цвета")
  (paren-color-spring "Теплые оттенки")
  (paren-color-fall "Осенние оттенки")
  (paren-color-winter "Холодные оттенки")

  
 (url: "URL:")
 (open-url... "Открыть URL...")
 (open-url "Открыть URL")
 (browse... "Браузер...")
 (bad-url "Неверный URL")
 (bad-url:this "Неверный URL: ~a")
 
 ;; Help Desk
 (help "Справка")
 (help-desk "Помощь")
 (plt:hd:search "Поиск")
 (plt:hd:feeling-lucky "Мне повезёт")
 (plt:hd:home "Домашняя страница справочного бюро") 
 ; next 3 are popup menu choices in help desk search frame
 (plt:hd:search-for-keyword "Искать в ключевых словах")
 (plt:hd:search-for-keyword-or-index "Искать в ключевых словах или в содержании")
 (plt:hd:search-for-keyword-or-index-or-text "Искать в ключевых словах, в содержании или тексте")
 (plt:hd:exact-match "Точное соответствие")
 (plt:hd:containing-match "Содержится упоминание")
 (plt:hd:regexp-match "Соответствие по регулярному выражению")
 (plt:hd:find-docs-for "Поиск документации по:")
 (plt:hd:search-stopped-too-many-matches "[Поиск прервался: слишком много совпадений]")
 (plt:hd:nothing-found-for "Для ~a не найдено")
 (plt:hd:and "и")
 (plt:hd:refresh "обновить")
 (plt:hd:refresh-all-manuals "обновить все руководства")
 (plt:hd:manual-installed-date "(инсталлировать ~a)")
 ; Help Desk configuration
 ;; refreshing manuals
 (plt:hd:refreshing-manuals "Перезагрузить руководства")
 (plt:hd:refresh-downloading... "Загрузить ~a...")
 (plt:hd:refresh-deleting... "Удалить старую версию ~a...")
 (plt:hd:refresh-installing... "Инсталировать новую версию ~a...")
 (plt:hd:refresh-clearing-indices "Очистить кешированные индексы")
 (plt:hd:refreshing-manuals-finished "Готово.")
 (plt:hd:about-help-desk "О помощи")
 (plt:hd:help-desk-about-string
  "Помощь - это полный источник информации о программном обеспечении PLT, включая DrRacket, MzScheme и MrEd.\n\nВерсия ~a\nCopyright (c) ~a-~a PLT")
 (plt:hd:help-on-help "Справка по справке")
 (plt:hd:help-on-help-details "Для получения справки по использованию помощи, выберите первую ссылку `Помощь' на её домашней странице. (Для перехода на домашнюю страницу нажмите кнопку `Домой' в верхней части окна помощи.)")
  (reload "Обновить") ;; refresh the page in a web browser
  (plt:hd:ask-about-separate-browser
   "Вы выбрали ссылку на данные из WWW. Желаете ли Вы просмотреть их в браузере помощи  или в окне внешнего браузера?")
  (plt:hd:homebrew-browser "Браузер помощи") ;; choice for the above string (in a button)
  (plt:hd:separate-browser "Внешний браузер") ;; other choice for the above string (also in a button)
  (plt:hd:external-link-in-help "Внешние ссылки в справке")
  (plt:hd:use-homebrew-browser "Использовать браузер помощи для внешних ссылок")
  (plt:hd:new-help-desk "Новое окно помощи")

  ;; in the Help Desk language dialog, title on the right.
  (plt:hd:manual-search-ordering "Руководство по поиску")
  
  ;; in the help-desk standalone font preference dialog, on a check box
  (use-drscheme-font-size "Использовать размер шрифта DrRacket")
  
  ;; in the preferences dialog in drscheme there is example text for help desk font size.
  ;; clicking the links in that text produces a dialog with this message
  (help-desk-this-is-just-example-text
   "Это пример текста для установки размера шрифта. Откройте соответствующую помощь (из меню Справка) для перехода по этим ссылкам.")

  ;; this appears in the bottom part of the frame the first time the user hits `f1' 
  ;; (assuming nothing else has loaded the documentation index first)
  ;; see also: cs-status-loading-docs-index
  (help-desk-loading-documentation-index "Помощь: загрузка индекса документации")
  
 ;; Help desk htty proxy
 (http-proxy "HTTP-прокси")
 (proxy-direct-connection "Прямое подключение")
 (proxy-use-proxy "Использовать прокси:")
 (proxy-host "Узел")
 (proxy-port "Порт")
 (proxy-bad-host "Неверный прокси-узел")

 ;; browser
 (rewind-in-browser-history "Назад")
 (forward-in-browser-history "Вперед")
 (home "В начало")
 (browser "Браузер")
 (external-browser-choice-title "Внешний браузер") ; title for radio-button set
 (browser-command-line-label "Командная строка:") ; label for radio button that is followed by text boxes
 (choose-browser "Выбрать браузер")
 (no-browser "Выбрать позже")
 (browser-cmdline-expl-line-1 "(Командная строка сформирована соединением  предшествующего текста, URL,") ; explanatory text for dialog, line 1
 (browser-cmdline-expl-line-2 "и последующего текста.)") ; ... line 2. (Anyone need more lines?)
 (install? "Инсталлировать?")  ;; if a .plt file is found (title of dialog)
 (you-have-selected-an-installable-package "Вы выбрали устанавливаемый пакет.")
 (do-you-want-to-install-it? "Вы хотите инсталлировать его?")
 (paren-file-size "(Размер файла- ~a байт)")
 (download-and-install "Загрузить и инсталлировать") ;; button label
 (download "Загрузить") ;; button label
 (save-downloaded-file/size "Сохранить загруженные файлы (~a байт) как") ;; label for get-file dialog
 (save-downloaded-file "Сохранить загруженные файлы как")  ;; label for get-file dialog
 (downloading "Загрузка") ;; dialog title
 (downloading-file... "Загрузка файла...")
 (package-was-installed "Пакет инсталлирован.")
 (download-was-saved "Загруженные файлы сохранены.")

 (install-plt-file-menu-item... "Инсталлировать .plt-файл...")
 (install-plt-file-dialog-title "Инсталлировать .plt-файл")
 (install-plt-web-tab "Web")
 (install-plt-file-tab "Файл")
 (install-plt-filename "Имя файла:")
 (install-plt-url "URL:")
 
 ;; install plt file when opened in drscheme strings
 (install-plt-file "Инсталлировать ~a или открыть для редактирования?")
 (install-plt-file/yes "Инсталлировать")
 (install-plt-file/no "Редактировать")

 (plt-installer-progress-window-title "Ход инсталляции") ;; frame title
 (plt-installer-abort-installation "Процес инсталляции прерван") ;; button label
 (plt-installer-aborted "Прервано.") ;; msg that appears in the installation window when installation is aborted

 ;;; about box
 (about-drscheme-frame-title "О DrRacket")
 
 ;;; save file in particular format prompting.
 (save-as-plain-text "Сохранить этот файл в обычном текcтовом формате?")
 (save-in-drs-format "Сохранить этот файл в специальном нетекстовом формате drscheme?")
 (yes "Да")     
 (no "Нет")
 
 ;; saving image (right click on an image to see the text)
  (save-image "Сохранить рисунок...")
  
 ;;; preferences
 (preferences "Настройки пользователя")
 (error-saving-preferences "Ошибка сохранения настроек пользователя: ~a")
 (error-saving-preferences-title "Ошибка сохранения настроек пользователя")
 (steal-the-lock-and-retry "Снять блокировку и повторить") ;; in the preferences error dialog; this happens when the lockfile exists (after 3 pref writes). 
 (error-reading-preferences "Ошибка чтения настроек пользователя")
 (prefs-file-locked "Файл настроек пользователя заблокирован (так как файл ~a существует), поэтому измененные настройки не будут сохранены. Отменить изменения?")
 (try-again "Попытаться снова") ;; button label
 (prefs-file-still-locked "Файл настроек пользователя все еще заблокирован (так как файл ~a существует), поэтому изменения настроек не будут сохранены..")
 (scheme-prefs-panel-label "Racket")
 (warnings-prefs-panel-label "Предупреждения")
 (editor-prefs-panel-label "Редактирование")
 (general-prefs-panel-label "Общее")
 (highlight-parens "Подсвечивать парные скобки")
 (fixup-open-brackets "Автокорекция открывающихся квадратных скобок")
 (fixup-close-parens "Автокорекция закрывающихся скобок")
 (flash-paren-match "Отображать соответствие скобок")
 (auto-save-files "Автосохранение файлов")
 (backup-files "Резервные копии файлов")
 (map-delete-to-backspace "Назначить Delete на Backspace")
 (verify-exit "Проверять завершение")
 (ask-before-changing-format "Спрашивать перед изменением формата")
 (wrap-words-in-editor-buffers "Переносить слова в буферах редактора")
 (show-status-line "Показать строку состояния")
 (count-columns-from-one "Считать номера столбцов, начиная с 1")
 (display-line-numbers "Отображать номера строк в буфере (нет смещения символов)")
 (show-line-and-column-numbers "Показывать номера строк и столбцов") ; used for popup menu; right click on line/column box in bottom of drs window
 (show-character-offsets "Показывать смещение символов") ; used for popup menu; right click on line/column box in bottom of drs window
 (enable-keybindings-in-menus "Разрешить сочетания клавиш в меню")
 (command-as-meta "Считать командную клавишу мета-клавишей") ;; macos/macos x only
 (reuse-existing-frames "Использовать существующие окна при открытии новых файлов")
 (default-fonts "Шрифты по умолчанию")
 (basic-gray-paren-match-color "Выделять парные скобки серым цветом") ; in prefs dialog
 (online-coloring-active "Интерактивная подсветка синтаксиса")
 (open-files-in-tabs "Открывать файлы в отдельных вкладках (не отдельных окнах)")
 (show-interactions-on-execute "Автоматически открывать окно интерпретатора при запуске программы")
  (switch-to-module-language-automatically "Автоматически переключаться на язык модуля при открытии модуля")
  (interactions-beside-definitions "Размещать окно интерпретатора возле окна определений") ;; in preferences, below the checkbox one line above this one
 (limit-interactions-size "Ограничить размер программы")
 (background-color "Цвет фона")
 (default-text-color "Цвет текста по умолчанию") ;; used for configuring colors, but doesn't need the word "color"
 (choose-a-background-color "Пожалуйста, выберите цвет фона")
 (revert-to-defaults "Вернуться к значению по умолчанию")
  
  (black-on-white-color-scheme "Черный на белом") ;; these two appear in the color preferences dialog on butttons
  (white-on-black-color-scheme "Белый на черном") ;; clicking the buttons changes teh color schemes to some defaults that've been set up.
  
 ; title of the color choosing dialog

 ; should have entire alphabet
 (font-example-string "Быстрая коричневая лиса перепрыгнула через ленивых собак.") 

 (change-font-button-label "Изменить")
 (fonts "Шрифты")
 (other... "Другой...") ;; used in the font choice menu item

 ; filled with type of font, eg modern, swiss, etc.
 (choose-a-new-font "Пожалуйста, выберите новый \"~a\" шрифт")

 (font-size-slider-label "Размер")
 (restart-to-see-font-changes "Перезапустите, чтобы увидеть смену типа шрифта")

 (font-prefs-panel-title "Шрифт")
 (font-name "Имя шрифта")
 (font-size "Размер шрифта")
 (set-font "Установить шрифт...")
 (font-smoothing-label  "Сглаживание шрифтов")
 (font-smoothing-none "Отсутствует")
 (font-smoothing-some "Для некоторых")
 (font-smoothing-all "Для всех")
 (font-smoothing-default "Использовать системные установки")
 (select-font-name "Выбор имени шрифта")
 (example-text "Пример текста:")
 (only-warn-once "Это предупреждение о том, что определения и вызовы не синхронизированы")
 
 ; warning message when lockfile is around
 (waiting-for-pref-lock "Ожидание завершения блокировки настроек пользователя...")
 (pref-lock-not-gone
  "Файл блокировки настроек пользователя: \n\n   ~a\n\nне позволяет сохранить изменения настроек. Убедитесь в отсутствии работающего програмного обеспечения Racket и удалите этот файл.")
 (still-locked-exit-anyway? "Настройки не сохранены. Все равно выйти?")
 
 ;;; indenting preferences panel
 (indenting-prefs-panel-label "Выравнивание")
 (indenting-prefs-extra-regexp "Дополнительные регулярные выражения")

 (square-bracket-prefs-panel-label "Квадратные скобки")
  
 ; filled with define, lambda, or begin
 (enter-new-keyword "Введите новое ~a-подобное ключевое слово:")
 (x-keyword "~a ключевое слово")
 (x-like-keywords "~a-подобное ключевое слово")

 ; used in Square bracket panel
 (skip-subexpressions "Количество подвыражений, которые будут пропущены")

 (expected-a-symbol "ожидается символ,  найдено: ~a")
 (already-used-keyword "\"~a\" -уже используется как ключевое слово")
 (add-keyword "Добавить")
 (remove-keyword "Удалить")
 
  ; repl color preferences
  (repl-colors "REPL")
  (repl-out-color "Вывод")
  (repl-value-color "Значения")
  (repl-error-color "Ошибки")
  
  ;;; find/replace
  (search-next "Далее")
  (search-previous "Назад")
  (search-match "Совпадение")  ;;; this one and the next one are singular/plural variants of each other
  (search-matches "Совпадения") 
  (search-replace "Заменить")
  (search-skip "Пропустить")
  (search-show-replace "Показать замены")
  (search-hide-replace "Скрыть замены")
  (find-case-sensitive "Учитывать регистр")  ;; the check box in both the docked & undocked search
  (find-anchor-based "Искать, используя привязки")

  ;; these string constants used to be used by searching,
  ;; but aren't anymore. They are still used by other tools, tho.
  (hide "Скрыть")
  (dock "Прикрепить")
  (undock "Открепить")
  
 ;;; multi-file-search
 (mfs-multi-file-search-menu-item "Поиск в файлах...")
 (mfs-string-match/graphics "Соответствие строк (обрабатывать файлы с графикой)")
 (mfs-regexp-match/no-graphics "Регулярные выражения (только для неформатированных текстовых файлов)")
 (mfs-searching... "Поиск...")
 (mfs-configure-search "Настройки поиска") ;; dialog title
 (mfs-files-section "Файлы")   ;; section in config dialog
 (mfs-search-section "Поиск") ;; section in config dialog
 (mfs-dir "Каталог")
 (mfs-recur-over-subdirectories "Искать во вложенных каталогах")
 (mfs-regexp-filename-filter "Фильтровать имена файлов по регулярных выражениях")
 (mfs-search-string "Искать строки")
 (mfs-drscheme-multi-file-search "многофайловый поиск - DrRacket") ;; results window and error message title
 (mfs-not-a-dir "\"~a\" не каталог")
 (mfs-open-file "Открыть файл")
 (mfs-stop-search "Остановить поиск")
 (mfs-case-sensitive-label "Учитывать регистр")
 (mfs-no-matches-found "Совпадения не найдены.")
 (mfs-search-interrupted "Поиск прерван.")
 
 ;;; reverting a file
 (are-you-sure-revert
  "Вы действительно уверены, что хотите вернуться к предыдущей версии файла? Внесенные изменения невозможно будет восстановить.")
 (are-you-sure-revert-title
  "Открыть заново?")
 
 ;;; saving a file
 ; ~a is filled with the filename
 (error-saving "Ошибка при сохранении") ;; title of error message dialog
 (error-saving-file/name "При сохранении произошла ошибка ~a.")
 (error-loading "Ошибка открытия")
 (error-loading-file/name "При открытии произошла ошибка ~a.")
 (unknown-filename "<< неизвестный >>")

 ;;; finder dialog
 (must-specify-a-filename "Определите имя файла")
 (file-does-not-exist "Файл \"~a\" не существует.")
 (ask-because-file-exists "Файл \"~a\" уже существует. Заменить его?")
 (dne-or-cycle "Файл \"~a\" содержит несуществующий каталог или цикл.")
 (get-file "Получить файл")
 (put-file "Поместить файл")
 (full-pathname "Полное имя")
 (show-dot-files "Показать файлы и каталоги, которые начинаются с точки.")
 (up-directory-button-label "В предыдущий каталог")
 (add-button-label "Добавить") ;;; for multi-file selection
 (add-all-button-label "Добавить все") ;;; for multi-file selection
 (remove-button-label "Удалить") ;;; for multi-file selection
 (file-wrong-form "Это имя Файла имеет неправильную форму.")
 (select-files "Выбор файлов")
 (select-file "Выбор файла")
 (dir-dne "Каталог не существует.")
 (file-dne "Файл не существует.")
 (empty-filename "Имя файла должно содержать символы.")
 (that-is-dir-name "Это имя каталога.")
 
 ;;; raw menu names -- these must match the 
 ;;; versions below, once the &s have been stripped.
 ;;; if they don't, DrRacket's menus will appear
 ;;; in the wrong order.
 (file-menu "Файл")
 (edit-menu "Правка")
 (help-menu "Справка")
 (windows-menu "Окна")
 
 ;;; menus
 ;;; - in menu labels, the & indicates a alt-key based shortcut.
 ;;; - sometimes, things are stuck in the middle of 
 ;;; menu item labels. For instance, in the case of
 ;;; the "Сохранить как" menu, you might see: "Сохранить определения как". 
 ;;; be careful of spacing, follow the English, if possible.
 ;;; - the ellipses in the `after' strings indicates that
 ;;; more information is required from the user before completing
 ;;; the command.

 (file-menu-label "&Файл")

 (new-info  "Открыть новый файл")
 (new-menu-item "&Новый")
 (new-...-menu-item "&Новый...")

 (open-info "Открыть файл с диска")
 (open-menu-item "&Открыть...")

 (open-recent-info "Список последних открытых файлов")
 (open-recent-menu-item "Открыть &последние")

 (revert-info "Заменить файл сохраненным на диске")
 (revert-menu-item "Пе&реоткрыть")

 (save-info "Сохранить этот файл на диск")
 (save-menu-item "&Сохранить")

 (save-as-info "Указать имя файла и сохранить этот файл на диск")
 (save-as-menu-item "Сохранить &как...")

 (print-info "Отправить файл на печать")
 (print-menu-item "&Печать...")

 (page-setup-info "Выбор конфигурации параметров печати")
 (page-setup-menu-item "Параметры страницы...")
 
 (close-info "Закрыть файл")
 (close-menu-item "&Закрыть")

 (quit-info "Закрыть все окна")
 (quit-menu-item-windows "&Выход")
 (quit-menu-item-others "&Выход")
 
 (edit-menu-label "&Правка")
 
 (undo-info "Отменить последнее действие")
 (undo-menu-item "О&тменить")

 (redo-info "Вернуть последнее действие")
 (redo-menu-item "Вер&нуть")

 (cut-info "Переместить выбранные элементы в буфер обмена для дальнейшей вставки")
 (cut-menu-item "Вы&резать")

 (copy-info "Копировать выбранные элементы в буфер обмена для дальнейшей вставки")
 (copy-menu-item "&Копировать")

 (paste-info "Вставить последний скопированный или вырезанный элемент вместо выбранных элементов")
 (paste-menu-item "Вс&тавить")

 (clear-info "Удалить выделенные элементы без изменения буфера или вставки")
 (clear-menu-item-windows "&Удалить")

 (select-all-info "Выделить весь документ")
 (select-all-menu-item "Выделить вс&е")
 
  (find-menu-item "Найти") ;; menu item
  (find-info "Переключает клавиатурный фокус между окном, в котором он выполняется, и панелью поиска")
  
 (find-next-info "Найти следующее вхождение")
 (find-next-menu-item "Найти далее")
  
 (find-previous-info "Найти предыдущее вхождение")
 (find-previous-menu-item "Найти предыдущее")
  
  (show-replace-menu-item "Показать замены")
  (hide-replace-menu-item "Скрыть замены")
  (show/hide-replace-info "Переключить видимость панели замены")

  (replace-menu-item "Заменить")
  (replace-info "Заменить искомое")
  
  (replace-all-info "Заменить все вхождения")
  (replace-all-menu-item "Заменить все")
  
  (find-case-sensitive-info "Переключиться между зависящим и независящим от регистра поиском")
  (find-case-sensitive-menu-item "Искать с учетом регистра")
  
  (complete-word "Завершения слов") ; the complete word menu item in the edit menu
  (no-completions "... завершения недоступны") ; shows up in the completions menu when there are no completions (in italics)

  (overwrite-mode "Режим замены")
  (enable-overwrite-mode-keybindings "Разрешить переключение в режим замены сочетанием клавиш")  
  
 (preferences-info "Конфигурация Ваших настроек")
 (preferences-menu-item "Настройки пользователя...")

 (keybindings-info "Показать активные сочетания клавиш")
 (keybindings-menu-item "Сочетания клавиш")
 (keybindings-show-active "Показать активные сочетания клавиш")
 (keybindings-frame-title "Сочетания клавиш")
 (keybindings-sort-by-name "Сортировать по имени")
 (keybindings-sort-by-key "Сортировать по ключу")
 (keybindings-add-user-defined-keybindings "Добавить определенные пользователем сочетания клавиш...")
 (keybindings-add-user-defined-keybindings/planet "Добавить определенные пользователем сочетания клавиш из PLaneT...")
 (keybindings-menu-remove "Удалить ~a")
 (keybindings-choose-user-defined-file "Пожалуйста, выберите файл, содержащий сочетания клавиш.")
 (keybindings-planet-malformed-spec "Неверные настройки PLaneT: ~a") ; the string will be what the user typed in
 (keybindings-type-planet-spec "Пожалуйста, введите необходимые настройки PLaneT")
  
 ; first ~a will be a string naming the file or planet package where the keybindings come from;
 ; second ~a will be an error message
 (keybindings-error-installing-file "Ошибка при инсталляции сочетаний клавиш ~a:\n\n~a")
  
 (user-defined-keybinding-error "Ошибка выполнения сочетания клавиш ~a\n\n~a")
 (user-defined-keybinding-malformed-file "Файл ~a не содержит модуля, написанного на языке оболочки/сочетаний клавиш.")  
 (user-defined-keybinding-malformed-file/found-lang "Файл ~a не содержит модуля, написанного на языке оболочки/сочетаний клавиш. Найден язык ~s")   
 
 ;; menu items in the "special" menu
 (insert-text-box-item "Вставить текстовый блок")
 (insert-image-item "Вставить рисунок...")
 (insert-comment-box-menu-item-label "Вставить комментарий")
 (insert-lambda "Вставить λ")

 (wrap-text-item "Переносить текст")

  ;; windows menu
 (windows-menu-label "О&кна")
 (minimize "Свернуть") ;; minimize and zoom are only used under mac os x
 (zoom "Изменить размер")
 (bring-frame-to-front "Показать поверх всех окон")       ;;; title of dialog
 (bring-frame-to-front... "Показать поверх всех окон...") ;;; corresponding title of menu item
 (most-recent-window "Последнее окно")
  (next-tab "Следующая вкладка")
  (prev-tab "Предыдущая вкладка")

 (view-menu-label "В&ид")
 (show-overview "Показать Ко&нтур программы") 
 (hide-overview "Скрыть Ко&нтур программы")
 (show-module-browser "Показать &Браузер модулей")
 (hide-module-browser "Скрыть &Браузер модулей")

  (help-menu-label "&Справка")
 (about-info "Сведения об авторах и подробностях применения программы")
 (about-menu-item "О программе...")
 
 ;; open here's new menu item
 (create-new-window-or-clear-current
  "Хотите ли Вы создать новое окно или очистить текущее?")
 (clear-current "Очистить текущее")
 (new-window "Новое окно")

 ;;; exiting and quitting ``are you sure'' dialog
 ;;; exit is used on windows, quit on macos, in English. Other
 ;;; languages probably use the same word on both platforms.
 (exit "Выход")
 (quit "Выход")
 (are-you-sure-exit "Вы уверены, что хотите выйти?")
 (are-you-sure-quit "Вы уверены, что хотите выйти?")
  ; these next two are only used in the quit/exit dialog
  ; on the button whose semantics is "dismiss this dialog".
  ; they are there to provide more flexibility for translations
  ; in English, they are just cancel.
 (dont-exit "Отмена") 
 (dont-quit "Отмена")
  
 ;;; autosaving
 (error-autosaving "Ошибка автосохранения \"~a\".") ;; ~a will be a filename
 (autosaving-turned-off "Автосохранение отключено\nво время записи файла.")
 (recover-autosave-files-frame-title "Восстановить автосохраненные файлы")
 (autosave-details "Подробности")
 (autosave-recover "Восстановлен")
 (autosave-unknown-filename "<<неизвестный>>")
  
  ;; these are labels in a dialog that drscheme displays
  ;; if you have leftover autosave files. to see the dialog,
  ;; start up drscheme and modify (but don't save) a file
  ;; (also, do this with an unsaved file). Wait for the autosave
  ;; files to appear (typically 5 minutes). Kill DrRacket
  ;; and restart it. You'll see the dialog
  (autosave-autosave-label: "Автосохранение файла:")
  (autosave-original-label: "Исходный файл:")
  (autosave-autosave-label "Автосохранение файла")
  (autosave-original-label "Исходный файл")
  (autosave-compare-files "Сравнить автосохраненные файлы")

  (autosave-show-autosave "Автосохранение файла") ;; title of a window showing the autosave file

  (autosave-explanation "DrRacket нашел Ваши файлы автосохранения, которые могут содержать несохраненную работу.")

  (autosave-recovered! "Восстановить!") ;; status of an autosave file
  (autosave-deleted "Удалить")       ;; status of an autosave file

  (autosave-error-deleting "Ошибка удаления ~a\n\n~a") ;; first is a filename, second is an error message from mz.
  (autosave-delete-button "Удалить")
  (autosave-delete-title "Удалить")  ;; title of a dialog asking for deletion confirmation
  (autosave-done "Завершено")
  
  ;; appears in the file dialog
  (autosave-restore-to-where? "Выберите место для записи автосохраненного файла.")
  
  
 ;;; file modified warning
 (file-has-been-modified
  "Файл был изменен после последнего автосохранения. Отменить изменения?")
 (overwrite-file-button-label "Перезаписать")
 
 (definitions-modified 
  "Текст файла определений был изменен. Пожалуйста, сохраните или переоткройте определения.")
 (drscheme-internal-error "Внутренняя ошибка DrRacket")
 
 ;;; tools
 (invalid-tool-spec "Некорректный файл info.rkt, содержащий описание инструмента в коллекции ~a. Ожидается строка или непустой список строк, получено: ~e")
 (error-invoking-tool-title "Ошибка вызова инструмента ~s;~s")
 (error-loading-tool-title "Ошибка загрузки инструмента ~s\n~a") ;; ~s filled with a path, ~a filled with an error message from an exn
 (tool-tool-names-same-length
  "В файле info.rkt для ~s ожидается, что `tool-names' и `tools' - списки равной длины, получено ~e и ~e")
 (tool-tool-icons-same-length
  "В файле info.rkt для ~s ожидается, что `tool-icons' и `tools' - списки равной длины, получено ~e и ~e")
 (tool-tool-urls-same-length
  "В файле info.rkt для ~s ожидается, что `tool-urls' и `tools' - списки равной длины, получено ~e и ~e")
 (error-getting-info-tool
  "ошибка при загрузке файла info.rkt для ~s")
 (tool-error-phase1 "Ошибка на 1 этапе для инструмента ~s; ~s")
 (tool-error-phase2 "Ошибка на 2 этапе для инструмента ~s; ~s")


 ;;; define popup menu
 (end-of-buffer-define "<< конец буфера >>")
 (sort-by-name "Упорядочить по имени")
 (sort-by-position "Упорядочить по позиции в файле")
 (no-definitions-found "<< определения не найдены >>")
 (jump-to-defn "Перейти к определению ~a")

 (recent-items-sort-by-age "Упорядочить по времени")
 (recent-items-sort-by-name "Упорядочить по имени")
 
 ;;; view menu
 (hide-definitions-menu-item-label "Скрыть окно о&пределений")
 (show-definitions-menu-item-label "Показать окно о&пределений")
 (definitions-menu-item-help-string "Показать/Скрыть окно определений")
 (show-interactions-menu-item-label "Показать окно &интерпретатора")
 (hide-interactions-menu-item-label "Скрыть окно &интерпретатора")
 (interactions-menu-item-help-string "Показать/Скрыть окно интерпретатора")
 (toolbar "Панель инструментов")
 (toolbar-on-top "Панель инструментов сверху")
 (toolbar-on-left "Панель инструментов слева")
 (toolbar-on-right "Панель инструментов справа")
 (toolbar-hidden "Скрыть панель инструментов")

 ;;; file menu
 (save-definitions-as "Сохранить определения &как...")
 (save-definitions "Сохранить определения")
 (print-definitions "Печать определений...")
 (about-drscheme "О DrRacket")
 (save-other "Другие способы сохранения")
 (save-definitions-as-text "Сохранить определения как текст...")
 (save-interactions "Сохранить окно интерпретатора")
 (save-interactions-as "Сохранить окно интерпретатора как...")
 (save-interactions-as-text "Сохранить окно интерпретатора как текст...")
 (print-interactions "Печатать окно интерпретатора...")
 (new-tab "Новая вкладка")
 (close-tab "Закрыть вкладку") ;; must not have any &s in it.
 (close-tab-amp "&Закрыть вкладку") ;; like close-tab, but with an ampersand on the same letter as the one in close-menu-item
  
 ;;; edit-menu
 (split-menu-item-label "&Разделить")
 (collapse-menu-item-label "&Обьединить")
 
 ;;; language menu
 (language-menu-name "&Язык")
 
 ;;; scheme-menu
 (scheme-menu-name "&Racket")
 (execute-menu-item-label "Выполнить")
 (execute-menu-item-help-string "Перезапустить программу в окне определений")
 (ask-quit-menu-item-label "Прервать программу")
 (ask-quit-menu-item-help-string "Прервать выполняющуюся программу")
 (force-quit-menu-item-label "Принудительно выйти из программы")
 (force-quit-menu-item-help-string "Прервать все текущие вычисления")
 (limit-memory-menu-item-label "Ограничение по памяти...")
 ;(limit-memory-msg-1 "Ограничение станет действовать после следующего запуска программы")
 ;(limit-memory-msg-2 "Для выполнения необходим, по крайней мере, один мегабайт.")  ;; minimum memory limit is now 8 megs
 (limit-memory-unlimited "Без ограничений")
 (limit-memory-limited "С ограничением")
 (limit-memory-megabytes "Мегабайт")
 (clear-error-highlight-menu-item-label "Очистить выделение ошибок")
 (clear-error-highlight-item-help-string "Удалить выделение ошибок")
 (reindent-menu-item-label "&Выровнять")
 (reindent-all-menu-item-label "Выровнять &все")
 (semicolon-comment-out-menu-item-label "&Закомментировать выделенное точкой с запятой")
 (box-comment-out-menu-item-label "&Закомментировать весь блок")
 (uncomment-menu-item-label "&Раскомментировать")

 (convert-to-semicolon-comment "Преобразовать в комментарий с точкой с запятой")
 
 ;;; executables
 (create-executable-menu-item-label "Создать исполняемый файл...")
 (create-executable-title "Создать исполняемый файл")
 (must-save-before-executable "Вы должны сохранить свою программу прежде, чем создать исполняемый файл.")
 (save-a-mred-launcher "Сохранить запускаемым в графической оболочке")
 (save-a-mzscheme-launcher "Сохранить запускаемым в текстовой оболочке")
 (save-a-mred-stand-alone-executable "Сохранить автономный исполняемый файл с графическим интерфейсом")
 (save-a-mzscheme-stand-alone-executable "Сохранить автономный исполняемый файл с текстовым интерфейсом")
 (save-a-mred-distribution "Сохранить дистрибутив с графическим интерфейсом")
 (save-a-mzscheme-distribution "Сохранить дистрибутив с текстовым интерфейсом")

 (definitions-not-saved "Окно определений не было сохранено.  Исполняемая программа будет использовать последнюю сохраненную версию окна определений. Продолжить?")
 ;; The "-explanatory-label" variants are the labels used for the radio buttons in
 ;;  the "Create Executable..." dialog for the "(module ...)" language.
 (launcher "Запуск в оболочке")
 (launcher-explanatory-label "Запуск в оболочке (только для текущего компьютера, выполнение исходного файла)")
 (stand-alone "Автономный")
 (stand-alone-explanatory-label "Автономный (только для текущего компьютера, выполнение скомпилированной копии)")
 (distribution "Дистрибутив")
 (distribution-explanatory-label "Дистрибутив (для установки на других машинах)")
 (executable-type "Тип")
 (executable-base "База")
 (filename "Имя файла: ")
 (create "Создать")
 (please-specify-a-filename "Пожалуйста, определите имя создаваемого файла.")
 (~a-must-end-with-~a
  "Имя ~a файла\n\n  ~a\n\nнеправильное. Имя файла должно заканчиваться \".~a\".")
 (macosx-executables-must-end-with-app
  "Имя файла\n\n  ~a\n\nнеправильное. Для MacOS X исполняемая программа должна быть каталогом, имя которого заканчивается .app.")
 (warning-directory-will-be-replaced
  "Предупреждение: каталог:\n\n  ~a\n\nбудет заменен. Продолжить?")
 
 (distribution-progress-window-title "Создание дистрибутива")
 (creating-executable-progress-status "Создание исполняемых файлов для дистрибутива...")
 (assembling-distribution-files-progress-status "Сборка файлов для дистрибутива...")
 (packing-distribution-progress-status "Упаковка дистрибутива...")

 (create-servlet "Создать сервлет...")

 ; the ~a is a language such as "module" or "algol60"
 (create-servlet-unsupported-language
  "Создание сервлета не поддерживается языком ~a.")
  
 ;;; buttons
 (execute-button-label "Выполнить") 
 (save-button-label "Сохранить")
 (break-button-label "Остановить")
 
 ;;; search help desk popup menu
 (search-help-desk-for "Поиск в помощи для \"~a\"")
 (exact-lucky-search-help-desk-for "Точный поиск в помощи для \"~a\"")

 ;; collapse and expand popup menu items
 (collapse-sexp "Свернуть S-выражение")
 (expand-sexp "Развернуть S-выражение")
 
 ;;; fraction dialog
 (enter-fraction "Введите дробь")
 (whole-part "Целая часть")
 (numerator "Числитель")
 (denominator "Знаменатель")
 (insert-number/bad-whole-part "Целая часть числа должна быть целым числом")
 (insert-number/bad-numerator "Числитель должен быть целым неотрицательным числом")
 (insert-number/bad-denominator "Знаменатель должен быть целым положительным числом")
 (insert-fraction-menu-item-label "Вставить дробь...")

 ;; number snip popup menu
 (show-decimal-expansion "Показать десятичной дробью")
 (show-mixed-fraction-view "Показать смешанной дробью")
 (show-improper-fraction-view "Показать неправильной дробью")
 (show-more-decimal-places "Показать больше позиций после десятичной точки")
 
 ;;; Teachpack messages
 (select-a-teachpack "Выбрать учебный пакет")
 (clear-teachpack "Очистить учебный пакет ~a")
 (teachpack-error-label "DrRacket - ошибка учебного пакета")
 (teachpack-didnt-load "Файл учебного пакета ~a не загружается должным образом.")
 (add-teachpack-menu-item-label "Добавить учебный пакет...")
 (clear-all-teachpacks-menu-item-label "Очистить все учебные пакеты")
 (drscheme-teachpack-message-title "Учебный пакет DrRacket")
 (already-added-teachpack "Учебный пакет ~a уже добавлен")
  
  ; ~a is filled with the teachpack's name; the message appears in the teachpack selection dialog when a user installs a new teachpack
  (compiling-teachpack "Компиляция учебного пакета ~a ...")
  (teachpack-pre-installed "Предварительно инсталлированные учебные пакеты")
  (teachpack-user-installed "Пользовательские учебные пакеты")
  (add-teachpack-to-list... "Добавить учебный пакет в список...")
  (teachpack-already-installed "Учебный пакет с именем '~a' уже был установлен. Перезаписать?")
  ; ~a is filled with a list of language names. Each name is separated by a newline and is indented two spaces (no commas, no 'and')
  (teachpacks-only-in-languages "Учебные пакеты доступны только на таких языках: ~a")
  
  
 ;;; Language dialog
 (introduction-to-language-dialog
  "Пожалуйста, выберите язык. Студентам во вводных курсам предпочтительно использовать язык по умолчанию.")
 (language-dialog-title "Выберите язык")
 (case-sensitive-label "Чувствительность к регистру")
 (output-style-label "Стиль вывода")
 (constructor-printing-style "Конструктор")
 (quasiquote-printing-style "Псевдоапострофы")
 (write-printing-style "ввод")
 (print-printing-style "печать")
 (sharing-printing-label "Показывать совместно используемое в значениях")
 (use-pretty-printer-label "Вставлять переводы строк в печатаемые значения")
 (input-syntax "Синтакис ввода")
 (dynamic-properties "Динамические свойства")
 (output-syntax "Синтаксис вывода")
  (teachpacks "Учебные пакеты") ;; label in the language dialog for the teaching languages
  (teachpacks-none "<< отсутствуют >>") ;; shows up under the previous string, when there are no teachpacks
 (no-debugging-or-profiling "Не отлаживать или профилировать")
 (debugging "Отладить")
 (debugging-and-profiling "Отладить и профилировать")
 (test-coverage "Включать набор синтаксических тестов")
 (show-details-button-label "Показать подробности")
 (hide-details-button-label "Скрыть подробности")
 (choose-language-menu-item-label "Выбрать язык...")
 (revert-to-language-defaults "Вернуть язык по умолчанию")
 (fraction-style "Стиль дроби")
 (use-mixed-fractions "Смешанные дроби")
 (use-repeating-decimals "Периодические десятичные дроби")
 (decimal-notation-for-rationals "Использовать десятичную запись для рациональных чисел")
 (enforce-primitives-group-box-label "Начальные привязки")
 (enforce-primitives-check-box-label "Запретить переопределение начальных привязок")
 (automatically-compile "Заполнить каталоги compiled/ (для более быстрой загрузки)")
 (preserve-stacktrace-information "Сохранить трассировку стека (отключает некоторые оптимизации)")
 (expression-level-stacktrace "Выражение уровня трассировки стека")
 (function-level-stacktrace "Функция уровня трассировки стека")


  ; used in the bottom left of the drscheme frame 
  ; used the popup menu from the just above; greyed out and only
  ; visible when some languages are in the history
  (recent-languages "Последние языки:")
  ; shows up in bottom-left programming language menu popup, when no langs are recorded
  (no-recently-chosen-languages "нет выбранных языков") 
  
 ;; startup wizard screen language selection section
 (please-select-a-language "Пожалуйста, выберите язык")
  
  
 ;;; languages
 (beginning-student "Начинающий студент")
 (beginning-one-line-summary "define, cond, структуры, константы и примитивы")
 (beginning-student/abbrev "Начинающий студент со списковыми сокращениями")
 (beginning/abbrev-one-line-summary "Начинающий студент со списковым стилем печати в цикле \"чтение-вычисление-печать\"")
 (intermediate-student "Средний студент")
 (intermediate-one-line-summary "Начинающий студент с лексической видимостью")
 (intermediate-student/lambda "Средний студент с лямбда-выражениями")
 (intermediate/lambda-one-line-summary "Средний студент с функциями высших порядков")
 (advanced-student "Продвинутый студент")
 (advanced-one-line-summary "Средний студент с лямбда-выражениями и мутацией")
 (how-to-design-programs "Как проектировать программы") ;; should agree with MIT Press on this one...
 (pretty-big-scheme "Pretty Big")
 (pretty-big-scheme-one-line-summary "Добавлены синтаксис и функции языков из \"Как проектировать программы\" в mzscheme и mred")
 (r5rs-language-name "R5RS")
 (r5rs-one-line-summary "R5RS с несущественными сокращениями")
 (expander "Макрорасширитель")
 (expander-one-line-summary "Выражение чаще раскрывается, чем вычисляется")
 (legacy-languages "Унаследованные языки")
 (teaching-languages "Учебные языки")
 (experimental-languages "Экспериментальные языки")
  (initial-language-category "Начальные языки")
  (no-language-chosen "Не выбран язык")
 
 (module-language-one-line-summary "Выполнение создает цикл \"чтение-вычисление-печать\" внутри модуля")
  (module-language-auto-text "Автоматическая строка #lang") ;; shows up in the details section of the module language  

  ;;; from the `not a language language' used initially in drscheme.
  (must-choose-language "DrRacket не способен выполнять программы при невыбранном языке программирования.")
  
  ; next two appear before and after the name of a text book (which will be in italics)
  (using-a-textbook-before "Использовать ")
  (using-a-textbook-after "?")
  
  ; next two are before and after a language
  (start-with-before "Начать с ")
  (start-with-after "")
  
  (seasoned-plt-schemer? "Опытный программист на PLT Scheme?")
  (looking-for-standard-scheme? "Просмотреть стандарт для Scheme?")

  ; the three string constants are  together and the middle
  ; one is hyperlinked to the dialog that suggests various languages
  (get-guidance-before "Или выберите пункт \"Выбрать язык...\" в меню \"Язык\", или ")
  (get-guidance-during "получите совет")
  (get-guidance-after ".")
    
 ;;; debug language
 (unknown-debug-frame "[неизвестный]")
 (backtrace-window-title "Обратная трассировка DrRacket")
 (files-interactions "При выполнении ~a") ;; filled with a filename
 (current-interactions "вызовов")
 (current-definitions "определений")
 (mzscheme-w/debug "Текстовый (MzScheme, включая Стандарт 1998 года)")
 (mzscheme-one-line-summary "PLT-реализация Scheme")
 (mred-w/debug "Графический (MrEd, включая MzScheme)")
 (mred-one-line-summary "Добавить поддержку графического интерфейса пользователя к MzScheme")

 ;; profiling
 (profiling-low-color "Нижний")
 (profiling-high-color "Верхний")
 (profiling-choose-low-color "Пожалуйста, выберите нижний цвет")
 (profiling-choose-high-color "Пожалуйста, выберите верхний цвет")
 (profiling "Профилирование")
 (profiling-example-text "(define (whee) (whee))")
 (profiling-color-config "Набор основных цветов") 
 (profiling-scale "Шкала распределения цветов")
 (profiling-sqrt "Пропорциональна корню квадратному")
 (profiling-linear "Линейная")
 (profiling-square "Квадратическая")
 (profiling-number "Число вызовов")
 (profiling-time "Общее время")
 (profiling-update "Обновить профиль")
 (profiling-col-percent-time "% времени")
 (profiling-col-function "Функция")
 (profiling-col-time-in-msec "миллисекунд")
 (profiling-col-calls "Вызовы")
 (profiling-show-profile "Показать профиль")
 (profiling-hide-profile "Скрыть профиль")
 (profiling-unknown-src "<< неизвестный >>")
 (profiling-no-information-available "Информация о профилировании недоступна. Пожалуйста, убедитесь, что профилирование допускается на выбранном языке, а программа выполнена.")
 (profiling-clear? "Изменения в окне определений лишают профилирующую информацию актуальности. Продолжить?")
 
 ;; test coverage
 (test-coverage-clear? "Изменения в окне определений лишают информацию о тестировании актуальности. Продолжить?")
 (test-coverage-clear-and-do-not-ask-again "Да, больше не спрашивать")
 (test-coverage-ask? "Спрашивать об очистке информации о тестировании")
  
 ;; tracing
 (tracing-enable-tracing "Допустить трассировку")
 (tracing-show-tracing-window "Показать трассировку")
 (tracing-hide-tracing-window "Скрыть трассировку")
 (tracing-tracing-nothing-to-show "Результаты трассировки еще не доступны. (Убедитесь, что выбранный язык поддерживает трассировку и что трассировка включена.)")

 ;;; repl stuff
 (evaluation-terminated "Прервать вычисления")
 (evaluation-terminated-explanation
  "Поток вычислений остановлен до следующего запуска.")
  
  ; The next three constants show up in the same dialog as the above evaluation-terminated string
  ; constants.
  ; The first two show up only when the user calls 'exit' (possibly with a status code).
  ; The third shows up when the program runs out of memory.
  (exited-successfully "Успешное завершение.")
  (exited-with-error-code "Завершение с кодом ошибки ~a.") ;; ~a is filled in with a number between 1 and 255
  (program-ran-out-of-memory "Программе не хватило памяти для завершения выполнения.")
 (last-stack-frame "Показать последнее значение стека")
 (last-stack-frames "Показать последние ~a значений стека")
 (next-stack-frames "Показать следующие ~a значений стека")
 
 ;;; welcoming message in repl
 (language "Язык")
 (custom "выбранный")
 (teachpack "Учебный пакет")
 (welcome-to "Добро пожаловать в")
 (version "версия")
 
 ;;; kill evaluation dialog
 (kill-evaluation? "Вы хотите прервать цикл вычислений?")
 (just-break "Только остановить")
 (kill "Прервать")
 (kill? "Прервать?")

 ;;; version checker
 (version:update-menu-item   "Проверить обновления...")
 (version:update-check       "Проверка обновлений") ; dialog title, with the next line
 (version:connecting-server  "Соединение с сервером версии Racket")
 (version:results-title      "Проверка версии Racket")
 (version:do-periodic-checks "Периодически проверять более новые версии Racket")
 (version:take-me-there      "Обновить") ; ...to the download website
 ;; the next one can appear alone, or followed by a comma and the one after that
 (version:plt-up-to-date     "У Вас новейшая версия Racket")
 (version:but-newer-alpha    "однако заметьте, что есть более новая альфа-версия")
 ;; This is used in this context: "Racket vNNN <<<*>>> http://download..."
 (version:now-available-at   "теперь доступно в")

 ;; insert menu
 (insert-menu "&Вставка")
 
 ;; large semi colon letters
 (insert-large-letters... "Вставить большие буквы...")
 (large-semicolon-letters "Большие закомментированные буквы")
 (text-to-insert "Вставить текст")

 (module-browser-filename-format "Полное имя: ~a (~a строк)")
 (module-browser-root-filename "Основное имя файла: ~a")
 (module-browser-font-size-gauge-label "Размер  шрифта")
 (module-browser-progress-label "Прогресс просмотра модулей")
 (module-browser-adding-file "Добавить файл: ~a...")
 (module-browser-laying-out-graph-label "Граф разметки")
 (module-browser-open-file-format "Открыть ~a")
 (module-browser "Браузер модуля") ;; frame title
 (module-browser... "Браузер модуля...") ;; menu item title
 (module-browser-error-expanding "Ошибка раскрытия имени программы:\n\n~a")
 (module-browser-show-lib-paths "Показывать файлы, загружаемые из библиотек")
 (module-browser-progress "Браузер модуля: ~a") ;; prefix in the status line
 (module-browser-compiling-defns "Браузер модуля: компиляция определений")
 (module-browser-show-lib-paths/short "Показывать пути к библиотекам") ;; check box label in show module browser pane in drscheme window.
 (module-browser-show-planet-paths/short "Показывать пути к PLanet") ;; check box label in show module browser pane in drscheme window.
 (module-browser-refresh "Обновить") ;; button label in show module browser pane in drscheme window.
; (module-browser-only-in-plt-and-module-langs
;  "Браузер модуля доступен только для программ на PLT-языках и на языке модуля (и только для программ, которые содержат модули).")
 (module-browser-name-length "Длина имени")
 (module-browser-name-short "Короткая")
 (module-browser-name-medium "Средняя")
 (module-browser-name-long "Длинная")
 (module-browser-name-very-long "Длинная, с фазами")  ;; like 'Long' but shows the phases where this file is loaded
 (module-browser-open-all "Открыть все показанные здесь файлы")

 (happy-birthday-matthias "С днем рождения, Матиас!")
 (happy-birthday-matthew "С днем рождения, Метью!")
 (happy-birthday-shriram "С днем рождения, Шрирам!")

 (mrflow-using-default-language-title "Язык, используемый по умолчанию")
 (mrflow-using-default-language "В используемом языке не определена таблица типов для его примитивов. Используйте стандарт Scheme.")
 (mrflow-button-title "Анализировать")
 ;(mrflow-unknown-style-delta-error-title "Неизвестный стиль блока дельта")
 ;(mrflow-unknown-style-delta-error "Неизвестный стиль блока дельта: ~a")
 (mrflow-popup-menu-show-type "Показать тип")
 (mrflow-popup-menu-hide-type "Скрыть тип")
 (mrflow-popup-menu-show-errors "Показать ошибки")
 (mrflow-popup-menu-hide-errors "Скрыть ошибки")
 ;(mrflow-read-exception-title "Исключение при вводе")
 ;(mrflow-read-exception "Исключение при вводе: ~a")
 ;(mrflow-syntax-exception-title "Синтаксическое исключение")
 ;(mrflow-syntax-exception "Синтаксическое исключение: ~a")
 ;(mrflow-unknown-exception-title "Неизвестное исключение")
 ;(mrflow-unknown-exception "Неизвестное исключение: ~a")
 ;(mrflow-language-primitives-error-title "Ошибка в примитивах языка")
 ;(mrflow-language-primitives-error "Неправильное имя файла для таблицы типов примитивов языка: ~a")
  
 (snips-and-arrows-popup-menu-tack-all-arrows "Соединить все стрелки")
 (snips-and-arrows-popup-menu-untack-all-arrows "Убрать все стрелки")
 (snips-and-arrows-user-action-disallowed-title "Пользовательские изменения в настоящее время запрещены")
 (snips-and-arrows-user-action-disallowed "Пользовательские изменения запрещены в редакторах, содержащих элементы из панели инструментов. Скройте все инструменты перед редактированием.")
 ;(snips-and-arrows-changing-terms-warning-title "Changing terms will be undoable")
 ;(snips-and-arrows-changing-terms-warning "Changing terms in an editor containing snips cannot be undone.  You can either cancel this action, remove the snips, and try the change again, or you can continue with the change, in which case the change will not be undoable (all others changes made before and afterward will still be undoable though).")
 (snips-and-arrows-hide-all-snips-in-editor "Скрыть все инструменты, вставленные в редактор")

 (xml-tool-insert-xml-box "Вставить блок XML")
 (xml-tool-insert-scheme-box "Вставить блок Racket")
 (xml-tool-insert-scheme-splice-box "Присоединить блок Racket")
 (xml-tool-xml-box "XML блок")
 (xml-tool-scheme-box "Racket блок")
 (xml-tool-scheme-splice-box "Присоединенный блок Racket")
 (xml-tool-switch-to-scheme "Переключиться на блок Racket")
 (xml-tool-switch-to-scheme-splice "Переключиться присоединенный на блок Racket")
 (xml-tool-eliminate-whitespace-in-empty-tags
  "Убирать пробелы в пустых тегах")
 (xml-tool-leave-whitespace-alone
  "Оставить по одному пробелу")
 
 (show-recent-items-window-menu-item "Показать недавно открытые файлы в отдельном окне")
 (show-recent-items-window-label "Недавно открытые файлы")
 (number-of-open-recent-items "Количество недавних элементов")
 (switch-anyway "Все равно перейти к файлу")

 (stepper-program-has-changed "Предупреждение: Программа была изменена.")
 (stepper-program-window-closed "Предупреждение: Окно программы закрыто.")

 (stepper-name "Пошаговое выполнение")
 (stepper-language-level-message "Пошаговое выполнение не работает для языка \"~a\".")
 (stepper-button-label "Шаг")

 (stepper-previous-application "Программа")
 (stepper-previous "Шаг")
 (stepper-next "Шаг")
 (stepper-next-application "Программа")
 (stepper-jump "Перейти...") ;; this one is changed.  action?
 (stepper-out-of-steps "Вычисления завершены ранее, чем достигнут искомый шаг.")
 (stepper-no-such-step/title "Шаг не найден")
 (stepper-no-such-step "Шаг, соответствующий критерию, не найден.")
 (stepper-no-such-step/earlier "Предыдущий шаг, соответствующий критерию, не найден.")
 (stepper-jump-to-beginning "в начало") ;; name changed from stepper-home to stepper-jump-to-beginning
 (stepper-jump-to-end "в конец") ;; content changed
 (stepper-jump-to-selected "к началу выбранного") ;; new

 (debug-tool-button-name "Отладить")

 (dialog-back "Назад")

 ;; warnings about closing a drscheme frame when the program
 ;; might still be doing something interesting
 (program-is-still-running "Программа в окне определений все еще выполняется. Все равно закрыть?")
  (program-has-open-windows "Программа в окне определений имеет открытые окна. Все равно закрыть?")
 
  ;; ml-command-line-arguments is for the command line arguments
  ;; label in the module language details in the language dialog.
  (ml-command-line-arguments "Аргументы командной строки как вектор строк")

  ;; ml-cp names are all for the module language collection path
  ;; configuration. See the details portion of the language dialog
  ;; for the module language (at the bottom).
  (ml-cp-default-collection-path "<<пути к коллекции по умолчанию>>")

  ;; in std get-directory 
  (ml-cp-choose-a-collection-path "Пожалуйста, выберите путь к коллекции")

  ;; err msg when adding default twice
  (ml-cp-default-already-present
   "Заданные по умолчанию пути к коллекции уже установлены")
  
  ;; title of this section of the dialog (possibly the word
  ;; `Collection' should not be translated)
  (ml-cp-collection-paths "Пути к коллекции")

  ;; button labels
  (ml-cp-add "Добавить")
  (ml-cp-add-default "Добавить по умолчанию")
  (ml-cp-remove "Удалить")
  (ml-cp-raise "Вверх")
  (ml-cp-lower "Вниз")
  
  (ml-always-show-#lang-line "Всегда показывать строку #lang модуля языка")

  ;; Profj
  (profj-java "Java")
  (profj-java-mode "Режим Java")
  (profj-java-coverage "Набор Java") ;; shows up in the preferences dialog under 'Color'

  (profj-beginner-lang "Начинающий")
  (profj-beginner-lang-one-line-summary "Java-подобный язык для обучения начинающих")
  (profj-full-lang "Полный")
  (profj-full-lang-one-line-summary "Подобный Java 1.0 (приблизительно 1.1)")
  (profj-advanced-lang "Опытный")
  (profj-advanced-lang-one-line-summary "Java-подобный язык для опытных пользователей")
  (profj-intermediate-lang "Средний")
  (profj-intermediate-lang-one-line-summary "Java-подобный язык для продолжающих обучение")
  (profj-intermediate-access-lang "Средний + доступ")
  (profj-intermediate-access-lang-one-line-summary "Java-подобый язык для продолжающих обучение с доступом к модификаторам")
  (profj-dynamic-lang "Java + динамическая типизация")
  (profj-dynamic-lang-one-summary "Java с возможностью динамической типизации")

  (profj-java-mode-color-heading "Цвета редактирования") ; Heading for preference to choose editing colors  
  (profj-java-mode-color-keyword "ключевые слова")
  (profj-java-mode-color-string "строка")
  (profj-java-mode-color-literal "литерал")
  (profj-java-mode-color-comment "комментарий")
  (profj-java-mode-color-error "ошибка")
  (profj-java-mode-color-identifier "идентификатор")
  (profj-java-mode-color-prim-type "примитивный тип") ; Example text for built-in Java types
  (profj-java-mode-color-default "по умолчанию")

  (profj-coverage-color-heading "Цвета окружения") ; Heading for preference to choose coverage colors
  (profj-coverage-color-covered "окружающее выражение") 
  
  (profj-language-config-display-preferences "Персональные настройки отображения") ; Heading for preferences controlling printing
  (profj-language-config-display-style "Стиль отображения")
  (profj-language-config-display-field "Класс + Поля")
  (profj-language-config-class "Класс")
  (profj-language-config-display-array "Выводить массивы целиком?")
  (profj-language-config-testing-preferences "Настройки тестирования") ; Heading for preferences controlling test behavior
  ;(profj-language-config-testing-enable "Отображать результаты тестирования при выполнении?") ; Run should be the word found on the Run button
  (profj-language-config-testing-coverage "Собрать информацию для тестирования?")
  (profj-language-config-support-test-language "Поддерживать тестирование языковых расширений?")
  (profj-language-config-testing-check "Разрешить проверку выражений?") ; check should not be translated
  (profj-language-config-classpath "Путь к классам")
  (profj-language-config-choose-classpath-directory "Выбрать каталог для добавления к пути к классам")
  (profj-language-config-classpath-display "Показать текущий") ; Button label to print the current classpath

  (profj-test-name-close-to-example "Имя класса ~a содержит фразу, близкую к примерам.")
  (profj-test-name-example-miscapitalized "Имя класса ~a с точностью до регистра содержит пример.")
  
   ;; Close testing window and do not run test cases any more
  ;(profj-test-results-close-and-disable "Закрыть и запретить тестирование")
  ;; Hide docked testing window and do not run test cases any more
  ;(profj-test-results-hide-and-disable "Скрыть и запретить тестирование")
  ;Renamed below
  ;(profj-test-results-window-title "Результаты тестирования")
  
  (profj-unsupported "Не поддерживается")
  (profj-executables-unsupported "Извините - в настоящий момент Java не поддерживает исполняемые файлы")

  (profj-convert-to-text-comment "Преобразовать в текстовый комментарий")
  (profj-convert-to-comment "Преобразовать в комментарий")

  (profj-executing-main "запуск main")

  (profj-insert-java-comment-box "Вставить блок Java-комментария")
  (profj-insert-java-interactions-box "Вставить блок Java-кода")

  ;;The Test engine tool
  ;;
  (test-engine-window-title "Результаты тестирования")
  ;;Following two appear in View menu, attach and free test report window from DrRacket frame
  (test-engine-dock-report "Прикрепить отчет о тестировании")
  (test-engine-undock-report "Открепить отчет о тестировании")
  ;;Following two appear in Racket (Java, etc) menu, cause Tests to be Run automatically or not
  (test-engine-enable-tests "Разрешить тестирование")
  (test-engine-disable-tests "Запретить тестирование")
 
  (test-engine-ran-1-test "Выполнен 1 тест.")
  (test-engine-ran-1-check "Выполнена 1 проверка.")
  ;; ditto, only plural
  (test-engine-ran-n-tests "Выполнено ~a тестов.")
  (test-engine-ran-n-checks "Выполнено ~a проверок.")
  (test-engine-1-test-passed "Тест пройден!")
  (test-engine-1-check-passed "Проверка закончена!")
  (test-engine-both-tests-passed "Оба теста пройдены!")
  (test-engine-both-checks-passed "Обе проверки закончены!")
  (test-engine-all-tests-passed "Все тесты пройдены!")
  (test-engine-all-checks-passed "Все проверки закончены!")
  (test-engine-all-n-tests-passed "Все ~a тестов пройдены!")
  (test-engine-all-n-checks-passed "Все ~a проверок закончены!")
  (test-engine-0-tests-passed "0 тестов пройдено.")
  (test-engine-0-checks-passed "0 проверок закончено.")
  (test-engine-m-of-n-tests-failed "~a из ~a тестов ошибочны.")
  (test-engine-m-of-n-checks-failed "~a из ~a проверок ошибочны.")
  (test-engine-must-be-tested "Эта программа должна быть протестирована!")
  (test-engine-is-unchecked "Эта программа непроверена!")
  (test-engine-tests-disabled "Тесты отключены.")
  (test-engine-should-be-tested "Эта программа должна быть проверена.")
  (test-engine-at-line-column "в строке ~a, столбце ~a")
  (test-engine-in-at-line-column "в ~a, строке ~a, столбце ~a")
  ; as in "column (unknown)"
  (test-engine-unknown "(неизвестно)")
  (test-engine-trace-error "Ошибка трассировки")

  ; The ~F is special marker for the offending values, which may be
  ; printed specially in DrRacket.
  (test-engine-check-encountered-error
   "ошибка проверки: вместо ожидаемого значения ~F. ~n   :: ~a")
  (test-engine-actual-value-differs-error
   "Фактическое значение ~F отличается от ожидаемого ~F.")
  (test-engine-actual-value-not-within-error
   "Фактическое значение ~F выходит за пределы ~v ожидаемого значения ~F.")
  (test-engine-encountered-error-error
   "ошибка проверки: следующая ошибка вместо ожидаемого значения~a~n   :: ~a")
  (test-engine-expected-error-error
   "ошибка проверки: ожидалась ошибка, но вместо этого получено значение ~F.~n ~a")

  ; section header
  (test-engine-check-failures "Ошибки тестирования:")
  ; section header
  (test-engine-signature-violations "Нарушения соглашения:")

  ; part of one phrase "signature <at line ...> to blame: procedure <...>
  (test-engine-signature "соглашение")
  (test-engine-to-blame "нарушено: процедура ")

  (test-engine-no-signature-violations "Нет нарушений соглашения.")
  (test-engine-1-signature-violation "1 нарушение соглашения.")
  (test-engine-n-signature-violations "~a нарушений соглашения.")

  ; as in got <value>, signature <at ...>
  (test-engine-got "получено")
  
  (profjWizward-insert-java-class "Вставить класс Java")
  (profjWizard-insert-java-union "Вставить объединение Java")
  
  ;; The Test Suite Tool
  ;; Errors
  (test-case-empty-error "Пустой тестовый набор")
  (test-case-too-many-expressions-error "Слишком много выражений в тестовом наборе.")
  ;; DrRacket window menu items
  (test-case-insert "Вставить тестовый набор")
  (test-case-disable-all "Отключить все тестовые наборы")
  (test-case-enable-all "Включить все тестовые наборы")
  
  ;; NOTE: The following string constants are labels of the test-case fields. The width
  ;;       of the field is determined by the length of the longest of the following three words.
  ;;       if the words are too long the test case will take up too much horizontal room and
  ;;       not look very good.
  ;; This string is the label of the expression that is being tested in a test case.
  (test-case-to-test "Проверяется")
  ;; This string is the label of the expression that is the expected value of the to-test expression.
  (test-case-expected "Должно быть")
  ;; This string is the label of the actual result of the to test expression.
  (test-case-actual "Фактически")
  (test-case-predicate "Предикат")
  (test-case-should-raise "Должен бы")
  ;; The label of a field of the test-case that describes the expected error message of a test case
  (test-case-error-message "Сообщение об ошибке")

  (test-case-menu-title "Тестовый набор")
  (test-case-switch-to-error-box "Переключиться в окно ошибочных тестов")
  (test-case-switch-to-nonerror-box "Переключиться в окно безошибочных тестов")
  (test-case-collapse "Свернуть тестовый набор")
  (test-case-show-actual "Показать фактическое значение")
  (test-case-enable "Разрешить тестовый набор")
  (test-case-show-predicate "Показать предикат")
  (test-case-show-error-message "Показать сообщение об ошибке")
  (test-case-convert-to-text "Преобразовать в текст")
  
  ;; Profj Boxes
  (profjBoxes-empty-error "Не заданы действия")
  (profjBoxes-too-many-expressions-error "Слишком много выражений")
  (profjBoxes-interactions-label "Действия")
  (profjBoxes-bad-java-id-error "Неверный Java ID")
  (profjBoxes-examples-label "Примеры")
  (profjBoxes-add-new-example-button "Добавить новый пример")
  (profjBoxes-type "Тип")
  ;; The Java identifier of an example of data
  (profjBoxes-name "Имя")
  (profjBoxes-value "Значение")
  (profjBoxes-insert-java-examples "Вставить примеры Java")
  (profjBoxes-insert-java-interactions "Вставить Java-код")

  ;; Slideshow
  (slideshow-hide-picts "Показать вложенные поля")
  (slideshow-show-picts "Показать рисунки")
  (slideshow-cannot-show-picts "Невозможно отобразить рисунки; запустите программу для кеширования размеров")
  (slideshow-insert-pict-box "Вставить блок рисунка") 

  ;; GUI Tool
  (gui-tool-heading "Инструмент графического интерфеса пользователя")
  (gui-tool-before-clicking-message "Перед выбором иконки на панеле используйте \"Вставить элемент графического интерфейса пользователя\" из меню \"Специальная вставка\" в корневой элемент графического интерфейса пользователя либо выберите уже вставленный элемент.")
  (gui-tool-show-gui-toolbar "Показать панель инструментов графического интерфейса пользователя")
  (gui-tool-hide-gui-toolbar "Скрыть панель инструментов графического интерфейса пользователя")
  (gui-tool-insert-gui "Вставить элемент графического интерфейса пользователя")

  ;; contract violation tracking
  
  ; tooltip for new planet icon in drscheme window (must have a planet violation logged to see it)
  (show-planet-contract-violations "Показать нарушения от PLaneT")

  ; buttons in the dialog that lists the recorded bug reports
  (bug-track-report "Файл отчета")
  (bug-track-forget "Не обрабатывать")
  (bug-track-forget-all "Все проигнорировать")
    
  ;; planet status messages in the bottom of the drscheme window; the ~a is filled with the name of the package
  (planet-downloading "PLaneT: загрузка ~a...")
  (planet-installing "PLaneT: инсталляция ~a...")
  (planet-finished "PLaneT: закончена с ~a.")
  (planet-no-status "PLaneT") ;; this can happen when there is status shown in a different and then the user switches to a tab where planet hasn't been used
  
  ;; string normalization. To see this, paste some text with a ligature into DrRacket
  ;; the first three strings are in the dialog that appears. The last one is in the preferences dialog
  (normalize "Нормализировать")
  (leave-alone "Оставить без изменений")
  (normalize-string-info "Строка, которую Вы вставили, содержит лигатуры и другие ненормализированные знаки. Нормализировать их?")
  (normalize-string-preference "Нормализировать вставленные строки")
  (ask-about-normalizing-strings "Спрашивать о нормализации строк")

  )
