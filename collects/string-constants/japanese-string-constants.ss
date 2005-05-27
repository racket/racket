#|

When modifying the string constants files,
please adhere to these guidelines:

- All the entries in english-string-constants.ss have the same format
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

- If you modify an existing string in english-string-constants.ss, go
  through all the *-string-constants.ss files for the other languages,
  comment out the old version of the modified string in each of these
  files, and put a short comment there telling us the English string
  has changed and needs to be re-translated.  Do not erase the old
  version, it might help us translate the new one.  Do not move it
  either.  Just comment it out and add the short comment.  After the
  next cvs update DrScheme will automatically tell us translators that
  a new string needs to be translated, we will find your comment in
  the file, and know what to do.
	Some evil evil people might think that, since DrScheme automatically
  informs us of new strings to be translated, an easier thing to do
  when modifying an existing string would be to simply rename it at
  the same time.  This works, except that if you do that, we
  translators will get two warnings from DrScheme:
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
  change the name of the string in *ALL* the *-string-constants.ss
  files.  That's a modification you can do without the help of a
  translator, so do it yourself.  It's not the job of the translators
  to clean up your naming mess for you.  Besides, you are the one who
  knows what you changed, so leaving the translators having to guess
  what you did is Not Nice(tm).

- If, for some reason, you need to remove a string (i.e. you changed
  your code and don't need the string anymore), remove the string in
  *ALL* the *-string-constants.ss files.  Again, you don't need the
  help of a translator to do that.  If you're not sure whether you
  might need the string in the future or not, just comment it out in
  *ALL* the files.

|#

(module japanese-string-constants "string-constant-lang.ss"
 ;;; when translating this constant, substitute name of actual langauge for `English'
 (is-this-your-native-language "Is Japanese Your Native Language?")

 (are-you-sure-you-want-to-switch-languages
  "This will change the language of the GUI, which requires you to restart DrScheme. Are you sure?")

 (interact-with-drscheme-in-language "Interact with DrScheme in Japanese")

 ;; these two should probably be the same in all languages excepet English.
 ;; they are the button labels (under macos and windows, respectively)
 ;; that go the with the string above.
 (accept-and-quit "Accept and Quit")
 (accept-and-exit "Accept and Exit")
 
 ;;; general purpose (DrScheme is hereby a word in every language, by decree of Robby :)
 (plt "PLT")
 (drscheme "DrScheme")
 (ok "OK")
 (cancel "キャンセル")
 (abort "中止")
 (untitled "Untitled")
 (untitled-n "Untitled ~a")
 (warning "警告")
 (error "エラー")
 (close "閉じる") ;; as in, close an open window. must match close-menu-item
                 ;; in the sense that, when the &s have been stripped from
                 ;; close-menu-item, it must be the same string as this.
 (stop "停止")   
 (&stop "停止(&S)") ;; for use in button and menu item labels, with short cut.
 (are-you-sure-delete? "~a を削除してよろしいですか？") ;; ~a is a filename or directory name
 (ignore "無視")
 (revert "復元")

 ;; label for a generic check box, often supported on dialogs
 ;; that ask a binary choice of the user. If checked, the
 ;; dialog isn't going to be shown again.
 (dont-ask-again "今後このメッセージを表示しない (現在の選択が使われます)")

 ;;; important urls
 (web-materials "関連するウェブサイト") ;; menu item title
 (tool-web-sites "ツールのウェブサイト")   ;; menu item title
 (drscheme-homepage "DrScheme")
 (plt-homepage "PLT")
 (how-to-use-scheme "How to Use Scheme") ;; title of a book.
 (teachscheme!-homepage "TeachScheme!") ;; probably this should be a `word' in all languages

 ;;; bug report form
 (cancel-bug-report? "バグ報告を中止しますか？")
 (are-you-sure-cancel-bug-report?
  "このバグ報告を送信せずに、中止してよろしいですか？")
 (bug-report-form "バグ報告フォーム")
 (bug-report-field-name "名前")
 (bug-report-field-email "メールアドレス")
 (bug-report-field-summary "概要")
 (bug-report-field-severity "深刻度")
 (bug-report-field-class "分類")
 (bug-report-field-priority "優先度")
 (bug-report-field-description "説明")
 (bug-report-field-reproduce1 "再現の手順")
 (bug-report-field-reproduce2 "")
 (bug-report-field-environment "環境")
 (bug-report-field-tools "ツール")
 (bug-report-field-docs-installed "インストールされている文書")
 (bug-report-field-language "言語")
 (bug-report-field-teachpacks "ティーチパック")
 (bug-report-field-collections "コレクション")
 (bug-report-field-human-language "自然言語")
 (bug-report-field-version "バージョン")
 (bug-report-synthesized-information "詳細情報")  ;; dialog title
 (bug-report-show-synthesized-info "詳細情報の表示")
 (bug-report-submit "送信")
 (bug-report-submit-menu-item "バグ報告の送信") ;; in Help Menu (drs & help desk)
 (sending-bug-report "バグ報告を送信中です")
 (error-sending-bug-report "バグ報告の送信エラー")
 (error-sending-bug-report-expln "バグ報告の送信中にエラーが発生しました。もし、インターネット接続が正常であるなら、\n\n    http://bugs.plt-scheme.org/\n\nを開いて、オンラインのウェブフォームからバグ報告を行ってください。お手間をかけて申し訳ありません。\n\nエラーメッセージ:\n~a")
 (bug-report-sent "バグ報告を送信しました")
 (bug-report-sent-detail "バグ報告を行っていただき有難うございました。30分以内に確認のメールがあなたに送られます。もし送られて来こない場合は、scheme@plt-scheme.org にご連絡ください。")
 (illegal-bug-report "バグ報告が正しく入力されていません")
 (pls-fill-in-field "フィールド \"~a\" を入力してください。")
 (malformed-email-address "メールアドレスの形式が正しくありません。")
 (pls-fill-in-either-description-or-reproduce " \"説明\" または \"再現の手順\" のいずれか一方は入力してください。")

 ;;; check syntax
 (check-syntax "構文の検証")
 (cs-italic "斜体")
 (cs-bold "太字")
 (cs-underline "下線")
 (cs-change-color "カラーの変更")
 (cs-tack/untack-arrow "矢印の連結/非連結")
 (cs-jump-to-next-bound-occurrence "Jump to Next Bound Occurrence")
 (cs-jump-to-binding "Jump to Binding Occurrence")
 (cs-jump-to-definition "Jump to Definition")
 (cs-error-message "エラーメッセージ")
 (cs-open-file "~a を開く")
 (cs-rename-var "~a の名前を変更")
 (cs-rename-id "Rename Identifier")
 (cs-rename-var-to "Rename ~a to:")
 (cs-name-duplication-error "The new name you have chosen, ~s, conflicts with an already established name in this scope.")
 (cs-rename-anyway "Rename Anyway")
 (cs-status-init "Check Syntax: Initializing environment for user code")
 (cs-status-coloring-program "Check Syntax: coloring expression")
 (cs-status-eval-compile-time "Check Syntax: eval compile time")
 (cs-status-expanding-expression "Check Syntax: expanding expression")
 (cs-mouse-over-import "binding ~s imported from ~s")

 (cs-lexical-variable "レキシカル変数")
 (cs-lexical-syntax "レキシカル構文")
 (cs-imported-variable "インポート変数")
 (cs-imported-syntax "インポート構文")

 ;;; info bar at botttom of drscheme frame
 (collect-button-label "GC")
 (read-only "読み取り専用")
 (read/write "読み書き可能")
 (auto-extend-selection "自動拡張")
 (overwrite "上書き")
 (running "実行中")
 (not-running "停止中")
 
 ;;; misc
 (welcome-to-something "~a にようこそ！")
 
 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "Welcome to DrScheme, version ~a, ~a")

 ; these appear on subsequent lines in the `Help|Welcome to DrScheme' dialog.
 (welcome-to-drscheme "Welcome to DrScheme")
 (version/language "version ~a, ~a")

 (goto-line "行番号を指定して移動")
 (goto-line-invalid-number
  "~a は無効な行番号です。行番号は 1 から ~a までの整数で指定してください。")
 (goto-position "位置を指定して移動")
 (no-full-name-since-not-saved
  "このファイルはまだ保存されていないため、完全な名前を持っていません。")
 (cannot-open-because-dne "~a が存在しなため、開くことができません。")
 (interactions-out-of-sync
  "警告: 対話ウィンドウは定義ウィンドウと同期していません。実行ボタンを押してください。")
 (file-is-not-saved "ファイル \"~a\" は保存されていません。")
 (save "保存")
 (please-choose-either "\"~a\" または \"~a\" のいずれかを選択してください。")
 (close-anyway "とにかく閉じる")
 (clear-anyway "とにかく消去する")

 ;; menu item title
 (log-definitions-and-interactions "定義と対話を記録する...")
 (stop-logging "記録の停止")
 (please-choose-a-log-directory "記録先のディレクトリを選択してください")
 (logging-to "記録先: ")
 (erase-log-directory-contents "記録先のディレクトリ ~a の内容を消去しますか？")
 (error-erasing-log-directory "記録先のディレクトリの内容を消去できませんでした。\n\n~a\n")

 ;; modes
 (mode-submenu-label "モード")
 (scheme-mode "Scheme モード")
 (text-mode "Text モード")

 (scheme-mode-color-symbol "シンボル")
 (scheme-mode-color-keyword "キーワード")
 (scheme-mode-color-comment "コメント")
 (scheme-mode-color-string "文字列")
 (scheme-mode-color-constant "定数")
 (scheme-mode-color-parenthesis "括弧")
 (scheme-mode-color-error "エラー")
 (scheme-mode-color-other "その他")
 ;; the ~a is filled in with one of the above (scheme-mode-*)
 (syntax-coloring-choose-color "Choose a color for ~a")
 (preferences-colors "Colors") ;; used in the preferences dialog
 
 (url "URL")
 (url: "URL:")
 (open-url... "URL を開く...")
 (open-url "URL を開く")
 (browse... "選択...")
 (bad-url "不正な URL")
 (bad-url:this "不正な URL: ~a")
 
 ;; Help Desk
 (help "ヘルプ")
 (help-desk "ヘルプデスク")
 (plt:hd:search-results "検索結果")
 (plt:hd:search "検索")
 (plt:hd:search-for "検索語")
 (plt:hd:lucky "Lucky!")
 (plt:hd:feeling-lucky "Feeling Lucky")
 (plt:hd:stop "停止")   
 (plt:hd:options "オプション") 
 (plt:hd:configure "設定")
 (plt:hd:home "ヘルプデスクのホーム") 
 (plt:hd:show-manuals "マニュアルの表示") 
 (plt:hd:send-bug-report "バグ報告の送信")
 (plt:hd:query-bug-reports "バグ報告の検索")
 ; next 3 are popup menu choices in help desk search frame
 (plt:hd:search-for-keyword "キーワードが")
 (plt:hd:search-for-keyword-or-index "キーワードか索引が")
 (plt:hd:search-for-keyword-or-index-or-text "キーワード,索引,テキストが")
 (plt:hd:exact-match "正確に一致")
 (plt:hd:containing-match "含んでいる")
 (plt:hd:regexp-match "正規表現にマッチ")
 (plt:hd:find-docs-for "検索:")
 (plt:hd:nothing-found-for-search-key "情報はありません \"~a\".")
 (plt:hd:searching "検索中...")
 (plt:hd:search-stopped "[検索を停止しました]")
 (plt:hd:search-stopped-too-many-matches "[検索を中止しました: ヒット件数が多過ぎます]")
 (plt:hd:nothing-found-for "情報はありません ~a")
 (plt:hd:error-finding-docs "ドキュメントが見つかりません\n\n~a")
 (plt:hd:and "and")
 (plt:hd:refresh "refresh")
 (plt:hd:refresh-all-manuals "refresh all manuals")
 (plt:hd:manual-installed-date "(installed ~a)")
 ; Help Desk configuration
 (plt:hd:configuration "PLT Help Desk configuration")
 (plt:hd:no-frames "No Frames")
 (plt:hd:use-frames "Use Frames")
 (plt:hd:use-html-frames "Use HTML frames")
 (plt:hd:search-pane-options "Search pane options")
 (plt:hd:height "Height")
 (plt:hd:bg-color "Background color")
 (plt:hd:pixels "pixels")
 (plt:hd:text-color "Text color")
 (plt:hd:link-color "Link color")
 (plt:hd:text-sample "Search pane text appears in this color")
 (plt:hd:link-sample "Search pane links appear in this color")
 (plt:hd:save-changes "Save changes")
 (plt:hd:reset "Reset")
 (plt:hd:defaults "Defaults")
 (plt:hd:javascript-note
    "The selections you make will be shown here if you have Javascript enabled and a recent, standards-compliant browser.")
 ;; refreshing manuals
 (plt:hd:refresh-downloading "Downloading ~a")
 (plt:hd:refresh-installing "Installing ~a")
 (plt:hd:refresh-progress "PLT manual download progress")
 (plt:hd:refresh-done "Done refreshing manuals")
 (plt:hd:refresh-installation-log "Installation log")
 (plt:hd:refresh-stopped "PLT manuals refresh stopped")
 (plt:hd:refreshing-manuals "Re-downloading Manuals")
 (plt:hd:refresh-downloading... "Downloading ~a...")
 (plt:hd:refresh-deleting... "Deleting old version of ~a...")
 (plt:hd:refresh-installing... "Installing new version of ~a...")
 (plt:hd:refresh-clearing-indicies "Clearing cached indices")
 (plt:hd:refreshing-manuals-finished "Finished.")
 (plt:hd:about-help-desk "ヘルプデスクについて")
 (plt:hd:help-desk-about-string
  "Help Desk is a complete source of information about PLT software, including DrScheme, MzScheme, and MrEd.\n\nVersion ~a\nCopyright (c) 1995-2003 PLT")
 (plt:hd:help-on-help "Help on Help")
 (plt:hd:help-on-help-details "For help on using Help Desk, follow the `How to use Help Desk' link on Help Desk's home page. (To get to the home page if you're not already there, click the `Home' button at the top of the Help Desk window.)")
  (reload "Reload") ;; refresh the page in a web browser
  (plt:hd:ask-about-separate-browser
   "You have selected a link to content from the world-wide web. Would you like to view it in the Help Desk browser, or would you like to use a separate browser program to view it?")
  (plt:hd:homebrew-browser "Help Desk Browser") ;; choice for the above string (in a button)
  (plt:hd:separate-browser "Separate Browser") ;; other choice for the above string (also in a button)
  (plt:hd:external-link-in-help "External URLs in Help")
  (plt:hd:use-homebrew-browser "Use Help Desk browser for external URLs")
  (plt:hd:new-help-desk "New Help Desk")
  (plt:hd:teaching-manuals "Student manuals")
  (plt:hd:professional-manuals "Professional manuals")
  (plt:hd:all-manuals "All manuals")

  ;; in the Help Desk language dialog, title on the right.
  (plt:hd:manual-search-ordering "Manual Search Order")


 ;; Help desk htty proxy
 (http-proxy "HTTP プロキシ")
 (proxy-direct-connection "直接接続")
 (proxy-use-proxy "プロキシを使う:")
 (proxy-host "ホスト")
 (proxy-port "ポート")
 (proxy-bad-host "Bad Proxy Host")

 ;; browser
 (rewind-in-browser-history "戻る")
 (forward-in-browser-history "進む")
 (home "ホーム")
 (browser "ブラウザ")
 (external-browser-choice-title "外部ブラウザ") ; title for radio-button set
 (browser-command-line-label "コマンドライン:") ; label for radio button that is followed by text boxes
 (choose-browser "Choose a Browser")
 (no-browser "Ask Later")
 (use-internal-browser-for-help "Read help with internal PLT browser") ; radio-button label
 (use-external-browser-for-help "Read help with external web browser") ; radio-button label
 (browser-cmdline-expl-line-1 "(Command line formed by concatenating pre-text, URL,") ; explanatory text for dialog, line 1
 (browser-cmdline-expl-line-2 "and post-text, with no extra spaces between them.)") ; ... line 2. (Anyone need more lines?)
 (cannot-display-url "Cannot display URL ~s: ~a")
 (install? "Install?")  ;; if a .plt file is found (title of dialog)
 (you-have-selected-an-installable-package "You have selected an installable package.")
 (do-you-want-to-install-it? "Do you want to install it?")
 (paren-file-size "(The file is ~a bytes)")
 (download-and-install "Download && Install") ;; button label
 (download "Download") ;; button label
 (save-downloaded-file/size "Save downloaded file (~a bytes) as") ;; label for get-file dialog
 (save-downloaded-file "Save downloaded file as")  ;; label for get-file dialog
 (downloading "Downloading") ;; dialog title
 (downloading-file... "Downloading file...")
 (package-was-installed "The package was installed.")
 (download-was-saved "The downloaded file was saved.")
 (getting-page "Getting Page") ;; dialog title

 (install-plt-file-menu-item... ".plt ファイルのインストール...")
 (install-plt-file-dialog-title ".plt ファイルのインストール")
 (install-plt-web-tab "ウェブ")
 (install-plt-file-tab "ファイル")
 (install-plt-filename "ファイル名:")
 (install-plt-url "URL:")
 
 ;; install plt file when opened in drscheme strings
 (install-plt-file "~a をインストールしますか？それとも編集用に開きますか？")
 (install-plt-file/yes "インストール")
 (install-plt-file/no "編集")

 (plt-installer-progress-window-title "インストールの進行状況") ;; frame title
 (plt-installer-abort-installation "インストールを中止") ;; button label
 (plt-installer-aborted "中止しました") ;; msg that appears in the installation window when installation is aborted

 ;;; about box
 (about-drscheme-frame-title "DrScheme について")
 (take-a-tour "ツアーの紹介！")
 (release-notes "リリースノート")
 (parenthetical-last-version "(previous version ~a)")
 (parenthetical-last-language "(previous language ~a)")
 (parenthetical-last-version/language "(previous version ~a, language ~a)")
 
 
 ;;; save file in particular format prompting.
 (save-as-plain-text "このファイルをプレーンテキストで保存しますか？")
 (save-in-drs-format "このファイルを DrScheme 専用のバイナリ形式で保存しますか？")
 (yes "はい")
 (no "いいえ")
 
 ;;; preferences
 (preferences "環境設定")
 (saving-preferences "保存の設定")
 (error-unmarshalling "Error unmarshalling ~a preference")
 (error-saving-preferences "Error saving preferences: ~a")
 (error-reading-preferences "Error reading preferences")
 (expected-list-of-length2 "expected a list of length 2")
 (scheme-prefs-panel-label "Scheme")
 (warnings-prefs-panel-label "警告")
 (editor-prefs-panel-label "編集")
 (general-prefs-panel-label "一般")
 (highlight-parens "対応する括弧の間を強調表示する")
 (fixup-parens "括弧を自動修正する")
 (flash-paren-match "対応する括弧をフラッシュする")
 (auto-save-files "ファイルを自動保存する")
 (backup-files "ファイルをバックアップする")
 (map-delete-to-backspace "Delete キーを Backspace キーとして処理する")
 (verify-exit "DrScheme 終了時に確認をとる")
 (ask-before-changing-format "保存形式を変更する前に確認をとる")
 (wrap-words-in-editor-buffers "エディタでテキストを折り返して表示する")
 (show-status-line "ステータス行を表示する")
 (count-columns-from-one "桁番号を 1 から数える")
 (display-line-numbers "Display line numbers in buffer; not character offsets")
 (enable-keybindings-in-menus "メニューのキーバインドを有効にする")
 (automatically-to-ps "自動的に PostScript ファイルに印刷する")
 (option-as-meta "Option キーを Meta キーとして処理する") ;; macos/macos x only
 (use-mdi "MDI ウィンドウを使用する") ;;; ms windows only -- use that window in a window thingy
 (separate-dialog-for-searching "Use separate dialog for searching")
 (reuse-existing-frames "Reuse existing frames when opening new files")
 (default-fonts "Default Fonts")
 (paren-match-color "Parenthesis highlight color") ; in prefs dialog
 (choose-color "Choose Color") ; in prefs dialog
 (online-coloring-active "Color syntax interactively")
 (open-files-in-tabs "Open files in separate tabs (not separate windows)")
 (show-interactions-on-execute "Automatically open interactions window when running a program")
 (limit-interactions-size "Limit interactions size")
 (background-color "Background color")
 (default-text-color "Default text") ;; used for configuring colors, but doesn't need the word "color"
 (choose-a-background-color "Please choose a background color")

 ; title of the color choosing dialog
 (choose-paren-highlight-color "Choose Parenthesis Highlight Color")

 ; should have entire alphabet
 (font-example-string "The quick brown fox jumped over the lazy dogs.") 

 (change-font-button-label "Change")
 (fonts "Fonts")

 ; filled with type of font, eg modern, swiss, etc.
 (choose-a-new-font "Please choose a new \"~a\" font")

 (font-size-slider-label "サイズ")
 (restart-to-see-font-changes "フォントの変更は再起動後に有効になります")

 (font-prefs-panel-title "フォント")
 (font-name "フォント名")
 (font-size "フォントサイズ")
 (set-font "Set Font...")
 (font-smoothing-label  "ﾌｫﾝﾄのｽﾑｰｼﾞﾝｸﾞ")
 (font-smoothing-none "なし")
 (font-smoothing-some "ある程度")
 (font-smoothing-all "すべて")
 (font-smoothing-default "システムの既定値を使う")
 (select-font-name "フォント名の選択")
 (example-text "サンプル テキスト:")
 (only-warn-once "定義と対話が同期していない場合、一度だけ警告する。")
 
 ; warning message when lockfile is around
 (waiting-for-pref-lock "環境設定のロックファイルを待っています...")
 (pref-lock-not-gone
  "The preferences lockfile:\n\n   ~a\n\nprevents the preferences from being saved. Ensure that no PLT software is running and delete this file.")
 (still-locked-exit-anyway? "環境設定が保存できませんでした。とりあえず終了しますか？")
 
 ;;; indenting preferences panel
 (indenting-prefs-panel-label "インデント")
 (indenting-prefs-extra-regexp "Extra regexp")

 ; filled with define, lambda, or begin
 (enter-new-keyword "Enter new ~a-like keyword:")
 (x-keyword "~a Keyword")
 (x-like-keywords "~a-like Keywords")

 (expected-a-symbol "expected a symbol, found: ~a")
 (already-used-keyword "\"~a\" is already a specially indented keyword")
 (add-keyword "追加")
 (remove-keyword "削除")
 
 ;;; find/replace
 (find-and-replace "検索と置換")
 (find "検索")
 (replace "置換")
 (dock "結合")
 (undock "分離")
 (use-separate-dialog-for-searching "Use separate dialog for searching")
 (replace&find-again "Replace && Find Again") ;;; need double & to get a single &
 (replace-to-end "Replace to End")
 (forward "前方")
 (backward "後方")
 (hide "Hide")
 
 ;;; multi-file-search
 (mfs-multi-file-search-menu-item "Search in Files...")
 (mfs-string-match/graphics "String match (handles files with graphics)")
 (mfs-regexp-match/no-graphics "Regular Expression (only raw text files)")
 (mfs-searching... "Searching...")
 (mfs-configure-search "Configure Search") ;; dialog title
 (mfs-files-section "Files")   ;; section in config dialog
 (mfs-search-section "Search") ;; section in config dialog
 (mfs-dir "Dir")
 (mfs-recur-over-subdirectories "Recur over subdirectories")
 (mfs-regexp-filename-filter "Regexp filename filter")
 (mfs-search-string "Search string")
 (mfs-drscheme-multi-file-search "DrScheme - Multi File Search") ;; results window and error message title
 (mfs-not-a-dir "\"~a\" is not a directory")
 (mfs-open-file "Open File")
 (mfs-stop-search "Stop Search")
 (mfs-case-sensitive-label "Case sensitive")
 (mfs-no-matches-found "No matches found.")
 (mfs-search-interrupted "Search aborted.")
 
 ;;; reverting a file
 (error-reverting "DrScheme - Error Reverting")
 (could-not-read "could not read \"~a\"")
 (are-you-sure-revert
  "Are you sure that you want to revert this file? This change cannot be undone.")
 (are-you-sure-revert-title
  "Revert?")
 
 ;;; saving a file
 ; ~a is filled with the filename
 (error-saving "保存に失敗") ;; title of error message dialog
 (error-saving-file/name "~a を保存中にエラーが発生しました")
 (error-loading "ロードに失敗")
 (error-loading-file/name "~a をロード中にエラーが発生しました")
 (unknown-filename "<< unknown >>")

 ;;; finder dialog
 (must-specify-a-filename "You must specify a file name")
 (file-does-not-exist "The file \"~a\" does not exist.")
 (ask-because-file-exists "The file \"~a\" already exists. Replace it?")
 (dne-or-cycle "The file \"~a\" contains a nonexistent directory or a cycle.")
 (get-file "Get file")
 (put-file "Put file")
 (full-pathname "Full pathname")
 (show-dot-files "Show files and directories that begin with a dot.")
 (up-directory-button-label "Up directory")
 (add-button-label "Add") ;;; for multi-file selection
 (add-all-button-label "Add all") ;;; for multi-file selection
 (remove-button-label "Remove") ;;; for multi-file selection
 (file-wrong-form "That filename does not have the right form.")
 (select-files "ファイルの選択")
 (select-file "ファイルの選択")
 (dir-dne "そのディレクトリは存在しません。")
 (file-dne "そのファイルは存在しません。")
 (empty-filename "The filename must have some letters in it.")
 (that-is-dir-name "それはディレクトリ名です。")
 
 ;;; raw menu names -- these must match the 
 ;;; versions below, once the &s have been stripped.
 ;;; if they don't, DrScheme's menus will appear
 ;;; in the wrong order.
 (file-menu "ファイル(F)")
 (edit-menu "編集(E)")
 (help-menu "ヘルプ(H)")
 (windows-menu "ウィンドウ(W)")
 
 ;;; menus
 ;;; - in menu labels, the & indicates a alt-key based shortcut.
 ;;; - sometimes, things are stuck in the middle of 
 ;;; menu item labels. For instance, in the case of
 ;;; the "Save As" menu, you might see: "Save Definitions As". 
 ;;; be careful of spacing, follow the English, if possible.
 ;;; - the ellipses in the `after' strings indicates that
 ;;; more information is required from the user before completing
 ;;; the command.

(file-menu-label "ファイル(&F)")

 (new-info  "新規ファイルを開きます")
 (new-menu-item "新規作成(&N)")
 (new-...-menu-item "新規作成(&N)...")

 (open-info "ディスクからファイルを開きます")
 (open-menu-item "開く(&O)...")
 (open-here-menu-item "ここに開く(&O)...")

 (open-recent-info "最近開いたファイルの一覧を表示します")
 (open-recent-menu-item "最近開いたファイル")
 
 (revert-info "このファイルをディスク上のファイルに戻します")
 (revert-menu-item "復元(&R)")

 (save-info "このファイルをディスクに保存します")
 (save-menu-item "保存(&S)")

 (save-as-info "このファイルにファイル名を指定してディスクに保存します")
 (save-as-menu-item "名前を付けて保存(&A)...")

 (print-info "このファイルをプリンタで印刷します")
 (print-menu-item "印刷(&P)...")

 (close-info "このファイルを閉じます")
 (close-menu-item "閉じる(&C)")

 (quit-info "すべてのウィンドウを閉じます")
 (quit-menu-item-windows "終了(&X)")
 (quit-menu-item-others "&Quit")
 
 (edit-menu-label "編集(&E)")
 
 (undo-info "直前の動作を元に戻します")
 (undo-menu-item "元に戻す(&U)")

 (redo-info "元に戻した直前の動作を再実行します")
 (redo-menu-item "やり直し(&R)")

 (cut-info "選択項目を切り取りクリップボードへコピーします")
 (cut-menu-item "切り取り(&U)")

 (copy-info "選択項目をクリップボードへコピーします")
 (copy-menu-item "コピー(&C)")

 (paste-info "現在の選択項目に、直前にコピーまたは切り取られた項目を貼り付けます")
 (paste-menu-item "貼り付け(&P)")

 (clear-info "選択項目を削除します")
 (clear-menu-item-others "削除")
 (clear-menu-item-windows "削除(&D)")

 (select-all-info "文書全体を選択します")
 (select-all-menu-item "すべて選択(&L)")
 
 (find-info "文字列を検索します")
 (find-menu-item "検索...")

 (find-again-info "直前の検索文字列と同じ文字列を検索します")
 (find-again-menu-item "再検索")
 
 (replace-and-find-again-info "現在のテキストを置換し、直前の検索文字列と同じ文字列を検索します")
 (replace-and-find-again-menu-item "置換と再検索")

 (preferences-info "環境設定を行います")
 (preferences-menu-item "環境設定...")

 (keybindings-info "現在有効なキーバインドを表示します")
 (keybindings-menu-item "キーバインド")
 (keybindings-show-active "現在有効なキーバインドを表示")
 (keybindings-frame-title "キーバインド")
 (keybindings-sort-by-name "名前で並べ替え")
 (keybindings-sort-by-key "キーで並べ替え")
 (keybindings-add-user-defined-keybindings "ユーザー定義のキーバインドを追加...")
 (keybindings-menu-remove "~a を削除")
 (keybindings-choose-user-defined-file "キーバインドを記述したファイルを選択してください")

 (user-defined-keybinding-error "キーバインド ~a\n\n~a を実行中にエラーが発生しました")
 (user-defined-keybinding-malformed-file "ファイル ~a には、言語 (lib \"keybinding-lang.ss\" \"framework\") で書かれたモジュールが含まれていません。")  
  
 ;; menu items in the "special" menu
 (insert-text-box-item "テキストボックスを挿入")
 (insert-pb-box-item "ペーストボード ボックスを挿入")
 (insert-image-item "画像を挿入...")
 (insert-comment-box-menu-item-label "コメント ボックスを挿入")
 (insert-lambda "ラムダ(λ)を挿入")
 (insert-delta "デルタ(定義)を挿入(&D)")

 (wrap-text-item "テキストを折り返す")

 (windows-menu-label "ウィンドウ(&W)")
 (bring-frame-to-front "フレームを前面に移動")       ;;; title of dialog
 (bring-frame-to-front... "フレームを前面に移動...") ;;; corresponding title of menu item
 (next-window "次のウィンドウ")
 (previous-window "前のウィンドウ")
 (most-recent-window "最近使用したウィンドウ")

 (view-menu-label "表示(&V)")
 (show-overview "プログラムの外観を表示") 
 (hide-overview "プログラムの外観を非表示")
 (show-module-browser "モジュール ブラウザを表示")
 (hide-module-browser "モジュール ブラウザを非表示")

 (help-menu-label "ヘルプ(&H)")
 (about-info "このアプリケーションの著作権と詳細情報を表示します")
 (about-menu-item "バージョン情報...")
 (help-menu-check-for-updates "アップデートの確認...")
 
 ;; open here's new menu item
 (create-new-window-or-clear-current
  "新規ウィンドウを作成しますか？それとも、現在のウィンドウを消去しますか？")
 (clear-current "現在のウィンドウを消去")
 (new-window "新規ウィンドウを作成")

 ;;; exiting and quitting ``are you sure'' dialog
 ;;; exit is used on windows, quit on macos, in English. Other
 ;;; languages probably use the same word on both platforms.
 (exit "終了")
 (quit "終了")
 (are-you-sure-exit "終了してよろしいですか？")
 (are-you-sure-quit "終了してよろしいですか？")
 
 ;;; autosaving
 (error-autosaving "\"~a\" を自動保存中にエラーが発生しました。") ;; ~a will be a filename
 (autosaving-turned-off "このファイルが保存されるまで、\n自動保存は無効になります。")
 (recover-autosave-files-frame-title "自動保存ファイルの復元")
 (autosave-details "詳細")
 (autosave-recover "復元")
 (autosave-unknown-filename "<<unknown>>")
  
  ;; these are labels in a dialog that drscheme displays
  ;; if you have leftover autosave files. to see the dialog,
  ;; start up drscheme and modify (but don't save) a file
  ;; (also, do this with an unsaved file). Wait for the autosave
  ;; files to appear (typically 5 minutes). Kill DrScheme
  ;; and restart it. You'll see the dialog
  (autosave-autosave-label: "自動保存ファイル:")
  (autosave-original-label: "元のファイル:")
  (autosave-autosave-label "自動保存ファイル")
  (autosave-original-label "元のファイル")
  (autosave-compare-files "自動保存ファイルの比較")

  (autosave-show-autosave "自動保存ファイル") ;; title of a window showing the autosave file

  (autosave-explanation "DrScheme は自動保存ファイルを検出しました。自動保存ファイルには、未保存の作業結果が含まれている可能性があります。")

  (autosave-recovered! "復元しました！") ;; status of an autosave file
  (autosave-deleted "削除しました")       ;; status of an autosave file

  (autosave-error-deleting "~a\nを削除中にエラーが発生しました。\n~a") ;; first is a filename, second is an error message from mz.
  (autosave-delete-button "削除")
  (autosave-delete-title "削除")  ;; title of a dialog asking for deletion confirmation
  (autosave-done "Done")
  
  ;; appears in the file dialog
  (autosave-restore-to-where? "自動保存ファイルを保存する場所を選択してください。")
  
  
 ;;; file modified warning
 (file-has-been-modified
  "このファイルは、前回保存された後に変更されています。変更を上書きしますか？")
 (overwrite-file-button-label "上書き")
 
 (definitions-modified 
  "The definitions text has been modified in the file-system; please save or revert the definitions text.")
 (drscheme-internal-error "DrScheme 内部エラー")
 
 ;;; tools
 (invalid-tool-spec "The tool specification in collection ~a's info.ss file is invalid. Expected either a string or a non-empty list of strings, got: ~e")
 (error-loading-tool-title "DrScheme - Error loading tool ~s; ~s")
 (error-invoking-tool-title "Error invoking tool ~s;~s")
 (tool-tool-names-same-length
  "expected `tool-names' and `tools' to be lists of the same length, in info.ss file for ~s, got ~e and ~e")
 (tool-tool-icons-same-length
  "expected `tool-icons' and `tools' to be lists of the same length, in info.ss file for ~s, got ~e and ~e")
 (tool-tool-urls-same-length
  "expected `tool-urls' and `tools' to be lists of the same length, in info.ss file for ~s, got ~e and ~e")
 (error-getting-info-tool
  "error loading info.ss file for ~s")
 (tool-error-phase1 "Error in phase 1 for tool ~s; ~s")
 (tool-error-phase2 "Error in phase 2 for tool ~s; ~s")


 ;;; define popup menu
 (end-of-buffer-define "<< バッファ終端 >>")
 (sort-by-name "名前で並び替え")
 (sort-by-position "ファイル内の位置で並び替え")
 (no-definitions-found "<< 定義がありません >>")
 (jump-to-defn "~a の定義に移動")

 (recent-items-sort-by-age "日時で並び替え")
 (recent-items-sort-by-name "名前で並び替え")
 
 ;;; view menu
 (hide-definitions-menu-item-label "定義を非表示(&D)")
 (show-definitions-menu-item-label "定義を表示(&D)")
 (definitions-menu-item-help-string "定義ウィンドウを表示/非表示します")
 (show-interactions-menu-item-label "対話を表示(&I)")
 (hide-interactions-menu-item-label "対話を非表示(&I)")
 (interactions-menu-item-help-string "対話ウィンドウを表示/非表示します")
 (show-toolbar "ツールバーを表示(&T)")
 (hide-toolbar "ツールバーを非表示(&T)")

 ;;; file menu
 (save-definitions-as "定義に名前を付けて保存(&A)...")
 (save-definitions "定義の保存")
 (print-definitions "定義を印刷...")
 (about-drscheme "DrSchemeについて")
 (save-other "その他の保存")
 (save-definitions-as-text "定義をテキストに保存...")
 (save-interactions "対話の保存")
 (save-interactions-as "対話に名前を付けて保存...")
 (save-interactions-as-text "対話をテキストに保存...")
 (print-interactions "対話を印刷...")
 (new-tab "新規タブ")
 (close-tab "タブを閉じる") ;; must not have any &s in it.
 
 ;;; edit-menu
 (split-menu-item-label "分割(&S)")
 (collapse-menu-item-label "分割解除(&O)")
 
 ;;; language menu
 (language-menu-name "言語(&L)")
 
 ;;; scheme-menu
 (scheme-menu-name "S&cheme")
 (execute-menu-item-label "実行")
 (execute-menu-item-help-string "定義ウィンドウのプログラムを再開始します")
 (break-menu-item-label "停止")
 (break-menu-item-help-string "現在の評価を停止します")
 (kill-menu-item-label "強制終了")
 (kill-menu-item-help-string "現在の評価を強制終了します")
 (clear-error-highlight-menu-item-label "エラー強調表示を消去")
 (clear-error-highlight-item-help-string "ピンク色のエラー強調表示を消去します")
 (reindent-menu-item-label "再インデント(&R)")
 (reindent-all-menu-item-label "すべてを再インデント(&A)")
 (semicolon-comment-out-menu-item-label "セミコロンでコメントアウト(&C)")
 (box-comment-out-menu-item-label "ボックスでコメントアウト(&C)")
 (uncomment-menu-item-label "コメント解除(&U)")

 (convert-to-semicolon-comment "セミコロン コメントに変換")
 
 ;;; executables
 (create-executable-menu-item-label "実行ファイルの作成...")
 (create-executable-title "実行ファイルの作成")
 (must-save-before-executable "実行ファイルを作成する前に、プログラムを保存してください。")
 (save-an-executable "実行ファイルの保存")
 (save-a-mred-launcher "MrEd ランチャの保存")
 (save-a-mzscheme-launcher "MzScheme ランチャの保存")
 (save-a-mred-stand-alone-executable "MrEd スタンドアロン実行ファイルの保存")
 (save-a-mzscheme-stand-alone-executable "MzScheme スタンドアロン実行ファイルの保存")

 (definitions-not-saved "定義ウィンドウが保存されていません。実行ファイルでは定義ウィンドウの最新の保存が使われます。よろしいですか？")
 (inline-saved-program-in-executable?
  "Inline the saved program in the executable? If yes, you can copy the executable to another ~a computer but the executable will be quite large. If not, you cannot copy the executable to another computer, but it will be much smaller. Additionally, if not, the executable will load the latest version of the program.")
 (use-mred-binary?
  "Use the mred binary for this executable?\n\nIf yes, your program can use the (lib \"mred.ss\" \"mred\") library. If no, DrScheme will use mzscheme as the binary for this executable and you cannot use that library.\n\nIf unsure, choose yes.")
 (inline-saved-program-in-executable/windows/path
   "WARNING! The generated executable relies on three DLLs: libmred.dll, libmzsch.gll, and libgc.dll, which are located at\n\n~a\n\nThe executable finds the DLLs either in the executable's directory or through the PATH enviornment variable.\n\nWhen you installed DrScheme, the installer adjusted the user's PATH to include the directory where the DLLs were installed. Beware of configuration or user changes since installation.\n\nIf you move the executable to another machine, you must also copy the DLLs to the other machine --- either to the same directory as the executable, or to a directory in the other machine's PATH.")
 (launcher "ランチャ")
 (stand-alone "スタンドアロン")
 (executable-type "Type")
 (executable-base "Base")
 (filename "ファイル名: ")
 (create "作成")
 (please-choose-an-executable-filename "Please choose a filename to save the executable.")
 (windows-executables-must-end-with-exe
  "The filename\n\n  ~a\n\nis illegal. Under Windows, executables must end with .exe.")
 (macosx-executables-must-end-with-app
  "The filename\n\n  ~a\n\nis illegal. Under MacOS X, executables must end with .app.")
 (warning-directory-will-be-replaced
  "WARNING: the directory:\n\n  ~a\n\nwill be replaced. Proceed?")
 
 (create-servlet "サーブレットの作成...")

 ; the ~a is a language such as "module" or "algol60"
 (create-servlet-unsupported-language
  "言語 ~a ではサーブレットの作成はできません。")
  
 ;;; buttons
 (execute-button-label "実行") 
 (save-button-label "保存")
 (break-button-label "停止")
 
 ;;; search help desk popup menu
 (search-help-desk-for "Search in Help Desk for \"~a\"")
 (exact-lucky-search-help-desk-for "Exact lucky search in Help Desk for \"~a\"")

 ;; collapse and expand popup menu items
 (collapse-sexp "S-式を縮小")
 (expand-sexp "S-式を展開")
 
 ;;; fraction dialog
 (enter-fraction "Enter Fraction")
 (whole-part "Whole Part")
 (numerator "Numerator")
 (denominator "Denominator")
 (invalid-number "Invalid number: must be an exact, real, non-integral number.")
 (insert-fraction-menu-item-label "Insert Fraction...")

 ;; number snip popup menu
 (show-decimal-expansion "View decimal expansion")
 (show-fraction-view "View as fraction")
 (show-mixed-fraction-view "View as mixed fraction")
 (show-improper-fraction-view "View as improper fraction")
 (show-more-decimal-places "Show more decimal places")
 
 ;;; Teachpack messages
 (select-a-teachpack "ティーチパックを選択")
 (clear-teachpack "Clear ~a Teachpack")
 (teachpack-error-label "DrScheme - Teachpack error")
 (teachpack-dne/cant-read "The teachpack file ~a does not exist or is not readable.")
 (teachpack-didnt-load "The teachpack file ~a did not load properly.")
 (teachpack-error-invoke "The teachpack file ~a raised an error when invoked.")
 (add-teachpack-menu-item-label "Add Teachpack...")
 (clear-all-teachpacks-menu-item-label "Clear All Teachpacks")
 (drscheme-teachpack-message-title "DrScheme Teachpack")
 (already-added-teachpack "Already added ~a teachpack")
 
 ;;; Language dialog
 (introduction-to-language-dialog
  "Please select a language. Students in most introductory courses should use the default language.")
 (language-dialog-title "Choose Language")
 (case-sensitive-label "Case sensitive")
 (output-style-label "Output Style")
 (constructor-printing-style "Constructor")
 (quasiquote-printing-style "Quasiquote")
 (write-printing-style "write")
 (print-printing-style "current-print")
 (sharing-printing-label "Show sharing in values")
 (use-pretty-printer-label "Insert newlines in printed values")
 (input-syntax "Input Syntax")
 (dynamic-properties "Dynamic Properties")
 (output-syntax "Output Syntax")
 (no-debugging-or-profiling "No debugging or profiling")
 (debugging "Debugging")
 (debugging-and-profiling "Debugging and profiling")
 (test-coverage "Syntactic test suite coverage")
 (whole/fractional-exact-numbers-label "Print numbers as fractions")
 (booleans-as-true/false-label "Print booleans using true and false")
 (show-details-button-label "詳細を表示")
 (hide-details-button-label "詳細を非表示")
 (choose-language-menu-item-label "言語の選択...")
 (revert-to-language-defaults "Revert to Language Defaults")
 (language-docs-button-label "Language Docs")
 (fraction-style "Fraction Style")
 (use-mixed-fractions "Mixed fractions")
 (use-repeating-decimals "Repeating decimals")
 (decimal-notation-for-rationals "Use decimal notation for rationals")
 (please-select-a-language "Please select a language")

 
 ;;; languages
 (beginning-student "Beginning Student")
 (beginning-one-line-summary "define, cond, structs, constants, and primitives")
 (beginning-student/abbrev "Beginning Student with List Abbreviations")
 (beginning/abbrev-one-line-summary "Beginner, with list style printing in the REPL")
 (intermediate-student "Intermediate Student")
 (intermediate-one-line-summary "Beginner plus lexical scope")
 (intermediate-student/lambda "Intermediate Student with lambda")
 (intermediate/lambda-one-line-summary "Intermediate plus higher-order functions")
 (advanced-student "Advanced Student")
 (advanced-one-line-summary "Intermediate plus lambda and mutation")
 (full-language "Full") ;; also in the HtDP languages section
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (r5rs-like-languages "R5RS-like")
 (pretty-big-scheme "Pretty Big (includes MrEd and Advanced)")
 (pretty-big-scheme-one-line-summary "Adds syntax and functions from the HtDP languages")
 (r5rs-lang-name "Standard (R5RS)")
 (r5rs-one-line-summary "R5RS, with no frills")
 (expander "Expander")
 (expander-one-line-summary "Expands, rather than evaluates, expressions")
 (professional-languages "Professional Languages")
 (teaching-languages "Teaching Languages")
 (experimental-languages "Experimental Languages")
 
 (module-language-one-line-summary "Run creates a REPL in the context of the module, including the module's declared language")
  

 ;;; debug language
 (unknown-debug-frame "[unknown]")
 (backtrace-window-title "Backtrace - DrScheme")
 (files-interactions "~a's interactions") ;; filled with a filename
 (current-interactions "interactions")
 (current-definitions "definitions")
 (mzscheme-w/debug "Textual (MzScheme, includes R5RS)")
 (mzscheme-one-line-summary "PLT's implementation Scheme")
 (mred-w/debug "Graphical (MrEd, includes MzScheme)")
 (mred-one-line-summary "Adds GUI support to MzScheme")

 ;; profiling
 (profiling-low-color "低い")
 (profiling-high-color "高い")
 (profiling-choose-low-color "低いPlease select a low color")
 (profiling-choose-high-color "Please select a high color")
 (profiling "プロファイル")
 (profiling-example-text "(define (whee) (whee))")
 (profiling-color-config "プロファイルのカラー範囲") 
 (profiling-scale "プロファイルのカラー尺度")
 (profiling-sqrt "平方根")
 (profiling-linear "線型")
 (profiling-square "二乗")
 (profiling-number "呼び出しの回数")
 (profiling-time "累積時間")
 (profiling-clear "プロファイルの消去")
 (profiling-update "プロファイルの更新")
 (profiling-col-percent-time "% Time")
 (profiling-col-function "関数")
 (profiling-col-name "名前")
 (profiling-col-time-in-msec "ミリ秒")
 (profiling-col-calls "Calls")
 (profiling-show-profile "プロファイルを表示")
 (profiling-hide-profile "プロファイルを非表示")
 (profiling-unknown-src "<< unknown >>")
 (profiling-no-information-available "プロファイル情報がありません。お使いの言語でプロファイリングを有効にし、プロファイル対象のプログラムを実行してください。")
 (profiling-clear? "定義ウィンドウを書き換えると、プロファイル情報が無効になります。よろしいですか？")
 
 ;; test coverage
 (test-coverage-clear? "定義ウィンドウを書き換えると、テスト カバレージ情報が無効になります。よろしいですか？")
 (test-coverage-clear-and-do-not-ask-again "Yes, and don't ask again")
 (test-coverage-ask? "Ask about clearing test coverage")
  
 ;; tracing
 (tracing-enable-tracing "トレースを有効にする")
 (tracing-show-tracing-window "トレースを表示")
 (tracing-hide-tracing-window "トレースを非表示")
 (tracing-tracing-nothing-to-show "トーレス結果がありません。お使いの言語でトレースがサポートされていることを確認し、トレースを有効にしてください。")

 ;;; repl stuff
 (evaluation-terminated "評価が終了しました。")
 (evaluation-terminated-explanation
  "評価スレッドはもう実行されていませんので、次に実行するまでは評価は行われません。")
 (last-stack-frame "最後のスタックフレームを表示する")
 (last-stack-frames "最後の  ~a スタックフレームを表示する")
 (next-stack-frames "次の ~a スタックフレームを表示する")
 
 ;;; welcoming message in repl
 (language "言語")
 (custom "カスタム")
 (teachpack "ティーチパック")
 (welcome-to "ようこそ")
 (version "バージョン")
 
 ;;; kill evaluation dialog
 (kill-evaluation? "評価を強制終了しますか？")
 (just-break "Just Break")
 (kill "強制終了")
 (kill? "強制終了？")

 ;;; version checker
 ;; the next two are used in the initial wizard dialog.
 ;; Note that vc-wizard-check-prompt can (should) have newlines so
 ;; it will not make the dialog too wide.
 (vc-wizard-check-note "The version you are about to install may not be the latest\n one. If you would like, DrScheme can check for you.")
 (vc-wizard-check-button "Check for Updates")
 (vc-update-check "Update check")
 (vc-please-wait "Please wait")
 (vc-connecting-version-server "Connecting to PLT version server")
 (vc-network-timeout "Network timeout") 
 (vc-cannot-connect  "Can't connect to PLT version server")
 (vc-network-failure "Network failure")
 (vc-old-binaries "Installed binaries for DrScheme (or MzScheme) are not up-to-date")
 (vc-binary-information-format "Installed binary version: ~a (iteration ~a)")
 (vc-details-format "~a~nDetails:~n~a")
 (vc-details-text "Details:~n")
 (vc-error-format "Error: ~a") 
 (vc-current-format "~a v.~a (iteration ~a) is up-to-date")
 (vc-update-format "~a v.~a (iteration ~a) needs updating to v.~a (iteration ~a)")
 (vc-binary-name "Binary")
 (vc-updates-available "Updates are available at")
 (vc-latest-binary-information-format "Latest released version: ~a (iteration ~a)")
 (vc-update-dialog-title "PLT update status")
 (vc-need-update-string "One or more installed PLT software packages needs updating")
 (vc-no-update-string "All installed PLT software packages are up-to-date")

 ;; special menu
 (special-menu "特殊(&P)")
 
 ;; large semi colon letters
 (insert-large-letters... "大きな文字を挿入...")
 (large-semicolon-letters "大きなセミコロン")
 (text-to-insert "挿入する文字列")

 (module-browser-filename-format "完全なファイル名: ~a (~a 行)")
 (module-browser-root-filename "ルート ファイル名: ~a")
 (module-browser-font-size-gauge-label "フォント サイズ")
 (module-browser-progress-label "Module overview progress")
 (module-browser-adding-file "ファイルを追加しています: ~a...")
 (module-browser-laying-out-graph-label "グラフを配置しています")
 (module-browser-open-file-format "開く ~a")
 (module-browser "モジュール ブラウザ") ;; frame title
 (module-browser... "モジュール ブラウザ...") ;; menu item title
 (module-browser-error-expanding "Error expanding the program:\n\n~a")
 (module-browser-show-lib-paths "Show files loaded by (lib ..) paths")
 (module-browser-progress "Module Browser: ~a") ;; prefix in the status line
 (module-browser-compiling-defns "Module Browser: compiling definitions")
 (module-browser-show-lib-paths/short "Follow lib requires") ;; check box label in show module browser pane in drscheme window.
 (module-browser-refresh "Refresh") ;; button label in show module browser pane in drscheme window.
 (module-browser-only-in-plt-and-module-langs
  "The module browser is only available for programs in the PLT languages and in the module language (and only for programs that have modules in them).")
 (module-browser-name-length "名前の長さ")
 (module-browser-name-short "短い")
 (module-browser-name-medium "普通")
 (module-browser-name-long "長い")
 (module-browser-open-all "ここに表示されているすべてのファイルを開く")

 (happy-birthday-matthias "お誕生日おめでとう, Matthias!")
 (happy-birthday-matthew "お誕生日おめでとう, Matthew!")
 (happy-birthday-shriram "お誕生日おめでとう, Shriram!")

 (mrflow-using-default-language-title "既定で使用する言語")
 (mrflow-using-default-language "現在使用している言語には、そのプリミティブ用の型テーブルがありません。代替として R5RS Scheme を使います。")
 (mrflow-button-title "解析")
 ;(mrflow-unknown-style-delta-error-title "Unknown Box Style Delta")
 ;(mrflow-unknown-style-delta-error "Unknown box style delta: ~a")
 (mrflow-coloring-error-title "未知のカラー")
 (mrflow-coloring-error "No style defined for color: ~a")
 (mrflow-popup-menu-show-type "Show Type")
 (mrflow-popup-menu-hide-type "Hide Type")
 (mrflow-popup-menu-show-errors "表示エラー")
 (mrflow-popup-menu-hide-errors "非表示エラー")
 ;(mrflow-read-exception-title "Read Exception")
 ;(mrflow-read-exception "Read exception: ~a")
 ;(mrflow-syntax-exception-title "Syntax Exception")
 ;(mrflow-syntax-exception "Syntax exception: ~a")
 ;(mrflow-unknown-exception-title "Unknown Exception")
 ;(mrflow-unknown-exception "Unknown exception: ~a")
 ;(mrflow-language-primitives-error-title "Language Primitives Error")
 ;(mrflow-language-primitives-error "Wrong filename for language primitives types table: ~a")
  
 (snips-and-arrows-popup-menu-tack-all-arrows "Tack All Arrows")
 (snips-and-arrows-popup-menu-untack-all-arrows "Untack All Arrows")
 (snips-and-arrows-user-action-disallowed-title "User Changes Currently Disallowed")
 (snips-and-arrows-user-action-disallowed "User changes are disallowed in editors that contain tool-inserted snips.  Hide all snips before modifying the content of the editor.")
 ;(snips-and-arrows-changing-terms-warning-title "Changing terms will be undoable")
 ;(snips-and-arrows-changing-terms-warning "Changing terms in an editor containing snips cannot be undone.  You can either cancel this action, remove the snips, and try the change again, or you can continue with the change, in which case the change will not be undoable (all others changes made before and afterward will still be undoable though).")
 (snips-and-arrows-hide-all-snips-in-editor "Hide all snips in editor")

 (xml-tool-menu "XML")
 (xml-tool-insert-xml-box "XML ボックスを挿入")
 (xml-tool-insert-scheme-box "Scheme ボックスを挿入")
 (xml-tool-insert-scheme-splice-box "Scheme Splice ボックスを挿入")
 (xml-tool-xml-box "XML ボックス")
 (xml-tool-scheme-box "Scheme ボックス")
 (xml-tool-scheme-splice-box "Scheme Splice ボックス")
 (xml-tool-switch-to-scheme "Scheme ボックスに切り替え")
 (xml-tool-switch-to-scheme-splice "Scheme Splice ボックスに切り替え")
 (xml-tool-eliminate-whitespace-in-empty-tags
  "空タグ内の空白を削除")
 (xml-tool-leave-whitespace-alone
  "空白を残す")
 
 (show-recent-items-window-menu-item "最近開いたファイルを別ウィンドウに表示する")
 (show-recent-items-window-label "最近開いたファイル")
 (number-of-open-recent-items "Number of recent items")
 (switch-anyway "Switch File Anyway")

 (stepper-program-has-changed "警告: プログラムが変更されました。")
 (stepper-program-window-closed "警告: プログラム ウィンドウが閉じました。")

 (stepper-home "Home")
 (stepper-name "Stepper")
 (stepper-language-level-message
  "The language level is set to \"~a\". Currently, the stepper works only for the \"~a\" through the \"~a\" language levels.")
 (stepper-button-label "Step")
 (stepper-previous-application "|< Application")
 (stepper-previous "< Step")
 (stepper-next "Step >")
 (stepper-next-application "Application >|")
 

 (wizard-next "次へ")
 (wizard-back "戻る")
 (wizard-finish "完了")

 ;; warnings about closing a drscheme frame when the program
 ;; might still be doing something interesting
 (program-is-still-running "定義ウィンドウ内のプログラムは実行中です。強制的に閉じますか？")
  (program-has-open-windows "定義ウィンドウ内のプログラムはウィンドウを開いています。このウィンドウを強制的に閉じますか？")
 
  ;; ml-command-line-arguments is for the command line arguments
  ;; label in the module language details in the language dialog.
  (ml-command-line-arguments "Command-line arguments as a vector of strings, in read syntax")

  ;; ml-cp names are all for the module language collection path
  ;; configuration. See the details portion of the language dialog
  ;; for the module language (at the bottom).
  (ml-cp-default-collection-path "<<default collection paths>>")

  ;; in std get-directory 
  (ml-cp-choose-a-collection-path "Please choose a collection path")

  ;; err msg when adding default twice
  (ml-cp-default-already-present
   "Default collection paths are already present")
  
  ;; title of this section of the dialog (possibly the word
  ;; `Collection' should not be translated)
  (ml-cp-collection-paths "Collection Paths")

  ;; button labels
  (ml-cp-add "Add")
  (ml-cp-add-default "Add Default")
  (ml-cp-remove "Remove")
  (ml-cp-raise "Raise")
  (ml-cp-lower "Lower")

  ;; Profj
  (profj-java "Java")
  (profj-java-mode "Java モード")
  (profj-java-mode-color-keyword "キーワード")
  (profj-java-mode-color-string "文字列")
  (profj-java-mode-color-literal "リテラル")
  (profj-java-mode-color-comment "コメント")
  (profj-java-mode-color-error "エラー")
  (profj-java-mode-color-identifier "識別子")
  (profj-java-mode-color-default "デフォルト")

  (profj-insert-java-comment-box "Java コメント ボックスを挿入")
  (profj-insert-java-interactions-box "Java 対話ボックスを挿入")

  ;; The Test Suite Tool
  ;; Errors
  (test-case-empty-error "Empty test case")
  (test-case-too-many-expressions-error "Too many expressions in a test case.")
  (test-case-not-at-top-level "Test case box not at top level")
  ;; Dr. Scheme window menu items
  (test-case-insert "Insert Test Case")
  (test-case-disable-all "Disable all Test Cases")
  (test-case-enable-all "Enable all Test Cases")
  
  ;; NOTE: The following string constants are labels of the test-case fields. The width
  ;;       of the field is determined by the length of the longest of the following three words.
  ;;       if the words are too long the test case will take up too much horizontal room and
  ;;       not look very good.
  ;; This string is the label of the expression that is being tested in a test case.
  (test-case-to-test "Test")
  ;; This string is the label of the expression that is the expected value of the to-test expression.
  (test-case-expected "Should be")
  ;; This string is the label of the actual result of the to test expression.
  (test-case-actual "Actual")
  (test-case-predicate "Predicate")
  (test-case-should-raise "Should Raise")
  ;; The label of a field of the test-case that describes the expected error message of a test case
  (test-case-error-message "Error Message")

  (test-case-menu-title "Test Case")
  (test-case-switch-to-error-box "Switch to Error Test Box")
  (test-case-switch-to-nonerror-box "Switch to Nonerror Test box")
  (test-case-collapse "Collapse Test Case")
  (test-case-show-actual "Show Actual Value")
  (test-case-enable "Enable Test Case")
  (test-case-show-predicate "Show Predicate")
  (test-case-show-error-message "Show Error Message")
  (test-case-convert-to-text "Convert to text")
  
  ;; Profj Boxes
  (profjBoxes-empty-error "Empty interaction")
  (profjBoxes-too-many-expressions-error "Too many expressions in a box")
  (profjBoxes-interactions-label "Interactions")
  (profjBoxes-bad-java-id-error "Malformed Java ID")
  (profjBoxes-examples-label "Examples")
  (profjBoxes-add-new-example-button "Add new example")
  (profjBoxes-type "Type")
  ;; The Java identifier of an example of data
  (profjBoxes-name "Name")
  (profjBoxes-value "Value")
  (profjBoxes-insert-java-examples "Insert Java Examples")
  (profjBoxes-insert-java-interactions "Insert Java Interactions")

  ;; Slideshow
  (slideshow-show-slideshow-panel "Show Slideshow Panel")
  (slideshow-hide-slideshow-panel "Hide Slideshow Panel")
  (slideshow-freeze-picts "Freeze These Picts")
  (slideshow-thaw-picts "Show Picts Under Mouse")
  (slideshow-hide-picts "Show Nested Boxes")
  (slideshow-show-picts "Show Picts")
  (slideshow-cannot-show-picts "Cannot show picts; run program to cache sizes first")
  (slideshow-insert-pict-box "Insert Pict Box") 

  ;; GUI Tool
  (gui-tool-heading "GUI Tool")
  (gui-tool-before-clicking-message "Before clicking a tool icon, use \"Insert GUI\" from the \"Special\" menu to insert a root GUI item, or select an already inserted GUI.")
  (gui-tool-show-gui-toolbar "Show GUI Toolbar")
  (gui-tool-hide-gui-toolbar "Hide GUI Toolbar")
  (gui-tool-insert-gui "Insert GUI")
  )
