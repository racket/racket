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

(module japanese-string-constants "string-constant-lang.rkt"
 ;;; when translating this constant, substitute name of actual language for `English'
 (is-this-your-native-language "Is Japanese Your Native Language?")

 (are-you-sure-you-want-to-switch-languages
  "GUI で使用する自然言語を変更します。DrRacket を再起動する必要があります。よろしいですか？")

 (interact-with-drscheme-in-language "DrRacket を日本語で使う")

 ;; these two should probably be the same in all languages excepet English.
 ;; they are the button labels (under macos and windows, respectively)
 ;; that go the with the string above.
 (accept-and-quit "承認して終了")
 (accept-and-exit "承認して終了")

 ;;; general purpose (DrRacket is hereby a word in every language, by decree of Robby :)
 (plt "PLT")
 (drscheme "DrRacket")
 (drracket "DrRacket")
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
  ;; One version for always using the current choice:
 (dont-ask-again-always-current "今後このメッセージを表示しない (現在の選択が使われます)")
  ;; One generic version (ie, on the Quit DrRacket dialog)
 (dont-ask-again                "今後このメッセージを表示しない")

 ;;; important urls
 (web-materials "関連するウェブサイト") ;; menu item title
 (tool-web-sites "ツールのウェブサイト")   ;; menu item title
 (plt-homepage "Racket")
 (pbd-homepage "Program by Design")

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
 (bug-report-field-description "説明")
 (bug-report-field-reproduce1 "再現の手順")
 (bug-report-field-reproduce2 "")
 (bug-report-field-environment "環境")
 (bug-report-field-docs-installed "インストールされている文書")
 (bug-report-field-collections "コレクション")
 (bug-report-field-human-language "自然言語")
 (bug-report-field-version "バージョン")
 (bug-report-synthesized-information "詳細情報")  ;; dialog title
 (bug-report-show-synthesized-info "詳細情報の表示")
 (bug-report-submit "送信")
 (bug-report-submit-menu-item "バグ報告の送信...") ;; in Help Menu (drs & help desk)
 (error-sending-bug-report "バグ報告の送信エラー")
 (error-sending-bug-report-expln "バグ報告の送信中にエラーが発生しました。もし、インターネット接続が正常であるなら、\n\n    http://bugs.racket-lang.org/\n\nを開いて、オンラインのウェブフォームからバグ報告を行ってください。お手間をかけて申し訳ありません。\n\nエラーメッセージ:\n~a")
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
 (cs-jump-to-next-bound-occurrence "次の束縛に移動")
 (cs-jump-to-binding "束縛へ移動")
 (cs-jump-to-definition "定義へ移動")
 (cs-error-message "エラーメッセージ")
 (cs-open-file "~a を開く")
 (cs-rename-var "~a の名前を変更")
 (cs-rename-id "識別子の名前変更")
 (cs-rename-var-to "~a を名前変更:")
 (cs-name-duplication-error "入力された新しい名前 ~s は、このスコープに存在する名前と競合します。")
 (cs-rename-anyway "とにかく名前変更")
 (cs-status-init "構文の検証: ユーザーコードのための環境を初期化しています")
 (cs-status-coloring-program "構文の検証: 式にカラーを付けています")
 (cs-status-eval-compile-time "構文の検証: コンパイル時間を評価しています")
 (cs-status-expanding-expression "構文の検証: 式を展開しています")
 (cs-status-loading-docs-index "構文の検証: ドキュメントの索引をロードしています")
 (cs-mouse-over-import "束縛 ~s が ~s からインポートされました")
 (cs-view-docs "~a のドキュメントを表示する")
 (cs-view-docs-from "~a 参照元は ~a")  ;; a completed version of the line above (cs-view-docs) is put into the first ~a and a list of modules (separated by commas) is put into the second ~a. Use check syntax and right-click on a documented variable (eg, 'require') to see this in use

 (cs-lexical-variable "レキシカル変数")
 (cs-imported-variable "インポート変数")

 ;;; info bar at botttom of drscheme frame
 (collect-button-label "GC")
  (read-only "読み取り専用")
 (auto-extend-selection "自動拡張")
 (overwrite "上書き")
 (running "実行中")
 (not-running "停止中")

 ;;; misc
 (welcome-to-something "~a にようこそ！")

 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "Welcome to DrRacket, version ~a, ~a")

 ; these appear on subsequent lines in the `Help|Welcome to DrRacket' dialog.
 (welcome-to-drscheme "Welcome to DrRacket")

 (goto-line "行番号を指定して移動")
 (goto-line-invalid-number
  "~a は無効な行番号です。行番号は 1 から ~a までの整数で指定してください。")
 (goto-position "位置を指定して移動")
 (no-full-name-since-not-saved
  "このファイルはまだ保存されていないため、完全な名前を持っていません。")
 (cannot-open-because-dne "~a が存在しなため、開くことができません。")

  (needs-execute-language-changed
   "警告: 言語が変更されました。[実行] をクリックしてください。")
  (needs-execute-teachpack-changed
   "警告: ティーチパックが変更されました。[実行] をクリックしてください。")
  (needs-execute-defns-edited
   "警告: 定義ウィンドウが変更されました。[実行] をクリックしてください。")

 (file-is-not-saved "ファイル \"~a\" は保存されていません。")
 (save "保存")
 (close-anyway "とにかく閉じる")
 (dont-save "保存しない")
 (clear-anyway "とにかく消去する")

 ;; menu item title
 (log-definitions-and-interactions "定義と対話を記録する...")
 (stop-logging "記録の停止")
 (please-choose-a-log-directory "記録先のディレクトリを選択してください")
 (logging-to "記録先: ")
 (erase-log-directory-contents "記録先のディレクトリ ~a の内容を消去しますか？")
 (error-erasing-log-directory "記録先のディレクトリの内容を消去できませんでした。\n\n~a\n")

  ;; menu items connected to the logger -- also in a button in the planet status line in the drs frame
  (show-log "ログを表示(&L)")
  (hide-log "ログを非表示(&L)")
  (logging-all "すべて") ;; in the logging window in drscheme, shows all logs simultaneously

 ;; modes
 (mode-submenu-label "モード")
 (scheme-mode "Scheme モード")
 (racket-mode "Racket モード")
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
 (syntax-coloring-choose-color "~aのカラーを選択してください。")
 (preferences-colors "カラー") ;; used in the preferences dialog

  ;; parenthesis color scheme string constants
  (parenthesis-color-scheme "括弧のカラースキーム") ;; label for the choice% menu in the preferences dialog
  (paren-color-basic-grey "Basic grey")
  (paren-color-shades-of-gray "Shades of grey")
  (paren-color-shades-of-blue "Shades of blue")
  (paren-color-spring "Spring")
  (paren-color-fall "Fall")
  (paren-color-winter "Winter")


 (url: "URL:")
 (open-url... "URL を開く...")
 (open-url "URL を開く")
 (browse... "選択...")
 (bad-url "不正な URL")
 (bad-url:this "不正な URL: ~a")

 ;; Help Desk
 (help "ヘルプ")
 (help-desk "ヘルプデスク")
 (plt:hd:search "検索")
 (plt:hd:feeling-lucky "Feeling Lucky")
 (plt:hd:home "ヘルプデスクのホーム")
 ; next 3 are popup menu choices in help desk search frame
 (plt:hd:search-for-keyword "キーワードが")
 (plt:hd:search-for-keyword-or-index "キーワードか索引が")
 (plt:hd:search-for-keyword-or-index-or-text "キーワード,索引,テキストが")
 (plt:hd:exact-match "正確に一致")
 (plt:hd:containing-match "含んでいる")
 (plt:hd:regexp-match "正規表現にマッチ")
 (plt:hd:find-docs-for "検索:")
 (plt:hd:search-stopped-too-many-matches "[検索を中止しました: ヒット件数が多過ぎます]")
 (plt:hd:nothing-found-for "情報はありません ~a")
 (plt:hd:and "and")
 (plt:hd:refresh "更新")
 (plt:hd:refresh-all-manuals "すべてのマニュアルを更新する")
 (plt:hd:manual-installed-date "(installed ~a)")
 ; Help Desk configuration
 ;; refreshing manuals
 (plt:hd:refreshing-manuals "マニュアルを再ダウンロードしています")
 (plt:hd:refresh-downloading... "~a をダウンロードしています...")
 (plt:hd:refresh-deleting... "古いバージョンの ~a を削除しています...")
 (plt:hd:refresh-installing... "新しいバージョンの ~a をインストールしています...")
 (plt:hd:refresh-clearing-indices "キャッシュ内の索引を消去しています")
 (plt:hd:refreshing-manuals-finished "完了しました。")
 (plt:hd:about-help-desk "ヘルプデスクについて")
 (plt:hd:help-desk-about-string
  "ヘルプデスクは PLT ソフトウェア (DrRacket, MzScheme, MrEd など) に関する完全な情報を提供します。\n\nバージョン ~a\nCopyright (c) ~a-~a PLT")
 (plt:hd:help-on-help "ヘルプのヘルプ")
 (plt:hd:help-on-help-details "ヘルプデスクの使い方については、ヘルプデスクのホームページにある最初の `Help Desk' というリンクをたどってください。(ホームページを表示するには、ヘルプデスク ウィンドウの上部にある [ホーム] ボタンを押します。)")
  (reload "再読み込み") ;; refresh the page in a web browser
  (plt:hd:ask-about-separate-browser
   "ウェブ上のコンテンツへのリンクを選択しました。リンクをヘルプデスク ブラウザで表示しますか？それとも別のブラウザで表示しますか？")
  (plt:hd:homebrew-browser "ヘルプデスク ブラウザ") ;; choice for the above string (in a button)
  (plt:hd:separate-browser "別のブラウザ") ;; other choice for the above string (also in a button)
  (plt:hd:external-link-in-help "ヘルプ内の外部 URL")
  (plt:hd:use-homebrew-browser "ヘルプデスク ブラウザで外部 URL を開く")
  (plt:hd:new-help-desk "新規ヘルプデスク")

  ;; in the Help Desk language dialog, title on the right.
  (plt:hd:manual-search-ordering "マニュアルの検索順序")

  ;; in the help-desk standalone font preference dialog, on a check box
  (use-drscheme-font-size "DrRacket のフォンサイズを使用する")
  
  ;; in the preferences dialog in drscheme there is example text for help desk font size.
  ;; clicking the links in that text produces a dialog with this message
  (help-desk-this-is-just-example-text
   "これはフォントサイズを設定するためのサンプルです。[ヘルプ] メニューから [ヘルプデスクを開く] を開いてリンクを辿ってください。")

  ;; this appears in the bottom part of the frame the first time the user hits `f1' 
  ;; (assuming nothing else has loaded the documentation index first)
  ;; see also: cs-status-loading-docs-index
  (help-desk-loading-documentation-index "ヘルプデスク: ドキュメントの索引を読み込んでいます")

 ;; Help desk htty proxy
 (http-proxy "HTTP プロキシ")
 (proxy-direct-connection "直接接続")
 (proxy-use-proxy "プロキシを使う:")
 (proxy-host "ホスト")
 (proxy-port "ポート")
 (proxy-bad-host "不正なプロキシ ホスト")

 ;; browser
 (rewind-in-browser-history "戻る")
 (forward-in-browser-history "進む")
 (home "ホーム")
 (browser "ブラウザ")
 (external-browser-choice-title "外部ブラウザ") ; title for radio-button set
 (browser-command-line-label "コマンドライン:") ; label for radio button that is followed by text boxes
 (choose-browser "ブラウザを選択してください")
 (no-browser "後で尋ねる")
 (browser-cmdline-expl-line-1 "(Command line formed by concatenating pre-text, URL,") ; explanatory text for dialog, line 1
 (browser-cmdline-expl-line-2 "and post-text, with no extra spaces between them.)") ; ... line 2. (Anyone need more lines?)
 (install? "インストールしますか？")  ;; if a .plt file is found (title of dialog)
 (you-have-selected-an-installable-package "インストール可能なパッケージを選択しました。")
 (do-you-want-to-install-it? "これをインストールしますか？")
 (paren-file-size "(このファイルのサイズは ~a バイトです)")
 (download-and-install "ダウンロード＋インストール") ;; button label
 (download "ダウンロード") ;; button label
 (save-downloaded-file/size "ダウンロードしたファイル (~a バイト) を、次の名前で保存") ;; label for get-file dialog
 (save-downloaded-file "ダウンロードしたファイルを、次の名前で保存")  ;; label for get-file dialog
 (downloading "ダウンロード中") ;; dialog title
 (downloading-file... "ファイルをダウンロード中...")
 (package-was-installed "パッケージがインストールされました。")
 (download-was-saved "ダウンロードされたファイルが保存されました。")

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
 (about-drscheme-frame-title "DrRacket について")

 ;;; save file in particular format prompting.
 (save-as-plain-text "このファイルをプレーンテキストで保存しますか？")
 (save-in-drs-format "このファイルを DrRacket 専用のバイナリ形式で保存しますか？")
 (yes "はい")
 (no "いいえ")

 ;; saving image (right click on an image to see the text)
  (save-image "画像を保存する...")

 ;;; preferences
 (preferences "環境設定")
 (error-saving-preferences "環境設定を保存時にエラーが発生しました: ~a")
 (error-saving-preferences-title "環境設定保存時のエラー")
 (steal-the-lock-and-retry "ロックを解除して再試行") ;; in the preferences error dialog; this happens when the lockfile exists (after 3 pref writes).
 (error-reading-preferences "環境設定を読み取り時にエラーが発生しました")
 (prefs-file-locked "環境設定ファイルがロックされています (ファイル ~a が存在します), 環境設定の変更を保存できません。環境設定の変更をキャンセルしますか？")
 (try-again "再試行") ;; button label
 (prefs-file-still-locked "環境設定ファイルが依然としてロックされているため (ファイル ~a が存在します), 環境設定の変更を保存できません。")
 (scheme-prefs-panel-label "Racket")
 (warnings-prefs-panel-label "警告")
 (editor-prefs-panel-label "編集")
 (general-prefs-panel-label "一般")
 (highlight-parens "対応する括弧の間を強調表示する")
 (fixup-open-brackets "左角括弧を自動調整する")
 (fixup-close-parens "右括弧を自動調整する")
 (flash-paren-match "対応する括弧をフラッシュする")
 (auto-save-files "ファイルを自動保存する")
 (backup-files "ファイルをバックアップする")
 (map-delete-to-backspace "Delete キーを Backspace キーとして処理する")
 (verify-exit "DrRacket 終了時に確認をとる")
 (ask-before-changing-format "保存形式を変更する前に確認をとる")
 (wrap-words-in-editor-buffers "エディタでテキストを折り返して表示する")
 (show-status-line "ステータス行を表示する")
 (count-columns-from-one "桁番号を 1 から数える")
 (display-line-numbers "バッファの行番号を表示 (文字オフセットではなく)")
 (show-line-and-column-numbers "行番号と桁番号を表示する") ; used for popup menu; right click on line/column box in bottom of drs window
 (show-character-offsets "文字オフセットを表示する") ; used for popup menu; right click on line/column box in bottom of drs window
 (enable-keybindings-in-menus "メニューのキーバインドを有効にする")
 (command-as-meta "Command キーを Meta キーとして処理する") ;; macos/macos x only
 (reuse-existing-frames "新しいファイルを開くときに既存のフレームを再利用する")
 (default-fonts "既定のフォント")
 (basic-gray-paren-match-color "基本グレー括弧の強調表示カラー") ; in prefs dialog
 (online-coloring-active "入力と同時に構文の色付けをする")
 (open-files-in-tabs "ファイルを別のタブに開く (別のウィンドウではなく)")
 (show-interactions-on-execute "プログラムを実行するときは、自動的に対話ウィンドウを開く")
  (switch-to-module-language-automatically "モジュールを開くときは、自動的にそのモジュール言語に切り替える")
  (interactions-beside-definitions "対話ウィンドウを定義ウィンドウの横に配置する") ;; in preferences, below the checkbox one line above this one
 (limit-interactions-size "対話ウィンドウに表示する文字数を制限する")
 (background-color "背景色")
 (default-text-color "既定のテキスト") ;; used for configuring colors, but doesn't need the word "color"
 (choose-a-background-color "背景色を選択してください")
 (revert-to-defaults "デフォルトに戻す")

  (black-on-white-color-scheme "白地に黒") ;; these two appear in the color preferences dialog on butttons
  (white-on-black-color-scheme "黒地に白") ;; clicking the buttons changes the color schemes to some defaults that've been set up.

 ; title of the color choosing dialog

 ; should have entire alphabet
 (font-example-string "The quick brown fox jumped over the lazy dogs.")

 (change-font-button-label "変更")
 (fonts "フォント")
 (other... "その他...") ;; used in the font choice menu item

 ; filled with type of font, eg modern, swiss, etc.
 (choose-a-new-font "フォントタイプが \"~a\" のフォントを選択してください")

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
  "環境設定のロックファイル:\n\n   ~a\n\n があるために、環境設定の保存ができません。Racket のソフトウェアが実行されていないことを確認した上で、このファイルを削除してください。")
 (still-locked-exit-anyway? "環境設定が保存できませんでした。とりあえず終了しますか？")

 ;;; indenting preferences panel
 (indenting-prefs-panel-label "インデント")
 (indenting-prefs-extra-regexp "正規表現")

 (square-bracket-prefs-panel-label "角括弧")

 ; filled with define, lambda, or begin
 (enter-new-keyword "新しい ~a のようなキーワードを入力してください:")
 (x-keyword "~a キーワード")
 (x-like-keywords "~a のようなキーワード")

 ; used in Square bracket panel
 (skip-subexpressions "スキップする部分式の個数")

 (expected-a-symbol "シンボルでなければなりません: ~a")
 (already-used-keyword "\"~a\" はすでに特別にインデントされるキーワードです")
 (add-keyword "追加")
 (remove-keyword "削除")

  ; repl color preferences
  (repl-colors "REPL")
  (repl-out-color "出力")
  (repl-value-color "値")
  (repl-error-color "エラー")

 ;;; find/replace
  (search-next "次")
  (search-previous "前")
  (search-match "一致")  ;;; this one and the next one are singular/plural variants of each other
  (search-matches "一致") 
  (search-replace "置換")
  (search-skip "無視")
  (search-show-replace "置換を表示")
  (search-hide-replace "置換を非表示")
  (find-case-sensitive "大小文字を区別")  ;; the check box in both the docked & undocked search
  (find-anchor-based "アンカーを用いて検索")

  ;; these string constants used to be used by searching,
  ;; but aren't anymore. They are still used by other tools, tho.
  (hide "隠す")
  (dock "結合")
  (undock "分離")

 ;;; multi-file-search
 (mfs-multi-file-search-menu-item "ファイルから検索...")
 (mfs-string-match/graphics "文字列の一致 (画像を含むファイルも検索)")
 (mfs-regexp-match/no-graphics "正規表現 (プレーンテキストのみ)")
 (mfs-searching... "検索中...")
 (mfs-configure-search "検索の設定") ;; dialog title
 (mfs-files-section "ファイル")   ;; section in config dialog
 (mfs-search-section "検索") ;; section in config dialog
 (mfs-dir "ディレクトリ")
 (mfs-recur-over-subdirectories "サブディレクトリ内を再帰的に検索")
 (mfs-regexp-filename-filter "ファイル名を正規表現でフィルタ")
 (mfs-search-string "検索語")
 (mfs-drscheme-multi-file-search "複数ファイルからの検索 - DrRacket") ;; results window and error message title
 (mfs-not-a-dir "\"~a\" はディレクトリではありません")
 (mfs-open-file "ファイルを開く")
 (mfs-stop-search "検索を中止")
 (mfs-case-sensitive-label "大小文字を区別する")
 (mfs-no-matches-found "見つかりませんでした。")
 (mfs-search-interrupted "検索は中止されました。")

 ;;; reverting a file
 (are-you-sure-revert
  "このファイルをディスク上の内容に復元しますか？この操作はやり直すことができないので注意してください。")
 (are-you-sure-revert-title
  "ファイル内容の復元")

 ;;; saving a file
 ; ~a is filled with the filename
 (error-saving "保存に失敗") ;; title of error message dialog
 (error-saving-file/name "~a を保存中にエラーが発生しました")
 (error-loading "ロードに失敗")
 (error-loading-file/name "~a をロード中にエラーが発生しました")
 (unknown-filename "<< unknown >>")

 ;;; finder dialog
 (must-specify-a-filename "ファイル名を指定してください")
 (file-does-not-exist "ファイル \"~a\" は存在しません。")
 (ask-because-file-exists "ファイル \"~a\" はすでに存在します。置換しますか？")
 (dne-or-cycle "The file \"~a\" contains a nonexistent directory or a cycle.")
 (get-file "Get file")
 (put-file "Put file")
 (full-pathname "Full pathname")
 (show-dot-files "ドットで始まるファイルやディレクトリを表示する。")
 (up-directory-button-label "上のディレクトリへ")
 (add-button-label "追加") ;;; for multi-file selection
 (add-all-button-label "すべて追加") ;;; for multi-file selection
 (remove-button-label "削除") ;;; for multi-file selection
 (file-wrong-form "そのファイル名は正しい形式ではありません。")
 (select-files "ファイルの選択")
 (select-file "ファイルの選択")
 (dir-dne "そのディレクトリは存在しません。")
 (file-dne "そのファイルは存在しません。")
 (empty-filename "ファイル名が空です。")
 (that-is-dir-name "それはディレクトリ名です。")

 ;;; raw menu names -- these must match the
 ;;; versions below, once the &s have been stripped.
 ;;; if they don't, DrRacket's menus will appear
 ;;; in the wrong order.
 (file-menu "ファイル")
 (edit-menu "編集")
 (help-menu "ヘルプ")
 (windows-menu "ウィンドウ")

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

 (page-setup-info "印刷パラメータの設定")
 (page-setup-menu-item "ページ設定...")

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
 (clear-menu-item-windows "削除(&D)")

 (select-all-info "文書全体を選択します")
 (select-all-menu-item "すべて選択(&L)")

  (find-menu-item "検索") ;; menu item
  (find-info "検索対象ウィンドウと検索バーの間でキーボード フォーカスを移動する")

 (find-next-info "検索ウィンドウ内の文字列が次に見つかるまでスキップ")
 (find-next-menu-item "次を検索")
  
 (find-previous-info "検索ウィンドウ内の文字列が前に見つかるまでスキップ")
 (find-previous-menu-item "前を検索")
  
  (show-replace-menu-item "置換を表示")
  (hide-replace-menu-item "置換を非表示")
  (show/hide-replace-info "置換パネルの表示/非表示を切り替える")

  (replace-menu-item "置換")
  (replace-info "黒い円の中の検索にヒットした部分を置換する")
  
  (replace-all-info "見つかった検索文字列をすべて置換する")
  (replace-all-menu-item "すべて置換する")
  
  (find-case-sensitive-info "大小文字を区別する/区別しないを切り替える")
  (find-case-sensitive-menu-item "大小文字を区別して検索")

  (complete-word "自動補完") ; the complete word menu item in the edit menu
  (no-completions "... 自動補完できません") ; shows up in the completions menu when there are no completions (in italics)
  
  (overwrite-mode "上書きモード")
  (enable-overwrite-mode-keybindings "上書きモードのキーバインドを有効にする")
  
 (preferences-info "環境設定を行います")
 (preferences-menu-item "環境設定...")

 (keybindings-info "現在有効なキーバインドを表示します")
 (keybindings-menu-item "キーバインド")
 (keybindings-show-active "現在有効なキーバインドを表示")
 (keybindings-frame-title "キーバインド")
 (keybindings-sort-by-name "名前で並べ替え")
 (keybindings-sort-by-key "キーで並べ替え")
 (keybindings-add-user-defined-keybindings "ユーザー定義のキーバインドを追加...")
 (keybindings-add-user-defined-keybindings/planet "ユーザー定義のキーバインドを PLaneT から追加...")
 (keybindings-menu-remove "~a を削除")
 (keybindings-choose-user-defined-file "キーバインドを記述したファイルを選択してください")
 (keybindings-planet-malformed-spec "PLaneT の指定が不正です: ~a") ; the string will be what the user typed in
 (keybindings-type-planet-spec "PLaneT の require 指定を入力してください (`require' は入力しないでください)")
  
 ; first ~a will be a string naming the file or planet package where the keybindings come from;
 ; second ~a will be an error message
 (keybindings-error-installing-file "キーバインドのインストール時にエラーが発生しました ~a:\n\n~a")

 (user-defined-keybinding-error "キーバインドを実行中にエラーが発生しました ~a\n\n~a")
 (user-defined-keybinding-malformed-file "ファイル ~a には、言語 framework/keybinding-lang で書かれたモジュールが含まれていません。")

 ;; menu items in the "special" menu
 (insert-text-box-item "テキストボックスを挿入")
 (insert-image-item "画像を挿入...")
 (insert-comment-box-menu-item-label "コメント ボックスを挿入")
 (insert-lambda "ラムダ(λ)を挿入")

 (wrap-text-item "テキストを折り返す")

  ;; windows menu
 (windows-menu-label "ウィンドウ(&W)")
 (minimize "最小化") ;; minimize and zoom are only used under mac os x
 (zoom "拡大")
 (bring-frame-to-front "フレームを前面に移動")       ;;; title of dialog
 (bring-frame-to-front... "フレームを前面に移動...") ;;; corresponding title of menu item
 (most-recent-window "最近使用したウィンドウ")
  (next-tab "次のタブ")
  (prev-tab "前のタブ")

 (view-menu-label "表示(&V)")
 (show-overview "プログラムの外観を表示")
 (hide-overview "プログラムの外観を非表示")
 (show-module-browser "モジュール ブラウザを表示")
 (hide-module-browser "モジュール ブラウザを非表示")

  (help-menu-label "ヘルプ(&H)")
 (about-info "このアプリケーションの著作権と詳細情報を表示します")
 (about-menu-item "バージョン情報...")

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
  ; these next two are only used in the quit/exit dialog
  ; on the button whose semantics is "dismiss this dialog".
  ; they are there to provide more flexibility for translations
  ; in English, they are just cancel.
 (dont-exit "キャンセル") 
 (dont-quit "キャンセル")

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
  ;; files to appear (typically 5 minutes). Kill DrRacket
  ;; and restart it. You'll see the dialog
  (autosave-autosave-label: "自動保存ファイル:")
  (autosave-original-label: "元のファイル:")
  (autosave-autosave-label "自動保存ファイル")
  (autosave-original-label "元のファイル")
  (autosave-compare-files "自動保存ファイルの比較")

  (autosave-show-autosave "自動保存ファイル") ;; title of a window showing the autosave file

  (autosave-explanation "DrRacket は自動保存ファイルを検出しました。自動保存ファイルには、未保存の作業結果が含まれている可能性があります。")

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
  "定義テキストがファイルシステム上で変更されました。定義テキストを保存するか復元してください。")
 (drscheme-internal-error "DrRacket 内部エラー")

 ;;; tools
 (invalid-tool-spec "コレクション ~a の info.rkt ファイルに記述されているツール仕様が不正です。文字列か空でない文字列リストでなければなりませんが、次の値が記述されています: ~e")
 (error-invoking-tool-title "ツール ~s を起動時にエラーが発生しました: ~s")
 (tool-tool-names-same-length
  "expected `tool-names' and `tools' to be lists of the same length, in info.rkt file for ~s, got ~e and ~e")
 (tool-tool-icons-same-length
  "expected `tool-icons' and `tools' to be lists of the same length, in info.rkt file for ~s, got ~e and ~e")
 (tool-tool-urls-same-length
  "expected `tool-urls' and `tools' to be lists of the same length, in info.rkt file for ~s, got ~e and ~e")
 (error-getting-info-tool
  "~s の info.rkt をロード時にエラーが発生しました")
 (tool-error-phase1 "ツール ~s のフェーズ 1 でエラーが発生しました: ~s")
 (tool-error-phase2 "ツール ~s のフェーズ 2 でエラーが発生しました: ~s")


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
 (toolbar "ツールバー")
 (toolbar-on-top "ツールバーを上側に表示する")
 (toolbar-on-left "ツールバーを左側に表示する")
 (toolbar-on-right "ツールバーを右側に表示する")
 (toolbar-hidden "ツールバーを非表示にする")

 ;;; file menu
 (save-definitions-as "定義に名前を付けて保存(&A)...")
 (save-definitions "定義の保存")
 (print-definitions "定義を印刷...")
 (about-drscheme "DrRacketについて")
 (save-other "その他の保存")
 (save-definitions-as-text "定義をテキストに保存...")
 (save-interactions "対話の保存")
 (save-interactions-as "対話に名前を付けて保存...")
 (save-interactions-as-text "対話をテキストに保存...")
 (print-interactions "対話を印刷...")
 (new-tab "新規タブ")
 (close-tab "タブを閉じる") ;; must not have any &s in it.
 (close-tab-amp "タブを閉じる(&C)") ;; like close-tab, but with an ampersand on the same letter as the one in close-menu-item

 ;;; edit-menu
 (split-menu-item-label "分割(&S)")
 (collapse-menu-item-label "分割解除(&O)")

 ;;; language menu
 (language-menu-name "言語(&L)")

 ;;; scheme-menu
 (scheme-menu-name "Ra&cket")
 (execute-menu-item-label "実行")
 (execute-menu-item-help-string "定義ウィンドウのプログラムを再開始します")
 (ask-quit-menu-item-label "プログラムを停止しますか？")
 (ask-quit-menu-item-help-string "現在の式評価のプライマリ スレッドを停止するには break-thread を使用してください")
 (force-quit-menu-item-label "プログラムを強制終了します")
 (force-quit-menu-item-help-string "現在の式評価を強制終了するには custodian-shutdown-all を使用してください")
 (limit-memory-menu-item-label "メモリを制限する...")
 (limit-memory-msg-1 "ここで指定したメモリ制限値は、プログラムを次回に実行するときに有効になります。")
 (limit-memory-msg-2 "制限値は 8MB 以上にしてください。")
 (limit-memory-unlimited "制限しない")
 (limit-memory-limited "制限する")
 (limit-memory-megabytes "MB")
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
 (save-a-mred-launcher "GRacket ランチャの保存")
 (save-a-mzscheme-launcher "Racket ランチャの保存")
 (save-a-mred-stand-alone-executable "GRacket スタンドアロン実行ファイルの保存")
 (save-a-mzscheme-stand-alone-executable "Racket スタンドアロン実行ファイルの保存")
 (save-a-mred-distribution "GRacket 配布物の保存")
 (save-a-mzscheme-distribution "Racket 配布物の保存")

 (definitions-not-saved "定義ウィンドウが保存されていません。実行ファイルでは定義ウィンドウの最新の保存が使われます。よろしいですか？")
 ;; The "-explanatory-label" variants are the labels used for the radio buttons in
 ;;  the "Create Executable..." dialog for the "(module ...)" language.
 (launcher "ランチャ")
 (launcher-explanatory-label "Launcher (for this machine only, runs from source)")
 (stand-alone "スタンドアロン")
 (stand-alone-explanatory-label "Stand-alone (for this machine only, run compiled copy)")
 (distribution "Distribution")
 (distribution-explanatory-label "Distribution (to install on other machines)")
 (executable-type "Type")
 (executable-base "Base")
 (filename "ファイル名: ")
 (create "作成")
 (please-specify-a-filename "作成するファイル名を指定してください。")
 (~a-must-end-with-~a
  "~a のファイル名\n\n  ~a\n\n は不正です。ファイル名の末尾は \".~a\" でなければなりません。")
 (macosx-executables-must-end-with-app
  "ファイル名\n\n  ~a\n\n は不正です。MacOS X では実行ファイルは末尾が .app のディレクトリでなければなりません。")
 (warning-directory-will-be-replaced
  "警告: ディレクトリ:\n\n  ~a\n\n を置換します。よろしいですか？")

 (distribution-progress-window-title "配布物作成の進行状況")
 (creating-executable-progress-status "配布物のための実行ファイルを作成しています...")
 (assembling-distribution-files-progress-status "配布物のファイルをまとめています...")
 (packing-distribution-progress-status "配布物を展開しています...")

 (create-servlet "サーブレットの作成...")

 ; the ~a is a language such as "module" or "algol60"
 (create-servlet-unsupported-language
  "言語 ~a ではサーブレットの作成はできません。")

 ;;; buttons
 (execute-button-label "実行")
 (save-button-label "保存")
 (break-button-label "停止")

 ;;; search help desk popup menu
 (search-help-desk-for "\"~a\" をヘルプデスクで検索")
 (exact-lucky-search-help-desk-for "\"~a\" をヘルプデスクでラッキー検索 (正確に一致するもの)")

 ;; collapse and expand popup menu items
 (collapse-sexp "S-式を縮小")
 (expand-sexp "S-式を展開")

 ;;; fraction dialog
 (enter-fraction "分数を入力してください")
 (whole-part "整数部")
 (numerator "分子")
 (denominator "分母")
 (insert-number/bad-whole-part "整数でなければなりません。")
 (insert-number/bad-numerator "分子は非負の整数でなければなりません。")
 (insert-number/bad-denominator "分母は正の整数でなければなりません。")
 (insert-fraction-menu-item-label "分数を挿入...")

 ;; number snip popup menu
 (show-decimal-expansion "小数で表示")
 (show-mixed-fraction-view "帯分数で表示")
 (show-improper-fraction-view "仮分数で表示")
 (show-more-decimal-places "小数の桁数を増やす")

 ;;; Teachpack messages
 (select-a-teachpack "ティーチパックの選択")
 (clear-teachpack "ティーチパック ~a を消去")
 (teachpack-error-label "DrRacket - ティーチパック エラー")
 (teachpack-didnt-load "ティーチパック ファイル ~a は、正しくロードされませんでした。")
 (add-teachpack-menu-item-label "ティーチパックの追加...")
 (clear-all-teachpacks-menu-item-label "すべてのティーチパックを消去")
 (drscheme-teachpack-message-title "DrRacket ティーチパック")
 (already-added-teachpack "ティーチパック ~a はすでに追加されています")

  ; ~a is filled with the teachpack's name; the message appears in the teachpack selection dialog when a user installs a new teachpack
  (compiling-teachpack "ティーチパック ~a をコンパイルしています ...")
  (teachpack-pre-installed "プレインストール済みのティーチパック")
  (teachpack-user-installed "ユーザーがインストールしたティーチパック")
  (add-teachpack-to-list... "ティーチパックをリストに追加する...")
  (teachpack-already-installed "'~a' という名前のティーチパックは既にインストールされています。上書きしますか？")
  ; ~a is filled with a list of language names. Each name is separated by a newline and is indented two spaces (no commas, no 'and')
  (teachpacks-only-in-languages "ティーチパックは次の言語でのみ使用可能です: ~a")


 ;;; Language dialog
 (introduction-to-language-dialog
  "言語を選択してください。ほとんどの入門コースの学生は、既定の言語を使うとよいでしょう。")
 (language-dialog-title "言語の選択")
 (case-sensitive-label "大小文字の区別")
 (output-style-label "出力形式")
 (constructor-printing-style "コンストラクタ スタイル")
 (quasiquote-printing-style "擬似クォート スタイル")
 (write-printing-style "write スタイル")
 (print-printing-style "print スタイル")
 (sharing-printing-label "値の表示に shared 構文を使う (値の共有構造を表示する)")
 (use-pretty-printer-label "値を表示するときに改行を挿入する")
 (input-syntax "入力の構文")
 (dynamic-properties "実行時のオプション") ;; "Dynamic Properties" の訳だが、これでいいか？
 (output-syntax "出力の構文")
  (teachpacks "ティーチパック") ;; label in the language dialog for the teaching languages
  (teachpacks-none "<< なし >>") ;; shows up under the previous string, when there are no teachpacks
 (no-debugging-or-profiling "デバッグもプロファイリングもしない")
 (debugging "デバッグ")
 (debugging-and-profiling "デバッグとプロファイリング")
 (test-coverage "構文上のテストスイート カバレージ")
 (show-details-button-label "詳細を表示")
 (hide-details-button-label "詳細を隠す")
 (choose-language-menu-item-label "言語の選択...")
 (revert-to-language-defaults "言語の既定値に戻す")
 (fraction-style "分数のスタイル")
 (use-mixed-fractions "帯分数")
 (use-repeating-decimals "循環小数")
 (decimal-notation-for-rationals "有理数を10進数で表示する")
 (enforce-primitives-group-box-label "初期束縛")
 (enforce-primitives-check-box-label "初期束縛の再定義を禁止する")
 (automatically-compile? "ソースファイルを自動的にコンパイルしますか？")

  ; used in the bottom left of the drscheme frame
  ; used the popup menu from the just above; greyed out and only
  ; visible when some languages are in the history
  (recent-languages "最近使用した言語:")
  ; shows up in bottom-left programming language menu popup, when no langs are recorded
  (no-recently-chosen-languages "最近使用した言語はありません")

 ;; startup wizard screen language selection section
 (please-select-a-language "言語を選択してください")


 ;;; languages
 (beginning-student "Beginning Student")
 (beginning-one-line-summary "定義, 条件式, 構造体, 定数, プリミティブ")
 (beginning-student/abbrev "Beginning Student with List Abbreviations")
 (beginning/abbrev-one-line-summary "Beginner で、REPL でリスト スタイルの表示を行う")
 (intermediate-student "Intermediate Student")
 (intermediate-one-line-summary "Beginner ＋ レキシカル スコープ")
 (intermediate-student/lambda "Intermediate Student with lambda")
 (intermediate/lambda-one-line-summary "Intermediate ＋ 高階関数")
 (advanced-student "Advanced Student")
 (advanced-one-line-summary "Intermediate ＋ lambda と mutation")
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (pretty-big-scheme "Pretty Big")
 (pretty-big-scheme-one-line-summary "syntax と HtDP 言語の関数を追加")
 (pretty-big-scheme-one-line-summary "HtDP 言語, mzscheme, mred/mred の構文と関数を追加")
 (r5rs-language-name "R5RS")
 (r5rs-one-line-summary "純粋な R5RS")
 (expander "Expander")
 (expander-one-line-summary "式を評価するのではなく展開する")
 (legacy-languages "レガシーな言語")
 (teaching-languages "学習用の言語")
 (experimental-languages "実験的な言語")
  (initial-language-category "初期言語")
  (no-language-chosen "言語が選択されていません")

 ;(module-language-one-line-summary "実行するとモジュールのコンテキスト内で REPL を作成する。モジュールで宣言された言語を含む。")
  (module-language-auto-text "#lang 行を自動的に追加する") ;; shows up in the details section of the module language

  ;;; from the `not a language language' used initially in drscheme.
  (must-choose-language "DrRacket は、プログラミング言語を選択しなければプログラムを実行できません。")

  ; next two appear before and after the name of a text book (which will be in italics)
  (using-a-textbook-before "")
  (using-a-textbook-after " を読みながら？")

  ; next two are before and after a language
  (start-with-before "")
  (start-with-after "")

  (seasoned-plt-schemer? "経験豊かな PLT Scheme のユーザーですか？")
  (looking-for-standard-scheme? "標準的な Scheme をお探しですか？")

  ; the three string constants are concatenated together and the middle
  ; one is hyperlinked to the dialog that suggests various languages
  (get-guidance-before "メニューから [言語]-[言語の選択] を選択するか、または、")
  (get-guidance-during "ガイド")
  (get-guidance-after "を参照してください。")

 ;;; debug language
 (unknown-debug-frame "[unknown]")
 (backtrace-window-title "バックトレース - DrRacket")
 (files-interactions "~a's interactions") ;; filled with a filename
 (current-interactions "interactions")
 (current-definitions "definitions")
 (mzscheme-w/debug "Textual (MzScheme, R5RS を含む)")
 (mzscheme-one-line-summary "PLT による Scheme 処理系")
 (mred-w/debug "Graphical (MrEd, MzScheme を含む)")
 (mred-one-line-summary "GUI サポートを MzScheme に追加")

 ;; profiling
 (profiling-low-color "低度")
 (profiling-high-color "高度")
 (profiling-choose-low-color "低度のカラーを選択してください")
 (profiling-choose-high-color "高度のカラーを選択してください")
 (profiling "プロファイル")
 (profiling-example-text "(define (whee) (whee))")
 (profiling-color-config "プロファイルのカラー範囲")
 (profiling-scale "プロファイルのカラー尺度")
 (profiling-sqrt "平方根")
 (profiling-linear "線型")
 (profiling-square "二乗")
 (profiling-number "呼び出しの回数")
 (profiling-time "累積時間")
 (profiling-update "プロファイルの更新")
 (profiling-col-percent-time "% Time")
 (profiling-col-function "関数")
 (profiling-col-time-in-msec "ミリ秒")
 (profiling-col-calls "Calls")
 (profiling-show-profile "プロファイルを表示")
 (profiling-hide-profile "プロファイルを非表示")
 (profiling-unknown-src "<< unknown >>")
 (profiling-no-information-available "プロファイル情報がありません。お使いの言語でプロファイリングを有効にし、プロファイル対象のプログラムを実行してください。")
 (profiling-clear? "定義ウィンドウを書き換えると、プロファイル情報が無効になります。よろしいですか？")

 ;; test coverage
 (test-coverage-clear? "定義ウィンドウを書き換えると、テスト カバレージ情報が無効になります。よろしいですか？")
 (test-coverage-clear-and-do-not-ask-again "「はい」を選択し、今後尋ねないようにする")
 (test-coverage-ask? "テスト カバレージを消去するときは、確認を行う")

 ;; tracing
 (tracing-enable-tracing "トレースを有効にする")
 (tracing-show-tracing-window "トレースを表示")
 (tracing-hide-tracing-window "トレースを非表示")
 (tracing-tracing-nothing-to-show "トーレス結果がありません。(お使いの言語でトレースがサポートされていることを確認し、トレースを有効にしてください。)")

 ;;; repl stuff
 (evaluation-terminated "評価が終了しました。")
 (evaluation-terminated-explanation
  "評価スレッドはもう実行されていませんので、次に実行するまでは評価は行われません。")

  ; The next three constants show up in the same dialog as the above evaluation-terminated string
  ; constants.
  ; The first two show up only when the user calls 'exit' (possibly with a status code).
  ; The third shows up when the program runs out of memory.
  (exited-successfully "正常に終了しました。")
  (exited-with-error-code "エラーコード ~a で終了しました。") ;; ~a is filled in with a number between 1 and 255
  (program-ran-out-of-memory "このプログラムはメモリを使い切りました。")
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
 (version:update-menu-item   "アップデートの確認...")
 (version:update-check       "アップデートの確認") ; dialog title, with the next line
 (version:connecting-server  "Racket バージョン サーバーに接続しています")
 (version:results-title      "Racket バージョンの確認")
 (version:do-periodic-checks "Racket の新しいバージョンを定期的に確認してください")
 (version:take-me-there      "ダウンロードする") ; ...to the download website
 ;; the next one can appear alone, or followed by a comma and the one after that
 (version:plt-up-to-date     "この Racket バージョンは最新です")
 (version:but-newer-alpha    "しかし、これより新しいアルファ リリースが存在します")
 ;; This is used in this context: "Racket vNNN <<<*>>> http://download..."
 (version:now-available-at   "が次のサイトから入手できます")

 ;; insert menu
 (insert-menu "挿入(&I)")

 ;; large semi colon letters
 (insert-large-letters... "大きな文字を挿入...")
 (large-semicolon-letters "大きなセミコロン")
 (text-to-insert "挿入する文字列")

 (module-browser-filename-format "完全なファイル名: ~a (~a 行)")
 (module-browser-root-filename "ルート ファイル名: ~a")
 (module-browser-font-size-gauge-label "フォント サイズ")
 (module-browser-progress-label "モジュールの概要の進行状況")
 (module-browser-adding-file "ファイルを追加しています: ~a...")
 (module-browser-laying-out-graph-label "グラフを配置しています")
 (module-browser-open-file-format "開く ~a")
 (module-browser "モジュール ブラウザ") ;; frame title
 (module-browser... "モジュール ブラウザ...") ;; menu item title
 (module-browser-error-expanding "プログラムを展開中にエラーが発生しました:\n\n~a")
 (module-browser-show-lib-paths "(lib ..) のパスでロードされたファイルを表示する")
 (module-browser-progress "モジュール ブラウザ: ~a") ;; prefix in the status line
 (module-browser-compiling-defns "モジュール ブラウザ: 定義をコンパイル中です")
 (module-browser-show-lib-paths/short "必要なライブラリを含める") ;; check box label in show module browser pane in drscheme window.
 (module-browser-refresh "更新") ;; button label in show module browser pane in drscheme window.
 (module-browser-refresh "再表示") ;; button label in show module browser pane in drscheme window.
; (module-browser-only-in-plt-and-module-langs
;  "モジュール ブラウザは PLT 言語、または、モジュール言語のプログラム (あるいは、それらの言語のモジュールを持つプログラム) でのみ利用可能です。")
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

 (xml-tool-insert-xml-box "XML ボックスを挿入")
 (xml-tool-insert-scheme-box "Racket ボックスを挿入")
 (xml-tool-insert-scheme-splice-box "Racket Splice ボックスを挿入")
 (xml-tool-xml-box "XML ボックス")
 (xml-tool-scheme-box "Racket ボックス")
 (xml-tool-scheme-splice-box "Racket Splice ボックス")
 (xml-tool-switch-to-scheme "Racket ボックスに切り替え")
 (xml-tool-switch-to-scheme-splice "Racket Splice ボックスに切り替え")
 (xml-tool-eliminate-whitespace-in-empty-tags
  "空タグ内の空白を削除")
 (xml-tool-leave-whitespace-alone
  "空白を残す")

 (show-recent-items-window-menu-item "最近開いたファイルを別ウィンドウに表示する")
 (show-recent-items-window-label "最近開いたファイル")
 (number-of-open-recent-items "[最近開いたファイル] に表示する数")
 (switch-anyway "Switch File Anyway")

 (stepper-program-has-changed "警告: プログラムが変更されました。")
 (stepper-program-window-closed "警告: プログラム ウィンドウが閉じました。")

 (stepper-name "ステッパ")
 (stepper-language-level-message "ステッパは \"~a\" 言語に対しては動作しません")
 (stepper-button-label "ステップ")
 (stepper-previous-application "アプリケーション")
 (stepper-previous "ステップ")
 (stepper-next "ステップ")
 (stepper-next-application "アプリケーション")
 (stepper-jump-to-beginning "ホーム")
 (stepper-jump-to-end "終端まで")

 (debug-tool-button-name "デバッグ")

 (dialog-back "戻る")

 ;; warnings about closing a drscheme frame when the program
 ;; might still be doing something interesting
 (program-is-still-running "定義ウィンドウ内のプログラムは実行中です。強制的に閉じますか？")
  (program-has-open-windows "定義ウィンドウ内のプログラムはウィンドウを開いています。このウィンドウを強制的に閉じますか？")

  ;; ml-command-line-arguments is for the command line arguments
  ;; label in the module language details in the language dialog.
  (ml-command-line-arguments "コマンドライン引数 (文字列ベクタの読み取り構文で指定)")

  ;; ml-cp names are all for the module language collection path
  ;; configuration. See the details portion of the language dialog
  ;; for the module language (at the bottom).
  (ml-cp-default-collection-path "<<既定のコレクション パス>>")

  ;; in std get-directory
  (ml-cp-choose-a-collection-path "コレクション パスを選択してください。")

  ;; err msg when adding default twice
  (ml-cp-default-already-present
   "既定のコレクション パスはすでに存在しています。")

  ;; title of this section of the dialog (possibly the word
  ;; `Collection' should not be translated)
  (ml-cp-collection-paths "コレクション パス")

  ;; button labels
  (ml-cp-add "追加")
  (ml-cp-add-default "既定の追加")
  (ml-cp-remove "削除")
  (ml-cp-raise "上へ")
  (ml-cp-lower "下へ")

  (ml-always-show-#lang-line "モジュール言語で常に #lang 行を表示する")

  ;; Profj
  (profj-java "Java")
  (profj-java-mode "Java モード")

  (profj-beginner-lang "Beginner")
  (profj-beginner-lang-one-line-summary "Java ライクな初級用のティーチング言語")
  (profj-full-lang "完全")
  (profj-full-lang-one-line-summary "Java 1.0 (一部は 1.1) のような言語")
  (profj-advanced-lang "Advanced")
  (profj-advanced-lang-one-line-summary "Java ライクナ上級用のティーチング言語")
  (profj-intermediate-lang "Intermediate")
  (profj-intermediate-lang-one-line-summary "Java ライクな中級用のティーチング言語")
  (profj-intermediate-access-lang "Intermediate + access")
  (profj-intermediate-access-lang-one-line-summary "Java ライクな中級用のティーチング言語 (アクセス修飾子付き)")
  (profj-dynamic-lang "Java+dynamic")
  (profj-dynamic-lang-one-summary "Java に動的型付け機能を付加した言語")

  (profj-java-mode-color-heading "カラーの編集") ; Heading for preference to choose editing colors
  (profj-java-mode-color-keyword "キーワード")
  (profj-java-mode-color-string "文字列")
  (profj-java-mode-color-literal "リテラル")
  (profj-java-mode-color-comment "コメント")
  (profj-java-mode-color-error "エラー")
  (profj-java-mode-color-identifier "識別子")
  (profj-java-mode-color-prim-type "基本型") ; Example text for built-in Java types
  (profj-java-mode-color-default "デフォルト")

  (profj-coverage-color-heading "カバレージのカラー") ; Heading for preference to choose coverage colors
  (profj-coverage-color-covered "カバーされた式")

  (profj-language-config-display-preferences "表示の環境設定") ; Heading for preferences controlling printing
  (profj-language-config-display-style "表示のスタイル")
  (profj-language-config-display-field "クラス + フィールド")
  (profj-language-config-class "クラス")
  (profj-language-config-display-array "配列の要素をすべて表示しますか？")
  (profj-language-config-testing-preferences "テストの環境設定") ; Heading for preferences controlling test behavior
  ;(profj-language-config-testing-enable "実行時にテスト結果を表示しますか？") ; Run should be the word found on the Run button
  (profj-language-config-testing-coverage "テストのためのカバレージ情報を収集しますか？")
  (profj-language-config-support-test-language "Support test Language extension?")
  (profj-language-config-testing-check "check 式を使用しますか？") ; check should not be translated
  (profj-language-config-classpath "クラスパス")
  (profj-language-config-choose-classpath-directory "クラスパスに追加するディレクトリを選択してください")
  (profj-language-config-classpath-display "現在のクラスパスを表示") ; Button label to print the current classpath

  (profj-test-name-close-to-example "クラス ~a の名前が Example に近いフレーズを含んでいます。")
  (profj-test-name-example-miscapitalized "クラス ~a の名前の大小文字が誤っています。")

   ;; Close testing window and do not run test cases any more
  ;(profj-test-results-close-and-disable "テストを閉じて無効にする")
  ;; Hide docked testing window and do not run test cases any more
  ;(profj-test-results-hide-and-disable "テストを非表示にして無効にする")
  ;Renamed below
  ;(profj-test-results-window-title "テスト結果")

  (profj-unsupported "サポートされていません")
  (profj-executables-unsupported "申し訳ありません。現バージョンでは Java の実行ファイルはサポートされていません")

  (profj-convert-to-text-comment "テキスト コメントに変換")
  (profj-convert-to-comment "コメントに変換")

  (profj-executing-main "main を実行しています")

  (profj-insert-java-comment-box "Java コメント ボックスを挿入")
  (profj-insert-java-interactions-box "Java 対話ボックスを挿入")

  ;;The Test engine tool
  ;;
  (test-engine-window-title "テスト結果")
  ;;Following two appear in View menu, attach and free test report window from DrRacket frame
  (test-engine-dock-report "テスト結果を切り離して表示する")
  (test-engine-undock-report "テスト結果を統合して表示する")
  ;;Following two appear in Racket (Java, etc) menu, cause Tests to be Run automatically or not
  (test-engine-enable-tests "テストを有効にする")
  (test-engine-disable-tests "テストを無効にする")
  
  (profjWizward-insert-java-class "Java クラスを挿入")
  (profjWizard-insert-java-union "Java Union を挿入")

  ;; The Test Suite Tool
  ;; Errors
  (test-case-empty-error "空のテストケース")
  (test-case-too-many-expressions-error "テストケース内に式が多過ぎます。")
  ;; DrRacket window menu items
  (test-case-insert "テストケースを挿入")
  (test-case-disable-all "すべてのテストケースを無効にする")
  (test-case-enable-all "すべてのテストケースを有効にする")

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
  (test-case-error-message "エラーメッセージ")

  (test-case-menu-title "テストケース")
  (test-case-switch-to-error-box "Switch to Error Test Box")
  (test-case-switch-to-nonerror-box "Switch to Nonerror Test box")
  (test-case-collapse "テストケースを縮小")
  (test-case-show-actual "実際の値を表示する")
  (test-case-enable "テストケースを有効にする")
  (test-case-show-predicate "Show Predicate")
  (test-case-show-error-message "エラーメッセージを表示する")
  (test-case-convert-to-text "テキストに変換する")

  ;; Profj Boxes
  (profjBoxes-empty-error "Empty interaction")
  (profjBoxes-too-many-expressions-error "ボックスに式が多過ぎです")
  (profjBoxes-interactions-label "Interactions")
  (profjBoxes-bad-java-id-error "不正な形式の Java ID です")
  (profjBoxes-examples-label "例")
  (profjBoxes-add-new-example-button "例を追加")
  (profjBoxes-type "Type")
  ;; The Java identifier of an example of data
  (profjBoxes-name "名前")
  (profjBoxes-value "値")
  (profjBoxes-insert-java-examples "Insert Java Examples")
  (profjBoxes-insert-java-interactions "Insert Java Interactions")

  ;; Slideshow
  (slideshow-hide-picts "Show Nested Boxes")
  (slideshow-show-picts "Show Picts")
  (slideshow-cannot-show-picts "Cannot show picts; run program to cache sizes first")
  (slideshow-insert-pict-box "Pict ボックスを挿入")

  ;; GUI Tool
  (gui-tool-heading "GUI Tool")
  (gui-tool-before-clicking-message "ツール アイコンをクリックする前に、[Special] メニューの [Insert GUI] からルート GUI 項目を挿入するか、すでに挿入されている GUI を選択してください。")
  (gui-tool-show-gui-toolbar "GUI ツールバーを表示")
  (gui-tool-hide-gui-toolbar "GUI ツールバーを非表示")
  (gui-tool-insert-gui "GUI を挿入")

  ;; contract violation tracking
  
  ; tooltip for new planet icon in drscheme window (must have a planet violation logged to see it)
  (show-planet-contract-violations "PLaneT の規約違反を表示する")

  ; buttons in the dialog that lists the recorded bug reports
  (bug-track-report "File Ticket")
  (bug-track-forget "Forget")
  (bug-track-forget-all "Forget All")
    
  ;; planet status messages in the bottom of the drscheme window; the ~a is filled with the name of the package
  (planet-downloading "PLaneT: ダウンロード中 ~a...")
  (planet-installing "PLaneT: インストール中 ~a...")
  (planet-finished "PLaneT: 完了 ~a.")
  (planet-no-status "PLaneT") ;; this can happen when there is status shown in a different and then the user switches to a tab where planet hasn't been used
  
  ;; string normalization. To see this, paste some text with a ligature into DrRacket
  ;; the first three strings are in the dialog that appears. The last one is in the preferences dialog
  (normalize "Normalize")
  (leave-alone "Leave alone")
  (normalize-string-info "The string you pasted contains ligatures or other non-normalized characters. Normalize them?")
  (normalize-string-preference "Normalize pasted strings")
  (ask-about-normalizing-strings "Ask about normalizing strings")
  
  )
