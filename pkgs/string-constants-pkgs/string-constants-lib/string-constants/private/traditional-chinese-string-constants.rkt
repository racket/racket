;; Computer converted from Simplified-Chineses string constants by Chongkai Zhu
(module traditional-chinese-string-constants "string-constant-lang.rkt"
  (is-this-your-native-language "中文是你的母語嗎？")
  
  (are-you-sure-you-want-to-switch-languages
   "為了改變界面語言，現在需要重新啟動DrRacket。你確定嗎？")
  
  (interact-with-drscheme-in-language "使用繁體中文作DrRacket界面語言")
  
  ;; these two should probably be the same in all languages excepet English.
  ;; they are the button labels (under macos and windows, respectively)
  ;; that go the with the string above.
  (accept-and-quit "接受並退出")
  (accept-and-exit "接受並退出")
  
  ;;; general purpose (DrRacket is hereby a word in every language, by decree of Robby :)
  (plt "PLT")
  (drscheme "DrRacket")
  (drracket "DrRacket")
  
  (ok "確定")
  (cancel "取消")
  (abort "中止")
  (untitled "未命名")
  (untitled-n "未命名~a")
  (warning "警告")
  (error "錯誤")
  (close "關閉") ;; as in, close an open window. must match close-menu-item
  ;; in the sense that, when the &s have been stripped from
  ;; close-menu-item, it must be the same string as this.
  (close-window "關閉視窗")
  (stop "停止")
  (&stop "停止(&S)") ;; for use in button and menu item labels, with short cut.
  (are-you-sure-delete? "確定要刪除~a嗎？") ;; ~a is a filename or directory name
  (are-you-sure-replace? "確定要替換~a嗎？") ;; ~a is a filename or directory name
  (ignore "忽略")
  (revert "還原")
  
  ;; label for a generic check box, often supported on dialogs
  ;; that ask a binary choice of the user. If checked, the
  ;; dialog isn't going to be shown again.
  ;; One version for always using the current choice:
  (dont-ask-again-always-current "不再詢問（總是使用當前設置）")
  ;; One generic version (ie, on the Quit DrRacket dialog)
  (dont-ask-again "不再詢問")
  
  ;;; important urls
  (web-materials "相關網站") ;; menu item title
  (tool-web-sites "工具網站")   ;; menu item title
  (plt-homepage "Racket")
  (pbd-homepage "Program by Design")
  
  ;;; bug report form
  (cancel-bug-report? "取消程序錯誤報告？")
  (are-you-sure-cancel-bug-report?
   "你確定要取消報告程序錯誤嗎？")
  (do-you-want-to-discard-or-save-this-bug-report
   "是保存還是丟棄這份程序錯誤報告?")
  (discard "丟棄") ;; a button label for a dialog box with the above question
  (bug-report-form "程序錯誤報告表")
  (bug-report-field-name "姓名")
  (bug-report-field-email "電子郵件")
  (bug-report-field-summary "標題")
  (bug-report-field-severity "嚴重度")
  (bug-report-field-class "類別")
  (bug-report-field-description "詳細描述")
  (bug-report-field-reproduce1 "再現程序錯")
  (bug-report-field-reproduce2 "誤的步驟")
  (bug-report-field-environment "環境")
  (bug-report-field-docs-installed "已安裝文檔")
  (bug-report-field-collections "Collections")
  (bug-report-field-human-language "自然語言")
  (bug-report-field-memory-use "內存使用")
  (bug-report-field-version "版本")
  (bug-report-synthesized-information "綜合信息")  ;; dialog title
  (bug-report-show-synthesized-info "顯示綜合信息")
  (bug-report-submit "提交")
  (close-and-save-bug-report "保存並關閉(&&)") ;; button in bug report dialog, next to cancel and bug-report-submit
  (bug-report-submit-menu-item "提交程序錯誤報告…");; same as above, but used when there are saved bug reports
  (saved-bug-reports-menu-item "保存程序錯誤報告") ;; in Help Menu, submenu title
  (disacard-all-saved-bug-reports "丟棄全部程序錯誤報告") ;; menu item: only shows up when there is more than one saved bug report
  (no-saved-bug-reports "不存在程序錯誤報告") ;; an info message that shows up as a disabled menu item when no saved bug reports are around
  (new-bug-report "新程序錯誤報告") ;; button label the user sees when there are saved bug reports, but the user asks to save another one.
  (close-and-save "保存並關閉") ;; button on the bottom of the bug report form
  (saved-unsubmitted-bug-reports "未提交的程序錯誤報告") 
  ;; the above string constant is next to previous line in same dialog, followed by list of bug report subjects (as buttons)
  (error-sending-bug-report "程序錯誤報告傳輸出錯")
  (error-sending-bug-report-expln "在傳輸程序錯誤報告的過程中出現了錯誤。如果你能夠正常瀏覽網路，請訪問：\n\n    http://bugs.racket-lang.org/\n\n使用网页上的表单提交程序错误报告。对于由此产生的不便，我们表示抱歉。\n\n传输错误详情：\n~a")
  (illegal-bug-report "非法的程序錯誤報告")
  (pls-fill-in-field "請填寫\"~a\"欄目")
  (malformed-email-address "電子郵件地址合格不正確")
  (pls-fill-in-either-description-or-reproduce "在「詳細描述」和「再現程序錯誤的步驟」兩欄中，請至少填寫一項。")
  
  ;;; check syntax
  (check-syntax "檢查語法")
  (cs-italic "斜體")
  (cs-bold "黑體")
  (cs-underline "下劃線")
  (cs-change-color "改變顏色")
  (cs-foreground-color "前景色")
  (cs-background-color "背景色")
  (cs-tack/untack-arrow "附加/取消箭頭")
  (cs-jump-to-next-bound-occurrence "跳至下一個被綁定出現")
  (cs-jump-to-binding "跳至綁定出現")
  (cs-jump-to-definition "跳至定義")
  (cs-error-message "出錯信息")
  (cs-open-file "打開~a")
  (cs-rename-var "重命名~a")
  (cs-rename-id "重命名標識符")
  (cs-rename-var-to "將~a重命名為：")
  (cs-name-duplication-error "你所選擇的新名稱~s與當前轄域內現有標識符相同。")
  (cs-rename-anyway "強制重命名")
  (cs-status-init "檢查語法：為用戶代碼初始化環境")
  (cs-status-coloring-program "檢查語法：為程序著色")
  (cs-status-eval-compile-time "檢查語法：編譯程序")
  (cs-status-expanding-expression "檢查語法：展開表達式")
  (cs-status-loading-docs-index "檢查語法：載入文檔索引")
  (cs-mouse-over-import "綁定~s由~s導入")  
  (cs-view-docs "察看~a的文檔")
  (cs-view-docs-from "~a（來自~a）")  ;; a completed version of the line above
  ;; (cs-view-docs) is put into the first ~a and a list of modules (separated by commas) 
  ;; is put into the second ~a. Use check syntax and right-click on a documented variable (eg, 'require') to see this in use
  
  (cs-lexical-variable "詞法變數")
  (cs-set!d-variable "set!過的變數")
  (cs-imported-variable "導入的變數")
  (cs-unused-require "無用的require")
  (cs-free-variable "自由變數")
  
  (cs-binder-count "~a次綁定出現")
  (cs-zero-varrefs "沒有綁定出現")
  (cs-one-varref "1次被綁定出現")
  (cs-n-varrefs "~a次被綁定出現") ;; expected to have one ~a formatter that will accept a number
  
  (cs-contract-my-obligation "Contract：本module的義務")
  (cs-contract-their-obligation "Contract：客戶module的義務")
  (cs-contract-both-obligation "Contract：本module和客戶module的共同義務")
  (cs-contract-unk-obligation "Contract：義務未知")
  
  
  ;; mode sub-menu in the "view" menu
  (cs-check-syntax-mode "檢查語法模式")
  (cs-mode-menu-show-my-obligations "我的Contract義務")
  (cs-mode-menu-show-client-obligations "客戶的Contract義務")
  (cs-mode-menu-show-syntax "語法範疇")
  
  ;; the documentation blue boxes in the upper-right corner of the drracket window
  (sc-read-more... "閱讀更多…")
  (sc-f2-to-un/lock "f2（解除）鎖定")
  
  ;; the online check syntax status messages (mouse over the bottom right of drracket's window to see the messages during online expansion's various phases)
  (online-expansion-running "語法檢查後台運行中")
  (online-expansion-only-raw-text-files-supported "僅支持純文本文件")
  (online-expansion-abnormal-termination "語法檢查後台異常退出")
  (online-expansion-finished-successfully "語法檢查後台成功運行")
  
  (jump-to-error "跳至錯誤")
  (online-expansion-is-disabled "語法檢查後台運行被禁用")
  ; these next two show up in the bar along the bottom of the drracket window
  (online-expansion-pending "語法檢查後台運行……")
  (online-expansion-finished "後台語法檢查完成") ;; note: there may still be errors in this case
  ; the next two show up in a menu when you click on the circle in the bottom right corner
  (disable-online-expansion "禁用後台語法檢查")
  (enable-online-expansion "啟用後台語法檢查")
  ;; the online expansion preferences pane
  (online-expansion "後台語法檢查") ;; title of prefs pane
  ; the different kinds of errors
  (online-expansion-show-read-errors-as "顯示read錯誤")
  (online-expansion-show-variable-errors-as "顯示未綁定變數")
  (online-expansion-show-other-errors-as "顯示其他錯誤")
  ; locations the errors can be shown
  (online-expansion-error-gold-highlight "使用高亮突出")
  (online-expansion-error-margin "在一側顯示")
  ; the label of a preference in the (string-constant online-expansion) section
  (show-arrows-on-mouseover "滑鼠懸停時顯示綁定及尾位置箭頭")
  ;;; info bar at botttom of drscheme frame
  (collect-button-label "垃圾收集")
  (read-only "只讀")
  (auto-extend-selection "自動延長")
  (overwrite "覆蓋")
  (running "運行中")
  (not-running "靜止中")
  
  ;;; misc
  (welcome-to-something "歡迎來到~a")
  
  ; this appears in the drscheme about box.
  (welcome-to-drscheme-version/language "歡迎使用DrRacket，版本~a，~a")
  
  ; these appear on subsequent lines in the `Help|Welcome to DrRacket' dialog.
  (welcome-to-drscheme "歡迎使用DrRacket")
  
  (goto-line "跳至行號")
  (goto-line-invalid-number
   "~a不是合法的行號。必須提供一個在1和~a之間的數字")
  (goto-position "跳至位置")
  (no-full-name-since-not-saved
   "當前文件還沒有被命名，因為從來沒有對它進行保存。")
  (cannot-open-because-dne "無法打開~a，文件不存在。")
  
  (needs-execute-language-changed
   "警告：語言改變了。請單擊「運行」。")
  (needs-execute-teachpack-changed
   "警告：教學包改變了。請單擊「運行」。")
  (needs-execute-defns-edited
   "警告：定義視窗改變了。請單擊「運行」。")
  
  (editor-changed-since-srcloc-recorded
   "編輯器在記錄原始位置之後被改動過，故高亮區域不一定準確反映源碼")
  
  (file-is-not-saved "文件\"~a\"還沒有保存過")
  (save "保存")
  (close-anyway "強制關閉")
  (dont-save "不保存")
  (clear-anyway "強制清空")
  
  ;; menu item title
  (log-definitions-and-interactions "記錄定義和交互的日誌…")
  (stop-logging "不再記錄日誌")
  (please-choose-a-log-directory "請選擇日誌目錄")
  (logging-to "記錄日誌到：")
  (erase-log-directory-contents "刪除日誌目錄~a中的內容？")
  (error-erasing-log-directory "刪除日誌出錯。\n\n~a\n")
  
  ;; menu items connected to the logger -- also in a button in the planet status line in the drs frame
  (show-log "顯示日誌(&L)")
  (hide-log "隱藏日誌(&L)")
  (logger-scroll-on-output "跟隨輸出") ; a checkbox in the logger pane
  (log-messages "日誌信息") ;; label for the drracket logging gui panel
  
  
  ;; modes
  (mode-submenu-label "模式")
  (scheme-mode "Scheme模式")
  (racket-mode "Racket模式")
  (text-mode "文本模式")
  
  (scheme-mode-color-symbol "符號")
  (scheme-mode-color-keyword "關鍵詞")
  (scheme-mode-color-comment "注釋")
  (scheme-mode-color-string "字元串")
  (scheme-mode-color-constant "常量")
  (scheme-mode-color-parenthesis "括號")
  (scheme-mode-color-hash-colon-keyword "#:關鍵詞")
  (scheme-mode-color-error "錯誤")
  (scheme-mode-color-other "其他")
  ;; the ~a is filled in with one of the above (scheme-mode-*)
  (syntax-coloring-choose-color "為~a選擇顏色")
  (preferences-colors "顏色") ;; used in the preferences dialog
  
  ;; parenthesis color scheme string constants
  (parenthesis-color-scheme "括號色彩調配") ;; label for the choice% menu in the preferences dialog
  (paren-color-basic-grey "單一灰色")
  (paren-color-shades-of-gray "漸變灰色")
  (paren-color-shades-of-blue "漸變藍色")
  (paren-color-spring "春")
  (paren-color-fall "秋")
  (paren-color-winter "冬")
  
  
  (url: "URL：")
  (open-url... "打開URL…")
  (open-url "打開URL")
  (browse... "瀏覽…")
  (bad-url "錯誤的URL")
  (bad-url:this "錯誤的URL：~a")
  
  ;; Help Desk
  (help "幫助")
  (racket-documentation "Racket文檔")
  (help-desk "幫助台")
  (plt:hd:search "搜索")
  (plt:hd:feeling-lucky "手氣不錯")
  (plt:hd:home "幫助台首頁") 
  ; next 3 are popup menu choices in help desk search frame
  (plt:hd:search-for-keyword "關鍵詞")
  (plt:hd:search-for-keyword-or-index "關鍵詞或索引")
  (plt:hd:search-for-keyword-or-index-or-text "關鍵詞、索引或普通文本")
  (plt:hd:exact-match "精確匹配")
  (plt:hd:containing-match "包含")
  (plt:hd:regexp-match "正則表達式匹配")
  (plt:hd:find-docs-for "搜索文檔：")
  (plt:hd:search-stopped-too-many-matches "[搜索中斷：過多匹配結果]")
  (plt:hd:nothing-found-for "找不到任何關於~a的信息")
  (plt:hd:and "並且")
  (plt:hd:refresh "更新")
  (plt:hd:refresh-all-manuals "更新所有手冊")
  (plt:hd:manual-installed-date "（~a已安裝）")
  ; Help Desk configuration
  ;; refreshing manuals
  (plt:hd:refreshing-manuals "重新下載手冊")
  (plt:hd:refresh-downloading... "下載~a…")
  (plt:hd:refresh-deleting... "刪除舊版本的~a…")
  (plt:hd:refresh-installing... "安裝新版本的~a…")
  (plt:hd:refresh-clearing-indices "清除緩存中的索引")
  (plt:hd:refreshing-manuals-finished "完成。")
  (plt:hd:about-help-desk "關於幫助台")
  (plt:hd:help-desk-about-string
   "幫助台是Racket軟體的完整信息來源。\n\n版本~a\n版權所有（c）~a—~a PLT")
  (plt:hd:help-on-help "關於幫助的幫助")
  (plt:hd:help-on-help-details
   "關於使用幫助台的幫助，請參見幫助台首頁中的第一個連結『幫助台』。（要進入幫助台的首頁，請單擊幫助台視窗上方的『首頁』按鈕。）")
  (reload "刷新") ;; refresh the page in a web browser
  (plt:hd:ask-about-separate-browser
   "你選擇了一個指向全球資訊網的連結。請問您是要在幫助台中打開該頁面，還是想使用瀏覽器程序瀏覽網頁？")
  (plt:hd:homebrew-browser "幫助台瀏覽器") ;; choice for the above string (in a button)
  (plt:hd:separate-browser "網路瀏覽器") ;; other choice for the above string (also in a button)
  (plt:hd:external-link-in-help "在幫助台中顯示外部URL")
  (plt:hd:use-homebrew-browser "對於外部URL，使用幫助台瀏覽")
  (plt:hd:new-help-desk "新幫助台")
  
  ;; in the Help Desk language dialog, title on the right.
  (plt:hd:manual-search-ordering "搜索手冊的順序")
  
  ;; in the help-desk standalone font preference dialog, on a check box
  (use-drscheme-font-size "使用和DrRacket相同的字體大小")
  
  ;; in the preferences dialog in drscheme there is example text for help desk font size.
  ;; clicking the links in that text produces a dialog with this message
  (help-desk-this-is-just-example-text
   "這裡顯示的只是示例字體大小的文字。要察看這些連結，請通過幫助菜單打開真正的幫助台。")
  
  ;; this appears in the bottom part of the frame the first time the user hits `f1' 
  ;; (assuming nothing else has loaded the documentation index first)
  ;; see also: cs-status-loading-docs-index
  (help-desk-loading-documentation-index "幫助台：正在讀入文檔索引")
  
  ;; Help desk htty proxy
  (http-proxy "HTTP代理")
  (proxy-direct-connection "直接連接")
  (proxy-use-proxy "使用代理伺服器：")
  (proxy-host "地址")
  (proxy-port "埠")
  (proxy-bad-host "無效代理伺服器")
  
  ;; browser
  (rewind-in-browser-history "後退")
  (forward-in-browser-history "前進")
  (home "主頁")
  (browser "瀏覽器")
  (external-browser-choice-title "外部瀏覽器") ; title for radio-button set
  (browser-command-line-label "命令行：") ; label for radio button that is followed by text boxes
  (choose-browser "選擇瀏覽器")
  (no-browser "以後再詢問")
  (browser-cmdline-expl-line-1 "（命令行由前綴文字，URL和後綴文字") ; explanatory text for dialog, line 1
  (browser-cmdline-expl-line-2 "連接而成，中間不含任何空格）") ; ... line 2. (Anyone need more lines?)
  (install? "安裝？")  ;; if a .plt file is found (title of dialog)
  (you-have-selected-an-installable-package "你選擇了一個可以安裝的軟體包。")
  (do-you-want-to-install-it? "是否安裝？")
  (paren-file-size "（該文件的長度是~a位元組）")
  (download-and-install "下載並安裝(&&)") ;; button label
  (download "下載") ;; button label
  (save-downloaded-file/size "下載文件(~a位元組)並保存為") ;; label for get-file dialog
  (save-downloaded-file "下載文件並保存為")  ;; label for get-file dialog
  (downloading "下載中") ;; dialog title
  (downloading-file... "下載文件中……")
  (package-was-installed "安裝已完成。")
  (download-was-saved "文件已保存。")
  
  (install-plt-file-menu-item... "安裝.plt文件…")
  (install-plt-file-dialog-title "安裝.plt文件")
  (install-plt-web-tab "網路")
  (install-plt-file-tab "文件")
  (install-plt-filename "文件名：")
  (install-plt-url "URL：")
  (install-plt-error-header "檢查下載的.plt文件時出錯。請檢查URL並重試。")
  
  ;; install plt file when opened in drscheme strings
  (install-plt-file "安裝~a，還是打開以供編輯？")
  (install-plt-file/yes "安裝")
  (install-plt-file/no "編輯")
  
  (plt-installer-progress-window-title "安裝進度") ;; frame title
  (plt-installer-abort-installation "取消安裝") ;; button label
  (plt-installer-aborted "安裝中止。") ;; msg that appears in the installation window when installation is aborted
  
  ;;; about box
  (about-drscheme-frame-title "關於DrRacket")
  
  ;;; save file in particular format prompting.
  (save-as-plain-text "保存本文件為純文本？")
  (save-in-drs-format "保存本文件為drscheme（非純文本）格式？")
  (yes "是")
  (no "否")
  
  ;; saving image (right click on an image to see the text)
  (save-image "保存圖片…")
  
  ;;; preferences
  (preferences "首選項")
  (error-saving-preferences "保存首選項時出錯：~a")
  (error-saving-preferences-title "保存首選項時出錯")
  (steal-the-lock-and-retry "取消鎖定並重試(&&)") ;; in the preferences error dialog; this happens when the lockfile exists (after 3 pref writes).
  
  (error-reading-preferences "讀取首選項時出錯")
  (error-reading-preferences-explanation "首選項文件被鎖定，故~a選項無法讀取")
  ;; in the above, ~a is filled with the name of the preference (a symbol)
  (dont-ask-again-until-drracket-restarted "不再詢問（直到DrRacket關閉）")
  ; difference between the above and below is one comes with a question (steal the lock or not) and the other with just a notation saying "the file is locked"
  (dont-notify-again-until-drracket-restarted "不再通知（直到DrRacket關閉）") 
  (prefs-file-locked "存儲首選項的文件被鎖定了（由於文件~a的存在），所以這些改動無法被保存。放棄修改？")
  (try-again "重試") ;; button label
  (give-up-and-use-the-default "放棄並使用默認值") ;; button label
  
  (prefs-file-still-locked "存儲首選項的文件仍然被鎖定（由於文件~a的存在）, 所以這些改動將不會被保存。")
  (prefs-file-locked-nothing-doing
   "首選項文件友~s鎖定，故修改不會被保存。")
  ;; the  ~s is filled with the lockfile; this string is (currently) used only on windows where lockfiles are less friendly (and there is no steal fallback)
  
  (scheme-prefs-panel-label "Racket")
  (warnings-prefs-panel-label "警告")
  (editor-prefs-panel-label "編輯")
  (general-prefs-panel-label "常規")
  (highlight-parens "加亮顯示匹配的括號")
  (fixup-open-brackets "自動調整開中括號")
  (fixup-close-parens "自動調整閉括號")
  (flash-paren-match "高亮顯示括號匹配")
  (auto-save-files "自動保存文件")
  (backup-files "保存備份文件")
  (map-delete-to-backspace "將delete轉換成backspace")
  (verify-exit "退出時確認")
  (ask-before-changing-format "改變保存方式時確認")
  (wrap-words-in-editor-buffers "在編輯器中自動換行")
  (show-status-line "顯示狀態欄")
  (count-columns-from-one "從一開始計算行號")
  (display-line-numbers "在編輯器中顯示行號")
  (show-line-and-column-numbers "顯示行號和列號(&&)") ; used for popup menu; right click on line/column box in bottom of drs window
  (show-character-offsets "顯示字元在文件中的位置") ; used for popup menu; right click on line/column box in bottom of drs window
  (enable-keybindings-in-menus "允許使用菜單中的快捷鍵")
  (printing-mode "列印模式")
  (print-using-platform-specific-mode "平台特定的列印")
  (print-to-ps "列印至PostScript文件")
  (print-to-pdf "列印至PDF文件")
  (command-as-meta "將command鍵當作meta") ;; macos/macos x only
  (reuse-existing-frames "在打開新文件時，使用現有的視窗")
  (default-fonts "默認字體")
  (paren-match-color "高亮顯示括號所使用的顏色") ; in prefs dialog
  (online-coloring-active "實時根據語法用顏色標記程序")
  (open-files-in-tabs "在不同的標籤下打開多個文件（不使用多個視窗）")
  (show-interactions-on-execute "在運行程序時自動打開交互視窗")
  (switch-to-module-language-automatically "打開module文件時自動切換至module語言")
  (interactions-beside-definitions "將定義視窗和交互視窗左右放置") ;; in preferences, below the checkbox one line above this one
  (show-line-numbers "顯示行號")
  (show-line-numbers/menu "顯示行號(&N)")  ;; just like the above, but capitalized for appearance in a menu item
  (hide-line-numbers/menu "隱藏行號(&N)")
  (show-line-numbers-in-definitions "在定義視窗中顯示全部行號")
  ;; the constant above shows up in the popup menu item in the bottom of
  ;; the drracket window; controls the line numbers on each line in the definitions; used in a checkable menu item
  (limit-interactions-size "限制交互視窗的大小")
  (background-color "背景顏色")
  (default-text-color "默認顏色") ;; used for configuring colors, but doesn't need the word "color"
  (choose-a-background-color "請選擇背景顏色")
  (revert-to-defaults "恢復默認")
  (undo-changes "不做修改並退出") ;; used in the preferences dialog to undo preference changes
  
  (black-on-white-color-scheme "白底黑字") ;; these two appear in the color preferences dialog on butttons
  (white-on-black-color-scheme "黑底白字") ;; clicking the buttons changes the color schemes to some defaults that've been set up.
  
  (add-spacing-between-lines "在行間額外填充一個像素")
  
  ; title of the color choosing dialog
  
  ; should have entire alphabet
  (font-example-string "繁體中文 by 朱崇愷")
  
  (change-font-button-label "更改")
  (fonts "字體")
  (other... "其他…") ;; used in the font choice menu item
  
  ; filled with type of font, eg modern, swiss, etc.
  (choose-a-new-font "請選擇新的「~a」字體")
  
  (font-size-slider-label "字型大小")
  (restart-to-see-font-changes "重新啟動，使修改生效")
  
  (font-prefs-panel-title "字體")
  (font-name "字體名稱")
  (font-size "字體大小")
  (set-font "設置字體…")
  (font-smoothing-label  "字體平滑度")
  (font-smoothing-none "無")
  (font-smoothing-some "部分")
  (font-smoothing-all "全部")
  (font-smoothing-default "使用系統默認值")
  (select-font-name "選擇字體")
  (example-text "示例文本：")
  (only-warn-once "當定義視窗和交互視窗不同步時，僅警告一次")
  
  ; warning message when lockfile is around
  (waiting-for-pref-lock "等待首選項設置文件解鎖……")
  (pref-lock-not-gone
   "首選項設置文件被\n\n~a\n\n鎖定。請確定沒有其他Racket軟體正在運行中，然後刪除該鎖定文件。")
  (still-locked-exit-anyway? "首選項無法保存。仍然退出？")
  
  ;;; indenting preferences panel
  (indenting-prefs-panel-label "縮進")
  (indenting-prefs-extra-regexp "額外的正則表達式")
  
  (square-bracket-prefs-panel-label "中括號")
  
  ; filled with define, lambda, or begin
  (enter-new-keyword "請輸入一個類似於~a的關鍵詞：")
  (x-keyword "~a關鍵詞")
  (x-like-keywords "~a類型的關鍵詞")
  
  ; used in Square bracket panel
  (skip-subexpressions "出現在中括號前的表達式數量")
  
  (expected-a-symbol "需要symbol，實得：~a")
  (already-used-keyword "「~a」已經是縮進關鍵詞了")
  (add-keyword "添加")
  (remove-keyword "刪除")
  
  ; repl color preferences
  (repl-colors "REPL")
  (repl-out-color "輸出")
  (repl-value-color "值")
  (repl-error-color "錯誤")
  
  ;;; find/replace
  (search-next "下一個")
  (search-previous "上一個")
  (search-match "匹配")  ;;; this one and the next one are singular/plural variants of each other
  (search-matches "匹配")
  (search-replace "替換")
  (search-skip "跳過")
  (search-show-replace "顯示替換")
  (search-hide-replace "隱藏替換")
  (find-case-sensitive "大小寫敏感")  ;; the check box in both the docked & undocked search
  (find-anchor-based "用錨進行搜索")
  
  ;; these string constants used to be used by searching,
  ;; but aren't anymore. They are still used by other tools, tho.
  (hide "隱藏")
  (dock "停靠")
  (undock "取消停靠")
  
  ;;; multi-file-search
  (mfs-multi-file-search-menu-item "在文件中查找(&F)…")
  (mfs-string-match/graphics "字元串匹配（可用與包含圖像的文件）")
  (mfs-regexp-match/no-graphics "正則表達式匹配（只適用於純文本文件）")
  (mfs-searching... "查找…")
  (mfs-configure-search "查找設置") ;; dialog title
  (mfs-files-section "文件")   ;; section in config dialog
  (mfs-search-section "查找") ;; section in config dialog
  (mfs-dir "目錄")
  (mfs-recur-over-subdirectories "包含子目錄")
  (mfs-regexp-filename-filter "文件名篩選（正則表達式）")
  (mfs-search-string "查找字元串")
  (mfs-drscheme-multi-file-search "多文件查找——DrRacket") ;; results window and error message title
  (mfs-not-a-dir "「~a」不是目錄")
  (mfs-open-file "打開文件")
  (mfs-stop-search "停止查找")
  (mfs-case-sensitive-label "大小寫敏感")
  (mfs-no-matches-found "沒有找到匹配結果。")
  (mfs-search-interrupted "查找中止。")
  (mfs-drscheme-multi-file-search-title "多文件查找「~a」——DrRacket") ;; the ~a format specifier is filled in with the search string
  
  ;;; reverting a file
  (are-you-sure-revert
   "你確定要復原該文件嗎？這一操作無法撤銷。")
  (are-you-sure-revert-title
   "復原？")
  
  ;;; saving a file
  ; ~a is filled with the filename
  (error-saving "保存出錯") ;; title of error message dialog
  (error-saving-file/name "在保存文件~a時出現錯誤。")
  (error-loading "讀取出錯")
  (error-loading-file/name "在讀取~a時出現錯誤.")
  (unknown-filename "《未知》")
  
  ;;; finder dialog
  (must-specify-a-filename "你必須指定文件名")
  (file-does-not-exist "文件「~a」不存在。")
  (ask-because-file-exists "文件「~a」已存在。是否替換？")
  (dne-or-cycle "文件「~a」中包含不存在的目錄或循環")
  (get-file "獲取文件")
  (put-file "放置文件")
  (full-pathname "完整路徑")
  (show-dot-files "由點號開始顯示文件名和目錄名。")
  (up-directory-button-label "上層目錄")
  (add-button-label "添加") ;;; for multi-file selection
  (add-all-button-label "全部添加") ;;; for multi-file selection
  (remove-button-label "移除") ;;; for multi-file selection
  (file-wrong-form "該文件名格式不正確")
  (select-files "選擇文件")
  (select-file "選擇文件")
  (dir-dne "該目錄不存在。")
  (file-dne "該文件不存在。")
  (empty-filename "文件名中必須包含文字。")
  (that-is-dir-name "這是一個目錄名。")
  
  ;;; raw menu names -- these must match the 
  ;;; versions below, once the &s have been stripped.
  ;;; if they don't, DrRacket's menus will appear
  ;;; in the wrong order.
  (file-menu "文件")
  (edit-menu "編輯")
  (help-menu "幫助")
  (windows-menu "視窗")
  (tabs-menu "標籤") ;; this is the name of the "Windows" menu under linux & windows
  
  ;;; menus
  ;;; - in menu labels, the & indicates a alt-key based shortcut.
  ;;; - sometimes, things are stuck in the middle of 
  ;;; menu item labels. For instance, in the case of
  ;;; the "Save As" menu, you might see: "Save Definitions As". 
  ;;; be careful of spacing, follow the English, if possible.
  ;;; - the ellipses in the `after' strings indicates that
  ;;; more information is required from the user before completing
  ;;; the command.
  
  (file-menu-label "文件(&F)")
  
  (new-info  "打開新文件")
  (new-menu-item "新建(&N)")
  (new-...-menu-item "新建(&N)…")
  
  (open-info "打開現有文件")
  (open-menu-item "打開(&O)…")
  
  (open-recent-info "最近使用過文件的列表")
  (open-recent-menu-item "最近使用過(&T)")
  
  (revert-info "將當前文件恢復為磁碟上的副本")
  (revert-menu-item "恢復(&R)")
  
  (save-info "保存當前文件")
  (save-menu-item "保存(&S)")
  
  (save-as-info "輸入新的文件名,保存當前文件")
  (save-as-menu-item "另存為(&A)…")
  
  (print-info "列印當前文件")
  (print-menu-item "列印(&P)…")
  
  (page-setup-info "設置列印參數")
  (page-setup-menu-item "頁面設置…")
  
  (close-info "關閉當前文件")
  (close-menu-item "關閉(&C)")
  (close-window-menu-item "關閉視窗(&C)")
  
  (quit-info "關閉所有視窗")
  (quit-menu-item-windows "退出(&X)")
  (quit-menu-item-others "退出(&Q)")
  
  (edit-menu-label "編輯(&E)")
  
  (undo-info "撤銷最近的操作")
  (undo-menu-item "撤銷(&U)")
  
  (redo-info "取消最近的撤銷操作")
  (redo-menu-item "重複(&R)")
  
  (cut-info "將當前選中的對象移入剪貼版")
  (cut-menu-item "剪切(&T)")
  
  (copy-info "將當前選中的對象複製到剪貼版")
  (copy-menu-item "複製(&C)")
  
  (paste-info "將剪貼版中個內容複製到當前位置")
  (paste-menu-item "粘貼(&P)")
  
  (clear-info "刪除當前選中的對象")
  (clear-menu-item-windows "刪除(&D)")
  
  (select-all-info "選中整個文件")
  (select-all-menu-item "全選(&L)")
  
  (find-menu-item "查找") ;; menu item
  (find-from-selection-menu-item "查找當前選中")
  (find-info "在主視窗和查找欄之間切換游標位置")
  
  (find-again-info "跳至該文本的下一個出現")
  (find-again-menu-item "查找下一個")
  
  (find-again-backwards-info "跳至該文本的前一個出現")
  (find-again-backwards-menu-item "查找上一個")
  
  (show-replace-menu-item "顯示替換")
  (hide-replace-menu-item "隱藏替換")
  (show/hide-replace-info "切換替換面板的可見性")
  
  (replace-menu-item "替換")
  (replace-info "替換當前圈出的查找結果")
  
  (replace-all-info "替換查找字元串的所有出現")
  (replace-all-menu-item "全部替換")
  
  (find-case-sensitive-info "切換大小寫敏感或不敏感查找")
  (find-case-sensitive-menu-item "大小寫敏感")
  
  (complete-word "自動完成") ; the complete word menu item in the edit menu
  (no-completions "……無自動完成結果") ; shows up in the completions menu when there are no completions (in italics)
  
  (overwrite-mode "覆蓋模式")
  (enable-overwrite-mode-keybindings "啟用覆蓋模式的快捷鍵")
  
  (enable-automatic-parens "自動關括號") ; should "and square brackets and quotes" appear here?
  
  (preferences-info "設置首選項")
  (preferences-menu-item "首選項…")
  
  (keybindings-info "顯示當前快捷鍵")
  (keybindings-menu-item "快捷鍵")
  (keybindings-show-active "顯示當前的快捷鍵")
  (keybindings-frame-title "快捷鍵")
  (keybindings-sort-by-name "按名稱排序")
  (keybindings-sort-by-key "按鍵名排序")
  (keybindings-add-user-defined-keybindings "添加自定義快捷鍵…")
  (keybindings-add-user-defined-keybindings/planet "從PLaneT添加自定義快捷鍵…")
  (keybindings-menu-remove "移除~a")
  (keybindings-choose-user-defined-file "請選擇一個包含快捷鍵的文件")
  (keybindings-planet-malformed-spec "錯誤的PLaneT名稱：~a") ; the string will be what the user typed in
  (keybindings-type-planet-spec "請輸入PLaneT包名稱（無需輸入『require』）")
  
  ; first ~a will be a string naming the file or planet package where the keybindings come from;
  ; second ~a will be an error message
  (keybindings-error-installing-file "安裝快捷鍵~a時出錯：\n\n~a")
  
  (user-defined-keybinding-error "快捷鍵~a出錯\n\n~a")
  (user-defined-keybinding-malformed-file "文件~a並不是一個使用framework/keybinding-lang語言編寫的module.")
  (user-defined-keybinding-malformed-file/found-lang
   "文件~a並不是一個使用framework/keybinding-lang語言編寫的module，而是由~s語言編寫")
  
  ;; menu items in the "special" menu
  (insert-text-box-item "插入文本框")
  (insert-image-item "插入圖片…")
  (insert-comment-box-menu-item-label "插入注釋框")
  (insert-lambda "插入λ")
  
  (wrap-text-item "自動換行")
  
  (windows-menu-label "視窗(&W)")
  (tabs-menu-label "標籤(&T)") ;; this is the name of the menu under linux & windows
  (minimize "最小化") ;; minimize and zoom are only used under mac os x
  (zoom "縮放")
  (bring-frame-to-front "前端顯示")       ;;; title of dialog
  (bring-frame-to-front... "前端顯示…") ;;; corresponding title of menu item
  (most-recent-window "最近的視窗")
  (next-tab "下一個標籤")
  (prev-tab "前一個標籤")
  ;; menu item in the windows menu under mac os x. first ~a is filled with a number between 1 and 9; second one is the filename of the tab
  (tab-i "標籤~a：~a")
  (tab-i/no-name "標籤~a")
  
  (view-menu-label "視圖(&V)")
  (show-overview "顯示程序輪廓(&P)") 
  (hide-overview "隱藏程序輪廓(&P)")
  (show-module-browser "顯示&Module瀏覽器")
  (hide-module-browser "隱藏&Module瀏覽器")
  
  (help-menu-label "幫助(&H)")
  (about-info "本軟體的詳細信息以及致謝名單")
  (about-menu-item "關於…")
  
  ;; open here's new menu item
  (create-new-window-or-clear-current
   "您是想打開一個新視窗，還是清空當前視窗？")
  (clear-current "清空當前")
  (new-window "新視窗")
  
  ;; popup menu when right-clicking in the gap between
  ;; the definitions and interactions window
  (change-to-vertical-alignment "左右分割")
  (change-to-horizontal-alignment "上下分割")
  
  ;;; exiting and quitting ``are you sure dialog
  ;;; exit is used on windows, quit on macos, in English. Other
  ;;; languages probably use the same word on both platforms.
  (exit "退出")
  (quit "退出")
  (are-you-sure-exit "你確定要退出嗎?")
  (are-you-sure-quit "你確定要退出嗎?")
  ; these next two are only used in the quit/exit dialog
  ; on the button whose semantics is "dismiss this dialog".
  ; they are there to provide more flexibility for translations
  ; in English, they are just cancel.
  (dont-exit "取消") 
  (dont-quit "取消")
  
  ;;; autosaving
  (error-autosaving "自動保存\"~a\"時出錯。") ;; ~a will be a filename
  (autosaving-turned-off "在文件被存檔之前，自動保存不會進行")
  (recover-autosave-files-frame-title "恢復自動保存的文件")
  (autosave-details "詳細情況")
  (autosave-recover "恢復")
  (autosave-unknown-filename "《未知》")
  
  ;; these are labels in a dialog that drscheme displays
  ;; if you have leftover autosave files. to see the dialog,
  ;; start up drscheme and modify (but don't save) a file
  ;; (also, do this with an unsaved file). Wait for the autosave
  ;; files to appear (typically 5 minutes). Kill DrRacket
  ;; and restart it. You'll see the dialog
  (autosave-autosave-label: "自動保存文件：")
  (autosave-original-label: "原始文件：")
  (autosave-autosave-label "自動保存文件")
  (autosave-original-label "原始文件")
  (autosave-compare-files "比較自動保存文件")
  
  (autosave-show-autosave "自動保存文件") ;; title of a window showing the autosave file
  
  (autosave-explanation "DrRacket發現了自動保存的文件，其中可能包含你沒有保存過的程序")
  
  (autosave-recovered! "已恢復！") ;; status of an autosave file
  (autosave-deleted "已刪除")       ;; status of an autosave file
  
  (autosave-error-deleting "刪除~a出錯\n\n~a") ;; first is a filename, second is an error message from mz.
  (autosave-delete-button "刪除")
  (autosave-delete-title "刪除")  ;; title of a dialog asking for deletion confirmation
  (autosave-done "完成")
  
  ;; appears in the file dialog
  (autosave-restore-to-where? "請選擇自動保存文件的位置")
  
  
  ;;; file modified warning
  (file-has-been-modified
   "文件在上次保存之後又改動過。覆蓋這些更改？")
  (overwrite-file-button-label "覆蓋")
  
  (definitions-modified 
    "當前磁碟文件已被修改；請保存或恢復文件。")
  (drscheme-internal-error "DrRacket內部錯誤")
  
  ;;; tools
  (invalid-tool-spec "Collection ~a中info.rkt的tool定義不正確。需要一個字元串或者一個非空表，實得：~e")
  (error-invoking-tool-title "調用tool ~s出錯；~s")
  (error-loading-tool-title "載入tool ~s出錯\n~a") ;; ~s filled with a path, ~a filled with an error message from an exn
  
  (tool-tool-names-same-length
   "在~s的info.rkt文件中，『tool-names』和『tools』應該是等長的表，實得~e和~e")
  (tool-tool-icons-same-length
   "在~s的info.rkt文件中，『tool-icons』和「tools』應該是等長的表，實得~e和~e")
  (tool-tool-urls-same-length
   "在~s的info.rkt文件中，『tool-urls』和「tools』應該是等長的表，實得~e和~e")
  (error-getting-info-tool
   "載入~s的info.rkt出錯")
  (tool-error-phase1 "tool ~s第一階段出錯；~s")
  (tool-error-phase2 "tool ~s第二階段出錯；~s")
  
  
  ;;; define popup menu
  (end-of-buffer-define "<<緩衝區結束>>")
  (sort-by-name "按名稱排序")
  (sort-by-position "按文件中的位置排序")
  (no-definitions-found "<<沒有定義>>")
  (jump-to-defn "跳至~a的定義")
  
  (recent-items-sort-by-age "按時間排序")
  (recent-items-sort-by-name "按名稱排序")
  
  ;;; view menu
  (hide-definitions-menu-item-label "隱藏定義(&D)")
  (show-definitions-menu-item-label "顯示定義(&D)")
  (definitions-menu-item-help-string "顯示/隱藏定義視窗")
  (show-interactions-menu-item-label "顯示交互(&I)")
  (hide-interactions-menu-item-label "隱藏交互(&I)")
  (use-horizontal-layout "左右分割")
  (use-vertical-layout "上下分割")
  (interactions-menu-item-help-string "顯示/隱藏交互視窗")
  (toolbar "工具欄")
  (toolbar-on-top "頂置工具欄")
  (toolbar-on-top-no-label "頂置工具欄並使用小圖標")
  (toolbar-on-left "左置工具欄")
  (toolbar-on-right "右置工具欄")
  (toolbar-hidden "隱藏工具欄")
  
  ;;; file menu
  (save-definitions-as "將定義另存為(&A)")
  (save-definitions "保存定義")
  (print-definitions "列印定義…")
  (about-drscheme "關於DrRacket")
  (save-other "保存其他")
  (save-definitions-as-text "將定義保存為文本…")
  (save-interactions "保存交互")
  (save-interactions-as "將交互另存為…")
  (save-interactions-as-text "將交互保存為文本…")
  (print-interactions "列印交互…")
  (new-tab "新建標籤")
  (close-tab "關閉標籤") ;; must not have any &s in it.
  (close-tab-amp "關閉標籤(&C)") ;; like close-tab, but with an ampersand on the same letter as the one in close-menu-item
  
  ;;; edit-menu
  (split-menu-item-label "分屏(&S)")
  (collapse-menu-item-label "合併(&O)")
  (find-longest-line "尋找最長一行")
  
  ;;; language menu
  (language-menu-name "語言(&L)")
  
  ;;; scheme-menu
  (scheme-menu-name "Ra&cket")
  (execute-menu-item-label "運行")
  (execute-menu-item-help-string "重新運行定義視窗中的程序")
  (ask-quit-menu-item-label "要求程序終止")
  (ask-quit-menu-item-help-string "使用break-thread中止當前計算得主執行緒")
  (force-quit-menu-item-label "強制終止")
  (force-quit-menu-item-help-string "使用custodian-shutdown-all退出當前計算")
  (limit-memory-menu-item-label "限制內存使用…")
  (limit-memory-msg-1 "內存限制會在下一次運行時生效，")
  (limit-memory-msg-2 "內存限制最低值為八兆位元組。")   (limit-memory-unlimited "無限制")
  (limit-memory-limited "限制")
  (limit-memory-megabytes "兆位元組(MB)")
  ; the next two constants are used together in the limit memory dialog; they are inserted
  ; one after another. The first one is shown in a bold font and the second is not.
  ; (the first can be the empty string)
  (limit-memory-warning-prefix "警告：")
  (limit-memory-warning 
   "不限制內存有潛在的危險。DrRacket將無法保護自己，當運行的程序分配過多內存時，DrRacket會崩潰.")
  
  (clear-error-highlight-menu-item-label "清除錯誤高亮顯示")
  (clear-error-highlight-item-help-string "清除錯誤區域的粉紅色高亮顯示")
  (jump-to-next-error-highlight-menu-item-label "跳至下一個高亮錯誤")
  (jump-to-prev-error-highlight-menu-item-label "跳至前一個高亮錯誤")
  (reindent-menu-item-label "調整縮進(&R)")
  (reindent-all-menu-item-label "全文調整縮進(&A)")
  (semicolon-comment-out-menu-item-label "用分號注釋(&C)")
  (box-comment-out-menu-item-label "用注釋框注釋(&C)")
  (uncomment-menu-item-label "取消注釋(&U)")
  
  (convert-to-semicolon-comment "轉化為分號注釋")
  
  ;;; executables
  (create-executable-menu-item-label "創建可執行程序(&E)…")
  (create-executable-title "創建可執行程序")
  (drracket-creates-executables-only-in-some-languages
   "在DrRacket中創建可執行程序僅支持下列語言選擇：在DrRacket語言對話框中選擇教學語言（DMdA或者HtDP）；在DrRacket語言對話框中選擇「Racket語言」，並且用#lang在文件開始處指定語言。\n\n請考慮使用命令行工具raco exe。")
  (must-save-before-executable "在創建可執行程序之前，你必須保存源程序")
  (save-a-mred-launcher "保存為GRacket啟動程序")
  (save-a-mzscheme-launcher "保存為Racket啟動程序")
  (save-a-mred-stand-alone-executable "保存為GRacket獨立可執行程序")
  (save-a-mzscheme-stand-alone-executable "保存為Racket獨立可執行程序")
  (save-a-mred-distribution "保存為GRacket可發布程序")
  (save-a-mzscheme-distribution "保存為Racket可發布程序")
  (error-creating-executable "創建可執行程序出錯：") ;; this is suffixed with an error message ala error-display-handler
  
  (definitions-not-saved "當前定義視窗中的程序並沒有被保存過。將使用最近保存過的版本來生成可執行程序。是否繼續？")
  ;; The "-explanatory-label" variants are the labels used for the radio buttons in
  ;;  the "Create Executable..." dialog for the "(module ...)" language.
  (launcher "啟動程序")
  (launcher-explanatory-label "啟動程序（僅在本機運行，運行原始碼）")
  (stand-alone "獨立程序")
  (stand-alone-explanatory-label "獨立程序（僅在本機運行，運行編譯代碼）")
  (distribution "可發布程序")
  (distribution-explanatory-label "可發布程序（可以在其它計算機上安裝並運行）")
  (executable-type "類型")
  (executable-base "基於")
  (filename "文件名：")
  (create "創建")
  (please-specify-a-filename "請指定文件名。")
  (~a-must-end-with-~a
   "~a文件名\n\n  ~a\n\n不合法。文件名必須以\".~a\"結尾。")
  (macosx-executables-must-end-with-app
   "~a文件名\n\n  ~a\n\n不合法。在MacOS X中，文件名必須以.app結尾。")
  (warning-directory-will-be-replaced
   "警告：目錄：\n\n  ~a\n\n將會被重置。繼續操作？")
  
  (distribution-progress-window-title "創建進程")
  (creating-executable-progress-status "創建可執行程序……")
  (assembling-distribution-files-progress-status "彙編……")
  (packing-distribution-progress-status "打包……")
  
  (create-servlet "創建Servlet……")
  
  ; the ~a is a language such as "module" or "algol60"
  (create-servlet-unsupported-language
   "無法為~a語言程序創建Servlet。")
  
  ;;; buttons
  (execute-button-label "運行") 
  (save-button-label "保存")
  (break-button-label "停止")
  
  ;;; search help desk popup menu
  (search-help-desk-for "在幫助台中搜索「~a」")
  (exact-lucky-search-help-desk-for "在幫助台中搜索最符合「~a」的一個頁面")
  
  ;; collapse and expand popup menu items
  (collapse-sexp "摺疊S表達式")
  (expand-sexp "擴展S表達式")
  
  ;;; fraction dialog
  (enter-fraction "輸入分數")
  (whole-part "整數部分")
  (numerator "分子")
  (denominator "分母")
  (insert-number/bad-whole-part "整數部分必須輸入一個整數")
  (insert-number/bad-numerator "分子必須是非負整數")
  (insert-number/bad-denominator "分母必須是正整數")
  (insert-fraction-menu-item-label "插入分數…")
  
  ;; number snip popup menu
  (show-decimal-expansion "用十進制表示")
  (show-mixed-fraction-view "用帶分數表示")
  (show-improper-fraction-view "用假分數表示")
  (show-more-decimal-places "顯示更多小數位")
  
  ;;; Teachpack messages
  (select-a-teachpack "選擇教學包")
  (clear-teachpack "卸載教學包~a")
  (teachpack-error-label "DrRacket——教學包出錯")
  (teachpack-didnt-load "無法裝載教學包~a。")
  (add-teachpack-menu-item-label "載入教學包……")
  (clear-all-teachpacks-menu-item-label "卸載全部教學包")
  (drscheme-teachpack-message-title "DrRacket教學包")
  (already-added-teachpack "教學包~a已裝載")
  
  ; ~a is filled with the teachpack's name; the message appears in the teachpack selection dialog when a user installs a new teachpack
  (compiling-teachpack "編譯教學包~a……")
  (teachpack-pre-installed "自帶的教學包")
  (teachpack-pre-installed/htdp "自帶的HtDP教學包")
  (teachpack-pre-installed/2htdp "自帶的HtDP/2e教學包")
  (teachpack-user-installed "用戶安裝的教學包")
  (add-teachpack-to-list... "添加教學包…")
  (teachpack-already-installed "已經存在一個名為'~a'的教學包。是否覆蓋？")
  ; ~a is filled with a list of language names. Each name is separated by a newline and is indented two spaces (no commas, no 'and')
  (teachpacks-only-in-languages "教學包菜單僅在下列語言中有效：~a\n\n在其他語言中，使用「require」。")
  
  ;;; Language dialog
  (introduction-to-language-dialog
   "請選擇語言。大部分入門級的學生都可以使用默認語言。")
  (language-dialog-title "語言選擇")
  (case-sensitive-label "大小寫敏感")
  (output-style-label "輸出格式")
  (constructor-printing-style "構造器")
  (quasiquote-printing-style "Quasiquote")
  (write-printing-style "write")
  (print-printing-style "print")
  (sharing-printing-label "顯示內存共享")
  (use-pretty-printer-label "print多個對象時自動換行")
  (input-syntax "輸入語法")
  (dynamic-properties "動態屬性")
  (output-syntax "輸出語法")
  (teachpacks "教學包") ;; label in the language dialog for the teaching languages
  (teachpacks-none "《無》") ;; shows up under the previous string, when there are no teachpacks
  (no-debugging-or-profiling "不調試，也不性能分析")
  (debugging "調試")
  (debugging-and-profiling "調試，並性能分析")
  (test-coverage "語法測試套件覆蓋")
  (show-details-button-label "顯示詳情")
  (hide-details-button-label "隱藏詳情")
  (choose-language-menu-item-label "選擇語言…")
  (revert-to-language-defaults "恢復默認語言設置")
  (fraction-style "分數格式")
  (use-mixed-fractions "帶分數")
  (use-repeating-decimals "循環小數")
  (decimal-notation-for-rationals "使用十進制表示有理數")
  (enforce-primitives-group-box-label "初始綁定")
  (enforce-primitives-check-box-label "不允許改變初始綁定")
  (automatically-compile "填充「compiled」目錄（載入更快）")
  (preserve-stacktrace-information "保留堆疊跟蹤信息（禁用某些優化）")
  (expression-level-stacktrace "表達式級堆疊跟蹤")
  (function-level-stacktrace "函數級堆疊跟蹤")
  (submodules-to-run "運行子module")
  (add-submodule "添加子module選項…") ;; menu item
  (add-submodule-title "添加子module") ;; title of dialog opened by above menu item
  
  ; used in the bottom left of the drscheme frame as the label
  ; above the programming language's name
  ; used the popup menu from the just above; greyed out and only
  ; visible when some languages are in the history
  (recent-languages "最近使用的語言：")
  ; shows up in bottom-left programming language menu popup, when no langs are recorded
  (no-recently-chosen-languages "沒有最近使用過的語言")
  
  ;; startup wizard screen language selection section
  (please-select-a-language "請選擇語言")
  
  
  ;;; languages
  (beginning-student "初級")
  (beginning-one-line-summary "define、cond、結構體、常量和基本操作")
  (beginning-student/abbrev "初級+縮寫的表")
  (beginning/abbrev-one-line-summary "在初級的基礎上，用縮寫形式輸出表")
  (intermediate-student "中級")
  (intermediate-one-line-summary "在初級的基礎上增加了詞法作用域")
  (intermediate-student/lambda "中級+lambda")
  (intermediate/lambda-one-line-summary "在中級的基礎上，增加了高階函數")
  (advanced-student "高級")
  (advanced-one-line-summary "在中級的基礎上，增加了lambda和賦值")
  (how-to-design-programs "程序設計方法/How to Design Programs") ;; should agree with MIT Press on this one...
  (pretty-big-scheme "大")
  (pretty-big-scheme-one-line-summary "MzScheme/MrEd加HtDP(程序設計方法)語言")
  (r5rs-lang-name "R5RS")
  (r5rs-one-line-summary "Scheme語言標準第5修改稿")
  (expander "Expander")
  (expander-one-line-summary "展開表達式，而不是求值")
  (legacy-languages "過去的語言")
  (teaching-languages "教學語言")
  (experimental-languages "實驗語言")
  (initial-language-category "初始語言")
  (no-language-chosen "還沒有選擇語言")
  (other-languages "其他語言")
  
  (module-language-name "由原始碼來確定語言")
  (module-language-one-line-summary "由#lang行來確定實際使用的語言")
  (module-language-auto-text "自動加入#lang行") ;; shows up in the details section of the module language
  
  ;; for the upper portion of the language dialog
  (the-racket-language "Racket語言")
  (choose-a-language "選擇語言")
  
  ;; the next two string constants appear in the
  ;; language dialog with a list
  ;; of example languages appearing between them
  (racket-language-discussion "你的程序將從#lang開始，從而指定想用的方言。例如：\n")
  (racket-language-discussion-end "\n……諸如此類")
  
  ;; the next three string constants are put into a message-box dialog
  ;; that appears when the user clicks on the example #lang languages
  ;; in the language dialog. The first one always appears and then either
  ;; the second or the third appears. The second one has the clicked
  ;; on #lang line placed into the ~a, and third one has the 
  ;; current #lang line in the first ~a and the clicked on in the second one.
  ;; The two comments are separated by a blank line.
  (racket-dialect-in-buffer-message 
   "Racket的方言一般而言由原始碼指定，而不是語言對話框這裡的選項指定。")
  (racket-dialect-add-new-#lang-line "這就是說，需要在定義視窗開頭添加「~a」嗎？")
  (racket-dialect-replace-#lang-line "這就是說，在你的文件里現在有「~a」；需要將其替換成「~a」嗎？")
  (racket-dialect-already-same-#lang-line "不過你的文件中已經包含「~a」；你可以開始編寫程序了！")
  
  ;; in the dialog containing the above strings, one of these is a button that appears
  (add-#lang-line "添加#lang行")
  (replace-#lang-line "替換#lang行")
  
  ;; for the 'new drracket user' dialog
  (use-language-in-source "使用代碼中指定的語言")
  
  ;;; from the `not a language language' used initially in drscheme.
  (must-choose-language "在繼續操作之前，你必須為DrRacket選擇一種程式語言。")
  
  ; next two appear before and after the name of a text book (which will be in italics)
  (using-a-textbook-before "使用")
  (using-a-textbook-after "？")
  
  ; next two are before and after a language
  (start-with-before "由")
  (start-with-after "開始？")
  
  (seasoned-plt-schemer? "PLT Scheme高手?")
  (racketeer? "你是Racketeer嗎？")
  (looking-for-standard-scheme? "想要標準的Scheme?")
  
  ; the three string constants are concatenated together and the middle
  ; one is hyperlinked to the dialog that suggests various languages
  (get-guidance-before "請使用「語言」菜單中的「選擇語言」對話框，或者")
  (get-guidance-during "由DrRacket幫助你選擇")
  (get-guidance-after "。")
  
  ;;; debug language
  (unknown-debug-frame "[未知]")
  (backtrace-window-title "向後跟蹤—DrRacket")
  (files-interactions "~a的交互") ;; filled with a filename
  (current-interactions "交互")
  (current-definitions "定義")
  (mzscheme-w/debug "文本（MzScheme，包含R5RS）")
  (mzscheme-one-line-summary "PLT的Scheme實現")
  (mred-w/debug "圖形（MrEd，包含MzScheme）")
  (mred-one-line-summary "在MzScheme的基礎上增加GUI支持")
  
  ;; profiling
  (profiling-low-color "低")
  (profiling-high-color "高")
  (profiling-choose-low-color "請選擇代表低的顏色")
  (profiling-choose-high-color "請選擇代表高的顏色")
  (profiling "性能分析")
  (profiling-example-text "(define (馬) (馬))")
  (profiling-color-config "性能分析色譜")
  (profiling-scale "性能分析的色彩比例")
  (profiling-sqrt "平方根")
  (profiling-linear "線性")
  (profiling-square "平方")
  (profiling-number "調用次數")
  (profiling-time "累積時間")
  (profiling-update "更新性能分析")
  (profiling-col-percent-time "%次")
  (profiling-col-function "函數")
  (profiling-col-time-in-msec "毫秒")
  (profiling-col-calls "調用")
  (profiling-show-profile "顯示性能分析")
  (profiling-hide-profile "隱藏性能分析")
  (profiling-unknown-src "《未知》")
  (profiling-no-information-available "沒有可用的性能分析信息。請確定在語言設置中啟用了性能分析，並且運行了當前程序。")
  (profiling-clear? "改變定義視窗的內容將導致性能分析信息失效。是否繼續？")
  
  ;; test coverage
  (test-coverage-clear? "改變定義視窗將導致測試覆蓋信息失效。是否繼續？")
  (test-coverage-clear-and-do-not-ask-again "是，並且不再詢問")
  (test-coverage-ask? "詢問清除測試覆蓋")
  
  (test-coverage-on "覆蓋的測試")
  (test-coverage-off "未覆蓋的測試")
  
  ;; tracing
  (tracing-enable-tracing "啟用跟蹤")
  (tracing-show-tracing-window "顯示跟蹤")
  (tracing-hide-tracing-window "隱藏跟蹤")
  (tracing-tracing-nothing-to-show "暫時沒有可用的跟蹤結果。(請檢查你所使用的語言是否支持跟蹤以及是否啟用了跟蹤。)")
  
  ;;; repl stuff
  (evaluation-terminated "計算已終止")
  (evaluation-terminated-explanation
   "計算執行緒已停止，在下一次執行之前不會進行計算。")
  
  ; The next three constants show up in the same dialog as the above evaluation-terminated string
  ; constants.
  ; The first two show up only when the user calls 'exit' (possibly with a status code).
  ; The third shows up when the program runs out of memory.
  (exited-successfully "成功退出。")
  (exited-with-error-code "退出，錯誤代碼~a。") ;; ~a is filled in with a number between 1 and 255
  (program-ran-out-of-memory "內存耗盡。")
  
  (show-evaluation-terminated-dialog "顯示『計算終止』對話框")
  (evaluation-terminated-ask "下次再顯示該對話框")
  
  (last-stack-frame "顯示最新的棧幀")
  (last-stack-frames "顯示前~a個棧幀")
  (next-stack-frames "顯示後~a個棧幀")
  
  ;;; welcoming message in repl
  (language "語言")
  (custom "自定義")
  (teachpack "教學包")
  (welcome-to "歡迎使用")
  (version "版本")
  
  ;;; kill evaluation dialog
  (kill-evaluation? "是否要終止計算？")
  (just-break "中斷")
  (kill "終止")
  (kill? "終止？")
  
  ;;; version checker
  (version:update-menu-item   "檢查更新…")
  (version:update-check       "檢查更新") ; dialog title, with the next line
  (version:connecting-server  "連接Racket版本伺服器")
  (version:results-title      "Racket版本檢查")
  (version:do-periodic-checks "定期檢查Racket版本更新")
  (version:take-me-there      "下載") ; ...to the download website
  ;; the next one can appear alone, or followed by a comma and the one after that
  (version:plt-up-to-date     "您現在使用的已經是最新版本的Racket")
  (version:but-newer-alpha    "但是還有一個更新的alpha版本")
  ;; This is used in this context: "Racket vNNN <<<*>>> http://download..."
  (version:now-available-at   "可以從這裡獲取：")
  
  ;; insert menu
  (insert-menu "插入(&I)")
  
  ;; large semi colon letters
  (insert-large-letters... "插入大字…")
  (large-semicolon-letters "帶分號的大字")
  (text-to-insert "要插入的文字")
  
  (module-browser-filename-format "文件全名：~a（共~a行）")
  (module-browser-root-filename "根文件名：~a")
  (module-browser-font-size-gauge-label "字體大小")
  (module-browser-progress-label "Module概覽進程")
  (module-browser-adding-file "添加文件：~a…")
  (module-browser-laying-out-graph-label "正在布局")
  (module-browser-open-file-format "打開~a")
  (module-browser "Module瀏覽器") ;; frame title
  (module-browser... "&Module瀏覽器…") ;; menu item title
  (module-browser-in-file "M&odule瀏覽~a") ;; menu item title; ~a is filled with a filename
  (module-browser-no-file "Module瀏覽存檔文件") ;; menu item title for above menu item; used when there is no saved file
  (module-browser-error-expanding "展開程序時出錯：\n\n~a")
  (module-browser-show-lib-paths "顯示通過(lib ..)載入的文件的路徑")
  (module-browser-progress "Module瀏覽器：~a") ;; prefix in the status line
  (module-browser-compiling-defns "Module瀏覽器：正在編譯定義")
  (module-browser-show-lib-paths/short "跟隨lib調用") ;; check box label in show module browser pane in drscheme window.
  (module-browser-show-planet-paths/short "跟隨planet調用") ;; check box label in show module browser pane in drscheme window.
  (module-browser-refresh "刷新") ;; button label in show module browser pane in drscheme window.
  (module-browser-highlight "高亮顯示") ;; used to search in the graph; the label on a text-field% object
  (module-browser-only-in-plt-and-module-langs
   "Module瀏覽器只能對基於module的程序中使用。")
  (module-browser-name-length "名稱長度")
  (module-browser-name-short "短")
  (module-browser-name-medium "中")
  (module-browser-name-long "長")
  (module-browser-name-very-long "長，包含階段")  ;; like 'Long' but shows the phases where this file is loaded
  (module-browser-open-all "打開所有這些文件")
  
  (happy-birthday-matthias "生日快樂，Matthias！")
  (happy-birthday-matthew "生日快樂，馬曉！")
  (happy-birthday-shriram "生日快樂，Shriram！")
  
  (mrflow-using-default-language-title "正在使用默認語言")
  (mrflow-using-default-language "當前使用的語言並不包含其原素的類型。改用R5RS Scheme。")
  (mrflow-button-title "分析")
  ;(mrflow-unknown-style-delta-error-title "Unknown Box Style Delta")
  ;(mrflow-unknown-style-delta-error "Unknown box style delta: ~a")
  (mrflow-popup-menu-show-type "顯示類型")
  (mrflow-popup-menu-hide-type "隱藏類型")
  (mrflow-popup-menu-show-errors "顯示錯誤")
  (mrflow-popup-menu-hide-errors "隱藏錯誤")
  ;(mrflow-read-exception-title "Read Exception")
  ;(mrflow-read-exception "Read exception: ~a")
  ;(mrflow-syntax-exception-title "Syntax Exception")
  ;(mrflow-syntax-exception "Syntax exception: ~a")
  ;(mrflow-unknown-exception-title "Unknown Exception")
  ;(mrflow-unknown-exception "Unknown exception: ~a")
  ;(mrflow-language-primitives-error-title "Language Primitives Error")
  ;(mrflow-language-primitives-error "Wrong filename for language primitives types table: ~a")
  
  (snips-and-arrows-popup-menu-tack-all-arrows "固定所有箭頭")
  (snips-and-arrows-popup-menu-untack-all-arrows "取消固定所有箭頭")
  (snips-and-arrows-user-action-disallowed-title "當前不允許用戶改變")
  (snips-and-arrows-user-action-disallowed
   "在編輯器中包含由系統插入的段落，所以不允許用戶改變。在修改之前請先隱藏所有這些段落。")
  ;(snips-and-arrows-changing-terms-warning-title "Changing terms will be undoable")
  (snips-and-arrows-hide-all-snips-in-editor "在編輯器中隱藏所有段落")
  
  (xml-tool-insert-xml-box "插入XML框")
  (xml-tool-insert-scheme-box "插入Racket框")
  (xml-tool-insert-scheme-splice-box "插入Racket接合框")
  (xml-tool-xml-box "XML框")
  (xml-tool-scheme-box "Racket框")
  (xml-tool-scheme-splice-box "Racket接合框")
  (xml-tool-switch-to-scheme "轉化為Racket框")
  (xml-tool-switch-to-scheme-splice "轉化為Racket接合框")
  (xml-tool-eliminate-whitespace-in-empty-tags
   "消除空白標籤中的空白")
  (xml-tool-leave-whitespace-alone
   "保留空白")
  
  (show-recent-items-window-menu-item "在單獨視窗中顯示最近使用過的文件")
  (show-recent-items-window-label "最近使用過的文件")
  (number-of-open-recent-items "最近使用的數量")
  (switch-anyway "強制切換文件")
  
  (stepper-program-has-changed "注意：程序已改變。")
  (stepper-program-window-closed "注意：程序視窗已關閉。")
  
  (stepper-name "單步執行器")
  (stepper-language-level-message "單步執行不支持語言「~a」。")
  (stepper-button-label "單步執行")
  
  (stepper-previous "上一步")
  (stepper-next "下一步")
  (stepper-jump "跳至…")
  (stepper-jump-to-beginning "最前")
  (stepper-jump-to-end "最後")
  (stepper-jump-to-selected "當前選中的開始")
  (stepper-jump-to-previous-application "前一個調用步驟")
  (stepper-jump-to-next-application "下一個調用步驟")
  (stepper-out-of-steps "已到達計算終結，未找到目標步驟。")
  (stepper-no-such-step/title "未找到步驟")
  (stepper-no-such-step "找不到符合該標準的步驟。")
  (stepper-no-such-step/earlier "前向找不到符合該標準的步驟。")
  
  (stepper-no-earlier-application-step "前向無調用步驟。")
  (stepper-no-later-application-step "無調用步驟。")
  
  (stepper-no-earlier-step "前向無步驟。")
  (stepper-no-later-step "無步驟。")
  
  (stepper-no-selected-step "選中區域中沒有步驟。可能這些是注釋？")
  
  (stepper-no-last-step "最後步驟暫未生成。")
  
  
  (debug-tool-button-name "調試")
  
  (dialog-back "後退")
  
  ;; warnings about closing a drscheme frame when the program
  ;; might still be doing something interesting
  (program-is-still-running "定義視窗中的程序還在運行中。強制退出？")
  (program-has-open-windows "定義視窗中的程序打開了其他視窗。強行關閉這些視窗？")
  
  ;; ml-command-line-arguments is for the command line arguments
  ;; label in the module language details in the language dialog.
  (ml-command-line-arguments "命令行參數是字元串的數組，以紅色顯示")
  
  ;; ml-cp names are all for the module language collection path
  ;; configuration. See the details portion of the language dialog
  ;; for the module language (at the bottom).
  (ml-cp-default-collection-path "<<默認collection路徑>>")
  
  ;; in std get-directory 
  (ml-cp-choose-a-collection-path "請選擇collection路徑")
  
  ;; err msg when adding default twice
  (ml-cp-default-already-present
   "默認collection路徑已存在")
  
  ;; title of this section of the dialog (possibly the word
  ;; `Collection' should not be translated)
  (ml-cp-collection-paths "Collection路徑")
  
  ;; button labels
  (ml-cp-add "添加")
  (ml-cp-add-default "加為默認")
  (ml-cp-remove "清除")
  (ml-cp-raise "上移")
  (ml-cp-lower "下移")
  
  (ml-always-show-#lang-line "在Module語言中，始終顯示#langhang")
  
  ;; Strings for Profj removed as ProfessorJ is no longer actively developed as part of DrScheme
  
  
  ;;The Test engine tool
  ;;
  (test-engine-window-title "測試結果")
  ;;Following two appear in View menu, attach and free test report window from DrRacket frame
  (test-engine-dock-report "停靠測試報告")
  (test-engine-undock-report "取消停靠測試報告")
  ;;Following two appear in Racket (Java, etc) menu, cause Tests to be Run automatically or not
  (test-engine-enable-tests "啟用測試")
  (test-engine-disable-tests "停用測試")
  
  (test-engine-ran-1-test "運行了1個test。")
  (test-engine-ran-1-check "運行了1個check。")
  ;; ditto, only plural
  (test-engine-ran-n-tests "運行了~a個test。")
  (test-engine-ran-n-checks "運行了~a個check。")
  (test-engine-1-test-passed "Test通過！")
  (test-engine-1-check-passed "Check通過！")
  (test-engine-both-tests-passed "全部test通過！")
  (test-engine-both-checks-passed "全部check通過！")
  (test-engine-all-tests-passed "全部test通過！")
  (test-engine-all-checks-passed "全部check通過！")
  (test-engine-all-n-tests-passed "所有~a個test通過！")
  (test-engine-all-n-checks-passed "所有~a個check通過！")
  (test-engine-0-tests-passed "0個test通過。")
  (test-engine-0-checks-passed "0個check通過。")
  (test-engine-m-of-n-tests-failed "~a個test失敗（共~a個test）。")
  (test-engine-m-of-n-checks-failed "~a個check失敗（共~a個check）。")
  (test-engine-must-be-tested "程序必須要測試！")
  (test-engine-is-unchecked "程序還沒有check！")
  (test-engine-tests-disabled "測試未啟用。")
  (test-engine-should-be-tested "程序需要測試。")
  (test-engine-at-line-column "於行~a，列~a")
  (test-engine-in-at-line-column "於文件~a，行~a，列~a")
  ; as in "column (unknown)"
  (test-engine-unknown "（未知）")
  (test-engine-trace-error "跟蹤錯誤")
  
  ; The ~F is special marker for the offending values, which may be
  ; printed specially in DrRacket.
  ;; no way to translate these as the order of expected value and actual value (also range) should be ordered differently in Chinese
  (test-engine-check-encountered-error
   "check-expect encountered the following error instead of the expected value, ~F. ~n   :: ~a")
  (test-engine-actual-value-differs-error
   "Actual value ~F differs from ~F, the expected value.")
  (test-engine-actual-value-not-within-error
   "Actual value ~F is not within ~v of expected value ~F.")
  (test-engine-encountered-error-error
   "check-error encountered the following error instead of the expected ~a~n   :: ~a")
  (test-engine-expected-error-error
   "check-error expected the following error, but instead received the value ~F.~n ~a")
  (test-engine-expected-an-error-error
   "check-error expected an error, but instead received the value ~F.")
  ;; members are appended to the message
  (test-engine-not-mem-error "Actual value ~F differs from all given members in ")
  (test-engine-not-range-error "Actual value ~F is not between ~F and ~F, inclusive.")
  
  ;; followed by list of variable bindings
  (test-engine-property-fail-error "Property falsifiable with")
  (test-engine-property-error-error "check-property遇到如下錯誤~n:: ~a")
  
  (signature-enable-checks "啟用Signature Checks")
  (signature-disable-checks "停用Signature Checks")
  
  ; section header
  (test-engine-check-failures "Check失敗：")
  ; section header
  (test-engine-signature-violations "Signature違規：")
  
  ; part of one phrase "signature <at line ...> to blame: procedure <...>
  (test-engine-signature "signature")
  (test-engine-to-blame "to blame: procedure")
  
  (test-engine-no-signature-violations "無signature違規。")
  (test-engine-1-signature-violation "1個signature違規。")
  (test-engine-n-signature-violations "~a個signature違規。")
  
  ; as in got <value>, signature <at ...>
  (test-engine-got "實得")
  
  ;; The Test Suite Tool
  ;; Errors
  (test-case-empty-error "空白test")
  (test-case-too-many-expressions-error "一個test中包含過多表達式。")
  ;; DrRacket window menu items
  (test-case-insert "插入Test Case")
  (test-case-disable-all "禁用所有Test Cases")
  (test-case-enable-all "允許所有Test Cases")
  
  ;; NOTE: The following string constants are labels of the test-case fields. The width
  ;;       of the field is determined by the length of the longest of the following three words.
  ;;       if the words are too long the test case will take up too much horizontal room and
  ;;       not look very good.
  ;; This string is the label of the expression that is being tested in a test case.
  (test-case-to-test "Test")
  ;; This string is the label of the expression that is the expected value of the to-test expression.
  (test-case-expected "應該為")
  ;; This string is the label of the actual result of the to test expression.
  (test-case-actual "實際值")
  (test-case-predicate "預測值")
  (test-case-should-raise "應該Raise")
  ;; The label of a field of the test-case that describes the expected error message of a test case
  (test-case-error-message "錯誤信息")
  
  (test-case-menu-title "Test Case")
  (test-case-switch-to-error-box "切換為Error Test框")
  (test-case-switch-to-nonerror-box "切換為Nonerror Test框")
  (test-case-collapse "摺疊Test Case")
  (test-case-show-actual "顯示實際值")
  (test-case-enable "啟用Test Case")
  (test-case-show-predicate "顯示預測值")
  (test-case-show-error-message "顯示錯誤信息")
  (test-case-convert-to-text "轉化為文本")
  
  ;; Slideshow
  (slideshow-hide-picts "顯示嵌套的框")
  (slideshow-show-picts "顯示圖片")
  (slideshow-cannot-show-picts "無法顯示圖片；請先運行程序建立大小")
  (slideshow-insert-pict-box "插入圖片框")
  
  ;; GUI Tool
  (gui-tool-heading "GUI工具")
  (gui-tool-before-clicking-message "在點擊工具圖標之前，請先使用「特殊符號」菜單中的「插入GUI」命令插入一個GUI根對象，或先選中另一個GUI。")
  (gui-tool-show-gui-toolbar "顯示GUI工具欄")
  (gui-tool-hide-gui-toolbar "隱藏GUI工具欄")
  (gui-tool-insert-gui "插入GUI")
  
  ;; contract violation tracking
  
  ; tooltip for new planet icon in drscheme window (must have a planet violation logged to see it)
  (show-planet-contract-violations "顯示PLaneT中的contract違背")
  
  ; buttons in the dialog that lists the recorded bug reports
  (bug-track-report "發送報告")
  (bug-track-forget "取消")
  (bug-track-forget-all "取消全部")
  
  ;; planet status messages in the bottom of the drscheme window; the ~a is filled with the name of the package
  (planet-downloading "PLaneT：正在下載~a……")
  (planet-installing "PLaneT：正在安裝~a……")
  (planet-finished "PLaneT：~a已完成。")
  (planet-docs-building "PLaneT：構建文檔（由~a觸發）……")
  (planet-no-status "PLaneT") ;; this can happen when there is status shown in a different and then the user switches to a tab where planet hasn't been used
  
  (bug-report-field-pkg "Package系統信息")
  
  ;; string normalization. To see this, paste some text with a ligature into DrRacket
  ;; the first three strings are in the dialog that appears. The last one is in the preferences dialog
  (normalize "標準化")
  (leave-alone "保留原樣")
  (normalize-string-info "粘帖來的文字包含未經標準化的連字。標準化？")
  (normalize-string-preference "標準化粘帖字元串")
  (ask-about-normalizing-strings "詢問是否標準化字元")
  
  (always-use-platform-specific-linefeed-convention "使用系統相關的換行符約定")
  
  ;; optimization coach
  (hide-optimization-coach "隱藏優化教練")
  (show-optimization-coach "顯示優化教練")
  
  ;; labels used (in a big font) in the background of the definitions and interactions windows
  (definitions-window-label "定義")
  (interactions-window-label "交互")
  (hide-defs/ints-label "隱藏定義/交互標籤") ;; popup menu
  (show-defs/ints-label "顯示定義/交互標籤") ;; preferences checkbox
  
  ;; menu item in the 'edit' menu; applies to editors with programs in them
  ;; (technically, editors that implement color:text<%>)
  (spell-check-string-constants "對字元串常量進行拼寫檢查")
  (spelling-dictionaries "拼寫檢查字典") ; (sub)menu whose items are the different possible dictionaries
  (default-spelling-dictionary "默認字典") ; first item in menu from previous line
  (misspelled-text-color "拼寫錯誤文本的顏色") ;; in the preferences dialog  
  (cannot-find-ispell-or-aspell-path "找不到aspell或ispell程式文件")
  ; puts the path to the spell program in the ~a and then the error message
  ; is put following this string (with a blank line in between)
  (spell-program-wrote-to-stderr-on-startup "拼寫程序（~a）給出錯誤信息：")
  )
