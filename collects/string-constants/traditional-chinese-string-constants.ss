;                                                              
;           #                    #                             
;       #   ##             ##    #     #       #       #       
;       #   ##             #     #     #       #   ##  #   ##  
;      ##   ##    ##       ###### ######       #   #   #   #   
;      # #########               #             ##  #   #   #   
;     #     ##            #      #      #    # # # #### ####   
;    #      ##            ####### #######    # #          #    
;   #################    #           #      ## #    ###### #   
;          ####             #########          #           #   
;         #### #                        #      #           #   
;        ## ## ##        ###############       #    ########   
;       ##  ##  ##           ##  #             #    #          
;      #    ##   ###        ##   #  ##         #    #       #  
;     #     ##     ##      #     #    ##       #    #       #  
;   ##      ##           ##   ####     ##      #    #       ## 
;           #                   #                    #######   
;                                                              
(module traditional-chinese-string-constants "string-constant-lang.ss"
  (is-this-your-native-language "你的母語是繁体中文嗎？")
  
  (are-you-sure-you-want-to-switch-languages
    "爲了改變界面語言，現在需要重新啓動DrScheme。你確定嗎？")
  
  (interact-with-drscheme-in-language "使用繁体中文作爲DrScheme界面語言")
  
  ;; these two should probably be the same in all languages excepet English.
  ;; they are the button labels (under macos and windows, respectively)
  ;; that go the with the string above.
  (accept-and-quit "接受並退出")
  (accept-and-exit "接受並退出")
  
  ;;; general purpose (DrScheme is hereby a word in every language, by decree of Robby :)
  (plt "PLT")
  (drscheme "DrScheme")
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
  (stop "停止")   
  (&stop "停止(&S)") ;; for use in button and menu item labels, with short cut.
  (are-you-sure-delete? "確定要刪除~a嗎？") ;; ~a is a filename or directory name
  (ignore "忽略")
  (revert "復原")
  
  ;; label for a generic check box, often supported on dialogs
  ;; that ask a binary choice of the user. If checked, the
  ;; dialog isn't going to be shown again.
  ;; One version for always using the current choice:
  (dont-ask-again-always-current "不再詢問(總是使用當前設置)")
  ;; One generic version (ie, on the Quit DrScheme dialog)
  (dont-ask-again                "不再詢問")
  
  ;;; important urls
  (web-materials "相關網站") ;; menu item title
  (tool-web-sites "Tools網站")   ;; menu item title
  (drscheme-homepage "DrScheme")
  (plt-homepage "PLT")
  (how-to-use-scheme "How to Use Scheme") ;; title of a book.
  (teachscheme!-homepage "TeachScheme!") ;; probably this should be a `word' in all languages
  
  ;;; bug report form
  (cancel-bug-report? "取消故障報告？")
  (are-you-sure-cancel-bug-report?
    "你確定要取消報告故障嗎？")
  (bug-report-form "故障報告表")
  (bug-report-field-name "姓名")
  (bug-report-field-email "電子郵件")
  (bug-report-field-summary "標題")
  (bug-report-field-severity "嚴重度")
  (bug-report-field-class "類別")
  (bug-report-field-description "詳細描述")
  (bug-report-field-reproduce1 "再現故障")
  (bug-report-field-reproduce2 "的步驟")
  (bug-report-field-environment "環境")
  (bug-report-field-docs-installed "已安裝文檔")
  (bug-report-field-collections "Collections")
  (bug-report-field-human-language "自然語言")
  (bug-report-field-version "版本")
  (bug-report-synthesized-information "綜合信息")  ;; dialog title
  (bug-report-show-synthesized-info "顯示綜合信息")
  (bug-report- "提交")
  (bug-report--menu-item "提交故障報告") ;; in Help Menu (drs & help desk)
  (error-sending-bug-report "故障報告傳輸出錯")
  (error-sending-bug-report-expln "在傳輸故障報告的過程中出現了錯誤。如果你能夠正常瀏覽網絡，請訪問：\n\n    http://bugs.plt-scheme.org/\n\n使用網頁上的表單提交錯誤報告。對於由此產生的不便，我們表示抱歉。\n\n傳輸錯誤詳情：\n~a")
  (illegal-bug-report "非法的故障報告")
  (pls-fill-in-field "請填寫\"~a\"欄目")
  (malformed-email-address "電子郵件地址不符合格式")
  (pls-fill-in-either-description-or-reproduce "在“詳細描述”和“再現故障的步驟”兩欄中，請至少填寫一項。")
  
  ;;; check syntax
  (check-syntax "檢查語法")
  (cs-italic "斜體")
  (cs-bold "黑體")
  (cs-underline "下劃線")
  (cs-change-color "改變顏色")
  (cs-tack/untack-arrow "附加/取消 箭頭")
  (cs-jump-to-next-bound-occurrence "下一個被綁定出現")
  (cs-jump-to-binding "綁定出現")
  (cs-jump-to-definition "定義")
  (cs-error-message "出錯信息")
  (cs-open-file "打開~a")
  (cs-rename-var "重命名~a")
  (cs-rename-id "重命名標識符")
  (cs-rename-var-to "將~a重命名為：")
  (cs-name-duplication-error "你所選擇的新名稱~s與當前轄域內現有標識符相同。")
  (cs-rename-anyway "強制重命名")
  (cs-status-init "語法檢查：為用戶代碼初始化環境")
  (cs-status-coloring-program "語法檢查：用顏色標注表達式")
  (cs-status--compile-time "語法檢查：編譯時")
  (cs-status-expanding-expression "語法檢查：擴展表達式")
  (cs-mouse-over-import "綁定~s為由~s導入")
  
  (cs-lexical-variable "詞匯變量")
  (cs-imported-variable "導入變量")
  
  ;;; info bar at botttom of drscheme frame
  (collect-button-label "垃圾收集")
  (read-only "只讀")
  (read/write "讀/寫")
  (auto-extend-selection "自動擴展")
  (overwrite "覆蓋")
  (running "運行中")
  (not-running "停止中")
  
  ;;; misc
  (welcome-to-something "歡迎來到~a")
  
  ; this appears in the drscheme about box.
  (welcome-to-drscheme-version/language "歡迎使用DrScheme，版本~a，~a")
  
  ; these appear on subsequent lines in the `Help|Welcome to DrScheme' dialog.
  (welcome-to-drscheme "歡迎使用DrScheme")
  
  (goto-line "跳至...行")
  (goto-line-invalid-number
    "~a不是合法的行號。必須提供一個在1和~a之間的數字")
  (goto-position "跳至...位置")
  (no-full-name-since-not-saved
    "當前文檔還沒有被命名，因為從來沒有對它進行保存。")
  (cannot-open-because-dne "無法打開~a，文檔不存在。")
  (interactions-out-of-sync
    "警告：交互窗口和定義窗口不同步。請單擊“運行”按鈕。")
  (file-is-not-saved "文檔\"~a\"還沒有保存過")
  (save "保存")
  (close-anyway "強制關閉")
  (clear-anyway "強制清空")
  
  ;; menu item title
  (log-definitions-and-interactions "記錄定義和交互的日至...")
  (stop-logging "不再記錄日至")
  (please-choose-a-log-directory "請選擇日志目錄")
  (logging-to "記錄日至到：")
  (erase-log-directory-contents "刪除日至目錄~a中的內容？")
  (error-erasing-log-directory "刪除日至出錯。\n\n~a\n")
  
  ;; modes
  (mode-submenu-label "模式")
  (scheme-mode "Scheme模式")
  (text-mode "文本模式")
  
  (scheme-mode-color-symbol "符號")
  (scheme-mode-color-keyword "關鍵字")
  (scheme-mode-color-comment "注釋")
  (scheme-mode-color-string "字符串")
  (scheme-mode-color-constant "常量")
  (scheme-mode-color-parenthesis "括號")
  (scheme-mode-color-error "錯誤")
  (scheme-mode-color-other "其他")
  ;; the ~a is filled in with one of the above (scheme-mode-*)
  (syntax-coloring-choose-color "為~a選擇顏色")
  (preferences-colors "顏色") ;; used in the preferences dialog
  
  (url: "URL:")
  (open-url... "打開URL...")
  (open-url "打開URL")
  (browse... "瀏覽...")
  (bad-url "錯誤的URL")
  (bad-url:this "錯誤的URL: ~a")
  
  ;; Help Desk
  (help "幫助")
  (help-desk "Help Desk")
  (plt:hd:search "搜索")
  (plt:hd:feeling-lucky "手氣不錯")
  (plt:hd:home "Help Desk首頁") 
  ; next 3 are popup menu choices in help desk search frame
  (plt:hd:search-for-keyword "關鍵字")
  (plt:hd:search-for-keyword-or-index "關鍵字或索引")
  (plt:hd:search-for-keyword-or-index-or-text "關鍵字、索引或普通文本")
  (plt:hd:exact-match "精確匹配")
  (plt:hd:containing-match "包含")
  (plt:hd:regexp-match "正則表達式匹配")
  (plt:hd:find-docs-for "搜索：")
  (plt:hd:search-stopped-too-many-matches "[搜索中斷：過多的匹配結果]")
  (plt:hd:nothing-found-for "找不到任何關於~a的信息")
  (plt:hd:and "並且")
  (plt:hd:refresh "更新")
  (plt:hd:refresh-all-manuals "更新所有手冊")
  (plt:hd:manual-installed-date "(~a已安裝)")
  ; Help Desk configuration
  ;; refreshing manuals
  (plt:hd:refreshing-manuals "重新下載手冊")
  (plt:hd:refresh-downloading... "正在下載~a...")
  (plt:hd:refresh-deleting... "刪除舊版本的~a...")
  (plt:hd:refresh-installing... "安裝新版本的~a...")
  (plt:hd:refresh-clearing-indicies "清除緩存中的索引")
  (plt:hd:refreshing-manuals-finished "完成。")
  (plt:hd:about-help-desk "關於Help Desk")
  (plt:hd:help-desk-about-string
    "Help Desk是PLT軟件的信息來源，其中包含了DrScheme，MzScheme和MrEd的全部信息。\n\n版本~a\n版權所有(c)1995-2006 PLT")
  (plt:hd:help-on-help "關於幫助的幫助")
  (plt:hd:help-on-help-details "如果你需要使用Help Desk的幫助，請在Help Desk的主頁中點擊鏈接“How to use Help Desk”。（要進入Help Desk的主頁，請單擊Help Desk窗口上方的“主頁”按鈕。）")
  (reload "刷新") ;; refresh the page in a web browser
  (plt:hd:ask-about-separate-browser
    "你選擇了一個指向萬維網的鏈接。請問您是要在Help Desk中打開該頁面，還是想使用瀏覽器程式瀏覽網頁？")
  (plt:hd:homebrew-browser "Help Desk") ;; choice for the above string (in a button)
  (plt:hd:separate-browser "網絡瀏覽器") ;; other choice for the above string (also in a button)
  (plt:hd:external-link-in-help "在Help中的外部URL")
  (plt:hd:use-homebrew-browser "對於外部URL，使用Help Desk瀏覽")
  (plt:hd:new-help-desk "新的Help Desk窗口")
  
  ;; in the Help Desk language dialog, title on the right.
  (plt:hd:manual-search-ordering "搜索手冊的順序")
  
  
  ;; Help desk htty proxy
  (http-proxy "HTTP代理")
  (proxy-direct-connection "直接連接")
  (proxy-use-proxy "使用代理服務器：")
  (proxy-host "地址")
  (proxy-port "端口")
  (proxy-bad-host "非法的代理服務器")
  
  ;; browser
  (rewind-in-browser-history "後退")
  (forward-in-browser-history "前進")
  (home "主頁")
  (browser "瀏覽器")
  (external-browser-choice-title "外部瀏覽器") ; title for radio-button set
  (browser-command-line-label "命令行：") ; label for radio button that is followed by text boxes
  (choose-browser "選擇瀏覽器")
  (no-browser "以後再詢問")
  (browser-cmdline-expl-line-1 "(命令行由pre-text，URL和post-text連接而成，") ; explanatory text for dialog, line 1
  (browser-cmdline-expl-line-2 "中間不含任何空格)") ; ... line 2. (Anyone need more lines?)
  (install? "安裝？")  ;; if a .plt file is found (title of dialog)
  (you-have-selected-an-installable-package "你選擇了一個可以安裝的軟件包。")
  (do-you-want-to-install-it? "是否安裝？")
  (paren-file-size "(該文檔的長度是~a字節)")
  (download-and-install "下載並安裝") ;; button label
  (download "下載") ;; button label
  (save-downloaded-file/size "下載文檔(~a字節)並保存為") ;; label for get-file dialog
  (save-downloaded-file "下載文檔並保存為")  ;; label for get-file dialog
  (downloading "下載中") ;; dialog title
  (downloading-file... "下載文檔中...")
  (package-was-installed "安裝已完成。")
  (download-was-saved "文檔已保存。")
  
  (install-plt-file-menu-item... "安裝.plt文檔...")
  (install-plt-file-dialog-title "安裝.plt文檔")
  (install-plt-web-tab "網絡文檔")
  (install-plt-file-tab "本地文檔")
  (install-plt-filename "文檔名：")
  (install-plt-url "URL:")
  
  ;; install plt file when opened in drscheme strings
  (install-plt-file "安裝~a，還是打開以供編輯？")
  (install-plt-file/yes "安裝")
  (install-plt-file/no "編輯")
  
  (plt-installer-progress-window-title "安裝程式") ;; frame title
  (plt-installer-abort-installation "取消安裝") ;; button label
  (plt-installer-aborted "安裝中止。") ;; msg that appears in the installation window when installation is aborted
  
  ;;; about box
  (about-drscheme-frame-title "關於DrScheme")
  (take-a-tour "教程")
  (release-notes "發行記錄")
  
  
  ;;; save file in particular format prompting.
  (save-as-plain-text "保存本文檔為純文本？")
  (save-in-drs-format "保存本文檔為drscheme(非純文本)格式？")
  (yes "是")
  (no "否")
  
  ;;; preferences
  (preferences "參數設置")
  (error-saving-preferences "保存參數時出錯:~a")
  (error-reading-preferences "讀取參數設置時出錯")
  (scheme-prefs-panel-label "Scheme")
  (warnings-prefs-panel-label "警告")
  (editor-prefs-panel-label "編輯")
  (general-prefs-panel-label "常規")
  (highlight-parens "加亮顯示匹配的括號")
  (fixup-parens "自動修改括號類型以保持匹配")
  (flash-paren-match "高亮顯示括號匹配")
  (auto-save-files "自動保存文檔")
  (backup-files "保存備份文檔")
  (map-delete-to-backspace "將delete轉換成backspace")
  (verify-exit "退出時確認")
  (ask-before-changing-format "改變保存方式時確認")
  (wrap-words-in-editor-buffers "在編輯器緩存中自動換行")
  (show-status-line "顯示狀態行")
  (count-columns-from-one "從一開始計算行號")
  (display-line-numbers "在緩沖區中顯示行號和列號")
  (enable-keybindings-in-menus "允許使用菜單中的快捷鍵")
  (automatically-to-ps "自動打印成postscript文檔")
  (option-as-meta "將option鍵當作meta") ;; macos/macos x only
  (separate-dialog-for-searching "使用單獨的搜索對話框")
  (reuse-existing-frames "在打開新文檔時，使用現有的框架")
  (default-fonts "默認字體")
  (paren-match-color "高亮顯示括號所使用的顏色") ; in prefs dialog
  (online-coloring-active "實時根據語法用顏色標記程式")
  (open-files-in-tabs "在不同的標簽下打開多個文檔（不使用多個窗口）")
  (show-interactions-on-ute "在運行程式時自動打開交互窗口")
  (limit-interactions-size "限制交互窗口的大小")
  (background-color "背景顏色")
  (default-text-color "默認顏色") ;; used for configuring colors, but doesn't need the word "color"
  (choose-a-background-color "請選擇背景顏色")
  
  ; title of the color choosing dialog
  
  ; should have entire alphabet
  (font-example-string "簡體中文 by 朱崇愷") 
  
  (change-font-button-label "更改")
  (fonts "字體")
  
  ; filled with type of font, eg modern, swiss, etc.
  (choose-a-new-font "請選擇一種新的“~a”字體")
  
  (font-size-slider-label "字號")
  (restart-to-see-font-changes "重新啟動，使修改生效")
  
  (font-prefs-panel-title "字體")
  (font-name "字體")
  (font-size "字號")
  (set-font "設置字體...")
  (font-smoothing-label  "字體平滑度設置")
  (font-smoothing-none "無")
  (font-smoothing-some "部分")
  (font-smoothing-all "全部")
  (font-smoothing-default "使用系統默認")
  (select-font-name "選擇字體")
  (example-text "示例文字")
  (only-warn-once "當定義窗口和交互窗口不同步時，僅警告一次")
  
  ; warning message when lockfile is around
  (waiting-for-pref-lock "等待參數設置文檔解鎖...")
  (pref-lock-not-gone
   "參數設置封鎖文檔：\n\n ~a\n\n禁止保存參數設置。請確定沒有其他PLT軟件正在運行中，然後刪除該封鎖文檔。")
  (still-locked-exit-anyway? "參數無法保存。仍然退出？")
  
  ;;; indenting preferences panel
  (indenting-prefs-panel-label "縮進")
  (indenting-prefs-extra-regexp "其他表達式")
  
  ; filled with define, lambda, or begin
  (enter-new-keyword "請輸入一個類似於~a的關鍵字：")
  (x-keyword "~a關鍵字")
  (x-like-keywords "~a類型的關鍵字")
  
  (expected-a-symbol "需要一個符號，得到a")
  (already-used-keyword "“~a”已經是縮進關鍵字了")
  (add-keyword "添加")
  (remove-keyword "刪除")
  
  ;;; find/replace
  (find-and-replace "查找並替換")
  (find "查找")
  (replace "替換")
  (dock "面板")
  (undock "對話框")
  (replace&find-again "替換並查找下一個") ;;; need double & to get a single &
  (replace-to-end "全部替換")
  (forward "下一個")
  (backward "上一個")
  (hide "隱藏")
  
  ;;; multi-file-search
  (mfs-multi-file-search-menu-item "在文檔中搜索...")
  (mfs-string-match/graphics "字符串匹配(可用與包含圖像的文檔)")
  (mfs-regexp-match/no-graphics "正則表達式匹配(只適用於純文本文檔)")
  (mfs-searching... "搜索...")
  (mfs-configure-search "搜索設置") ;; dialog title
  (mfs-files-section "文檔")   ;; section in config dialog
  (mfs-search-section "搜索") ;; section in config dialog
  (mfs-dir "目錄")
  (mfs-recur-over-subdirectories "包含子目錄")
  (mfs-regexp-filename-filter "文檔名篩選(正則表達式)")
  (mfs-search-string "查找字符串")
  (mfs-drscheme-multi-file-search "DrScheme──多文檔查找") ;; results window and error message title
  (mfs-not-a-dir "\"~a\"不是目錄")
  (mfs-open-file "打開文檔")
  (mfs-stop-search "停止搜索")
  (mfs-case-sensitive-label "大小寫敏感")
  (mfs-no-matches-found "沒有找到匹配結果。")
  (mfs-search-interrupted "搜索中止。")
  
  ;;; reverting a file
  (are-you-sure-revert
    "你確定要恢復這個文檔嗎？這一操作無法撤銷。")
  (are-you-sure-revert-title
    "恢復？")
  
  ;;; saving a file
  ; ~a is filled with the filename
  (error-saving "無法保存") ;; title of error message dialog
  (error-saving-file/name "在保存文檔~a時出現錯誤。")
  (error-loading "無法讀取")
  (error-loading-file/name "在讀取~a時出現錯誤.")
  (unknown-filename "<<未知>>")
  
  ;;; finder dialog
  (must-specify-a-filename "你必須指定一個文檔名")
  (file-does-not-exist "文檔\"~a\"不存在。")
  (ask-because-file-exists "文檔\"~a\"已存在。是否替換？")
  (dne-or-cycle "文檔\"~a\"中包含一個不存在的目錄，或者一個循環")
  (get-file "Get file")
  (put-file "Put file")
  (full-pathname "完整路徑")
  (show-dot-files "顯示點號開始文檔/目錄名。")
  (up-directory-button-label "上層目錄")
  (add-button-label "添加") ;;; for multi-file selection
  (add-all-button-label "全部添加") ;;; for multi-file selection
  (remove-button-label "移除") ;;; for multi-file selection
  (file-wrong-form "該文檔名格式不正確")
  (select-files "選擇多個文檔")
  (select-file "選擇單個文檔")
  (dir-dne "該目錄不存在。")
  (file-dne "該文檔不存在。")
  (empty-filename "文檔名中必須包含文字。")
  (that-is-dir-name "這是一個目錄的名字。")
  
  ;;; raw menu names -- these must match the 
  ;;; versions below, once the &s have been stripped.
  ;;; if they don't, DrScheme's menus will appear
  ;;; in the wrong order.
  (file-menu "文檔")
  (edit-menu "編輯")
  (help-menu "幫助")
  (windows-menu "窗口")
  
  ;;; menus
  ;;; - in menu labels, the & indicates a alt-key based shortcut.
  ;;; - sometimes, things are stuck in the middle of 
  ;;; menu item labels. For instance, in the case of
  ;;; the "Save As" menu, you might see: "Save Definitions As". 
  ;;; be careful of spacing, follow the English, if possible.
  ;;; - the ellipses in the `after' strings indicates that
  ;;; more information is required from the user before completing
  ;;; the command.
  
  (file-menu-label "文檔(&F)")
  
  (new-info  "新建文檔")
  (new-menu-item "新建(&N)")
  (new-...-menu-item "新建(&N)...")
  
  (open-info "打開現有文檔")
  (open-menu-item "打開(&O)...")
  (open-here-menu-item "從這裡打開(&O)...")
  
  (open-recent-info "最近使用過文檔的列表")
  (open-recent-menu-item "最近使用過的文檔")
  
  (revert-info "將當前文檔恢復為磁盤上的副本")
  (revert-menu-item "恢復(&R)")
  
  (save-info "保存當前文檔")
  (save-menu-item "保存(&S)")
  
  (save-as-info "輸入新的文檔名,保存當前文檔")
  (save-as-menu-item "另存為(&A)...")
  
  (print-info "打印當前文檔")
  (print-menu-item "打印(&P)...")
  
  (close-info "關閉當前文檔")
  (close-menu-item "關閉(&C)")
  
  (quit-info "關閉所有窗口")
  (quit-menu-item-windows "退出(&X)")
  (quit-menu-item-others "退出(&Q)")
  
  (edit-menu-label "編輯(&E)")
  
  (undo-info "撤銷最近的操作")
  (undo-menu-item "撤銷(&U)")
  
  (redo-info "取消最近的撤銷操作")
  (redo-menu-item "重復(&R)")
  
  (cut-info "將當前選中的對象移入剪貼版")
  (cut-menu-item "剪切(&T)")
  
  (copy-info "將當前選中的對象復制到剪貼版")
  (copy-menu-item "復制(&C)")
  
  (paste-info "將剪貼版中個內容復制到當前位置")
  (paste-menu-item "粘貼(&P)")
  
  (clear-info "刪除當前選中的對象")
  (clear-menu-item-windows "刪除(&D)")
  
  (select-all-info "選中整個文檔")
  (select-all-menu-item "全選(&L)")
  
  (find-info "搜索某個字符串")
  (find-menu-item "查找...")
  
  (find-again-info "繼續搜索該字符串")
  (find-again-menu-item "查找下一個")
  
  (replace-and-find-again-info "替換當前文本，然後繼續查找原字符串")
  (replace-and-find-again-menu-item "替換並查找下一個")
  
  (preferences-info "設置控制參數")
  (preferences-menu-item "參數設置...")
  
  (keybindings-info "顯示當前熱鍵綁定")
  (keybindings-menu-item "熱鍵綁定")
  (keybindings-show-active "顯示熱鍵綁定")
  (keybindings-frame-title "熱鍵綁定")
  (keybindings-sort-by-name "按名稱排序")
  (keybindings-sort-by-key "按鍵名排序")
  (keybindings-add-user-defined-keybindings "添加自定義熱鍵綁定...")
  (keybindings-menu-remove "取消~a")
  (keybindings-choose-user-defined-file "請選擇一個包含熱鍵綁定的文檔")
  
  (user-defined-keybinding-error "熱鍵綁定出錯~a\n\n~a")
  (user-defined-keybinding-malformed-file "文檔~a並不是一個按照(lib \"keybinding-lang.ss\" \"framework\")語言編寫的module.")  
  
  ;; menu items in the "special" menu
  (insert-text-box-item "插入文本框")
  (insert-image-item "插入圖片...")
  (insert-comment-box-menu-item-label "插入注釋框")
  (insert-lambda "插入λ")
  
  (wrap-text-item "自動換行")
  
  (windows-menu-label "窗口(&W)")
  (bring-frame-to-front "前端顯示")       ;;; title of dialog
  (bring-frame-to-front... "前端顯示...") ;;; corresponding title of menu item
  (most-recent-window "最近的窗口")
  
  (view-menu-label "視圖(&V)")
  (show-overview "顯示程式輪廓") 
  (hide-overview "隱藏程式輪廓")
  (show-module-browser "顯示module瀏覽器")
  (hide-module-browser "隱藏module瀏覽器")
  
  (help-menu-label "幫助(&H)")
  (about-info "本程式的詳細信息以及致謝名單")
  (about-menu-item "關於...")
  
  ;; open here's new menu item
  (create-new-window-or-clear-current
    "您是想打開一個新窗口，還是清空當前窗口？")
  (clear-current "清空當前")
  (new-window "新窗口")
  
  ;;; exiting and quitting ``are you sure'' dialog
  ;;; exit is used on windows, quit on macos, in English. Other
  ;;; languages probably use the same word on both platforms.
  (exit "退出")
  (quit "退出")
  (are-you-sure-exit "你確定要退出嗎?")
  (are-you-sure-quit "你確定要退出嗎?")
  
  ;;; autosaving
  (error-autosaving "自動保存為\"~a\"時出錯。") ;; ~a will be a filename
  (autosaving-turned-off "在一個文檔沒有被手工保存之間，自動保存也不會進行")
  (recover-autosave-files-frame-title "從自動保存中恢復")
  (autosave-details "詳細情況")
  (autosave-recover "恢復")
  (autosave-unknown-filename "<<未知>>")
  
  ;; these are labels in a dialog that drscheme displays
  ;; if you have leftover autosave files. to see the dialog,
  ;; start up drscheme and modify (but don't save) a file
  ;; (also, do this with an unsaved file). Wait for the autosave
  ;; files to appear (typically 5 minutes). Kill DrScheme
  ;; and restart it. You'll see the dialog
  (autosave-autosave-label: "自動保存文檔：")
  (autosave-original-label: "原始文檔：")
  (autosave-autosave-label "自動保存文檔")
  (autosave-original-label "原始文檔")
  (autosave-compare-files "比較自動保存文檔")
  
  (autosave-show-autosave "自動保存文檔") ;; title of a window showing the autosave file
  
  (autosave-explanation "DrScheme發現了自動保存的文檔，其中可能包含你沒有保存過的程式")
  
  (autosave-recovered! "已恢復！") ;; status of an autosave file
  (autosave-deleted "已刪除")       ;; status of an autosave file
  
  (autosave-error-deleting "刪除~a出錯\n\n~a") ;; first is a filename, second is an error message from mz.
  (autosave-delete-button "刪除")
  (autosave-delete-title "刪除")  ;; title of a dialog asking for deletion confirmation
  (autosave-done "完成")
  
  ;; appears in the file dialog
  (autosave-restore-to-where? "請選擇自動保存文檔的位置")
  
  
  ;;; file modified warning
  (file-has-been-modified
    "要保存你所作的更改嗎？")
  (overwrite-file-button-label "保存")
  
  (definitions-modified 
    "當前磁盤文檔已被修改；請保存或恢復文檔。")
  (drscheme-internal-error "DrScheme內部錯誤")
  
  ;;; tools
  (invalid-tool-spec "Collection ~a中info.ss的tool定義不正確。需要一個字符串或者一個非空表，得到：~e")
  (error-invoking-tool-title "調用tool ~s出錯；~s")
  (tool-tool-names-same-length
    "在~s的info.ss文檔中，“tool-names”和“tools”應該是等長的表，得到~e和~e")
  (tool-tool-icons-same-length
    "在~s的info.ss文檔中，“tool-icons”和“tools”應該是等長的表，得到~e和~e")
  (tool-tool-urls-same-length
    "在~s的info.ss文檔中，“tool-urls”和“tools”應該是等長的表，得到~e和~e")
  (error-getting-info-tool
   "載入~s的info.ss出錯")
  (tool-error-phase1 "tool ~s第一階段出錯;~s")
  (tool-error-phase2 "tool ~s第二階段出錯;~s")
  
  
  ;;; define popup menu
  (end-of-buffer-define "<<緩沖區結束>>")
  (sort-by-name "按名字排序")
  (sort-by-position "按文檔中的位置排序")
  (no-definitions-found "<<沒有任何定義>>")
  (jump-to-defn "跳至~a的定義")
  
  (recent-items-sort-by-age "按時間排序")
  (recent-items-sort-by-name "按名字排序")
  
  ;;; view menu
  (hide-definitions-menu-item-label "隱藏定義(&D)")
  (show-definitions-menu-item-label "顯示定義(&D)")
  (definitions-menu-item-help-string "顯示/隱藏定義窗口")
  (show-interactions-menu-item-label "顯示交互(&I)")
  (hide-interactions-menu-item-label "隱藏交互(&I)")
  (interactions-menu-item-help-string "顯示/隱藏交互窗口")
  (show-toolbar "顯示工具欄(&T)")
  (hide-toolbar "隱藏工具欄(&T)")
  
  ;;; file menu
  (save-definitions-as "將定義另存為(&A)")
  (save-definitions "保存定義")
  (print-definitions "打印定義...")
  (about-drscheme "關於DrScheme")
  (save-other "其他保存方式")
  (save-definitions-as-text "將定義保存為文本...")
  (save-interactions "保存交互")
  (save-interactions-as "將交互另存為...")
  (save-interactions-as-text "將交互保存為文本...")
  (print-interactions "打印交互...")
  (new-tab "新建標簽")
  (close-tab "關閉標簽") ;; must not have any &s in it.
  
  ;;; edit-menu
  (split-menu-item-label "分屏(&S)")
  (collapse-menu-item-label "合並(&O)")
  
  ;;; language menu
  (language-menu-name "語言(&L)")
  
  ;;; scheme-menu
  (scheme-menu-name "S&cheme")
  (ute-menu-item-label "運行")
  (ute-menu-item-help-string "運行定義窗口中的程式")
  (break-menu-item-label "中斷")
  (break-menu-item-help-string "中斷當前計算")
  (kill-menu-item-label "終止")
  (kill-menu-item-help-string "終止當前計算")
  (clear-error-highlight-menu-item-label "清除錯誤高亮顯示")
  (clear-error-highlight-item-help-string "清除錯誤區域的粉紅色高亮顯示")
  (reindent-menu-item-label "調整縮進(&R)")
  (reindent-all-menu-item-label "全文調整縮進(&A)")
  (semicolon-comment-out-menu-item-label "用分號注釋(&C)")
  (box-comment-out-menu-item-label "用注釋框注釋(&C)")
  (uncomment-menu-item-label "取消注釋(&U)")
  
  (convert-to-semicolon-comment "轉化為分號注釋")
  
  ;;; utables
  (create-utable-menu-item-label "創建可執行程式...")
  (create-utable-title "創建可執行程式")
  (must-save-before-utable "在創建可執行程式之前，你必須保存源程式")
  (save-an-utable "保存為可執行程式")
  (save-a-mred-launcher "保存為MrEd程式")
  (save-a-mzscheme-launcher "保存為MzScheme程式")
  (save-a-mred-stand-alone-utable "保存為MrEd可執行程式")
  (save-a-mzscheme-stand-alone-utable "保存為MzScheme可執行程式")
  
  (definitions-not-saved "當前定義窗口中的程式並沒有被保存過。將使用最近保存過的版本來生成可執行程式。繼續？")
  (inline-saved-program-in-utable?
    "是否將程式嵌入到可執行文檔中？如果選擇是，那麼你可以將可執行文檔復制到其他~a計算機上使用，但是這將會大大增加可執行文檔的大小；否則，那麼你可以得到一個小得多的可執行文檔，但是無法將它復制到其它計算機上使用。此外，如果選擇否，該可執行文檔每次運行時都會自動載入最新保存的程式。")
  (inline-saved-program-in-utable/windows/path
    "注意！生成的程式文檔運行時需要三個DLL庫：libmred.dll、libmzsch.gll和libgc.dll。它們位於\n\n~a\n\n程式文檔執行時會在當前目錄或者PATH環境變量中尋找DLL庫。\n\nDrScheme在安裝時已經將這些DLL所在的目錄放入PATH變量。請不要手工修改這些設置。\n\n如果你要將該可執行文檔復制到其它計算機上，必須同時復制這幾個DLL文檔──你可以將DLL文檔和可執行程式放在同一個目錄下，也可以將DLL文檔放在一個PATH目錄中。")
  (launcher "啟動程式")
  (stand-alone "獨立的")
  (utable-type "類型")
  (utable-base "基")
  (filename "文檔名：")
  (create "創建")
  ;; ! FIXME ! : there are several of "utable"s in this file that should be "executable" !
  ;; "choose-an-executable" changed to "specify-a"
  ;(please-choose-an-utable-filename "請選擇可執行文檔的名稱。")
  ;; Replaced by generic ~a-must-end-with-~a
  ;(windows-utables-must-end-with-exe
  ;  "文檔名\n\n  ~a\n\n不合法。Windows可執行文檔必須以.exe結尾。")
  ;(macosx-utables-must-end-with-app
  ;  "文檔名\n\n  ~a\n\n不合法。MacOS X可執行文檔必須以.app結尾。")
  (warning-directory-will-be-replaced
    "警告：目錄：\n\n  ~a\n\n將會被重置。繼續操作？")
  
  (create-servlet "創建Servlet...")
  
  ; the ~a is a language such as "module" or "algol60"
  (create-servlet-unsupported-language
    "無法為~a語言程式創建Servlet。")
  
  ;;; buttons
  (ute-button-label "運行") 
  (save-button-label "保存")
  (break-button-label "停止")
  
  ;;; search help desk popup menu
  (search-help-desk-for "在Help Desk中搜索“~a”")
  (exact-lucky-search-help-desk-for "在Help Desk中搜索最符合“~a”的一個頁面")
  
  ;; collapse and expand popup menu items
  (collapse-sexp "折疊sexpression")
  (expand-sexp "擴展sexpression")
  
  ;;; fraction dialog
  (enter-fraction "輸入分數")
  (whole-part "整數部分")
  (numerator "分子")
  (denominator "分母")
  (invalid-number "無效的輸入：必須輸入一個精確的、不是整數的實數")
  (insert-fraction-menu-item-label "插入分數...")
  
  ;; number snip popup menu
  (show-decimal-expansion "用十進制表示")
  (show-mixed-fraction-view "用帶分數表示")
  (show-improper-fraction-view "用假分數表示")
  (show-more-decimal-places "先是更多小數位")
  
  ;;; Teachpack messages
  (select-a-teachpack "選擇教學包")
  (clear-teachpack "卸載教學包~a")
  (teachpack-error-label "DrScheme──教學包出錯")
  (teachpack-didnt-load "無法裝載教學包~a。")
  (add-teachpack-menu-item-label "加載教學包...")
  (clear-all-teachpacks-menu-item-label "卸載全部教學包")
  (drscheme-teachpack-message-title "DrScheme教學包")
  (already-added-teachpack "教學包~a已裝載")
  
  ;;; Language dialog
  (introduction-to-language-dialog
    "請選擇語言。大部分入門級的學生都可以使用默認語言。")
  (language-dialog-title "語言選擇")
  (case-sensitive-label "大小寫敏感")
  (output-style-label "輸出格式")
  (constructor-printing-style "構造器")
  (quasiquote-printing-style "Quasiquote")
  (write-printing-style "write")
  (print-printing-style "current-print")
  (sharing-printing-label "Show sharing in values")
  (use-pretty-printer-label "print多個對象時自動換行")
  (input-syntax "輸入語法")
  (dynamic-properties "Dynamic Properties")
  (output-syntax "輸出語法")
  (no-debugging-or-profiling "No debugging or profiling")
  (debugging "Debugging")
  (debugging-and-profiling "Debugging and profiling")
  (test-coverage "Syntactic test suite coverage")
  (show-details-button-label "顯示詳情")
  (hide-details-button-label "隱藏詳情")
  (choose-language-menu-item-label "選擇語言...")
  (revert-to-language-defaults "恢復默認語言設置")
  (fraction-style "分數格式")
  (use-mixed-fractions "帶分數")
  (use-repeating-decimals "循環小數")
  (decimal-notation-for-rationals "使用十進制表示有理數")
  (please-select-a-language "請選擇語言")
  
  
  ;;; languages
  (beginning-student "初級")
  (beginning-one-line-summary "define、cond、結構體、常量和基本操作")
  (beginning-student/abbrev "初級+縮寫的表")
  (beginning/abbrev-one-line-summary "在初級的基礎上，用縮寫形式輸出表")
  (intermediate-student "中級")
  (intermediate-one-line-summary "在初級的基礎上增加詞法作用域")
  (intermediate-student/lambda "中級+lambda")
  (intermediate/lambda-one-line-summary "在中級的基礎上，增加高階函數")
  (advanced-student "高級")
  (advanced-one-line-summary "在中級的基礎上，增加lambda和賦值")
  (how-to-design-programs "程式設計方法/How to Design Programs") ;; should agree with MIT Press on this one...
  (pretty-big-scheme "Pretty Big (包括MrEd和高級)")
  (pretty-big-scheme-one-line-summary "Adds syntax and functions from the HtDP languages")
  (r5rs-lang-name "標準(R5RS)")
  (r5rs-one-line-summary "Scheme語言標準第5修改稿")
  (expander "Expander")
  (expander-one-line-summary "Expands, rather than uates, expressions")
  (professional-languages "正式語言")
  (teaching-languages "教學語言")
  (experimental-languages "實驗語言")
  
  (module-language-one-line-summary "Run creates a REPL in the context of the module, including the module's declared language")
  
  
  ;;; debug language
  (unknown-debug-frame "[unknown]")
  (backtrace-window-title "Backtrace - DrScheme")
  (files-interactions "~a's interactions") ;; filled with a filename
  (current-interactions "interactions")
  (current-definitions "definitions")
  (mzscheme-w/debug "Textual (MzScheme, 包含R5RS)")
  (mzscheme-one-line-summary "PLT的Scheme實現")
  (mred-w/debug "Graphical (MrEd, 包含 MzScheme)")
  (mred-one-line-summary "在MzScheme的基礎上增加GUI支持")
  
  ;;; welcoming message in repl
  (language "語言")
  (custom "自定義")
  (teachpack "教學包")
  (welcome-to "歡迎使用")
  (version "版本")
  
  ;;; kill uation dialog
  (kill-uation? "是否要終止計算？")
  (just-break "中斷")
  (kill "終止")
  (kill? "終止？")
  
  ;;; version checker
  (version:update-menu-item "檢查更新...")
  
  ;; special menu
  (special-menu "特殊符號(&P)")
  
  ;; large semi colon letters
  (insert-large-letters... "插入大字...")
  (large-semicolon-letters "帶分號的大字")
  (text-to-insert "要插入的文字")
  
  (module-browser-filename-format "文檔全名: ~a (共~a行)")
  (module-browser-root-filename "根文檔名: ~a")
  (module-browser-font-size-gauge-label "字號")
  (module-browser-progress-label "Module overview progress")
  (module-browser-adding-file "添加文檔: ~a...")
  (module-browser-laying-out-graph-label "Laying out graph")
  (module-browser-open-file-format "打開~a")
  (module-browser "Module瀏覽器") ;; frame title
  (module-browser... "Module瀏覽器...") ;; menu item title
  (module-browser-error-expanding "Error expanding the program:\n\n~a")
  (module-browser-show-lib-paths "Show files loaded by (lib ..) paths")
  (module-browser-progress "Module瀏覽器：~a") ;; prefix in the status line
  (module-browser-compiling-defns "Module瀏覽器：compiling definitions")
  (module-browser-show-lib-paths/short "Follow lib requires") ;; check box label in show module browser pane in drscheme window.
  (module-browser-refresh "Refresh") ;; button label in show module browser pane in drscheme window.
  (module-browser-only-in-plt-and-module-langs
    "Module瀏覽器只能在PLT語言和module語言(並且要求程式中有module)中使用。")
  (module-browser-name-length "Name length")
  (module-browser-name-short "Short")
  (module-browser-name-medium "Medium")
  (module-browser-name-long "Long")
  (module-browser-open-all "Open all files shown here")

  (mrflow-using-default-language-title "Default Language Used")
  (mrflow-using-default-language "The language currently used does not have a type table defined for its primitives. Using R5RS Scheme instead.")
  (mrflow-button-title "分析")
  ;(mrflow-unknown-style-delta-error-title "Unknown Box Style Delta")
  ;(mrflow-unknown-style-delta-error "Unknown box style delta: ~a")
  (mrflow-popup-menu-show-type "Show Type")
  (mrflow-popup-menu-hide-type "Hide Type")
  (mrflow-popup-menu-show-errors "Show Errors")
  (mrflow-popup-menu-hide-errors "Hide Errors")
  ;(mrflow-read-exception-title "Read Exception")
  ;(mrflow-read-exception "Read exception: ~a")
  ;(mrflow-syntax-exception-title "Syntax Exception")
  ;(mrflow-syntax-exception "Syntax exception: ~a")
  ;(mrflow-unknown-exception-title "Unknown Exception")
  ;(mrflow-unknown-exception "Unknown exception: ~a")
  ;(mrflow-language-primitives-error-title "Language Primitives Error")
  ;(mrflow-language-primitives-error "Wrong filename for language primitives types table: ~a")

  (xml-tool-insert-xml-box "插入XML框")
  (xml-tool-insert-scheme-box "插入Scheme框")
  (xml-tool-insert-scheme-splice-box "插入Scheme接合框")
  (xml-tool-xml-box "XML框")
  (xml-tool-scheme-box "Scheme框")
  (xml-tool-scheme-splice-box "Scheme接合框")
  (xml-tool-switch-to-scheme "轉變成Scheme框")
  (xml-tool-switch-to-scheme-splice "轉變成Scheme接合框")
  (xml-tool-eliminate-whitespace-in-empty-tags
    "Eliminiate whitespace in empty tags")
  (xml-tool-leave-whitespace-alone
    "Leave whitespace alone")
  
  (show-recent-items-window-menu-item "在單獨窗口中顯示最近使用的文檔")
  (show-recent-items-window-label "最近使用的文檔")
  (number-of-open-recent-items "Number of recent items")
  (switch-anyway "Switch File Anyway")
  
  (stepper-program-has-changed "注意：程式已改變。")
  (stepper-program-window-closed "注意：程式窗口已關閉。")
  
  (stepper-home "還原")
  (stepper-name "單步執行器")
  (stepper-language-level-message
    "您選擇的語言是“~a”。目前，stepper只支持“~a”和“~a”之間的語言。")
  (stepper-button-label "單步執行")
  (stepper-previous-application "|< 調用")
  (stepper-previous "< 上一步")
  (stepper-next "下一步 >")
  (stepper-next-application "調用 >|")
  
  
  (dialog-back "後退")
  
  ;; warnings about closing a drscheme frame when the program
  ;; might still be doing something interesting
  (program-is-still-running "定義窗口中的程式還在運行中。強制退出？")
  (program-has-open-windows "定義窗口中的打開了其他窗口。強行關閉這些窗口？")
  
  ;; Profj
  (profj-java "Java")
  (profj-java-mode "Java模式")
  (profj-java-mode-color-keyword "關鍵字")
  (profj-java-mode-color-string "字符串")
  (profj-java-mode-color-literal "文字")
  (profj-java-mode-color-comment "注釋")
  (profj-java-mode-color-error "錯誤")
  (profj-java-mode-color-identifier "標示符")
  (profj-java-mode-color-default "默認值")
  
  (profj-insert-java-comment-box "插入Java注釋框")
  (profj-insert-java-interactions-box "插入Java交互框")
  
  ;; The Test Suite Tool
  ;; Errors
  (test-case-empty-error "Empty test case")
  (test-case-too-many-expressions-error "Too many expressions in a test case.")
  ;; Dr. Scheme window menu items
  (test-case-insert "插入Test Case")
  (test-case-disable-all "禁用所有Test Cases")
  (test-case-enable-all "允許所有Test Cases")
  
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
  (profjBoxes-insert-java-examples "插入Java Examples")
  (profjBoxes-insert-java-interactions "插入Java Interactions")
  
  ;; Slideshow
  (slideshow-show-slideshow-panel "顯示Slideshow面板")
  (slideshow-hide-slideshow-panel "隱藏Slideshow面板")
  (slideshow-freeze-picts "Freeze These Picts")
  (slideshow-thaw-picts "Show Picts Under Mouse")
  (slideshow-hide-picts "Show Nested Boxes")
  (slideshow-show-picts "Show Picts")
  (slideshow-cannot-show-picts "Cannot show picts; run program to cache sizes first")
  (slideshow-insert-pict-box "插入Pict框") 
  
  ;; GUI Tool
  (gui-tool-heading "GUI工具")
  (gui-tool-before-clicking-message "在點擊工具圖標之前，請先使用“特殊符號”菜單中的“插入GUI”命令插入一個GUI根對象，或先選中另一個GUI。")
  (gui-tool-show-gui-toolbar "顯示GUI工具欄")
  (gui-tool-hide-gui-toolbar "隱藏GUI工具欄")
  (gui-tool-insert-gui "插入GUI")
  )
