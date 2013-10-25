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
(module simplified-chinese-string-constants "string-constant-lang.rkt" 
  (is-this-your-native-language "中文是你的母语吗？")
  
  (are-you-sure-you-want-to-switch-languages
   "为了改变界面语言，现在需要重新启动DrRacket。你确定吗？")
  
  (interact-with-drscheme-in-language "使用简体中文作DrRacket界面语言")
  
  ;; these two should probably be the same in all languages excepet English.
  ;; they are the button labels (under macos and windows, respectively)
  ;; that go the with the string above.
  (accept-and-quit "接受并退出")
  (accept-and-exit "接受并退出")
  
  ;;; general purpose (DrRacket is hereby a word in every language, by decree of Robby :)
  (plt "PLT")
  (drscheme "DrRacket")
  (drracket "DrRacket")
  (ok "确定")
  (cancel "取消")
  (abort "中止")
  (untitled "未命名")
  (untitled-n "未命名~a")
  (warning "警告")
  (error "错误")
  (close "关闭") ;; as in, close an open window. must match close-menu-item
  ;; in the sense that, when the &s have been stripped from
  ;; close-menu-item, it must be the same string as this.
  (close-window "关闭视窗")
  (stop "停止")
  (&stop "停止(&S)") ;; for use in button and menu item labels, with short cut.
  (are-you-sure-delete? "确定要删除~a吗？") ;; ~a is a filename or directory name
  (are-you-sure-replace? "确定要替换~a吗？") ;; ~a is a filename or directory name
  (ignore "忽略")
  (revert "还原")
  
  ;; label for a generic check box, often supported on dialogs
  ;; that ask a binary choice of the user. If checked, the
  ;; dialog isn't going to be shown again.
  ;; One version for always using the current choice:
  (dont-ask-again-always-current "不再询问（总是使用当前设置）")
  ;; One generic version (ie, on the Quit DrRacket dialog)
  (dont-ask-again "不再询问")
  
  ;;; important urls
  (web-materials "相关网站") ;; menu item title
  (tool-web-sites "工具网站")   ;; menu item title
  (plt-homepage "Racket")
  (pbd-homepage "Program by Design")
  
  ;;; bug report form
  (cancel-bug-report? "取消程序错误报告？")
  (are-you-sure-cancel-bug-report?
   "你确定要取消报告程序错误吗？")
  (do-you-want-to-discard-or-save-this-bug-report
   "是保存还是丢弃这份程序错误报告?")
  (discard "丢弃") ;; a button label for a dialog box with the above question
  (bug-report-form "程序错误报告表")
  (bug-report-field-name "姓名")
  (bug-report-field-email "电子邮件")
  (bug-report-field-summary "标题")
  (bug-report-field-severity "严重度")
  (bug-report-field-class "类别")
  (bug-report-field-description "详细描述")
  (bug-report-field-reproduce1 "再现程序错")
  (bug-report-field-reproduce2 "误的步骤")
  (bug-report-field-environment "环境")
  (bug-report-field-docs-installed "已安装文档")
  (bug-report-field-collections "Collections")
  (bug-report-field-human-language "自然语言")
  (bug-report-field-memory-use "内存使用")
  (bug-report-field-version "版本")
  (bug-report-synthesized-information "综合信息")  ;; dialog title
  (bug-report-show-synthesized-info "显示综合信息")
  (bug-report-submit "提交")
  (close-and-save-bug-report "保存并关闭(&&)") ;; button in bug report dialog, next to cancel and bug-report-submit
  (bug-report-submit-menu-item "提交程序错误报告…");; same as above, but used when there are saved bug reports
  (saved-bug-reports-menu-item "保存程序错误报告") ;; in Help Menu, submenu title
  (disacard-all-saved-bug-reports "丢弃全部程序错误报告") ;; menu item: only shows up when there is more than one saved bug report
  (no-saved-bug-reports "不存在程序错误报告") ;; an info message that shows up as a disabled menu item when no saved bug reports are around
  (new-bug-report "新程序错误报告") ;; button label the user sees when there are saved bug reports, but the user asks to save another one.
  (close-and-save "保存并关闭") ;; button on the bottom of the bug report form
  (saved-unsubmitted-bug-reports "未提交的程序错误报告") 
  ;; the above string constant is next to previous line in same dialog, followed by list of bug report subjects (as buttons)
  (error-sending-bug-report "程序错误报告传输出错")
  (error-sending-bug-report-expln "在传输程序错误报告的过程中出现了错误。如果你能够正常浏览网络，请访问：\n\n    http://bugs.racket-lang.org/\n\n使用网页上的表单提交程序错误报告。对于由此产生的不便，我们表示抱歉。\n\n传输错误详情：\n~a")
  (illegal-bug-report "非法的程序错误报告")
  (pls-fill-in-field "请填写\"~a\"栏目")
  (malformed-email-address "电子邮件地址合格不正确")
  (pls-fill-in-either-description-or-reproduce "在“详细描述”和“再现程序错误的步骤”两栏中，请至少填写一项。")
  
  ;;; check syntax
  (check-syntax "检查语法")
  (cs-italic "斜体")
  (cs-bold "黑体")
  (cs-underline "下划线")
  (cs-change-color "改变颜色")
  (cs-foreground-color "前景色")
  (cs-background-color "背景色")
  (cs-tack/untack-arrow "附加/取消箭头")
  (cs-jump-to-next-bound-occurrence "跳至下一个被绑定出现")
  (cs-jump-to-binding "跳至绑定出现")
  (cs-jump-to-definition "跳至定义")
  (cs-error-message "出错信息")
  (cs-open-file "打开~a")
  (cs-rename-var "重命名~a")
  (cs-rename-id "重命名标识符")
  (cs-rename-var-to "将~a重命名为：")
  (cs-name-duplication-error "你所选择的新名称~s与当前辖域内现有标识符相同。")
  (cs-rename-anyway "强制重命名")
  (cs-status-init "检查语法：为用户代码初始化环境")
  (cs-status-coloring-program "检查语法：为程序着色")
  (cs-status-eval-compile-time "检查语法：编译程序")
  (cs-status-expanding-expression "检查语法：展开表达式")
  (cs-status-loading-docs-index "检查语法：加载文档索引")
  (cs-mouse-over-import "绑定~s由~s导入")  
  (cs-view-docs "察看~a的文档")
  (cs-view-docs-from "~a（来自~a）")  ;; a completed version of the line above
  ;; (cs-view-docs) is put into the first ~a and a list of modules (separated by commas) 
  ;; is put into the second ~a. Use check syntax and right-click on a documented variable (eg, 'require') to see this in use
  
  (cs-lexical-variable "词法变量")
  (cs-set!d-variable "set!过的变量")
  (cs-imported-variable "导入的变量")
  (cs-unused-require "无用的require")
  (cs-free-variable "自由变量")
  
  (cs-binder-count "~a次绑定出现")
  (cs-zero-varrefs "没有绑定出现")
  (cs-one-varref "1次被绑定出现")
  (cs-n-varrefs "~a次被绑定出现") ;; expected to have one ~a formatter that will accept a number
  
  (cs-contract-my-obligation "Contract：本module的义务")
  (cs-contract-their-obligation "Contract：客户module的义务")
  (cs-contract-both-obligation "Contract：本module和客户module的共同义务")
  (cs-contract-unk-obligation "Contract：义务未知")
  
  
  ;; mode sub-menu in the "view" menu
  (cs-check-syntax-mode "检查语法模式")
  (cs-mode-menu-show-my-obligations "我的Contract义务")
  (cs-mode-menu-show-client-obligations "客户的Contract义务")
  (cs-mode-menu-show-syntax "语法范畴")
  
  ;; the documentation blue boxes in the upper-right corner of the drracket window
  (sc-read-more... "阅读更多…")
  (sc-f2-to-un/lock "f2（解除）锁定")
  
  ;; the online check syntax status messages (mouse over the bottom right of drracket's window to see the messages during online expansion's various phases)
  (online-expansion-running "语法检查后台运行中")
  (online-expansion-only-raw-text-files-supported "仅支持纯文本文件")
  (online-expansion-abnormal-termination "语法检查后台异常退出")
  (online-expansion-finished-successfully "语法检查后台成功运行")
  
  (jump-to-error "跳至错误")
  (online-expansion-is-disabled "语法检查后台运行被禁用")
  ; these next two show up in the bar along the bottom of the drracket window
  (online-expansion-pending "语法检查后台运行……")
  (online-expansion-finished "后台语法检查完成") ;; note: there may still be errors in this case
  ; the next two show up in a menu when you click on the circle in the bottom right corner
  (disable-online-expansion "禁用后台语法检查")
  (enable-online-expansion "启用后台语法检查")
  ;; the online expansion preferences pane
  (online-expansion "后台语法检查") ;; title of prefs pane
  ; the different kinds of errors
  (online-expansion-show-read-errors-as "显示read错误")
  (online-expansion-show-variable-errors-as "显示未绑定变量")
  (online-expansion-show-other-errors-as "显示其他错误")
  ; locations the errors can be shown
  (online-expansion-error-gold-highlight "使用高亮突出")
  (online-expansion-error-margin "在一侧显示")
  ; the label of a preference in the (string-constant online-expansion) section
  (show-arrows-on-mouseover "鼠标悬停时显示绑定及尾位置箭头")
  ;;; info bar at botttom of drscheme frame
  (collect-button-label "垃圾收集")
  (read-only "只读")
  (auto-extend-selection "自动延长")
  (overwrite "覆盖")
  (running "运行中")
  (not-running "静止中")
  
  ;;; misc
  (welcome-to-something "欢迎来到~a")
  
  ; this appears in the drscheme about box.
  (welcome-to-drscheme-version/language "欢迎使用DrRacket，版本~a，~a")
  
  ; these appear on subsequent lines in the `Help|Welcome to DrRacket' dialog.
  (welcome-to-drscheme "欢迎使用DrRacket")
  
  (goto-line "跳至行号")
  (goto-line-invalid-number
   "~a不是合法的行号。必须提供一个在1和~a之间的数字")
  (goto-position "跳至位置")
  (no-full-name-since-not-saved
   "当前文件还没有被命名，因为从来没有对它进行保存。")
  (cannot-open-because-dne "无法打开~a，文件不存在。")
  
  (needs-execute-language-changed
   "警告：语言改变了。请单击“运行”。")
  (needs-execute-teachpack-changed
   "警告：教学包改变了。请单击“运行”。")
  (needs-execute-defns-edited
   "警告：定义视窗改变了。请单击“运行”。")
  
  (editor-changed-since-srcloc-recorded
   "编辑器在记录原始位置之后被改动过，故高亮区域不一定准确反映源码")
  
  (file-is-not-saved "文件\"~a\"还没有保存过")
  (save "保存")
  (close-anyway "强制关闭")
  (dont-save "不保存")
  (clear-anyway "强制清空")
  
  ;; menu item title
  (log-definitions-and-interactions "记录定义和交互的日志…")
  (stop-logging "不再记录日志")
  (please-choose-a-log-directory "请选择日志目录")
  (logging-to "记录日志到：")
  (erase-log-directory-contents "删除日志目录~a中的内容？")
  (error-erasing-log-directory "删除日志出错。\n\n~a\n")
  
  ;; menu items connected to the logger -- also in a button in the planet status line in the drs frame
  (show-log "显示日志(&L)")
  (hide-log "隐藏日志(&L)")
  (logger-scroll-on-output "跟随输出") ; a checkbox in the logger pane
  (log-messages "日志信息") ;; label for the drracket logging gui panel
  
  
  ;; modes
  (mode-submenu-label "模式")
  (scheme-mode "Scheme模式")
  (racket-mode "Racket模式")
  (text-mode "文本模式")
  
  (scheme-mode-color-symbol "符号")
  (scheme-mode-color-keyword "关键词")
  (scheme-mode-color-comment "注释")
  (scheme-mode-color-string "字符串")
  (scheme-mode-color-constant "常量")
  (scheme-mode-color-parenthesis "括号")
  (scheme-mode-color-hash-colon-keyword "#:关键词")
  (scheme-mode-color-error "错误")
  (scheme-mode-color-other "其他")
  ;; the ~a is filled in with one of the above (scheme-mode-*)
  (syntax-coloring-choose-color "为~a选择颜色")
  (preferences-colors "颜色") ;; used in the preferences dialog
  
  ;; parenthesis color scheme string constants
  (parenthesis-color-scheme "括号色彩调配") ;; label for the choice% menu in the preferences dialog
  (paren-color-basic-grey "单一灰色")
  (paren-color-shades-of-gray "渐变灰色")
  (paren-color-shades-of-blue "渐变蓝色")
  (paren-color-spring "春")
  (paren-color-fall "秋")
  (paren-color-winter "冬")
  
  
  (url: "URL：")
  (open-url... "打开URL…")
  (open-url "打开URL")
  (browse... "浏览…")
  (bad-url "错误的URL")
  (bad-url:this "错误的URL：~a")
  
  ;; Help Desk
  (help "帮助")
  (racket-documentation "Racket文档")
  (help-desk "帮助台")
  (plt:hd:search "搜索")
  (plt:hd:feeling-lucky "手气不错")
  (plt:hd:home "帮助台首页") 
  ; next 3 are popup menu choices in help desk search frame
  (plt:hd:search-for-keyword "关键词")
  (plt:hd:search-for-keyword-or-index "关键词或索引")
  (plt:hd:search-for-keyword-or-index-or-text "关键词、索引或普通文本")
  (plt:hd:exact-match "精确匹配")
  (plt:hd:containing-match "包含")
  (plt:hd:regexp-match "正则表达式匹配")
  (plt:hd:find-docs-for "搜索文档：")
  (plt:hd:search-stopped-too-many-matches "[搜索中断：过多匹配结果]")
  (plt:hd:nothing-found-for "找不到任何关于~a的信息")
  (plt:hd:and "并且")
  (plt:hd:refresh "更新")
  (plt:hd:refresh-all-manuals "更新所有手册")
  (plt:hd:manual-installed-date "（~a已安装）")
  ; Help Desk configuration
  ;; refreshing manuals
  (plt:hd:refreshing-manuals "重新下载手册")
  (plt:hd:refresh-downloading... "下载~a…")
  (plt:hd:refresh-deleting... "删除旧版本的~a…")
  (plt:hd:refresh-installing... "安装新版本的~a…")
  (plt:hd:refresh-clearing-indices "清除缓存中的索引")
  (plt:hd:refreshing-manuals-finished "完成。")
  (plt:hd:about-help-desk "关于帮助台")
  (plt:hd:help-desk-about-string
   "帮助台是Racket软件的完整信息来源。\n\n版本~a\n版权所有（c）~a—~a PLT")
  (plt:hd:help-on-help "关于帮助的帮助")
  (plt:hd:help-on-help-details
   "关于使用帮助台的帮助，请参见帮助台首页中的第一个链接‘帮助台’。（要进入帮助台的首页，请单击帮助台视窗上方的‘首页’按钮。）")
  (reload "刷新") ;; refresh the page in a web browser
  (plt:hd:ask-about-separate-browser
   "你选择了一个指向万维网的链接。请问您是要在帮助台中打开该页面，还是想使用浏览器程序浏览网页？")
  (plt:hd:homebrew-browser "帮助台浏览器") ;; choice for the above string (in a button)
  (plt:hd:separate-browser "网络浏览器") ;; other choice for the above string (also in a button)
  (plt:hd:external-link-in-help "在帮助台中显示外部URL")
  (plt:hd:use-homebrew-browser "对于外部URL，使用帮助台浏览")
  (plt:hd:new-help-desk "新帮助台")
  
  ;; in the Help Desk language dialog, title on the right.
  (plt:hd:manual-search-ordering "搜索手册的顺序")
  
  ;; in the help-desk standalone font preference dialog, on a check box
  (use-drscheme-font-size "使用和DrRacket相同的字体大小")
  
  ;; in the preferences dialog in drscheme there is example text for help desk font size.
  ;; clicking the links in that text produces a dialog with this message
  (help-desk-this-is-just-example-text
   "这里显示的只是示例字体大小的文字。要察看这些链接，请通过帮助菜单打开真正的帮助台。")
  
  ;; this appears in the bottom part of the frame the first time the user hits `f1' 
  ;; (assuming nothing else has loaded the documentation index first)
  ;; see also: cs-status-loading-docs-index
  (help-desk-loading-documentation-index "帮助台：正在读入文档索引")
  
  ;; Help desk htty proxy
  (http-proxy "HTTP代理")
  (proxy-direct-connection "直接连接")
  (proxy-use-proxy "使用代理服务器：")
  (proxy-host "地址")
  (proxy-port "端口")
  (proxy-bad-host "无效代理服务器")
  
  ;; browser
  (rewind-in-browser-history "后退")
  (forward-in-browser-history "前进")
  (home "主页")
  (browser "浏览器")
  (external-browser-choice-title "外部浏览器") ; title for radio-button set
  (browser-command-line-label "命令行：") ; label for radio button that is followed by text boxes
  (choose-browser "选择浏览器")
  (no-browser "以后再询问")
  (browser-cmdline-expl-line-1 "（命令行由前缀文字，URL和后缀文字") ; explanatory text for dialog, line 1
  (browser-cmdline-expl-line-2 "连接而成，中间不含任何空格）") ; ... line 2. (Anyone need more lines?)
  (install? "安装？")  ;; if a .plt file is found (title of dialog)
  (you-have-selected-an-installable-package "你选择了一个可以安装的软件包。")
  (do-you-want-to-install-it? "是否安装？")
  (paren-file-size "（该文件的长度是~a字节）")
  (download-and-install "下载并安装(&&)") ;; button label
  (download "下载") ;; button label
  (save-downloaded-file/size "下载文件(~a字节)并保存为") ;; label for get-file dialog
  (save-downloaded-file "下载文件并保存为")  ;; label for get-file dialog
  (downloading "下载中") ;; dialog title
  (downloading-file... "下载文件中……")
  (package-was-installed "安装已完成。")
  (download-was-saved "文件已保存。")
  
  (install-plt-file-menu-item... "安装.plt文件…")
  (install-plt-file-dialog-title "安装.plt文件")
  (install-plt-web-tab "网络")
  (install-plt-file-tab "文件")
  (install-plt-filename "文件名：")
  (install-plt-url "URL：")
  (install-plt-error-header "检查下载的.plt文件时出错。请检查URL并重试。")
  
  ;; install plt file when opened in drscheme strings
  (install-plt-file "安装~a，还是打开以供编辑？")
  (install-plt-file/yes "安装")
  (install-plt-file/no "编辑")
  
  (plt-installer-progress-window-title "安装进度") ;; frame title
  (plt-installer-abort-installation "取消安装") ;; button label
  (plt-installer-aborted "安装中止。") ;; msg that appears in the installation window when installation is aborted
  
  ;;; about box
  (about-drscheme-frame-title "关于DrRacket")
  
  ;;; save file in particular format prompting.
  (save-as-plain-text "保存本文件为纯文本？")
  (save-in-drs-format "保存本文件为drscheme（非纯文本）格式？")
  (yes "是")
  (no "否")
  
  ;; saving image (right click on an image to see the text)
  (save-image "保存图片…")
  
  ;;; preferences
  (preferences "首选项")
  (error-saving-preferences "保存首选项时出错：~a")
  (error-saving-preferences-title "保存首选项时出错")
  (steal-the-lock-and-retry "取消锁定并重试(&&)") ;; in the preferences error dialog; this happens when the lockfile exists (after 3 pref writes).
  
  (error-reading-preferences "读取首选项时出错")
  (error-reading-preferences-explanation "首选项文件被锁定，故~a选项无法读取")
  ;; in the above, ~a is filled with the name of the preference (a symbol)
  (dont-ask-again-until-drracket-restarted "不再询问（直到DrRacket关闭）")
  ; difference between the above and below is one comes with a question (steal the lock or not) and the other with just a notation saying "the file is locked"
  (dont-notify-again-until-drracket-restarted "不再通知（直到DrRacket关闭）") 
  (prefs-file-locked "存储首选项的文件被锁定了（由于文件~a的存在），所以这些改动无法被保存。放弃修改？")
  (try-again "重试") ;; button label
  (give-up-and-use-the-default "放弃并使用默认值") ;; button label
  
  (prefs-file-still-locked "存储首选项的文件仍然被锁定（由于文件~a的存在）, 所以这些改动将不会被保存。")
  (prefs-file-locked-nothing-doing
   "首选项文件友~s锁定，故修改不会被保存。")
  ;; the  ~s is filled with the lockfile; this string is (currently) used only on windows where lockfiles are less friendly (and there is no steal fallback)
  
  (scheme-prefs-panel-label "Racket")
  (warnings-prefs-panel-label "警告")
  (editor-prefs-panel-label "编辑")
  (general-prefs-panel-label "常规")
  (highlight-parens "加亮显示匹配的括号")
  (fixup-open-brackets "自动调整开中括号")
  (fixup-close-parens "自动调整闭括号")
  (flash-paren-match "高亮显示括号匹配")
  (auto-save-files "自动保存文件")
  (backup-files "保存备份文件")
  (map-delete-to-backspace "将delete转换成backspace")
  (verify-exit "退出时确认")
  (ask-before-changing-format "改变保存方式时确认")
  (wrap-words-in-editor-buffers "在编辑器中自动换行")
  (show-status-line "显示状态栏")
  (count-columns-from-one "从一开始计算行号")
  (display-line-numbers "在编辑器中显示行号")
  (show-line-and-column-numbers "显示行号和列号(&&)") ; used for popup menu; right click on line/column box in bottom of drs window
  (show-character-offsets "显示字符在文件中的位置") ; used for popup menu; right click on line/column box in bottom of drs window
  (enable-keybindings-in-menus "允许使用菜单中的快捷键")
  (printing-mode "打印模式")
  (print-using-platform-specific-mode "平台特定的打印")
  (print-to-ps "打印至PostScript文件")
  (print-to-pdf "打印至PDF文件")
  (command-as-meta "将command键当作meta") ;; macos/macos x only
  (reuse-existing-frames "在打开新文件时，使用现有的视窗")
  (default-fonts "默认字体")
  (paren-match-color "高亮显示括号所使用的颜色") ; in prefs dialog
  (online-coloring-active "实时根据语法用颜色标记程序")
  (open-files-in-tabs "在不同的标签下打开多个文件（不使用多个视窗）")
  (show-interactions-on-execute "在运行程序时自动打开交互视窗")
  (switch-to-module-language-automatically "打开module文件时自动切换至module语言")
  (interactions-beside-definitions "将定义视窗和交互视窗左右放置") ;; in preferences, below the checkbox one line above this one
  (show-line-numbers "显示行号")
  (show-line-numbers/menu "显示行号(&N)")  ;; just like the above, but capitalized for appearance in a menu item
  (hide-line-numbers/menu "隐藏行号(&N)")
  (show-line-numbers-in-definitions "在定义视窗中显示全部行号")
  ;; the constant above shows up in the popup menu item in the bottom of
  ;; the drracket window; controls the line numbers on each line in the definitions; used in a checkable menu item
  (limit-interactions-size "限制交互视窗的大小")
  (background-color "背景颜色")
  (default-text-color "默认颜色") ;; used for configuring colors, but doesn't need the word "color"
  (choose-a-background-color "请选择背景颜色")
  (revert-to-defaults "恢复默认")
  (undo-changes "不做修改并退出") ;; used in the preferences dialog to undo preference changes
  
  (black-on-white-color-scheme "白底黑字") ;; these two appear in the color preferences dialog on butttons
  (white-on-black-color-scheme "黑底白字") ;; clicking the buttons changes the color schemes to some defaults that've been set up.
  
  (add-spacing-between-lines "在行间额外填充一个像素")
  
  ; title of the color choosing dialog
  
  ; should have entire alphabet
  (font-example-string "简体中文 by 朱崇恺")
  
  (change-font-button-label "更改")
  (fonts "字体")
  (other... "其他…") ;; used in the font choice menu item
  
  ; filled with type of font, eg modern, swiss, etc.
  (choose-a-new-font "请选择新的“~a”字体")
  
  (font-size-slider-label "字号")
  (restart-to-see-font-changes "重新启动，使修改生效")
  
  (font-prefs-panel-title "字体")
  (font-name "字体名称")
  (font-size "字体大小")
  (set-font "设置字体…")
  (font-smoothing-label  "字体平滑度")
  (font-smoothing-none "无")
  (font-smoothing-some "部分")
  (font-smoothing-all "全部")
  (font-smoothing-default "使用系统默认值")
  (select-font-name "选择字体")
  (example-text "示例文本：")
  (only-warn-once "当定义视窗和交互视窗不同步时，仅警告一次")
  
  ; warning message when lockfile is around
  (waiting-for-pref-lock "等待首选项设置文件解锁……")
  (pref-lock-not-gone
   "首选项设置文件被\n\n~a\n\n锁定。请确定没有其他Racket软件正在运行中，然后删除该锁定文件。")
  (still-locked-exit-anyway? "首选项无法保存。仍然退出？")
  
  ;;; indenting preferences panel
  (indenting-prefs-panel-label "缩进")
  (indenting-prefs-extra-regexp "额外的正则表达式")
  
  (square-bracket-prefs-panel-label "中括号")
  
  ; filled with define, lambda, or begin
  (enter-new-keyword "请输入一个类似于~a的关键词：")
  (x-keyword "~a关键词")
  (x-like-keywords "~a类型的关键词")
  
  ; used in Square bracket panel
  (skip-subexpressions "出现在中括号前的表达式数量")
  
  (expected-a-symbol "需要symbol，实得：~a")
  (already-used-keyword "“~a”已经是缩进关键词了")
  (add-keyword "添加")
  (remove-keyword "删除")
  
  ; repl color preferences
  (repl-colors "REPL")
  (repl-out-color "输出")
  (repl-value-color "值")
  (repl-error-color "错误")
  
  ;;; find/replace
  (search-next "下一个")
  (search-previous "上一个")
  (search-match "匹配")  ;;; this one and the next one are singular/plural variants of each other
  (search-matches "匹配")
  (search-replace "替换")
  (search-skip "跳过")
  (search-show-replace "显示替换")
  (search-hide-replace "隐藏替换")
  (find-case-sensitive "大小写敏感")  ;; the check box in both the docked & undocked search
  (find-anchor-based "用锚进行搜索")
  
  ;; these string constants used to be used by searching,
  ;; but aren't anymore. They are still used by other tools, tho.
  (hide "隐藏")
  (dock "停靠")
  (undock "取消停靠")
  
  ;;; multi-file-search
  (mfs-multi-file-search-menu-item "在文件中查找(&F)…")
  (mfs-string-match/graphics "字符串匹配（可用与包含图像的文件）")
  (mfs-regexp-match/no-graphics "正则表达式匹配（只适用于纯文本文件）")
  (mfs-searching... "查找…")
  (mfs-configure-search "查找设置") ;; dialog title
  (mfs-files-section "文件")   ;; section in config dialog
  (mfs-search-section "查找") ;; section in config dialog
  (mfs-dir "目录")
  (mfs-recur-over-subdirectories "包含子目录")
  (mfs-regexp-filename-filter "文件名筛选（正则表达式）")
  (mfs-search-string "查找字符串")
  (mfs-drscheme-multi-file-search "多文件查找——DrRacket") ;; results window and error message title
  (mfs-not-a-dir "“~a”不是目录")
  (mfs-open-file "打开文件")
  (mfs-stop-search "停止查找")
  (mfs-case-sensitive-label "大小写敏感")
  (mfs-no-matches-found "没有找到匹配结果。")
  (mfs-search-interrupted "查找中止。")
  (mfs-drscheme-multi-file-search-title "多文件查找“~a”——DrRacket") ;; the ~a format specifier is filled in with the search string
  
  ;;; reverting a file
  (are-you-sure-revert
   "你确定要复原该文件吗？这一操作无法撤销。")
  (are-you-sure-revert-title
   "复原？")
  
  ;;; saving a file
  ; ~a is filled with the filename
  (error-saving "保存出错") ;; title of error message dialog
  (error-saving-file/name "在保存文件~a时出现错误。")
  (error-loading "读取出错")
  (error-loading-file/name "在读取~a时出现错误.")
  (unknown-filename "《未知》")
  
  ;;; finder dialog
  (must-specify-a-filename "你必须指定文件名")
  (file-does-not-exist "文件“~a”不存在。")
  (ask-because-file-exists "文件“~a”已存在。是否替换？")
  (dne-or-cycle "文件“~a”中包含不存在的目录或循环")
  (get-file "获取文件")
  (put-file "放置文件")
  (full-pathname "完整路径")
  (show-dot-files "由点号开始显示文件名和目录名。")
  (up-directory-button-label "上层目录")
  (add-button-label "添加") ;;; for multi-file selection
  (add-all-button-label "全部添加") ;;; for multi-file selection
  (remove-button-label "移除") ;;; for multi-file selection
  (file-wrong-form "该文件名格式不正确")
  (select-files "选择文件")
  (select-file "选择文件")
  (dir-dne "该目录不存在。")
  (file-dne "该文件不存在。")
  (empty-filename "文件名中必须包含文字。")
  (that-is-dir-name "这是一个目录名。")
  
  ;;; raw menu names -- these must match the 
  ;;; versions below, once the &s have been stripped.
  ;;; if they don't, DrRacket's menus will appear
  ;;; in the wrong order.
  (file-menu "文件")
  (edit-menu "编辑")
  (help-menu "帮助")
  (windows-menu "视窗")
  (tabs-menu "标签") ;; this is the name of the "Windows" menu under linux & windows
  
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
  
  (new-info  "打开新文件")
  (new-menu-item "新建(&N)")
  (new-...-menu-item "新建(&N)…")
  
  (open-info "打开现有文件")
  (open-menu-item "打开(&O)…")
  
  (open-recent-info "最近使用过文件的列表")
  (open-recent-menu-item "最近使用过(&T)")
  
  (revert-info "将当前文件恢复为磁盘上的副本")
  (revert-menu-item "恢复(&R)")
  
  (save-info "保存当前文件")
  (save-menu-item "保存(&S)")
  
  (save-as-info "输入新的文件名,保存当前文件")
  (save-as-menu-item "另存为(&A)…")
  
  (print-info "打印当前文件")
  (print-menu-item "打印(&P)…")
  
  (page-setup-info "设置打印参数")
  (page-setup-menu-item "页面设置…")
  
  (close-info "关闭当前文件")
  (close-menu-item "关闭(&C)")
  (close-window-menu-item "关闭视窗(&C)")
  
  (quit-info "关闭所有视窗")
  (quit-menu-item-windows "退出(&X)")
  (quit-menu-item-others "退出(&Q)")
  
  (edit-menu-label "编辑(&E)")
  
  (undo-info "撤销最近的操作")
  (undo-menu-item "撤销(&U)")
  
  (redo-info "取消最近的撤销操作")
  (redo-menu-item "重复(&R)")
  
  (cut-info "将当前选中的对象移入剪贴版")
  (cut-menu-item "剪切(&T)")
  
  (copy-info "将当前选中的对象复制到剪贴版")
  (copy-menu-item "复制(&C)")
  
  (paste-info "将剪贴版中个内容复制到当前位置")
  (paste-menu-item "粘贴(&P)")
  
  (clear-info "删除当前选中的对象")
  (clear-menu-item-windows "删除(&D)")
  
  (select-all-info "选中整个文件")
  (select-all-menu-item "全选(&L)")
  
  (find-menu-item "查找") ;; menu item
  (find-from-selection-menu-item "查找当前选中")
  (find-info "在主视窗和查找栏之间切换光标位置")
  
  (find-again-info "跳至该文本的下一个出现")
  (find-again-menu-item "查找下一个")
  
  (find-again-backwards-info "跳至该文本的前一个出现")
  (find-again-backwards-menu-item "查找上一个")
  
  (show-replace-menu-item "显示替换")
  (hide-replace-menu-item "隐藏替换")
  (show/hide-replace-info "切换替换面板的可见性")
  
  (replace-menu-item "替换")
  (replace-info "替换当前圈出的查找结果")
  
  (replace-all-info "替换查找字符串的所有出现")
  (replace-all-menu-item "全部替换")
  
  (find-case-sensitive-info "切换大小写敏感或不敏感查找")
  (find-case-sensitive-menu-item "大小写敏感")
  
  (complete-word "自动完成") ; the complete word menu item in the edit menu
  (no-completions "……无自动完成结果") ; shows up in the completions menu when there are no completions (in italics)
  
  (overwrite-mode "覆盖模式")
  (enable-overwrite-mode-keybindings "启用覆盖模式的快捷键")
  
  (enable-automatic-parens "自动关括号") ; should "and square brackets and quotes" appear here?
  
  (preferences-info "设置首选项")
  (preferences-menu-item "首选项…")
  
  (keybindings-info "显示当前快捷键")
  (keybindings-menu-item "快捷键")
  (keybindings-show-active "显示当前的快捷键")
  (keybindings-frame-title "快捷键")
  (keybindings-sort-by-name "按名称排序")
  (keybindings-sort-by-key "按键名排序")
  (keybindings-add-user-defined-keybindings "添加自定义快捷键…")
  (keybindings-add-user-defined-keybindings/planet "从PLaneT添加自定义快捷键…")
  (keybindings-menu-remove "移除~a")
  (keybindings-choose-user-defined-file "请选择一个包含快捷键的文件")
  (keybindings-planet-malformed-spec "错误的PLaneT名称：~a") ; the string will be what the user typed in
  (keybindings-type-planet-spec "请输入PLaneT包名称（无需输入‘require’）")
  
  ; first ~a will be a string naming the file or planet package where the keybindings come from;
  ; second ~a will be an error message
  (keybindings-error-installing-file "安装快捷键~a时出错：\n\n~a")
  
  (user-defined-keybinding-error "快捷键~a出错\n\n~a")
  (user-defined-keybinding-malformed-file "文件~a并不是一个使用framework/keybinding-lang语言编写的module.")
  (user-defined-keybinding-malformed-file/found-lang
   "文件~a并不是一个使用framework/keybinding-lang语言编写的module，而是由~s语言编写")
  
  ;; menu items in the "special" menu
  (insert-text-box-item "插入文本框")
  (insert-image-item "插入图片…")
  (insert-comment-box-menu-item-label "插入注释框")
  (insert-lambda "插入λ")
  
  (wrap-text-item "自动换行")
  
  (windows-menu-label "视窗(&W)")
  (tabs-menu-label "标签(&T)") ;; this is the name of the menu under linux & windows
  (minimize "最小化") ;; minimize and zoom are only used under mac os x
  (zoom "缩放")
  (bring-frame-to-front "前端显示")       ;;; title of dialog
  (bring-frame-to-front... "前端显示…") ;;; corresponding title of menu item
  (most-recent-window "最近的视窗")
  (next-tab "下一个标签")
  (prev-tab "前一个标签")
  ;; menu item in the windows menu under mac os x. first ~a is filled with a number between 1 and 9; second one is the filename of the tab
  (tab-i "标签~a：~a")
  (tab-i/no-name "标签~a")
  
  (view-menu-label "视图(&V)")
  (show-overview "显示程序轮廓(&P)") 
  (hide-overview "隐藏程序轮廓(&P)")
  (show-module-browser "显示&Module浏览器")
  (hide-module-browser "隐藏&Module浏览器")
  
  (help-menu-label "帮助(&H)")
  (about-info "本软件的详细信息以及致谢名单")
  (about-menu-item "关于…")
  
  ;; open here's new menu item
  (create-new-window-or-clear-current
   "您是想打开一个新视窗，还是清空当前视窗？")
  (clear-current "清空当前")
  (new-window "新视窗")
  
  ;; popup menu when right-clicking in the gap between
  ;; the definitions and interactions window
  (change-to-vertical-alignment "左右分割")
  (change-to-horizontal-alignment "上下分割")
  
  ;;; exiting and quitting ``are you sure'' dialog
  ;;; exit is used on windows, quit on macos, in English. Other
  ;;; languages probably use the same word on both platforms.
  (exit "退出")
  (quit "退出")
  (are-you-sure-exit "你确定要退出吗?")
  (are-you-sure-quit "你确定要退出吗?")
  ; these next two are only used in the quit/exit dialog
  ; on the button whose semantics is "dismiss this dialog".
  ; they are there to provide more flexibility for translations
  ; in English, they are just cancel.
  (dont-exit "取消") 
  (dont-quit "取消")
  
  ;;; autosaving
  (error-autosaving "自动保存\"~a\"时出错。") ;; ~a will be a filename
  (autosaving-turned-off "在文件被存盘之前，自动保存不会进行")
  (recover-autosave-files-frame-title "恢复自动保存的文件")
  (autosave-details "详细情况")
  (autosave-recover "恢复")
  (autosave-unknown-filename "《未知》")
  
  ;; these are labels in a dialog that drscheme displays
  ;; if you have leftover autosave files. to see the dialog,
  ;; start up drscheme and modify (but don't save) a file
  ;; (also, do this with an unsaved file). Wait for the autosave
  ;; files to appear (typically 5 minutes). Kill DrRacket
  ;; and restart it. You'll see the dialog
  (autosave-autosave-label: "自动保存文件：")
  (autosave-original-label: "原始文件：")
  (autosave-autosave-label "自动保存文件")
  (autosave-original-label "原始文件")
  (autosave-compare-files "比较自动保存文件")
  
  (autosave-show-autosave "自动保存文件") ;; title of a window showing the autosave file
  
  (autosave-explanation "DrRacket发现了自动保存的文件，其中可能包含你没有保存过的程序")
  
  (autosave-recovered! "已恢复！") ;; status of an autosave file
  (autosave-deleted "已删除")       ;; status of an autosave file
  
  (autosave-error-deleting "删除~a出错\n\n~a") ;; first is a filename, second is an error message from mz.
  (autosave-delete-button "删除")
  (autosave-delete-title "删除")  ;; title of a dialog asking for deletion confirmation
  (autosave-done "完成")
  
  ;; appears in the file dialog
  (autosave-restore-to-where? "请选择自动保存文件的位置")
  
  
  ;;; file modified warning
  (file-has-been-modified
   "文件在上次保存之后又改动过。覆盖这些更改？")
  (overwrite-file-button-label "覆盖")
  
  (definitions-modified 
    "当前磁盘文件已被修改；请保存或恢复文件。")
  (drscheme-internal-error "DrRacket内部错误")
  
  ;;; tools
  (invalid-tool-spec "Collection ~a中info.rkt的tool定义不正确。需要一个字符串或者一个非空表，实得：~e")
  (error-invoking-tool-title "调用tool ~s出错；~s")
  (error-loading-tool-title "载入tool ~s出错\n~a") ;; ~s filled with a path, ~a filled with an error message from an exn
  
  (tool-tool-names-same-length
   "在~s的info.rkt文件中，‘tool-names’和‘tools’应该是等长的表，实得~e和~e")
  (tool-tool-icons-same-length
   "在~s的info.rkt文件中，‘tool-icons’和“tools’应该是等长的表，实得~e和~e")
  (tool-tool-urls-same-length
   "在~s的info.rkt文件中，‘tool-urls’和“tools’应该是等长的表，实得~e和~e")
  (error-getting-info-tool
   "载入~s的info.rkt出错")
  (tool-error-phase1 "tool ~s第一阶段出错；~s")
  (tool-error-phase2 "tool ~s第二阶段出错；~s")
  
  
  ;;; define popup menu
  (end-of-buffer-define "<<缓冲区结束>>")
  (sort-by-name "按名称排序")
  (sort-by-position "按文件中的位置排序")
  (no-definitions-found "<<没有定义>>")
  (jump-to-defn "跳至~a的定义")
  
  (recent-items-sort-by-age "按时间排序")
  (recent-items-sort-by-name "按名称排序")
  
  ;;; view menu
  (hide-definitions-menu-item-label "隐藏定义(&D)")
  (show-definitions-menu-item-label "显示定义(&D)")
  (definitions-menu-item-help-string "显示/隐藏定义视窗")
  (show-interactions-menu-item-label "显示交互(&I)")
  (hide-interactions-menu-item-label "隐藏交互(&I)")
  (use-horizontal-layout "左右分割")
  (use-vertical-layout "上下分割")
  (interactions-menu-item-help-string "显示/隐藏交互视窗")
  (toolbar "工具栏")
  (toolbar-on-top "顶置工具栏")
  (toolbar-on-top-no-label "顶置工具栏并使用小图标")
  (toolbar-on-left "左置工具栏")
  (toolbar-on-right "右置工具栏")
  (toolbar-hidden "隐藏工具栏")
  
  ;;; file menu
  (save-definitions-as "将定义另存为(&A)")
  (save-definitions "保存定义")
  (print-definitions "打印定义…")
  (about-drscheme "关于DrRacket")
  (save-other "保存其他")
  (save-definitions-as-text "将定义保存为文本…")
  (save-interactions "保存交互")
  (save-interactions-as "将交互另存为…")
  (save-interactions-as-text "将交互保存为文本…")
  (print-interactions "打印交互…")
  (new-tab "新建标签")
  (close-tab "关闭标签") ;; must not have any &s in it.
  (close-tab-amp "关闭标签(&C)") ;; like close-tab, but with an ampersand on the same letter as the one in close-menu-item
  
  ;;; edit-menu
  (split-menu-item-label "分屏(&S)")
  (collapse-menu-item-label "合并(&O)")
  (find-longest-line "寻找最长一行")
  
  ;;; language menu
  (language-menu-name "语言(&L)")
  
  ;;; scheme-menu
  (scheme-menu-name "Ra&cket")
  (execute-menu-item-label "运行")
  (execute-menu-item-help-string "重新运行定义视窗中的程序")
  (ask-quit-menu-item-label "要求程序终止")
  (ask-quit-menu-item-help-string "使用break-thread中止当前计算得主线程")
  (force-quit-menu-item-label "强制终止")
  (force-quit-menu-item-help-string "使用custodian-shutdown-all退出当前计算")
  (limit-memory-menu-item-label "限制内存使用…")
  (limit-memory-msg-1 "内存限制会在下一次运行时生效，")
  (limit-memory-msg-2 "内存限制最低值为八兆字节。")   (limit-memory-unlimited "无限制")
  (limit-memory-limited "限制")
  (limit-memory-megabytes "兆字节(MB)")
  ; the next two constants are used together in the limit memory dialog; they are inserted
  ; one after another. The first one is shown in a bold font and the second is not.
  ; (the first can be the empty string)
  (limit-memory-warning-prefix "警告：")
  (limit-memory-warning 
   "不限制内存有潜在的危险。DrRacket将无法保护自己，当运行的程序分配过多内存时，DrRacket会崩溃.")
  
  (clear-error-highlight-menu-item-label "清除错误高亮显示")
  (clear-error-highlight-item-help-string "清除错误区域的粉红色高亮显示")
  (jump-to-next-error-highlight-menu-item-label "跳至下一个高亮错误")
  (jump-to-prev-error-highlight-menu-item-label "跳至前一个高亮错误")
  (reindent-menu-item-label "调整缩进(&R)")
  (reindent-all-menu-item-label "全文调整缩进(&A)")
  (semicolon-comment-out-menu-item-label "用分号注释(&C)")
  (box-comment-out-menu-item-label "用注释框注释(&C)")
  (uncomment-menu-item-label "取消注释(&U)")
  
  (convert-to-semicolon-comment "转化为分号注释")
  
  ;;; executables
  (create-executable-menu-item-label "创建可执行程序(&E)…")
  (create-executable-title "创建可执行程序")
  (drracket-creates-executables-only-in-some-languages
   "在DrRacket中创建可执行程序仅支持下列语言选择：在DrRacket语言对话框中选择教学语言（DMdA或者HtDP）；在DrRacket语言对话框中选择“Racket语言”，并且用#lang在文件开始处指定语言。\n\n请考虑使用命令行工具raco exe。")
  (must-save-before-executable "在创建可执行程序之前，你必须保存源程序")
  (save-a-mred-launcher "保存为GRacket启动程序")
  (save-a-mzscheme-launcher "保存为Racket启动程序")
  (save-a-mred-stand-alone-executable "保存为GRacket独立可执行程序")
  (save-a-mzscheme-stand-alone-executable "保存为Racket独立可执行程序")
  (save-a-mred-distribution "保存为GRacket可发布程序")
  (save-a-mzscheme-distribution "保存为Racket可发布程序")
  (error-creating-executable "创建可执行程序出错：") ;; this is suffixed with an error message ala error-display-handler
  
  (definitions-not-saved "当前定义视窗中的程序并没有被保存过。将使用最近保存过的版本来生成可执行程序。是否继续？")
  ;; The "-explanatory-label" variants are the labels used for the radio buttons in
  ;;  the "Create Executable..." dialog for the "(module ...)" language.
  (launcher "启动程序")
  (launcher-explanatory-label "启动程序（仅在本机运行，运行源代码）")
  (stand-alone "独立程序")
  (stand-alone-explanatory-label "独立程序（仅在本机运行，运行编译代码）")
  (distribution "可发布程序")
  (distribution-explanatory-label "可发布程序（可以在其它计算机上安装并运行）")
  (executable-type "类型")
  (executable-base "基于")
  (filename "文件名：")
  (create "创建")
  (please-specify-a-filename "请指定文件名。")
  (~a-must-end-with-~a
   "~a文件名\n\n  ~a\n\n不合法。文件名必须以\".~a\"结尾。")
  (macosx-executables-must-end-with-app
   "~a文件名\n\n  ~a\n\n不合法。在MacOS X中，文件名必须以.app结尾。")
  (warning-directory-will-be-replaced
   "警告：目录：\n\n  ~a\n\n将会被重置。继续操作？")
  
  (distribution-progress-window-title "创建进程")
  (creating-executable-progress-status "创建可执行程序……")
  (assembling-distribution-files-progress-status "汇编……")
  (packing-distribution-progress-status "打包……")
  
  (create-servlet "创建Servlet……")
  
  ; the ~a is a language such as "module" or "algol60"
  (create-servlet-unsupported-language
   "无法为~a语言程序创建Servlet。")
  
  ;;; buttons
  (execute-button-label "运行") 
  (save-button-label "保存")
  (break-button-label "停止")
  
  ;;; search help desk popup menu
  (search-help-desk-for "在帮助台中搜索“~a”")
  (exact-lucky-search-help-desk-for "在帮助台中搜索最符合“~a”的一个页面")
  
  ;; collapse and expand popup menu items
  (collapse-sexp "折叠S表达式")
  (expand-sexp "扩展S表达式")
  
  ;;; fraction dialog
  (enter-fraction "输入分数")
  (whole-part "整数部分")
  (numerator "分子")
  (denominator "分母")
  (insert-number/bad-whole-part "整数部分必须输入一个整数")
  (insert-number/bad-numerator "分子必须是非负整数")
  (insert-number/bad-denominator "分母必须是正整数")
  (insert-fraction-menu-item-label "插入分数…")
  
  ;; number snip popup menu
  (show-decimal-expansion "用十进制表示")
  (show-mixed-fraction-view "用带分数表示")
  (show-improper-fraction-view "用假分数表示")
  (show-more-decimal-places "显示更多小数位")
  
  ;;; Teachpack messages
  (select-a-teachpack "选择教学包")
  (clear-teachpack "卸载教学包~a")
  (teachpack-error-label "DrRacket——教学包出错")
  (teachpack-didnt-load "无法装载教学包~a。")
  (add-teachpack-menu-item-label "加载教学包……")
  (clear-all-teachpacks-menu-item-label "卸载全部教学包")
  (drscheme-teachpack-message-title "DrRacket教学包")
  (already-added-teachpack "教学包~a已装载")
  
  ; ~a is filled with the teachpack's name; the message appears in the teachpack selection dialog when a user installs a new teachpack
  (compiling-teachpack "编译教学包~a……")
  (teachpack-pre-installed "自带的教学包")
  (teachpack-pre-installed/htdp "自带的HtDP教学包")
  (teachpack-pre-installed/2htdp "自带的HtDP/2e教学包")
  (teachpack-user-installed "用户安装的教学包")
  (add-teachpack-to-list... "添加教学包…")
  (teachpack-already-installed "已经存在一个名为'~a'的教学包。是否覆盖？")
  ; ~a is filled with a list of language names. Each name is separated by a newline and is indented two spaces (no commas, no 'and')
  (teachpacks-only-in-languages "教学包菜单仅在下列语言中有效：~a\n\n在其他语言中，使用“require”。")
  
  ;;; Language dialog
  (introduction-to-language-dialog
   "请选择语言。大部分入门级的学生都可以使用默认语言。")
  (language-dialog-title "语言选择")
  (case-sensitive-label "大小写敏感")
  (output-style-label "输出格式")
  (constructor-printing-style "构造器")
  (quasiquote-printing-style "Quasiquote")
  (write-printing-style "write")
  (print-printing-style "print")
  (sharing-printing-label "显示内存共享")
  (use-pretty-printer-label "print多个对象时自动换行")
  (input-syntax "输入语法")
  (dynamic-properties "动态属性")
  (output-syntax "输出语法")
  (teachpacks "教学包") ;; label in the language dialog for the teaching languages
  (teachpacks-none "《无》") ;; shows up under the previous string, when there are no teachpacks
  (no-debugging-or-profiling "不调试，也不性能分析")
  (debugging "调试")
  (debugging-and-profiling "调试，并性能分析")
  (test-coverage "语法测试套件覆盖")
  (show-details-button-label "显示详情")
  (hide-details-button-label "隐藏详情")
  (choose-language-menu-item-label "选择语言…")
  (revert-to-language-defaults "恢复默认语言设置")
  (fraction-style "分数格式")
  (use-mixed-fractions "带分数")
  (use-repeating-decimals "循环小数")
  (decimal-notation-for-rationals "使用十进制表示有理数")
  (enforce-primitives-group-box-label "初始绑定")
  (enforce-primitives-check-box-label "不允许改变初始绑定")
  (automatically-compile "填充“compiled”目录（加载更快）")
  (preserve-stacktrace-information "保留堆栈跟踪信息（禁用某些优化）")
  (expression-level-stacktrace "表达式级堆栈跟踪")
  (function-level-stacktrace "函数级堆栈跟踪")
  (submodules-to-run "运行子module")
  (add-submodule "添加子module选项…") ;; menu item
  (add-submodule-title "添加子module") ;; title of dialog opened by above menu item
  
  ; used in the bottom left of the drscheme frame as the label
  ; above the programming language's name
  ; used the popup menu from the just above; greyed out and only
  ; visible when some languages are in the history
  (recent-languages "最近使用的语言：")
  ; shows up in bottom-left programming language menu popup, when no langs are recorded
  (no-recently-chosen-languages "没有最近使用过的语言")
  
  ;; startup wizard screen language selection section
  (please-select-a-language "请选择语言")
  
  
  ;;; languages
  (beginning-student "初级")
  (beginning-one-line-summary "define、cond、结构体、常量和基本操作")
  (beginning-student/abbrev "初级+缩写的表")
  (beginning/abbrev-one-line-summary "在初级的基础上，用缩写形式输出表")
  (intermediate-student "中级")
  (intermediate-one-line-summary "在初级的基础上增加了词法作用域")
  (intermediate-student/lambda "中级+lambda")
  (intermediate/lambda-one-line-summary "在中级的基础上，增加了高阶函数")
  (advanced-student "高级")
  (advanced-one-line-summary "在中级的基础上，增加了lambda和赋值")
  (how-to-design-programs "程序设计方法/How to Design Programs") ;; should agree with MIT Press on this one...
  (pretty-big-scheme "大")
  (pretty-big-scheme-one-line-summary "MzScheme/MrEd加HtDP(程序设计方法)语言")
  (r5rs-lang-name "R5RS")
  (r5rs-one-line-summary "Scheme语言标准第5修改稿")
  (expander "Expander")
  (expander-one-line-summary "展开表达式，而不是求值")
  (legacy-languages "过去的语言")
  (teaching-languages "教学语言")
  (experimental-languages "实验语言")
  (initial-language-category "初始语言")
  (no-language-chosen "还没有选择语言")
  (other-languages "其他语言")
  
  (module-language-name "由源代码来确定语言")
  (module-language-one-line-summary "由#lang行来确定实际使用的语言")
  (module-language-auto-text "自动加入#lang行") ;; shows up in the details section of the module language
  
  ;; for the upper portion of the language dialog
  (the-racket-language "Racket语言")
  (choose-a-language "选择语言")
  
  ;; the next two string constants appear in the
  ;; language dialog with a list
  ;; of example languages appearing between them
  (racket-language-discussion "你的程序将从#lang开始，从而指定想用的方言。例如：\n")
  (racket-language-discussion-end "\n……诸如此类")
  
  ;; the next three string constants are put into a message-box dialog
  ;; that appears when the user clicks on the example #lang languages
  ;; in the language dialog. The first one always appears and then either
  ;; the second or the third appears. The second one has the clicked
  ;; on #lang line placed into the ~a, and third one has the 
  ;; current #lang line in the first ~a and the clicked on in the second one.
  ;; The two comments are separated by a blank line.
  (racket-dialect-in-buffer-message 
   "Racket的方言一般而言由源代码指定，而不是语言对话框这里的选项指定。")
  (racket-dialect-add-new-#lang-line "这就是说，需要在定义视窗开头添加“~a”吗？")
  (racket-dialect-replace-#lang-line "这就是说，在你的文件里现在有“~a”；需要将其替换成“~a”吗？")
  (racket-dialect-already-same-#lang-line "不过你的文件中已经包含“~a”；你可以开始编写程序了！")
  
  ;; in the dialog containing the above strings, one of these is a button that appears
  (add-#lang-line "添加#lang行")
  (replace-#lang-line "替换#lang行")
  
  ;; for the 'new drracket user' dialog
  (use-language-in-source "使用代码中指定的语言")
  
  ;;; from the `not a language language' used initially in drscheme.
  (must-choose-language "在继续操作之前，你必须为DrRacket选择一种编程语言。")
  
  ; next two appear before and after the name of a text book (which will be in italics)
  (using-a-textbook-before "使用")
  (using-a-textbook-after "？")
  
  ; next two are before and after a language
  (start-with-before "由")
  (start-with-after "开始？")
  
  (seasoned-plt-schemer? "PLT Scheme高手?")
  (racketeer? "你是Racketeer吗？")
  (looking-for-standard-scheme? "想要标准的Scheme?")
  
  ; the three string constants are concatenated together and the middle
  ; one is hyperlinked to the dialog that suggests various languages
  (get-guidance-before "请使用“语言”菜单中的“选择语言”对话框，或者")
  (get-guidance-during "由DrRacket帮助你选择")
  (get-guidance-after "。")
  
  ;;; debug language
  (unknown-debug-frame "[未知]")
  (backtrace-window-title "向后跟踪—DrRacket")
  (files-interactions "~a的交互") ;; filled with a filename
  (current-interactions "交互")
  (current-definitions "定义")
  (mzscheme-w/debug "文本（MzScheme，包含R5RS）")
  (mzscheme-one-line-summary "PLT的Scheme实现")
  (mred-w/debug "图形（MrEd，包含MzScheme）")
  (mred-one-line-summary "在MzScheme的基础上增加GUI支持")
  
  ;; profiling
  (profiling-low-color "低")
  (profiling-high-color "高")
  (profiling-choose-low-color "请选择代表低的颜色")
  (profiling-choose-high-color "请选择代表高的颜色")
  (profiling "性能分析")
  (profiling-example-text "(define (马) (马))")
  (profiling-color-config "性能分析色谱")
  (profiling-scale "性能分析的色彩比例")
  (profiling-sqrt "平方根")
  (profiling-linear "线性")
  (profiling-square "平方")
  (profiling-number "调用次数")
  (profiling-time "累积时间")
  (profiling-update "更新性能分析")
  (profiling-col-percent-time "%次")
  (profiling-col-function "函数")
  (profiling-col-time-in-msec "毫秒")
  (profiling-col-calls "调用")
  (profiling-show-profile "显示性能分析")
  (profiling-hide-profile "隐藏性能分析")
  (profiling-unknown-src "《未知》")
  (profiling-no-information-available "没有可用的性能分析信息。请确定在语言设置中启用了性能分析，并且运行了当前程序。")
  (profiling-clear? "改变定义视窗的内容将导致性能分析信息失效。是否继续？")
  
  ;; test coverage
  (test-coverage-clear? "改变定义视窗将导致测试覆盖信息失效。是否继续？")
  (test-coverage-clear-and-do-not-ask-again "是，并且不再询问")
  (test-coverage-ask? "询问清除测试覆盖")
  
  (test-coverage-on "覆盖的测试")
  (test-coverage-off "未覆盖的测试")
  
  ;; tracing
  (tracing-enable-tracing "启用跟踪")
  (tracing-show-tracing-window "显示跟踪")
  (tracing-hide-tracing-window "隐藏跟踪")
  (tracing-tracing-nothing-to-show "暂时没有可用的跟踪结果。(请检查你所使用的语言是否支持跟踪以及是否启用了跟踪。)")
  
  ;;; repl stuff
  (evaluation-terminated "计算已终止")
  (evaluation-terminated-explanation
   "计算线程已停止，在下一次执行之前不会进行计算。")
  
  ; The next three constants show up in the same dialog as the above evaluation-terminated string
  ; constants.
  ; The first two show up only when the user calls 'exit' (possibly with a status code).
  ; The third shows up when the program runs out of memory.
  (exited-successfully "成功退出。")
  (exited-with-error-code "退出，错误代码~a。") ;; ~a is filled in with a number between 1 and 255
  (program-ran-out-of-memory "内存耗尽。")
  
  (show-evaluation-terminated-dialog "显示‘计算终止’对话框")
  (evaluation-terminated-ask "下次再显示该对话框")
  
  (last-stack-frame "显示最新的栈帧")
  (last-stack-frames "显示前~a个栈帧")
  (next-stack-frames "显示后~a个栈帧")
  
  ;;; welcoming message in repl
  (language "语言")
  (custom "自定义")
  (teachpack "教学包")
  (welcome-to "欢迎使用")
  (version "版本")
  
  ;;; kill evaluation dialog
  (kill-evaluation? "是否要终止计算？")
  (just-break "中断")
  (kill "终止")
  (kill? "终止？")
  
  ;;; version checker
  (version:update-menu-item   "检查更新…")
  (version:update-check       "检查更新") ; dialog title, with the next line
  (version:connecting-server  "连接Racket版本服务器")
  (version:results-title      "Racket版本检查")
  (version:do-periodic-checks "定期检查Racket版本更新")
  (version:take-me-there      "下载") ; ...to the download website
  ;; the next one can appear alone, or followed by a comma and the one after that
  (version:plt-up-to-date     "您现在使用的已经是最新版本的Racket")
  (version:but-newer-alpha    "但是还有一个更新的alpha版本")
  ;; This is used in this context: "Racket vNNN <<<*>>> http://download..."
  (version:now-available-at   "可以从这里获取：")
  
  ;; insert menu
  (insert-menu "插入(&I)")
  
  ;; large semi colon letters
  (insert-large-letters... "插入大字…")
  (large-semicolon-letters "带分号的大字")
  (text-to-insert "要插入的文字")
  
  (module-browser-filename-format "文件全名：~a（共~a行）")
  (module-browser-root-filename "根文件名：~a")
  (module-browser-font-size-gauge-label "字体大小")
  (module-browser-progress-label "Module概览进程")
  (module-browser-adding-file "添加文件：~a…")
  (module-browser-laying-out-graph-label "正在布局")
  (module-browser-open-file-format "打开~a")
  (module-browser "Module浏览器") ;; frame title
  (module-browser... "&Module浏览器…") ;; menu item title
  (module-browser-in-file "M&odule浏览~a") ;; menu item title; ~a is filled with a filename
  (module-browser-no-file "Module浏览存盘文件") ;; menu item title for above menu item; used when there is no saved file
  (module-browser-error-expanding "展开程序时出错：\n\n~a")
  (module-browser-show-lib-paths "显示通过(lib ..)加载的文件的路径")
  (module-browser-progress "Module浏览器：~a") ;; prefix in the status line
  (module-browser-compiling-defns "Module浏览器：正在编译定义")
  (module-browser-show-lib-paths/short "跟随lib调用") ;; check box label in show module browser pane in drscheme window.
  (module-browser-show-planet-paths/short "跟随planet调用") ;; check box label in show module browser pane in drscheme window.
  (module-browser-refresh "刷新") ;; button label in show module browser pane in drscheme window.
  (module-browser-highlight "高亮显示") ;; used to search in the graph; the label on a text-field% object
  (module-browser-only-in-plt-and-module-langs
   "Module浏览器只能对基于module的程序中使用。")
  (module-browser-name-length "名称长度")
  (module-browser-name-short "短")
  (module-browser-name-medium "中")
  (module-browser-name-long "长")
  (module-browser-name-very-long "长，包含阶段")  ;; like 'Long' but shows the phases where this file is loaded
  (module-browser-open-all "打开所有这些文件")
  
  (happy-birthday-matthias "生日快乐，Matthias！")
  (happy-birthday-matthew "生日快乐，马晓！")
  (happy-birthday-shriram "生日快乐，Shriram！")
  
  (mrflow-using-default-language-title "正在使用默认语言")
  (mrflow-using-default-language "当前使用的语言并不包含其原素的类型。改用R5RS Scheme。")
  (mrflow-button-title "分析")
  ;(mrflow-unknown-style-delta-error-title "Unknown Box Style Delta")
  ;(mrflow-unknown-style-delta-error "Unknown box style delta: ~a")
  (mrflow-popup-menu-show-type "显示类型")
  (mrflow-popup-menu-hide-type "隐藏类型")
  (mrflow-popup-menu-show-errors "显示错误")
  (mrflow-popup-menu-hide-errors "隐藏错误")
  ;(mrflow-read-exception-title "Read Exception")
  ;(mrflow-read-exception "Read exception: ~a")
  ;(mrflow-syntax-exception-title "Syntax Exception")
  ;(mrflow-syntax-exception "Syntax exception: ~a")
  ;(mrflow-unknown-exception-title "Unknown Exception")
  ;(mrflow-unknown-exception "Unknown exception: ~a")
  ;(mrflow-language-primitives-error-title "Language Primitives Error")
  ;(mrflow-language-primitives-error "Wrong filename for language primitives types table: ~a")
  
  (snips-and-arrows-popup-menu-tack-all-arrows "固定所有箭头")
  (snips-and-arrows-popup-menu-untack-all-arrows "取消固定所有箭头")
  (snips-and-arrows-user-action-disallowed-title "当前不允许用户改变")
  (snips-and-arrows-user-action-disallowed
   "在编辑器中包含由系统插入的段落，所以不允许用户改变。在修改之前请先隐藏所有这些段落。")
  ;(snips-and-arrows-changing-terms-warning-title "Changing terms will be undoable")
  (snips-and-arrows-hide-all-snips-in-editor "在编辑器中隐藏所有段落")
  
  (xml-tool-insert-xml-box "插入XML框")
  (xml-tool-insert-scheme-box "插入Racket框")
  (xml-tool-insert-scheme-splice-box "插入Racket接合框")
  (xml-tool-xml-box "XML框")
  (xml-tool-scheme-box "Racket框")
  (xml-tool-scheme-splice-box "Racket接合框")
  (xml-tool-switch-to-scheme "转化为Racket框")
  (xml-tool-switch-to-scheme-splice "转化为Racket接合框")
  (xml-tool-eliminate-whitespace-in-empty-tags
   "消除空白标签中的空白")
  (xml-tool-leave-whitespace-alone
   "保留空白")
  
  (show-recent-items-window-menu-item "在单独视窗中显示最近使用过的文件")
  (show-recent-items-window-label "最近使用过的文件")
  (number-of-open-recent-items "最近使用的数量")
  (switch-anyway "强制切换文件")
  
  (stepper-program-has-changed "注意：程序已改变。")
  (stepper-program-window-closed "注意：程序视窗已关闭。")
  
  (stepper-name "单步执行器")
  (stepper-language-level-message "单步执行不支持语言“~a”。")
  (stepper-button-label "单步执行")
  
  (stepper-previous "上一步")
  (stepper-next "下一步")
  (stepper-jump "跳至…")
  (stepper-jump-to-beginning "最前")
  (stepper-jump-to-end "最后")
  (stepper-jump-to-selected "当前选中的开始")
  (stepper-jump-to-previous-application "前一个调用步骤")
  (stepper-jump-to-next-application "下一个调用步骤")
  (stepper-out-of-steps "已到达计算终结，未找到目标步骤。")
  (stepper-no-such-step/title "未找到步骤")
  (stepper-no-such-step "找不到符合该标准的步骤。")
  (stepper-no-such-step/earlier "前向找不到符合该标准的步骤。")
  
  (stepper-no-earlier-application-step "前向无调用步骤。")
  (stepper-no-later-application-step "无调用步骤。")
  
  (stepper-no-earlier-step "前向无步骤。")
  (stepper-no-later-step "无步骤。")
  
  (stepper-no-selected-step "选中区域中没有步骤。可能这些是注释？")
  
  (stepper-no-last-step "最后步骤暂未生成。")
  
  
  (debug-tool-button-name "调试")
  
  (dialog-back "后退")
  
  ;; warnings about closing a drscheme frame when the program
  ;; might still be doing something interesting
  (program-is-still-running "定义视窗中的程序还在运行中。强制退出？")
  (program-has-open-windows "定义视窗中的程序打开了其他视窗。强行关闭这些视窗？")
  
  ;; ml-command-line-arguments is for the command line arguments
  ;; label in the module language details in the language dialog.
  (ml-command-line-arguments "命令行参数是字符串的数组，以红色显示")
  
  ;; ml-cp names are all for the module language collection path
  ;; configuration. See the details portion of the language dialog
  ;; for the module language (at the bottom).
  (ml-cp-default-collection-path "<<默认collection路径>>")
  
  ;; in std get-directory 
  (ml-cp-choose-a-collection-path "请选择collection路径")
  
  ;; err msg when adding default twice
  (ml-cp-default-already-present
   "默认collection路径已存在")
  
  ;; title of this section of the dialog (possibly the word
  ;; `Collection' should not be translated)
  (ml-cp-collection-paths "Collection路径")
  
  ;; button labels
  (ml-cp-add "添加")
  (ml-cp-add-default "加为默认")
  (ml-cp-remove "清除")
  (ml-cp-raise "上移")
  (ml-cp-lower "下移")
  
  (ml-always-show-#lang-line "在Module语言中，始终显示#langhang")
  
  ;; Strings for Profj removed as ProfessorJ is no longer actively developed as part of DrScheme
  
  
  ;;The Test engine tool
  ;;
  (test-engine-window-title "测试结果")
  ;;Following two appear in View menu, attach and free test report window from DrRacket frame
  (test-engine-dock-report "停靠测试报告")
  (test-engine-undock-report "取消停靠测试报告")
  ;;Following two appear in Racket (Java, etc) menu, cause Tests to be Run automatically or not
  (test-engine-enable-tests "启用测试")
  (test-engine-disable-tests "停用测试")
  
  (test-engine-ran-1-test "运行了1个test。")
  (test-engine-ran-1-check "运行了1个check。")
  ;; ditto, only plural
  (test-engine-ran-n-tests "运行了~a个test。")
  (test-engine-ran-n-checks "运行了~a个check。")
  (test-engine-1-test-passed "Test通过！")
  (test-engine-1-check-passed "Check通过！")
  (test-engine-both-tests-passed "全部test通过！")
  (test-engine-both-checks-passed "全部check通过！")
  (test-engine-all-tests-passed "全部test通过！")
  (test-engine-all-checks-passed "全部check通过！")
  (test-engine-all-n-tests-passed "所有~a个test通过！")
  (test-engine-all-n-checks-passed "所有~a个check通过！")
  (test-engine-0-tests-passed "0个test通过。")
  (test-engine-0-checks-passed "0个check通过。")
  (test-engine-m-of-n-tests-failed "~a个test失败（共~a个test）。")
  (test-engine-m-of-n-checks-failed "~a个check失败（共~a个check）。")
  (test-engine-must-be-tested "程序必须要测试！")
  (test-engine-is-unchecked "程序还没有check！")
  (test-engine-tests-disabled "测试未启用。")
  (test-engine-should-be-tested "程序需要测试。")
  (test-engine-at-line-column "于行~a，列~a")
  (test-engine-in-at-line-column "于文件~a，行~a，列~a")
  ; as in "column (unknown)"
  (test-engine-unknown "（未知）")
  (test-engine-trace-error "跟踪错误")
  
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
  (test-engine-property-error-error "check-property遇到如下错误~n:: ~a")
  
  (signature-enable-checks "启用Signature Checks")
  (signature-disable-checks "停用Signature Checks")
  
  ; section header
  (test-engine-check-failures "Check失败：")
  ; section header
  (test-engine-signature-violations "Signature违规：")
  
  ; part of one phrase "signature <at line ...> to blame: procedure <...>
  (test-engine-signature "signature")
  (test-engine-to-blame "to blame: procedure")
  
  (test-engine-no-signature-violations "无signature违规。")
  (test-engine-1-signature-violation "1个signature违规。")
  (test-engine-n-signature-violations "~a个signature违规。")
  
  ; as in got <value>, signature <at ...>
  (test-engine-got "实得")
  
  ;; The Test Suite Tool
  ;; Errors
  (test-case-empty-error "空白test")
  (test-case-too-many-expressions-error "一个test中包含过多表达式。")
  ;; DrRacket window menu items
  (test-case-insert "插入Test Case")
  (test-case-disable-all "禁用所有Test Cases")
  (test-case-enable-all "允许所有Test Cases")
  
  ;; NOTE: The following string constants are labels of the test-case fields. The width
  ;;       of the field is determined by the length of the longest of the following three words.
  ;;       if the words are too long the test case will take up too much horizontal room and
  ;;       not look very good.
  ;; This string is the label of the expression that is being tested in a test case.
  (test-case-to-test "Test")
  ;; This string is the label of the expression that is the expected value of the to-test expression.
  (test-case-expected "应该为")
  ;; This string is the label of the actual result of the to test expression.
  (test-case-actual "实际值")
  (test-case-predicate "预测值")
  (test-case-should-raise "应该Raise")
  ;; The label of a field of the test-case that describes the expected error message of a test case
  (test-case-error-message "错误信息")
  
  (test-case-menu-title "Test Case")
  (test-case-switch-to-error-box "切换为Error Test框")
  (test-case-switch-to-nonerror-box "切换为Nonerror Test框")
  (test-case-collapse "折叠Test Case")
  (test-case-show-actual "显示实际值")
  (test-case-enable "启用Test Case")
  (test-case-show-predicate "显示预测值")
  (test-case-show-error-message "显示错误信息")
  (test-case-convert-to-text "转化为文本")
  
  ;; Slideshow
  (slideshow-hide-picts "显示嵌套的框")
  (slideshow-show-picts "显示图片")
  (slideshow-cannot-show-picts "无法显示图片；请先运行程序建立大小")
  (slideshow-insert-pict-box "插入图片框")
  
  ;; GUI Tool
  (gui-tool-heading "GUI工具")
  (gui-tool-before-clicking-message "在点击工具图标之前，请先使用“特殊符号”菜单中的“插入GUI”命令插入一个GUI根对象，或先选中另一个GUI。")
  (gui-tool-show-gui-toolbar "显示GUI工具栏")
  (gui-tool-hide-gui-toolbar "隐藏GUI工具栏")
  (gui-tool-insert-gui "插入GUI")
  
  ;; contract violation tracking
  
  ; tooltip for new planet icon in drscheme window (must have a planet violation logged to see it)
  (show-planet-contract-violations "显示PLaneT中的contract违背")
  
  ; buttons in the dialog that lists the recorded bug reports
  (bug-track-report "发送报告")
  (bug-track-forget "取消")
  (bug-track-forget-all "取消全部")
  
  ;; planet status messages in the bottom of the drscheme window; the ~a is filled with the name of the package
  (planet-downloading "PLaneT：正在下载~a……")
  (planet-installing "PLaneT：正在安装~a……")
  (planet-finished "PLaneT：~a已完成。")
  (planet-docs-building "PLaneT：构建文档（由~a触发）……")
  (planet-no-status "PLaneT") ;; this can happen when there is status shown in a different and then the user switches to a tab where planet hasn't been used
  
  (bug-report-field-pkg "Package系统信息")
  
  ;; string normalization. To see this, paste some text with a ligature into DrRacket
  ;; the first three strings are in the dialog that appears. The last one is in the preferences dialog
  (normalize "标准化")
  (leave-alone "保留原样")
  (normalize-string-info "粘帖来的文字包含未经标准化的连字。标准化？")
  (normalize-string-preference "标准化粘帖字符串")
  (ask-about-normalizing-strings "询问是否标准化字符")
  
  (always-use-platform-specific-linefeed-convention "使用系统相关的换行符约定")
  
  ;; optimization coach
  (hide-optimization-coach "隐藏优化教练")
  (show-optimization-coach "显示优化教练")
  
  ;; labels used (in a big font) in the background of the definitions and interactions windows
  (definitions-window-label "定义")
  (interactions-window-label "交互")
  (hide-defs/ints-label "隐藏定义/交互标签") ;; popup menu
  (show-defs/ints-label "显示定义/交互标签") ;; preferences checkbox
  
  ;; menu item in the 'edit' menu; applies to editors with programs in them
  ;; (technically, editors that implement color:text<%>)
  (spell-check-string-constants "对字符串常量进行拼写检查")
  (spelling-dictionaries "拼写检查字典") ; (sub)menu whose items are the different possible dictionaries
  (default-spelling-dictionary "默认字典") ; first item in menu from previous line
  (misspelled-text-color "拼写错误文本的颜色") ;; in the preferences dialog  
  (cannot-find-ispell-or-aspell-path "找不到aspell或ispell程式文件")
  ; puts the path to the spell program in the ~a and then the error message
  ; is put following this string (with a blank line in between)
  (spell-program-wrote-to-stderr-on-startup "拼写程序（~a）给出错误信息：")
  )
