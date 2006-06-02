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
(module simplified-chinese-string-constants "string-constant-lang.ss"
  (is-this-your-native-language "你的母语是简体中文吗？")
  
  (are-you-sure-you-want-to-switch-languages
   "为了改变界面语言，现在需要重新启动DrScheme。你确定吗？")
  
  (interact-with-drscheme-in-language "使用简体中文作为DrScheme界面语言")
  
  ;; these two should probably be the same in all languages excepet English.
  ;; they are the button labels (under macos and windows, respectively)
  ;; that go the with the string above.
  (accept-and-quit "接受并退出")
  (accept-and-exit "接受并退出")
  
  ;;; general purpose (DrScheme is hereby a word in every language, by decree of Robby :)
  (plt "PLT")
  (drscheme "DrScheme")
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
  (stop "停止")   
  (&stop "停止(&S)") ;; for use in button and menu item labels, with short cut.
  (are-you-sure-delete? "确定要删除~a吗？") ;; ~a is a filename or directory name
  (ignore "忽略")
  (revert "复原")
  
  ;; label for a generic check box, often supported on dialogs
  ;; that ask a binary choice of the user. If checked, the
  ;; dialog isn't going to be shown again.
  ;; One version for always using the current choice:
  (dont-ask-again-always-current "不再询问(总是使用当前设置)")
  ;; One generic version (ie, on the Quit DrScheme dialog)
  (dont-ask-again                "不再询问")
  
  ;;; important urls
  (web-materials "相关网站") ;; menu item title
  (tool-web-sites "Tools网站")   ;; menu item title
  (drscheme-homepage "DrScheme")
  (plt-homepage "PLT")
  (how-to-use-scheme "How to Use Scheme") ;; title of a book.
  (teachscheme!-homepage "TeachScheme!") ;; probably this should be a `word' in all languages
  
  ;;; bug report form
  (cancel-bug-report? "取消故障报告？")
  (are-you-sure-cancel-bug-report?
   "你确定要取消报告故障吗？")
  (bug-report-form "故障报告表")
  (bug-report-field-name "姓名")
  (bug-report-field-email "电子邮件")
  (bug-report-field-summary "标题")
  (bug-report-field-severity "严重度")
  (bug-report-field-class "类别")
  (bug-report-field-description "详细描述")
  (bug-report-field-reproduce1 "再现故障")
  (bug-report-field-reproduce2 "的步骤")
  (bug-report-field-environment "环境")
  (bug-report-field-docs-installed "已安装文档")
  (bug-report-field-collections "Collections")
  (bug-report-field-human-language "自然语言")
  (bug-report-field-version "版本")
  (bug-report-synthesized-information "综合信息")  ;; dialog title
  (bug-report-show-synthesized-info "显示综合信息")
  (bug-report-submit "提交")
  (bug-report-submit-menu-item "提交故障报告") ;; in Help Menu (drs & help desk)
  (error-sending-bug-report "故障报告传输出错")
  (error-sending-bug-report-expln "在传输故障报告的过程中出现了错误。如果你能够正常浏览网络，请访问：\n\n    http://bugs.plt-scheme.org/\n\n使用网页上的表单提交错误报告。对于由此产生的不便，我们表示抱歉。\n\n传输错误详情：\n~a")
  (illegal-bug-report "非法的故障报告")
  (pls-fill-in-field "请填写\"~a\"栏目")
  (malformed-email-address "电子邮件地址不符合格式")
  (pls-fill-in-either-description-or-reproduce "在“详细描述”和“再现故障的步骤”两栏中，请至少填写一项。")
  
  ;;; check syntax
  (check-syntax "检查语法")
  (cs-italic "斜体")
  (cs-bold "黑体")
  (cs-underline "下划线")
  (cs-change-color "改变颜色")
  (cs-tack/untack-arrow "附加/取消 箭头")
  (cs-jump-to-next-bound-occurrence "下一个被绑定出现")
  (cs-jump-to-binding "绑定出现")
  (cs-jump-to-definition "定义")
  (cs-error-message "出错信息")
  (cs-open-file "打开~a")
  (cs-rename-var "重命名~a")
  (cs-rename-id "重命名标识符")
  (cs-rename-var-to "将~a重命名为：")
  (cs-name-duplication-error "你所选择的新名称~s与当前辖域内现有标识符相同。")
  (cs-rename-anyway "强制重命名")
  (cs-status-init "语法检查：为用户代码初始化环境")
  (cs-status-coloring-program "语法检查：用颜色标注表达式")
  (cs-status-eval-compile-time "语法检查：编译时eval")
  (cs-status-expanding-expression "语法检查：扩展表达式")
  (cs-mouse-over-import "绑定~s由~s导入")
  
  (cs-lexical-variable "词汇变量")
  (cs-imported-variable "导入变量")
  
  ;;; info bar at botttom of drscheme frame
  (collect-button-label "垃圾收集")
  (read-only "只读")
  (read/write "读/写")
  (auto-extend-selection "自动扩展")
  (overwrite "覆盖")
  (running "运行中")
  (not-running "停止中")
  
  ;;; misc
  (welcome-to-something "欢迎来到~a")
  
  ; this appears in the drscheme about box.
  (welcome-to-drscheme-version/language "欢迎使用DrScheme，版本~a，~a")
  
  ; these appear on subsequent lines in the `Help|Welcome to DrScheme' dialog.
  (welcome-to-drscheme "欢迎使用DrScheme")
  
  (goto-line "跳至...行")
  (goto-line-invalid-number
   "~a不是合法的行号。必须提供一个在1和~a之间的数字")
  (goto-position "跳至...位置")
  (no-full-name-since-not-saved
   "当前文件还没有被命名，因为从来没有对它进行保存。")
  (cannot-open-because-dne "无法打开~a，文件不存在。")
  (interactions-out-of-sync
   "警告：交互窗口和定义窗口不同步。请单击“运行”按钮。")
  (file-is-not-saved "文件\"~a\"还没有保存过")
  (save "保存")
  (close-anyway "强制关闭")
  (clear-anyway "强制清空")
  
  ;; menu item title
  (log-definitions-and-interactions "记录定义和交互的日至...")
  (stop-logging "不再记录日至")
  (please-choose-a-log-directory "请选择日志目录")
  (logging-to "记录日至到：")
  (erase-log-directory-contents "删除日至目录~a中的内容？")
  (error-erasing-log-directory "删除日至出错。\n\n~a\n")
  
  ;; modes
  (mode-submenu-label "模式")
  (scheme-mode "Scheme模式")
  (text-mode "文本模式")
  
  (scheme-mode-color-symbol "符号")
  (scheme-mode-color-keyword "关键字")
  (scheme-mode-color-comment "注释")
  (scheme-mode-color-string "字符串")
  (scheme-mode-color-constant "常量")
  (scheme-mode-color-parenthesis "括号")
  (scheme-mode-color-error "错误")
  (scheme-mode-color-other "其他")
  ;; the ~a is filled in with one of the above (scheme-mode-*)
  (syntax-coloring-choose-color "为~a选择颜色")
  (preferences-colors "颜色") ;; used in the preferences dialog
  
  (url: "URL:")
  (open-url... "打开URL...")
  (open-url "打开URL")
  (browse... "浏览...")
  (bad-url "错误的URL")
  (bad-url:this "错误的URL: ~a")
  
  ;; Help Desk
  (help "帮助")
  (help-desk "Help Desk")
  (plt:hd:search "搜索")
  (plt:hd:feeling-lucky "手气不错")
  (plt:hd:home "Help Desk首页") 
  ; next 3 are popup menu choices in help desk search frame
  (plt:hd:search-for-keyword "关键字")
  (plt:hd:search-for-keyword-or-index "关键字或索引")
  (plt:hd:search-for-keyword-or-index-or-text "关键字、索引或普通文本")
  (plt:hd:exact-match "精确匹配")
  (plt:hd:containing-match "包含")
  (plt:hd:regexp-match "正则表达式匹配")
  (plt:hd:find-docs-for "搜索：")
  (plt:hd:search-stopped-too-many-matches "[搜索中断：过多的匹配结果]")
  (plt:hd:nothing-found-for "找不到任何关于~a的信息")
  (plt:hd:and "并且")
  (plt:hd:refresh "更新")
  (plt:hd:refresh-all-manuals "更新所有手册")
  (plt:hd:manual-installed-date "(~a已安装)")
  ; Help Desk configuration
  ;; refreshing manuals
  (plt:hd:refreshing-manuals "重新下载手册")
  (plt:hd:refresh-downloading... "正在下载~a...")
  (plt:hd:refresh-deleting... "删除旧版本的~a...")
  (plt:hd:refresh-installing... "安装新版本的~a...")
  (plt:hd:refresh-clearing-indicies "清除缓存中的索引")
  (plt:hd:refreshing-manuals-finished "完成。")
  (plt:hd:about-help-desk "关于Help Desk")
  (plt:hd:help-desk-about-string
   "Help Desk是PLT软件的信息来源，其中包含了DrScheme，MzScheme和MrEd的全部信息。\n\n版本~a\n版权所有(c)1995-2006 PLT")
  (plt:hd:help-on-help "关于帮助的帮助")
  (plt:hd:help-on-help-details "如果你需要使用Help Desk的帮助，请在Help Desk的主页中点击链接“How to use Help Desk”。（要进入Help Desk的主页，请单击Help Desk窗口上方的“主页”按钮。）")
  (reload "刷新") ;; refresh the page in a web browser
  (plt:hd:ask-about-separate-browser
   "你选择了一个指向万维网的链接。请问您是要在Help Desk中打开该页面，还是想使用浏览器程序浏览网页？")
  (plt:hd:homebrew-browser "Help Desk") ;; choice for the above string (in a button)
  (plt:hd:separate-browser "网络浏览器") ;; other choice for the above string (also in a button)
  (plt:hd:external-link-in-help "在Help中的外部URL")
  (plt:hd:use-homebrew-browser "对于外部URL，使用Help Desk浏览")
  (plt:hd:new-help-desk "新的Help Desk窗口")
  
  ;; in the Help Desk language dialog, title on the right.
  (plt:hd:manual-search-ordering "搜索手册的顺序")
  
  ;; in the help-desk standalone font preference dialog, on a check box
  (use-drscheme-font-size "使用和DrScheme相同的字号")
  
  ;; Help desk htty proxy
  (http-proxy "HTTP代理")
  (proxy-direct-connection "直接连接")
  (proxy-use-proxy "使用代理服务器：")
  (proxy-host "地址")
  (proxy-port "端口")
  (proxy-bad-host "非法的代理服务器")
  
  ;; browser
  (rewind-in-browser-history "后退")
  (forward-in-browser-history "前进")
  (home "主页")
  (browser "浏览器")
  (external-browser-choice-title "外部浏览器") ; title for radio-button set
  (browser-command-line-label "命令行：") ; label for radio button that is followed by text boxes
  (choose-browser "选择浏览器")
  (no-browser "以后再询问")
  (browser-cmdline-expl-line-1 "(命令行由pre-text，URL和post-text连接而成，") ; explanatory text for dialog, line 1
  (browser-cmdline-expl-line-2 "中间不含任何空格)") ; ... line 2. (Anyone need more lines?)
  (install? "安装？")  ;; if a .plt file is found (title of dialog)
  (you-have-selected-an-installable-package "你选择了一个可以安装的软件包。")
  (do-you-want-to-install-it? "是否安装？")
  (paren-file-size "(该文件的长度是~a字节)")
  (download-and-install "下载并安装") ;; button label
  (download "下载") ;; button label
  (save-downloaded-file/size "下载文件(~a字节)并保存为") ;; label for get-file dialog
  (save-downloaded-file "下载文件并保存为")  ;; label for get-file dialog
  (downloading "下载中") ;; dialog title
  (downloading-file... "下载文件中...")
  (package-was-installed "安装已完成。")
  (download-was-saved "文件已保存。")
  
  (install-plt-file-menu-item... "安装.plt文件...")
  (install-plt-file-dialog-title "安装.plt文件")
  (install-plt-web-tab "网络文件")
  (install-plt-file-tab "本地文件")
  (install-plt-filename "文件名：")
  (install-plt-url "URL:")
  
  ;; install plt file when opened in drscheme strings
  (install-plt-file "安装~a，还是打开以供编辑？")
  (install-plt-file/yes "安装")
  (install-plt-file/no "编辑")
  
  (plt-installer-progress-window-title "安装程序") ;; frame title
  (plt-installer-abort-installation "取消安装") ;; button label
  (plt-installer-aborted "安装中止。") ;; msg that appears in the installation window when installation is aborted
  
  ;;; about box
  (about-drscheme-frame-title "关于DrScheme")
  (take-a-tour "教程")
  (release-notes "发行记录")
  
  
  ;;; save file in particular format prompting.
  (save-as-plain-text "保存本文件为纯文本？")
  (save-in-drs-format "保存本文件为drscheme(非纯文本)格式？")
  (yes "是")
  (no "否")
  
  ;;; preferences
  (preferences "参数设置")
  (error-saving-preferences "保存参数时出错:~a")
  (error-reading-preferences "读取参数设置时出错")
  (scheme-prefs-panel-label "Scheme")
  (warnings-prefs-panel-label "警告")
  (editor-prefs-panel-label "编辑")
  (general-prefs-panel-label "常规")
  (highlight-parens "加亮显示匹配的括号")
  (fixup-open-parens "自动调整开括号")
  (fixup-close-parens "自动调整闭括号")
  (flash-paren-match "高亮显示括号匹配")
  (auto-save-files "自动保存文件")
  (backup-files "保存备份文件")
  (map-delete-to-backspace "将delete转换成backspace")
  (verify-exit "退出时确认")
  (ask-before-changing-format "改变保存方式时确认")
  (wrap-words-in-editor-buffers "在编辑器缓存中自动换行")
  (show-status-line "显示状态行")
  (count-columns-from-one "从一开始计算行号")
  (display-line-numbers "在缓冲区中显示行号和列号")
  (enable-keybindings-in-menus "允许使用菜单中的快捷键")
  (automatically-to-ps "自动打印成postscript文件")
  (option-as-meta "将option键当作meta") ;; macos/macos x only
  (separate-dialog-for-searching "使用单独的搜索对话框")
  (reuse-existing-frames "在打开新文件时，使用现有的框架")
  (default-fonts "默认字体")
  (paren-match-color "高亮显示括号所使用的颜色") ; in prefs dialog
  (online-coloring-active "实时根据语法用颜色标记程序")
  (open-files-in-tabs "在不同的标签下打开多个文件（不使用多个窗口）")
  (show-interactions-on-execute "在运行程序时自动打开交互窗口")
  (limit-interactions-size "限制交互窗口的大小")
  (background-color "背景颜色")
  (default-text-color "默认颜色") ;; used for configuring colors, but doesn't need the word "color"
  (choose-a-background-color "请选择背景颜色")
  
  ; title of the color choosing dialog
  
  ; should have entire alphabet
  (font-example-string "简体中文 by 朱崇恺") 
  
  (change-font-button-label "更改")
  (fonts "字体")
  
  ; filled with type of font, eg modern, swiss, etc.
  (choose-a-new-font "请选择一种新的“~a”字体")
  
  (font-size-slider-label "字号")
  (restart-to-see-font-changes "重新启动，使修改生效")
  
  (font-prefs-panel-title "字体")
  (font-name "字体")
  (font-size "字号")
  (set-font "设置字体...")
  (font-smoothing-label  "字体平滑度设置")
  (font-smoothing-none "无")
  (font-smoothing-some "部分")
  (font-smoothing-all "全部")
  (font-smoothing-default "使用系统默认")
  (select-font-name "选择字体")
  (example-text "示例文字")
  (only-warn-once "当定义窗口和交互窗口不同步时，仅警告一次")
  
  ; warning message when lockfile is around
  (waiting-for-pref-lock "等待参数设置文件解锁...")
  (pref-lock-not-gone
   "参数设置封锁文件：\n\n ~a\n\n禁止保存参数设置。请确定没有其他PLT软件正在运行中，然后删除该封锁文件。")
  (still-locked-exit-anyway? "参数无法保存。仍然退出？")
  
  ;;; indenting preferences panel
  (indenting-prefs-panel-label "缩进")
  (indenting-prefs-extra-regexp "其他表达式")
  
  ; filled with define, lambda, or begin
  (enter-new-keyword "请输入一个类似于~a的关键字：")
  (x-keyword "~a关键字")
  (x-like-keywords "~a类型的关键字")
  
  (expected-a-symbol "需要一个符号，得到a")
  (already-used-keyword "“~a”已经是缩进关键字了")
  (add-keyword "添加")
  (remove-keyword "删除")
  
  ;;; find/replace
  (find-and-replace "查找并替换")
  (find "查找")
  (replace "替换")
  (dock "面板")
  (undock "对话框")
  (replace&find-again "替换并查找下一个") ;;; need double & to get a single &
  (replace-to-end "全部替换")
  (forward "下一个")
  (backward "上一个")
  (hide "隐藏")
  
  ;;; multi-file-search
  (mfs-multi-file-search-menu-item "在文件中搜索...")
  (mfs-string-match/graphics "字符串匹配(可用与包含图像的文件)")
  (mfs-regexp-match/no-graphics "正则表达式匹配(只适用于纯文本文件)")
  (mfs-searching... "搜索...")
  (mfs-configure-search "搜索设置") ;; dialog title
  (mfs-files-section "文件")   ;; section in config dialog
  (mfs-search-section "搜索") ;; section in config dialog
  (mfs-dir "目录")
  (mfs-recur-over-subdirectories "包含子目录")
  (mfs-regexp-filename-filter "文件名筛选(正则表达式)")
  (mfs-search-string "查找字符串")
  (mfs-drscheme-multi-file-search "DrScheme——多文件查找") ;; results window and error message title
  (mfs-not-a-dir "\"~a\"不是目录")
  (mfs-open-file "打开文件")
  (mfs-stop-search "停止搜索")
  (mfs-case-sensitive-label "大小写敏感")
  (mfs-no-matches-found "没有找到匹配结果。")
  (mfs-search-interrupted "搜索中止。")
  
  ;;; reverting a file
  (are-you-sure-revert
   "你确定要恢复这个文件吗？这一操作无法撤销。")
  (are-you-sure-revert-title
   "恢复？")
  
  ;;; saving a file
  ; ~a is filled with the filename
  (error-saving "无法保存") ;; title of error message dialog
  (error-saving-file/name "在保存文件~a时出现错误。")
  (error-loading "无法读取")
  (error-loading-file/name "在读取~a时出现错误.")
  (unknown-filename "<<未知>>")
  
  ;;; finder dialog
  (must-specify-a-filename "你必须指定一个文件名")
  (file-does-not-exist "文件\"~a\"不存在。")
  (ask-because-file-exists "文件\"~a\"已存在。是否替换？")
  (dne-or-cycle "文件\"~a\"中包含一个不存在的目录，或者一个循环")
  (get-file "Get file")
  (put-file "Put file")
  (full-pathname "完整路径")
  (show-dot-files "显示点号开始文件/目录名。")
  (up-directory-button-label "上层目录")
  (add-button-label "添加") ;;; for multi-file selection
  (add-all-button-label "全部添加") ;;; for multi-file selection
  (remove-button-label "移除") ;;; for multi-file selection
  (file-wrong-form "该文件名格式不正确")
  (select-files "选择多个文件")
  (select-file "选择单个文件")
  (dir-dne "该目录不存在。")
  (file-dne "该文件不存在。")
  (empty-filename "文件名中必须包含文字。")
  (that-is-dir-name "这是一个目录的名字。")
  
  ;;; raw menu names -- these must match the 
  ;;; versions below, once the &s have been stripped.
  ;;; if they don't, DrScheme's menus will appear
  ;;; in the wrong order.
  (file-menu "文件")
  (edit-menu "编辑")
  (help-menu "帮助")
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
  
  (file-menu-label "文件(&F)")
  
  (new-info  "新建文件")
  (new-menu-item "新建(&N)")
  (new-...-menu-item "新建(&N)...")
  
  (open-info "打开现有文件")
  (open-menu-item "打开(&O)...")
  (open-here-menu-item "从这里打开(&O)...")
  
  (open-recent-info "最近使用过文件的列表")
  (open-recent-menu-item "最近使用过的文件")
  
  (revert-info "将当前文件恢复为磁盘上的副本")
  (revert-menu-item "恢复(&R)")
  
  (save-info "保存当前文件")
  (save-menu-item "保存(&S)")
  
  (save-as-info "输入新的文件名,保存当前文件")
  (save-as-menu-item "另存为(&A)...")
  
  (print-info "打印当前文件")
  (print-menu-item "打印(&P)...")
  
  (close-info "关闭当前文件")
  (close-menu-item "关闭(&C)")
  
  (quit-info "关闭所有窗口")
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
  
  (find-info "搜索某个字符串")
  (find-menu-item "查找...")
  
  (find-again-info "继续搜索该字符串")
  (find-again-menu-item "查找下一个")
  
  (replace-and-find-again-info "替换当前文本，然后继续查找原字符串")
  (replace-and-find-again-menu-item "替换并查找下一个")
  
  (preferences-info "设置控制参数")
  (preferences-menu-item "参数设置...")
  
  (keybindings-info "显示当前热键绑定")
  (keybindings-menu-item "热键绑定")
  (keybindings-show-active "显示热键绑定")
  (keybindings-frame-title "热键绑定")
  (keybindings-sort-by-name "按名称排序")
  (keybindings-sort-by-key "按键名排序")
  (keybindings-add-user-defined-keybindings "添加自定义热键绑定...")
  (keybindings-menu-remove "取消~a")
  (keybindings-choose-user-defined-file "请选择一个包含热键绑定的文件")
  
  (user-defined-keybinding-error "热键绑定出错~a\n\n~a")
  (user-defined-keybinding-malformed-file "文件~a并不是一个按照(lib \"keybinding-lang.ss\" \"framework\")语言编写的module.")  
  
  ;; menu items in the "special" menu
  (insert-text-box-item "插入文本框")
  (insert-image-item "插入图片...")
  (insert-comment-box-menu-item-label "插入注释框")
  (insert-lambda "插入λ")
  
  (wrap-text-item "自动换行")
  
  (windows-menu-label "窗口(&W)")
  (bring-frame-to-front "前端显示")       ;;; title of dialog
  (bring-frame-to-front... "前端显示...") ;;; corresponding title of menu item
  (most-recent-window "最近的窗口")
  
  (view-menu-label "视图(&V)")
  (show-overview "显示程序轮廓") 
  (hide-overview "隐藏程序轮廓")
  (show-module-browser "显示module浏览器")
  (hide-module-browser "隐藏module浏览器")
  
  (help-menu-label "帮助(&H)")
  (about-info "本程序的详细信息以及致谢名单")
  (about-menu-item "关于...")
  
  ;; open here's new menu item
  (create-new-window-or-clear-current
   "您是想打开一个新窗口，还是清空当前窗口？")
  (clear-current "清空当前")
  (new-window "新窗口")
  
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
  (error-autosaving "自动保存为\"~a\"时出错。") ;; ~a will be a filename
  (autosaving-turned-off "在一个文件没有被手工保存之间，自动保存也不会进行")
  (recover-autosave-files-frame-title "从自动保存中恢复")
  (autosave-details "详细情况")
  (autosave-recover "恢复")
  (autosave-unknown-filename "<<未知>>")
  
  ;; these are labels in a dialog that drscheme displays
  ;; if you have leftover autosave files. to see the dialog,
  ;; start up drscheme and modify (but don't save) a file
  ;; (also, do this with an unsaved file). Wait for the autosave
  ;; files to appear (typically 5 minutes). Kill DrScheme
  ;; and restart it. You'll see the dialog
  (autosave-autosave-label: "自动保存文件：")
  (autosave-original-label: "原始文件：")
  (autosave-autosave-label "自动保存文件")
  (autosave-original-label "原始文件")
  (autosave-compare-files "比较自动保存文件")
  
  (autosave-show-autosave "自动保存文件") ;; title of a window showing the autosave file
  
  (autosave-explanation "DrScheme发现了自动保存的文件，其中可能包含你没有保存过的程序")
  
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
   "要保存你所作的更改吗？")
  (overwrite-file-button-label "保存")
  
  (definitions-modified 
   "当前磁盘文件已被修改；请保存或恢复文件。")
  (drscheme-internal-error "DrScheme内部错误")
  
  ;;; tools
  (invalid-tool-spec "Collection ~a中info.ss的tool定义不正确。需要一个字符串或者一个非空表，得到：~e")
  (error-invoking-tool-title "调用tool ~s出错；~s")
  (tool-tool-names-same-length
   "在~s的info.ss文件中，“tool-names”和“tools”应该是等长的表，得到~e和~e")
  (tool-tool-icons-same-length
   "在~s的info.ss文件中，“tool-icons”和“tools”应该是等长的表，得到~e和~e")
  (tool-tool-urls-same-length
   "在~s的info.ss文件中，“tool-urls”和“tools”应该是等长的表，得到~e和~e")
  (error-getting-info-tool
   "载入~s的info.ss出错")
  (tool-error-phase1 "tool ~s第一阶段出错;~s")
  (tool-error-phase2 "tool ~s第二阶段出错;~s")
  
  
  ;;; define popup menu
  (end-of-buffer-define "<<缓冲区结束>>")
  (sort-by-name "按名字排序")
  (sort-by-position "按文件中的位置排序")
  (no-definitions-found "<<没有任何定义>>")
  (jump-to-defn "跳至~a的定义")
  
  (recent-items-sort-by-age "按时间排序")
  (recent-items-sort-by-name "按名字排序")
  
  ;;; view menu
  (hide-definitions-menu-item-label "隐藏定义(&D)")
  (show-definitions-menu-item-label "显示定义(&D)")
  (definitions-menu-item-help-string "显示/隐藏定义窗口")
  (show-interactions-menu-item-label "显示交互(&I)")
  (hide-interactions-menu-item-label "隐藏交互(&I)")
  (interactions-menu-item-help-string "显示/隐藏交互窗口")
  (show-toolbar "显示工具栏(&T)")
  (hide-toolbar "隐藏工具栏(&T)")
  
  ;;; file menu
  (save-definitions-as "将定义另存为(&A)")
  (save-definitions "保存定义")
  (print-definitions "打印定义...")
  (about-drscheme "关于DrScheme")
  (save-other "其他保存方式")
  (save-definitions-as-text "将定义保存为文本...")
  (save-interactions "保存交互")
  (save-interactions-as "将交互另存为...")
  (save-interactions-as-text "将交互保存为文本...")
  (print-interactions "打印交互...")
  (new-tab "新建标签")
  (close-tab "关闭标签") ;; must not have any &s in it.
  
  ;;; edit-menu
  (split-menu-item-label "分屏(&S)")
  (collapse-menu-item-label "合并(&O)")
  
  ;;; language menu
  (language-menu-name "语言(&L)")
  
  ;;; scheme-menu
  (scheme-menu-name "S&cheme")
  (execute-menu-item-label "运行")
  (execute-menu-item-help-string "运行定义窗口中的程序")
  (break-menu-item-label "中断")
  (break-menu-item-help-string "中断当前计算")
  (kill-menu-item-label "终止")
  (kill-menu-item-help-string "终止当前计算")
  (clear-error-highlight-menu-item-label "清除错误高亮显示")
  (clear-error-highlight-item-help-string "清除错误区域的粉红色高亮显示")
  (reindent-menu-item-label "调整缩进(&R)")
  (reindent-all-menu-item-label "全文调整缩进(&A)")
  (semicolon-comment-out-menu-item-label "用分号注释(&C)")
  (box-comment-out-menu-item-label "用注释框注释(&C)")
  (uncomment-menu-item-label "取消注释(&U)")
  
  (convert-to-semicolon-comment "转化为分号注释")
  
  ;;; executables
  (create-executable-menu-item-label "创建可执行程序...")
  (create-executable-title "创建可执行程序")
  (must-save-before-executable "在创建可执行程序之前，你必须保存源程序")
  (save-a-mred-launcher "保存为MrEd程序")
  (save-a-mzscheme-launcher "保存为MzScheme程序")
  (save-a-mred-stand-alone-executable "保存为MrEd可执行程序")
  (save-a-mzscheme-stand-alone-executable "保存为MzScheme可执行程序")
  
  (definitions-not-saved "当前定义窗口中的程序并没有被保存过。将使用最近保存过的版本来生成可执行程序。继续？")
  (launcher "启动程序")
  (stand-alone "独立的")
  (executable-type "类型")
  (executable-base "基")
  (filename "文件名：")
  (create "创建")
  ;; "choose-an-executable" changed to "specify-a"
  ; (please-choose-an-executable-filename "请选择可执行文件的名称。")
  ;; Replaced by generic ~a-must-end-with-~a
  ;(windows-executables-must-end-with-exe
  ; "文件名\n\n  ~a\n\n不合法。Windows可执行文件必须以.exe结尾。")
  ;(macosx-executables-must-end-with-app
  ; "文件名\n\n  ~a\n\n不合法。MacOS X可执行文件必须以.app结尾。")
  (warning-directory-will-be-replaced
   "警告：目录：\n\n  ~a\n\n将会被重置。继续操作？")
  
  (create-servlet "创建Servlet...")
  
  ; the ~a is a language such as "module" or "algol60"
  (create-servlet-unsupported-language
   "无法为~a语言程序创建Servlet。")
  
  ;;; buttons
  (execute-button-label "运行") 
  (save-button-label "保存")
  (break-button-label "停止")
  
  ;;; search help desk popup menu
  (search-help-desk-for "在Help Desk中搜索“~a”")
  (exact-lucky-search-help-desk-for "在Help Desk中搜索最符合“~a”的一个页面")
  
  ;; collapse and expand popup menu items
  (collapse-sexp "折叠sexpression")
  (expand-sexp "扩展sexpression")
  
  ;;; fraction dialog
  (enter-fraction "输入分数")
  (whole-part "整数部分")
  (numerator "分子")
  (denominator "分母")
  (invalid-number "无效的输入：必须输入一个精确的、不是整数的实数")
  (insert-fraction-menu-item-label "插入分数...")
  
  ;; number snip popup menu
  (show-decimal-expansion "用十进制表示")
  (show-mixed-fraction-view "用带分数表示")
  (show-improper-fraction-view "用假分数表示")
  (show-more-decimal-places "先是更多小数位")
  
  ;;; Teachpack messages
  (select-a-teachpack "选择教学包")
  (clear-teachpack "卸载教学包~a")
  (teachpack-error-label "DrScheme——教学包出错")
  (teachpack-didnt-load "无法装载教学包~a。")
  (add-teachpack-menu-item-label "加载教学包...")
  (clear-all-teachpacks-menu-item-label "卸载全部教学包")
  (drscheme-teachpack-message-title "DrScheme教学包")
  (already-added-teachpack "教学包~a已装载")
  
  ;;; Language dialog
  (introduction-to-language-dialog
   "请选择语言。大部分入门级的学生都可以使用默认语言。")
  (language-dialog-title "语言选择")
  (case-sensitive-label "大小写敏感")
  (output-style-label "输出格式")
  (constructor-printing-style "构造器")
  (quasiquote-printing-style "Quasiquote")
  (write-printing-style "write")
  (print-printing-style "current-print")
  (sharing-printing-label "Show sharing in values")
  (use-pretty-printer-label "print多个对象时自动换行")
  (input-syntax "输入语法")
  (dynamic-properties "Dynamic Properties")
  (output-syntax "输出语法")
  (no-debugging-or-profiling "No debugging or profiling")
  (debugging "Debugging")
  (debugging-and-profiling "Debugging and profiling")
  (test-coverage "Syntactic test suite coverage")
  (show-details-button-label "显示详情")
  (hide-details-button-label "隐藏详情")
  (choose-language-menu-item-label "选择语言...")
  (revert-to-language-defaults "恢复默认语言设置")
  (fraction-style "分数格式")
  (use-mixed-fractions "带分数")
  (use-repeating-decimals "循环小数")
  (decimal-notation-for-rationals "使用十进制表示有理数")
  (please-select-a-language "请选择语言")
  
  
  ;;; languages
  (beginning-student "初级")
  (beginning-one-line-summary "define、cond、结构体、常量和基本操作")
  (beginning-student/abbrev "初级+缩写的表")
  (beginning/abbrev-one-line-summary "在初级的基础上，用缩写形式输出表")
  (intermediate-student "中级")
  (intermediate-one-line-summary "在初级的基础上增加词法作用域")
  (intermediate-student/lambda "中级+lambda")
  (intermediate/lambda-one-line-summary "在中级的基础上，增加高阶函数")
  (advanced-student "高级")
  (advanced-one-line-summary "在中级的基础上，增加lambda和赋值")
  (how-to-design-programs "程序设计方法/How to Design Programs") ;; should agree with MIT Press on this one...
  (pretty-big-scheme "Pretty Big (包括MrEd和高级)")
  (pretty-big-scheme-one-line-summary "Adds syntax and functions from the HtDP languages")
  (r5rs-lang-name "标准(R5RS)")
  (r5rs-one-line-summary "Scheme语言标准第5修改稿")
  (expander "Expander")
  (expander-one-line-summary "Expands, rather than evaluates, expressions")
  (professional-languages "正式语言")
  (teaching-languages "教学语言")
  (experimental-languages "实验语言")
  
  (module-language-one-line-summary "Run creates a REPL in the context of the module, including the module's declared language")
  
  
  ;;; debug language
  (unknown-debug-frame "[unknown]")
  (backtrace-window-title "Backtrace - DrScheme")
  (files-interactions "~a's interactions") ;; filled with a filename
  (current-interactions "interactions")
  (current-definitions "definitions")
  (mzscheme-w/debug "Textual (MzScheme, 包含R5RS)")
  (mzscheme-one-line-summary "PLT的Scheme实现")
  (mred-w/debug "Graphical (MrEd, 包含 MzScheme)")
  (mred-one-line-summary "在MzScheme的基础上增加GUI支持")
  
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
  (version:update-menu-item "检查更新...")
  
  ;; special menu
  (special-menu "特殊符号(&P)")
  
  ;; large semi colon letters
  (insert-large-letters... "插入大字...")
  (large-semicolon-letters "带分号的大字")
  (text-to-insert "要插入的文字")
  
  (module-browser-filename-format "文件全名: ~a (共~a行)")
  (module-browser-root-filename "根文件名: ~a")
  (module-browser-font-size-gauge-label "字号")
  (module-browser-progress-label "Module overview progress")
  (module-browser-adding-file "添加文件: ~a...")
  (module-browser-laying-out-graph-label "Laying out graph")
  (module-browser-open-file-format "打开~a")
  (module-browser "Module浏览器") ;; frame title
  (module-browser... "Module浏览器...") ;; menu item title
  (module-browser-error-expanding "Error expanding the program:\n\n~a")
  (module-browser-show-lib-paths "Show files loaded by (lib ..) paths")
  (module-browser-progress "Module浏览器：~a") ;; prefix in the status line
  (module-browser-compiling-defns "Module浏览器：compiling definitions")
  (module-browser-show-lib-paths/short "Follow lib requires") ;; check box label in show module browser pane in drscheme window.
  (module-browser-refresh "Refresh") ;; button label in show module browser pane in drscheme window.
  (module-browser-only-in-plt-and-module-langs
   "Module浏览器只能在PLT语言和module语言(并且要求程序中有module)中使用。")
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
  (xml-tool-switch-to-scheme "转变成Scheme框")
  (xml-tool-switch-to-scheme-splice "转变成Scheme接合框")
  (xml-tool-eliminate-whitespace-in-empty-tags
   "Eliminiate whitespace in empty tags")
  (xml-tool-leave-whitespace-alone
   "Leave whitespace alone")
  
  (show-recent-items-window-menu-item "在单独窗口中显示最近使用的文件")
  (show-recent-items-window-label "最近使用的文件")
  (number-of-open-recent-items "Number of recent items")
  (switch-anyway "Switch File Anyway")
  
  (stepper-program-has-changed "注意：程序已改变。")
  (stepper-program-window-closed "注意：程序窗口已关闭。")
  
  (stepper-home "还原")
  (stepper-name "单步执行器")
  (stepper-language-level-message
   "您选择的语言是“~a”。目前，stepper只支持“~a”和“~a”之间的语言。")
  (stepper-button-label "单步执行")
  (stepper-previous-application "|< 调用")
  (stepper-previous "< 上一步")
  (stepper-next "下一步 >")
  (stepper-next-application "调用 >|")
  
  (debug-tool-button-name "调试")
  
  (dialog-back "后退")
  
  ;; warnings about closing a drscheme frame when the program
  ;; might still be doing something interesting
  (program-is-still-running "定义窗口中的程序还在运行中。强制退出？")
  (program-has-open-windows "定义窗口中的打开了其他窗口。强行关闭这些窗口？")
  
  ;; Profj
  (profj-java "Java")
  (profj-java-mode "Java模式")
  (profj-java-mode-color-keyword "关键字")
  (profj-java-mode-color-string "字符串")
  (profj-java-mode-color-literal "文字")
  (profj-java-mode-color-comment "注释")
  (profj-java-mode-color-error "错误")
  (profj-java-mode-color-identifier "标示符")
  (profj-java-mode-color-default "默认值")
  
  (profj-insert-java-comment-box "插入Java注释框")
  (profj-insert-java-interactions-box "插入Java交互框")
  
  ;; The Test Suite Tool
  ;; Errors
  (test-case-empty-error "Empty test case")
  (test-case-too-many-expressions-error "Too many expressions in a test case.")
  ;; Dr. Scheme window menu items
  (test-case-insert "插入Test Case")
  (test-case-disable-all "禁用所有Test Cases")
  (test-case-enable-all "允许所有Test Cases")
  
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
  (slideshow-show-slideshow-panel "显示Slideshow面板")
  (slideshow-hide-slideshow-panel "隐藏Slideshow面板")
  (slideshow-freeze-picts "Freeze These Picts")
  (slideshow-thaw-picts "Show Picts Under Mouse")
  (slideshow-hide-picts "Show Nested Boxes")
  (slideshow-show-picts "Show Picts")
  (slideshow-cannot-show-picts "Cannot show picts; run program to cache sizes first")
  (slideshow-insert-pict-box "插入Pict框") 
  
  ;; GUI Tool
  (gui-tool-heading "GUI工具")
  (gui-tool-before-clicking-message "在点击工具图标之前，请先使用“特殊符号”菜单中的“插入GUI”命令插入一个GUI根对象，或先选中另一个GUI。")
  (gui-tool-show-gui-toolbar "显示GUI工具栏")
  (gui-tool-hide-gui-toolbar "隐藏GUI工具栏")
  (gui-tool-insert-gui "插入GUI")
  )
