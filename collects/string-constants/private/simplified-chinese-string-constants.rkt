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
  ;; One generic version (ie, on the Quit DrRacket dialog)
  (dont-ask-again                "不再询问")
  
  ;;; important urls
  (web-materials "相关网站") ;; menu item title
  (tool-web-sites "Tools网站")   ;; menu item title
  (plt-homepage "Racket")
  (pbd-homepage "Program by Design")
  
  ;;; bug report form
  (cancel-bug-report? "取消程序错误报告？")
  (are-you-sure-cancel-bug-report?
   "你确定要取消报告程序错误吗？")
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
  (bug-report-submit-menu-item "提交程序错误报告...") ;; in Help Menu (drs & help desk)
  (error-sending-bug-report "程序错误报告传输出错")
  (error-sending-bug-report-expln "在传输程序错误报告的过程中出现了错误。如果你能够正常浏览网络，请访问：\n\n    http://bugs.racket-lang.org/\n\n使用网页上的表单提交程序错误报告。对于由此产生的不便，我们表示抱歉。\n\n传输错误详情：\n~a")
  (illegal-bug-report "非法的程序错误报告")
  (pls-fill-in-field "请填写\"~a\"栏目")
  (malformed-email-address "电子邮件地址不符合格式")
  (pls-fill-in-either-description-or-reproduce "在“详细描述”和“再现程序错误的步骤”两栏中，请至少填写一项。")
  
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
  (cs-status-coloring-program "语法检查：为程序着色")
  (cs-status-eval-compile-time "语法检查：编译程序")
  (cs-status-expanding-expression "语法检查：展开表达式")
  (cs-mouse-over-import "绑定~s由~s导入")
  
  (cs-view-docs "察看~a的文档")
  
  (cs-lexical-variable "lexical变量")
  (cs-imported-variable "imported的变量")
  
  ;;; info bar at botttom of drscheme frame
  (collect-button-label "垃圾回收")
  (read-only "只读")
  (auto-extend-selection "自动扩展")
  (overwrite "覆盖")
  (running "运行中")
  (not-running "静止中")
  
  ;;; misc
  (welcome-to-something "欢迎来到~a")
  
  ; this appears in the drscheme about box.
  (welcome-to-drscheme-version/language "欢迎使用DrRacket，版本~a，~a")
  
  ; these appear on subsequent lines in the `Help|Welcome to DrRacket' dialog.
  (welcome-to-drscheme "欢迎使用DrRacket")
  
  (goto-line "跳至...行")
  (goto-line-invalid-number
   "~a不是合法的行号。必须提供一个在1和~a之间的数字")
  (goto-position "跳至...位置")
  (no-full-name-since-not-saved
   "当前文件还没有被命名，因为从来没有对它进行保存。")
  (cannot-open-because-dne "无法打开~a，文件不存在。")
  
  (needs-execute-language-changed
   "警告：选择的语言改变了。请单击“运行”。")
  (needs-execute-teachpack-changed
   "警告：教学包改变了。请单击“运行”。")
  (needs-execute-defns-edited
   "警告：定义视窗改变了。请单击“运行”。")
  (file-is-not-saved "文件\"~a\"还没有保存过")
  
  (save "保存")
  (close-anyway "强制关闭")
  (dont-save "不保存")
  (clear-anyway "强制清空")
  
  ;; menu item title
  (log-definitions-and-interactions "记录定义和交互的日志...")
  (stop-logging "不再记录日志")
  (please-choose-a-log-directory "请选择日志目录")
  (logging-to "记录日志到：")
  (erase-log-directory-contents "删除日志目录~a中的内容？")
  (error-erasing-log-directory "删除日志出错。\n\n~a\n")
  
  ;; modes
  (mode-submenu-label "模式")
  (scheme-mode "Racket模式")
  (text-mode "文本模式")
  
  (scheme-mode-color-symbol "符号")
  (scheme-mode-color-keyword "关键词")
  (scheme-mode-color-comment "注释")
  (scheme-mode-color-string "字符串")
  (scheme-mode-color-constant "常量")
  (scheme-mode-color-parenthesis "括号")
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
  (plt:hd:search-for-keyword "关键词")
  (plt:hd:search-for-keyword-or-index "关键词或索引")
  (plt:hd:search-for-keyword-or-index-or-text "关键词、索引或普通文本")
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
  (plt:hd:refresh-downloading... "下载~a...")
  (plt:hd:refresh-deleting... "删除旧版本的~a...")
  (plt:hd:refresh-installing... "安装新版本的~a...")
  (plt:hd:refresh-clearing-indices "清除缓存中的索引")
  (plt:hd:refreshing-manuals-finished "完成。")
  (plt:hd:about-help-desk "关于Help Desk")
  (plt:hd:help-desk-about-string
   "Help Desk是PLT软件的信息来源，其中包含了DrRacket，MzScheme和MrEd的全部信息。\n\n版本~a\n版权所有(c)~a-~a PLT")
  (plt:hd:help-on-help "关于帮助的帮助")
  (plt:hd:help-on-help-details "关于使用Help Desk的帮助，请参见Help Desk主页中的第一个链接“Help Desk”。（要进入Help Desk的主页，请单击Help Desk视窗上方的“主页”按钮。）")
  (reload "刷新") ;; refresh the page in a web browser
  (plt:hd:ask-about-separate-browser
   "你选择了一个指向万维网的链接。请问您是要在Help Desk中打开该页面，还是想使用浏览器程序浏览网页？")
  (plt:hd:homebrew-browser "Help Desk") ;; choice for the above string (in a button)
  (plt:hd:separate-browser "网络浏览器") ;; other choice for the above string (also in a button)
  (plt:hd:external-link-in-help "在Help中的外部URL")
  (plt:hd:use-homebrew-browser "对于外部URL，使用Help Desk浏览")
  (plt:hd:new-help-desk "新的Help Desk视窗")
  
  ;; in the Help Desk language dialog, title on the right.
  (plt:hd:manual-search-ordering "搜索手册的顺序")
  
  ;; in the help-desk standalone font preference dialog, on a check box
  (use-drscheme-font-size "使用和DrRacket相同的字号")
  
  ;; in the preferences dialog in drscheme there is example text for help desk font size.
  ;; clicking the links in that text produces a dialog with this message
  (help-desk-this-is-just-example-text
   "这里显示的只是示例字体大小的文字。要察看这些链接，请通过帮助菜单打开真正的Help Desk。")
  
  ;; this appears in the bottom part of the frame the first time the user hits `f1' 
  ;; (assuming nothing else has loaded the documentation index first)
  ;; see also: cs-status-loading-docs-index
  (help-desk-loading-documentation-index "Help Desk：正在读入文档索引")
  
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
  (browser-cmdline-expl-line-1 "(命令行由前缀文字，URL和后缀文字") ; explanatory text for dialog, line 1
  (browser-cmdline-expl-line-2 "连接而成，中间不含任何空格)") ; ... line 2. (Anyone need more lines?)
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
  (about-drscheme-frame-title "关于DrRacket")
  
  ;;; save file in particular format prompting.
  (save-as-plain-text "保存本文件为纯文本？")
  (save-in-drs-format "保存本文件为drscheme(非纯文本)格式？")
  (yes "是")
  (no "否")
  
  ;; saving image (right click on an image to see the text)
  (save-image "保存图片...")
  
  ;;; preferences
  (preferences "参数设置")
  (error-saving-preferences "保存参数时出错:~a")
  (error-saving-preferences-title "保存参数时出错")
  (steal-the-lock-and-retry "取消锁定并重试") ;; in the preferences error dialog; this happens when the lockfile exists (after 3 pref writes).
  (error-reading-preferences "读取参数设置时出错")
  (prefs-file-locked "存储参数的文件被锁定了（由于文件~a的存在），所以这些参数无法被保存。放弃修改？")
  (try-again "重试") ;; button label
  (prefs-file-still-locked "存储参数的文件仍然被锁定（由于文件~a的存在）, 所以这些参数将不会被保存。")
  (scheme-prefs-panel-label "Racket")
  (warnings-prefs-panel-label "警告")
  (editor-prefs-panel-label "编辑")
  (general-prefs-panel-label "常规")
  (highlight-parens "加亮显示匹配的括号")
  (fixup-close-parens "自动调整闭括号")
  (fixup-open-brackets "自动调整中括号")
  (flash-paren-match "高亮显示括号匹配")
  (auto-save-files "自动保存文件")
  (backup-files "保存备份文件")
  (map-delete-to-backspace "将delete转换成backspace")
  (verify-exit "退出时确认")
  (ask-before-changing-format "改变保存方式时确认")
  (wrap-words-in-editor-buffers "在编辑器中自动换行")
  (show-status-line "显示状态栏")
  (count-columns-from-one "从1开始计算行号")
  (display-line-numbers "在编辑器中显示行号")
  (show-line-and-column-numbers "显示行号和列号") ; used for popup menu; right click on line/column box in bottom of drs window
  (show-character-offsets "显示字符在文件中的位置") ; used for popup menu; right click on line/column box in bottom of drs window
  (enable-keybindings-in-menus "允许使用菜单中的快捷键")
  (command-as-meta "将command键当作meta") ;; macos/macos x only
  (reuse-existing-frames "在打开新文件时，使用现有的视窗")
  (default-fonts "默认字体")
  (paren-match-color "高亮显示括号所使用的颜色") ; in prefs dialog
  (online-coloring-active "实时根据语法用颜色标记程序")
  (open-files-in-tabs "在不同的标签下打开多个文件（不使用多个视窗）")
  (show-interactions-on-execute "在运行程序时自动打开交互视窗")
  (switch-to-module-language-automatically "打开module文件时自动切换至module语言")
  (interactions-beside-definitions "将定义视窗和交互视窗左右放置") ;; in preferences, below the checkbox one line above this one
  (limit-interactions-size "限制交互视窗的大小")
  (background-color "背景颜色")
  (default-text-color "默认颜色") ;; used for configuring colors, but doesn't need the word "color"
  (choose-a-background-color "请选择背景颜色")
  (revert-to-defaults "恢复默认")
  
  (black-on-white-color-scheme "白底黑字") ;; these two appear in the color preferences dialog on butttons
  (white-on-black-color-scheme "黑底白字") ;; clicking the buttons changes the color schemes to some defaults that've been set up.
  
  ; title of the color choosing dialog
  
  ; should have entire alphabet
  (font-example-string "中文 by 朱崇恺") 
  
  (change-font-button-label "更改")
  (fonts "字体")
  (other... "其他...") ;; used in the font choice menu item
  
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
  (only-warn-once "当定义视窗和交互视窗不同步时，仅警告一次")
  
  ; warning message when lockfile is around
  (waiting-for-pref-lock "等待参数设置文件解锁...")
  (pref-lock-not-gone
   "参数设置封锁文件：\n\n ~a\n\n禁止保存参数设置。请确定没有其他Racket软件正在运行中，然后删除该封锁文件。")
  (still-locked-exit-anyway? "参数无法保存。仍然退出？")
  
  ;;; indenting preferences panel
  (indenting-prefs-panel-label "缩进")
  (indenting-prefs-extra-regexp "其他表达式")
  
  (square-bracket-prefs-panel-label "中括号")
  
  ; filled with define, lambda, or begin
  (enter-new-keyword "请输入一个类似于~a的关键词：")
  (x-keyword "~a关键词")
  (x-like-keywords "~a类型的关键词")
  
  ; used in Square bracket panel
  (skip-subexpressions "出现在中括号前的表达式数量")
  
  (expected-a-symbol "需要一个符号，得到a")
  (already-used-keyword "“~a”已经是缩进关键词了")
  (add-keyword "添加")
  (remove-keyword "删除")
  
  ; repl color preferences
  (repl-colors "REPL")
  (repl-out-color "输出")
  (repl-value-color "值")
  (repl-error-color "错误")
  
  ;;; find/replace
  (find-and-replace "查找并替换")
  (find "查找")
  (replace "替换")
  (dock "面板")
  (undock "对话框")
  (replace&find "替换并查找") ;;; need double & to get a single &
  (forward "下一个")
  (backward "上一个")
  (hide "隐藏")
  (find-case-sensitive "大小写敏感")  ;; the check box in both the docked & undocked search
  (find-anchor-based "用锚进行搜索")
  
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
  (mfs-drscheme-multi-file-search "多文件查找——DrRacket") ;; results window and error message title
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
  (get-file "Get文件")
  (put-file "Put文件")
  (full-pathname "完整路径")
  (show-dot-files "显示从点号开始的文件/目录名。")
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
  ;;; if they don't, DrRacket's menus will appear
  ;;; in the wrong order.
  (file-menu "文件")
  (edit-menu "编辑")
  (help-menu "帮助")
  (windows-menu "视窗")
  
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
  
  (open-recent-info "最近使用过文件的列表")
  (open-recent-menu-item "最近使用过的文件(&T)")
  
  (revert-info "将当前文件恢复为磁盘上的副本")
  (revert-menu-item "恢复(&R)")
  
  (save-info "保存当前文件")
  (save-menu-item "保存(&S)")
  
  (save-as-info "输入新的文件名,保存当前文件")
  (save-as-menu-item "另存为(&A)...")
  
  (print-info "打印当前文件")
  (print-menu-item "打印(&P)...")
  
  (page-setup-info "设置打印参数")
  (page-setup-menu-item "页面设置...")
  
  (close-info "关闭当前文件")
  (close-menu-item "关闭(&C)")
  
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
  (find-info "在主视窗和搜索栏之间切换光标位置")
  
  (find-again-info "跳至该文本的下一个出现")
  (find-again-menu-item "查找下一个")
  
  (find-again-backwards-info "跳至该文本的前一个出现")
  (find-again-backwards-menu-item "查找上一个")
  
  (replace-and-find-again-info "替换当前文本，同时查找下一个出现")
  (replace-and-find-again-menu-item "替换并查找下一个")
  
  (replace-and-find-again-backwards-info "替换当前文本，同时查找前一个出现")
  (replace-and-find-again-backwards-menu-item "替换并查找上一个")
  
  (replace-all-info "替换搜索字符串的全部出现")
  (replace-all-menu-item "全部替换")
  
  (find-case-sensitive-info "设置搜索大小写敏感或不敏感")
  (find-case-sensitive-menu-item "大小写敏感")
  
  (complete-word "自动完成") ; the complete word menu item in the edit menu
  (no-completions "... 无") ; shows up in the completions menu when there are no completions (in italics)
  
  (preferences-info "设置控制参数")
  (preferences-menu-item "参数设置...")
  
  (keybindings-info "显示当前热键绑定")
  (keybindings-menu-item "热键绑定")
  (keybindings-show-active "显示热键绑定")
  (keybindings-frame-title "热键绑定")
  (keybindings-sort-by-name "按名称排序")
  (keybindings-sort-by-key "按键名排序")
  (keybindings-add-user-defined-keybindings "添加自定义热键绑定...")
  (keybindings-add-user-defined-keybindings/planet "从PLaneT添加自定义热键绑定...")
  (keybindings-menu-remove "移除~a")
  (keybindings-choose-user-defined-file "请选择一个包含热键绑定的文件")
  (keybindings-planet-malformed-spec "错误的PLaneT名称：~a") ; the string will be what the user typed in
  (keybindings-type-planet-spec "请输入正确的PLaneT名称（无需输入`require'）")
  
  ; first ~a will be a string naming the file or planet package where the keybindings come from;
  ; second ~a will be an error message
  (keybindings-error-installing-file "安装热键绑定~a时出错:\n\n~a")
  
  (user-defined-keybinding-error "热键绑定出错~a\n\n~a")
  (user-defined-keybinding-malformed-file "文件~a并不是一个按照framework/keybinding-lang语言编写的module.")  
  
  ;; menu items in the "special" menu
  (insert-text-box-item "插入文本框")
  (insert-image-item "插入图片...")
  (insert-comment-box-menu-item-label "插入注释框")
  (insert-lambda "插入λ")
  
  (wrap-text-item "自动换行")
  
  (windows-menu-label "视窗(&W)")
  (minimize "最小化") ;; minimize and zoom are only used under mac os x
  (zoom "缩放")
  (bring-frame-to-front "前端显示")       ;;; title of dialog
  (bring-frame-to-front... "前端显示...") ;;; corresponding title of menu item
  (most-recent-window "最近的视窗")
  
  (view-menu-label "视图(&V)")
  (show-overview "显示程序轮廓") 
  (hide-overview "隐藏程序轮廓")
  (show-module-browser "显示module浏览器")
  (hide-module-browser "隐藏module浏览器")
  
  (help-menu-label "帮助(&H)")
  (about-info "本软件的详细信息以及致谢名单")
  (about-menu-item "关于...")
  
  ;; open here's new menu item
  (create-new-window-or-clear-current
   "您是想打开一个新视窗，还是清空当前视窗？")
  (clear-current "清空当前")
  (new-window "新视窗")
  
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
   "要保存你所作的更改吗？")
  (overwrite-file-button-label "保存")
  
  (definitions-modified 
    "当前磁盘文件已被修改；请保存或恢复文件。")
  (drscheme-internal-error "DrRacket内部错误")
  
  ;;; tools
  (invalid-tool-spec "Collection ~a中info.rkt的tool定义不正确。需要一个字符串或者一个非空表，得到：~e")
  (error-invoking-tool-title "调用tool ~s出错；~s")
  (tool-tool-names-same-length
   "在~s的info.rkt文件中，“tool-names”和“tools”应该是等长的表，得到~e和~e")
  (tool-tool-icons-same-length
   "在~s的info.rkt文件中，“tool-icons”和“tools”应该是等长的表，得到~e和~e")
  (tool-tool-urls-same-length
   "在~s的info.rkt文件中，“tool-urls”和“tools”应该是等长的表，得到~e和~e")
  (error-getting-info-tool
   "载入~s的info.rkt出错")
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
  (definitions-menu-item-help-string "显示/隐藏定义视窗")
  (show-interactions-menu-item-label "显示交互(&I)")
  (hide-interactions-menu-item-label "隐藏交互(&I)")
  (interactions-menu-item-help-string "显示/隐藏交互视窗")
  (toolbar "工具栏")
  (toolbar-on-top "顶置工具栏")
  (toolbar-on-left "左置工具栏")
  (toolbar-on-right "右置工具栏")
  (toolbar-hidden "隐藏工具栏")
  
  ;;; file menu
  (save-definitions-as "将定义另存为(&A)")
  (save-definitions "保存定义")
  (print-definitions "打印定义...")
  (about-drscheme "关于DrRacket")
  (save-other "其他保存方式")
  (save-definitions-as-text "将定义保存为文本...")
  (save-interactions "保存交互")
  (save-interactions-as "将交互另存为...")
  (save-interactions-as-text "将交互保存为文本...")
  (print-interactions "打印交互...")
  (new-tab "新建标签")
  (close-tab "关闭标签") ;; must not have any &s in it.
  (close-tab-amp "关闭标签(&C)") ;; like close-tab, but with an ampersand on the same letter as the one in close-menu-item
  
  ;;; edit-menu
  (split-menu-item-label "分屏(&S)")
  (collapse-menu-item-label "合并(&O)")
  
  ;;; language menu
  (language-menu-name "语言(&L)")
  
  ;;; scheme-menu
  (scheme-menu-name "Ra&cket")
  (execute-menu-item-label "运行")
  (execute-menu-item-help-string "运行定义视窗中的程序")
  (ask-quit-menu-item-label "中断")
  (ask-quit-menu-item-help-string "使用break-thread中止当前计算得主线程")
  (force-quit-menu-item-label "终止")
  (force-quit-menu-item-help-string "使用custodian-shutdown-all退出当前计算")
  (limit-memory-menu-item-label "限制内存使用...")
  ;(limit-memory-msg-1 "内存限制会在下一次运行")
  ;(limit-memory-msg-2 "时生效。内存限制最低值为1megabyte.") ;; memory limit is now 8 megabytes
  (limit-memory-unlimited "无限制")
  (limit-memory-limited "限制")
  (limit-memory-megabytes "Megabytes")
  (clear-error-highlight-menu-item-label "清除错误高亮显示")
  (clear-error-highlight-item-help-string "清除错误区域的粉红色高亮显示")
  (reindent-menu-item-label "调整缩进(&R)")
  (reindent-all-menu-item-label "全文调整缩进(&A)")
  (semicolon-comment-out-menu-item-label "用分号注释(&C)")
  (box-comment-out-menu-item-label "用注释框注释(&C)")
  (uncomment-menu-item-label "取消注释(&U)")
  
  (convert-to-semicolon-comment "转化为（分号）注释")
  
  ;;; executables
  (create-executable-menu-item-label "创建可执行程序...")
  (create-executable-title "创建可执行程序")
  (must-save-before-executable "在创建可执行程序之前，你必须保存源程序")
  (save-a-mred-launcher "保存为GRacket程序")
  (save-a-mzscheme-launcher "保存为Racket程序")
  (save-a-mred-stand-alone-executable "保存为GRacket可执行程序")
  (save-a-mzscheme-stand-alone-executable "保存为Racket可执行程序")
  (save-a-mred-distribution "保存为GRacket可发布程序")
  (save-a-mzscheme-distribution "保存为Racket可发布程序")
  
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
  
  (distribution-progress-window-title "发布进程")
  (creating-executable-progress-status "创建可执行程序...")
  (assembling-distribution-files-progress-status "汇编...")
  (packing-distribution-progress-status "打包...")
  
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
  (collapse-sexp "折叠S表达式")
  (expand-sexp "扩展S表达式")
  
  ;;; fraction dialog
  (enter-fraction "输入分数")
  (whole-part "整数部分")
  (numerator "分子")
  (denominator "分母")
  (insert-number/bad-whole-part "必须输入一个整数")
  (insert-number/bad-numerator "分子必须是非负整数")
  (insert-number/bad-denominator "分母必须是正整数")
  (insert-fraction-menu-item-label "插入分数...")
  
  ;; number snip popup menu
  (show-decimal-expansion "用十进制表示")
  (show-mixed-fraction-view "用带分数表示")
  (show-improper-fraction-view "用假分数表示")
  (show-more-decimal-places "先是更多小数位")
  
  ;;; Teachpack messages
  (select-a-teachpack "选择教学包")
  (clear-teachpack "卸载教学包~a")
  (teachpack-error-label "DrRacket——教学包出错")
  (teachpack-didnt-load "无法装载教学包~a。")
  (add-teachpack-menu-item-label "加载教学包...")
  (clear-all-teachpacks-menu-item-label "卸载全部教学包")
  (drscheme-teachpack-message-title "DrRacket教学包")
  (already-added-teachpack "教学包~a已装载")
  
  ; ~a is filled with the teachpack's name; the message appears in the teachpack selection dialog when a user installs a new teachpack
  (compiling-teachpack "编译教学包~a...")
  (teachpack-pre-installed "自带的教学包")
  (teachpack-user-installed "用户安装的教学包")
  (add-teachpack-to-list... "添加教学包...")
  (teachpack-already-installed "已经存在一个名为'~a'的教学包。是否覆盖？")
  ; ~a is filled with a list of language names. Each name is separated by a newline and is indented two spaces (no commas, no 'and')
  (teachpacks-only-in-languages "教学抱仅在下列语言中有效：~a")
  
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
  (teachpacks "教学包") ;; label in the language dialog for the teaching languages
  (teachpacks-none "<< 无 >>") ;; shows up under the previous string, when there are no teachpacks
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
  (enforce-primitives-group-box-label "初始绑定")
  (enforce-primitives-check-box-label "不允许改变初始绑定")
  
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
  (legacy-languages "历代语言")
  (teaching-languages "教学语言")
  (experimental-languages "实验语言")
  (initial-language-category "初始语言")
  (no-language-chosen "还没有选择语言")
  
  ;(module-language-one-line-summary "运行程序将提供一个包含该module的REPL")
  
  ;;; from the `not a language language' used initially in drscheme.
  (must-choose-language "在继续操作之前，你必须为DrRacket选择一种语言。")
  
  ; next two appear before and after the name of a text book (which will be in italics)
  (using-a-textbook-before "使用")
  (using-a-textbook-after "（教科书）？")
  
  ; next two are before and after a language
  (start-with-before "由")
  (start-with-after "开始？")
  
  (seasoned-plt-schemer? "PLT Scheme高手?")
  (looking-for-standard-scheme? "标准的Scheme?")
  
  ; the three string constants are concatenated together and the middle
  ; one is hyperlinked to the dialog that suggests various languages
  (get-guidance-before "请通过“语言”菜单中的“选择语言”命名进行语言选择，或者")
  (get-guidance-during "由DrRacket帮助你选择")
  (get-guidance-after "。")
  
  ;;; debug language
  (unknown-debug-frame "[未知]")
  (backtrace-window-title "向后跟踪 - DrRacket")
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
  (profiling "Profiling")
  (profiling-example-text "(define (马) (马))")
  (profiling-color-config "Profiling色谱")
  (profiling-scale "Profiling的色彩比例")
  (profiling-sqrt "平方根")
  (profiling-linear "线性")
  (profiling-square "平方")
  (profiling-number "调用次数")
  (profiling-time "累积次数")
  (profiling-update "更新Profile")
  (profiling-col-percent-time "%次")
  (profiling-col-function "函数")
  (profiling-col-time-in-msec "毫秒")
  (profiling-col-calls "调用")
  (profiling-show-profile "显示Profile")
  (profiling-hide-profile "隐藏Profile")
  (profiling-unknown-src "<< 未知 >>")
  (profiling-no-information-available "没有可用的profiling信息。请确定你（在语言设置中）启用了profiling，并且运行了当前程序。")
  (profiling-clear? "改变定义视窗的内容将导致profiling信息失效。是否继续？")
  
  ;;The Test engine tool
  ;;
  (test-engine-window-title "测试结果")
  ;;Following two appear in View menu, attach and free test report window from DrRacket frame
  (test-engine-dock-report "在面板中显示测试报告")
  (test-engine-undock-report "独立显示测试报告")
  ;;Following two appear in Racket (Java, etc) menu, cause Tests to be Run automatically or not
  (test-engine-enable-tests "启用测试功能")
  (test-engine-disable-tests "停用测试功能")
  
  ;; test coverage
  (test-coverage-clear? "改变定义视窗将导致测试覆盖信息失效。是否继续？")
  (test-coverage-clear-and-do-not-ask-again "是，并且不再询问")
  (test-coverage-ask? "询问清除测试覆盖")
  
  ;; tracing
  (tracing-enable-tracing "启用跟踪")
  (tracing-show-tracing-window "显示跟踪")
  (tracing-hide-tracing-window "隐藏跟踪")
  (tracing-tracing-nothing-to-show "暂时没有可用的跟踪结果。(请检查你所使用的语言是否支持跟踪以及是否启用了跟踪。)")
  
  ;;; repl stuff
  (evaluation-terminated "计算已终止")
  (evaluation-terminated-explanation
   "Evaluation线程已停止，在下一次执行之前不会进行计算。")
  
  ; The next three constants show up in the same dialog as the above evaluation-terminated string
  ; constants.
  ; The first two show up only when the user calls 'exit' (possibly with a status code).
  ; The third shows up when the program runs out of memory.
  (exited-successfully "成功退出。")
  (exited-with-error-code "退出，错误代码~a。") ;; ~a is filled in with a number between 1 and 255
  (program-ran-out-of-memory "内存耗尽。")
  (last-stack-frame "显示最新的stack frame")
  (last-stack-frames "显示前~a个stack frames")
  (next-stack-frames "显示后~a个stack frames")
  
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
  (version:update-menu-item   "检查更新...")
  (version:update-check       "检查更新") ; dialog title, with the next line
  (version:connecting-server  "连接Racket版本服务器")
  (version:results-title      "Racket版本检查")
  (version:do-periodic-checks "自动定期检查Racket版本更新")
  (version:take-me-there      "下载") ; ...to the download website
  ;; the next one can appear alone, or followed by a comma and the one after that
  (version:plt-up-to-date     "您现在使用的已经是当前版的Racket")
  (version:but-newer-alpha    "但是还有一个更新的alpha版本")
  ;; This is used in this context: "Racket vNNN <<<*>>> http://download..."
  (version:now-available-at   "可以从这里获取：")
  
  ;; insert menu
  (insert-menu "插入(&I)")
  
  ;; large semi colon letters
  (insert-large-letters... "插入大字...")
  (large-semicolon-letters "带分号的大字")
  (text-to-insert "要插入的文字")
  
  (module-browser-filename-format "文件全名: ~a (共~a行)")
  (module-browser-root-filename "根文件名: ~a")
  (module-browser-font-size-gauge-label "字号")
  (module-browser-progress-label "Module overview progress")
  (module-browser-adding-file "添加文件: ~a...")
  (module-browser-laying-out-graph-label "正在为图布局")
  (module-browser-open-file-format "打开~a")
  (module-browser "Module浏览器") ;; frame title
  (module-browser... "Module浏览器...") ;; menu item title
  (module-browser-error-expanding "展开程序时出错：\n\n~a")
  (module-browser-show-lib-paths "显示通过(lib ..)加载的文件的路径")
  (module-browser-progress "Module浏览器：~a") ;; prefix in the status line
  (module-browser-compiling-defns "Module浏览器：正在编译定义")
  (module-browser-show-lib-paths/short "显示lib调用") ;; check box label in show module browser pane in drscheme window.
  (module-browser-show-planet-paths/short "显示planet调用") ;; check box label in show module browser pane in drscheme window.
  (module-browser-refresh "刷新") ;; button label in show module browser pane in drscheme window.
;  (module-browser-only-in-plt-and-module-langs
;   "Module浏览器只能在PLT语言和module语言(并且要求程序中有module)中使用。")
  (module-browser-name-length "名称长度")
  (module-browser-name-short "短")
  (module-browser-name-medium "中")
  (module-browser-name-long "长")
  (module-browser-open-all "打开所有这些文件")
  
  (happy-birthday-matthias "生日快乐，Matthias！")
  (happy-birthday-matthew "生日快乐，马晓！")
  (happy-birthday-shriram "生日快乐，Shriram！")
  
  (mrflow-using-default-language-title "正在使用默认语言")
  (mrflow-using-default-language "当前使用的语言并不包含定义primitive类型的标。改用R5RS Scheme。")
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
  
  (xml-tool-insert-xml-box "插入XML框")
  (xml-tool-insert-scheme-box "插入Racket框")
  (xml-tool-insert-scheme-splice-box "插入Racket接合框")
  (xml-tool-xml-box "XML框")
  (xml-tool-scheme-box "Racket框")
  (xml-tool-scheme-splice-box "Racket接合框")
  (xml-tool-switch-to-scheme "转变成Racket框")
  (xml-tool-switch-to-scheme-splice "转变成Racket接合框")
  
  (show-recent-items-window-menu-item "在单独视窗中显示最近使用的文件")
  (show-recent-items-window-label "最近使用的文件")
  (number-of-open-recent-items "最近使用项目的数量")
  (switch-anyway "Switch File Anyway")
  
  (stepper-program-has-changed "注意：程序已改变。")
  (stepper-program-window-closed "注意：程序视窗已关闭。")
  
  (stepper-name "单步执行器")
  (stepper-language-level-message "单步执行不支持语言“~a”。")
  (stepper-button-label "单步执行")
  (stepper-previous-application "调用")
  (stepper-previous "上一步")
  (stepper-next "下一步")
  (stepper-next-application "调用")
  (stepper-jump-to-beginning "源程序")
  (stepper-jump-to-end "最终运行结果")
  
  (debug-tool-button-name "调试")
  
  (dialog-back "后退")
  
  ;; warnings about closing a drscheme frame when the program
  ;; might still be doing something interesting
  (program-is-still-running "定义视窗中的程序还在运行中。强制退出？")
  (program-has-open-windows "定义视窗中的程序打开了其他视窗。强行关闭这些视窗？")
  
  ;; Profj
  (profj-insert-java-comment-box "插入Java注释框")
  (profj-insert-java-interactions-box "插入Java交互框")
  
  ;; The Test Suite Tool
  ;; DrRacket window menu items
  (test-case-insert "插入Test Case")
  (test-case-disable-all "禁用所有Test Cases")
  (test-case-enable-all "允许所有Test Cases")
  
  ;; Slideshow
  (slideshow-show-slideshow-panel "显示Slideshow面板")
  (slideshow-hide-slideshow-panel "隐藏Slideshow面板")
  (slideshow-insert-pict-box "插入图片框")
  
  ;; GUI Tool
  (gui-tool-heading "GUI工具")
  (gui-tool-before-clicking-message "在点击工具图标之前，请先使用“特殊符号”菜单中的“插入GUI”命令插入一个GUI根对象，或先选中另一个GUI。")
  (gui-tool-show-gui-toolbar "显示GUI工具栏")
  (gui-tool-hide-gui-toolbar "隐藏GUI工具栏")
  (gui-tool-insert-gui "插入GUI")

  ;; contract violation tracking
  
  ; tooltip for new planet icon in drscheme window (must have a planet violation logged to see it)
  (show-planet-contract-violations "显示PLaneT中的违背contract")

  (ml-always-show-#lang-line "在Module语言中,总是显示#lang行")
  
  ; buttons in the dialog that lists the recorded bug reports
  (bug-track-report "File Ticket")
  (bug-track-forget "Forget")
  (bug-track-forget-all "Forget All")
  )
