;;
;; Racket Korean Translation Team <racket-korean-translation@googlegroups.com>
;; or you can touch us from http://www.code-share.org
;;
;; Translated by: Jae sung Chung <abattery@gmail.com>,
;;                Jieung Kim <je1004k@gmail.com>,
;;                Yujeong Cho <mayflower418@gmail.com>,
;;                Sung Gyeong Bae <imai0917@gmail.com>
;;
(module korean-string-constants "string-constant-lang.rkt"
 (is-this-your-native-language "한글을 사용하시겠습니까?")

 (are-you-sure-you-want-to-switch-languages
  "GUI 사용 언어를 변경하기 위해 DrRacket을 다시 시작합니다. 변경하시겠습니까?")

 (interact-with-drscheme-in-language "한글로 DrRacket 사용하기")

 ;; these two should probably be the same in all languages excepet English.
 ;; they are the button labels (under macos and windows, respectively)
 ;; that go the with the string above.
 (accept-and-quit "확인하고 닫기")
 (accept-and-exit "확인하고 종료")
 
 ;;; general purpose (DrRacket is hereby a word in every language, by decree of Robby :)
 (plt "PLT")
 (drscheme "DrRacket")
 (ok "확인")
 (cancel "취소")
 (abort "중단")
 (untitled "제목없음")
 (untitled-n "제목없음 ~a")
 (warning "경고")
 (error "오류")
 (close "닫기") ;; as in, close an open window. must match close-menu-item
                 ;; in the sense that, when the &s have been stripped from
                 ;; close-menu-item, it must be the same string as this.
 (stop "중지")   
 (&stop "중지 (&S)") ;; for use in button and menu item labels, with short cut.
 (are-you-sure-delete? "정말 ~a (을)를 삭제하시겠습니까?") ;; ~a is a filename or directory name
 (ignore "무시")
 (revert "복구")

 ;; label for a generic check box, often supported on dialogs
 ;; that ask a binary choice of the user. If checked, the
 ;; dialog isn't going to be shown again.
 ;; One version for always using the current choice:
 (dont-ask-again-always-current "다시 묻지 않기 (항상 현재 설정을 사용합니다)")
 ;; One generic version (ie, on the Quit DrRacket dialog)
 (dont-ask-again                "다시 묻지 않기")

 ;;; important urls
 (web-materials "관련 사이트") ;; menu item title
 (tool-web-sites "참고 사이트")   ;; menu item title
 (plt-homepage "Racket")
 (pbd-homepage "Program by Design")

 ;;; bug report form
 (cancel-bug-report? "오류 보고를 취소하시겠습니까?")
 (are-you-sure-cancel-bug-report?
  "정말로 이 오류 보고를 취소하시겠습니까?")
 (bug-report-form "오류 보고 형식")
 (bug-report-field-name "이름")
 (bug-report-field-email "이메일")
 (bug-report-field-summary "요약")
 (bug-report-field-severity "심각 정도")
 (bug-report-field-class "분류")
 (bug-report-field-description "설명")
 (bug-report-field-reproduce1 "재구현")
 (bug-report-field-reproduce2 "방법")
 (bug-report-field-environment "환경")
 (bug-report-field-docs-installed "설치된 문서")
 (bug-report-field-collections "Collections")
 (bug-report-field-human-language "자연어")
  (bug-report-field-memory-use "메모리 사용량")
 (bug-report-field-version "버전")
 (bug-report-synthesized-information "세부 정보")  ;; dialog title
 (bug-report-show-synthesized-info "세부 정보 보기")
 (bug-report-submit "제출")
 (bug-report-submit-menu-item "오류 보고 제출...") ;; in Help Menu (drs & help desk)
 (error-sending-bug-report "오류 보고 보내기 오류")
 (error-sending-bug-report-expln "오류 보고 도중에 오류가 발생했습니다. 다른 인터넷 연결이 원활하다면, 다음 사이트를 방문하여:\n\n    http://bugs.racket-lang.org/\n\n 오류 보고를 온라인 형식으로
제출하여 주십시오. 불편을 드려 죄송합니다.\n\n 오류 메세지는 다음과 같습니다 :\n~a")
 (illegal-bug-report "형식에 맞지 않는 오류 보고")
 (pls-fill-in-field "\"~a\" 항목을 채워 주십시오")
 (malformed-email-address "잘못된 이메일 주소")
 (pls-fill-in-either-description-or-reproduce "설명 항목 또는 재구현 방법 항목을 채워 주십시오.")

 ;;; check syntax
 (check-syntax "문법 검사")
 (cs-italic "기울임")
 (cs-bold "굵게")
 (cs-underline "밑줄")
 (cs-change-color "색깔 바꾸기")
 (cs-tack/untack-arrow "화살표 보기/숨기기") ;;;확인하기!
 (cs-jump-to-next-bound-occurrence "다음 참조로 이동하기")
 (cs-jump-to-binding "원래 참조로 이동하기")
 (cs-jump-to-definition "정의로 이동하기")
 (cs-error-message "오류 메세지")
 (cs-open-file "~a 열기")
 (cs-rename-var "~a 이름 바꾸기")
 (cs-rename-id "식별자 이름바꾸기")
 (cs-rename-var-to "~a 을(를) 바꿀 이름:")
 (cs-name-duplication-error "새로 만든 이름 ~s 은(는) 이미 범위 안에 존재합니다.")
 (cs-rename-anyway "무시하고 계속")
 (cs-status-init "문법 검사: 사용자 코드로 환경 초기화")
 (cs-status-coloring-program "문법 검사: 표현 색칠하기") ;확인하기
 (cs-status-eval-compile-time "문법 검사: 컴파일 시간 계산")
 (cs-status-expanding-expression "문법 검사: 표현 확장")
 (cs-status-loading-docs-index "문법 검사: 문서 목차 가져오기")
 (cs-mouse-over-import "~s 에서 가져온 ~s 묶기")
 (cs-view-docs "~a 문서 보기")
 (cs-view-docs-from "~a 에서 온 ~a")  ;; a completed version of the line above (cs-view-docs) is put into the first ~a and a list of modules (separated by commas) is put into the second ~a. Use check syntax and right-click on a documented variable (eg, 'require') to see this in use
  
 (cs-lexical-variable "lexical 변수") ;확인하기
 (cs-set!d-variable "set!’d 변수")
 (cs-imported-variable "imported 변수")

 ;; mode sub-menu in the "view" menu
 (cs-check-syntax-mode "문법 검사 모드")
 (cs-mode-menu-show-my-obligations "내가 계획한 것에 대한 의무") ;확인하기
 (cs-mode-menu-show-client-obligations "남이 계획한 것에 대한 의무") ;확인하기
 (cs-mode-menu-show-syntax "문법적 분류")

 ;;; info bar at botttom of drscheme frame
 (collect-button-label "GC") ;garbage collector
  (read-only "읽기 전용")
 (auto-extend-selection "자동 확장")
 (overwrite "덮어쓰기")
 (running "실행 중")
 (not-running "정지") ;확인하기. 이상
 
 ;;; misc
 (welcome-to-something "~a 에 오신것을 환영합니다.")
 
 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "DrRacket 버전 ~a, ~a에 오신것을 환영합니다.")

 ; these appear on subsequent lines in the `Help|Welcome to DrRacket' dialog.
 (welcome-to-drscheme "DrRacket에 환영합니다.")

 (goto-line "줄 이동")
 (goto-line-invalid-number
  "~a 는 잘못된 줄 번호입니다. 줄 번호는 1과 ~a 사이여야 합니다.")
 (goto-position "위치 이동")
 (no-full-name-since-not-saved
  "파일을 저장하지 않아 이름이 존재하지 않습니다.")
 (cannot-open-because-dne "~a(은)는 존재하지 않으므로 열 수 없습니다.")

  (needs-execute-language-changed
   "경고: 언어가 바뀌었습니다. 실행 버튼을 눌러주세요.")
  (needs-execute-teachpack-changed
   "경고: 배움꾸러미가 바뀌었습니다. 실행 버튼을 눌러주세요.")
  (needs-execute-defns-edited
   "경고: 정의창이 바뀌었습니다. 실행 버튼을 눌러주세요.")
  
  (editor-changed-since-srcloc-recorded
   "저장소 기록 이후로 편집기가 바뀌었습니다. 명암처리 된 부분이 올바른 저장소와 일치하지 않을 수도 있습니다.")
  
 (file-is-not-saved "\"~a\" 파일이 저장되지 않았습니다.")
 (save "저장")
 (close-anyway "무시하고 닫기")
 (dont-save "저장 안함")
 (clear-anyway "무시하고 초기화") ;clear? 확인하기

 ;; menu item title
 (log-definitions-and-interactions "정의와 대화 기록하기")
 (stop-logging "기록 중지")
 (please-choose-a-log-directory "기록을 저장할 폴더를 선택하세요")
 (logging-to "기록 중: ")
 (erase-log-directory-contents "기록 폴더 ~a의 내용을 삭제하시겠습니까?")
 (error-erasing-log-directory "기록 폴더의 내용을 삭제하는 도중에 오류가 발생했습니다.\n\n~a\n")

  ;; menu items connected to the logger -- also in a button in the planet status line in the drs frame
  (show-log "보여주기 기록하기 (&L)")
  (hide-log "숨기기 기록하기 (&L)")
  (logging-all "모두") ;; in the logging window in drscheme, shows all logs simultaneously
  
 ;; modes
 (mode-submenu-label "모드")
 (scheme-mode "Racket 모드")
 (text-mode "텍스트 모드")

 (scheme-mode-color-symbol "기호")
 (scheme-mode-color-keyword "키워드")
 (scheme-mode-color-comment "주석")
 (scheme-mode-color-string "문자열")
 (scheme-mode-color-constant "상수")
 (scheme-mode-color-parenthesis "괄호")
 (scheme-mode-color-error "오류")
 (scheme-mode-color-other "그 외")
 ;; the ~a is filled in with one of the above (scheme-mode-*)
 (syntax-coloring-choose-color "~a의 색상을 선택하세요")
 (preferences-colors "색상") ;; used in the preferences dialog
 
  ;; parenthesis color scheme string constants
  (parenthesis-color-scheme "괄호 색상") ;; label for the choice% menu in the preferences dialog
  (paren-color-basic-grey "기본 회색")
  (paren-color-shades-of-gray "회색 계열")
  (paren-color-shades-of-blue "파란색 계열")
  (paren-color-spring "봄")
  (paren-color-fall "가을")
  (paren-color-winter "겨울")

  
 (url: "URL:")
 (open-url... "URL 열기...")
 (open-url "URL 열기")
 (browse... "찾기...")
 (bad-url "잘못된 URL")
 (bad-url:this "잘못된 URL: ~a")
 
 ;; Help Desk
 (help "도움말")
 (help-desk "도움말")
 (plt:hd:search "찾기")
 (plt:hd:feeling-lucky "느낌이 옴")
 (plt:hd:home "도움말 홈") 
 ; next 3 are popup menu choices in help desk search frame
 (plt:hd:search-for-keyword "키워드 입력") ;확인하기. entry?
 (plt:hd:search-for-keyword-or-index "키워드 또는 목차 입력")
 (plt:hd:search-for-keyword-or-index-or-text "키워드, 목차, 텍스트 입력")
 (plt:hd:exact-match "정확하게 일치")
 (plt:hd:containing-match "문구 포함")
 (plt:hd:regexp-match "정규 표현 일치")
 (plt:hd:find-docs-for "관련 문서 찾기:")
 (plt:hd:search-stopped-too-many-matches "[찾기 중단: 일치하는 결과가 너무 많습니다.]")
 (plt:hd:nothing-found-for " ~a의 검색 결과가 없습니다.")
 (plt:hd:and "그리고")
 (plt:hd:refresh "새로 고침")
 (plt:hd:refresh-all-manuals "모든 설명서 새로 고침")
 (plt:hd:manual-installed-date "( ~a 설치됨)")
 ; Help Desk configuration
 ;; refreshing manuals
 (plt:hd:refreshing-manuals "설명서 다시 내려받는 중")
 (plt:hd:refresh-downloading... "~a 내려받는 중...")
 (plt:hd:refresh-deleting... "~a 구버전 삭제 중...")
 (plt:hd:refresh-installing... "~a 새버전 설치 중...")
 (plt:hd:refresh-clearing-indices "저장된 목록 초기화 중")
 (plt:hd:refreshing-manuals-finished "완료.")
 (plt:hd:about-help-desk "도움말에 대해서")
 (plt:hd:help-desk-about-string
  "도움말은 Racket 소프트웨어에 대한 정보를 제공합니다.\n\nVersion ~a\nCopyright (c) ~a-~a PLT")
 (plt:hd:help-on-help "도움말 도움")
 (plt:hd:help-on-help-details "도움말 사용에 대한 도움말은 도움말 홈페이지의 'Help Desk' 링크에 있습니다. (도움말 창 맨 위의 `Home' 버튼을 클릭하면 도움말 홈페이지로 이동합니다.)")
  (reload "다시 불러오기") ;; refresh the page in a web browser
  (plt:hd:ask-about-separate-browser
   "인터넷을 여는 링크를 선택하셨습니다. 도움말 브라우저에서 보시겠습니까, 아니면 별도의 브라우저 프로그램에서 보시겠습니까?")
  (plt:hd:homebrew-browser "도움말 브라우저") ;; choice for the above string (in a button)
  (plt:hd:separate-browser "별도의 브라우저") ;; other choice for the above string (also in a button)
  (plt:hd:external-link-in-help "도움말의 외부 URL")
  (plt:hd:use-homebrew-browser "외부 URL 도움말 브라우저로 보기")
  (plt:hd:new-help-desk "새 도움말")

  ;; in the Help Desk language dialog, title on the right.
  (plt:hd:manual-search-ordering "설명서 찾기 순서")
  
  ;; in the help-desk standalone font preference dialog, on a check box
  (use-drscheme-font-size "DrRacket의 글꼴 크기 사용하기")
  
  ;; in the preferences dialog in drscheme there is example text for help desk font size.
  ;; clicking the links in that text produces a dialog with this message
  (help-desk-this-is-just-example-text
   "이것은 글꼴 크기 예시 텍스트입니다. 링크로 이동하려면 도움말 메뉴에서 도움말를 여십시오.")

  ;; this appears in the bottom part of the frame the first time the user hits `f1' 
  ;; (assuming nothing else has loaded the documentation index first)
  ;; see also: cs-status-loading-docs-index
  (help-desk-loading-documentation-index "도움말: 설명서 목록 가져오는 중")
  
 ;; Help desk htty proxy
 (http-proxy "HTTP 프록시")
 (proxy-direct-connection "직접 연결")
 (proxy-use-proxy "프록시 사용:")
 (proxy-host "호스트")
 (proxy-port "포트")
 (proxy-bad-host "잘못된 프록시 호스트")

 ;; browser
 (rewind-in-browser-history "뒤로")
 (forward-in-browser-history "앞으로")
 (home "홈")
 (browser "브라우저")
 (external-browser-choice-title "외부 브라우저") ; title for radio-button set
 (browser-command-line-label "명령행:") ; label for radio button that is followed by text boxes
 (choose-browser "브라우저를 선택하세요")
 (no-browser "나중에 묻기")
 (browser-cmdline-expl-line-1 "(pre-text, URL, post-text를 잘라서") ; explanatory text for dialog, line 1
 (browser-cmdline-expl-line-2 "중간에 공백 없이 명령행을 만듭니다.)") ; ... line 2. (Anyone need more lines?)
 (install? "설치하시겠습니까?")  ;; if a .plt file is found (title of dialog)
 (you-have-selected-an-installable-package "설치가능한 꾸러미를 선택하셨습니다.")
 (do-you-want-to-install-it? "꾸러미를 설치하시겠습니까?")
 (paren-file-size "(파일의 크기는 ~a 바이트입니다.)")
 (download-and-install "내려받기 && 설치") ;; button label
 (download "내려받기") ;; button label
 (save-downloaded-file/size "내려받은 파일을 (~a 바이트) 저장할 이름") ;; label for get-file dialog
 (save-downloaded-file "내려받은 파일을 저장할 이름")  ;; label for get-file dialog
 (downloading "내려받는 중") ;; dialog title
 (downloading-file... "파일 내려받는 중...")
 (package-was-installed "꾸러미를 설치하였습니다.")
 (download-was-saved "내려받은 파일을 저장하였습니다.")

 (install-plt-file-menu-item... ".plt 파일 설치하기")
 (install-plt-file-dialog-title ".plt 파일 설치하기")
 (install-plt-web-tab "웹")
 (install-plt-file-tab "파일")
 (install-plt-filename "파일명:")
 (install-plt-url "URL:")
 
 ;; install plt file when opened in drscheme strings
 (install-plt-file " ~a을(를) 설치하거나 열어서 편집하시겠습니까?")
 (install-plt-file/yes "설치")
 (install-plt-file/no "편집")

 (plt-installer-progress-window-title "설치 진행 상황") ;; frame title
 (plt-installer-abort-installation "설치 중단") ;; button label
 (plt-installer-aborted "중단됨") ;; msg that appears in the installation window when installation is aborted

 ;;; about box
 (about-drscheme-frame-title "DrRacket 소개")
 
 ;;; save file in particular format prompting.
 (save-as-plain-text "텍스트 파일로 저장하시겠습니까?")
 (save-in-drs-format "DrRacket(non-text) 형식으로 저장하시겠습니까?")
 (yes "예")
 (no "아니오")
 
 ;; saving image (right click on an image to see the text)
  (save-image "이미지 저장")
  
 ;;; preferences
 (preferences "설정")
 (error-saving-preferences "설정을 저장하는 도중 오류가 발생했습니다: ~a")
 (error-saving-preferences-title "설정을 저장하는 도중 오류가 발생했습니다.")
 (steal-the-lock-and-retry "잠금을 해제하고 다시 시도하십시오") ;; in the preferences error dialog; this happens when the lockfile exists (after 3 pref writes). ;확인하기 Steal?
 (error-reading-preferences "설정을 읽던 도중 오류가 발생했습니다.")
 (prefs-file-locked "설정 파일이 잠겨있습니다(~a 파일이 존재합니다), 설정변경을 저장할 수 없습니다. 설정 변경을 취소하시겠습니까?")
 (try-again "재시도") ;; button label
 (prefs-file-still-locked "설정 파일이 여전히 잠겨있습니다(~a 파일이 존재합니다), 설정변경을 저장할 수 없습니다.")
 (scheme-prefs-panel-label "Racket")
 (warnings-prefs-panel-label "경고")
 (editor-prefs-panel-label "편집")
 (general-prefs-panel-label "일반")
 (highlight-parens "대응되는 괄호에 강조하기")
 (fixup-open-brackets "자동으로 여는 대괄호'['에 적용")
 (fixup-close-parens "자동으로 닫는 괄호에 적용") ;확인하기 parens == ')' ?, adjust?
 (flash-paren-match "대응되는 괄호 블록처리")
 (auto-save-files "파일 자동 저장")
 (backup-files "파일 백업")
 (map-delete-to-backspace "Delete 키를 Backspace 키로 설정")
 (verify-exit "종료 확인")
 (ask-before-changing-format "파일 형식을 바꾸기 전에 확인하기")
 (wrap-words-in-editor-buffers "편집기 버퍼에 있는 단어 단위로 줄 넘기기") ;확인하기!!!!!!@!!!!!!@#!@%@#^$#$#%% -_-
 (show-status-line "상태표시줄 보기")
 (count-columns-from-one "열번호를 1부터 시작")
 (display-line-numbers "버퍼에 글자 수가 아니라 행번호:줄번호로 표시");확인하기
 (show-line-and-column-numbers "행번호 && 열번호 표시") ; used for popup menu; right click on line/column box in bottom of drs window
 (show-character-offsets "글자수 보기") ; used for popup menu; right click on line/column box in bottom of drs window
 (enable-keybindings-in-menus "메뉴 단축키 사용")
 (command-as-meta "명령 키 메타로 취급") ;; macos/macos x only
 (reuse-existing-frames "새로운 파일을 열 때 기존 프레임 사용")
 (default-fonts "기본 글꼴")
 (basic-gray-paren-match-color "강조 색") ; in prefs dialog
 (online-coloring-active "입력과 동시에 문법 색 입히기");확인하기
 (open-files-in-tabs "파일을 별도의 탭에 열기(새 창에 열지 않기)")
 (show-interactions-on-execute "프로그램 실행 시 자동으로 대화창 열기")
  (switch-to-module-language-automatically "모듈을 열 때 모듈 언어로 자동으로 바꾸기")
  (interactions-beside-definitions "대화창을 정의창 옆에 두기") ;; in preferences, below the checkbox one line above this one
 (limit-interactions-size "대화창 크기 한계 설정")
 (background-color "배경 색")
 (default-text-color "텍스트 초기화") ;; used for configuring colors, but doesn't need the word "color"
 (choose-a-background-color "배경 색을 선택해 주세요")
 (revert-to-defaults "설정 초기화")
  
  (black-on-white-color-scheme "흰색 배경에 검정 글씨") ;; these two appear in the color preferences dialog on butttons
  (white-on-black-color-scheme "검정 배경에 흰 글씨") ;; clicking the buttons changes the color schemes to some defaults that've been set up.
  
 ; title of the color choosing dialog

 ; should have entire alphabet
 (font-example-string "The quick brown fox jumped over the lazy dogs.") 

 (change-font-button-label "바꾸기")
 (fonts "글꼴")
 (other... "그 외...") ;; used in the font choice menu item

 ; filled with type of font, eg modern, swiss, etc.
 (choose-a-new-font "새로운 \"~a\" 글꼴을 선택해 주세요")

 (font-size-slider-label "크기")
 (restart-to-see-font-changes "글꼴 변경사항을 적용하시려면 다시 시작 하세요")

 (font-prefs-panel-title "글꼴")
 (font-name "글꼴 이름")
 (font-size "글꼴 크기")
 (set-font "글꼴을 설정합니다")
 (font-smoothing-label  "글꼴 뭉개기")
 (font-smoothing-none "하지 않음")
 (font-smoothing-some "약간")
 (font-smoothing-all "완전히")
 (font-smoothing-default "시스템 설정 사용")
 (select-font-name "글꼴 이름 선택")
 (example-text "미리보기:")
 (only-warn-once "정의창과 대화창이 동기화 되지 않았을 때 한번만 경고")
 
 ; warning message when lockfile is around
 (waiting-for-pref-lock "설정 잠금파일을 위해 기다리는 중입니다")
 (pref-lock-not-gone
  "설정 잠금파일:\n\n   ~a\n\n때문에 설정을 저장할 수 없습니다. 실행중인 Racket 프로그램이 없는 것을 확인하고 이 파일을 삭제해주십시오.")
 (still-locked-exit-anyway? "설정파일이 성공적으로 저장되지 않았습니다. 종료하시겠습니까?")
 
 ;;; indenting preferences panel
 (indenting-prefs-panel-label "들여쓰기")
 (indenting-prefs-extra-regexp "다른 정규 표현식")

 (square-bracket-prefs-panel-label "대괄호")
  
 ; filled with define, lambda, or begin
 (enter-new-keyword "~a 같은 키워드들을 입력하세요:") ;확인하기 <- 이거 뭐 하는 거임;;ㅠ
 (x-keyword "~a 키워드")
 (x-like-keywords "~a 같은 키워드")

 ; used in Square bracket panel
 (skip-subexpressions "건너 뛸 부분표현식의 수") ;확인하기

 (expected-a-symbol "기호의 자리에: ~a")
 (already-used-keyword "\"~a\" 는 이미 특별히 들여쓰기할 키워드입니다.")
 (add-keyword "추가")
 (remove-keyword "삭제")
 
  ; repl color preferences
  (repl-colors "REPL")
  (repl-out-color "출력")
  (repl-value-color "값")
  (repl-error-color "오류")
  
  ;;; find/replace
  (search-next "다음")
  (search-previous "이전")
  (search-match "일치")  ;;; this one and the next one are singular/plural variants of each other
  (search-matches "일치") 
  (search-replace "바꾸기")
  (search-skip "건너뛰기")
  (search-show-replace "바꾸기 보기")
  (search-hide-replace "바꾸기 숨기기")
  (find-case-sensitive "대/소문자 구별")  ;; the check box in both the docked & undocked search
  (find-anchor-based "기준 설정하여 찾기");확인하기 anchor?

  ;; these string constants used to be used by searching,
  ;; but aren't anymore. They are still used by other tools, tho.
  (hide "숨기기")
  (dock "붙이기") ;확인하기
  (undock "떼기")
  
 ;;; multi-file-search
 (mfs-multi-file-search-menu-item "파일에서 찾기...")
 (mfs-string-match/graphics "문자열 검색 (그림이 있는 파일도 가능)")
 (mfs-regexp-match/no-graphics "정규 표현식 (텍스트파일만 가능)")
 (mfs-searching... "검색중...")
 (mfs-configure-search "찾기 설정") ;; dialog title
 (mfs-files-section "파일")   ;; section in config dialog
 (mfs-search-section "검색") ;; section in config dialog
 (mfs-dir "경로")
 (mfs-recur-over-subdirectories "하위디렉토리도 검색")
 (mfs-regexp-filename-filter "다음 정규표현식에 맞는 파일")
 (mfs-search-string "찾을 문자열")
 (mfs-drscheme-multi-file-search "다중 파일 검색 - DrRacket") ;; results window and error message title
 (mfs-not-a-dir "\"~a\" 는 디렉토리가 아닙니다")
 (mfs-open-file "파일 열기")
 (mfs-stop-search "검색 중지")
 (mfs-case-sensitive-label "대/소문자 구별")
 (mfs-no-matches-found "일치하는 검색결과가 없습니다.")
 (mfs-search-interrupted "검색이 중지되었습니다.")
 
 ;;; reverting a file
 (are-you-sure-revert
  "정말로 이 파일을 되돌리시겠습니까? 변경을 취소할 수 없습니다.")
 (are-you-sure-revert-title
  "되돌리시겠습니까?")
 
 ;;; saving a file
 ; ~a is filled with the filename
 (error-saving "저장 오류") ;; title of error message dialog
 (error-saving-file/name "~a을(를) 저장하는 도중 오류가 발생했습니다.")
 (error-loading "불러오기 오류")
 (error-loading-file/name "~a을(를) 불러오는 도중 오류가 발생했습니다.")
 (unknown-filename "<< 알 수 없는 파일 >>")

 ;;; finder dialog
 (must-specify-a-filename "파일명을 설정하십시오")
 (file-does-not-exist "\"~a\" 파일이 존재하지 않습니다.")
 (ask-because-file-exists "\"~a\" 파일이 이미 존재합니다. 덮어쓰시겠습니까?")
 (dne-or-cycle "\"~a\"은(는) 존재하지 않는 디렉토리 혹은 순환참조를 포함하고 있습니다.")
 (get-file "파일 가져오기")
 (put-file "파일 내보내기")
 (full-pathname "절대 경로명")
 (show-dot-files ".으로 시작하는 파일과 디렉토리 보여주기")
 (up-directory-button-label "상위 디렉토리")
 (add-button-label "추가") ;;; for multi-file selection
 (add-all-button-label "모두 추가") ;;; for multi-file selection
 (remove-button-label "삭제") ;;; for multi-file selection
 (file-wrong-form "파일명의 형식이 잘못되었습니다.")
 (select-files "파일 선택하기") 
 (select-file "파일 선택하기")
 (dir-dne "디렉토리가 존재하지 않습니다.")
 (file-dne "파일이 존재하지 않습니다.")
 (empty-filename "파일명에는 글자가 포함되어야 합니다.")
 (that-is-dir-name "디렉토리의 이름입니다.")
 
 ;;; raw menu names -- these must match the 
 ;;; versions below, once the &s have been stripped.
 ;;; if they don't, DrRacket's menus will appear
 ;;; in the wrong order.
 (file-menu "파일")
 (edit-menu "편집")
 (help-menu "도움말")
 (windows-menu "윈도우")
 
 ;;; menus
 ;;; - in menu labels, the & indicates a alt-key based shortcut.
 ;;; - sometimes, things are stuck in the middle of 
 ;;; menu item labels. For instance, in the case of
 ;;; the "Save As" menu, you might see: "Save Definitions As". 
 ;;; be careful of spacing, follow the English, if possible.
 ;;; - the ellipses in the `after' strings indicates that
 ;;; more information is required from the user before completing
 ;;; the command.

 (file-menu-label "파일(&F)")

 (new-info  "새 파일 열기")
 (new-menu-item "새로 열기 (&N)")
 (new-...-menu-item "새로 열기 (&N)")

 (open-info "디스크에서 파일 가져와서 열기")
 (open-menu-item "열기 (&O)")

 (open-recent-info "최근 열어본 파일 목록")
 (open-recent-menu-item "최근 파일 열기 (&T)")

 (revert-info "디스크에 있는 파일 복사본으로 되돌리기")
 (revert-menu-item "되돌리기 (&R)")

 (save-info "디스크에 저장하기")
 (save-menu-item "저장하기 (&S)")

 (save-as-info "파일명을 묻고 파일을 디스크에 저장하기")
 (save-as-menu-item "을 다른 이름으로 저장 (&A)")

 (print-info "파일을 프린터로 보내기")
 (print-menu-item "인쇄하기 (&P)")

 (page-setup-info "인쇄 설정")
 (page-setup-menu-item "페이지 설정")

 (close-info "파일 닫기")
 (close-menu-item "닫기 (&C)")

 (quit-info "모든 창 닫기")
 (quit-menu-item-windows "종료하기 (&X)")
 (quit-menu-item-others "끝내기 (&Q)")
 
 (edit-menu-label "편집(&E)")
 
 (undo-info "가장 최근 작업 되돌리기")
 (undo-menu-item "되돌리기 (&U)")

 (redo-info "가장 최근 되돌린 작업 되돌리기")
 (redo-menu-item "다시 실행 (&R)")

 (cut-info "선택된 항목을 클립보드로 옮기기")
 (cut-menu-item "잘라내기 (&T)")

 (copy-info "선택된 항목을 클립보드에 복사하기")
 (copy-menu-item "복사하기 (&C)")

 (paste-info "가장 최근 복사하거나 잘라낸 항목을 선택된 항목의 자리에 붙여넣기")
 (paste-menu-item "붙여넣기 (&P)")

 (clear-info "선택된 항목을 클립보드에 관계없이 지우기")
 (clear-menu-item-windows "삭제하기 (&D)")

 (select-all-info "전체 선택하기")
 (select-all-menu-item "전체 선택 (&L)")
 
  (find-menu-item "찾기") ;; menu item
  (find-info "입력 위치를 탐색창과 찾기바 사이에서 전환하기")
  
 (find-next-info "검색된 다음 문자열로 건너뛰기")
 (find-next-menu-item "다음 찾기")
  
 (find-previous-info "검색된 이전 문자열로 건너뛰기")
 (find-previous-menu-item "이전 찾기")
  
  (show-replace-menu-item "바꾸기 보기")
  (hide-replace-menu-item "바꾸기 숨기기")
  (show/hide-replace-info "바꾸기 패널 보기/숨기기 전환")

  (replace-menu-item "바꾸기")
  (replace-info "진한 동그라미 안에 있는 검색 결과를 바꾸기")
  
  (replace-all-info "모든 검색 결과 바꾸기")
  (replace-all-menu-item "모두 바꾸기")
  
  (find-case-sensitive-info "대/소문자 구별 검색과 구별하지 않는 검색 사이를 전환")
  (find-case-sensitive-menu-item "대/소문자 구별 검색")
  
  (complete-word "단어 완성") ; the complete word menu item in the edit menu
  (no-completions "완성된 단어 없음") ; shows up in the completions menu when there are no completions (in italics)
  
  (overwrite-mode "덮어쓰기 모드")
  (enable-overwrite-mode-keybindings "덮어쓰기 모드 단축키 활성화")
  
 (preferences-info "설정 바꾸기")
 (preferences-menu-item "설정...")

 (keybindings-info "현재 활성화된 단축키 보기")
 (keybindings-menu-item "단축키")
 (keybindings-show-active "활성화된 단축키 보기")
 (keybindings-frame-title "단축키")
 (keybindings-sort-by-name "이름으로 정렬")
 (keybindings-sort-by-key "단축키로 정렬")
 (keybindings-add-user-defined-keybindings "사용자 정의 단축키 보기")
 (keybindings-add-user-defined-keybindings/planet "PLaneT으로부터 사용자 정의 단축키 추가")
 (keybindings-menu-remove "~a 삭제")
 (keybindings-choose-user-defined-file "단축키 저장 파일을 선택하여 주십시오")
 (keybindings-planet-malformed-spec "설정이 잘못되었습니다: ~a") ; the string will be what the user typed in
 (keybindings-type-planet-spec "PLaneT 요구 설정 입력하기(`요구'없이)") ;확인하기 require spec??
  
 ; first ~a will be a string naming the file or planet package where the keybindings come from;
 ; second ~a will be an error message
 (keybindings-error-installing-file "단축키 ~a 설치 도중에 오류가 발생했습니다:\n\n~a")
  
 (user-defined-keybinding-error "단축키 ~a 실행 도중에 오류가 발생했습니다 \n\n~a")
 (user-defined-keybinding-malformed-file "~a 파일은 framework/keybinding-lang language로 쓰인 모듈을 포함하고 있지 않습니다.")  
 (user-defined-keybinding-malformed-file/found-lang "~a 파일은 framework/keybinding-lang language로 쓰여진 모듈을 포함하고 있지 않습니다. 대신에 ~s 를 발견하였습니다.")  
  
 ;; menu items in the "special" menu
 (insert-text-box-item "텍스트 상자 넣기")
 (insert-image-item "그림 넣기")
 (insert-comment-box-menu-item-label "주석상자 넣기")
 (insert-lambda "λ 넣기")

 (wrap-text-item "자동 줄 바꿈")

  ;; windows menu
 (windows-menu-label "창(&W)")
 (minimize "최소화") ;; minimize and zoom are only used under mac os x
 (zoom "확대/축소")
 (bring-frame-to-front "프레임 앞으로 가져오기")       ;;; title of dialog
 (bring-frame-to-front... "프레임을 앞으로 가져옵니다") ;;; corresponding title of menu item
 (most-recent-window "가장 최근 창")
  (next-tab "다음 탭")
  (prev-tab "이전 탭")
  (tab-i "탭 ~a: ~a") ;; menu item in the windows menu under mac os x. first ~a is filled with a number between 1 and 9; second one is the filename of the tab

 (view-menu-label "보기(&V)")
 (show-overview "프로그램 윤곽 보기 (&P)") 
 (hide-overview "프로그램 윤곽 숨기기 (&P)")
 (show-module-browser "모듈 브라우저 보기 (&M)")
 (hide-module-browser "모듈 브라우저 숨기기 (&M)")

  (help-menu-label "도움말(&H)")
 (about-info "출처와 세부 정보")
 (about-menu-item "소개...")
 
 ;; open here's new menu item
 (create-new-window-or-clear-current
  "새 창을 만드시겠습니까, 아니면 현재 창을 초기화하시겠습니까?")
 (clear-current "현재 창 초기화")
 (new-window "새 창 열기")

 ;;; exiting and quitting ``are you sure'' dialog
 ;;; exit is used on windows, quit on macos, in English. Other
 ;;; languages probably use the same word on both platforms.
 (exit "종료")
 (quit "닫기")
 (are-you-sure-exit "종료하시겠습니까?")
 (are-you-sure-quit "닫으시겠습니까?")
  ; these next two are only used in the quit/exit dialog
  ; on the button whose semantics is "dismiss this dialog".
  ; they are there to provide more flexibility for translations
  ; in English, they are just cancel.
 (dont-exit "취소") 
 (dont-quit "취소")
  
 ;;; autosaving
 (error-autosaving "자동 저장 중에 오류가 발생하였습니다 \"~a\".") ;; ~a will be a filename
 (autosaving-turned-off "파일이 저장될때까지 자동 저장을 하지 않습니다.")
 (recover-autosave-files-frame-title "자동 저장된 파일 복구하기")
 (autosave-details "세부정보")
 (autosave-recover "복구")
 (autosave-unknown-filename "<<알 수 없는 파일>>")
  
  ;; these are labels in a dialog that drscheme displays
  ;; if you have leftover autosave files. to see the dialog,
  ;; start up drscheme and modify (but don't save) a file
  ;; (also, do this with an unsaved file). Wait for the autosave
  ;; files to appear (typically 5 minutes). Kill DrRacket
  ;; and restart it. You'll see the dialog
  (autosave-autosave-label: "자동 저장 파일:")
  (autosave-original-label: "기존 파일:")
  (autosave-autosave-label "자동 저장 파일")
  (autosave-original-label "기존 파일")
  (autosave-compare-files "자동 저장 파일 비교하기")

  (autosave-show-autosave "자동 저장 파일") ;; title of a window showing the autosave file

  (autosave-explanation "DrRacket이 저장되지 않은 내용이 포함되어 있을 수 있는 자동 저장 파일을 찾았습니다.")

  (autosave-recovered! "복구되었습니다!") ;; status of an autosave file
  (autosave-deleted "삭제되었습니다")       ;; status of an autosave file

  (autosave-error-deleting "~a을(를) 삭제하는 도중에 오류가 발생하였습니다\n\n~a") ;; first is a filename, second is an error message from mz.
  (autosave-delete-button "삭제")
  (autosave-delete-title "삭제")  ;; title of a dialog asking for deletion confirmation
  (autosave-done "완료")
  
  ;; appears in the file dialog
  (autosave-restore-to-where? "자동 저장 파일을 저장할 곳을 선택하세요.")
  
  
 ;;; file modified warning
 (file-has-been-modified
  "마지막으로 저장한 후로 파일이 변경되었습니다. 변경 사항을 저장하시겠습니까?")
 (overwrite-file-button-label "덮어쓰기")
 
 (definitions-modified 
  "정의 텍스트가 파일시스템에서 변경되었습니다; 정의 텍스트를 저장하거나 되돌리십시오.")
 (drscheme-internal-error "DrRacket 내부 오류")
 
 ;;; tools
 (invalid-tool-spec "info.rkt 파일(collection ~a) 안에 있는 도구 세부 정보가 잘못되었습니다. 문자열 또는 문자열의 목록이 있어야 하는데, ~e이(가) 있습니다.")
 (error-invoking-tool-title "~s 도구를 가져오는데 ;~s 오류가 발생하였습니다.")
 (error-loading-tool-title "~s 도구를 불러오는데 \n~a 오류가 발생하였습니다.") ;; ~s filled with a path, ~a filled with an error message from an exn
 (tool-tool-names-same-length
  "info.rkt 파일(~s)의 `tool-names' 와 `tools' 이 같은 길이의 목록이어야 하는데 ~e와(과) ~e이(가) 있습니다.")
 (tool-tool-icons-same-length
  "info.rkt 파일(~s)의 `tool-icons' 와 `tools' 이 같은 길이의 목록이어야 하는데 ~e와(과) ~e이(가) 있습니다.")
 (tool-tool-urls-same-length
  "info.rkt 파일(~s)의 `tool-urls' 와 `tools' 이 같은 길이의 목록이어야 하는데 ~e와(과) ~e이(가) 있습니다.")
 (error-getting-info-tool
  "info.rkt 파일(~s)을 불러오는데 오류가 발생하였습니다.")
 (tool-error-phase1 "도구 ~s의 phase1에 ~s 오류가 발생하였습니다.")
 (tool-error-phase2 "도구 ~s의 phase2에 ~s 오류가 발생하였습니다.")


 ;;; define popup menu
 (end-of-buffer-define "<< 버퍼의 끝>>")
 (sort-by-name "이름으로 정렬하기")
 (sort-by-position "파일 내 위치로 정렬하기")
 (no-definitions-found "<< 정의를 찾지 못하였습니다>>")
 (jump-to-defn "~a의 정의로 건너뛰기")

 (recent-items-sort-by-age "나이로 정렬")
 (recent-items-sort-by-name "이름으로 정렬")
 
 ;;; view menu
 (hide-definitions-menu-item-label "정의 숨기기 (&D)")
 (show-definitions-menu-item-label "정의 보기 (&D)")
 (definitions-menu-item-help-string "정의창 보기/숨기기")
 (show-interactions-menu-item-label "대화 보기 (&I)")
 (hide-interactions-menu-item-label "대화 숨기기 (&I)")
 (interactions-menu-item-help-string "대화창 보기/숨기기")
 (toolbar "도구모음")
 (toolbar-on-top "도구모음 위에 두기")
 (toolbar-on-left "도구모음 왼쪽에 두기")
 (toolbar-on-right "도구모음 오른쪽에 두기")
 (toolbar-hidden "도구모음 숨기기")

 ;;; file menu
 (save-definitions-as "다른 이름으로 정의 저장하기 (&A)")
 (save-definitions "정의 저장하기")
 (print-definitions "정의 인쇄하기")
 (about-drscheme "DrRacket 소개")
 (save-other "추가 저장 도구")
 (save-definitions-as-text "텍스트 문서로 정의 저장하기")
 (save-interactions "대화 저장하기")
 (save-interactions-as "다른 이름으로 대화 저장하기")
 (save-interactions-as-text "텍스트 문서로 대화 저장하기")
 (print-interactions "대화 인쇄하기")
 (new-tab "새 탭")
 (close-tab "탭 닫기") ;; must not have any &s in it.
 (close-tab-amp "탭 닫기 (&C)") ;; like close-tab, but with an ampersand on the same letter as the one in close-menu-item
  
 ;;; edit menu
 (split-menu-item-label "쪼개기 (&S)")
 (collapse-menu-item-label "붙이기 (&O)")
 
 ;;; language menu
 (language-menu-name "언어(&L)")
 
 ;;; scheme-menu
 (scheme-menu-name "Racket(&C)")
 (execute-menu-item-label "실행")
 (execute-menu-item-help-string "정의창의 프로그램 다시 시작하기")
 (ask-quit-menu-item-label "프로그램 중단 요청")
 (ask-quit-menu-item-help-string "중단 스레드를 사용하여 현재 계산중인 스레드를 중단");확인하기
 (force-quit-menu-item-label "프로그램 강제중단")
 (force-quit-menu-item-help-string "작업관리자를 사용하여 현재 계산을 중단")
 (limit-memory-menu-item-label "메모리 제한")
 (limit-memory-msg-1 "메모리 제한은 8MB 이상이어야 하며, 바꾼 제한 설")
 (limit-memory-msg-2 "정은 프로그램을 다시 시작할 때부터 적용됩니다.")
 (limit-memory-unlimited "제한없음")
 (limit-memory-limited "제한있음")
 (limit-memory-megabytes "Megabytes")
 (clear-error-highlight-menu-item-label "오류 강조 표시 없애기")
 (clear-error-highlight-item-help-string "분홍색 강조 표시를 지웁니다")
 (reindent-menu-item-label "다시 들여쓰기 (&R)")
 (reindent-all-menu-item-label "전부 들여쓰기 (&A)")
 (semicolon-comment-out-menu-item-label "세미콜론으로 주석처리하기 (&C)")
 (box-comment-out-menu-item-label "상자로 주석처리하기 (&C)")
 (uncomment-menu-item-label "주석처리 취소 (&U)")

 (convert-to-semicolon-comment "세미콜론 주석으로 바꾸기")
 
 ;;; executables
 (create-executable-menu-item-label "실행파일 만들기")
 (create-executable-title "실행파일 만들기")
 (must-save-before-executable "실행파일을 만들기 전에 프로그램을 저장하셔야 합니다.")
 (save-a-mred-launcher "GRacket Launcher 저장") ;확인하기 Launcher?
 (save-a-mzscheme-launcher "Racket Launcher 저장")
 (save-a-mred-stand-alone-executable "독립형 GRacket 실행파일 저장") ;stand-alone
 (save-a-mzscheme-stand-alone-executable "독립형 Racket 실행파일 저장")
 (save-a-mred-distribution "GRacket 배포판 저장")
 (save-a-mzscheme-distribution "Racket 배포판 저장")

 (definitions-not-saved "정의창의 프로그램이 저장되지 않았습니다. 실행파일은 정의창에서 마지막으로 저장된 버전을 사용합니다 계속하시겠습니까?")
 ;; The "-explanatory-label" variants are the labels used for the radio buttons in
 ;;  the "Create Executable..." dialog for the "(module ...)" language.
 (launcher "Launcher")
 (launcher-explanatory-label "Launcher (이 장치에만 적용, 이 코드를 실행함)");확인하기 runs from source
 (stand-alone "독립형") ;stand-alone
 (stand-alone-explanatory-label "독립형 (이 장치에만 적용, 컴파일된 코드를 실행함)")
 (distribution "배포")
 (distribution-explanatory-label "배포 (다른 장치에서 사용가능)")
 (executable-type "종류")
 (executable-base "기반")
 (filename "파일이름: ")
 (create "생성")
 (please-specify-a-filename "생성할 파일 이름을 정해주십시오.")
 (~a-must-end-with-~a
  "파일이름 ~a\n\n  ~a\n\n은(는) 잘못되었습니다. 파일이름은 반드시 \".~a\".로 끝나야 합니다")
 (macosx-executables-must-end-with-app
  "파일이름\n\n  ~a\n\n은(는) 잘못되었습니다. MacOS X 에서, 실행파일은 반드시 .app으로 끝나는 디렉토리 내에 있어야 합니다.")
 (warning-directory-will-be-replaced
  "경고: 디렉토리:\n\n  ~a\n\n가 덮어씌워집니다. 계속하시겠습니까?")
 
 (distribution-progress-window-title "배포 과정 진행")
 (creating-executable-progress-status "배포를 위한 실행파일을 생성합니다")
 (assembling-distribution-files-progress-status "배포를 위해 파일을 어셈블리어로 번역합니다")
 (packing-distribution-progress-status "배포판을 압축합니다")

 (create-servlet "서블릿 생성")

 ; the ~a is a language such as "module" or "algol60"
 (create-servlet-unsupported-language
  "~a 언어에서는 서블릿 생성을 할 수 없습니다.")
  
 ;;; buttons
 (execute-button-label "실행") 
 (save-button-label "저장")
 (break-button-label "중지")
 
 ;;; search help desk popup menu
 (search-help-desk-for "헬프데스크에서 \"~a\" 검색")
 (exact-lucky-search-help-desk-for "헬프데스크에서 \"~a\" lucky search 하기")

 ;; collapse and expand popup menu items
 (collapse-sexp "S-expression 접기")
 (expand-sexp "S-expression 펼치기")
 
 ;;; fraction dialog
 (enter-fraction "분수 입력")
 (whole-part "정수부")
 (numerator "분자")
 (denominator "분모")
 (insert-number/bad-whole-part "정수부는 반드시 정수여야 합니다.")
 (insert-number/bad-numerator "분자는 반드시 음이 아닌 정수여야 합니다.")
 (insert-number/bad-denominator "분모는 양의 정수여야 합니다.")
 (insert-fraction-menu-item-label "분수 넣기")

 ;; number snip popup menu
 (show-decimal-expansion "소수로 보기")
 (show-mixed-fraction-view "대분수로 보기")
 (show-improper-fraction-view "가분수로 보기")
 (show-more-decimal-places "소숫점자리 더 보기")
 
 ;;; Teachpack messages
 (select-a-teachpack "배움꾸러미 선택")
 (clear-teachpack "~a 배움꾸러미 빼기")
 (teachpack-error-label "DrRacket - 배움꾸러미 오류")
 (teachpack-didnt-load "~a 배움꾸러미 파일을 불러오는 데 실패했습니다.")
 (add-teachpack-menu-item-label "배움꾸러미 추가")
 (clear-all-teachpacks-menu-item-label "모든 배움꾸러미 빼기")
 (drscheme-teachpack-message-title "DrRacket 배움꾸러미")
 (already-added-teachpack "~a 배움꾸러미가 이미 추가되어 있습니다.")
  
  ; ~a is filled with the teachpack's name; the message appears in the teachpack selection dialog when a user installs a new teachpack
  (compiling-teachpack "~a 배움꾸러미를 컴파일중입니다.")
  (teachpack-pre-installed "이미 설치된 배움꾸러미입니다.")
  (teachpack-pre-installed/htdp "이미 설치된 HTDP 배움꾸러미입니다.")
  (teachpack-pre-installed/2htdp "이미 설치된 HTDP/2e 배움꾸러미입니다.")
  (teachpack-user-installed "사용자 정의 배움꾸러미입니다.")
  (add-teachpack-to-list... "배움꾸러미를 목록에 추가하기")
  (teachpack-already-installed "배움꾸러미 '~a' 이(가) 이미 설치되어 있습니다. 덮어쓰시겠습니까?")
  ; ~a is filled with a list of language names. Each name is separated by a newline and is indented two spaces (no commas, no 'and')
  (teachpacks-only-in-languages "배움꾸러미는 다음 언어에서만 사용가능합니다: ~a")
   
 ;;; Language dialog
 (introduction-to-language-dialog
  "언어를 고르십시오. 입문과목을 듣는 학생은 기본 언어를 사용을 권장합니다.")
 (language-dialog-title "언어를 고르십시오.")
 (case-sensitive-label "대/소문자 구별")
 (output-style-label "출력 스타일")
 (constructor-printing-style "생성자")
 (quasiquote-printing-style "Quasiquote") ; 확인하기 ;따로 나타낼 말이 없을 듯. manual에서 보고 어떤 말인지 알아내는 수 밖에는... (manual에 설명 나와있긴 하네...)
 (write-printing-style "write")
 (print-printing-style "print")
 (sharing-printing-label "공유하는 값 보기")
 (use-pretty-printer-label "출력된 값 안에 새줄 넣기")
 (input-syntax "입력 문법")
 (dynamic-properties "동적 속성")
 (output-syntax "출력 문법")
  (teachpacks "배움꾸러미") ;; label in the language dialog for the teaching languages
  (teachpacks-none "<< 없음 >>") ;; shows up under the previous string, when there are no teachpacks
 (no-debugging-or-profiling "디버깅 또는 프로파일링 안하기")
 (debugging "디버깅")
 (debugging-and-profiling "디버깅과 프로파일링")
 (test-coverage "논리적 테스트 모음 도달범위")
 (show-details-button-label "상세히 보기")
 (hide-details-button-label "상세히 감추기")
 (choose-language-menu-item-label "언어 고르기")
 (revert-to-language-defaults "기본 언어로 되돌리기")
 (fraction-style "분수 모양")
 (use-mixed-fractions "대분수")
 (use-repeating-decimals "순환소수")
 (decimal-notation-for-rationals "유리수를 십진수로 나타내기")
 (enforce-primitives-group-box-label "원시 함수") ;initial binding
 (enforce-primitives-check-box-label "원시 함수 재정의 금지")
 (automatically-compile "compiled/ 디렉토리를 덧붙이기 (빠른 로딩)")
 (preserve-stacktrace-information "스택추적 보존하기 (일부 최적화 끄기)")
 (expression-level-stacktrace "표현-수준의 스택추적")
 (function-level-stacktrace "함수-수준의 스택추적")
  
  
  ; used in the bottom left of the drscheme frame 
  ; used the popup menu from the just above; greyed out and only
  ; visible when some languages are in the history
  (recent-languages "최근 언어:")
  ; shows up in bottom-left programming language menu popup, when no langs are recorded
  (no-recently-chosen-languages "최근 선택된 언어가 없습니다") 
  
 ;; startup wizard screen language selection section
 (please-select-a-language "언어를 고르십시오.")
  
  
 ;;; languages
 (beginning-student "입문 학생")
 (beginning-one-line-summary "define, cond, structs, constants, and primitives")
 (beginning-student/abbrev "입문 학생 (List 축약 포함)")
 (beginning/abbrev-one-line-summary "입문 그리고 REPL안의 list 스타일 출력")
 (intermediate-student "중급 학생")
 (intermediate-one-line-summary "입문 + 어휘적 범위")
 (intermediate-student/lambda "중급 학생 (lambda 포함)")
 (intermediate/lambda-one-line-summary "중급 + higher-order 함수")
 (advanced-student "고급 학생")
 (advanced-one-line-summary "중급 + lambda + mutation")
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (pretty-big-scheme "매우 큰 것")
 (pretty-big-scheme-one-line-summary "HtDP 언어, mzscheme, & mred/mred의 문법과 함수들 추가됨")
 (r5rs-language-name "R5RS")
 (r5rs-one-line-summary "R5RS (frills 미포함)")
 (expander "펼치기")
 (expander-one-line-summary "계산하는 대신 먼저 표현을 펼친다.")
 (legacy-languages "기존 언어") ;확인하기 물려받은 언어?
 (teaching-languages "교육용 언어")
 (experimental-languages "실험용 언어")
  (initial-language-category "초기 언어")
  (no-language-chosen "언어가 선택되지 않았습니다.")
 
  (module-language-name "소스를 통해서 언어를 결정합니다.")
  (module-language-one-line-summary "#lang으로 사용 언어를 결정합니다.")
  (module-language-auto-text "자동 #lang 줄") ;; shows up in the details section of the module language
   
  ;; for the upper portion of the language dialog
  (use-language-in-source "소스에 정의된 언어를 이용합니다.")
  (choose-a-language "언어를 고르십시오.")
  (lang-in-source-discussion
   "프로그램 시작에 위치한 #lang 줄이 그 프로그램의 언어를 선택합니다. 이 방법은 DrRacket의 기본 기능이며 널리 쓰이고 있습니다.")
  
  ;;; from the `not a language language' used initially in drscheme.
  (must-choose-language "DrRacket는 프로그래밍 언어를 선택하기 전에는 프로그램들을 처리할수 없습니다.")
  
  ; next two appear before and after the name of a text book (which will be in italics)
  (using-a-textbook-before "사용중")
  (using-a-textbook-after "?")
  
  ; next two are before and after a language
  (start-with-before "언어시작 - ")
  (start-with-after "")
  
  (seasoned-plt-schemer? "숙련된 PLT Schemer입니까?")
  (racketeer? "Racketeer입니까?")
  (looking-for-standard-scheme? "표준 Scheme을 찾습니까?")

  ; the three string constants are concatenated together and the middle
  ; one is hyperlinked to the dialog that suggests various languages
  (get-guidance-before "\"Language\" 메뉴안의 \"언어 고르십시오.\"를 고르거나, 또는 ")
  (get-guidance-during "사용 안내 가져오기")
  (get-guidance-after ".")
      
 ;;; debug language
 (unknown-debug-frame "[알수없음]")
 (backtrace-window-title "역추적 - DrRacket")
 (files-interactions "~a의 대화") ;; filled with a filename
 (current-interactions "대화")
 (current-definitions "정의")
 (mzscheme-w/debug "원문 (R5RS를 포함한 MzScheme)")
 (mzscheme-one-line-summary "PLT가 구현한 Scheme")
 (mred-w/debug "Graphical (MzScheme를 포함한 MzEd)")
 (mred-one-line-summary "MzScheme의 GUI 지원 추가")

 ;; profiling
 (profiling-low-color "낮은 범위의 색상") ;확인하기 낮은 범위/ 좁은 범위
 (profiling-high-color "높은 범위의 색상")
 (profiling-choose-low-color "낮은 범위의 색상을 선택하십시오.")
 (profiling-choose-high-color "높은 범위의 색상을 선택하십시오.")
 (profiling "프로파일링")  ;; 다시 한번 확인해 볼 것 어떤 의미로 쓰이는지 다시 봐야 될 듯
 (profiling-example-text "(define (whee) (whee))")
 (profiling-color-config "프로파일링 색상 범위") 
 (profiling-scale "프로파일링 색상 크기")
 (profiling-sqrt "제곱근")
 (profiling-linear "선형함수")
 (profiling-square "이차함수")
 (profiling-number "호출 횟수")
 (profiling-time "누적 시간")
 (profiling-update "프로파일 업데이트")
 (profiling-col-percent-time "% 시간")
 (profiling-col-function "함수")
 (profiling-col-time-in-msec "Msec")
 (profiling-col-calls "호출 횟수")
 (profiling-show-profile "프로파일 보기")
 (profiling-hide-profile "프로파일 감추기")
 (profiling-unknown-src "<< 알수없음 >>")
 (profiling-no-information-available "프로파일링 정보가 없습니다. 선택한 언어에서 프로파일링이 사용가능하고 프로그램을 실행해야합니다.")
 (profiling-clear? "정의창의 내용이 변경되어 프로파일링 정보를 무효화합니다. 계속하시겠습니까?")
 
 ;; test coverage
 (test-coverage-clear? "정의창의 내용이 변경되어 테스트 범위 정보를 무효화합니다. 계속하시겠습니까?")
 (test-coverage-clear-and-do-not-ask-again "계속, 다시 묻지 않음")
 (test-coverage-ask? "테스트 범위 지우기 묻기")
  
 ;; tracing
 (tracing-enable-tracing "Tracing 사용하기")
 (tracing-show-tracing-window "Tracing 보기")
 (tracing-hide-tracing-window "Tracing 감추기")
 (tracing-tracing-nothing-to-show "Tracing 결과가 아직 없습니다. (당신의 언어가 tracing 지원하는 지와 tracing이 켜져있는 것을 확인하십시오.)")

 ;;; repl stuff
 (evaluation-terminated "계산 중단")
 (evaluation-terminated-explanation
  "계산을 수행하던 스레드가 더 이상 작동하지 않습니다. 따라서 다음 실행까지 계산이 이루어지지 않을 것입니다.")
  
  ; The next three constants show up in the same dialog as the above evaluation-terminated string
  ; constants.
  ; The first two show up only when the user calls 'exit' (possibly with a status code).
  ; The third shows up when the program runs out of memory.
  (exited-successfully "성공적으로 종료")
  (exited-with-error-code "오류 코드 ~a와 함께 종료") ;; ~a is filled in with a number between 1 and 255
  (program-ran-out-of-memory "이 프로그램을 위한 메모리가 없습니다.")
 (last-stack-frame "마지막 스택 프레임 보기")
 (last-stack-frames "마지막 ~a개 스택 프레임들 보기")
 (next-stack-frames "다음 ~a개 스택 프레임들 보기")
 
 ;;; welcoming message in repl
 (language "언어")
 (custom "사용자정의")
 (teachpack "배움꾸러미") ; 확인
 (welcome-to "환영합니다.")
 (version "버전")
 
 ;;; kill evaluation dialog
 (kill-evaluation? "계산을 멈추시겠습니까?")
 (just-break "강제 중단")
 (kill "멈춤")
 (kill? "멈춤?")

 ;;; version checker
 (version:update-menu-item   "업데이트를 확인합니다.")
 (version:update-check       "업데이트 확인") ; dialog title, with the next line
 (version:connecting-server  "Racket 버전 서버에 접속합니다.")
 (version:results-title      "Racket 버전 확인")
 (version:do-periodic-checks "주기적으로 새로운 Racket 버전을 확인")
 (version:take-me-there      "내려받기 웹사이트") ; ...to the download website
 ;; the next one can appear alone, or followed by a comma and the one after that
 (version:plt-up-to-date     "당신의 Racket은 최신 버전입니다.")
 (version:but-newer-alpha    "하지만 새로 공개된 알파버전이 있습니다.")
 ;; This is used in this context: "Racket vNNN <<<*>>> http://download..."
 (version:now-available-at   "은(는) 지금 사용가능합니다.")

 ;; insert menu
 (insert-menu "삽입(&I)")
 
 ;; large semi colon letters
 (insert-large-letters... "큰 글자 넣기")
 (large-semicolon-letters "큰 세미콜론 글자")
 (text-to-insert "글자 입력")

 (module-browser-filename-format "전체 파일이름: ~a (~a 줄)")
 (module-browser-root-filename "최상위 파일이름: ~a")
 (module-browser-font-size-gauge-label "글꼴 크기")
 (module-browser-progress-label "모듈 진행 개요") ; 확인
 (module-browser-adding-file "파일 추가: ~a...")
 (module-browser-laying-out-graph-label "그래프 모양")
 (module-browser-open-file-format "열기 ~a")
 (module-browser "모듈 탐색기") ;; frame title
 (module-browser... "모듈 탐색기 (&M)") ;; menu item title
 (module-browser-error-expanding "프로그램 오류:\n\n~a") ; 확인
 (module-browser-show-lib-paths "(lib ..) 경로에 의해 로드된 파일 보기")
 (module-browser-progress "모듈 탐색기: ~a") ;; prefix in the status line
 (module-browser-compiling-defns "모듈 탐색기: 정의 컴파일")
 (module-browser-show-lib-paths/short "lib 필수사항 따르기") ;; check box label in show module browser pane in drscheme window. ; 확인
 (module-browser-show-planet-paths/short "PLaneT 필수사항 따르기") ;; check box label in show module browser pane in drscheme window.
 (module-browser-refresh "새로고침") ;; button label in show module browser pane in drscheme window.
 (module-browser-only-in-plt-and-module-langs
  "모듈 탐색기는 모듈기반 프로그램에서만 사용가능합니다.")
 (module-browser-name-length "이름 길이") 
 (module-browser-name-short "짧은") ; 확인하기
 (module-browser-name-medium "중간")
 (module-browser-name-long "긴") 
 (module-browser-name-very-long "단계들과 함께 긴")  ;; like 'Long' but shows the phases where this file is loaded 
 (module-browser-open-all "여기있는 모든 파일 열기")

 (happy-birthday-matthias "생일 축하합니다, Matthias!")
 (happy-birthday-matthew "생일 축하합니다, Matthew!")
 (happy-birthday-shriram "생일 축하합니다, Shriram!")

 (mrflow-using-default-language-title "기본 언어 사용함")
 (mrflow-using-default-language "사용된 언어는 현재 내장 타입 테이블이 없습니다. 대신 R5RS Scheme 사용합니다.") ; 확인
 (mrflow-button-title "분석하기")
 ;(mrflow-unknown-style-delta-error-title "알수없는 상자 스타일 델타") ; 확인
 ;(mrflow-unknown-style-delta-error "알수없는 상자 스타일 델타: ~a")
 (mrflow-popup-menu-show-type "타입 보기")
 (mrflow-popup-menu-hide-type "타입 감추기")
 (mrflow-popup-menu-show-errors "오류 보기")
 (mrflow-popup-menu-hide-errors "오류 감추기")
 ;(mrflow-read-exception-title "예외 읽기")
 ;(mrflow-read-exception "예외 읽기: ~a")
 ;(mrflow-syntax-exception-title "문법 예외")
 ;(mrflow-syntax-exception "문법 예외: ~a")
 ;(mrflow-unknown-exception-title "알수없는 예외")
 ;(mrflow-unknown-exception "알수없는 예외: ~a")
 ;(mrflow-language-primitives-error-title "언어 내장함수 에러") ; 확인
 ;(mrflow-language-primitives-error "잘못된 언어 내장 타입 테이블의 파일이름: ~a")
  
 (snips-and-arrows-popup-menu-tack-all-arrows "모든 화살표 고정")
 (snips-and-arrows-popup-menu-untack-all-arrows "모든 화살표 떼어내기")
 (snips-and-arrows-user-action-disallowed-title "사용자 변경이 현재 허락되지 않습니다.") ; 확인
 (snips-and-arrows-user-action-disallowed "사용자 변경이 도구가 포함된 snips에서 허락되지 않습니다. 편집기 상의 내용을 수정하기 전에 모든 snips를 숨기십시오.j")
 ;(snips-and-arrows-changing-terms-warning-title "용어 변경은 취소가능합니다.") ; 확인
 ;(snips-and-arrows-changing-terms-warning "snips이 포함된 편집기 내의 용어 변경은 취소가 불가능합니다. 동작을 취소하고, snips을 삭제하고, 다시 변경을 시도하십시오. 아니면, 변경이 취소불가능하지만 변경을 계속 할 수 있습니다. (모든 다른 앞과 뒤의 변경은 여전히 취소가능합니다.)")
 (snips-and-arrows-hide-all-snips-in-editor "편집기 상의 모든 snips 감추기")

 (xml-tool-insert-xml-box "XML 상자 넣기")
 (xml-tool-insert-scheme-box "Racket 상자 넣기")
 (xml-tool-insert-scheme-splice-box "Racket 쪼개기 상자 넣기")
 (xml-tool-xml-box "XML 상자")
 (xml-tool-scheme-box "Racket 상자")
 (xml-tool-scheme-splice-box "Racket 쪼개기 상자")
 (xml-tool-switch-to-scheme "Racket 상자로 변경")
 (xml-tool-switch-to-scheme-splice "Racket 쪼개기 상자로 변경")
 (xml-tool-eliminate-whitespace-in-empty-tags
  "비어있는 태그사이 공백 제거")
 (xml-tool-leave-whitespace-alone
  "공백 남기기")
 
 (show-recent-items-window-menu-item "분리된 창의 최근 열린 파일들을 보기")
 (show-recent-items-window-label "최근 열린 파일들")
 (number-of-open-recent-items "최근 열린 파일 갯수")
 (switch-anyway "무시하고 파일 변경") ;확인

 (stepper-program-has-changed "경고: 프로그램이 변경되었습니다.")
 (stepper-program-window-closed "경고: 프로그램 창이 사라졌습니다.")

 (stepper-name "한 단계씩 실행")
 (stepper-language-level-message "한 단계씩 실행은 \"~a\" 언어에서 지원하지 않습니다.")
 (stepper-button-label "한 단계씩 실행")

 (stepper-previous "이전단계")
 (stepper-next "다음단계")
 (stepper-jump "건너뛰기...") 
 (stepper-jump-to-beginning "처음으로")
 (stepper-jump-to-end "끝으로")
 (stepper-jump-to-selected "선택된 것의 처음으로")
 (stepper-jump-to-previous-application "이전 어플리케이션 단계로")
 (stepper-jump-to-next-application "다음 어플리케이션 단계로")
 (stepper-out-of-steps "한단계씩 실행이 실행되기전에 계산이 끝났습니다.")
 (stepper-no-such-step/title "해당 단계를 찾을 수 없습니다.")
 (stepper-no-such-step "조건을 만족하는 단계를 찾을 수 없습니다.")
 (stepper-no-such-step/earlier "조건을 만족하는 이전 단계를 찾을 수 없습니다..")
  
 (debug-tool-button-name "디버그") ;확인하기 (이름 바꿀까요? 버그잡기 / 오류찾기 / 벌레잡기 / 살충)

 (dialog-back "이전")

 ;; warnings about closing a drscheme frame when the program
 ;; might still be doing something interesting
 (program-is-still-running "정의창 안의 프로그램이 아직 실행중입니다. 그래도 종료하시겠습니까?")
  (program-has-open-windows "정의창 안의 프로그램이 열려있는 윈도우창이 있습니다. 그래도 종료하시겠습니까?")
 
  ;; ml-command-line-arguments is for the command line arguments
  ;; label in the module language details in the language dialog.
  (ml-command-line-arguments "문자열의 벡터로 명령행 인수 지정") ;확인하기

  ;; ml-cp names are all for the module language collection path
  ;; configuration. See the details portion of the language dialog
  ;; for the module language (at the bottom).
  (ml-cp-default-collection-path "<<기본 collection 경로>>")

  ;; in std get-directory 
  (ml-cp-choose-a-collection-path "Collection 폴더를 고르십시오.")

  ;; err msg when adding default twice
  (ml-cp-default-already-present
   "기본 collection 경로는 이미 포함되어 있습니다.")
  
  ;; title of this section of the dialog (possibly the word
  ;; `Collection' should not be translated)
  (ml-cp-collection-paths "Collection 경로")

  ;; button labels
  (ml-cp-add "추가")
  (ml-cp-add-default "기본값 추가")
  (ml-cp-remove "제거")
  (ml-cp-raise "올리기")
  (ml-cp-lower "내리기")
  
  (ml-always-show-#lang-line "모듈 언어 설정에서 항상 #lang 줄 보이기")

  ;; Profj
  (profj-java "자바")
  (profj-java-mode "자바 모드")
  (profj-java-coverage "자바 구현 범위") ;; shows up in the preferences dialog under 'Color'
  
  (profj-beginner-lang "초보자")
  (profj-beginner-lang-one-line-summary "자바와 유사한 초보 교육 언어")
  (profj-full-lang "전부")
  (profj-full-lang-one-line-summary "자바 1.0과 유사 (일부 1.1)")
  (profj-advanced-lang "고급")
  (profj-advanced-lang-one-line-summary "자바와 유사한 고급 교육 언어")
  (profj-intermediate-lang "중급")
  (profj-intermediate-lang-one-line-summary "자바와 유사한 중급 교육 언어")
  (profj-intermediate-access-lang "중급 + 접근자")
  (profj-intermediate-access-lang-one-line-summary "자바와 유사한 중급 교육 언어(접근자 포함)")
  (profj-dynamic-lang "자바 + 동적 타입")
  (profj-dynamic-lang-one-summary "자바 (동적 타입 포함)")

  (profj-java-mode-color-heading "색상 수정") ; Heading for preference to choose editing colors  
  (profj-java-mode-color-keyword "주요 단어")
  (profj-java-mode-color-string "문자열")
  (profj-java-mode-color-literal "글자")
  (profj-java-mode-color-comment "주석")
  (profj-java-mode-color-error "오류")
  (profj-java-mode-color-identifier "식별자")
  (profj-java-mode-color-prim-type "원시 타입") ; Example text for built-in Java types
  (profj-java-mode-color-default "기본값")

  (profj-coverage-color-heading "적용 범위 색상") ; Heading for preference to choose coverage colors
  (profj-coverage-color-covered "적용 범위") 
  
  (profj-language-config-display-preferences "화면 설정") ; Heading for preferences controlling printing
  (profj-language-config-display-style "화면 모습")
  (profj-language-config-display-field "클래스 + 필드") ; 확인
  (profj-language-config-class "클래스")
  (profj-language-config-display-array "배열의 모든 내용을 출력")
  (profj-language-config-testing-preferences "테스팅 설정") ; Heading for preferences controlling test behavior
  ;(profj-language-config-testing-enable "실행시 테스팅 결과를 출력") ; Run should be the word found on the Run button
  (profj-language-config-testing-coverage "테스트로부터 적용 범위 정보를 구하시겠습니까?")
  (profj-language-config-support-test-language "테스트 언어 확장기능 사용")
  (profj-language-config-testing-check "check 표현 허가") ; check should not be translated
  (profj-language-config-classpath "클래스 경로")
  (profj-language-config-choose-classpath-directory "클래스 경로에 추가할 디렉토리")
  (profj-language-config-classpath-display "현재 클래스 경로 보기") ; Button label to print the current classpath

  (profj-test-name-close-to-example "클래스 ~a 이름은 예제와 비슷한 표현을 포함한다.") ; 확인
  (profj-test-name-example-miscapitalized "클래스 ~a 이름은 잘못 대문자화한 예제을 포함한다.")
  
   ;; Close testing window and do not run test cases any more
  ;(profj-test-results-close-and-disable "닫고 테스팅 끄기")
  ;; Hide docked testing window and do not run test cases any more
  ;(profj-test-results-hide-and-disable "감추고 테스팅 끄기")
  ;Renamed below
  ;(profj-test-results-window-title "테스트 결과")
  
  (profj-unsupported "지원하지 않음")
  (profj-executables-unsupported "죄송합니다 - 현재 버전의 자바는 이 실행파일을 지원하지 않습니다.")

  (profj-convert-to-text-comment "문자 주석으로 변환")
  (profj-convert-to-comment "주석으로 변환")

  (profj-executing-main "main 실행중")

  (profj-insert-java-comment-box "자바 주석 상자 삽입")
  (profj-insert-java-interactions-box "자바 대화 상자 삽입")

  ;;The Test engine tool
  ;;
  (test-engine-window-title "테스트 결과")
  ;;Following two appear in View menu, attach and free test report window from DrRacket frame
  (test-engine-dock-report "테스트 보고서 열기") ;확인
  (test-engine-undock-report "테스트 보고서 닫기")
  ;;Following two appear in Racket (Java, etc) menu, cause Tests to be Run automatically or not
  (test-engine-enable-tests "테스트 켜기")
  (test-engine-disable-tests "테스트 끄기")

  (test-engine-ran-1-test "1개의 테스트 실행함.")
  (test-engine-ran-1-check "1개 점검 실행함.")
  ;; ditto, only plural
  (test-engine-ran-n-tests "~a개 테스트들 실행함.")
  (test-engine-ran-n-checks "~a개 점검 실행함.")
  (test-engine-1-test-passed "테스트 통과함!")
  (test-engine-1-check-passed "점검 통과함!")
  (test-engine-both-tests-passed "2개 테스트 통과함!")
  (test-engine-both-checks-passed "2개 점검 통과함!")
  (test-engine-all-tests-passed "모든 테스트 통과함!")
  (test-engine-all-checks-passed "모든 점검 통과함!")
  (test-engine-all-n-tests-passed "모든 ~a개 테스트 통과함!")
  (test-engine-all-n-checks-passed "모든 ~a개 점검 통과함!")
  (test-engine-0-tests-passed "0개 테스트 통과함.")
  (test-engine-0-checks-passed "0개 점검 통과함.")
  (test-engine-m-of-n-tests-failed "~a개 중 ~a개 테스트 실패함.")
  (test-engine-m-of-n-checks-failed "~a개 중 ~a개 점검 실패함.")
  (test-engine-must-be-tested "이 프로그램은 테스트가 꼭 필요합니다!")
  (test-engine-is-unchecked "이 프로그램은 점검이 필요합니다!")
  (test-engine-tests-disabled "테스트 기능이 꺼져있습니다.")
  (test-engine-should-be-tested "이 프로그램은 테스트가 필요합니다.")
  (test-engine-at-line-column "~a줄 ~a칸")
  (test-engine-in-at-line-column "~a 파일  ~a줄 ~a칸")
  ; as in "칸 (알수없음)"
  (test-engine-unknown "(알수없음)")
  (test-engine-trace-error "추적 오류")

  ; The ~F is special marker for the offending values, which may be
  ; printed specially in DrRacket.
  (test-engine-check-encountered-error
   "check-expect에서 기대한 값 대신 다음의 오류가 발생했습니다. ~F. ~n   :: ~a")
  (test-engine-actual-value-differs-error
   "결과값 ~F이(가) 기대한 값 ~F와(과) 다릅니다.")
  (test-engine-actual-value-not-within-error
   "결과값 ~F은(는) ~v (기대한 값 ~F) 안에 없습니다..")
  (test-engine-encountered-error-error
   "check-error에서 기대와 달리 다음의 오류를 발생했습니다. ~a~n   :: ~a")
  (test-engine-expected-error-error
   "check-error에서 기대했던 다음의 오류 대신 다음의 값을 받았습니다. ~F.~n ~a")
  ;; members are appended to the message
  (test-engine-not-mem-error "결과값 ~F 은(는) 주어진 값들과 모두 달랐습니다.")
  (test-engine-not-range-error "결과값 ~F 은(는) ~F 에서 ~F까지 안에 포함되지 못했습니다.")

  ;; followed by list of variable bindings
  (test-engine-property-fail-error "속성이 잘못되었습니다:") ; 확인
  (test-engine-property-error-error "check-property에서 다음의 오류가 발생했습니다. ~n:: ~a")

  ; section header
  (test-engine-check-failures "점검 실패:")
  ; section header
  (test-engine-signature-violations "서명 위반:") ;확인

  ; part of one phrase "signature <at line ...> to blame: procedure <...>
  (test-engine-signature "서명") ; 확인
  (test-engine-to-blame "책임 : 처리 절차")

  (test-engine-no-signature-violations "서명 점검 성공.")
  (test-engine-1-signature-violation "1개 서명 위반.")
  (test-engine-n-signature-violations "~a개 서명 위반.")

  ; as in got <value>, signature <at ...>
  (test-engine-got "맞습니까?") ; 확인

  (profjWizward-insert-java-class "자바 클래스 삽입")
  (profjWizard-insert-java-union "자바 공용체(union) 삽입")
  
  ;; The Test Suite Tool
  ;; Errors
  (test-case-empty-error "테스트 케이스가 비어 있습니다.")
  (test-case-too-many-expressions-error "너무 많은 표현이 하나의 테스트 케이스에 있습니다.")
  ;; DrRacket window menu items
  (test-case-insert "테스트 케이스 삽입")
  (test-case-disable-all "모든 테스트 케이스 끄기")
  (test-case-enable-all "모든 테스트 케이스 켜기")
  
  ;; NOTE: The following string constants are labels of the test-case fields. The width
  ;;       of the field is determined by the length of the longest of the following three words.
  ;;       if the words are too long the test case will take up too much horizontal room and
  ;;       not look very good.
  ;; This string is the label of the expression that is being tested in a test case.
  (test-case-to-test "테스트")
  ;; This string is the label of the expression that is the expected value of the to-test expression.
  (test-case-expected "기대 값") ;확인하기 should be
  ;; This string is the label of the actual result of the to test expression.
  (test-case-actual "결과값")
  (test-case-predicate "기대 값") ;확인하기 predicate
  (test-case-should-raise "기대한 오류") ;확인하기 should raise
  ;; The label of a field of the test-case that describes the expected error message of a test case
  (test-case-error-message "오류 정보")

  (test-case-menu-title "테스트 케이스")
  (test-case-switch-to-error-box "오류 테스트 상자로 변경") ;확인하기 error test box
  (test-case-switch-to-nonerror-box "오류없는 테스트 상자로 변경") ;확인하기 nonerror test box
  (test-case-collapse "테스트 케이스를 접습니다")
  (test-case-show-actual "결과값 보기")
  (test-case-enable "테스트 케이스 켜기")
  (test-case-show-predicate "기대 값 보기") ;확인하기 predicate
  (test-case-show-error-message "오류 정보 보기")
  (test-case-convert-to-text "문자로 변경")
  
  ;; Profj Boxes
  (profjBoxes-empty-error "대화가 비어있습니다.")
  (profjBoxes-too-many-expressions-error "한 개의 상자에 너무 많은 표현이 있습니다.")
  (profjBoxes-interactions-label "대화")
  (profjBoxes-bad-java-id-error "잘못된 자바 ID")
  (profjBoxes-examples-label "예제")
  (profjBoxes-add-new-example-button "새로운 예제 추가")
  (profjBoxes-type "타입")
  ;; The Java identifier of an example of data
  (profjBoxes-name "이름")
  (profjBoxes-value "값")
  (profjBoxes-insert-java-examples "자바 예제 삽입")
  (profjBoxes-insert-java-interactions "자바 정의 삽입")

  ;; Slideshow
  (slideshow-hide-picts "중첩된 상자 보기")
  (slideshow-show-picts "그림 보기")
  (slideshow-cannot-show-picts "그림을 열수 없습니다; 우선 크기를 캐쉬하도록 프로그램을 실행하십시오.") ; 다시 확인
  (slideshow-insert-pict-box "그림 상자 삽입")

  ;; GUI Tool
  (gui-tool-heading "GUI 도구")
  (gui-tool-before-clicking-message "툴 아이콘을 클릭하기 이전에 \"Special\"안의 \"GUI 삽입\" menu을 이용해 최상위 GUI 아이템을 추가하거나 이미 삽입된 GUI를 선택하십시오.")
  (gui-tool-show-gui-toolbar "GUI 툴바 보기")
  (gui-tool-hide-gui-toolbar "GUI 툴바 감추기")
  (gui-tool-insert-gui "GUI 삽입")

  ;; contract violation tracking
  
  ; tooltip for new planet icon in drscheme window (must have a planet violation logged to see it)
  (show-planet-contract-violations "PLaneT 계약 위반")

  ; buttons in the dialog that lists the recorded bug reports
  (bug-track-report "파일 티켓")
  (bug-track-forget "그만두기"); 확인하기 forget
  (bug-track-forget-all "모두 그만두기")
    
  ;; planet status messages in the bottom of the drscheme window; the ~a is filled with the name of the package
  (planet-downloading "PLaneT: ~a 내려받는 중...")
  (planet-installing "PLaneT: ~a 설치 중...")
  (planet-finished "PLaneT: ~a 완료.")
  (planet-docs-building "PLaneT: ~a - docs 만드는 중...")
  (planet-no-status "PLaneT") ;; this can happen when there is status shown in a different and then the user switches to a tab where planet hasn't been used
  
  ;; string normalization. To see this, paste some text with a ligature into DrRacket
  ;; the first three strings are in the dialog that appears. The last one is in the preferences dialog
  (normalize "표준화한다")
  (leave-alone "그대로둔다")
  (normalize-string-info "붙인 문자열에 합자 또는 표준화 되지 않은 문자가 포함되어있습니다. 이 문자열을 표준화하시겠습니까?")
  (normalize-string-preference "붙인 문자열을 표준화한다") ;확인하기 실행버튼 찾아보고 재현해볼 것
  (ask-about-normalizing-strings "문자열을 표준화하시겠습니까?")
  ) 
