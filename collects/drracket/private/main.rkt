#lang racket/unit

(require string-constants
         mzlib/contract
         "drsig.rkt"
         "frame-icon.rkt"
         mred
         framework
         mzlib/class
         racket/list
         racket/path
         racket/file
         racket/dict
         racket/set
         browser/external
         setup/plt-installer)

(import [prefix drracket:app: drracket:app^]
        [prefix drracket:unit: drracket:unit^]
        [prefix drracket:get/extend: drracket:get/extend^]
        [prefix drracket:language-configuration: drracket:language-configuration/internal^]
        [prefix drracket:language: drracket:language^]
        [prefix drracket:module-language: drracket:module-language/int^]
        [prefix drracket:tools: drracket:tools^]
        [prefix drracket:debug: drracket:debug^]
        [prefix drracket:frame: drracket:frame^]
        [prefix drracket:font: drracket:font^]
        [prefix drracket:modes: drracket:modes^]
        [prefix drracket:help-desk: drracket:help-desk^]
        [prefix drracket:multi-file-search: drracket:multi-file-search^])
(export)

(define (drr:set-default name val predicate)
  (preferences:set-default 
   name val predicate 
   #:aliases (list (string->symbol (regexp-replace #rx"^drracket:" (symbol->string name) "drscheme:")))))

(frame:current-icon todays-icon)
  
(application-file-handler
 (let ([default (application-file-handler)])
   (λ (name)
     (if (null? (get-top-level-windows))
         (handler:edit-file name)
         (default name)))))

(application-quit-handler
 (let ([default (application-quit-handler)])
   (λ ()
     (if (null? (get-top-level-windows))
         (when (exit:user-oks-exit)
           (exit:exit))
         (default)))))

(application-about-handler
 (λ ()
   (drracket:app:about-drscheme)))

(drracket:modes:add-initial-modes)

(finder:default-filters
 `(["Racket Sources" "*.rkt;*.scrbl;*.rktl;*.rktd;*.ss;*.scm"]
   ,@(finder:default-filters)))

(application:current-app-name (string-constant drscheme))

(preferences:set-default 'drracket:logger-receiver-string "error debug@GC debug@PLaneT" string?)
(preferences:set-default 'drracket:logger-scroll-to-bottom? #t boolean?)

(preferences:set-default 'drracket:submodules-to-choose-from 
                         '((main) (test)) 
                         (cons/c (list/c 'main)
                                 (cons/c (list/c 'test)
                                         (listof (listof symbol?)))))
(preferences:set-default 'drracket:defs/ints-labels #t boolean?)

(drr:set-default 'drracket:language-dialog:hierlist-default #f (λ (x) (or (not x) (and (list? x) (andmap string? x)))))
(preferences:set-default 'drracket:language-dialog:teaching-hierlist-default #f (λ (x) (or (not x) (and (list? x) (andmap string? x)))))

(drr:set-default 'drracket:create-executable-gui-type 'stand-alone (λ (x) (memq x '(launcher stand-alone distribution))))
(drr:set-default 'drracket:create-executable-gui-base 'racket (λ (x) (memq x '(racket gracket))))

(drr:set-default 'drracket:logger-gui-tab-panel-level 0 (λ (x) (and (exact-integer? x) (<= 0 x 5)))) 

(drr:set-default 'drracket:saved-bug-reports 
                         '() 
                         (λ (ll) 
                           (and (list? ll)
                                (andmap
                                 (λ (l)
                                   (and (list? l)
                                        (andmap (λ (x) (and (pair? x)
                                                            (symbol? (car x))
                                                            (string? (cdr x))))
                                                l)))
                                 ll))))

(drr:set-default 'drracket:module-language-first-line-special? #t boolean?)

(drr:set-default 'drracket:defns-popup-sort-by-name? #f boolean?)
(drr:set-default 'drracket:show-line-numbers? #f boolean?)

(drr:set-default 'drracket:toolbar-state 
                 '(#f . top)
                 (λ (x) (and (pair? x)
                             (boolean? (car x))
                             (memq (cdr x) '(left top top-no-label right)))))

(drr:set-default 'drracket:htdp:last-set-teachpacks
                         '() 
                         (λ (x)
                           (and (list? x)
                                (andmap (λ (x)
                                          (and (list? x)
                                               (pair? x)
                                               (eq? (car x) 'lib)
                                               (andmap string? (cdr x))))
                                        x))))
(drr:set-default 'drracket:defs/ints-horizontal #f boolean?)

(drr:set-default 'drracket:child-only-memory-limit (* 1024 1024 128)
                         (λ (x) (or (boolean? x)
                                    (exact-integer? x)
                                    (x . >= . (* 1024 1024 8)))))

(drr:set-default 'drracket:recent-language-names 
                         null 
                         (λ (x) 
                           (and (list? x) 
                                (andmap 
                                 (λ (x)
                                   (and (pair? x)
                                        (string? (car x))))
                                 x))))
(drr:set-default 'drracket:show-interactions-on-execute #t boolean?)
(drr:set-default 'drracket:open-in-tabs #f boolean?)
(drr:set-default 'drracket:toolbar-shown #t boolean?)
(drr:set-default 'drracket:user-defined-keybindings
                         '()
                         (λ (x) (and (list? x) 
                                     (andmap (λ (x) (or (path? x) (drracket:frame:planet-spec? x)))
                                             x))))
(drr:set-default 'drracket:install-plt-dialog
                         '(#t "" "") ; url-selected?, url string, file string
                         (λ (x) (and (list? x) (= 3 (length x))
                                     (boolean? (car x))
                                     (andmap string? (cdr x)))))

(preferences:set-un/marshall 
 'drracket:user-defined-keybindings
 (λ (in) (map (λ (x) (if (path? x) (path->bytes x) x))
              in))
 (λ (ex) (if (list? ex)
             (map (λ (x) (if (bytes? x) (bytes->path x) x)) ex)
             '())))

(let ([number-between-zero-and-one?
       (λ (x) (and (number? x) (<= 0 x 1)))])
  (drr:set-default 'drracket:unit-window-size-percentage 
                           1/2 
                           number-between-zero-and-one?)
  (drr:set-default 'drracket:module-browser-size-percentage
                           1/5
                           number-between-zero-and-one?)
  (drr:set-default 'drracket:logging-size-percentage
                           3/4
                           number-between-zero-and-one?))

(drr:set-default 'drracket:module-browser:name-length 1 
                         (λ (x) (memq x '(0 1 2 3))))

(let ([frame-width 600]
      [frame-height 650]
      [window-trimming-upper-bound-width 20]
      [window-trimming-upper-bound-height 50])
  (let-values ([(w h) (get-display-size)])
    (set! frame-width (min frame-width (- w window-trimming-upper-bound-width)))
    (set! frame-height (min frame-height (- h window-trimming-upper-bound-height))))
  (frame:setup-size-pref 'drracket:window-size 
                         frame-width
                         frame-height 
                         #:position-preferences
                         'drracket:window-position))

(drr:set-default 'drracket:backtrace-window-width 400 number?)
(drr:set-default 'drracket:backtrace-window-height 300 number?)
(drr:set-default 'drracket:backtrace-window-x 0 number?)
(drr:set-default 'drracket:backtrace-window-y 0 number?)

(drr:set-default 'drracket:profile-how-to-count 'time
                         (λ (x)
                           (memq x '(time count))))
(drr:set-default 'drracket:profile:low-color
                         (make-object color% 150 255 150)
                         (λ (x) (is-a? x color%)))
(drr:set-default 'drracket:profile:high-color
                         (make-object color% 255 150 150)
                         (λ (x) (is-a? x color%)))
(drr:set-default 'drracket:profile:scale
                         'linear
                         (λ (x) (memq x '(sqrt linear square))))

(drr:set-default 'drracket:test-coverage-ask-about-clearing? #t boolean?)

;; size is in editor positions
(drr:set-default 'drracket:repl-buffer-size 
                         '(#t . 1000)
                         (λ (x)
                           (and (pair? x)
                                (boolean? (car x))
                                (integer? (cdr x))
                                (<= 1 (cdr x) 10000))))

(let ([marshall-color 
       (λ (c)
         (list (send c red) (send c green) (send c blue)))]
      [unmarshall-color
       (λ (l)
         (if (and (list? l) 
                  (= 3 (length l))
                  (andmap (λ (x) (and number? (<= 0 x 255)))
                          l))
             (make-object color% (car l) (cadr l) (caddr l))
             (make-object color% 0 0 0)))])
  (preferences:set-un/marshall 
   'drracket:profile:low-color
   marshall-color
   unmarshall-color)
  (preferences:set-un/marshall 
   'drracket:profile:high-color
   marshall-color
   unmarshall-color))

(drr:set-default 
 'drracket:keybindings-window-size
 (cons 400 600)
 (λ (x) (and (pair? x)
             (number? (car x))
             (number? (cdr x)))))

(drr:set-default
 'drracket:execute-warning-once
 #f
 (λ (x)
   (or (eq? x #t)
       (not x))))

(drr:set-default 'drracket:switch-to-module-language-automatically? #t boolean?)

(drr:set-default
 'drracket:default-tools-configuration
 'load
 (lambda (p)
   (memq p '(load skip))))

(drr:set-default
 'drracket:tools-configuration
 null
 list?)

  (drr:set-default 'drracket:module-overview:label-font-size 12 number?)
  (drr:set-default 'drracket:module-overview:window-height 500 number?)
  (drr:set-default 'drracket:module-overview:window-width 500 number?)
  (drr:set-default 'drracket:module-browser:hide-paths '(lib)
                           (λ (x)
                             (and (list? x)
                                  (andmap symbol? x))))
  

(drracket:font:setup-preferences)
(color-prefs:add-background-preferences-panel)
(racket:add-preferences-panel)
(racket:add-coloring-preferences-panel)
(preferences:add-editor-checkbox-panel)
(preferences:add-warnings-checkbox-panel)
(preferences:add-scheme-checkbox-panel)
(preferences:add-general-checkbox-panel)

(let ([make-check-box
       (λ (pref-sym string parent [extra-functionality #f])
         (let ([q (make-object check-box%
                    string
                    parent
                    (λ (checkbox evt)
                      (define value (send checkbox get-value))
                      (preferences:set pref-sym value)
                      (when extra-functionality
                        (extra-functionality value))))])
           (preferences:add-callback pref-sym (λ (p v) (send q set-value v)))
           (send q set-value (preferences:get pref-sym))))])
  (preferences:add-to-general-checkbox-panel
   (λ (editor-panel)
     (make-check-box 'drracket:open-in-tabs 
                     (string-constant open-files-in-tabs)
                     editor-panel)
     

     (make-check-box 'drracket:show-interactions-on-execute 
                     (string-constant show-interactions-on-execute)
                     editor-panel)
     
     (make-check-box 'drracket:switch-to-module-language-automatically?
                     (string-constant switch-to-module-language-automatically)
                     editor-panel)
     
     (make-check-box 'drracket:defs/ints-horizontal
                     (string-constant interactions-beside-definitions)
                     editor-panel)

     (make-check-box 'drracket:module-language-first-line-special?
                     (string-constant ml-always-show-#lang-line)
                     editor-panel)))
  
  (preferences:add-to-editor-checkbox-panel
   (λ (editor-panel)
     (make-check-box 'drracket:show-line-numbers?
                     (string-constant show-line-numbers)
                     editor-panel)
     
     (make-check-box 'drracket:defs/ints-labels
                     (string-constant show-defs/ints-label)
                     editor-panel)
     
     ;; come back to this one.
     #;
     (letrec ([hp (new horizontal-panel% 
                       (parent editor-panel)
                       (alignment '(left top))
                       (stretchable-height #f))]
              [cb (new check-box%
                       (label (string-constant limit-interactions-size))
                       (parent hp)
                       (callback (λ (cb v) (cb-callback))))]
              [sl (new slider% 
                       (label #f)
                       (parent hp)
                       (min-value 1)
                       (max-value 10000)
                       (callback
                        (λ (sl _) (sl-callback))))]
              [cb-callback
               (λ ()
                 (preferences:set 'drracket:repl-buffer-size
                                  (cons (send cb get-value)
                                        (cdr (preferences:get 'drracket:repl-buffer-size)))))]
              [sl-callback
               (λ ()
                 (preferences:set 'drracket:repl-buffer-size
                                  (cons (car (preferences:get 'drracket:repl-buffer-size))
                                        (send sl get-value))))]
              [update-controls
               (λ (v)
                 (let ([on? (car v)])
                   (send sl enable on?)
                   (send cb set-value on?)
                   (send sl set-value (cdr v))))])
       (preferences:add-callback 'drracket:repl-buffer-size (λ (p v) (update-controls v)))
       (update-controls (preferences:get 'drracket:repl-buffer-size)))))
  
  (preferences:add-to-warnings-checkbox-panel
   (λ (warnings-panel)
     (make-check-box 'drracket:execute-warning-once 
                     (string-constant only-warn-once)
                     warnings-panel)
     (make-check-box 'drracket:test-coverage-ask-about-clearing?
                     (string-constant test-coverage-ask?)
                     warnings-panel)
     (make-check-box 'drracket:show-killed-dialog
                     (string-constant show-evaluation-terminated-dialog)
                     warnings-panel))))

(drracket:debug:add-prefs-panel)
(install-help-browser-preference-panel)
(drracket:tools:add-prefs-panel)

(drracket:language:register-capability 'drscheme:tabify-menu-callback 
                                       (or/c false/c (-> (is-a?/c text%) number? number? void?))
                                       (λ (t a b) (send t tabify-selection a b)))
(drracket:language:register-capability 'drscheme:autocomplete-words (listof string?) '())
(drracket:language:register-capability 'drscheme:define-popup
                                       (or/c (cons/c string? string?) 
                                             (list/c string? string? string?)
                                             #f)
                                       (list "(define" "(define ...)" "δ"))

;; The default is #f to keep whatever the user chose as their context.
;; If it's "", then we will kill the user's choice.
(drracket:language:register-capability 'drscheme:help-context-term
                                       (or/c false/c string?)
                                       #f)

(drracket:language:register-capability 'drscheme:special:insert-fraction (flat-contract boolean?) #t)
(drracket:language:register-capability 'drscheme:special:insert-large-letters (flat-contract boolean?) #t)
(drracket:language:register-capability 'drscheme:special:insert-lambda (flat-contract boolean?) #t)
(drracket:language:register-capability 'drscheme:special:insert-image (flat-contract boolean?) #t)
(drracket:language:register-capability 'drscheme:special:insert-comment-box (flat-contract boolean?) #t)
(drracket:language:register-capability 'drscheme:language-menu-title 
                                       (flat-contract string?)
                                       (string-constant scheme-menu-name))

(drracket:language:register-capability 'drscheme:teachpack-menu-items
                                       (or/c false/c (flat-contract drracket:unit:teachpack-callbacks?))
                                       #f)

(handler:current-create-new-window
 (let ([drscheme-current-create-new-window
        (λ (filename)
          (drracket:unit:open-drscheme-window filename))])
   drscheme-current-create-new-window))

;; add a catch-all handler to open drscheme files
(handler:insert-format-handler 
 "Units"
 (λ (filename) #t)
 drracket:unit:open-drscheme-window)

;; add a handler to open .plt files.
(handler:insert-format-handler
 "PLT Files"
 (λ (filename)
   (and (regexp-match? #rx"^(?i:plt)$"
                       (or (filename-extension filename) #""))
        (gui-utils:get-choice
         (format (string-constant install-plt-file) filename)
         (string-constant install-plt-file/yes)
         (string-constant install-plt-file/no))))
 (λ (filename)
   (run-installer filename)
   #f))

;; trim old console-previous-exprs preferences to compenstate 
;; for a bug that let it grow without bound
(let* ([max-len 30]
       [trim (λ (exprs save)
               (when (list? exprs)
                 (let ([len (length exprs)])
                   (when (> len max-len)
                     (save (drop exprs (- len max-len)))))))])
  (let ([framework-prefs (get-preference 'plt:framework-prefs #:timeout-lock-there (λ (x) #f))])
    (when (and (list? framework-prefs)
               (andmap pair? framework-prefs))
      (let ([exprs-pref (assq 'drscheme:console-previous-exprs framework-prefs)])
        (when exprs-pref
          (trim (second exprs-pref)
                (λ (trimmed)
                  (put-preferences (list 'plt:framework-prefs)
                                   (list (dict-set framework-prefs 'drscheme:console-previous-exprs (list trimmed)))
                                   void)))))))
  (trim (get-preference 'plt:framework-pref:drscheme:console-previous-exprs #:timeout-lock-there (λ (x) #f))
        (λ (trimmed)
          (put-preferences (list 'plt:framework-pref:drscheme:console-previous-exprs)
                           (list trimmed)
                           void))))

(drracket:tools:load/invoke-all-tools
 (λ () (void))
 (λ () 
   (drracket:language-configuration:add-built-in-languages)
   (drracket:module-language:add-module-language)
   (drracket:language-configuration:add-info-specified-languages)))

;; no more extension after this point
(drracket:get/extend:get-interactions-canvas)
(drracket:get/extend:get-definitions-canvas)
(drracket:get/extend:get-unit-frame)
(drracket:get/extend:get-interactions-text)
(drracket:get/extend:get-definitions-text)
(drracket:language-configuration:get-languages)

;; this default can only be set *after* the
;; languages have all be registered by tools
(drr:set-default
 drracket:language-configuration:settings-preferences-symbol
 (drracket:language-configuration:get-default-language-settings)
 drracket:language-configuration:language-settings?)

;; if the unmarshaller returns #f, that will fail the
;; test for this preference, reverting back to the default.
;; In that case, the default is specified in the pref.rkt file
;; of the default collection and may not be the default
;; specified above (of course).
(preferences:set-un/marshall
 drracket:language-configuration:settings-preferences-symbol
 (λ (x)
   (let ([lang (drracket:language-configuration:language-settings-language x)]
         [settings (drracket:language-configuration:language-settings-settings x)])
     (list (send lang get-language-numbers)
           (send lang marshall-settings settings))))
 (λ (x)
   (and (list? x)
        (= 2 (length x))
        (let* ([lang-nums (first x)]
               [marshalled-settings (second x)]
               [lang (ormap
                      (λ (x)
                        (and (or (equal? (send x get-language-numbers) lang-nums)
                                 
                                 ;; this second branch of the `or' corresdponds
                                 ;; to preferences saved from earlier versions of
                                 ;; drscheme, for a sort of backwards compatibility
                                 (equal? (send x get-language-position) lang-nums))
                             x))
                      (drracket:language-configuration:get-languages))])
          (and lang
               (let ([settings (send lang unmarshall-settings marshalled-settings)])
                 (drracket:language-configuration:language-settings
                  lang
                  (or settings (send lang default-settings)))))))))

  (preferences:set-default 'drracket:online-compilation-default-on #t boolean?)
  (preferences:set-default 'drracket:online-expansion:read-in-defs-errors 
                           'margin
                           (or/c 'margin 'gold))
  (preferences:set-default 'drracket:online-expansion:variable-errors 
                           'margin
                           (or/c 'margin 'gold))
  (preferences:set-default 'drracket:online-expansion:other-errors
                           'margin
                           (or/c 'margin 'gold))
  
  (preferences:set-default 'drracket:show-killed-dialog #t boolean?)
  
  (drr:set-default 'drracket:multi-file-search:recur? #t boolean?)
  (drr:set-default 'drracket:multi-file-search:filter? #t boolean?)
  (drr:set-default 'drracket:multi-file-search:filter-regexp "\\.(rkt.?|scrbl|ss|scm)$" string?)
  (drr:set-default 'drracket:multi-file-search:search-string "" string?)
  (drr:set-default 'drracket:multi-file-search:search-type
                           1
                           (λ (x) 
                             (and (number? x)
                                  (exact? x)
                                  (integer? x)
                                  (<= 0 x)
                                  (< x (length drracket:multi-file-search:search-types)))))
  
  ;; drracket:mult-file-search:search-check-boxes : (listof (listof boolean))
  (drr:set-default 'drracket:multi-file-search:search-check-boxes 
                           (map (λ (x) (map cdr (drracket:multi-file-search:search-type-params x)))
                                drracket:multi-file-search:search-types)
                           (λ (x) 
                             (and (list? x)
                                  (andmap (λ (x)
                                            (and (list? x)
                                                 (andmap boolean? x)))
                                          x))))
  
  (drr:set-default 'drracket:multi-file-search:percentages
                           '(1/3 2/3)
                           (λ (x) (and (list? x)
                                       (= 2 (length x))
                                       (= 1 (apply + x)))))
  
  (drr:set-default 'drracket:multi-file-search:frame-size '(300 . 400) 
                           (λ (x) (and (pair? x)
                                       (number? (car x))
                                       (number? (cdr x)))))
  (drr:set-default 'drracket:multi-file-search:directories 
                   '()
                   (lambda (x) (and (list? x) (andmap string? x))))
  (drr:set-default 'drracket:multi-file-search:directory 
                           ;; The default is #f because
                           ;; filesystem-root-list is expensive under Windows
                           #f 
                           (lambda (x) (or (not x) (string? x))))

  
  (drr:set-default 'drracket:large-letters-font #f (λ (x)
                                                       (or (and (pair? x)
                                                                (string? (car x))
                                                                (let ([i (cdr x)])
                                                                  (and (integer? i)
                                                                       (<= 1 i 255))))
                                                           (not x))))
  (drr:set-default 'drracket:module-language:auto-text "#lang racket\n" string?)

(let ([drs-handler-recent-items-super%
       (class (drracket:frame:basics-mixin
               (frame:standard-menus-mixin
                frame:basic%))
         (define/override (edit-menu:between-select-all-and-find menu)
           (void))
         (super-new))])
  (handler:set-recent-items-frame-superclass drs-handler-recent-items-super%))

(cond
  [(current-eventspace-has-menu-root?)
   (drracket:frame:create-root-menubar)
   (preferences:set 'framework:exit-when-no-frames #f)]
  [else
   (preferences:set 'framework:exit-when-no-frames #t)]) 


(define repl-error-pref 'drracket:read-eval-print-loop:error-color)
(define repl-out-pref 'drracket:read-eval-print-loop:out-color)
(define repl-value-pref 'drracket:read-eval-print-loop:value-color)
(color-prefs:register-color-preference repl-value-pref
                                       "text:ports value"
                                       (make-object color% 0 0 175)
                                       (make-object color% 57 89 216))
(color-prefs:register-color-preference repl-error-pref
                                       "text:ports err"
                                       (let ([sd (make-object style-delta% 'change-italic)])
                                         (send sd set-delta-foreground (make-object color% 255 0 0))
                                         sd))
(color-prefs:register-color-preference repl-out-pref
                                       "text:ports out"
                                       (make-object color% 150 0 150)
                                       (make-object color% 192 46 214))
(color-prefs:add-to-preferences-panel 
 (string-constant repl-colors)
 (λ (parent)
   (color-prefs:build-color-selection-panel parent
                                            repl-value-pref
                                            "text:ports value"
                                            (string-constant repl-value-color))
   (color-prefs:build-color-selection-panel parent
                                            repl-error-pref
                                            "text:ports err"
                                            (string-constant repl-error-color))
   (color-prefs:build-color-selection-panel parent
                                            repl-out-pref
                                            "text:ports out"
                                            (string-constant repl-out-color))))


(define test-coverage-on-style-pref (string->symbol drracket:debug:test-coverage-on-style-name))
(define test-coverage-off-style-pref (string->symbol drracket:debug:test-coverage-off-style-name))

(color-prefs:register-color-preference test-coverage-on-style-pref
                                       drracket:debug:test-coverage-on-style-name
                                       (send the-color-database find-color "forest green"))
(color-prefs:register-color-preference test-coverage-off-style-pref
                                       drracket:debug:test-coverage-off-style-name
                                       (send the-color-database find-color "maroon"))
(color-prefs:add-to-preferences-panel 
 "Module Language"
 (λ (parent)
   (color-prefs:build-color-selection-panel parent
                                            test-coverage-on-style-pref
                                            drracket:debug:test-coverage-on-style-name
                                            (string-constant test-coverage-on))
   (color-prefs:build-color-selection-panel parent
                                            test-coverage-off-style-pref
                                            drracket:debug:test-coverage-off-style-name
                                            (string-constant test-coverage-off))))

(drracket:module-language:initialize-prefs-panel)

(let* ([find-frame
        (λ (item)
          (let loop ([item item])
            (cond
              [(is-a? item top-level-window<%>)
               (and (is-a? item drracket:unit:frame%)
                    item)]
              [(is-a? item menu-item<%>)
               (loop (send item get-parent))]
              [(is-a? item menu-bar%)
               (loop (send item get-frame))]
              [else #f])))]
       [dc
        (λ (item)
          (let ([frame (find-frame item)])
            (send item enable (and frame (> (length (send frame get-tabs)) 1)))))])
  (group:add-to-windows-menu
   (λ (windows-menu)
     (define sprefix (if (eq? (system-type) 'windows)
                         (cons 'shift (get-default-shortcut-prefix))
                         (get-default-shortcut-prefix)))
     (new menu-item%
          [parent windows-menu]
          [label (string-constant prev-tab)]
          [shortcut #\[]
          [shortcut-prefix sprefix]
          [demand-callback dc]
          [callback (λ (item _) 
                      (let ([frame (find-frame item)])
                        (when frame
                          (send frame prev-tab))))])
     (new menu-item% 
          [parent windows-menu]
          [label (string-constant next-tab)]
          [shortcut #\]]
          [shortcut-prefix sprefix]
          [demand-callback dc]
          [callback (λ (item _) 
                      (let ([frame (find-frame item)])
                        (when frame
                          (send frame next-tab))))])
     
     (let ([frame (find-frame windows-menu)])
       (unless (or (not frame) (= 1 (send frame get-tab-count)))
         (unless (eq? (system-type) 'macosx)
           (new separator-menu-item% [parent windows-menu]))
         (for ([i (in-range 0 (send frame get-tab-count))]
               #:when (< i 9))
           (new menu-item% 
                [parent windows-menu]
                [label (format (string-constant tab-i)
                               (+ i 1)
                               (send frame get-tab-filename i))]
                [shortcut (integer->char (+ (char->integer #\1) i))]
                [callback
                 (λ (a b)
                   (send frame change-to-nth-tab i))]))))
     
     (when (eq? (system-type) 'macosx)
       (new separator-menu-item% [parent windows-menu])))))

;; Check for any files lost last time.
;; Ignore the framework's empty frames test, since
;;   the autosave information window may appear and then
;;   go away (leaving no frames temporarily) but we are
;;   not going to be exiting yet.
(autosave:restore-autosave-files/gui)

;; the initial window doesn't set the 
;; unit object's state correctly, yet.
(define (make-basic)
  (let* ([frame (drracket:unit:open-drscheme-window)]
         [interactions-edit (send frame get-interactions-text)]
         [definitions-edit (send frame get-interactions-text)]
         [filename (send definitions-edit get-filename)])
    (unless filename
      (send frame update-shown)
      (send (send frame get-interactions-canvas) focus))
    (send frame show #t)))

;; FIXME: get this from racket/list ?
(define (remove-duplicates files)
  (let loop ([files files])
    (cond
      [(null? files) null]
      [else (if (member (car files) (cdr files))
                (loop (cdr files))
                (cons (car files) (loop (cdr files))))])))

;; Queue a callback here to open the first frame
;; and install the user's keybindings so that the modules
;; that are being loaded by drracket are all finished.
;; This makes sure that drracket exports
;; are all set up a) in case a user keybinding file uses
;; them, and b) before we trigger the dynamic 
;; requires that can happen when the module language looks
;; at the #lang line (which can end up loading drracket itself
;; in a bad way leading to errors like this:
;;   link: reference (phase 0) to a variable in module: ...
;;   that is uninitialized (phase level 0); 
;;   reference appears in module: ...)

(queue-callback
 (λ ()
   
   ;; install user's keybindings
   (for-each drracket:frame:add-keybindings-item 
             (preferences:get 'drracket:user-defined-keybindings))
   
   ;; NOTE: drscheme-normal.rkt sets current-command-line-arguments to
   ;; the list of files to open, after parsing out flags like -h
   (let* ([files-to-open 
           (if (preferences:get 'drracket:open-in-tabs)
               (vector->list (current-command-line-arguments))
               (reverse (vector->list (current-command-line-arguments))))]
          [normalized/filtered
           (let loop ([files files-to-open])
             (cond
               [(null? files) null]
               [else (let ([file (car files)])
                       (if (file-exists? file)
                           (cons (normalize-path file) (loop (cdr files)))
                           (begin
                             (message-box
                              (string-constant drscheme)
                              (format (string-constant cannot-open-because-dne) file))
                             (loop (cdr files)))))]))]
          [no-dups (remove-duplicates normalized/filtered)]
          [frames
           (map (λ (f) (handler:edit-file
                        f
                        (λ () (drracket:unit:open-drscheme-window f))))
                no-dups)])
     (when (null? (filter (λ (x) x) frames))
       (make-basic))
     (when (and (preferences:get 'drracket:open-in-tabs)
                (not (null? no-dups)))
       (handler:edit-file (car no-dups))))))
