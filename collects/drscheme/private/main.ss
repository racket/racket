#lang scheme/unit

(require string-constants
         mzlib/contract
         "drsig.ss"
         mred
         framework
         mzlib/class
         mzlib/list
         racket/path
         browser/external
         setup/plt-installer)

(import [prefix drscheme:app: drscheme:app^]
        [prefix drscheme:unit: drscheme:unit^]
        [prefix drscheme:get/extend: drscheme:get/extend^]
        [prefix drscheme:language-configuration: drscheme:language-configuration/internal^]
        [prefix drscheme:language: drscheme:language^]
        [prefix drscheme:module-language: drscheme:module-language^]
        [prefix drscheme:tools: drscheme:tools^]
        [prefix drscheme:debug: drscheme:debug^]
        [prefix drscheme:frame: drscheme:frame^]
        [prefix drscheme:font: drscheme:font^]
        [prefix drscheme:modes: drscheme:modes^]
        [prefix drscheme:help-desk: drscheme:help-desk^])
(export)



(when (eq? (system-type) 'unix)
  (let ()
    ;; avoid building the mask unless we use it
    (define todays-icon
      (make-object bitmap% 
        (build-path (collection-path "icons")
                    (case (date-week-day (seconds->date (current-seconds)))
                      [(6 0) "plt-logo-red-shiny.png"]
                      [else "plt-logo-red-diffuse.png"]))
        'png/mask))
    
    (define todays-icon-bw-mask 
      (and (send todays-icon ok?)
           (send todays-icon get-loaded-mask)
           (let* ([w (send todays-icon get-width)]
                  [h (send todays-icon get-height)]
                  [bm (make-object bitmap% w h #t)]
                  [color-mask (send todays-icon get-loaded-mask)]
                  [src-bytes (make-bytes (* w h 4) 0)]
                  [dest-bits (make-bytes (* w h 4) 255)]
                  [bdc (make-object bitmap-dc% bm)]
                  [black (send the-color-database find-color "black")]
                  [white (send the-color-database find-color "white")])
             (send color-mask get-argb-pixels 0 0 w h src-bytes #t)
             (for ([i (in-range 0 w)])
               (for ([j (in-range 0 h)])
                 (let ([b (= (bytes-ref src-bytes (* 4 (+ i (* j h)))) 0)])
                   (send bdc set-pixel i j (if b white black)))))
             (send bdc set-bitmap #f)
             bm)))
    
    (send todays-icon set-loaded-mask todays-icon-bw-mask)
    (frame:current-icon todays-icon)))
  
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
   (drscheme:app:about-drscheme)))

(drscheme:modes:add-initial-modes)

(namespace-set-variable-value! 'help-desk:frame-mixin drscheme:frame:basics-mixin)

(finder:default-filters (list* '("Scheme (.ss)" "*.ss")
                               '("Scheme (.scm)" "*.scm")
                               (finder:default-filters)))
(application:current-app-name (string-constant drscheme))

(preferences:set-default 'drscheme:logger-gui-tab-panel-level 0 (λ (x) (and (exact-integer? x) (<= 0 x 5)))) 

(preferences:set-default 'drscheme:saved-bug-reports 
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

(preferences:set-default 'drscheme:module-language-first-line-special? #t boolean?)

(preferences:set-default 'drscheme:defns-popup-sort-by-name? #f boolean?)

(preferences:set-default 'drscheme:toolbar-state 
                         '(#f . top)
                         (λ (x) (and (pair? x)
                                     (boolean? (car x))
                                     (memq (cdr x) '(left top right)))))

(preferences:set-default 'drscheme:htdp:last-set-teachpacks
                         '() 
                         (λ (x)
                           (and (list? x)
                                (andmap (λ (x)
                                          (and (list? x)
                                               (pair? x)
                                               (eq? (car x) 'lib)
                                               (andmap string? (cdr x))))
                                        x))))
(preferences:set-default 'drscheme:defs/ints-horizontal #f boolean?)
(preferences:set-default 'drscheme:unit-window-max? #f boolean?)
(preferences:set-default 'drscheme:frame:initial-position #f 
                         (λ (x) (or (not x)
                                    (and (pair? x)
                                         (number? (car x))
                                         (number? (cdr x))))))

(preferences:set-default 'drscheme:child-only-memory-limit (* 1024 1024 128)
                         (λ (x) (or (boolean? x)
                                    (integer? x)
                                    (x . >= . (* 1024 1024 1)))))

(preferences:set-default 'drscheme:recent-language-names 
                         null 
                         (λ (x) 
                           (and (list? x) 
                                (andmap 
                                 (λ (x)
                                   (and (pair? x)
                                        (string? (car x))))
                                 x))))
(preferences:set-default 'drscheme:show-interactions-on-execute #t boolean?)
(preferences:set-default 'drscheme:open-in-tabs #f boolean?)
(preferences:set-default 'drscheme:toolbar-shown #t boolean?)
(preferences:set-default 'drscheme:user-defined-keybindings
                         '()
                         (λ (x) (and (list? x) 
                                     (andmap (λ (x) (or (path? x) (drscheme:frame:planet-spec? x)))
                                             x))))
(preferences:set-default 'drscheme:install-plt-dialog
                         '(#t "" "") ; url-selected?, url string, file string
                         (λ (x) (and (list? x) (= 3 (length x))
                                     (boolean? (car x))
                                     (andmap string? (cdr x)))))

(preferences:set-un/marshall 
 'drscheme:user-defined-keybindings
 (λ (in) (map (λ (x) (if (path? x) (path->bytes x) x))
              in))
 (λ (ex) (if (list? ex)
             (map (λ (x) (if (bytes? x) (bytes->path x) x)) ex)
             '())))

(let ([number-between-zero-and-one?
       (λ (x) (and (number? x) (<= 0 x 1)))])
  (preferences:set-default 'drscheme:unit-window-size-percentage 
                           1/2 
                           number-between-zero-and-one?)
  (preferences:set-default 'drscheme:module-browser-size-percentage
                           1/5
                           number-between-zero-and-one?)
  (preferences:set-default 'drscheme:logging-size-percentage
                           3/4
                           number-between-zero-and-one?))

(preferences:set-default 'drscheme:module-browser:name-length 1 
                         (λ (x) (memq x '(0 1 2 3))))

(let ([frame-width 600]
      [frame-height 650]
      [window-trimming-upper-bound-width 20]
      [window-trimming-upper-bound-height 50])
  (let-values ([(w h) (get-display-size)])
    (set! frame-width (min frame-width (- w window-trimming-upper-bound-width)))
    (set! frame-height (min frame-height (- h window-trimming-upper-bound-height))))
  (preferences:set-default 'drscheme:unit-window-width frame-width number?)
  (preferences:set-default 'drscheme:unit-window-height frame-height number?))

(preferences:set-default 'drscheme:backtrace-window-width 400 number?)
(preferences:set-default 'drscheme:backtrace-window-height 300 number?)
(preferences:set-default 'drscheme:backtrace-window-x 0 number?)
(preferences:set-default 'drscheme:backtrace-window-y 0 number?)

(preferences:set-default 'drscheme:profile-how-to-count 'time
                         (λ (x)
                           (memq x '(time count))))
(preferences:set-default 'drscheme:profile:low-color
                         (make-object color% 150 255 150)
                         (λ (x) (is-a? x color%)))
(preferences:set-default 'drscheme:profile:high-color
                         (make-object color% 255 150 150)
                         (λ (x) (is-a? x color%)))
(preferences:set-default 'drscheme:profile:scale
                         'linear
                         (λ (x) (memq x '(sqrt linear square))))

(preferences:set-default 'drscheme:test-coverage-ask-about-clearing? #t boolean?)

;; size is in editor positions
(preferences:set-default 'drscheme:repl-buffer-size 
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
   'drscheme:profile:low-color
   marshall-color
   unmarshall-color)
  (preferences:set-un/marshall 
   'drscheme:profile:high-color
   marshall-color
   unmarshall-color))

(preferences:set-default 
 'drscheme:keybindings-window-size
 (cons 400 600)
 (λ (x) (and (pair? x)
             (number? (car x))
             (number? (cdr x)))))

(preferences:set-default
 'drscheme:execute-warning-once
 #f
 (λ (x)
   (or (eq? x #t)
       (not x))))

(preferences:set-default 'drscheme:switch-to-module-language-automatically? #t boolean?)

(preferences:set-default
 'drscheme:default-tools-configuration
 'load
 (lambda (p)
   (memq p '(load skip))))

(preferences:set-default
 'drscheme:tools-configuration
 null
 list?)


(drscheme:font:setup-preferences)
(color-prefs:add-background-preferences-panel)
(scheme:add-preferences-panel)
(scheme:add-coloring-preferences-panel)
(preferences:add-editor-checkbox-panel)
(preferences:add-warnings-checkbox-panel)
(preferences:add-scheme-checkbox-panel)
(preferences:add-general-checkbox-panel)

(let ([make-check-box
       (λ (pref-sym string parent)
         (let ([q (make-object check-box%
                    string
                    parent
                    (λ (checkbox evt)
                      (preferences:set 
                       pref-sym 
                       (send checkbox get-value))))])
           (preferences:add-callback pref-sym (λ (p v) (send q set-value v)))
           (send q set-value (preferences:get pref-sym))))])
  (preferences:add-to-general-checkbox-panel
   (λ (editor-panel)
     (make-check-box 'drscheme:open-in-tabs 
                     (string-constant open-files-in-tabs)
                     editor-panel)
     (make-check-box 'drscheme:show-interactions-on-execute 
                     (string-constant show-interactions-on-execute)
                     editor-panel)
     
     (make-check-box 'drscheme:switch-to-module-language-automatically?
                     (string-constant switch-to-module-language-automatically)
                     editor-panel)
     
     (make-check-box 'drscheme:defs/ints-horizontal
                     (string-constant interactions-beside-definitions)
                     editor-panel)
     
     (make-check-box 'drscheme:module-language-first-line-special?
                     (string-constant ml-always-show-#lang-line)
                     editor-panel)))
  
  (preferences:add-to-editor-checkbox-panel
   (λ (editor-panel)
     (void)
     
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
                 (preferences:set 'drscheme:repl-buffer-size
                                  (cons (send cb get-value)
                                        (cdr (preferences:get 'drscheme:repl-buffer-size)))))]
              [sl-callback
               (λ ()
                 (preferences:set 'drscheme:repl-buffer-size
                                  (cons (car (preferences:get 'drscheme:repl-buffer-size))
                                        (send sl get-value))))]
              [update-controls
               (λ (v)
                 (let ([on? (car v)])
                   (send sl enable on?)
                   (send cb set-value on?)
                   (send sl set-value (cdr v))))])
       (preferences:add-callback 'drscheme:repl-buffer-size (λ (p v) (update-controls v)))
       (update-controls (preferences:get 'drscheme:repl-buffer-size)))))
  
  (preferences:add-to-warnings-checkbox-panel
   (λ (warnings-panel)
     (make-check-box 'drscheme:execute-warning-once 
                     (string-constant only-warn-once)
                     warnings-panel)
     (make-check-box 'drscheme:test-coverage-ask-about-clearing?
                     (string-constant test-coverage-ask?)
                     warnings-panel))))
(drscheme:debug:add-prefs-panel)
(install-help-browser-preference-panel)
(drscheme:tools:add-prefs-panel)

(drscheme:language:register-capability 'drscheme:tabify-menu-callback 
                                       (or/c false/c (-> (is-a?/c text%) number? number? void?))
                                       (λ (t a b) (send t tabify-selection a b)))
(drscheme:language:register-capability 'drscheme:autocomplete-words (listof string?) '())
(drscheme:language:register-capability 'drscheme:define-popup
                                       (or/c (cons/c string? string?) 
                                             (list/c string? string? string?)
                                             #f)
                                       (list "(define" "(define ...)" "δ"))

;; The default is #f to keep whatever the user chose as their context.
;; If it's "", then we will kill the user's choice.
(drscheme:language:register-capability 'drscheme:help-context-term
                                       (or/c false/c string?)
                                       #f)

(drscheme:language:register-capability 'drscheme:special:insert-fraction (flat-contract boolean?) #t)
(drscheme:language:register-capability 'drscheme:special:insert-large-letters (flat-contract boolean?) #t)
(drscheme:language:register-capability 'drscheme:special:insert-lambda (flat-contract boolean?) #t)
(drscheme:language:register-capability 'drscheme:special:insert-image (flat-contract boolean?) #t)
(drscheme:language:register-capability 'drscheme:special:insert-comment-box (flat-contract boolean?) #t)
(drscheme:language:register-capability 'drscheme:language-menu-title 
                                       (flat-contract string?)
                                       (string-constant scheme-menu-name))

(drscheme:language:register-capability 'drscheme:teachpack-menu-items
                                       (or/c false/c (flat-contract drscheme:unit:teachpack-callbacks?))
                                       #f)

(handler:current-create-new-window
 (let ([drscheme-current-create-new-window
        (λ (filename)
          (drscheme:unit:open-drscheme-window filename))])
   drscheme-current-create-new-window))

;; add a catch-all handler to open drscheme files
(handler:insert-format-handler 
 "Units"
 (λ (filename) #t)
 drscheme:unit:open-drscheme-window)

;; add a handler to open .plt files.
(handler:insert-format-handler 
 "PLT Files"
 (λ (filename)
   (let ([ext (filename-extension filename)])
     (and ext
          (or (bytes=? #"PLT" ext)
              (bytes=? #"plt" ext))
          (gui-utils:get-choice 
           (format (string-constant install-plt-file) filename)
           (string-constant install-plt-file/yes)
           (string-constant install-plt-file/no)))))
 (λ (filename)
   (run-installer filename)
   #f))

(drscheme:tools:load/invoke-all-tools
 (λ () (void))
 (λ () 
   (drscheme:language-configuration:add-built-in-languages)
   (drscheme:module-language:add-module-language)
   (drscheme:language-configuration:add-info-specified-languages)))

;; no more extension after this point
(drscheme:get/extend:get-interactions-canvas)
(drscheme:get/extend:get-definitions-canvas)
(drscheme:get/extend:get-unit-frame)
(drscheme:get/extend:get-interactions-text)
(drscheme:get/extend:get-definitions-text)
(drscheme:language-configuration:get-languages)

;; this default can only be set *after* the
;; languages have all be registered by tools
(preferences:set-default
 drscheme:language-configuration:settings-preferences-symbol
 (drscheme:language-configuration:get-default-language-settings)
 drscheme:language-configuration:language-settings?)

;; if the unmarshaller returns #f, that will fail the
;; test for this preference, reverting back to the default.
;; In that case, the default is specified in the pref.ss file
;; of the default collection and may not be the default
;; specified above (of course).
(preferences:set-un/marshall
 drscheme:language-configuration:settings-preferences-symbol
 (λ (x)
   (let ([lang (drscheme:language-configuration:language-settings-language x)]
         [settings (drscheme:language-configuration:language-settings-settings x)])
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
                      (drscheme:language-configuration:get-languages))])
          (and lang
               (let ([settings (send lang unmarshall-settings marshalled-settings)])
                 (drscheme:language-configuration:make-language-settings
                  lang
                  (or settings (send lang default-settings)))))))))

(let ([drs-handler-recent-items-super%
       (class (drscheme:frame:basics-mixin
               (frame:standard-menus-mixin
                frame:basic%))
         (define/override (edit-menu:between-select-all-and-find menu)
           (void))
         (super-new))])
  (handler:set-recent-items-frame-superclass drs-handler-recent-items-super%))

(cond
  [(current-eventspace-has-menu-root?)
   (drscheme:frame:create-root-menubar)
   (preferences:set 'framework:exit-when-no-frames #f)]
  [else
   (preferences:set 'framework:exit-when-no-frames #t)])


(let* ([sl (editor:get-standard-style-list)]
       [sd (make-object style-delta%)])
  (send sd set-delta-foreground (make-object color% 255 0 0))
  (send sl new-named-style 
        "drscheme:text:ports err"
        (send sl find-or-create-style
              (send sl find-named-style "text:ports err")
              sd)))  
(define repl-error-pref 'drscheme:read-eval-print-loop:error-color)
(define repl-out-pref 'drscheme:read-eval-print-loop:out-color)
(define repl-value-pref 'drscheme:read-eval-print-loop:value-color)
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

(let* ([find-frame
        (λ (item)
          (let loop ([item item])
            (cond
              [(is-a? item top-level-window<%>)
               (and (is-a? item drscheme:unit:frame%)
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
     (new menu-item%
          [parent windows-menu] [label (string-constant prev-tab)] [shortcut #\[]
          [demand-callback dc]
          [callback (λ (item _) 
                      (let ([frame (find-frame item)])
                        (when frame
                          (send frame prev-tab))))])
     (new menu-item% [parent windows-menu] [label (string-constant next-tab)] [shortcut #\]]
          [demand-callback dc]
          [callback (λ (item _) 
                      (let ([frame (find-frame item)])
                        (when frame
                          (send frame next-tab))))])
     (let ([frame (find-frame windows-menu)])
       (unless (or (not frame) (= 1 (send frame get-tab-count)))
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
     (new separator-menu-item% [parent windows-menu]))))

;; Check for any files lost last time.
;; Ignore the framework's empty frames test, since
;;   the autosave information window may appear and then
;;   go away (leaving no frames temporarily) but we are
;;   not going to be exiting yet.
(autosave:restore-autosave-files/gui)

;; install user's keybindings
(for-each drscheme:frame:add-keybindings-item 
          (preferences:get 'drscheme:user-defined-keybindings))

;; the initial window doesn't set the 
;; unit object's state correctly, yet.
(define (make-basic)
  (let* ([frame (drscheme:unit:open-drscheme-window)]
         [interactions-edit (send frame get-interactions-text)]
         [definitions-edit (send frame get-interactions-text)]
         [filename (send definitions-edit get-filename)])
    (unless filename
      (send frame update-shown)
      (send (send frame get-interactions-canvas) focus))
    (send frame show #t)))

(define (remove-duplicates files)
  (let loop ([files files])
    (cond
      [(null? files) null]
      [else (if (member (car files) (cdr files))
                (loop (cdr files))
                (cons (car files) (loop (cdr files))))])))

;; NOTE: drscheme-normal.ss sets current-command-line-arguments to
;; the list of files to open, after parsing out flags like -h
(let* ([files-to-open 
        (if (preferences:get 'drscheme:open-in-tabs)
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
                     (λ () (drscheme:unit:open-drscheme-window f))))
             no-dups)])
  (when (null? (filter (λ (x) x) frames))
    (make-basic))
  (when (and (preferences:get 'drscheme:open-in-tabs)
             (not (null? no-dups)))
    (handler:edit-file (car no-dups))))
