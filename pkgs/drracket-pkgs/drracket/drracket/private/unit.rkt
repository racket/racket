#lang racket/base

(require racket/contract
         racket/unit
         racket/class
         racket/path
         racket/port
         racket/list
         racket/match
         string-constants
         framework
         mrlib/name-message
         mrlib/bitmap-label
         mrlib/include-bitmap
         mrlib/switchable-button
         mrlib/cache-image-snip
         (prefix-in image-core: mrlib/image-core)
         mrlib/include-bitmap
         mrlib/close-icon
         net/sendurl
         net/url
         
         drracket/private/drsig
         "auto-language.rkt"
         "insert-large-letters.rkt"
         "get-defs.rkt"
         "local-member-names.rkt"
         "eval-helpers-and-pref-init.rkt"
         "parse-logger-args.rkt"
         "get-module-path.rkt"
         "named-undefined.rkt"
         (prefix-in drracket:arrow: "../arrow.rkt")
         (prefix-in icons: images/compile-time)
         mred
         (prefix-in mred: mred)
         
         mzlib/date
         
         framework/private/aspell
         framework/private/logging-timer
         
         setup/collects
         scribble/xref
         setup/xref
         scribble/tag
         (only-in scribble/base doc-prefix))

(provide unit@)

(define module-browser-progress-constant (string-constant module-browser-progress))
(define status-compiling-definitions (string-constant module-browser-compiling-defns))
(define show-lib-paths (string-constant module-browser-show-lib-paths/short))
(define show-planet-paths (string-constant module-browser-show-planet-paths/short))
(define refresh (string-constant module-browser-refresh))

(define oprintf
  (let ([op (current-output-port)])
    (λ args
      (apply fprintf op args))))

;; ===================================================================================================
;; Compiled bitmaps

(require (for-syntax
          racket/base
          (prefix-in icons: (combine-in images/icons/file images/icons/control images/icons/style
                                        images/icons/stickman images/logos))))

(define execute-bitmap
  (icons:compiled-bitmap (icons:play-icon #:color icons:run-icon-color
                                          #:height (icons:toolbar-icon-height))))
(define break-bitmap
  (icons:compiled-bitmap (icons:stop-icon #:color icons:halt-icon-color
                                          #:height (icons:toolbar-icon-height))))
(define small-save-bitmap
  (icons:compiled-bitmap (icons:small-save-icon #:height (icons:toolbar-icon-height))))
(define save-bitmap
  (icons:compiled-bitmap (icons:save-icon #:height (icons:toolbar-icon-height))))

(begin-for-syntax
  (define stickman-height 18)
  (define num-running-frames 12))

(define running-frame-list
  (icons:compiled-bitmap-list
   (for/list ([t  (in-range 0 1 (/ 1 num-running-frames))])
     (icons:running-stickman-icon t #:height stickman-height))))
(define running-frames (list->vector running-frame-list))

(define standing-frame
  (icons:compiled-bitmap
   (icons:standing-stickman-icon #:height stickman-height)))

(define very-small-planet-bitmap
  (icons:compiled-bitmap (icons:planet-logo #:height (icons:toolbar-icon-height))))

;; ===================================================================================================

(define-unit unit@
  (import [prefix help-desk: drracket:help-desk^]
          [prefix drracket:app: drracket:app^]
          [prefix drracket:frame: drracket:frame^]
          [prefix drracket:text: drracket:text^]
          [prefix drracket:rep: drracket:rep/int^]
          [prefix drracket:language-configuration: drracket:language-configuration/internal^]
          [prefix drracket:language: drracket:language^]
          [prefix drracket:get/extend: drracket:get/extend^]
          [prefix drracket:module-overview: drracket:module-overview^]
          [prefix drracket:tools: drracket:tools^]
          [prefix drracket:init: drracket:init^]
          [prefix drracket:module-language: drracket:module-language/int^]
          [prefix drracket:module-language-tools: drracket:module-language-tools^]
          [prefix drracket:modes: drracket:modes^]
          [prefix drracket:debug: drracket:debug^]
          [prefix drracket: drracket:interface^])
  (export (rename drracket:unit/int^ [-frame% frame%]))
  (init-depend drracket:module-language/int^)

  (define struct:teachpack-callbacks struct:drracket:unit:teachpack-callbacks)
  (define teachpack-callbacks? drracket:unit:teachpack-callbacks?)
  (define teachpack-callbacks-get-names drracket:unit:teachpack-callbacks-get-names)
  (define teachpack-callbacks-add drracket:unit:teachpack-callbacks-add)
  (define teachpack-callbacks-remove drracket:unit:teachpack-callbacks-remove)
  (define teachpack-callbacks-remove-all drracket:unit:teachpack-callbacks-remove-all)
  (define make-teachpack-callbacks drracket:unit:teachpack-callbacks)
  (define teachpack-callbacks drracket:unit:teachpack-callbacks)
  
  (keymap:add-to-right-button-menu
   (let ([old (keymap:add-to-right-button-menu)])
     (λ (menu text event)
       (old menu text event)
       (when (and (is-a? text text%)
                  (or (is-a? text (get-definitions-text%))
                      (is-a? text drracket:rep:text%))
                  (is-a? event mouse-event%))
         
         (let ([add-sep
                (let ([added? #f])
                  (λ ()
                    (unless added?
                      (set! added? #t)
                      (new separator-menu-item% [parent menu]))))])
           
           (add-search-help-desk-menu-item text menu
                                           (let-values ([(x y)
                                                         (send text dc-location-to-editor-location
                                                               (send event get-x)
                                                               (send event get-y))])
                                             (send text find-position x y))
                                           add-sep)
           
           (when (is-a? text editor:basic<%>)
             (let-values ([(pos text) (send text get-pos/text event)])
               (when (and pos (is-a? text text%))
                 (send text split-snip pos)
                 (send text split-snip (+ pos 1))
                 (let ([snip (send text find-snip pos 'after-or-none)])
                   (when (or (is-a? snip image-snip%)
                             (is-a? snip image-core:image%)
                             (is-a? snip cache-image-snip%))
                     (add-sep)
                     (define (save-image-callback _1 _2)
                       (define fn
                         (put-file #f 
                                   (send text get-top-level-window)
                                   #f "untitled.png" "png"))
                       (when fn
                         (define kind (filename->kind fn))
                         (cond
                           [kind
                            (cond
                              [(or (is-a? snip image-snip%)
                                   (is-a? snip cache-image-snip%))
                               (send (send snip get-bitmap) save-file fn kind)]
                              [else
                               (image-core:save-image-as-bitmap snip fn kind)])]
                           [else
                            (message-box 
                             (string-constant drscheme)
                             "Must choose a filename that ends with either .png, .jpg, .xbm, or .xpm"
                             #:dialog-mixin frame:focus-table-mixin)])))
                     (new menu-item%
                          [parent menu]
                          [label (string-constant save-image)]
                          [callback save-image-callback]))))))
           
           (void))))))
  
  (define (add-search-help-desk-menu-item text menu position [add-sep void])
    (let* ([end (send text get-end-position)]
           [start (send text get-start-position)])
      (unless (= 0 (send text last-position))
        (let* ([str (if (= end start)
                        (find-symbol text position)
                        (send text get-text start end))]
               ;; almost the same code as "search-help-desk" in "rep.rkt"
               [l (send text get-canvas)]
               [l (and l (send l get-top-level-window))]
               [l (and l (is-a? l drracket:unit:frame<%>) (send l get-definitions-text))]
               [l (and l (send l get-next-settings))]
               [l (and l (drracket:language-configuration:language-settings-language l))]
               [ctxt (and l (send l capability-value 'drscheme:help-context-term))]
               [name (and l (send l get-language-name))])
          (unless (string=? str "")
            (add-sep)
            (let ([short-str (shorten-str str 50)])
              (make-object menu-item%
                (gui-utils:format-literal-label
                 (string-constant search-help-desk-for) 
                 (if (equal? short-str str)
                     str
                     (string-append short-str "...")))
                menu
                (λ x (help-desk:help-desk str (list ctxt name))))
              (void)))))))
  
  (define (filename->kind fn)
    (let ([ext (filename-extension fn)])
      (and ext
           (let ([sym (string->symbol (bytes->string/utf-8 ext))])
             (ormap (λ (pr) (and (equal? sym (car pr)) (cadr pr)))
                    allowed-extensions)))))
  
  (define allowed-extensions '((png png)
                               (jpg jpeg)
                               (xbm xbm)
                               (xpm xpm)))
  
  
  
  ;; find-symbol : number -> string
  ;; finds the symbol around the position `pos' (approx)
  (define (find-symbol text pos)
    (cond
      [(and (is-a? text racket:text<%>)
            (not (send text is-stopped?)))
       (let* ([before (send text get-backward-sexp pos)]
              [before+ (and before (send text get-forward-sexp before))]
              [after (send text get-forward-sexp pos)]
              [after- (and after (send text get-backward-sexp after))])
         
         (define (get-tokens start end)
           (let loop ([i start])
             (cond
               [(and (< i end)
                     (< i (send text last-position)))
                (define-values (tstart tend) (send text get-token-range i))
                (cons (list (send text classify-position i) tstart tend)
                      (loop tend))]
               [else '()])))
         
         ;; find-searchable-tokens : number number -> (or/c #f (list symbol number number))
         (define (find-searchable-tokens start end)
           (define tokens (get-tokens start end))
           (for/or ([tok tokens])
             (define type (list-ref tok 0))
             (cond [(or (equal? type 'symbol)
                        (equal? type 'hash-colon-keyword)
                        ;; The token may have been categorized as a keyword due to
                        ;; its presence in the tabification preferences:
                        (equal? type 'keyword))
                    tok]
                   [else
                    #f])))
         
         (define searchable-token 
           (or (and before before+ 
                    (<= before pos before+)
                    (find-searchable-tokens before before+))
               (and after after- 
                    (<= after- pos after)
                    (find-searchable-tokens after- after))))
         (if searchable-token
             (send text get-text (list-ref searchable-token 1) (list-ref searchable-token 2))
             ""))]
      [else
       (send text split-snip pos)
       (send text split-snip (+ pos 1))
       (let ([snip (send text find-snip pos 'after)])
         (if (is-a? snip string-snip%)
             (let* ([before
                     (let loop ([i (- pos 1)]
                                [chars null])
                       (if (< i 0)
                           chars
                           (let ([char (send text get-character i)])
                             (if (non-letter? char)
                                 chars
                                 (loop (- i 1)
                                       (cons char chars))))))]
                    [after
                     (let loop ([i pos])
                       (if (< i (send text last-position))
                           (let ([char (send text get-character i)])
                             (if (non-letter? char)
                                 null
                                 (cons char (loop (+ i 1)))))
                           null))])
               (apply string (append before after)))
             ""))]))
  
  ;; non-letter? : char -> boolean
  ;; returns #t if the character belongs in a symbol (approx) and #f it is
  ;; a divider between symbols (approx)
  (define (non-letter? x)
    (or (char-whitespace? x)
        (memq x '(#\` #\' #\, #\; #\"
                      #\{ #\( #\[ #\] #\) #\}))))      
  (define (shorten-str str len)
    (if ((string-length str) . <= . len)
        str
        (substring str 0 len)))
  
  
  ;                                                                                              
  ;                                                                                              
  ;                                                                                              
  ;    ;;;                         ;                           ;   ;          ;                  
  ;   ;                                                        ;              ;                  
  ;   ;                       ;                                ;              ;                  
  ;  ;;;;  ; ;  ;;;     ;;;  ;;;;  ;    ;;;    ; ;;         ;; ;   ;   ;;;    ;    ;;;     ;; ;  
  ;   ;    ;;  ;   ;   ;   ;  ;    ;   ;   ;   ;;  ;       ;  ;;   ;  ;   ;   ;   ;   ;   ;  ;;  
  ;   ;    ;       ;  ;       ;    ;  ;     ;  ;   ;      ;    ;   ;      ;   ;  ;     ; ;    ;  
  ;   ;    ;    ;;;;  ;       ;    ;  ;     ;  ;   ;      ;    ;   ;   ;;;;   ;  ;     ; ;    ;  
  ;   ;    ;   ;   ;  ;       ;    ;  ;     ;  ;   ;      ;    ;   ;  ;   ;   ;  ;     ; ;    ;  
  ;   ;    ;   ;   ;   ;   ;  ;    ;   ;   ;   ;   ;       ;  ;;   ;  ;   ;   ;   ;   ;   ;  ;;  
  ;   ;    ;    ;;;;;   ;;;    ;;  ;    ;;;    ;   ;        ;; ;   ;   ;;;;;  ;    ;;;     ;; ;  
  ;                                                                                           ;  
  ;                                                                                      ;    ;  
  ;                                                                                       ;;;;   
  
  (define (get-fraction-from-user parent)
    (define dlg (make-object dialog% (string-constant enter-fraction)))
    (define hp (make-object horizontal-panel% dlg))
    (make-object message% (string-constant whole-part) hp)
    (define whole
      (keymap:call/text-keymap-initializer
       (λ ()
         (make-object text-field% #f hp void))))
    (define vp (make-object vertical-panel% hp))
    (define hp2 (make-object horizontal-panel% vp))
    (define num 
      (keymap:call/text-keymap-initializer
       (λ ()
         (make-object text-field% #f hp2 void))))
    (define num-m (make-object message% (string-constant numerator) hp2))
    (define hp3 (make-object horizontal-panel% vp))
    (define den       
      (keymap:call/text-keymap-initializer
       (λ ()
         (make-object text-field% #f hp3 void))))
    (define den-m (make-object message% (string-constant denominator) hp3))
    (define bp (make-object horizontal-panel% dlg))
    (define ok? #f)
    (define (validate-number)
      (define num-s (string->number (send num get-value)))
      (define den-s (string->number (send den get-value)))
      (define whole-s (if (string=? (send whole get-value) "")
                          0
                          (string->number (send whole get-value))))
      (cond
        [(or (not whole-s) (not (integer? whole-s)))
         (string-constant insert-number/bad-whole-part)]
        [(or (not num-s) (not (integer? num-s)) (< num-s 0))
         (string-constant insert-number/bad-numerator)]
        [(or (not den-s) (not (integer? den-s)) (<= den-s 0))
         (string-constant insert-number/bad-denominator)]
        [else
         (if (< whole-s 0)
             (- whole-s (/ num-s den-s))
             (+ whole-s (/ num-s den-s)))]))
    (define (ok-callback)
      (define v (validate-number))
      (cond
        [(number? v)
         (set! ok? #t)
         (send dlg show #f)]
        [else 
         (message-box
          (string-constant drscheme)
          v
          dlg
          #:dialog-mixin frame:focus-table-mixin)]))
    (define (cancel-callback) (send dlg show #f))
    (define-values (ok cancel) 
      (gui-utils:ok/cancel-buttons
       bp
       (λ (x y) (ok-callback))
       (λ (x y) (cancel-callback))))
    (let ([mw (max (send den-m get-width) (send num-m get-width))])
      (send den-m min-width mw)
      (send num-m min-width mw))
    (send bp set-alignment 'right 'center)
    (send dlg show #t)
    (and ok?
         (let ([v (validate-number)])
           (and (number? v)
                v))))
  
  ;; create-executable : (instanceof drracket:unit:frame<%>) -> void
  (define (create-executable frame)
    (define definitions-text (send frame get-definitions-text))
    (define program-filename (send definitions-text get-filename))
    (define settings (send definitions-text get-next-settings))
    (cond
      [(not (drracket:language-configuration:language-allows-executable-creation?
             (drracket:language-configuration:language-settings-language settings)))
       (message-box (string-constant drscheme)
                    (string-constant drracket-creates-executables-only-in-some-languages)
                    frame
                    #:dialog-mixin frame:focus-table-mixin)]
      [(not program-filename)
       (message-box (string-constant create-executable-title)
                    (string-constant must-save-before-executable)
                    frame
                    #:dialog-mixin frame:focus-table-mixin)]
      [else
       (when (or (not (send definitions-text is-modified?))
                 (gui-utils:get-choice
                  (string-constant definitions-not-saved)
                  (string-constant yes)
                  (string-constant no)
                  (string-constant drscheme)
                  #f
                  frame))
         (send (drracket:language-configuration:language-settings-language settings)
               create-executable
               (drracket:language-configuration:language-settings-settings settings)
               frame
               program-filename))]))
  
  (define-values (get-program-editor-mixin add-to-program-editor-mixin)
    (let* ([program-editor-mixin
            (mixin (editor:basic<%> (class->interface text%)) () 
              (init-rest args) 
              (inherit get-top-level-window) 
              
              (define/private (reset-highlighting) 
                (let ([f (get-top-level-window)]) 
                  (when (and f 
                             (is-a? f drracket:unit:frame<%>)) 
                    (let ([interactions-text (send f get-interactions-text)]) 
                      (when (object? interactions-text) 
                        (send interactions-text reset-highlighting)))))) 
              
              (define/augment (after-insert x y) 
                (reset-highlighting) 
                (inner (void) after-insert x y)) 
              
              (define/augment (after-delete x y) 
                (reset-highlighting) 
                (inner (void) after-delete x y)) 
              
              (apply super-make-object args))]
           [get-program-editor-mixin
            (λ ()
              (drracket:tools:only-in-phase 'drracket:unit:get-program-editor-mixin
                                            'phase2
                                            'init-complete)
              program-editor-mixin)]
           [add-to-program-editor-mixin
            (λ (mixin)
              (drracket:tools:only-in-phase 'drracket:unit:add-to-program-editor-mixin 'phase1)
              (let ([old program-editor-mixin])
                (set! program-editor-mixin (λ (x) (mixin (old x))))))])
      (values get-program-editor-mixin
              add-to-program-editor-mixin)))
  
  ;; this sends a message to its frame when it gets the focus
  (define make-searchable-canvas%
    (λ (%)
      (class %
        (inherit get-top-level-window)
        (define/override (on-focus on?)
          (when on?
            (send (get-top-level-window) make-searchable this))
          (super on-focus on?))
        (super-new))))
  
  (define interactions-canvas% 
    (class (make-searchable-canvas%
            (canvas:info-mixin
             (canvas:wide-snip-mixin
              (canvas:info-mixin
               canvas:color%))))
      (init [style '()])
      (super-new (style (cons 'auto-hscroll style)))))
  
  
  (define definitions-canvas%
    (class (make-searchable-canvas% (canvas:delegate-mixin (canvas:info-mixin canvas:color%)))
      (init [style '()])
      (super-new (style (cons 'auto-hscroll style)))))
  
  ;                                                                                                  
  ;                                                                                                  
  ;                                                                                                  
  ;       ;           ;;;            ;        ;                                                      
  ;       ;          ;    ;                                                                          
  ;       ;          ;                   ;                                   ;                   ;   
  ;    ;; ;    ;;;  ;;;;;;;  ; ;;    ;  ;;;;  ;    ;;;    ; ;;     ;;;      ;;;;   ;;;  ;     ; ;;;; 
  ;   ;  ;;   ;   ;  ;    ;  ;;  ;   ;   ;    ;   ;   ;   ;;  ;   ;          ;    ;   ;  ;   ;   ;   
  ;  ;    ;  ;    ;  ;    ;  ;   ;   ;   ;    ;  ;     ;  ;   ;   ;;         ;   ;    ;   ; ;    ;   
  ;  ;    ;  ;;;;;;  ;    ;  ;   ;   ;   ;    ;  ;     ;  ;   ;    ;;        ;   ;;;;;;    ;     ;   
  ;  ;    ;  ;       ;    ;  ;   ;   ;   ;    ;  ;     ;  ;   ;      ;       ;   ;        ; ;    ;   
  ;   ;  ;;   ;      ;    ;  ;   ;   ;   ;    ;   ;   ;   ;   ;      ;       ;    ;      ;   ;   ;   
  ;    ;; ;    ;;;;  ;    ;  ;   ;   ;    ;;  ;    ;;;    ;   ;   ;;;         ;;   ;;;; ;     ;   ;; 
  ;                                                                                                  
  ;                                                                                                  
  ;                                                                                                  
  
  
  (define get-definitions-text%
    (let ([definitions-text% #f])
      (λ ()
        (drracket:tools:only-in-phase 'phase2 'init-complete)
        (unless definitions-text%
          (set! definitions-text% (make-definitions-text%)))
        definitions-text%)))
  
  (define (show-line-numbers?)
    (preferences:get 'drracket:show-line-numbers?))
  
  (define (make-definitions-text%)
    (let ([definitions-super%
            (text:line-numbers-mixin
             (text:first-line-mixin
              (drracket:module-language:module-language-put-file-mixin
               (racket:text-mixin
                (color:text-mixin
                 (drracket:rep:drs-bindings-keymap-mixin
                  (mode:host-text-mixin
                   (text:delegate-mixin
                    (text:foreground-color-mixin
                     (drracket:rep:drs-autocomplete-mixin
                      (λ (x) x)
                      (text:normalize-paste-mixin
                       (text:column-guide-mixin
                        text:info%))))))))))))])
       ((get-program-editor-mixin)
        (class* definitions-super% (drracket:unit:definitions-text<%>)
          (inherit get-top-level-window is-locked? lock while-unlocked
                   highlight-first-line is-printing?)
          
          (define interactions-text #f)
          (define/public (set-interactions-text it)
            (set! interactions-text it))
          
          (define tab #f)
          (define/public (get-tab) tab)
          (define/public (set-tab t) (set! tab t))
          
          (inherit begin-edit-sequence end-edit-sequence
                   delete insert last-position paragraph-start-position
                   get-character)
          
          (define save-file-metadata #f)
          
          (define/pubment (begin-metadata-changes)
            (set! ignore-edits? #t)
            (inner (void) begin-metadata-changes))
          (define/pubment (end-metadata-changes)
            (set! ignore-edits? #f)
            (inner (void) end-metadata-changes))
          
          (define/augment (on-save-file filename fmt)
            (inner (void) on-save-file filename fmt)
            (define lang (drracket:language-configuration:language-settings-language next-settings))
            (define settings (drracket:language-configuration:language-settings-settings 
                              next-settings))
            (define name-mod (send lang get-reader-module))
            (when name-mod
              ;; the reader-module method's result is used a test of whether or
              ;; not the get-metadata method is used for this language
              (let ([metadata (send lang get-metadata (filename->modname filename) settings)])
                (begin-edit-sequence #f #f)
                (begin-metadata-changes)
                (let ([locked? (is-locked?)])
                  (when locked? (lock #f))
                  (set! save-file-metadata metadata)
                  (while-unlocked
                   (λ ()
                     (insert metadata 0 0)))
                  (when locked? (lock #t))))))
          (define/private (filename->modname filename)
            (let-values ([(base name dir) (split-path filename)])
              (string->symbol (regexp-replace #rx"\\.[^.]*$"
                                              (path->string name)
                                              ""))))
          
          (define/augment (after-save-file success?)
            (when success?
              (let ([filename (get-filename)])
                (when filename
                  ;; if a filesystem error happens, just give up
                  ;; on setting the file creator and type.
                  (with-handlers ([exn:fail:filesystem? void])
                    (let-values ([(creator type) (file-creator-and-type filename)])
                      (file-creator-and-type filename #"DrSc" type))))))
            (when save-file-metadata
              (let ([modified? (is-modified?)]
                    [locked? (is-locked?)])
                (when locked? (lock #f))
                (while-unlocked
                 (λ ()
                   (delete 0 (string-length save-file-metadata))))
                (when locked? (lock #t))
                (set! save-file-metadata #f)
                ;; restore modification status to where it was before the metadata is removed
                (set-modified modified?)
                (end-metadata-changes)
                (end-edit-sequence)))
            (inner (void) after-save-file success?))
          
          (define/augment (on-load-file filename format)
            (inner (void) on-load-file filename format)
            (begin-edit-sequence #f #f))
          (define/augment (after-load-file success?)
            (when success?
              (let-values ([(module-language module-language-settings)
                            (get-module-language/settings)])
                (let-values ([(matching-language settings)
                              (pick-new-language
                               this
                               (drracket:language-configuration:get-languages)
                               module-language
                               module-language-settings)])
                  (cond
                    [matching-language
                     (set-next-settings
                      (drracket:language-configuration:language-settings 
                       matching-language
                       settings)
                      #f)]
                    [else
                     (define lang (drracket:language-configuration:language-settings-language
                                   (get-next-settings)))
                     (when (send lang get-reader-module)
                       (set-next-settings
                        (drracket:language-configuration:get-default-language-settings)
                        #f))])))
              (set-modified #f))
            
            (end-edit-sequence)
            (inner (void) after-load-file success?))
          
          (define/augment (on-lexer-valid valid?)
            (inner (void) on-lexer-valid valid?)
            (let ([f (get-top-level-window)])
              (when (and f
                         (is-a? f drracket:unit:frame<%>))
                (send f set-color-status! valid?))))
          
          (define/override (get-can-close-parent)
            (and tab (send tab get-frame)))
          
          (inherit is-modified? run-after-edit-sequence)
          (define/override (set-modified mod?)
            (super set-modified mod?)
            (run-after-edit-sequence
             (λ ()
               (let ([f (get-top-level-window)])
                 (when (and f
                            (is-a? f drracket:unit:frame<%>))
                   (send f update-save-button))))))
          (define/override set-filename
            (case-lambda
              [(fn) (set-filename fn #f)]
              [(fn tmp?)
               (super set-filename fn tmp?)
               (let ([f (get-top-level-window)])
                 (when (and f
                            (is-a? f drracket:unit:frame<%>))
                   (send f update-save-message)))]))
          
          (field
           [needs-execution-state #f]
           [already-warned-state #f]
           [execute-settings (preferences:get 
                              drracket:language-configuration:settings-preferences-symbol)]
           [next-settings execute-settings])
          
          (define/private (set-needs-execution-state! s) (set! needs-execution-state s))
          
          ;; get-needs-execution-message : -> (or/c string #f)
          ;; returns the current warning message if "Run" should be clicked (ie, if the
          ;; state of the REPL is out of sync with drscheme).
          (define/public (get-needs-execution-message)
            (and (not already-warned-state)
                 (or (and (not (this-and-next-language-the-same?))
                          (string-constant needs-execute-language-changed))
                     needs-execution-state)))
          
          (define/pubment (get-next-settings) next-settings)
          (define/pubment (set-next-settings _next-settings [update-prefs? #t])
            (when (or 
                   (send (drracket:language-configuration:language-settings-language _next-settings)
                         get-reader-module)
                   (send (drracket:language-configuration:language-settings-language next-settings)
                         get-reader-module))
              (set-modified #t))
            (set! next-settings _next-settings)
            (let ([f (get-top-level-window)])
              (when (and f
                         (is-a? f drracket:unit:frame<%>))
                (send f language-changed)))
            
            (highlight-first-line
             (is-a? (drracket:language-configuration:language-settings-language _next-settings)
                    drracket:module-language:module-language<%>))
            
            (let ([lang (drracket:language-configuration:language-settings-language next-settings)]
                  [sets (drracket:language-configuration:language-settings-settings next-settings)])
              (preferences:set
               'drracket:recent-language-names
               (limit-length
                (remove-duplicate-languages
                 (cons (cons (send lang get-language-name)
                             (send lang marshall-settings sets))
                       (preferences:get 'drracket:recent-language-names)))
                10)))
            
            (when update-prefs?
              (preferences:set
               drracket:language-configuration:settings-preferences-symbol
               next-settings))
            
            (remove-auto-text)
            (insert-auto-text)
            (after-set-next-settings _next-settings))
          
          (define/pubment (after-set-next-settings s)
            (inner (void) after-set-next-settings s))
          
          (define/public (this-and-next-language-the-same?)
            (define execute-lang
              (drracket:language-configuration:language-settings-language execute-settings))
            (define next-lang 
              (drracket:language-configuration:language-settings-language next-settings))
            (and (equal? (send execute-lang get-language-position)
                         (send next-lang get-language-position))
                 (equal? (send execute-lang marshall-settings 
                               (drracket:language-configuration:language-settings-settings
                                execute-settings))
                         (send execute-lang marshall-settings 
                               (drracket:language-configuration:language-settings-settings
                                next-settings)))))
          
          (define/pubment (set-needs-execution-message msg)
            (set-needs-execution-state! msg))
          (define/pubment (teachpack-changed)
            (set-needs-execution-state! (string-constant needs-execute-teachpack-changed)))
          (define/pubment (just-executed)
            (set! execute-settings next-settings)
            (set-needs-execution-state! #f)
            (send tab clear-execution-state)
            (set! already-warned-state #f))
          (define/pubment (already-warned?)
            already-warned-state)
          (define/pubment (already-warned)
            (set! already-warned-state #t))
          
          ;; the really-modified? flag determines if there 
          ;; is a modification that is not the insertion of the auto-text
          (define really-modified? #f)
          
          ;; when this flag is #t, edits to the buffer do not count as 
          ;; user's edits and so the yellow warning does not appear
          (define ignore-edits? #f)
          
          (define/augment (after-insert x y)
            (unless ignore-edits?
              (set! really-modified? #t)
              (set-needs-execution-state! (string-constant needs-execute-defns-edited)))
            (inner (void) after-insert x y))
          (define/augment (after-delete x y)
            (unless ignore-edits?
              (set! really-modified? #t)
              (set-needs-execution-state! (string-constant needs-execute-defns-edited)))
            (inner (void) after-delete x y))
          
          (define/override (is-special-first-line? l) 
            (and (preferences:get 'drracket:module-language-first-line-special?)
                 (is-lang-line? l)))
          
          (inherit get-filename)
          
          (inherit get-filename/untitled-name)
          (define/private (get-date-string)
            (string-append
             (date->string (seconds->date (current-seconds)))
             " "
             (get-filename/untitled-name)))
          
          (define/override (on-paint before dc left top right bottom dx dy draw-caret)
            (super on-paint before dc left top right bottom dx dy draw-caret)
            
            ;; [Disabled] For printing, put date and filename in the top margin:
            (when (and #f before (is-printing?))
              (let ([h (box 0)]
                    [w (box 0)])
                (send (current-ps-setup) get-editor-margin w h)
                (unless ((unbox h) . < . 2)
                  (let ([font (make-font #:size (inexact->exact (ceiling (* 1/2 (unbox h))))
                                         #:family 'modern)]
                        [old-font (send dc get-font)])
                    (send dc set-font font)
                    (send dc draw-text (get-date-string) 0 0)
                    (send dc set-font old-font)))))
            
            ;; draw the arrows
            (when before
              (when error-arrows
                (define old-pen (send dc get-pen))
                (define old-brush (send dc get-brush))
                (send dc set-brush "red" 'solid)
                (define font-size-factor
                  (cond
                    [(<= (editor:get-current-preferred-font-size) 12) 1]
                    [else (* (editor:get-current-preferred-font-size) 1/8)]))
                (define pen-width font-size-factor)
                (define arrow-head-size (* 8 font-size-factor))
                (define arrow-root-radius (* 1 font-size-factor))
                (send dc set-pen (send the-pen-list find-or-create-pen "red" 1 'solid))
                (let loop ([pts error-arrows])
                  (cond
                    [(null? pts) (void)]
                    [(null? (cdr pts)) (void)]
                    [else (define pt1 (car pts))
                          (define pt2 (cadr pts))
                          (draw-arrow dc dx dy pt1 pt2
                                      pen-width arrow-head-size arrow-root-radius)
                          (loop (cdr pts))]))
                (send dc set-pen old-pen)
                (send dc set-brush old-brush))))
          
          (define/private (draw-arrow dc dx dy pt1 pt2 pen-width arrow-head-size arrow-root-radius)
            (define-values (x1 y1) 
              (find-poss (srcloc-source pt1) (- (srcloc-position pt1) 1) (srcloc-position pt1)))
            (define-values (x2 y2)
              (find-poss (srcloc-source pt2) (- (srcloc-position pt2) 1) (srcloc-position pt2)))
            (drracket:arrow:draw-arrow dc x1 y1 x2 y2 dx dy
                                       #:pen-width pen-width
                                       #:arrow-head-size arrow-head-size
                                       #:arrow-root-radius arrow-root-radius))
          
          (inherit dc-location-to-editor-location)
          (define/private (find-poss text left-pos right-pos)
            (let ([xlb (box 0)]
                  [ylb (box 0)]
                  [xrb (box 0)]
                  [yrb (box 0)])
              (send text position-location left-pos xlb ylb #t)
              (send text position-location right-pos xrb yrb #f)
              (let*-values ([(xl-off yl-off) (send text editor-location-to-dc-location 
                                                   (unbox xlb)
                                                   (unbox ylb))]
                            [(xl yl) (dc-location-to-editor-location xl-off yl-off)]
                            [(xr-off yr-off) (send text editor-location-to-dc-location
                                                   (unbox xrb)
                                                   (unbox yrb))]
                            [(xr yr) (dc-location-to-editor-location xr-off yr-off)])
                (values (/ (+ xl xr) 2)
                        (/ (+ yl yr) 2)))))
          
          (define/public (still-untouched?)
            (and (or (= (last-position) 0) (not really-modified?))
                 (not (is-modified?))
                 (not (get-filename))))
          ;; inserts the auto-text if any
          (define/public (insert-auto-text)
            (define lang
              (drracket:language-configuration:language-settings-language
               next-settings))
            (define auto-text
              (and (not really-modified?)
                   (not (get-filename))
                   (is-a? lang drracket:module-language:module-language<%>)
                   (send lang get-auto-text
                         (drracket:language-configuration:language-settings-settings
                          next-settings))))
            (when auto-text
              (set! ignore-edits? #t)
              (begin-edit-sequence #f #f)
              (insert auto-text)
              (set-modified #f)
              (set! ignore-edits? #f)
              (end-edit-sequence)
              (set! really-modified? #f)))
          (define/private (remove-auto-text)
            (when (and (not really-modified?)
                       (not (get-filename))
                       (> (last-position) 0))
              (begin-edit-sequence #f #f)
              (send this erase)
              (set-modified #f)
              (end-edit-sequence)
              (set! really-modified? #f)))
          
          (inherit invalidate-bitmap-cache)
          (define/public (set-error-arrows arrows)
            (unless (eq? arrows error-arrows)
              (set! error-arrows arrows)
              (invalidate-bitmap-cache)))
          
          (define error-arrows #f)
          
          (super-new [show-line-numbers? (show-line-numbers?)])
          
          (highlight-first-line
           (is-a? (drracket:language-configuration:language-settings-language next-settings)
                  drracket:module-language:module-language<%>))
          (inherit set-max-undo-history)
          (set-max-undo-history 'forever)))))
  
  ;; is-lang-line? : string -> boolean
  ;; given the first line in the editor, this returns #t if it is a #lang line.
  (define (is-lang-line? l)
    (let ([m (regexp-match #rx"^#(!|(lang ))([-+_/a-zA-Z0-9]+)(.|$)" l)])
      (and m
           (let ([lang-name (list-ref m 3)]
                 [last-char (list-ref m 4)])
             (and (not (char=? #\/ (string-ref lang-name 0)))
                  (not (char=? #\/ (string-ref lang-name (- (string-length lang-name) 1))))
                  (or (string=? "" last-char)
                      (char-whitespace? (string-ref last-char 0))))))))
  
  ;; test cases for is-lang-line?
  #;
  (printf "~s\n"
          (list (is-lang-line? "#lang x")
                (is-lang-line? "#lang racket")
                (is-lang-line? "#lang racket ")
                (not (is-lang-line? "#lang racketα"))
                (not (is-lang-line? "#lang racket/ "))
                (not (is-lang-line? "#lang /racket "))
                (is-lang-line? "#lang rac/ket ")
                (is-lang-line? "#lang r6rs")
                (is-lang-line? "#!r6rs")
                (is-lang-line? "#!r6rs ")
                (not (is-lang-line? "#!/bin/sh"))))
  
  (define (get-module-language/settings)
    (let* ([module-language
            (and (preferences:get 'drracket:switch-to-module-language-automatically?)
                 (ormap 
                  (λ (lang)
                    (and (is-a? lang drracket:module-language:module-language<%>)
                         lang))
                  (drracket:language-configuration:get-languages)))]
           [module-language-settings
            (let ([prefs-setting (preferences:get 
                                  drracket:language-configuration:settings-preferences-symbol)])
              (cond
                [(eq? (drracket:language-configuration:language-settings-language prefs-setting)
                      module-language)
                 (drracket:language-configuration:language-settings-settings prefs-setting)]
                [else 
                 (and module-language
                      (send module-language default-settings))]))])
      (values module-language module-language-settings)))
  
  
  
  
  ;                                                       
  ;                                                       
  ;                                                       
  ;                                                       
  ;      ;;;          ;;;;;;;                             
  ;      ;;;         ;;;                                  
  ;   ;; ;;;   ;;;; ;;;;; ;;; ;;; ;;    ;;;;              
  ;  ;;;;;;;  ;; ;;;;;;;; ;;; ;;;;;;;  ;; ;;;             
  ;  ;;; ;;; ;;; ;;; ;;;  ;;; ;;; ;;; ;;; ;;;             
  ;  ;;; ;;; ;;;;;;; ;;;  ;;; ;;; ;;; ;;;;;;;             
  ;  ;;; ;;; ;;;     ;;;  ;;; ;;; ;;; ;;;     ;;; ;;; ;;; 
  ;  ;;;;;;;  ;;;;;; ;;;  ;;; ;;; ;;;  ;;;;;; ;;; ;;; ;;; 
  ;   ;; ;;;   ;;;;  ;;;  ;;; ;;; ;;;   ;;;;  ;;; ;;; ;;; 
  ;                                                       
  ;                                                       
  ;                                                       
  ;                                                       
  
  ;; get-pos : text mouse-event% -> (union #f number)
  (define (get-pos text event)
    (let*-values ([(event-x event-y)
                   (values (send event get-x)
                           (send event get-y))]
                  [(x y) (send text dc-location-to-editor-location
                               event-x 
                               event-y)])
      (let* ([on-it? (box #f)]
             [pos (send text find-position x y #f on-it?)])
        (and (unbox on-it?)
             pos))))
  
  (let ([old (keymap:add-to-right-button-menu)])
    (keymap:add-to-right-button-menu
     (λ (menu editor event)
       (when (is-a? editor text%)
         (let* ([canvas (send editor get-canvas)]
                [frame (and canvas (send canvas get-top-level-window))])
           (when (is-a? frame drracket:unit:frame<%>)
             (let* ([language-settings (send (send frame get-definitions-text) get-next-settings)]
                    [new-language (drracket:language-configuration:language-settings-language
                                   language-settings)]
                    [capability-info (send new-language capability-value 'drscheme:define-popup)])
               (when capability-info
                 (let* ([current-pos (get-pos editor event)]
                        [current-word (and current-pos (get-current-word editor current-pos))]
                        [defn (and current-word
                                   (ormap (λ (defn) (and (string=? current-word (defn-name defn))
                                                         defn))
                                          (get-definitions (car capability-info)
                                                           #f
                                                           editor)))])
                   (when defn
                     (new separator-menu-item% (parent menu))
                     (new menu-item%
                          (parent menu)
                          (label (gui-utils:format-literal-label (string-constant jump-to-defn)
                                                                 (defn-name defn)))
                          (callback (λ (x y)
                                      (send editor set-position (defn-start-pos defn))))))))))))
       (old menu editor event))))
  
  ;; get-current-word : editor number -> string
  ;; returns the string that is being clicked on
  (define (get-current-word editor pos)
    (let* ([search
            (λ (dir offset)
              (let loop ([pos pos])
                (cond
                  [(or (= pos 0) 
                       (= pos (send editor last-position)))
                   pos]
                  [(memq (send editor get-character pos)
                         '(#\space #\return #\newline #\( #\) #\[ #\] #\tab))
                   (offset pos)]
                  [else (loop (dir pos))])))]
           [before (search sub1 add1)]
           [after (search add1 (λ (x) x))])
      (send editor get-text before after)))
  
  (define func-defs-canvas%
    (class name-message%
      (init-field frame)
      
      (unless (is-a? frame drracket:unit:frame<%>)
        (error 'func-defs-canvas "frame is not a drracket:unit:frame<%>"))
      
      (define sort-by-name? (preferences:get 'drracket:defns-popup-sort-by-name?))
      (define sorting-name (if sort-by-name?
                               (string-constant sort-by-position) 
                               (string-constant sort-by-name)))
      (define/private (change-sorting-order)
        (set! sort-by-name? (not sort-by-name?))
        (preferences:set 'drracket:defns-popup-sort-by-name? sort-by-name?)
        (set! sorting-name (if sort-by-name?
                               (string-constant sort-by-position) 
                               (string-constant sort-by-name))))
      
      (define define-popup-capability-info
        (drracket:language:get-capability-default 'drscheme:define-popup))
      
      (inherit set-message set-hidden?)
      (define/public (language-changed new-language vertical?)
        (set! define-popup-capability-info
              (send new-language capability-value 'drscheme:define-popup))
        (let ([define-name (get-define-popup-name define-popup-capability-info
                                                  vertical?)])
          (cond
            [define-name
              (set-message #f define-name)
              (set-hidden? #f)]
            [else
             (set-hidden? #t)])))
      (define/override (fill-popup menu reset)
        (when define-popup-capability-info
          (let* ([text (send frame get-definitions-text)]
                 [unsorted-defns (get-definitions (car define-popup-capability-info)
                                                  (not sort-by-name?)
                                                  text)]
                 [defns (if sort-by-name?
                            (sort
                             unsorted-defns
                             (λ (x y) (string-ci<=? (defn-name x) (defn-name y))))
                            unsorted-defns)])
            (make-object menu:can-restore-menu-item% sorting-name
              menu
              (λ (x y)
                (change-sorting-order)))
            (make-object separator-menu-item% menu)
            (if (null? defns)
                (send (make-object menu:can-restore-menu-item%
                        (string-constant no-definitions-found)
                        menu
                        void)
                      enable #f)
                (let loop ([defns defns])
                  (unless (null? defns)
                    (let* ([defn (car defns)]
                           [checked? 
                            (let ([t-start (send text get-start-position)]
                                  [t-end (send text get-end-position)]
                                  [d-start (defn-start-pos defn)]
                                  [d-end (defn-end-pos defn)])
                              (or (<= t-start d-start t-end)
                                  (<= t-start d-end t-end)
                                  (<= d-start t-start t-end d-end)))]
                           [item
                            (make-object (if checked?
                                             menu:can-restore-checkable-menu-item%
                                             menu:can-restore-menu-item%)
                              (gui-utils:quote-literal-label (defn-name defn))
                              
                              menu
                              (λ (x y)
                                (reset)
                                (send text set-position (defn-start-pos defn) (defn-start-pos defn))
                                (let ([canvas (send text get-canvas)])
                                  (when canvas
                                    (send canvas focus)))))])
                      (when checked?
                        (send item check #t))
                      (loop (cdr defns)))))))))
      
      (super-new (label "(define ...)") ;; this default is quickly changed
                 [string-constant-untitled (string-constant untitled)]
                 [string-constant-no-full-name-since-not-saved 
                  (string-constant no-full-name-since-not-saved)])))
  
  (define (set-box/f! b v) (when (box? b) (set-box! b v)))
  
  ;                                        
  ;                                        
  ;                                        
  ;                                        
  ;   ;;;;                                 
  ;  ;;;                                   
  ;  ;;;; ;;; ;;;;;;;  ;;; ;; ;;;    ;;;;  
  ;  ;;;; ;;;;;;;;;;;; ;;;;;;;;;;;  ;; ;;; 
  ;  ;;;  ;;;  ;;  ;;; ;;; ;;; ;;; ;;; ;;; 
  ;  ;;;  ;;;    ;;;;; ;;; ;;; ;;; ;;;;;;; 
  ;  ;;;  ;;;  ;;; ;;; ;;; ;;; ;;; ;;;     
  ;  ;;;  ;;;  ;;; ;;; ;;; ;;; ;;;  ;;;;;; 
  ;  ;;;  ;;;   ;;;;;; ;;; ;;; ;;;   ;;;;  
  ;                                        
  ;                                        
  ;                                        
  ;                                        
  
  (define dragable/def-int-mixin
    (mixin (panel:dragable<%>) ()
      (init-field unit-frame)
      (inherit get-percentages popup-menu 
               set-orientation get-vertical?)
      (define/augment (after-percentage-change)
        (let ([percentages (get-percentages)])
          (when (and (= 1
                        (length (send unit-frame get-definitions-canvases))
                        (length (send unit-frame get-interactions-canvases)))
                     (= 2 (length percentages)))
            (preferences:set 'drracket:unit-window-size-percentage (car percentages))))
        (inner (void) after-percentage-change))
      (define/override (right-click-in-gap evt before after)
        (define menu (new popup-menu%))
        (define vertical? (get-vertical?))
        (new menu-item%
             [parent menu]
             [label 
              ;; something seems to be wrong with the get-vertical? method...
              (if vertical?
                  (string-constant change-to-vertical-alignment)
                  (string-constant change-to-horizontal-alignment))]
             [callback
              (λ (a b) 
                (preferences:set 'drracket:defs/ints-horizontal vertical?)
                (set-orientation vertical?))])
        (popup-menu menu (send evt get-x) (send evt get-y)))
      (super-new)))
  
  (define vertical-dragable/def-int% (dragable/def-int-mixin panel:vertical-dragable%))
  (define horizontal-dragable/def-int% (dragable/def-int-mixin panel:horizontal-dragable%))
  
  (define tab%
    (class* object% (drracket:rep:context<%> drracket:unit:tab<%>)
      (init-field frame
                  defs
                  i
                  defs-shown?
                  ints-shown?)
      (define enabled? #t)
      (field [ints #f]
             [visible-defs #f]
             [visible-ints #f]
             [focus-d/i 'defs])
      
      ;; only called to initialize this tab.
      ;; the interactions editor should be invariant.
      (define/public (set-ints i) (set! ints i)) 
      
      (define/public-final (get-frame) frame)
      (define/public-final (get-defs) defs)
      (define/public-final (get-ints) ints)
      (define/public-final (get-visible-defs) (values visible-defs defs-shown?))
      (define/public-final (set-visible-defs vd ds?)
        (set! visible-defs vd)
        (set! defs-shown? ds?))
      (define/public-final (get-visible-ints) (values visible-ints ints-shown?))
      (define/public-final (set-visible-ints vi is?)
        (set! visible-ints vi)
        (set! ints-shown? is?))
      (define/public-final (set-focus-d/i di)
        (set! focus-d/i di))
      (define/public-final (get-focus-d/i) focus-d/i)
      (define/public-final (get-i) i)
      (define/public-final (set-i _i) (set! i _i))
      (define/public (disable-evaluation)
        (set! enabled? #f)
        (send ints lock #t)
        (send frame disable-evaluation-in-tab this))
      (define/public (enable-evaluation)
        (set! enabled? #t)
        (send ints lock #f)
        (send frame enable-evaluation-in-tab this))
      (define/public (get-enabled) enabled?)
      
      (define last-touched (current-inexact-milliseconds))
      (define/public-final (touched) (set! last-touched (current-inexact-milliseconds)))
      (define/public-final (get-last-touched) last-touched)
      
      ;; current-execute-warning is a snapshot of the needs-execution-message
      ;; that is taken each time repl submission happens, and it gets reset
      ;; when "Run" is clicked.
      (define current-execute-warning #f)
      (define/pubment (repl-submit-happened)
        (set! current-execute-warning (send defs get-needs-execution-message))
        (update-execute-warning-gui))
      (define/public (get-current-execute-warning) current-execute-warning)
      (define/public (clear-execution-state) 
        (set! current-execute-warning #f)
        (update-execute-warning-gui)
        (send defs already-warned))
      (define/public (update-execute-warning-gui)
        (when (is-current-tab?)
          (send frame show/hide-warning-message 
                (get-current-execute-warning)
                (λ () 
                  ;; this callback might be run with a different tab ...
                  (send (send frame get-current-tab) clear-execution-state)))))
      
      (define/public (get-directory)
        (define bx (box #f))
        (define filename (send defs get-filename bx))
        (get-init-dir 
         (and (not (unbox bx)) filename)))
      
      (define/pubment (can-close?)
        (and (send defs can-close?)
             (send ints can-close?)
             (inner #t can-close?)))
      (define/pubment (on-close)
        (send defs on-close)
        (send ints on-close)
        (inner (void) on-close))
      
      ;; this should really do something local to the tab, but
      ;; for now it doesn't.
      (define/public (ensure-rep-shown rep) 
        (send frame ensure-rep-shown rep))
      
      (field [thread-to-break-box (make-weak-box #f)]
             [custodian-to-kill-box (make-weak-box #f)]
             [offer-kill? #f])
      
      ;; break-callback : -> void
      (define/public (break-callback)
        (let ([thread-to-break (weak-box-value thread-to-break-box)]
              [custodian-to-kill (weak-box-value custodian-to-kill-box)])
          (cond
            [(or (not thread-to-break)
                 (not custodian-to-kill))
             (bell)]
            [offer-kill? 
             (if (user-wants-kill?)
                 (when thread-to-break
                   (break-thread thread-to-break))
                 (when custodian-to-kill
                   (custodian-shutdown-all custodian-to-kill)))]
            [else
             (when thread-to-break
               (break-thread thread-to-break))
             ;; only offer a kill the next time if 
             ;; something got broken.
             (set! offer-kill? #t)])))
      
      ;; user-wants-kill? : -> boolean
      ;; handles events, so be sure to check state
      ;; after calling to avoid race conditions.
      (define/private (user-wants-kill?)
        (gui-utils:get-choice
         (string-constant kill-evaluation?)
         (string-constant just-break)
         (string-constant kill)
         (string-constant kill?)
         'diallow-close
         frame))
      
      ;; reset-offer-kill
      (define/public (reset-offer-kill)
        (set! offer-kill? #f))
      
      ;; get-breakables : -> (union #f thread) (union #f cust) -> void
      (define/public (get-breakables)
        (values (weak-box-value thread-to-break-box) (weak-box-value custodian-to-kill-box)))
      
      ;; set-breakables : (union #f thread) (union #f cust) -> void
      (define/public (set-breakables thd cust)
        (set! thread-to-break-box (make-weak-box thd))
        (set! custodian-to-kill-box (make-weak-box cust)))
      
      (define/pubment (clear-annotations)
        (inner (void) clear-annotations)
        (send ints reset-highlighting))
      
      (define running? #f)
      (define/public-final (is-running?) running?)
      (define/public (update-running b?) 
        (set! running? b?)
        (send frame update-running b?))
      
      (define/public-final (is-current-tab?) (eq? this (send frame get-current-tab)))
      
      (define log-visible? #f)
      (define/public-final (toggle-log)
        (set! log-visible? (not log-visible?))
        (send frame show/hide-log log-visible?)
        (send (get-ints) enable/disable-capture-log log-visible?))
      (define/public-final (hide-log)
        (when log-visible? (toggle-log)))
      (define/public-final (update-log)
        (send frame show/hide-log log-visible?)
        (send frame set-logger-text-field-value (send (get-ints) get-user-log-receiver-args-str)))
      (define/public-final (update-logger-window command)
        (when (is-current-tab?)
          (send frame update-logger-window command)))
      
      (define current-planet-status #f)
      (define/public-final (new-planet-status a b)
        (set! current-planet-status (cons a b))
        (update-planet-status))
      (define/public-final (clear-planet-status) 
        (set! current-planet-status #f)
        (update-planet-status))
      (define/public-final (update-planet-status)
        (send frame show-planet-status 
              (and current-planet-status 
                   (car current-planet-status))
              (and current-planet-status 
                   (cdr current-planet-status))))
      
      (super-new)))
  
  ;; should only be called by the tab% object (and the class itself)
  (define-local-member-name 
    disable-evaluation-in-tab
    enable-evaluation-in-tab
    update-toolbar-visibility
    show/hide-log
    set-logger-text-field-value
    show-planet-status)
  
  (define frame-mixin
    (mixin (drracket:frame:<%> frame:searchable-text<%> frame:delegate<%> frame:size-pref<%>)
      (drracket:unit:frame<%>)
      (init filename)
      (inherit set-label-prefix get-show-menu
               get-menu%
               get-area-container
               update-info
               get-file-menu
               search-hidden?
               unhide-search
               hide-search
               file-menu:get-close-item
               file-menu:get-save-item
               file-menu:get-save-as-item
               file-menu:get-revert-item
               file-menu:get-print-item
               set-delegated-text)
      
      (define resizable-panel (drr-named-undefined 'resizable-panel))
      (define definitions-canvas (drr-named-undefined 'definitions-canvas))
      (define definitions-canvases (drr-named-undefined 'definitions-canvases))
      (define interactions-canvas (drr-named-undefined 'interactions-canvas))
      (define interactions-canvases (drr-named-undefined 'interactions-canvases))
      (define button-panel (drr-named-undefined 'button-panel))
      
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;; execute warning
      ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define execute-warning-panel #f)
      (define execute-warning-parent-panel #f)
      (define execute-warning-canvas #f)
      (define/public-final (show/hide-warning-message msg hide-canvas)
        (when (and execute-warning-parent-panel
                   execute-warning-panel)
          (cond
            [msg
             (cond
               [execute-warning-canvas
                (send execute-warning-canvas set-message msg)]
               [else
                (set! execute-warning-canvas
                      (new execute-warning-canvas% 
                           [stretchable-height #t]
                           [parent execute-warning-panel]
                           [message msg]))
                (new close-icon% 
                     [parent execute-warning-panel]
                     [bg-color "yellow"]
                     [callback (λ () (hide-canvas))])])
             (send execute-warning-parent-panel
                   change-children
                   (λ (l) (append (remq execute-warning-panel l)
                                  (list execute-warning-panel))))]
            [else
             (when execute-warning-canvas
               (send execute-warning-parent-panel
                     change-children
                     (λ (l) (remq execute-warning-panel l)))
               (send execute-warning-canvas set-message #f))])))
      
      
      ;; bind the proc to a field
      ;; so it stays alive as long 
      ;; as the frame stays alive
      (define show-line-numbers-pref-fn
        (let ([fn (lambda (pref value)
                    (when show-line-numbers-menu-item
                      (send show-line-numbers-menu-item set-label
                            (if value
                                (string-constant hide-line-numbers/menu)
                                (string-constant show-line-numbers/menu))))
                    (show-line-numbers! value))])
          (preferences:add-callback
           'drracket:show-line-numbers?
           fn
           #t)
          fn))
      (define show-line-numbers-menu-item #f)
      
      (define/override (add-line-number-menu-items menu)
        (define on? (preferences:get 'drracket:show-line-numbers?))
        (new separator-menu-item% [parent menu])
        (new checkable-menu-item% 
             [label (string-constant show-line-numbers-in-definitions)]
             [parent menu]
             [checked on?]
             [callback
              (λ (c dc)
                (preferences:set 'drracket:show-line-numbers? (not on?)))])
        (super add-line-number-menu-items menu))
      
      (define/private (show-line-numbers! show)
        (for ([tab tabs])
          (define text (send tab get-defs))
          (send text show-line-numbers! show))
        (send definitions-canvas refresh))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;; logging 
      ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define logger-panel #f)
      (define logger-parent-panel #f)
      
      ;; logger-gui-content-panel: (or/c #f (is-a?/c vertical-panel%))
      ;; this is #f when the GUI has not been built yet. After
      ;; it becomes a panel, it is always a panel 
      ;; (altho the panel might not always be shown)
      (define logger-gui-content-panel #f)
      (define logger-gui-canvas #f)
      (define logger-checkbox #f)
      (define logger-text-field #f)
      
      ;; logger-gui-text: (or/c #f (is-a?/c text%))
      ;; this is #f when the GUI has not been built or when the logging panel is hidden
      ;; in that case, the logging messages aren't begin saved in an editor anywhere
      (define logger-gui-text #f)
      
      (define logger-menu-item #f)
      
      (define/public-final (show/hide-log show?)
        (let ([p (preferences:get 'drracket:logging-size-percentage)])
          (begin-container-sequence)
          (cond
            [logger-gui-content-panel
             (send logger-parent-panel change-children
                   (λ (l)
                     (cond
                       [(or (and show? (member logger-panel l))
                            (and (not show?)
                                 (not (member logger-panel l))))
                        ;; if things are already up to date, only update the logger text
                        (when show?
                          (update-logger-window #f))
                        l]
                       [show? 
                        (new-logger-text)
                        (send logger-gui-canvas set-editor logger-gui-text)
                        (update-logger-window #f)
                        (send logger-menu-item set-label (string-constant hide-log))
                        (append (remq logger-panel l) (list logger-panel))]
                       [else
                        (send logger-menu-item set-label (string-constant show-log))
                        (set! logger-gui-text #f)
                        (send logger-gui-canvas set-editor #f)
                        (remq logger-panel l)])))]
            [else
             (when show? ;; if we want to hide and it isn't built yet, do nothing
               (define logger-gui-content-panel-parent (new vertical-panel%
                                                            [style '(border)]
                                                            [parent logger-panel]
                                                            [stretchable-height #t]))
               (set! logger-gui-content-panel
                     (new horizontal-panel%
                          [parent logger-gui-content-panel-parent]
                          [stretchable-height #f]))
               (new-logger-text)
               (set! logger-gui-canvas 
                     (new editor-canvas%
                          [parent logger-gui-content-panel-parent]
                          [style '(transparent no-border)]
                          [editor logger-gui-text]))
               (new message% [label (string-constant log-messages)] [parent logger-gui-content-panel])
               (new button% 
                    [label (string-constant help)]
                    [callback (λ (x y) 
                                (define-values (path tag) 
                                  (xref-tag->path+anchor 
                                   (load-collections-xref)
                                   (make-section-tag
                                    "follow-log"
                                    #:doc '(lib "scribblings/drracket/drracket.scrbl"))))
                                (define url (path->url path))
                                (define url2 (if tag
                                                 (make-url (url-scheme url)
                                                           (url-user url)
                                                           (url-host url)
                                                           (url-port url)
                                                           (url-path-absolute? url)
                                                           (url-path url)
                                                           (url-query url)
                                                           tag)
                                                 url))
                                (send-url (url->string url2)))]
                    [parent logger-gui-content-panel])
               (set! logger-text-field
                     (keymap:call/text-keymap-initializer
                      (λ ()
                        (new text-field% 
                             [parent logger-gui-content-panel] 
                             [label "‹level›@‹name› ..."]
                             [init-value 
                              (send (get-interactions-text) get-user-log-receiver-args-str)]
                             [callback
                              (λ (tf evt)
                                (define str (send (send tf get-editor) get-text))
                                (define args (parse-logger-args str))
                                (preferences:set 'drracket:logger-receiver-string str)
                                (send (get-interactions-text) set-user-log-receiver-args
                                      str
                                      (if (null? args) #f args))
                                (set-logger-text-field-bg-color args))]))))
               (set-logger-text-field-bg-color (parse-logger-args (send logger-text-field get-value)))
               (set! logger-checkbox 
                     (new check-box%
                          [label (string-constant logger-scroll-on-output)]
                          [callback (λ (a b) (preferences:set 'drracket:logger-scroll-to-bottom?
                                                              (send logger-checkbox get-value)))]
                          [parent logger-gui-content-panel]
                          [value (preferences:get 'drracket:logger-scroll-to-bottom?)]))
               (new button% 
                    [label (string-constant hide-log)]
                    [callback (λ (x y) (send current-tab hide-log))]
                    [parent logger-gui-content-panel])
               (send logger-menu-item set-label (string-constant hide-log))
               (update-logger-window #f)
               (send logger-parent-panel change-children (λ (l) (append l (list logger-panel)))))])
          (with-handlers ([exn:fail? void])
            (send logger-parent-panel set-percentages (list p (- 1 p))))
          (update-logger-button-label)
          (end-container-sequence)))
      
      (define/public (set-logger-text-field-value str)
        (when logger-text-field
          (send logger-text-field set-value str)
          (set-logger-text-field-bg-color (parse-logger-args str))))
      
      (define/private (set-logger-text-field-bg-color good?)
        (send logger-text-field set-field-background 
              (send the-color-database find-color (if good? "white" "pink"))))
      
      (define/private (log-shown?)
        (and logger-gui-content-panel
             (member logger-panel (send logger-parent-panel get-children))))
      
      (define/private (new-logger-text)
        (set! logger-gui-text (new (text:hide-caret/selection-mixin 
                                    (editor:standard-style-list-mixin
                                     text:line-spacing%))))
        (send logger-gui-text lock #t))
      
      (define/public (update-logger-window command)
        (when logger-gui-text 
          (define admin (send logger-gui-text get-admin))
          (define canvas (send logger-gui-text get-canvas))
          (when (and canvas admin)
            (define logger-messages (send interactions-text get-logger-messages))
            (cond
              [(and (pair? command)
                    (pair? logger-messages)
                    ;; just flush and redraw everything if there is one (or zero) logger messages
                    (pair? (cdr logger-messages)))
               (define msg (cdr command))
               (define scroll? (if (object? logger-checkbox)
                                   (send logger-checkbox get-value)
                                   #t))
               (send logger-gui-text begin-edit-sequence)
               (send logger-gui-text lock #f)
               (case (car command)
                 [(add-line) (void)]
                 [(clear-last-and-add-line)
                  (send logger-gui-text delete
                        0
                        (send logger-gui-text paragraph-start-position 1)
                        #f)])
               (send logger-gui-text insert
                     "\n"
                     (send logger-gui-text last-position)
                     (send logger-gui-text last-position)
                     #f)
               (send logger-gui-text insert 
                     msg 
                     (send logger-gui-text last-position)
                     (send logger-gui-text last-position)
                     #f)
               (when scroll?
                 (send logger-gui-text scroll-to-position
                       (send logger-gui-text
                             paragraph-start-position
                             (send logger-gui-text last-paragraph))))
               (send logger-gui-text end-edit-sequence)
               (send logger-gui-text lock #t)]
              [else
               (send logger-gui-text begin-edit-sequence)
               (send logger-gui-text lock #f)
               (send logger-gui-text erase)
               
               (define (insert-one msg)
                 (send logger-gui-text insert msg 0 0)) 
               
               (unless (null? logger-messages)
                 ;; skip the last newline in the buffer
                 (insert-one (car logger-messages))
                 (for ([msg (in-list (cdr (send interactions-text get-logger-messages)))])
                   (insert-one "\n")
                   (insert-one msg)))
               
               (send logger-gui-text lock #t)
               (send logger-gui-text end-edit-sequence)]))))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;; planet status 
      ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define planet-status-parent-panel #f)
      (define planet-status-panel #f)
      (define planet-message #f)
      (define planet-logger-button #f)
      ;; local-member-name
      (define/public (show-planet-status tag package)
        (cond
          [(and (not tag)
                (not package)
                (or (not planet-status-parent-panel)
                    (not (member planet-status-panel 
                                 (send planet-status-parent-panel get-children)))))
           ;; if there is no information and there is no GUI there, don't do anything
           (void)]
          [else
           (when planet-status-panel
             (unless planet-message
               (new message% 
                    [parent planet-status-panel]
                    [label drracket:debug:small-planet-bitmap])
               (set! planet-message (new message% 
                                         [parent planet-status-panel] 
                                         [label ""]
                                         [stretchable-width #t]))
               (set! planet-logger-button
                     (new button%
                          [font small-control-font]
                          [parent planet-status-panel]
                          [label (string-constant show-log)]
                          [callback (λ (a b) (send current-tab toggle-log))]))
               (update-logger-button-label)
               (new close-icon%
                    [parent planet-status-panel]
                    [callback (λ () 
                                (send planet-status-parent-panel change-children
                                      (λ (l)
                                        (remq planet-status-panel l)))
                                (send current-tab clear-planet-status))]))
             (send planet-message set-label 
                   (case tag
                     [(download)
                      (format (string-constant planet-downloading) package)]
                     [(install)
                      (format (string-constant planet-installing) package)]
                     [(docs-build)
                      (format (string-constant planet-docs-building) package)]
                     [(finish)
                      (format (string-constant planet-finished) package)]
                     [else
                      (string-constant planet-no-status)]))
             (send planet-status-parent-panel change-children
                   (λ (l)
                     (if (memq planet-status-panel l)
                         l
                         (append (remq planet-status-panel l) (list planet-status-panel))))))]))
      
      (define/private (update-logger-button-label)
        (when planet-logger-button
          (send planet-logger-button set-label
                (if (and logger-gui-text
                         (member logger-panel (send logger-parent-panel get-children)))
                    (string-constant hide-log)
                    (string-constant show-log)))))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;; transcript
      ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      
      ;; transcript : (union #f string[directory-name])
      (field [transcript #f]
             [definitions-transcript-counter 0]  ;; number
             [interactions-transcript-counter 0] ;; number
             [transcript-parent-panel #f]    ;; panel (unitialized short time only)
             [transcript-panel #f]           ;; panel (unitialized short time only)
             [transcript-menu-item #f])      ;; menu-item (unitialized short time only)
      ;; record-definitions : -> void
      (define/private (record-definitions)
        (when transcript
          (set! definitions-transcript-counter (+ definitions-transcript-counter 1))
          (send definitions-text save-file 
                (build-path transcript (format "~a-definitions" 
                                               (pad-two definitions-transcript-counter)))
                'copy)))
      
      ;; record-ineractions : -> void
      (define/private (record-interactions)
        (when transcript
          (set! interactions-transcript-counter (+ interactions-transcript-counter 1))
          (send interactions-text save-file 
                (build-path transcript (format "~a-interactions" 
                                               (pad-two interactions-transcript-counter)))
                'copy)))
      
      ;; pad-two : number -> string
      ;; pads a number to two digits?
      (define/private (pad-two n)
        (cond
          [(<= 0 n 9) (format "0~a" n)]
          [else (format "~a" n)]))
      
      ;; start-transcript : -> void
      ;; turns on the transcript and shows the transcript gui
      (define/private (start-transcript)
        (let ([transcript-directory (mred:get-directory
                                     (string-constant please-choose-a-log-directory)
                                     this)])
          (when (and transcript-directory
                     (ensure-empty transcript-directory))
            (send transcript-menu-item set-label (string-constant stop-logging))
            (set! transcript transcript-directory)
            (set! definitions-transcript-counter 0)
            (set! interactions-transcript-counter 0)
            (build-transcript-panel)
            (record-definitions))))
      
      ;; stop-transcript : -> void
      ;; turns off the transcript procedure
      (define/private (stop-transcript)
        (record-interactions)
        (send transcript-menu-item set-label (string-constant log-definitions-and-interactions))
        (set! transcript #f)
        (send transcript-panel change-children (λ (l) null)))
      
      ;; build-transcript-panel : -> void
      ;; builds the contents of the transcript panel
      (define/private (build-transcript-panel)
        (define hp (make-object horizontal-panel% transcript-panel '(border)))
        (make-object message% (string-constant logging-to) hp)
        (send (make-object message% (path->string transcript) hp) stretchable-width #t)
        (make-object button% (string-constant stop-logging) hp (λ (x y) (stop-transcript))))
      
      ;; ensure-empty : string[directory] -> boolean
      ;; if the transcript-directory is empty, just return #t
      ;; if not, ask the user about emptying it. 
      ;;   if they say yes, try to empty it.
      ;;     if that fails, report the error and return #f.
      ;;     if it succeeds, return #t.
      ;;   if they say no, return #f.
      (define/private (ensure-empty transcript-directory)
        (let ([dir-list (directory-list transcript-directory)])
          (or (null? dir-list)
              (let ([query (message-box 
                            (string-constant drscheme)
                            (gui-utils:format-literal-label
                             (string-constant erase-log-directory-contents)
                             transcript-directory)
                            this
                            '(yes-no)
                            #:dialog-mixin frame:focus-table-mixin)])
                (cond
                  [(equal? query 'no) 
                   #f]
                  [(equal? query 'yes)
                   (with-handlers ([exn:fail:filesystem?
                                    (λ (exn)
                                      (message-box 
                                       (string-constant drscheme)
                                       (gui-utils:format-literal-label 
                                        (string-constant error-erasing-log-directory)
                                        (if (exn? exn)
                                            (format "~a" (exn-message exn))
                                            (format "~s" exn)))
                                       this
                                       #:dialog-mixin frame:focus-table-mixin)
                                      #f)])
                     (for-each (λ (file) (delete-file (build-path transcript-directory file)))
                               dir-list)
                     #t)])))))
      
      (define/override (make-root-area-container cls parent)
        (let* ([_module-browser-parent-panel
                (super make-root-area-container 
                       (make-two-way-prefs-dragable-panel% panel:horizontal-dragable%
                                                           'drracket:module-browser-size-percentage)
                       parent)]
               [_module-browser-panel (new vertical-panel%
                                           (parent _module-browser-parent-panel)
                                           (alignment '(left center))
                                           (stretchable-width #f))]
               [planet-status-outer-panel (new vertical-panel% [parent _module-browser-parent-panel])]
               [execute-warning-outer-panel (new vertical-panel% [parent planet-status-outer-panel])]
               [logger-outer-panel (new (make-two-way-prefs-dragable-panel%
                                         panel:vertical-dragable%
                                         'drracket:logging-size-percentage)
                                        [parent execute-warning-outer-panel])]
               [trans-outer-panel (new vertical-panel% [parent logger-outer-panel])]
               [root (make-object cls trans-outer-panel)])
          (set! module-browser-parent-panel _module-browser-parent-panel)
          (set! module-browser-panel _module-browser-panel)
          (send module-browser-parent-panel change-children (λ (l) (remq module-browser-panel l)))
          (set! logger-parent-panel logger-outer-panel)
          (set! logger-panel (new vertical-panel% [parent logger-parent-panel]))
          (send logger-parent-panel change-children (lambda (x) (remq logger-panel x)))
          
          (set! execute-warning-parent-panel execute-warning-outer-panel)
          (set! execute-warning-panel (new horizontal-panel% 
                                           [parent execute-warning-parent-panel]
                                           [stretchable-height #f]))
          (send execute-warning-parent-panel change-children (λ (l) (remq execute-warning-panel l)))
          
          (set! transcript-parent-panel (new horizontal-panel%
                                             (parent trans-outer-panel)
                                             (stretchable-height #f)))
          (set! transcript-panel (make-object horizontal-panel% transcript-parent-panel))
          (set! planet-status-parent-panel (new vertical-panel% 
                                                [parent planet-status-outer-panel]
                                                [stretchable-height #f]))
          (set! planet-status-panel (new horizontal-panel% 
                                         [parent planet-status-parent-panel]))
          (send planet-status-parent-panel change-children (λ (l) (remq planet-status-panel l)))
          (unless (toolbar-shown?)
            (send transcript-parent-panel change-children (λ (l) '())))
          (send logger-outer-panel enable-two-way-prefs)
          (send _module-browser-parent-panel enable-two-way-prefs)
          
          root))
      
      (inherit show-info hide-info is-info-hidden?)
      (field [toolbar-state (preferences:get 'drracket:toolbar-state)]
             [toolbar-top-menu-item #f]
             [toolbar-top-no-label-menu-item #f]
             [toolbar-left-menu-item #f]
             [toolbar-right-menu-item #f]
             [toolbar-hidden-menu-item #f]
             [toolbar-menu #f])
      
      ;; returns #t if the toolbar is visible, #f otherwise
      (define/private (toolbar-shown?) (car toolbar-state))
      
      (define/private (change-toolbar-state new-state)
        (set! toolbar-state new-state)
        (preferences:set 'drracket:toolbar-state new-state)
        (update-toolbar-visibility))
      
      (define/override (on-toolbar-button-click) 
        (change-toolbar-state (cons (not (car toolbar-state)) (cdr toolbar-state))))
      (define/private (set-toolbar-left) (change-toolbar-state (cons #f 'left)))
      (define/private (set-toolbar-right) (change-toolbar-state (cons #f 'right)))
      (define/private (set-toolbar-top) (change-toolbar-state (cons #f 'top)))
      (define/private (set-toolbar-top-no-label) (change-toolbar-state (cons #f 'top-no-label)))
      (define/private (set-toolbar-hidden) (change-toolbar-state (cons #t (cdr toolbar-state))))
      
      (define/public (update-toolbar-visibility)
        (let* ([hidden? (toolbar-is-hidden?)]
               [left? (toolbar-is-left?)]
               [right? (toolbar-is-right?)]
               [top? (toolbar-is-top?)]
               [top-no-label? (toolbar-is-top-no-label?)])
          
          (send toolbar-left-menu-item check left?)
          (send toolbar-right-menu-item check right?)
          (send toolbar-top-menu-item check top?)
          (send toolbar-top-no-label-menu-item check top-no-label?)
          (send toolbar-hidden-menu-item check hidden?)
          
          (cond
            [hidden?
             (hide-info)
             (send top-outer-panel change-children (λ (l) '()))
             (send transcript-parent-panel change-children (λ (l) '()))]
            [top? (orient/show #t)]
            [top-no-label? (orient/show #t)]
            [left? (orient/show #t)]
            [right? (orient/show #f)]))
        (update-defs/ints-resize-corner))
      
      (define/private (toolbar-is-hidden?)
        (car (preferences:get 'drracket:toolbar-state)))
      (define/private (toolbar-is-top?)
        (and (not (toolbar-is-hidden?))
             (equal? (cdr (preferences:get 'drracket:toolbar-state))
                     'top)))
      (define/private (toolbar-is-right?)
        (and (not (toolbar-is-hidden?))
             (equal? (cdr (preferences:get 'drracket:toolbar-state))
                     'right)))
      (define/private (toolbar-is-left?)
        (and (not (toolbar-is-hidden?))
             (equal? (cdr (preferences:get 'drracket:toolbar-state))
                     'left)))
      (define/private (toolbar-is-top-no-label?)
        (and (not (toolbar-is-hidden?))
             (equal? (cdr (preferences:get 'drracket:toolbar-state))
                     'top-no-label)))
      
      (define/private (orient/show bar-at-beginning?)
        (let ([vertical? (or (toolbar-is-left?) (toolbar-is-right?))])
          (begin-container-sequence)
          (show-info)
          
          ;; orient the button panel and all panels inside it.
          (let loop ([obj button-panel])
            (when (is-a? obj area-container<%>)
              (when (or (is-a? obj vertical-panel%)
                        (is-a? obj horizontal-panel%)
                        (is-a? obj panel:discrete-sizes<%>))
                (unless (equal? (send obj get-orientation) (not vertical?))
                  (send obj set-orientation (not vertical?))))
              (for-each loop (send obj get-children))))
          (sort-toolbar-buttons-panel)
          
          (set-toolbar-label-visibilities/check-registered)
          
          (send top-outer-panel stretchable-height vertical?)
          (send top-outer-panel stretchable-width (not vertical?))
          (send top-panel set-orientation (not vertical?))
          (send toolbar/rest-panel set-orientation vertical?)
          (send toolbar/rest-panel change-children
                (λ (l)
                  (if bar-at-beginning?
                      (cons top-outer-panel (remq top-outer-panel l))
                      (append (remq top-outer-panel l) (list top-outer-panel)))))
          (send top-outer-panel change-children (λ (l) (list top-panel)))
          (send transcript-parent-panel change-children (λ (l) (list transcript-panel)))
          
          (let* ([settings (send definitions-text get-next-settings)]
                 [language (drracket:language-configuration:language-settings-language settings)]
                 [name (get-define-popup-name (send language capability-value 'drscheme:define-popup)
                                              vertical?)])
            (when name
              (send func-defs-canvas set-message #f name)))
          (send name-message set-short-title vertical?)
          (send name-panel set-orientation (not vertical?))
          (if vertical?
              (send name-panel set-alignment 'right 'top)
              (send name-panel set-alignment 'left 'center))
          (end-container-sequence)))
      
      ;; this table uses object identity on buttons(!)
      (define toolbar-buttons (make-hasheq))
      (define smallest #f)
      
      (define/public (register-toolbar-button b #:number [number/f #f])
        (add-to-toolbar-buttons 'register-toolbar-button b number/f)
        (set-toolbar-label-visibilities/check-registered)
        (sort-toolbar-buttons-panel))
      
      (define/public (register-toolbar-buttons bs #:numbers [numbers/fs (make-list (length bs) #f)])
        (for ([b (in-list bs)]
              [n (in-list numbers/fs)]) 
          (add-to-toolbar-buttons 'register-toolbar-buttons b n))
        (set-toolbar-label-visibilities/check-registered)
        
        ;; sort panel contents
        (define panels '())
        (for ([tb (in-list bs)])
          (define parent (send tb get-parent))
          (unless (memq parent panels)
            (set! panels (cons parent panels))))
        (for ([panel (in-list panels)])
          (sort-toolbar-buttons-panel)))
      
      (define/private (add-to-toolbar-buttons who button number/f)
        (define number (or number/f (if smallest (- smallest 1) 100)))
        (define prev (hash-ref toolbar-buttons button #f))
        (when (and prev (not (= prev number)))
          (error who "cannot add toolbar button ~s with number ~a; already added with ~a"
                 (send button get-label)
                 number
                 prev))
        (when (or (not smallest) (< number smallest))
          (set! smallest number))
        (hash-set! toolbar-buttons button number))
      
      (define/private (in-toolbar-list? b) (hash-ref toolbar-buttons b #f))
      
      (define/public (unregister-toolbar-button b)
        (hash-remove! toolbar-buttons b)
        (set! smallest
              (if (zero? (hash-count toolbar-buttons))
                  #f
                  (apply min (hash-map toolbar-buttons (λ (x y) y)))))
        (void))
      
      (define/public (sort-toolbar-buttons-panel)
        (define bp (get-button-panel))
        (when (is-a? bp panel%)
          (let sort-loop ([panel bp])
            (define min #f)
            (send panel change-children
                  (λ (l)
                    (define sub-panel-nums (make-hash))
                    (for ([x (in-list l)])
                      (when (is-a? x area-container<%>)
                        (hash-set! sub-panel-nums x (sort-loop x))))
                    (define (key i)
                      (or (let loop ([item i])
                            (cond
                              [(is-a? item area-container<%>)
                               (hash-ref sub-panel-nums item)]
                              [else
                               (hash-ref toolbar-buttons item #f)]))
                          -5000))
                    (define (min/f a b)
                      (cond
                        [(and a b) (min a b)]
                        [else (or a b)]))
                    (define cmp
                      (cond
                        [(is-a? panel vertical-pane%) >=]
                        [(is-a? panel horizontal-pane%) <=]
                        [else
                         (if (send panel get-orientation) ;; horizontal is #t
                             <=
                             >=)]))
                    (define ans (sort l cmp #:key key))
                    (set! min (if (null? ans)
                                  #f
                                  (key (car ans))))
                    ans))
            min)
          (void)))

      (define/private (set-toolbar-label-visibilities/check-registered)
        (define label-visible? (toolbar-is-top?))
        (for ([(button number) (in-hash toolbar-buttons)])
          (send button set-label-visible label-visible?))
        
        (let loop ([obj button-panel])
          (cond
            [(is-a? obj area-container<%>)
             (for-each loop (send obj get-children))]
            [(is-a? obj switchable-button%)
             (unless (in-toolbar-list? obj)
               (error 'register-toolbar-button 
                      "found a switchable-button% that is not registered, label ~s"
                      (send obj get-label)))]
            [else (void)])))
      
      (field [remove-show-status-line-callback
              (preferences:add-callback
               'framework:show-status-line
               (λ (p v)
                 (update-defs/ints-resize-corner/pref v)))])
      
      (define/private (update-defs/ints-resize-corner)
        (update-defs/ints-resize-corner/pref
         (preferences:get 'framework:show-status-line)))
      
      (define/private (update-defs/ints-resize-corner/pref si-pref)
        (let ([bottom-material? (and (not (car toolbar-state))
                                     si-pref)])
          (let loop ([cs definitions-canvases])
            (cond
              [(null? cs) (void)]
              [(null? (cdr cs))
               (send (car cs) set-resize-corner (and (not bottom-material?)
                                                     (not interactions-shown?)))]
              [else
               (send (car cs) set-resize-corner #f)
               (loop (cdr cs))]))
          (let loop ([cs interactions-canvases])
            (cond
              [(null? cs) (void)]
              [(null? (cdr cs))
               (send (car cs) set-resize-corner (and (not bottom-material?) 
                                                     interactions-shown?))]
              [else
               (send (car cs) set-resize-corner #f)
               (loop (cdr cs))]))))
      
      [define definitions-item #f]
      [define interactions-item #f]
      [define name-message #f]
      [define save-button #f]
      [define save-init-shown? #f]
      
      [define/private set-save-init-shown? (λ (x) (set! save-init-shown? x))]
      
      [define canvas-show-mode #f]
      [define allow-split? #f]
      [define forced-quit? #f]
      [define search-canvas #f]
      
      (define/public (make-searchable canvas)
        (update-info)
        (set! search-canvas canvas))
      
      (define was-locked? #f)
      
      (define/public-final (disable-evaluation-in-tab tab)
        (when (eq? tab current-tab)
          (disable-evaluation)))
      
      (define/pubment (disable-evaluation)
        (when execute-menu-item
          (send execute-menu-item enable #f))
        (send execute-button enable #f)
        (inner (void) disable-evaluation))
      
      (define/public-final (enable-evaluation-in-tab tab)
        (when (eq? tab current-tab)
          (enable-evaluation)))
      
      (define/pubment (enable-evaluation)
        (when execute-menu-item
          (send execute-menu-item enable #t))
        (send execute-button enable #t)
        (inner (void) enable-evaluation))
      
      (inherit set-label)
      (inherit modified)
      (define/public (update-save-button)
        (let ([mod? (send definitions-text is-modified?)])
          (modified mod?)
          (if save-button
              (unless (equal? mod? (send save-button is-shown?))
                (send save-button show mod?))
              (set! save-init-shown? mod?))
          (update-tab-label current-tab)))
      
      (define/public (language-changed)
        (define settings (send definitions-text get-next-settings))
        (define language (drracket:language-configuration:language-settings-language settings))
        (send func-defs-canvas language-changed language (or (toolbar-is-left?)
                                                             (toolbar-is-right?)))
        (send language-message set-yellow/lang
              (not (send definitions-text this-and-next-language-the-same?))
              (string-append (send language get-language-name)
                             (if (send language default-settings? 
                                       (drracket:language-configuration:language-settings-settings
                                        settings))
                                 ""
                                 (string-append " " (string-constant custom)))))
        (update-teachpack-menu)
        (when (is-a? language-specific-menu menu%)
          (define label (send language-specific-menu get-label))
          (define new-label (send language capability-value 'drscheme:language-menu-title))
          (unless (equal? label new-label)
            (send language-specific-menu set-label new-label))))
      
      (define/public (get-language-menu) language-specific-menu)
      
      ;; update-save-message : -> void
      ;; sets the save message. If input is #f, uses the frame's
      ;; title.
      (define/public (update-save-message)
        (when name-message
          (let ([filename (send definitions-text get-filename)])
            (send name-message set-message 
                  (if filename #t #f)
                  (send definitions-text get-filename/untitled-name))))
        (update-tabs-labels))
      
      (define/private (update-tabs-labels)
        (for-each (λ (tab) (update-tab-label tab)) tabs)
        (send tabs-panel set-selection (send current-tab get-i))
        (send (send tabs-panel get-parent)
              change-children
              (λ (l)
                (cond
                  [(= (send tabs-panel get-number) 1)
                   (remq tabs-panel l)]
                  [else
                   (if (memq tabs-panel l)
                       l
                       (cons tabs-panel l))]))))
      
      (define/private (update-tab-label tab)
        (let ([label (gui-utils:trim-string (get-defs-tab-label (send tab get-defs) tab) 200)])
          (unless (equal? label (send tabs-panel get-item-label (send tab get-i)))
            (send tabs-panel set-item-label (send tab get-i) label))))
      
      (define/public (get-tab-filename i)
        (get-defs-tab-filename (send (list-ref tabs i) get-defs)))
      
      (define/private (get-defs-tab-label defs tab)
        (define tab-index
          (for/or ([i (in-list tabs)]
                   [n (in-naturals 1)])
            (and (eq? i tab) n)))
        (define i-prefix
          (cond
            [(not tab-index) ""]
            [(<= tab-index 8) (format "~a: " tab-index)]
            [(= tab-index (get-tab-count)) "9: "]
            [else ""]))
        (add-modified-flag
         defs
         (string-append
          i-prefix
          (get-defs-tab-filename defs))))
      
      (define/private (get-defs-tab-filename defs)
        (let ([fn (send defs get-filename)])
          (if fn
              (get-tab-label-from-filename fn)
              (send defs get-filename/untitled-name))))
      
      ;; tab-label-cache-valid : (listof path)
      ;; If the current set of filenames in the tabs is the
      ;;   same set of filenames as in this list, then the
      ;;   tab-label-cache is valid; otherwise not
      (define tab-label-cache-valid '())
      
      ;; tab-label-cache : path -o> string
      (define tab-label-cache (make-hasheq))
      
      (define/private (get-tab-label-from-filename fn)
        (define current-paths (map (lambda (tab) (send (send tab get-defs) get-filename))
                                   tabs))
        (unless (and (= (length tab-label-cache-valid) (length current-paths))
                     (andmap eq? tab-label-cache-valid current-paths))
          (set! tab-label-cache-valid current-paths)
          (set! tab-label-cache (make-hasheq)))
        (define nfn (normalize-path/exists fn))
        (hash-ref! tab-label-cache 
                   fn
                   (lambda () 
                     (path->string
                      (or (shrink-path-wrt
                           nfn
                           (filter values
                                   (for/list ([other-tab (in-list tabs)])
                                     (define fn (send (send other-tab get-defs) get-filename))
                                     (and fn (normalize-path/exists fn)))))
                          (let-values ([(base name dir?) (split-path nfn)])
                            name))))))

      (define/private (normalize-path/exists fn)
        (if (file-exists? fn)
            (normalize-path fn)
            fn))

      (define/private (add-modified-flag text string)
        (if (send text is-modified?)
            (let ([prefix (get-save-diamond-prefix)])
              (if prefix
                  (string-append prefix string)
                  string))
            string))
      
      (define/private (get-save-diamond-prefix)
        (let ([candidate-prefixes 
               ;; be sure asterisk is at the end of each list,
               ;; since that's a relatively safe character
               (case (system-type)
                 [(unix windows) '("★ " "◆ " "• " "* ")]
                 [else '("◆ " "★ " "• " "* ")])])
          (ormap
           (lambda (candidate)
             (and (andmap (λ (x) (send normal-control-font screen-glyph-exists? x #t))
                          (string->list candidate))
                  candidate))
           candidate-prefixes)))
      
      [define/override get-canvas% (λ () (drracket:get/extend:get-definitions-canvas))]
      
      (define/public (update-running running?)
        (send running-canvas set-running running?))
      (define/public (ensure-defs-shown)
        (unless definitions-shown?
          (toggle-show/hide-definitions)
          (update-shown)))
      (define/public (ensure-rep-shown rep)
        (unless (eq? rep interactions-text)
          (let loop ([tabs tabs])
            (unless (null? tabs)
              (let ([tab (car tabs)])
                (if (eq? (send tab get-ints) rep)
                    (change-to-tab tab)
                    (loop (cdr tabs)))))))
        (unless interactions-shown?
          (toggle-show/hide-interactions)
          (update-shown)))
      (define/public (ensure-rep-hidden)
        (when interactions-shown?
          (toggle-show/hide-interactions)
          (update-shown)))
      
      (define/override (get-editor%) (drracket:get/extend:get-definitions-text))
      (define/public (still-untouched?)
        (and (send definitions-text still-untouched?)
             (let* ([prompt (send interactions-text get-prompt)]
                    [first-prompt-para
                     (let loop ([n 0])
                       (cond
                         [(n . <= . (send interactions-text last-paragraph))
                          (if (string=?
                               (send interactions-text get-text 
                                     (send interactions-text paragraph-start-position n)
                                     (+ (send interactions-text paragraph-start-position n)
                                        (string-length prompt)))
                               prompt)
                              n
                              (loop (+ n 1)))]
                         [else #f]))])
               (and first-prompt-para
                    (= first-prompt-para (send interactions-text last-paragraph))
                    (equal? 
                     (send interactions-text get-text
                           (send interactions-text paragraph-start-position first-prompt-para)
                           (send interactions-text paragraph-end-position first-prompt-para))
                     (send interactions-text get-prompt))))))
      (define/public (change-to-file name)
        (cond
          [(and name (file-exists? name))
           (ensure-rep-hidden)
           (send definitions-text begin-edit-sequence #t #f)
           (send definitions-text load-file/gui-error name)
           (send definitions-text end-edit-sequence)
           (send language-message set-yellow #f)]
          [name
           (send definitions-text set-filename name)]
          [else (send definitions-text clear)])
        (send definitions-canvas focus))
      
      
      
      
      
      
      ;                                            
      ;                                            
      ;                                            
      ;                           ;                
      ;                           ;                
      ;                           ;                
      ;   ; ;;  ;;     ;;;     ;; ;    ;;;    ;;;  
      ;   ;;  ;;  ;   ;   ;   ;  ;;   ;   ;  ;     
      ;   ;   ;   ;  ;     ; ;    ;  ;    ;  ;;    
      ;   ;   ;   ;  ;     ; ;    ;  ;;;;;;   ;;   
      ;   ;   ;   ;  ;     ; ;    ;  ;          ;  
      ;   ;   ;   ;   ;   ;   ;  ;;   ;         ;  
      ;   ;   ;   ;    ;;;     ;; ;    ;;;;  ;;;   
      ;                                            
      ;                                            
      ;                                            
      
      
      (define/private (add-modes-submenu edit-menu)
        (new menu%
             [parent edit-menu]
             [label (string-constant mode-submenu-label)]
             [demand-callback
              (λ (menu)
                (for ([item (in-list (send menu get-items))])
                  (send item delete))
                (for ([mode (in-list (drracket:modes:get-modes))])
                  (define item
                    (new checkable-menu-item%
                         (label (drracket:modes:mode-name mode))
                         (parent menu)
                         (callback 
                          (λ (_1 _2) (send definitions-text set-current-mode
                                           mode)))))
                  (when (send definitions-text is-current-mode? mode)
                    (send item check #t))))]))
      
      
      
      
      ;                                                                                         
      ;                                                                                         
      ;                                                                                         
      ;                  ;   ;           ;                  ;   ;                               
      ;                  ;               ;                  ;   ;                               
      ;                  ;       ;      ;                   ;   ;                               
      ;    ;;;   ; ;;    ;   ;  ;;;;    ;     ;;;    ;;;    ;   ;   ;;;    ; ;;     ;;;    ;;;  
      ;   ;      ;;  ;   ;   ;   ;      ;    ;   ;  ;   ;   ;   ;  ;   ;   ;;  ;   ;      ;   ; 
      ;   ;;     ;    ;  ;   ;   ;      ;   ;      ;     ;  ;   ;      ;   ;    ;  ;;    ;    ; 
      ;    ;;    ;    ;  ;   ;   ;     ;    ;      ;     ;  ;   ;   ;;;;   ;    ;   ;;   ;;;;;; 
      ;      ;   ;    ;  ;   ;   ;     ;    ;      ;     ;  ;   ;  ;   ;   ;    ;     ;  ;      
      ;      ;   ;;  ;   ;   ;   ;     ;     ;   ;  ;   ;   ;   ;  ;   ;   ;;  ;      ;   ;     
      ;   ;;;    ; ;;    ;   ;    ;;  ;       ;;;    ;;;    ;   ;   ;;;;;  ; ;;    ;;;     ;;;; 
      ;          ;                    ;                                    ;                    
      ;          ;                    ;                                    ;                    
      ;          ;                                                         ;                    
      
      
      (inherit get-edit-target-window)
      
      (define/public (split)
        (let ([canvas-to-be-split (get-edit-target-window)])
          (cond
            [(memq canvas-to-be-split definitions-canvases)
             (split-definitions canvas-to-be-split)]
            [(memq canvas-to-be-split interactions-canvases)
             (split-interactions canvas-to-be-split)]
            [else (bell)])))
      
      (define/private (split-definitions canvas-to-be-split)
        (handle-split canvas-to-be-split
                      (λ (x) (set! definitions-canvases x))
                      definitions-canvases
                      (drracket:get/extend:get-definitions-canvas)
                      definitions-text))
      
      (define/private (split-interactions canvas-to-be-split)
        (handle-split canvas-to-be-split
                      (λ (x) (set! interactions-canvases x))
                      interactions-canvases
                      (drracket:get/extend:get-interactions-canvas)
                      interactions-text))
      
      (define/private (handle-split canvas-to-be-split set-canvases! canvases canvas% text)
        (let-values ([(ox oy ow oh cursor-y)
                      (get-visible-region canvas-to-be-split)])
          (let ([orig-percentages (send resizable-panel get-percentages)]
                [orig-canvases (send resizable-panel get-children)]
                [new-canvas (new canvas% 
                                 (parent resizable-panel)
                                 (editor text)
                                 (style '()))])
            
            (set-canvases!
             (let loop ([canvases canvases])
               (cond
                 [(null? canvases) (error 'split "couldn't split; didn't find canvas")]
                 [else
                  (let ([canvas (car canvases)])
                    (if (eq? canvas canvas-to-be-split)
                        (list* new-canvas
                               canvas
                               (cdr canvases))
                        (cons canvas (loop (cdr canvases)))))])))
            
            (update-shown)
            
            ;; with-handlers prevents bad calls to set-percentages
            ;; might still leave GUI in bad state, however.
            (with-handlers ([exn:fail? (λ (x) (void))])
              (send resizable-panel set-percentages
                    (let loop ([canvases orig-canvases]
                               [percentages orig-percentages])
                      (cond
                        [(null? canvases)
                         (error 'split "couldn't split; didn't find canvas")]
                        [(null? percentages)
                         (error 'split "wrong number of percentages: ~s ~s"
                                orig-percentages
                                (send resizable-panel get-children))]
                        [else (let ([canvas (car canvases)])
                                (if (eq? canvas-to-be-split canvas)
                                    (list* (/ (car percentages) 2)
                                           (/ (car percentages) 2)
                                           (cdr percentages))
                                    (cons
                                     (car percentages)
                                     (loop (cdr canvases)
                                           (cdr percentages)))))]))))
            
            (set-visible-region new-canvas ox oy ow oh cursor-y)
            (set-visible-region canvas-to-be-split ox oy ow oh cursor-y)
            
            (send new-canvas focus))))
      
      ;; split-demand : menu-item -> void
      ;; enables the menu-item if splitting is allowed, disables otherwise
      (define/private (split-demand item)
        (let ([canvas-to-be-split (get-edit-target-window)])
          (send item enable
                (or (memq canvas-to-be-split definitions-canvases)
                    (memq canvas-to-be-split interactions-canvases))))) 
      
      ;; collapse-demand : menu-item -> void
      ;; enables the menu-item if collapsing is allowed, disables otherwise
      (define/private (collapse-demand item)
        (let ([canvas-to-be-split (get-edit-target-window)])
          (cond
            [(memq canvas-to-be-split definitions-canvases)
             (send item enable (2 . <= . (length definitions-canvases)))]
            [(memq canvas-to-be-split interactions-canvases)
             (send item enable (2 . <= . (length interactions-canvases)))]
            [else
             (send item enable #f)])))
      
      ;; get-visible-region : editor-canvas -> number number number number (union #f number)
      ;; calculates the visible region of the editor in this editor-canvas, returning
      ;; four numbers for the x, y, width and height of the visible region
      ;; also, the last two booleans indiciate if the beginning and the end
      ;; of the selection was visible before the split, respectively.
      (define/private (get-visible-region canvas)
        (send canvas call-as-primary-owner
              (λ ()
                (let* ([text (send canvas get-editor)]
                       [admin (send text get-admin)]
                       [start (send text get-start-position)]
                       [end (send text get-end-position)])
                  (let-values ([(x y w h) (get-visible-area admin)])
                    (let ([ysb (box 0)])
                      (send text position-location (send text get-start-position) #f ysb)
                      (values x y w h
                              (and (= start end)
                                   (<= y (unbox ysb) (+ y h))
                                   (unbox ysb)))))))))
      
      ;; set-visible-region : editor-canvas number number number number (union #f number) -> void
      ;; sets the visible region of the text displayed by the editor canvas
      ;; to be the middle of the region (vertically) specified by x, y, w, and h.
      ;; if start-visible? and/or end-visible? are true, some special handling
      ;; is done to try to keep the start and end visible, with precendence
      ;; given to start if both are #t.
      (define/private (set-visible-region canvas x y w h cursor-y)
        (send canvas call-as-primary-owner
              (λ ()
                (let* ([text (send canvas get-editor)]
                       [admin (send text get-admin)]
                       [nwb (box 0)]
                       [nhb (box 0)])
                  (send admin get-view #f #f nwb nhb)
                  (let* ([nw (unbox nwb)]
                         [nh (unbox nhb)]
                         
                         [nx x]
                         [raw-y (- (+ y (/ h 2)) (/ nh 2))]
                         [ny (if (and cursor-y 
                                      (not (<= raw-y cursor-y (+ raw-y nh))))
                                 (- cursor-y (/ nh 2))
                                 raw-y)])
                    (send canvas scroll-to nx ny nw nh #t)
                    (void))))))
      
      ;; get-visible-area : admin -> number number number number
      ;; returns the visible area for this admin
      (define/private (get-visible-area admin)
        (let ([bx (box 0)]
              [by (box 0)]
              [bw (box 0)]
              [bh (box 0)])
          (send admin get-view bx by bw bh)
          (values (unbox bx)
                  (unbox by)
                  (unbox bw)
                  (unbox bh))))
      
      (define/public (collapse)
        (let* ([target (get-edit-target-window)])
          (cond
            [(memq target definitions-canvases)
             (collapse-definitions target)]
            [(memq target interactions-canvases)
             (collapse-interactions target)]
            [else (bell)])))
      
      (define/private (collapse-definitions target)
        (handle-collapse
         target
         (λ () definitions-canvases)
         (λ (c) (set! definitions-canvases c))))
      
      (define/private (collapse-interactions target)
        (handle-collapse
         target
         (λ () interactions-canvases)
         (λ (c) (set! interactions-canvases c))))
      
      (define/private (handle-collapse target get-canvases set-canvases!)
        (if (= 1 (length (get-canvases)))
            (bell)
            (let* ([old-percentages (send resizable-panel get-percentages)]
                   [soon-to-be-bigger-canvas #f]
                   [percentages
                    (if (and target (eq? (car (get-canvases)) target))
                        (begin
                          (set! soon-to-be-bigger-canvas (cadr (get-canvases)))
                          (cons (+ (car old-percentages)
                                   (cadr old-percentages))
                                (cddr old-percentages)))
                        (let loop ([canvases (cdr (get-canvases))]
                                   [prev-canvas (car (get-canvases))]
                                   [percentages (cdr old-percentages)]
                                   [prev-percentage (car old-percentages)])
                          (cond
                            [(null? canvases)
                             (error 'collapse "internal error.1")]
                            [(null? percentages)
                             (error 'collapse "internal error.2")]
                            [else
                             (if (and target (eq? (car canvases) target))
                                 (begin
                                   (set! soon-to-be-bigger-canvas prev-canvas)
                                   (cons (+ (car percentages)
                                            prev-percentage)
                                         (cdr percentages)))
                                 (cons prev-percentage
                                       (loop (cdr canvases)
                                             (car canvases)
                                             (cdr percentages)
                                             (car percentages))))])))])
              (unless soon-to-be-bigger-canvas
                (error 'collapse "internal error.3"))
              (set-canvases! (remq target (get-canvases)))
              (update-shown)
              
              (let ([target-admin 
                     (send target call-as-primary-owner
                           (λ ()
                             (send (send target get-editor) get-admin)))]
                    [to-be-bigger-admin 
                     (send soon-to-be-bigger-canvas call-as-primary-owner
                           (λ ()
                             (send (send soon-to-be-bigger-canvas get-editor) get-admin)))])
                (let-values ([(bx by bw bh) (get-visible-area target-admin)])
                  
                  ;; this line makes the soon-to-be-bigger-canvas bigger
                  ;; if it fails, we're out of luck, but at least we don't crash.
                  (with-handlers ([exn:fail? (λ (x) (void))])
                    (send resizable-panel set-percentages percentages))
                  
                  (let-values ([(ax ay aw ah) (get-visible-area to-be-bigger-admin)])
                    (send soon-to-be-bigger-canvas scroll-to
                          bx
                          (- by (/ (- ah bh) 2))
                          aw
                          ah
                          #t))))
              
              (send target set-editor #f)
              (send soon-to-be-bigger-canvas focus))))
      ;                                                                          
      ;                                                                          
      ;                                                                          
      ;          ;                                                               
      ;          ;                                                               
      ;          ;                                                               
      ;    ;;;   ; ;;     ;;;   ;   ;   ;      ; ;;  ;;     ;;;   ; ;;    ;   ;  
      ;   ;      ;;  ;   ;   ;  ;   ;   ;      ;;  ;;  ;   ;   ;  ;;  ;   ;   ;  
      ;   ;;     ;   ;  ;     ;  ; ; ; ;       ;   ;   ;  ;    ;  ;   ;   ;   ;  
      ;    ;;    ;   ;  ;     ;  ; ; ; ;       ;   ;   ;  ;;;;;;  ;   ;   ;   ;  
      ;      ;   ;   ;  ;     ;  ; ; ; ;       ;   ;   ;  ;       ;   ;   ;   ;  
      ;      ;   ;   ;   ;   ;    ;   ;        ;   ;   ;   ;      ;   ;   ;  ;;  
      ;   ;;;    ;   ;    ;;;     ;   ;        ;   ;   ;    ;;;;  ;   ;    ;; ;  
      ;                                                                          
      ;                                                                          
      ;                                                                          
      
      
      (define interactions-shown? #t)
      (define definitions-shown? #t)
      
      (define/private (toggle-show/hide-definitions)
        (set! definitions-shown? (not definitions-shown?))
        (unless definitions-shown?
          (set! interactions-shown? #t)))
      (define/private (toggle-show/hide-interactions)
        (set! interactions-shown? (not interactions-shown?))
        (unless  interactions-shown?
          (set! definitions-shown? #t)))
      
      (define (immediate-children parent children)
        (define (immediate child)
          (let loop ([child child])
            (define immediate-parent (send child get-parent))
            (if (and immediate-parent 
                     (eq? immediate-parent parent))
                child
                (loop immediate-parent))))
        (for/list ([child children])
          (immediate child)))
      
      (define/override (update-shown)
        (super update-shown)
        (let ([new-children
               (foldl
                (λ (shown? children sofar)
                  (if shown?
                      (append children sofar)
                      sofar))
                null
                (list interactions-shown?
                      definitions-shown?)
                (list interactions-canvases
                      definitions-canvases))]
              [old-children (send resizable-panel get-children)]
              [p (preferences:get 'drracket:unit-window-size-percentage)])
          (update-defs/ints-resize-corner)
          (send definitions-item set-label 
                (if definitions-shown?
                    (string-constant hide-definitions-menu-item-label)
                    (string-constant show-definitions-menu-item-label)))
          (send interactions-item set-label 
                (if interactions-shown?
                    (string-constant hide-interactions-menu-item-label)
                    (string-constant show-interactions-menu-item-label)))
          (send resizable-panel begin-container-sequence)
          
          ;; this might change the unit-window-size-percentage, so save/restore it
          (send resizable-panel change-children
                (λ (old)
                  (immediate-children resizable-panel new-children)))
          
          (preferences:set 'drracket:unit-window-size-percentage p)
          ;; restore preferred interactions/definitions sizes
          (when (and (= 1 (length definitions-canvases))
                     (= 1 (length interactions-canvases))
                     (= 2 (length new-children)))
            (with-handlers ([exn:fail? (λ (x) (void))])
              (send resizable-panel set-percentages
                    (list p (- 1 p)))))
          
          (send resizable-panel end-container-sequence)
          (when (ormap (λ (child)
                         (and (is-a? child editor-canvas%)
                              (not (send child has-focus?))))
                       (send resizable-panel get-children))
            (let ([new-focus
                   (let loop ([children (send resizable-panel get-children)])
                     (cond
                       [(null? children) (void)]
                       [else (let ([child (car children)])
                               (if (is-a? child editor-canvas%)
                                   child
                                   (loop (cdr children))))]))]
                  [old-focus
                   (ormap (λ (x) (and (is-a? x editor-canvas%) (send x has-focus?) x))
                          old-children)])
              
              ;; conservatively, only scroll when the focus stays in the same place.
              (when old-focus
                (when (eq? old-focus new-focus)
                  (let ([ed (send old-focus get-editor)])
                    (when ed
                      (send ed scroll-to-position 
                            (send ed get-start-position)
                            #f
                            (send ed get-end-position))))))
              
              (send new-focus focus)))
          
          (for-each
           (λ (get-item)
             (let ([item (get-item)])
               (when item
                 (send item enable definitions-shown?))))
           (list (λ () (file-menu:get-revert-item))
                 (λ () (file-menu:get-save-item))
                 (λ () (file-menu:get-save-as-item))
                 ;(λ () (file-menu:save-as-text-item)) ; Save As Text...
                 (λ () (file-menu:get-print-item))))
          (send file-menu:print-interactions-item enable interactions-shown?)))
      
      (define/augment (can-close?)
        (and (andmap (lambda (tab)
                       (or (eq? tab current-tab)
                           (and (send (send tab get-defs) can-close?)
                                (send (send tab get-ints) can-close?))))
                     tabs)
             (send interactions-text can-close?)
             (inner #t can-close?)))
      (define/augment (on-close)
        (inner (void) on-close)
        (for-each (lambda (tab)
                    (unless (eq? tab current-tab)
                      (send (send tab get-defs) on-close)
                      (send (send tab get-ints) on-close)))
                  tabs)
        (when (eq? this newest-frame)
          (set! newest-frame #f))
        (when transcript
          (stop-transcript))
        (remove-show-status-line-callback)
        (remove-bug-icon-callback)
        (send interactions-text on-close))
      
      ;; execute-callback : -> void
      ;; uses the state of the button to determine if an execution is
      ;; already running. This function is called from many places, not
      ;; just the execute button.
      (define/public (execute-callback)
        (when (send execute-button is-enabled?)
          
          ;; if the language is not-a-language, and the buffer looks like a module,
          ;; automatically make the switch to the module language
          (let ([next-settings (send definitions-text get-next-settings)])
            (when (is-a? (drracket:language-configuration:language-settings-language next-settings) 
                         drracket:language-configuration:not-a-language-language<%>)
              (when (looks-like-module? definitions-text)
                (define-values (module-language module-language-settings)
                  (get-module-language/settings))
                (when (and module-language module-language-settings)
                  (send definitions-text set-next-settings 
                        (drracket:language-configuration:language-settings
                         module-language
                         module-language-settings))))))
          
          (check-if-save-file-up-to-date)
          (when (preferences:get 'drracket:show-interactions-on-execute)
            (ensure-rep-shown interactions-text))
          (when transcript
            (record-definitions)
            (record-interactions))
          (send definitions-text just-executed)
          (send language-message set-yellow #f)
          (send interactions-canvas focus)
          (send interactions-text reset-console)
          (send interactions-text clear-undos)
          
          (define name (send definitions-text get-port-name))
          (define defs-copy (new text%))
          (send defs-copy set-style-list (send definitions-text get-style-list)) ;; speeds up the copy
          (send definitions-text copy-self-to defs-copy)
          (define text-port (open-input-text-editor defs-copy 0 'end values name #t))
          (port-count-lines! text-port)
          (send interactions-text evaluate-from-port
                text-port
                #t
                (λ ()
                  (parameterize ([current-eventspace drracket:init:system-eventspace])
                    (queue-callback 
                     (λ ()
                       (send interactions-text clear-undos))))))))
      
      (inherit revert save)
      (define/private (check-if-save-file-up-to-date)
        (when (send definitions-text save-file-out-of-date?)
          (let ([user-choice 
                 (message-box/custom
                  (string-constant drscheme)
                  (string-constant definitions-modified)
                  (string-constant ignore)
                  (string-constant revert)
                  #f
                  this
                  '(caution default=2 number-order)
                  1
                  #:dialog-mixin frame:focus-table-mixin)])
            (case user-choice
              [(1) (void)]
              [(2) (revert)]))))
      
      (inherit get-menu-bar get-focus-object get-edit-target-object)
      
      (define/override (get-editor) definitions-text)
      (define/override (get-canvas)
        (initialize-definitions-canvas)
        definitions-canvas)
      
      (define (create-definitions-canvas)
        (new (drracket:get/extend:get-definitions-canvas)
             [parent resizable-panel]
             [editor definitions-text]))
      
      (define/private (initialize-definitions-canvas)
        (unless definitions-canvas
          (set! definitions-canvas (create-definitions-canvas))))
      
      ;; wire the definitions text to the interactions text and initialize it.
      (define/private (init-definitions-text tab)
        (let ([defs (send tab get-defs)]
              [ints (send tab get-ints)])
          (send defs set-interactions-text ints)
          (send defs set-tab tab)
          (send ints set-definitions-text defs)
          (send defs change-mode-to-match)
          (send defs insert-auto-text)))
      
      
      ;                              
      ;                              
      ;                @@            
      ;    @            @            
      ;   @@@@@   $@$:  @-@$   :@@+@ 
      ;    @        -@  @+ *$  @$ -@ 
      ;    @     -$@$@  @   @  :@@$- 
      ;    @     $*  @  @   @     *@ 
      ;    @: :$ @- *@  @  +$  @  :@ 
      ;    :@@$- -$$-@@@@+@$   $+@@: 
      ;                              
      ;                              
      ;                              
      ;                              
      
      (define/public (get-current-tab) current-tab)
      
      ;; create-new-tab : -> void
      ;; creates a new tab and updates the GUI for that new tab
      (define/public create-new-tab
        (lambda ([filename #f])
          (let* ([defs (new (drracket:get/extend:get-definitions-text))]
                 [tab-count (length tabs)]
                 [new-tab (new (drracket:get/extend:get-tab)
                               (defs defs)
                               (i tab-count)
                               (frame this)
                               (defs-shown? #t)
                               (ints-shown? (not filename)))]
                 [ints (make-object (drracket:get/extend:get-interactions-text) new-tab)])
            (send new-tab set-ints ints)
            (set! tabs (append tabs (list new-tab)))
            (send tabs-panel append 
                  (gui-utils:trim-string
                   (if filename
                       (get-tab-label-from-filename filename)
                       (get-defs-tab-label defs #f))
                   200))
            (init-definitions-text new-tab)
            (when filename (send defs load-file filename))
            (change-to-nth-tab (- (send tabs-panel get-number) 1))
            (send ints initialize-console)
            (send tabs-panel set-selection (- (send tabs-panel get-number) 1))
            (set! newest-frame this)
            (update-menu-bindings))))
      
      ;; change-to-tab : tab -> void
      ;; updates current-tab, definitions-text, and interactactions-text
      ;; to be the nth tab. Also updates the GUI to show the new tab
      (inherit begin-container-sequence end-container-sequence)
      (define/public (change-to-tab tab)
        (unless (eq? current-tab tab)
          (let ([old-tab current-tab])
            (save-visible-tab-regions)
            (set! current-tab tab)
            (set! definitions-text (send current-tab get-defs))
            (set! interactions-text (send current-tab get-ints))
            
            (begin-container-sequence)
            (send definitions-text begin-edit-sequence #t #f)
            (send interactions-text begin-edit-sequence #t #f)
            (for-each (λ (defs-canvas) (send defs-canvas set-editor definitions-text #f))
                      definitions-canvases)
            (for-each (λ (ints-canvas) (send ints-canvas set-editor interactions-text #f))
                      interactions-canvases)
            
            (update-save-message)
            (update-save-button)
            (language-changed)
            (set-delegated-text definitions-text)
            
            (send definitions-text update-frame-filename)
            (update-running (send current-tab is-running?))
            (when (let ([tlw (get-top-level-focus-window)])
                    (and tlw (eq? this tlw)))
              (send current-tab touched))
            (on-tab-change old-tab current-tab)
            (send tab update-log)
            (send tab update-planet-status)
            (send tab update-execute-warning-gui)
            (restore-visible-tab-regions)
            (for-each (λ (defs-canvas) (send defs-canvas refresh))
                      definitions-canvases)
            (for-each (λ (ints-canvas) (send ints-canvas refresh))
                      interactions-canvases)
            (set-color-status! (send definitions-text is-lexer-valid?))
            (send definitions-text end-edit-sequence)
            (send interactions-text end-edit-sequence)
            (end-container-sequence)
            
            (case (send current-tab get-focus-d/i)
              [(defs) 
               (send (car definitions-canvases) focus)
               (set-text-to-search (send (car definitions-canvases) get-editor))]
              [(ints)
               (send (car interactions-canvases) focus)
               (set-text-to-search (send (car interactions-canvases) get-editor))]))))
      
      (define/pubment (on-tab-change from-tab to-tab)
        (let ([old-enabled (send from-tab get-enabled)]
              [new-enabled (send to-tab get-enabled)])
          (unless (eq? old-enabled new-enabled)
            (if new-enabled
                (enable-evaluation)
                (disable-evaluation))))
        
        (inner (void) on-tab-change from-tab to-tab))
      
      (define/public (next-tab) (change-to-delta-tab +1))
      (define/public (prev-tab) (change-to-delta-tab -1))
      
      (define/private (change-to-delta-tab dt)
        (change-to-nth-tab (modulo (+ (send current-tab get-i) dt) (length tabs))))

      ;; Re-orders the tabs according to the specified order
      (define/public (reorder-tabs tab-order)
        (unless (and
                  ((listof exact-nonnegative-integer?) tab-order)
                  (equal? (sort tab-order <)
                          (range (length tabs))))
          (raise-argument-error 'reorder-tabs 
                 "list of unique integers from 0 to n where n is the current number of tabs"
                 tab-order))
        (begin-container-sequence)
        (define-values (new-tabs-rev new-labels-rev)
          (for/fold ([new-tabs '()]
                     [new-labels '()]
                     )([new-i (in-naturals)]
                       [old-i tab-order])
            (define t (list-ref tabs old-i))
            (send t set-i new-i)
            (values (cons t new-tabs)
                    (cons (send tabs-panel get-item-label old-i)
                          new-labels))))
        (set! tabs (reverse new-tabs-rev))
        (send tabs-panel set (reverse new-labels-rev))
        (send tabs-panel set-selection (send current-tab get-i))
        (end-container-sequence)
        (update-menu-bindings)
        (update-tabs-labels))

      ;; Swaps the current tab with its right-hand neighbor
      (define/public (move-current-tab-right)
        (define i (send current-tab get-i))
        (unless (= i (- (length tabs) 1))
          (reorder-tabs
           (append (range i)
                   (list (+ i 1) i)
                   (range (+ i 2) (length tabs))))))

      ;; Swaps the current tab with its left-hand neighbor
      (define/public (move-current-tab-left)
        (define i (send current-tab get-i))
        (unless (= i 0)
          (reorder-tabs
           (append (range (- i 1))
                   (list i (- i 1))
                   (range (+ i 1) (length tabs))))))

      (define/public-final (close-current-tab)
        (cond
          [(null? tabs) (void)]
          [(null? (cdr tabs)) (void)]
          [else
           (let loop ([l-tabs tabs])
             (cond
               [(null? l-tabs) (error 'close-current-tab "uh oh.3")]
               [else
                (let ([tab (car l-tabs)])
                  (if (eq? tab current-tab)
                      (when (close-tab tab)
                        (for-each (lambda (t) (send t set-i (- (send t get-i) 1)))
                                  (cdr l-tabs))
                        (set! tabs (remq tab tabs))
                        (send tabs-panel delete (send tab get-i))
                        (update-menu-bindings) 
                        (change-to-tab
                         (argmax (λ (tab) (send tab get-last-touched)) 
                                 tabs)))
                      (loop (cdr l-tabs))))]))]))
      
      ;; a helper private method for close-current-tab -- doesn't close an arbitrary tab.
      (define/private (close-tab tab)
        (cond
          [(send tab can-close?)
           (send tab on-close)
           #t]
          [else #f]))
      
      (define/public (open-in-new-tab filename)
        (create-new-tab filename))
      
      (define/public (get-tab-count) (length tabs))
      (define/public (change-to-nth-tab n)
        (unless (< n (length tabs))
          (error 'change-to-nth-tab "number too big ~s" n))
        (change-to-tab (list-ref tabs n)))
      
      (define/private (save-visible-tab-regions)
        (send current-tab set-visible-ints
              (get-tab-visible-regions interactions-text)
              interactions-shown?)
        (send current-tab set-visible-defs 
              (get-tab-visible-regions definitions-text)
              definitions-shown?)
        (send current-tab set-focus-d/i
              (if (ormap (λ (x) (send x has-focus?)) interactions-canvases)
                  'ints
                  'defs)))
      
      (define/private (get-tab-visible-regions txt)
        (map (λ (canvas) 
               (let-values ([(x y w h _) (get-visible-region canvas)])
                 (list x y w h)))
             (send txt get-canvases)))
      
      (inherit set-text-to-search reflow-container)
      (define/private (restore-visible-tab-regions)
        (define (fix-up-canvas-numbers txt regions ints?)
          (when regions
            (let* ([canvases (send txt get-canvases)]
                   [canvases-count (length canvases)]
                   [regions-count (length regions)])
              (cond
                [(> canvases-count regions-count)
                 (let loop ([i (- canvases-count regions-count)]
                            [canvases canvases])
                   (unless (zero? i)
                     (if ints?
                         (collapse-interactions (car canvases))
                         (collapse-definitions (car canvases)))
                     (loop (- i 1)
                           (cdr canvases))))]
                [(= canvases-count regions-count)
                 (void)]
                [(< canvases-count regions-count)
                 (let loop ([i (- regions-count canvases-count)]
                            [canvases canvases])
                   (unless (zero? i)
                     (if ints?
                         (split-interactions (car canvases))
                         (split-definitions (car canvases)))
                     (loop (- i 1) 
                           (cdr canvases))))]))))
        
        (define (set-visible-regions txt regions)
          (when regions
            (for-each (λ (canvas region) 
                        (set-visible-region canvas
                                            (first region)
                                            (second region)
                                            (third region)
                                            (fourth region)
                                            #f))
                      (send txt get-canvases)
                      regions)))
        
        (let-values ([(vi is?) (send current-tab get-visible-ints)]
                     [(vd ds?) (send current-tab get-visible-defs)])
          (set! interactions-shown? is?)
          (set! definitions-shown? ds?)
          (update-shown)
          (reflow-container) ;; without this one, the percentages in the
                             ;; resizable-panel are not up to date with the children
          (fix-up-canvas-numbers definitions-text vd #f)
          (fix-up-canvas-numbers interactions-text vi #t)
          (reflow-container)
          (set-visible-regions definitions-text vd)
          (set-visible-regions interactions-text vi)))
      
      (define/private (pathname-equal? p1 p2)
        (with-handlers ([exn:fail? (λ (x) #f)])
          (string=? (path->string (normal-case-path (normalize-path p1)))
                    (path->string (normal-case-path (normalize-path p2))))))
      
      (define/override (make-visible filename)
        (let ([tab (find-matching-tab filename)])
          (when tab
            (change-to-tab tab))))
      
      (define/public (find-matching-tab filename)
        (define fn-path (if (string? filename)
                            (string->path filename)
                            filename))
        (for/or ([tab (in-list tabs)])
          (define tab-filename (send (send tab get-defs) get-filename))
          (and tab-filename
               (pathname-equal? fn-path tab-filename)
               tab)))
      
      (define/override (editing-this-file? filename)
        (ormap (λ (tab)
                 (let ([fn (send (send tab get-defs) get-filename)])
                   (and fn
                        (pathname-equal? fn filename))))
               tabs))
      
      (define/override (get-menu-item%)
        (class (super get-menu-item%)
          (inherit get-label get-plain-label)
          (define/override (restore-keybinding)
            (cond
              [(equal? (get-plain-label) (string-constant close))
               (update-close-menu-item-shortcut this)]
              [(equal? (get-plain-label) (string-constant close-tab))
               (update-close-tab-menu-item-shortcut this)]
              [else (super restore-keybinding)]))
          (super-new)))
      
      (define/override (on-activate active?)
        (when active?
          (send (get-current-tab) touched))
        (super on-activate active?))
      
      (define/private (update-menu-bindings)
        (when close-tab-menu-item
          (update-close-tab-menu-item-shortcut close-tab-menu-item))
        (update-close-menu-item-shortcut (file-menu:get-close-item)))
      
      (define/private (update-close-tab-menu-item-shortcut item)
        (define just-one? (and (pair? tabs) (null? (cdr tabs))))
        (send item set-label (if just-one? 
                                 (string-constant close-tab)
                                 (string-constant close-tab-amp)))
        (when (preferences:get 'framework:menu-bindings)
          (send item set-shortcut (if just-one? #f #\w))))
      
      (define/private (update-close-menu-item-shortcut item)
        (cond
          [(equal? (system-type) 'unix)
           (send item set-label (string-constant close-menu-item))]
          [else
           (define just-one? (and (pair? tabs) (null? (cdr tabs))))
           (send item set-label (if just-one?
                                    (string-constant close-window-menu-item)
                                    (string-constant close-window)))
           (when (preferences:get 'framework:menu-bindings)
             (send item set-shortcut-prefix (if just-one? 
                                                (get-default-shortcut-prefix) 
                                                (cons 'shift (get-default-shortcut-prefix)))))]))
      
      (define/override (file-menu:close-callback item control)
        (define just-one? (and (pair? tabs) (null? (cdr tabs))))
        (if (and (equal? (system-type) 'unix)
                   (not just-one?))
            (close-current-tab)
            (super file-menu:close-callback item control)))
      
      ;; offer-to-save-file : path -> void
      ;; bring the tab that edits the file named by `path' to the front
      ;; and opens a dialog asking if it should be saved.
      (define/public (offer-to-save-file path)
        (let ([original-tab current-tab]
              [tab-to-save (find-matching-tab path)])
          (when tab-to-save
            (let ([defs-to-save (send tab-to-save get-defs)])
              (when (send defs-to-save is-modified?)
                (unless (eq? tab-to-save original-tab)
                  (change-to-tab tab-to-save))
                (send defs-to-save user-saves-or-not-modified? #f)
                (unless (eq? tab-to-save original-tab)
                  (change-to-tab original-tab)))))))
      
      
      ;;
      ;; end tabs
      ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define/public (get-definitions-text) definitions-text)
      (define/public (get-interactions-text) interactions-text)
      
      (define/public (get-definitions/interactions-panel-parent)
        toolbar/rest-panel)
      
      (inherit delegated-text-shown? hide-delegated-text show-delegated-text
               set-show-menu-sort-key)
      (define/override (add-show-menu-items show-menu)
        (super add-show-menu-items show-menu)
        (set! definitions-item
              (make-object menu:can-restore-menu-item%
                (string-constant hide-definitions-menu-item-label)
                (get-show-menu)
                (λ (_1 _2) 
                  (toggle-show/hide-definitions)
                  (update-shown))
                #\d
                (string-constant definitions-menu-item-help-string)))
        (set-show-menu-sort-key definitions-item 101)
        (set! interactions-item
              (make-object menu:can-restore-menu-item%
                (string-constant show-interactions-menu-item-label)
                (get-show-menu)
                (λ (_1 _2) 
                  (toggle-show/hide-interactions)
                  (update-shown))
                #\e
                (string-constant interactions-menu-item-help-string)))
        (set-show-menu-sort-key interactions-item 102)
        
        (let ([layout-item
               (new menu:can-restore-menu-item%
                    [label (string-constant use-horizontal-layout)]
                    [parent (get-show-menu)]
                    [callback (λ (x y) 
                                (define vertical? (send resizable-panel get-vertical?)) 
                                (preferences:set 'drracket:defs/ints-horizontal vertical?)
                                (send resizable-panel set-orientation vertical?)
                                (define update-shown? (or (not interactions-shown?)
                                                          (not definitions-shown?)))
                                (unless interactions-shown?
                                  (toggle-show/hide-interactions))
                                (unless definitions-shown?
                                  (toggle-show/hide-definitions))
                                (when update-shown?
                                  (update-shown)))]
                    [demand-callback
                     (λ (mi) (send mi set-label (if (send resizable-panel get-vertical?)
                                                    (string-constant use-horizontal-layout)
                                                    (string-constant use-vertical-layout))))]
                    [shortcut (if (member 'shift (get-default-shortcut-prefix))
                                  #f
                                  #\l)]
                    [shortcut-prefix (if (member 'shift (get-default-shortcut-prefix))
                                         (get-default-shortcut-prefix)
                                         (cons 'shift (get-default-shortcut-prefix)))])])
          (set-show-menu-sort-key layout-item 103))
        
        (let ([overview-menu-item 
               (new menu:can-restore-menu-item%
                    (shortcut #\u)
                    (label 
                     (if (delegated-text-shown?)
                         (string-constant hide-overview)
                         (string-constant show-overview)))
                    (parent (get-show-menu))
                    (callback
                     (λ (menu evt)
                       (if (delegated-text-shown?)
                           (begin
                             (send menu set-label (string-constant show-overview))
                             (preferences:set 'framework:show-delegate? #f)
                             (hide-delegated-text))
                           (begin
                             (send menu set-label (string-constant hide-overview))
                             (preferences:set 'framework:show-delegate? #t)
                             (show-delegated-text))))))])
          (set-show-menu-sort-key overview-menu-item 301))
        
        (set! module-browser-menu-item
              (new menu:can-restore-menu-item%
                   (label (if module-browser-shown?
                              (string-constant hide-module-browser)
                              (string-constant show-module-browser)))
                   (parent (get-show-menu))
                   (callback
                    (λ (menu evt)
                      (if module-browser-shown?
                          (hide-module-browser)
                          (show-module-browser))))))
        (set-show-menu-sort-key module-browser-menu-item 401)
        
        (set! toolbar-menu (new menu% 
                                [parent show-menu]
                                [label (string-constant toolbar)]))
        (set-show-menu-sort-key toolbar-menu 1)
        (set! toolbar-left-menu-item
              (new checkable-menu-item%
                   [label (string-constant toolbar-on-left)]
                   [parent toolbar-menu]
                   [callback (λ (x y) (set-toolbar-left))]
                   [checked #f]))
        (set! toolbar-top-menu-item
              (new checkable-menu-item%
                   [label (string-constant toolbar-on-top)]
                   [parent toolbar-menu]
                   [callback (λ (x y) (set-toolbar-top))]
                   [checked #f]))
        (set! toolbar-top-no-label-menu-item
              (new checkable-menu-item%
                   [label (string-constant toolbar-on-top-no-label)]
                   [parent toolbar-menu]
                   [callback (λ (x y) (set-toolbar-top-no-label))]
                   [checked #f]))
        (set! toolbar-right-menu-item
              (new checkable-menu-item%
                   [label (string-constant toolbar-on-right)]
                   [parent toolbar-menu]
                   [callback (λ (x y) (set-toolbar-right))]
                   [checked #f]))
        (set! toolbar-hidden-menu-item
              (new checkable-menu-item%
                   [label (string-constant toolbar-hidden)]
                   [parent toolbar-menu]
                   [callback (λ (x y) (set-toolbar-hidden))]
                   [checked #f]))
        
        (set! logger-menu-item
              (new menu-item%
                   [label (string-constant show-log)]
                   [parent show-menu]
                   [callback
                    (λ (x y) (send current-tab toggle-log))]))
        (set-show-menu-sort-key logger-menu-item 205)
        
        
        (set! show-line-numbers-menu-item
              (new menu:can-restore-menu-item%
                   [label (if (show-line-numbers?)
                              (string-constant hide-line-numbers/menu)
                              (string-constant show-line-numbers/menu))]
                   [parent (get-show-menu)]
                   [callback (lambda (self event)
                               (define value (preferences:get 'drracket:show-line-numbers?))
                               (preferences:set 'drracket:show-line-numbers? (not value))
                               (show-line-numbers! (not value)))]))
        (set-show-menu-sort-key show-line-numbers-menu-item 302)
        
        (define show-column-guide-menu-item
          (new menu:can-restore-menu-item%
               [label ""]
               [parent (get-show-menu)]
               [demand-callback (λ (itm) 
                                  (define pv (preferences:get 'framework:column-guide-width))
                                  (send itm set-label
                                        (format (if (car pv)
                                                    (string-constant hide-column-width-guide)
                                                    (string-constant show-column-width-guide))
                                                (cadr pv))))]
               [callback (λ (self evt)
                           (define ov (preferences:get 'framework:column-guide-width))
                           (preferences:set 'framework:column-guide-width
                                            (list (not (car ov)) (cadr ov))))]))
        (set-show-menu-sort-key show-column-guide-menu-item 303)
        
        (let ()
          (define (font-adjust adj label key shortcut)
            (define (adj-font _1 _2)
              (editor:set-current-preferred-font-size
               (adj 
                (editor:get-current-preferred-font-size))))
            (define (on-demand item)
              (define lab 
                (format 
                 label 
                 (adj 
                  (editor:get-current-preferred-font-size))))
              (send item set-label lab))
            (define item
             (new menu:can-restore-menu-item%
                  (shortcut shortcut)
                  (label "")
                  (parent (get-show-menu))
                  (callback adj-font)
                  (demand-callback on-demand)))
            (set-show-menu-sort-key item key))
          (font-adjust add1 (string-constant increase-font-size) -2 #\=)
          (font-adjust sub1 (string-constant decrease-font-size) -3 #\-))
        
        (let ([split
               (new menu:can-restore-menu-item%
                    (shortcut (if (equal? (system-type) 'macosx) #f #\m))
                    (label (string-constant split-menu-item-label))
                    (parent (get-show-menu))
                    (callback (λ (x y) (split)))
                    (demand-callback (λ (item) (split-demand item))))]
              [collapse
               (new menu:can-restore-menu-item% 
                    (shortcut (if (or (equal? (system-type) 'macosx)
                                      (member 'shift (get-default-shortcut-prefix)))
                                  #f
                                  #\m))
                    (shortcut-prefix (cond
                                       [(or (equal? (system-type) 'macosx)
                                            (member 'shift (get-default-shortcut-prefix)))
                                        (get-default-shortcut-prefix)]
                                       [else
                                        (cons 'shift (get-default-shortcut-prefix))]))
                    (label (string-constant collapse-menu-item-label))
                    (parent (get-show-menu))
                    (callback (λ (x y) (collapse)))
                    (demand-callback (λ (item) (collapse-demand item))))])
          (set-show-menu-sort-key split 2)
          (set-show-menu-sort-key collapse 3)))
      

;                                                  
;                                                  
;                                                  
;                                                  
;                          ;;;         ;;;         
;                          ;;;         ;;;         
;  ;;; ;; ;;;    ;;;    ;; ;;; ;;; ;;; ;;;   ;;;;  
;  ;;;;;;;;;;;  ;;;;;  ;;;;;;; ;;; ;;; ;;;  ;; ;;; 
;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; 
;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;;;;; 
;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;     
;  ;;; ;;; ;;;  ;;;;;  ;;;;;;; ;;;;;;; ;;;  ;;;;;; 
;  ;;; ;;; ;;;   ;;;    ;; ;;;  ;; ;;; ;;;   ;;;;  
;                                                  
;                                                  
;                                                  
;                                                  
;                                                      
;                                                      
;                                                      
;                                                      
;  ;;;                                                 
;  ;;;                                                 
;  ;;; ;;  ;;; ;; ;;;   ;;; ;;; ;;; ;;;;    ;;;;  ;;; ;
;  ;;;;;;; ;;;;; ;;;;;  ;;; ;;; ;;;;;; ;;  ;; ;;; ;;;;;
;  ;;; ;;; ;;;  ;;; ;;;  ;;;;;;;;; ;;;    ;;; ;;; ;;;  
;  ;;; ;;; ;;;  ;;; ;;;  ;;;; ;;;;  ;;;;  ;;;;;;; ;;;  
;  ;;; ;;; ;;;  ;;; ;;;  ;;;; ;;;;    ;;; ;;;     ;;;  
;  ;;;;;;; ;;;   ;;;;;    ;;   ;;  ;; ;;;  ;;;;;; ;;;  
;  ;;; ;;  ;;;    ;;;     ;;   ;;   ;;;;    ;;;;  ;;;  
;                                                      
;                                                      
;                                                      
;                                                      

      (field [module-browser-shown? #f]
             [module-browser-parent-panel #f]
             [module-browser-panel #f]
             [module-browser-ec #f]
             [module-browser-button #f]
             [module-browser-lib-path-check-box #f]
             [module-browser-planet-path-check-box #f]
             [module-browser-name-length-choice #f]
             [module-browser-pb #f]
             [module-browser-menu-item 'module-browser-menu-item-unset])
      
      (inherit open-status-line close-status-line update-status-line)
      
      (define/private (show-module-browser)
        (when module-browser-panel
          (when (can-browse-language?)
            (set! module-browser-shown? #t)
            (send module-browser-menu-item set-label (string-constant hide-module-browser))
            (update-module-browser-pane))))
      
      (define/private (hide-module-browser)
        (when module-browser-panel
          (set! module-browser-shown? #f)
          (send module-browser-menu-item set-label (string-constant show-module-browser))
          (set! module-browser-mouse-over-status-line-open? #f)
          (close-status-line 'plt:module-browser:mouse-over)
          (send module-browser-parent-panel change-children
                (λ (l)
                  (remq module-browser-panel l)))))
      
      (define/private (can-browse-language?)
        (let* ([lang/config (send (get-definitions-text) get-next-settings)]
               [lang (drracket:language-configuration:language-settings-language lang/config)]
               [strs (send lang get-language-position)]
               [can-browse?
                (or (is-a? lang drracket:module-language:module-language<%>)
                    (ormap (λ (x) (regexp-match #rx"PLT" x))
                           strs))])
          (unless can-browse?
            (message-box (string-constant drscheme)
                         (string-constant module-browser-only-in-plt-and-module-langs)
                         #:dialog-mixin frame:focus-table-mixin))
          can-browse?))
      
      (define module-browser-mouse-over-status-line-open? #f)
      (define/private (update-module-browser-pane)
        (open-status-line 'plt:module-browser:mouse-over)
        (set! module-browser-mouse-over-status-line-open? #t)
        (send module-browser-panel begin-container-sequence)
        (unless module-browser-ec 
          (set! module-browser-pb 
                (drracket:module-overview:make-module-overview-pasteboard
                 #t
                 (λ (x) (mouse-currently-over x))))
          (set! module-browser-ec (make-object editor-canvas%
                                    module-browser-panel
                                    module-browser-pb))
          
          (let* ([show-callback
                  (λ (cb key)
                    (if (send cb get-value)
                        (send module-browser-pb show-visible-paths key)
                        (send module-browser-pb remove-visible-paths key))
                    (preferences:set 'drracket:module-browser:hide-paths 
                                     (send module-browser-pb get-hidden-paths)))]
                 [mk-checkbox
                  (λ (key label)
                    (new check-box%
                         (parent module-browser-panel)
                         (label label)
                         (value (not (memq key (preferences:get 
                                                'drracket:module-browser:hide-paths))))
                         (callback 
                          (λ (cb _) 
                            (show-callback cb key)))))])
            (set! module-browser-lib-path-check-box (mk-checkbox 'lib show-lib-paths))
            (set! module-browser-planet-path-check-box (mk-checkbox 'planet show-planet-paths)))
          
          (set! module-browser-name-length-choice
                (new choice%
                     (parent module-browser-panel)
                     (label (string-constant module-browser-name-length))
                     (choices (list (string-constant module-browser-name-short)
                                    (string-constant module-browser-name-medium)
                                    (string-constant module-browser-name-long)
                                    (string-constant module-browser-name-very-long)))
                     (selection (preferences:get 'drracket:module-browser:name-length))
                     (callback
                      (λ (x y)
                        (let ([selection (send module-browser-name-length-choice get-selection)])
                          (preferences:set 'drracket:module-browser:name-length selection)
                          (update-module-browser-name-length selection))))))
          (update-module-browser-name-length 
           (preferences:get 'drracket:module-browser:name-length))
          
          (set! module-browser-button 
                (new button%
                     (parent module-browser-panel)
                     (label refresh)
                     (callback (λ (x y) (update-module-browser-pane)))
                     (stretchable-width #t))))
        
        (let ([p (preferences:get 'drracket:module-browser-size-percentage)])
          (send module-browser-parent-panel change-children
                (λ (l)
                  (cons module-browser-panel
                        (remq module-browser-panel l))))
          (with-handlers ([exn:fail? void])
            (send module-browser-parent-panel set-percentages (list p (- 1 p))))
          (send module-browser-parent-panel end-container-sequence)
          (calculate-module-browser)))
      
      (define/private (update-module-browser-name-length i)
        (send module-browser-pb set-name-length 
              (case i
                [(0) 'short]
                [(1) 'medium]
                [(2) 'long]
                [(3) 'very-long])))
      
      (define/private (mouse-currently-over snips)
        (when module-browser-mouse-over-status-line-open?
          (if (null? snips)
              (update-status-line 'plt:module-browser:mouse-over #f)
              (let* ([snip (car snips)]
                     [lines (send snip get-lines)]
                     [name (or (send snip get-filename)
                               (send snip get-word))]
                     [str (if lines
                              (format (string-constant module-browser-filename-format) name lines)
                              name)])
                (update-status-line 'plt:module-browser:mouse-over str)))))
      
      (define/private (calculate-module-browser)
        (let ([mod-tab current-tab])
          (let-values ([(old-break-thread old-custodian) (send mod-tab get-breakables)])
            (open-status-line 'plt:module-browser)
            (update-status-line 'plt:module-browser status-compiling-definitions)
            (send module-browser-button enable #f)
            (send module-browser-lib-path-check-box enable #f)
            (send module-browser-planet-path-check-box enable #f)
            (send module-browser-name-length-choice enable #f)
            (disable-evaluation-in-tab current-tab)
            (drracket:module-overview:fill-pasteboard 
             module-browser-pb
             (drracket:language:make-text/pos
              definitions-text
              0
              (send definitions-text last-position))
             (λ (str) (update-status-line 
                       'plt:module-browser 
                       (format module-browser-progress-constant str)))
             (λ (user-thread user-custodian)
               (send mod-tab set-breakables user-thread user-custodian)))
            (send mod-tab set-breakables old-break-thread old-custodian)
            (send mod-tab enable-evaluation)
            (send module-browser-button enable #t)
            (send module-browser-lib-path-check-box enable #t)
            (send module-browser-planet-path-check-box enable #t)
            (send module-browser-name-length-choice enable #t)
            (close-status-line 'plt:module-browser))))
      
      
      ;                                            
      ;                                            
      ;                                            
      ;                                            
      ;                                            
      ;                                            
      ;   ; ;;  ;;     ;;;   ; ;;    ;   ;    ;;;  
      ;   ;;  ;;  ;   ;   ;  ;;  ;   ;   ;   ;     
      ;   ;   ;   ;  ;    ;  ;   ;   ;   ;   ;;    
      ;   ;   ;   ;  ;;;;;;  ;   ;   ;   ;    ;;   
      ;   ;   ;   ;  ;       ;   ;   ;   ;      ;  
      ;   ;   ;   ;   ;      ;   ;   ;  ;;      ;  
      ;   ;   ;   ;    ;;;;  ;   ;    ;; ;   ;;;   
      ;                                            
      ;                                            
      ;                                            
      
      (define execute-menu-item #f)
      (define file-menu:print-interactions-item #f)
      (define file-menu:create-new-tab-item #f)
      
      (define/override (file-menu:between-new-and-open file-menu)
        (set! file-menu:create-new-tab-item
              (new menu:can-restore-menu-item%
                   (label (string-constant new-tab))
                   (shortcut #\t)
                   (parent file-menu)
                   (callback
                    (λ (x y)
                      (create-new-tab))))))
      [define/override file-menu:between-open-and-revert
        (lambda (file-menu)
          (new menu:can-restore-menu-item%
               [label (string-constant open-require-path)]
               [shortcut (if (member 'shift (get-default-shortcut-prefix)) #f #\o)]
               [shortcut-prefix (if (member 'shift (get-default-shortcut-prefix))
                                    (get-default-shortcut-prefix)
                                    (cons 'shift (get-default-shortcut-prefix)))]
               [parent file-menu]
               [callback
                (λ (x y)
                  (define editing-path (send (get-definitions-text) get-filename))
                  (define editing-module-path
                    (and editing-path
                         (match (path->module-path editing-path)
                           [`(lib ,(? string? s))
                            (define m (regexp-match #rx"^(.*/)[^/]*$" s))
                            (and m
                                 (list-ref m 1))]
                           [else #f])))
                  ;; editing-module-path won't find anything interesting
                  ;; if the get-module-path-from-user is using some other
                  ;; racket binary
                  (define pth
                    (get-module-path-from-user
                     #:init (or editing-module-path
                                (preferences:get 'drracket:open-module-path-last-used))
                     #:pref 'drracket:open-module-path-last-used
                     #:current-directory 
                     (and editing-path
                          (let-values ([(base name dir?) (split-path editing-path)])
                            base))))
                  (when pth (handler:edit-file pth)))])
          (super file-menu:between-open-and-revert file-menu)
          (make-object separator-menu-item% file-menu))]
      (define close-tab-menu-item #f)
      (define/override (file-menu:between-close-and-quit file-menu)
        (unless (equal? (system-type) 'unix)
          (set! close-tab-menu-item
                (new (get-menu-item%)
                     (label (string-constant close-tab))
                     (demand-callback
                      (λ (item)
                        (send item enable (1 . < . (send tabs-panel get-number)))))
                     (parent file-menu)
                     (callback
                      (λ (x y)
                        (close-current-tab))))))
        (super file-menu:between-close-and-quit file-menu))
      
      (define/override (file-menu:save-string) (string-constant save-definitions))
      (define/override (file-menu:save-as-string) (string-constant save-definitions-as))
      (define/override (file-menu:between-save-as-and-print file-menu)
        (let ([sub-menu (make-object menu% (string-constant save-other) file-menu)])
          (make-object menu:can-restore-menu-item%
            (string-constant save-definitions-as-text)
            sub-menu
            (λ (_1 _2)
              (let ([filename (send definitions-text put-file #f #f)])
                (when filename
                  (send definitions-text save-file/gui-error filename 'text)))))
          (make-object menu:can-restore-menu-item%
            (string-constant save-interactions)
            sub-menu
            (λ (_1 _2) 
              (send interactions-text save-file/gui-error)))
          (make-object menu:can-restore-menu-item%
            (string-constant save-interactions-as)
            sub-menu
            (λ (_1 _2)
              (let ([filename (send interactions-text put-file #f #f)])
                (when filename
                  (send interactions-text save-file/gui-error filename 'standard)))))
          (make-object menu:can-restore-menu-item%
            (string-constant save-interactions-as-text)
            sub-menu
            (λ (_1 _2)
              (let ([filename (send interactions-text put-file #f #f)])
                (when filename
                  (send interactions-text save-file/gui-error filename 'text)))))
          (make-object separator-menu-item% file-menu)
          (set! transcript-menu-item
                (make-object menu:can-restore-menu-item%
                  (string-constant log-definitions-and-interactions)
                  file-menu
                  (λ (x y)
                    (if transcript
                        (stop-transcript)
                        (start-transcript)))))
          (make-object separator-menu-item% file-menu)
          (super file-menu:between-save-as-and-print file-menu)))
      
      [define/override file-menu:print-string (λ () (string-constant print-definitions))]
      (define/override (file-menu:between-print-and-close file-menu)
        (set! file-menu:print-interactions-item
              (make-object menu:can-restore-menu-item%
                (string-constant print-interactions)
                file-menu
                (λ (_1 _2)
                  (send interactions-text print
                        #t 
                        #t
                        (preferences:get 'framework:print-output-mode)))))
        (super file-menu:between-print-and-close file-menu))
      
      (define/override (edit-menu:between-find-and-preferences edit-menu)
        (super edit-menu:between-find-and-preferences edit-menu)
        
        (define (aspell-callback f)
          (define problem (aspell-problematic?))
          (cond
            [problem
             (message-box (string-constant drscheme) problem)
             (f #t)]
            [else
             (f #f)]))
        
        (define (mk-menu-item checking-turned-on? 
                              turn-checking-on
                              pref-sym
                              shortcut
                              label)
          (new menu:can-restore-checkable-menu-item%
               [label label]
               [shortcut (if (member 'shift (get-default-shortcut-prefix))
                             #f
                             shortcut)]
               [shortcut-prefix (if (member 'shift (get-default-shortcut-prefix))
                                    (get-default-shortcut-prefix)
                                    (cons 'shift (get-default-shortcut-prefix)))]
               [parent edit-menu]
               [demand-callback
                (λ (item)
                  (define ed (get-edit-target-object))
                  (define on? (and ed (is-a? ed color:text<%>)))
                  (send item enable ed)
                  (send item check (and on? (checking-turned-on? ed))))]
               [callback
                (λ (item evt)
                  (aspell-callback
                   (λ (problem?)
                     (cond
                       [problem? (preferences:set pref-sym #f)]
                       [else
                        (define ed (get-edit-target-object))
                        (define old-val (checking-turned-on? ed))
                        (preferences:set pref-sym (not old-val))
                        (turn-checking-on ed (not old-val))]))))]))
        (mk-menu-item (λ (ed) (send ed get-spell-check-strings)) 
                      (λ (ed new-val) (send ed set-spell-check-strings new-val))
                      'framework:spell-check-strings?
                      #\c
                      (string-constant spell-check-string-constants))
        (mk-menu-item (λ (ed) (send ed get-spell-check-text)) 
                      (λ (ed new-val) (send ed set-spell-check-text new-val))
                      'framework:spell-check-text?
                      #\t
                      (string-constant spell-check-scribble-text))
        
        (new menu:can-restore-menu-item%
             [label (string-constant spell-skip-to-next-misspelled-word)]
             [shortcut (if (member 'shift (get-default-shortcut-prefix))
                           #f
                           #\n)]
             [shortcut-prefix (if (member 'shift (get-default-shortcut-prefix))
                                  (get-default-shortcut-prefix)
                                  (cons 'shift (get-default-shortcut-prefix)))]
             [parent edit-menu]
             [demand-callback
              (λ (item)
                (define ed (get-edit-target-object))
                (define on? (and ed 
                                 (is-a? ed color:text<%>)
                                 (= (send ed get-start-position) (send ed get-end-position))))
                (send item enable on?))]
             [callback
              (λ (item evt)
                (aspell-callback
                 (λ (problem?)
                   (unless problem?
                     (define ed (get-edit-target-object))
                     (define orig-pos (send ed get-start-position))
                     
                     (define (search start end mispelled?)
                       (let loop ([p start])
                         (cond
                           [(< p end)
                            (define sp (send ed get-spell-suggestions p))
                            (define found-something? (if mispelled?
                                                         (list? sp)
                                                         (not (list? sp))))
                            (cond
                              [found-something? p]
                              [else (loop (+ p 1))])]
                           [else #f])))
                     
                     (define first-well-spelled (or (search orig-pos (send ed last-position) #f)
                                                    (search 0 orig-pos #f)))
                     (cond
                       [first-well-spelled 
                        (define mispelled (or (search first-well-spelled (send ed last-position) #t)
                                              (search 0 first-well-spelled #t)))
                        (cond
                          [mispelled (send ed set-position mispelled)]
                          [else (bell)])]
                       [else (bell)])))))])
        
        (new menu:can-restore-menu-item%
             [label (string-constant spell-suggest-corrections)]
             [shortcut (if (member 'shift (get-default-shortcut-prefix))
                           #f
                           #\k)]
             [shortcut-prefix (if (member 'shift (get-default-shortcut-prefix))
                                  (get-default-shortcut-prefix)
                                  (cons 'shift (get-default-shortcut-prefix)))]
             [parent edit-menu]
             [demand-callback
              (λ (item)
                (define ed (get-edit-target-object))
                (define on? (and ed 
                                 (is-a? ed color:text<%>)
                                 (= (send ed get-start-position) (send ed get-end-position))))
                (send item enable on?))]
             [callback
              (λ (item evt)
                (aspell-callback
                 (λ (problem?)
                   (unless problem?
                     (define ed (get-edit-target-object))
                     (define orig-pos (send ed get-start-position))
                     (match (send ed get-spell-suggestions orig-pos)
                       [(list start end (cons first rest))
                        (define suggestions (cons first rest))
                        (define choice
                          (get-choices-from-user (string-constant spell-correction-suggestions)
                                                 (string-constant spell-choose-replacement-word)
                                                 suggestions
                                                 this
                                                 '(0)))
                        (when choice
                          (send ed begin-edit-sequence)
                          (send ed delete start end)
                          (send ed insert (list-ref suggestions (car choice)) start start)
                          (send ed end-edit-sequence))]
                       [_ (bell)])))))])
        
        (define dicts (get-aspell-dicts))
        (when dicts
          (define dicts-menu (new menu:can-restore-underscore-menu% 
                                  [parent edit-menu]
                                  [label (string-constant spelling-dictionaries)]))
          (define (mk-item dict label)
            (new menu:can-restore-checkable-menu-item%
                 [parent dicts-menu]
                 [label label]
                 [callback
                  (λ (item evt)
                    (define ed (get-edit-target-object))
                    (when (and ed (is-a? ed color:text<%>))
                      (preferences:set 'framework:aspell-dict dict)
                      (send ed set-spell-current-dict dict)))]
                 [demand-callback 
                  (λ (item)
                    (define ed (get-edit-target-object))
                    (send item enable (and ed (is-a? ed color:text<%>)))
                    (send item check 
                          (and ed 
                               (is-a? ed color:text<%>)
                               (equal? dict (send ed get-spell-current-dict)))))]))
          (mk-item #f (string-constant default-spelling-dictionary))
          (new separator-menu-item% [parent dicts-menu])
          (for ([dict (in-list dicts)])
            (mk-item dict dict)))
        (new menu:can-restore-menu-item%
             [label (string-constant complete-word)]
             [shortcut #\/]
             [parent edit-menu]
             [demand-callback
              (λ (mi)
                (send mi enable
                      (let ([ed (get-edit-target-object)])
                        (and ed
                             (is-a? ed text:autocomplete<%>)))))]
             [callback (λ (x y)
                         (send (get-edit-target-object) auto-complete))])
        (add-modes-submenu edit-menu))
      
      (define/override (edit-menu:between-select-all-and-find edit-menu)
        (new menu:can-restore-checkable-menu-item%
             [label (string-constant overwrite-mode)]
             [parent edit-menu]
             [demand-callback
              (λ (mi)
                (let ([target (get-edit-target-object)])
                  (send mi enable (is-a? target text%))
                  (when (is-a? target text%)
                    (send mi check (and target (send target get-overwrite-mode))))))]
             [callback (λ (x y)
                         (let ([target (get-edit-target-object)])
                           (send target set-overwrite-mode
                                 (not (send target get-overwrite-mode)))))])
        (super edit-menu:between-select-all-and-find edit-menu))
      
      ;; capability-menu-items : hash-table[menu -o> (listof (list menu-item number key)))
      (define capability-menu-items (make-hasheq))
      (define/public (register-capability-menu-item key menu)
        (let ([items (send menu get-items)])
          (when (null? items)
            (error 'register-capability-menu-item "menu ~e has no items" menu))
          (let* ([menu-item (last items)]
                 [this-one (list menu-item (- (length items) 1) key)]
                 [old-ones (hash-ref capability-menu-items menu (λ () '()))])
            (hash-set! capability-menu-items menu (cons this-one old-ones)))))
      
      (define/private (update-items/capability menu)
        (let* ([old-items (send menu get-items)]
               [new-items (begin '(get-items/capability menu)
                                 old-items)])
          (unless (equal? old-items new-items)
            (for-each (λ (i) (send i delete)) old-items)
            (for-each (λ (i) (send i restore)) new-items))))
      (define/private (get-items/capability menu)
        (let loop ([capability-items (reverse (hash-ref capability-menu-items menu '()))]
                   [all-items (send menu get-items)]
                   [i 0])
          (cond
            [(null? capability-items) all-items]
            [(pair? capability-items)
             (let* ([cap-item-list (car capability-items)]
                    [cap-item (list-ref cap-item-list 0)]
                    [cap-num (list-ref cap-item-list 1)]
                    [cap-key (list-ref cap-item-list 2)])
               (cond
                 [(= cap-num i)
                  (let ([is-on? (get-current-capability-value cap-key)])
                    (cond
                      [is-on?
                       (cond
                         [(null? all-items)
                          (cons cap-item (loop (cdr capability-items) null (+ i 1)))]
                         [(pair? all-items)
                          (if (eq? (car all-items) cap-item)
                              (cons cap-item (loop (cdr capability-items) (cdr all-items) (+ i 1)))
                              (cons cap-item (loop (cdr capability-items) all-items (+ i 1))))])]
                      [else
                       (cond
                         [(null? all-items)
                          (loop (cdr capability-items) null (+ i 1))]
                         [(pair? all-items)
                          (if (eq? (car all-items) cap-item)
                              (loop (cdr capability-items) (cdr all-items) (+ i 1))
                              (loop (cdr capability-items) all-items (+ i 1)))])]))]
                 [else (cons (car all-items)
                             (loop capability-items
                                   (cdr all-items)
                                   (+ i 1)))]))])))
      
      (define/private (get-current-capability-value key)
        (define language-settings (send (get-definitions-text) get-next-settings))
        (define new-language
          (drracket:language-configuration:language-settings-language language-settings))
        (send new-language capability-value key))
      
      (define language-menu 'uninited-language-menu)
      (define language-specific-menu 'language-specific-menu-not-yet-init)
      (define insert-menu 'insert-menu-not-yet-init)
      (define/public (get-insert-menu) insert-menu)
      (define/public (get-special-menu) insert-menu)
      
      (define/public (choose-language-callback)
        (let ([new-settings (drracket:language-configuration:language-dialog
                             #f
                             (send definitions-text get-next-settings)
                             this)])
          (when new-settings
            (send definitions-text set-next-settings new-settings))))
      
      ;; must be called from on-demand (on each menu click), or the state won't be handled properly
      (define/private (update-teachpack-menu)
        (for-each (λ (item) (send item delete)) teachpack-items)
        (let ([tp-callbacks (get-current-capability-value 'drscheme:teachpack-menu-items)])
          (cond
            [tp-callbacks
             (let* ([language (drracket:language-configuration:language-settings-language
                               (send (get-definitions-text) get-next-settings))]
                    [settings (drracket:language-configuration:language-settings-settings
                               (send (get-definitions-text) get-next-settings))]
                    [tp-names ((teachpack-callbacks-get-names tp-callbacks) settings)]
                    [update-settings
                     (λ (settings)
                       (send (get-definitions-text) set-next-settings 
                             (drracket:language-configuration:language-settings language settings))
                       (send (get-definitions-text) teachpack-changed)
                       (update-teachpack-menu))])
               (set! teachpack-items
                     (list*
                      (make-object separator-menu-item% language-menu)
                      (new menu:can-restore-menu-item%
                           [label (string-constant add-teachpack-menu-item-label)]
                           [parent language-menu]
                           [callback
                            (λ (_1 _2)
                              (update-settings ((teachpack-callbacks-add tp-callbacks)
                                                settings
                                                this)))])
                      (let ([mi (new menu:can-restore-menu-item% 
                                     [label (string-constant clear-all-teachpacks-menu-item-label)]
                                     [parent language-menu]
                                     [callback
                                      (λ (_1 _2) 
                                        (update-settings 
                                         ((teachpack-callbacks-remove-all tp-callbacks)
                                          settings)))])])
                        (send mi enable (not (null? tp-names)))
                        mi)
                      (map (λ (name)
                             (new menu:can-restore-menu-item%
                                  [label (gui-utils:format-literal-label 
                                          (string-constant clear-teachpack)
                                          name)]
                                  [parent language-menu]
                                  [callback
                                   (λ (item evt)
                                     (update-settings 
                                      ((teachpack-callbacks-remove tp-callbacks)
                                       settings name)))]))
                           tp-names))))]
            [else 
             (set! teachpack-items 
                   (list
                    (new menu:can-restore-menu-item%
                         [label (string-constant add-teachpack-menu-item-label)]
                         [parent language-menu]
                         [callback
                          (λ (_1 _2)
                            (message-box 
                             (string-constant drscheme)
                             (gui-utils:format-literal-label 
                              (string-constant teachpacks-only-in-languages)
                              (apply 
                               string-append
                               (reverse
                                (filter
                                 values
                                 (map (λ (l) 
                                        (and 
                                         (send l capability-value 'drscheme:teachpack-menu-items)
                                         (format "\n  ~a" (send l get-language-name))))
                                      (drracket:language-configuration:get-languages))))))
                             this
                             #:dialog-mixin frame:focus-table-mixin))])))])))
      
      (define/private (initialize-menus)
        (define mb (get-menu-bar))
        (set! language-menu (new (get-menu%) 
                                 [label (string-constant language-menu-name)]
                                 [parent mb]))
        (set! language-specific-menu (new (get-menu%) 
                                          [label (drracket:language:get-capability-default
                                                  'drscheme:language-menu-title)]
                                          [parent mb]))
        (define ((send-method method) _1 _2)
          (define text (get-focus-object))
          (when (is-a? text racket:text<%>)
            (method text)))
        (define (show/hide-capability-menus)
          (for ([menu (in-list (send (get-menu-bar) get-items))])
            (update-items/capability menu)))
        
        (make-object menu:can-restore-menu-item%
          (string-constant choose-language-menu-item-label)
          language-menu
          (λ (_1 _2) (choose-language-callback))
          #\l)
        
        (set! execute-menu-item
              (make-object menu:can-restore-menu-item%
                (string-constant execute-menu-item-label)
                language-specific-menu
                (λ (_1 _2) (execute-callback))
                #\r
                (string-constant execute-menu-item-help-string)))
        (make-object menu:can-restore-menu-item%
          (string-constant ask-quit-menu-item-label)
          language-specific-menu
          (λ (_1 _2) (send current-tab break-callback))
          #\b
          (string-constant ask-quit-menu-item-help-string))
        (make-object menu:can-restore-menu-item%
          (string-constant force-quit-menu-item-label)
          language-specific-menu
          (λ (_1 _2) (send interactions-text kill-evaluation))
          #\k
          (string-constant force-quit-menu-item-help-string))
        (when (custodian-memory-accounting-available?)
          (new menu-item%
               [label (string-constant limit-memory-menu-item-label)]
               [parent language-specific-menu]
               [callback
                (λ (item b)
                  (let ([num (get-mbytes this 
                                         (let ([limit (send interactions-text get-custodian-limit)])
                                           (and limit
                                                (floor (/ limit 1024 1024)))))])
                    (when num
                      (cond
                        [(equal? num #t)
                         (preferences:set 'drracket:child-only-memory-limit #f)
                         (send interactions-text set-custodian-limit #f)]
                        [else
                         (preferences:set 'drracket:child-only-memory-limit 
                                          (* 1024 1024 num))
                         (send interactions-text set-custodian-limit
                               (* 1024 1024 num))]))))]))
        
        (new menu:can-restore-menu-item%
             (label (string-constant clear-error-highlight-menu-item-label))
             (parent language-specific-menu)
             (callback
              (λ (_1 _2) 
                (let* ([tab  (get-current-tab)]
                       [ints (send tab get-ints)]
                       [defs (send tab get-defs)])
                  (send ints reset-error-ranges)
                  (send defs clear-test-coverage))))
             (help-string (string-constant clear-error-highlight-item-help-string))
             (demand-callback
              (λ (item)
                (let* ([tab (get-current-tab)]
                       [ints (send tab get-ints)])
                  (send item enable (or (send ints get-error-ranges)
                                        (send tab get-test-coverage-info-visible?)))))))
        
        (new menu:can-restore-menu-item%
             (label (string-constant jump-to-next-error-highlight-menu-item-label))
             (parent language-specific-menu)
             (shortcut #\.)
             (callback (λ (_1 _2) (jump-to-next-error-loc)))
             (demand-callback
              (λ (item)
                (let* ([tab (get-current-tab)]
                       [ints (send tab get-ints)])
                  (send item enable (send ints get-error-ranges))))))
        (new menu:can-restore-menu-item%
             (label (string-constant jump-to-prev-error-highlight-menu-item-label))
             (parent language-specific-menu)
             (shortcut (if (equal? (system-type) 'macosx) #\. #\,))
             (shortcut-prefix (if (equal? (system-type) 'macosx)
                                  (cons 'shift (get-default-shortcut-prefix))
                                  (get-default-shortcut-prefix)))
             (callback (λ (_1 _2) (jump-to-previous-error-loc)))
             (demand-callback
              (λ (item)
                (let* ([tab (get-current-tab)]
                       [ints (send tab get-ints)])
                  (send item enable (send ints get-error-ranges))))))
        (make-object separator-menu-item% language-specific-menu)
        (make-object menu:can-restore-menu-item%
          (string-constant create-executable-menu-item-label)
          language-specific-menu
          (λ (x y) (create-executable this)))
        (make-object menu:can-restore-menu-item%
          (string-constant module-browser...)
          language-specific-menu
          (λ (x y) (drracket:module-overview:module-overview this)))
        (let ()
          (define (update-menu-item i)
            (define fn (send definitions-text get-filename))
            (define lab-str (compute-label-string fn))
            (send i set-label lab-str)
            (send i enable fn))
          (define i (new menu:can-restore-menu-item%
                         [label ""]
                         [parent language-specific-menu]
                         [demand-callback update-menu-item]
                         [callback (λ (x y) 
                                     (define fn (send definitions-text get-filename))
                                     (when fn
                                       (drracket:module-overview:module-overview/file fn this)))]))
          (update-menu-item i))
        (make-object separator-menu-item% language-specific-menu)
        
        (let ([cap-val
               (λ ()
                 (define tab (get-current-tab))
                 (define defs (send tab get-defs))
                 (define settings (send defs get-next-settings))
                 (define language 
                   (drracket:language-configuration:language-settings-language settings))
                 (send language capability-value 'drscheme:tabify-menu-callback))])
          (new menu:can-restore-menu-item%
               [label (string-constant reindent-menu-item-label)]
               [parent language-specific-menu]
               [demand-callback (λ (m) (send m enable (cap-val)))]
               [callback (send-method 
                          (λ (x)
                            (let ([f (cap-val)])
                              (when f
                                (f x
                                   (send x get-start-position)
                                   (send x get-end-position))))))])
          
          (new menu:can-restore-menu-item%
               [label (string-constant reindent-all-menu-item-label)]
               [parent language-specific-menu]
               [callback 
                (send-method 
                 (λ (x)
                   (let ([f (cap-val)])
                     (when f
                       (f x 0 (send x last-position))))))]
               [shortcut #\i]
               [demand-callback (λ (m) (send m enable (cap-val)))]))
        
        (make-object menu:can-restore-menu-item%
          (string-constant box-comment-out-menu-item-label)
          language-specific-menu
          (send-method (λ (x) (send x box-comment-out-selection))))
        (make-object menu:can-restore-menu-item%
          (string-constant semicolon-comment-out-menu-item-label)
          language-specific-menu
          (send-method (λ (x) (send x comment-out-selection))))
        (make-object menu:can-restore-menu-item%
          (string-constant uncomment-menu-item-label)
          language-specific-menu
          (λ (x y)
            (let ([text (get-focus-object)])
              (when (is-a? text text%)
                (let ([admin (send text get-admin)])
                  (cond
                    [(is-a? admin editor-snip-editor-admin<%>)
                     (let ([es (send admin get-snip)])
                       (cond
                         [(is-a? es comment-box:snip%)
                          (let ([es-admin (send es get-admin)])
                            (when es-admin
                              (let ([ed (send es-admin get-editor)])
                                (when (is-a? ed racket:text<%>)
                                  (send ed uncomment-box/selection)))))]
                         [else (send text uncomment-selection)]))]
                    [else (send text uncomment-selection)]))))))
        
        (set! insert-menu
              (new (get-menu%)
                   [label (string-constant insert-menu)]
                   [parent mb]
                   [demand-callback
                    (λ (insert-menu)
                      ;; just here for convience -- it actually 
                      ;; works on all menus, not just the special menu
                      (show/hide-capability-menus))]))
        
        (let ([has-editor-on-demand
               (λ (menu-item)
                 (let ([edit (get-edit-target-object)])
                   (send menu-item enable (and edit (is-a? edit editor<%>)))))]
              [callback
               (λ (menu evt)
                 (let ([edit (get-edit-target-object)])
                   (when (and edit
                              (is-a? edit editor<%>))
                     (let ([number (get-fraction-from-user this)])
                       (when number
                         (send edit insert
                               (number-snip:make-fraction-snip number #f)))))
                   #t))]
              [insert-lambda
               (λ ()
                 (let ([edit (get-edit-target-object)])
                   (when (and edit
                              (is-a? edit editor<%>))
                     (send edit insert "\u03BB")))
                 #t)]
              [insert-large-semicolon-letters
               (λ ()
                 (let ([edit (get-edit-target-object)])
                   (when edit
                     (define language-settings (send definitions-text get-next-settings))
                     (define-values(comment-prefix comment-character)
                       (if language-settings
                           (send (drracket:language-configuration:language-settings-language
                                  language-settings)
                                 get-comment-character)
                           (values ";" #\;)))
                     (insert-large-letters comment-prefix comment-character edit this))))]
              [c% (get-menu-item%)])
          
          (frame:add-snip-menu-items 
           insert-menu 
           c%
           (λ (item)
             (let ([label (send item get-label)])
               (cond
                 [(equal? label (string-constant insert-comment-box-menu-item-label))
                  (register-capability-menu-item 'drscheme:special:insert-comment-box insert-menu)]
                 [(equal? label (string-constant insert-image-item))
                  (register-capability-menu-item 'drscheme:special:insert-image insert-menu)]))))
          
          (make-object c% (string-constant insert-fraction-menu-item-label)
            insert-menu callback 
            #f #f
            has-editor-on-demand)
          (register-capability-menu-item 'drscheme:special:insert-fraction insert-menu)
          
          (make-object c% (string-constant insert-large-letters...)
            insert-menu
            (λ (x y) (insert-large-semicolon-letters))
            #f #f
            has-editor-on-demand)
          (register-capability-menu-item 'drscheme:special:insert-large-letters insert-menu)
          
          (make-object c% (string-constant insert-lambda)
            insert-menu
            (λ (x y) (insert-lambda))
            #\\
            #f
            has-editor-on-demand)
          (register-capability-menu-item 'drscheme:special:insert-lambda insert-menu))
        
        (frame:reorder-menus this))
      
      (define/public (jump-to-previous-error-loc)
        (define-values (before after sorted) (find-before-and-after))
        (unless (null? sorted)
          (jump-to-source-loc (or before (last sorted)))))
      
      (define/public (jump-to-next-error-loc)
        (define-values (before after sorted) (find-before-and-after))
        (unless (null? sorted)
          (jump-to-source-loc (or after (car sorted)))))
      
      (define/private (find-before-and-after)
        (define tab (get-current-tab))
        (define pos (send (send tab get-defs) get-start-position))
        (define ranges (or (send (send tab get-ints) get-error-ranges) '()))
        (define sorted (sort ranges < #:key srcloc-position))
        (let loop ([before #f]
                   [lst sorted])
          (cond
            [(null? lst)
             (values before #f sorted)]
            [else
             (define fst (car lst))
             (cond 
               [(= pos (- (srcloc-position fst) 1))
                (values before 
                        (if (null? (cdr lst))
                            #f
                            (cadr lst))
                        sorted)]
               [(< pos (- (srcloc-position fst) 1))
                (values before fst sorted)]
               [else (loop (car lst) (cdr lst))])])))
      
      (define/private (jump-to-source-loc srcloc)
        (define ed (srcloc-source srcloc))
        (send ed set-position (- (srcloc-position srcloc) 1))
        (send ed set-caret-owner #f 'global)
        (send (get-interactions-text) highlight-a-single-error srcloc))
      
      (define/public (move-to-interactions) 
        (ensure-rep-shown (get-interactions-text))
        (send (get-interactions-canvas) focus))

      
      ;                          
      ;                          
      ;                          
      ;                          
      ;   ++-@@-   -+@+- +++: :++
      ;   +@@-+@  -@-:-@--@-   -@
      ;   :@:  @: @+   ++ @::@::@
      ;   :@   @: @@@@@@@ +--@--*
      ;   :@   @: @-      -@+*+@:
      ;   -@: :@- +@:::+@ :@@:@@ 
      ;   @@@ +@@: +@@@+:  ++ ++ 
      ;                          
      ;                          
      ;                          
      
      (define definitions-text (new (drracket:get/extend:get-definitions-text)))
      
      ;; tabs : (listof tab)
      (define tabs (list (new (drracket:get/extend:get-tab)
                              (defs definitions-text)
                              (frame this)
                              (i 0)
                              (defs-shown? #t)
                              (ints-shown? #t))))
      (define/public-final (get-tabs) tabs)
      
      ;; current-tab : tab
      ;; corresponds to the tabs-panel's active button.
      (define current-tab (car tabs))
      
      (define interactions-text (new (drracket:get/extend:get-interactions-text) 
                                     (context (car tabs))))
      (send (car tabs) set-ints interactions-text)
      
      (init-definitions-text (car tabs))
      
      (define/override (adjust-size-when-monitor-setup-changes?) 
        (= 1 (for/sum ([f (in-list (get-top-level-windows))])
               (if (is-a? f drracket:unit:frame<%>)
                   1
                   0))))
        
      (super-new
       [filename filename]
       [style '(toolbar-button fullscreen-button)]
       [size-preferences-key 'drracket:window-size]
       [position-preferences-key 'drracket:window-position])
      
      (initialize-menus)
      
      
      ;                                                                               
      ;                                                                               
      ;                                                                               
      ;                                 ;       ;                                     
      ;                                 ;       ;                                     
      ;                                 ;       ;                                 ;   
      ;   ; ;;    ;;;    ; ;;     ;;;   ;       ;   ;;;   ;     ;  ;;;    ;   ;  ;;;; 
      ;   ;;  ;  ;   ;   ;;  ;   ;   ;  ;       ;  ;   ;  ;     ; ;   ;   ;   ;   ;   
      ;   ;    ;     ;   ;   ;  ;    ;  ;       ;      ;   ;   ; ;     ;  ;   ;   ;   
      ;   ;    ;  ;;;;   ;   ;  ;;;;;;  ;       ;   ;;;;   ;   ; ;     ;  ;   ;   ;   
      ;   ;    ; ;   ;   ;   ;  ;       ;       ;  ;   ;    ; ;  ;     ;  ;   ;   ;   
      ;   ;;  ;  ;   ;   ;   ;   ;      ;       ;  ;   ;    ; ;   ;   ;   ;  ;;   ;   
      ;   ; ;;    ;;;;;  ;   ;    ;;;;  ;       ;   ;;;;;    ;     ;;;     ;; ;    ;; 
      ;   ;                                                  ;                        
      ;   ;                                                  ;                        
      ;   ;                                                 ;                         
      
      
      (define toolbar/rest-panel (new vertical-panel% [parent (get-area-container)]))
      
      ;; most contain only top-panel (or nothing)
      (define top-outer-panel (new horizontal-panel% 
                                   [parent toolbar/rest-panel]
                                   [alignment '(right top)]
                                   [stretchable-height #f]))
      
      [define top-panel (make-object horizontal-panel% top-outer-panel)]
      [define name-panel (new horizontal-panel%
                              (parent top-panel)
                              (alignment '(left center))
                              (stretchable-width #f)
                              (stretchable-height #f))]
      (define panel-with-tabs (new vertical-panel%
                                   (parent (get-definitions/interactions-panel-parent))))
      (define tabs-panel (new tab-panel% 
                              (font small-control-font)
                              (parent panel-with-tabs)
                              (stretchable-height #f)
                              (style '(deleted no-border))
                              (choices '("first name"))
                              (callback (λ (x y)
                                          (let ([sel (send tabs-panel get-selection)])
                                            (when sel
                                              (change-to-nth-tab sel)))))))
      (set! resizable-panel (new (if (preferences:get 'drracket:defs/ints-horizontal)
                                       horizontal-dragable/def-int%
                                       vertical-dragable/def-int%)
                                   (unit-frame this)
                                   (parent panel-with-tabs)))
      
      [set! definitions-canvas #f]
      (initialize-definitions-canvas)
      (set! definitions-canvases (list definitions-canvas))
      
      (set! interactions-canvas (new (drracket:get/extend:get-interactions-canvas)
                                       (parent resizable-panel)
                                       (editor interactions-text)))
      (set! interactions-canvases (list interactions-canvas))
      
      
      (define/public (get-definitions-canvases) 
        ;; before definition, just return null
        (if (pair? definitions-canvases)
            definitions-canvases
            null))
      (define/public (get-interactions-canvases)
        ;; before definition, just return null
        (if (pair? interactions-canvases)
            interactions-canvases
            null))
      
      (define/public (get-definitions-canvas) definitions-canvas)
      (define/public (get-interactions-canvas) interactions-canvas)
      
      (set! save-button
            (new switchable-button%
                 [parent top-panel]
                 [callback (λ (x) (when definitions-text
                                    (save)
                                    (send definitions-canvas focus)))]
                 [bitmap save-bitmap]
                 [alternate-bitmap small-save-bitmap]
                 [label (string-constant save-button-label)]))
      (register-toolbar-button save-button)
      
      (set! name-message (new drs-name-message% [parent name-panel]))
      (send name-message stretchable-width #t)
      (send name-message set-allow-shrinking 160)
      [define teachpack-items null]
      [define break-button (void)]
      [define execute-button (void)]
      (set! button-panel (new panel:horizontal-discrete-sizes% 
                              [parent top-panel]
                              [stretchable-width #t]
                              [alignment '(right center)]))
      (define/public (get-execute-button) execute-button)
      (define/public (get-break-button) break-button)
      (define/public (get-button-panel) button-panel)
      
      (inherit get-info-panel)
      
      (define color-status-canvas 
        (let ()
          (define on-string "()")
          (define color-status-canvas
            (new canvas% 
                 [parent (get-info-panel)]
                 [style '(transparent)]
                 [stretchable-width #f]
                 [paint-callback
                  (λ (c dc)
                    (when (number? th)
                      (unless color-valid?
                        (let-values ([(cw ch) (send c get-client-size)])
                          (send dc set-font small-control-font)
                          (send dc draw-text on-string 0 (- (/ ch 2) (/ th 2)))))))]))
          (define-values (tw th ta td) 
            (send (send color-status-canvas get-dc) get-text-extent
                  on-string small-control-font))
          (send color-status-canvas min-width (inexact->exact (ceiling tw)))
          color-status-canvas))
      (define color-valid? #t)
      (define/public (set-color-status! v?)
        (when color-status-canvas
          (set! color-valid? v?) 
          (send color-status-canvas refresh-now)))
      
      (define running-canvas
        (new running-canvas% [parent (get-info-panel)]))
      
      (define bug-icon
        (let* ([info-panel (get-info-panel)]
               [btn 
                (new switchable-button%
                     [parent info-panel]
                     [callback (λ (x) (show-saved-bug-reports-window))]
                     [bitmap very-small-planet-bitmap]
                     [vertical-tight? #t]
                     [label (string-constant show-planet-contract-violations)])])
          (send btn set-label-visible #f)
          (send info-panel change-children 
                (λ (l)
                  (cons btn (remq* (list btn) l))))
          btn))
      (define/private (set-bug-label v)
        (if (null? v)
            (send bug-icon show #f)
            (send bug-icon show #t)))
      (set-bug-label (preferences:get 'drracket:saved-bug-reports))
      (define remove-bug-icon-callback
        (preferences:add-callback
         'drracket:saved-bug-reports
         (λ (p v)
           (set-bug-label v))))
      
      [define func-defs-canvas (new func-defs-canvas% 
                                    (parent name-panel)
                                    (frame this))]
      
      (set! execute-button
            (new switchable-button%
                 [parent button-panel]
                 [callback (λ (x) (execute-callback))]
                 [bitmap execute-bitmap]
                 [label (string-constant execute-button-label)]))
      (register-toolbar-button execute-button #:number 100)
      
      (set! break-button
            (new switchable-button%
                 [parent button-panel]
                 [callback (λ (x) (send current-tab break-callback))]
                 [bitmap break-bitmap]
                 [label (string-constant break-button-label)]))
      (register-toolbar-button break-button #:number 101)
      
      (send top-panel change-children
            (λ (l)
              (list name-panel save-button button-panel)))
      
      (send top-panel stretchable-height #f)
      (inherit get-label)
      (let ([m (send definitions-canvas get-editor)])
        (set-save-init-shown?
         (and m (send m is-modified?))))
      
      (define language-message
        (let* ([info-panel (get-info-panel)]
               [p (new vertical-panel% 
                       [parent info-panel]
                       [alignment '(left center)])]
               [language-message (new language-label-message% [parent p] [frame this])])
          (send info-panel change-children 
                (λ (l)
                  (list* p
                         (remq* (list p)
                                l))))
          language-message))
      
      (update-save-message)
      (update-save-button)
      (language-changed)
      (set-delegated-text definitions-text)
      
      (cond
        [filename
         (set! definitions-shown? #t)
         (set! interactions-shown? #f)]
        [else
         (set! definitions-shown? #t)
         (set! interactions-shown? #t)])
      
      (update-shown)
      
      (when (= 2 (length (send resizable-panel get-children)))
        (send resizable-panel set-percentages
              (let ([p (preferences:get 'drracket:unit-window-size-percentage)])
                (list p (- 1 p)))))
      
      (set-label-prefix (string-constant drscheme))
      (set! newest-frame this)
      ;; a callback might have happened that initializes set-color-status! before the
      ;; definitions text is connected to the frame, so we do an extra initialization
      ;; now, once we know we have the right connection
      (set-color-status! (send definitions-text is-lexer-valid?))
      (send definitions-canvas focus)))
  
  ;; get-define-popup-name : (or/c #f (cons/c string? string?) (list/c string? string? string))
  ;;                         boolean 
  ;;                      -> (or/c #f string?)
  (define (get-define-popup-name info vertical?)
    (and info
         (if vertical?
             (if (pair? (cdr info))
                 (list-ref info 2)
                 "δ")
             (if (pair? (cdr info))
                 (list-ref info 1)
                 (cdr info)))))
  
  
  (define execute-warning-canvas%
    (class canvas%
      (inherit stretchable-height get-dc get-client-size min-height)
      (init-field message)
      (define/public (set-message _msg) (set! message _msg))
      
      (define/override (on-paint)
        (let ([dc (get-dc)])
          (let-values ([(w h) (get-client-size)])
            (send dc set-pen "yellow" 1 'solid)
            (send dc set-brush "yellow" 'solid)
            (send dc draw-rectangle 0 0 w h)
            (when message
              (let* ([base normal-control-font]
                     [face (send base get-face)])
                (if face
                    (send dc set-font (send the-font-list find-or-create-font
                                            (send base get-point-size)
                                            face
                                            (send base get-family)
                                            (send base get-style)
                                            'bold))
                    (send dc set-font (send the-font-list find-or-create-font
                                            (send base get-point-size)
                                            (send base get-family)
                                            (send base get-style)
                                            'bold))))
              (let-values ([(tw th _1 _2) (send dc get-text-extent message)])
                (send dc draw-text message 
                      (floor (- (/ w 2) (/ tw 2)))
                      (floor (- (/ h 2) (/ th 2)))))))))
      (super-new [style '(no-focus)])
      (let-values ([(w h d a) (send (get-dc) get-text-extent "Xy")])
        (min-height (+ 4 (floor (inexact->exact h)))))))
  
  
  ;                                                   
  ;                                                   
  ;                                                   
  ;                                                   
  ;                               ;;;                 
  ;                                                   
  ;  ;;; ;;;; ;;; ;;; ;;  ;;; ;;  ;;; ;;; ;;   ;; ;;; 
  ;  ;;;;;;;; ;;; ;;;;;;; ;;;;;;; ;;; ;;;;;;; ;;;;;;; 
  ;  ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; 
  ;  ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; 
  ;  ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; 
  ;  ;;;  ;;;;;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;;;;; 
  ;  ;;;   ;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ;; ;;; 
  ;                                               ;;; 
  ;                                           ;;;;;;  
  ;                                                   
  ;                                                   
  
  (define running-canvas%
    (class canvas%
      (inherit get-dc refresh get-client-size)
      
      (define running-frame-delay 200)  ; 5 FPS at the most (if user program is blocked or waiting)
      (define num-running-frames (vector-length running-frames))
      (define is-running? #f)
      (define frame 0)
      (define timer (make-object logging-timer% (λ () (refresh) (yield)) #f))
      
      (define/public (set-running r?)
        (cond [r?    (unless is-running? (set! frame 4))
                     (send timer start running-frame-delay #f)]
              [else  (send timer stop)
                     (refresh)])
        (set! is-running? r?))
      
      (define/override (on-paint) 
        (define dc (get-dc))
        (define bm (cond [is-running?  (define bm (vector-ref running-frames frame))
                                       (set! frame (modulo (+ frame 1) num-running-frames))
                                       bm]
                         [else  standing-frame]))
        (define-values (w h) (get-client-size))
        (send dc draw-bitmap bm 
              (- (/ w 2) (/ (send bm get-width) 2))
              (- (/ h 2) (/ (send bm get-height) 2))))
      
      (super-new [stretchable-width #f]
                 [stretchable-height #f]
                 [style '(transparent no-focus)])
      
      (inherit min-width min-height)
      
      (define all-running-frames (cons standing-frame running-frame-list))
      (min-width (apply max (map (λ (x) (send x get-width)) all-running-frames)))
      (min-height (apply max (map (λ (x) (send x get-height)) all-running-frames)))))
  
  ;; get-mbytes : top-level-window -> (union #f  ;; cancel
  ;;                                         integer[>=100] ;; a limit
  ;;                                         #t) ;; no limit
  (define (get-mbytes parent current-limit)
    (define d (new dialog%
                   [label (string-constant drscheme)]
                   [parent parent]))
    (define msg1 (new message%
                      [parent d]
                      [label (string-constant limit-memory-msg-1)]))
    (define msg1.5 (new message%
                        [parent d]
                        [label (string-constant limit-memory-msg-2)]))
    
    (define top-hp (new horizontal-panel%
                        [parent d] 
                        [stretchable-height #f]
                        [alignment '(left center)]))
    (define bot-hp (new horizontal-panel%
                        [parent d]
                        [stretchable-height #f]
                        [alignment '(left bottom)]))
    (define limited-rb
      (new radio-box%
           [label #f]
           [choices (list (string-constant limit-memory-limited))]
           [callback (λ (a b)
                       (send unlimited-rb set-selection #f)
                       (cb-checked))]
           [parent top-hp]))
    (define unlimited-rb
      (new radio-box%
           [label #f]
           [choices (list (string-constant limit-memory-unlimited))]
           [callback (λ (a b) 
                       (send limited-rb set-selection #f)
                       (cb-checked))]
           [parent bot-hp]))
    
    (define unlimited-warning-panel (new horizontal-panel%
                                         [parent d]
                                         [stretchable-width #t]
                                         [stretchable-height #f]))
    
    (define (show-unlimited-warning)
      (when (null? (send unlimited-warning-panel get-children))
        (send d begin-container-sequence)
        (define t (new text%))
        (send t insert (string-constant limit-memory-warning-prefix))
        (define between-pos (send t last-position))
        (send t insert (string-constant limit-memory-warning))
        
        (define sdb (make-object style-delta% 'change-family 'system))
        (send sdb set-delta-face (send normal-control-font get-face))
        (send sdb set-size-mult 0)
        (send sdb set-size-add (send normal-control-font get-point-size))
        (send sdb set-size-in-pixels-off #t)
        (send sdb set-weight-on 'bold)
        (define sd (make-object style-delta%))
        (send sd copy sdb)
        (send sd set-weight-on 'normal)
        
        (send t change-style sdb 0 between-pos)
        (send t change-style sd between-pos (send t last-position))
        (define ec (new editor-canvas% 
                        [editor t]
                        [parent unlimited-warning-panel]
                        [style '(no-border no-focus hide-hscroll hide-vscroll transparent)]
                        [horiz-margin 12]))
        (send t auto-wrap #t)
        (send d reflow-container)
        (send ec set-line-count (+ 1 (send t position-line (send t last-position))))
        (send t hide-caret #t)
        (send t lock #t)
        (send d end-container-sequence)
        (send unlimited-rb focus)))
    
    (define (cb-checked)
      (cond
        [(send limited-rb get-selection)
         (send tb enable #t)
         (send msg2 enable #t)
         (background black-foreground-sd)
         (let ([e (send tb get-editor)])
           (send e set-position 0 (send e last-position)))
         (send tb focus)]
        [else
         (show-unlimited-warning)
         (send tb enable #f)
         (send msg2 enable #f)
         (background gray-foreground-sd)])
      (update-ok-button-state))
    
    (define tb
      (keymap:call/text-keymap-initializer
       (λ ()
         (new text-field%
              [label #f]
              [parent top-hp]
              [init-value (if current-limit
                              (format "~a" current-limit)
                              "128")]
              [stretchable-width #f]
              [min-width 100]
              [callback
               (λ (tf e)
                 (let ([ed (send tf get-editor)])
                   (cond 
                     [(is-valid-number? ed)
                      (background clear-sd)]
                     [else
                      (background yellow-sd)]))
                 (update-ok-button-state))]))))
    
    (define (update-ok-button-state)
      (cond
        [(send limited-rb get-selection)
         (send ok-button enable (is-valid-number? (send tb get-editor)))]
        [else
         (send ok-button enable #t)]))
    
    (define msg2 (new message% [parent top-hp] [label (string-constant limit-memory-megabytes)]))
    (define bp (new horizontal-panel% [parent d]))
    (define-values (ok-button cancel-button)
      (gui-utils:ok/cancel-buttons
       bp 
       (λ (a b) 
         (cond
           [(send limited-rb get-selection)
            (set! result (string->number (send (send tb get-editor) get-text)))]
           [else
            (set! result #t)])
         (send d show #f))
       (λ (a b) (send d show #f))))
    
    (define result #f)
    
    (define clear-sd (make-object style-delta%))
    (define yellow-sd (make-object style-delta%))
    
    (define black-foreground-sd (make-object style-delta%))
    (define gray-foreground-sd (make-object style-delta%))
    
    (define (is-valid-number? txt)
      (let* ([n (string->number (send txt get-text))])
        (and n
             (exact-positive-integer? n)
             (8 . <= . n))))
    
    (define (background sd)
      (let ([txt (send tb get-editor)])
        (send txt change-style sd 0 (send txt last-position))))
    
    (send clear-sd set-delta-background "white")
    (send yellow-sd set-delta-background "yellow")
    (send black-foreground-sd set-delta-foreground "black")
    (send gray-foreground-sd set-delta-foreground "gray")
    (send d set-alignment 'left 'center)
    (send bp set-alignment 'right 'center)
    (cond
      [current-limit
       (send limited-rb set-selection 0)
       (send unlimited-rb set-selection #f)]
      [else
       (send unlimited-rb set-selection 0)
       (send limited-rb set-selection #f)])
    (update-ok-button-state)
    (cb-checked)
    (let ([e (send tb get-editor)])
      (send e set-position 0 (send e last-position)))
    (cond 
      [current-limit (send tb focus)]
      [else (send unlimited-rb focus)])
    (send d show #t)
    result)
  
  (define (limit-length l n)
    (let loop ([l l]
               [n n])
      (cond
        [(or (null? l) (zero? n))  null]
        [else (cons (car l) (loop (cdr l) (- n 1)))])))
  (define (remove-duplicate-languages l)
    (reverse
     (let loop ([l (reverse l)])
       (cond
         [(null? l) l]
         [else
          (if (member (car (car l)) (map car (cdr l)))
              (loop (cdr l))
              (cons (car l) (loop (cdr l))))]))))
  
  (define language-label-message%
    (class name-message%
      (init-field frame)
      (inherit refresh)
      
      (inherit set-message)
      (define yellow? #f)
      (define/override (get-background-color) (and yellow? "yellow"))
      (define/public (set-yellow y?) 
        (set! yellow? y?)
        (refresh))
      (define/public (set-yellow/lang y? lang) 
        (set-message #f lang)
        (set-yellow y?))
      
      (define/override (fill-popup menu reset)
        (let ([added-one? #f])
          (send (new menu-item%
                     [label (string-constant recent-languages)]
                     [callback void]
                     [parent menu])
                enable #f)
          (for-each
           (λ (name/settings)
             (let* ([name (car name/settings)]
                    [marshalled-settings (cdr name/settings)]
                    [lang (ormap
                           (λ (l) (and (equal? (send l get-language-name) name) l))
                           (drracket:language-configuration:get-languages))])
               (when lang
                 ;; this test can fail when a language has been added wrongly via the tools interface
                 ;; just ignore that menu item, in that case.
                 (let ([settings (or (send lang unmarshall-settings marshalled-settings)
                                     (send lang default-settings))])
                   (when lang
                     (set! added-one? #t)
                     (new menu-item%
                          [parent menu]
                          [label (send lang get-language-name)]
                          [callback
                           (λ (x y)
                             (send (send frame get-definitions-text)
                                   set-next-settings
                                   (drracket:language-configuration:language-settings
                                    lang
                                    settings)))]))))))
           (preferences:get 'drracket:recent-language-names))
          (unless added-one?
            (send (new menu-item% 
                       [label (string-append
                               "  << "
                               (string-constant no-recently-chosen-languages)
                               " >>")]
                       [parent menu]
                       [callback void])
                  enable #f))
          (new separator-menu-item% [parent menu]))
        (new menu-item%
             [label (string-constant choose-language-menu-item-label)]
             [parent menu]
             [callback 
              (λ (x y)
                (send frame choose-language-callback))]))
      
      (super-new [label ""]
                 [font small-control-font]
                 [string-constant-untitled (string-constant untitled)]
                 [string-constant-no-full-name-since-not-saved 
                  (string-constant no-full-name-since-not-saved)])
      
      (inherit set-allow-shrinking)
      (set-allow-shrinking 50)))
  
  
  
  ;                                                                            
  ;                                                                            
  ;                                                                            
  ;                                                                            
  ;  ;;;                                                             ;         
  ;  ;;;                                                           ;;;         
  ;  ;;; ;;  ;;; ;;;  ;; ;;;     ;;; ;; ;;;;  ;;; ;;    ;;;   ;;; ;;;;;  ;;;;  
  ;  ;;;;;;; ;;; ;;; ;;;;;;;     ;;;;; ;; ;;; ;;;;;;;  ;;;;;  ;;;;;;;;; ;;; ;; 
  ;  ;;; ;;; ;;; ;;; ;;; ;;;     ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ;;;  ;;;    
  ;  ;;; ;;; ;;; ;;; ;;; ;;;     ;;;  ;;;;;;; ;;; ;;; ;;; ;;; ;;;  ;;;   ;;;;  
  ;  ;;; ;;; ;;; ;;; ;;; ;;;     ;;;  ;;;     ;;; ;;; ;;; ;;; ;;;  ;;;     ;;; 
  ;  ;;;;;;; ;;;;;;; ;;;;;;;     ;;;   ;;;;;; ;;;;;;;  ;;;;;  ;;;  ;;;; ;; ;;; 
  ;  ;;; ;;   ;; ;;;  ;; ;;;     ;;;    ;;;;  ;;; ;;    ;;;   ;;;   ;;;  ;;;;  
  ;                      ;;;                  ;;;                              
  ;                  ;;;;;;                   ;;;                              
  ;                                                                            
  ;                                                                            
  
  
  ;; record-saved-bug-report : (listof (cons symbol string)) -> void
  ;; =Kernel= =Handler=
  (define (record-saved-bug-report table)
    (let ([recorded (preferences:get 'drracket:saved-bug-reports)])
      (unless (member table recorded)
        (preferences:set 'drracket:saved-bug-reports (shorten-to (cons table recorded) 15)))))
  
  ;; shorten-to : (listof X) number -> (listof X)
  ;; drops items from the end of the list to bring it back down to `n' items
  (define (shorten-to l n)
    (let loop ([l l]
               [n n])
      (cond
        [(zero? n) '()]
        [(null? l) '()]
        [else (cons (car l) (loop (cdr l) (- n 1)))])))
  
  (define saved-bug-reports-window #f)
  (define saved-bug-reports-panel #f)
  (define (init-saved-bug-reports-window)
    (unless saved-bug-reports-window
      (let ()
        (set! saved-bug-reports-window (new frame:basic%
                                            [label (string-constant drscheme)]
                                            [width 600]))
        (set! saved-bug-reports-panel
              (new vertical-panel% [parent (send saved-bug-reports-window get-area-container)]))
        (define hp (new horizontal-panel% 
                        [parent (send saved-bug-reports-window get-area-container)] 
                        [stretchable-width #f] 
                        [alignment '(right center)]))
        (define forget-all (new button% 
                                [label (string-constant bug-track-forget-all)] 
                                [callback 
                                 (λ (_1 _2)
                                   (send saved-bug-reports-window show #f)
                                   (preferences:set 'drracket:saved-bug-reports '()))]
                                [parent hp]))
        (void))))
  
  (preferences:add-callback
   'drracket:saved-bug-reports
   (λ (p v)
     (when saved-bug-reports-window
       (when (send saved-bug-reports-window is-shown?)
         (cond
           [(null? v)
            (send saved-bug-reports-window show #f)]
           [else
            (refresh-saved-bug-reports-window v)])))))
  
  (define (refresh-saved-bug-reports-window pref)
    (send saved-bug-reports-window begin-container-sequence)
    (send saved-bug-reports-panel change-children (λ (l) '()))
    (for-each
     (λ (item)
       (let ()
         (define (lookup k [default ""])
           (let loop ([item item])
             (cond
               [(null? item) default]
               [else (let ([rib (car item)])
                       (if (eq? (car rib) k)
                           (cdr rib)
                           (loop (cdr item))))])))
         (define vp
           (new vertical-panel% 
                [style '(border)]
                [parent saved-bug-reports-panel]
                [stretchable-height #f]))
         (define hp
           (new horizontal-panel% 
                [parent vp]
                [stretchable-height #f]))
         (define first-line-msg 
           (let ([desc (lookup 'description #f)])
             (and desc
                  (new message%
                       [label (read-line (open-input-string desc))]
                       [parent vp]
                       [stretchable-width #t]
                       [font (send (send (editor:get-standard-style-list) find-named-style 
                                         "Standard")
                                   get-font)]))))
         (define msg (new message% 
                          [stretchable-width #t]
                          [label (string-append (lookup 'component "<<unknown component>>")
                                                (let ([v (lookup 'version #f)])
                                                  (if v
                                                      (string-append " " v)
                                                      "")))]
                          [parent hp]))
         (define forget (new button% 
                             [parent hp] 
                             [callback (λ (x y) (forget-saved-bug-report item))]
                             [label (string-constant bug-track-forget)]))
         (define report (new button% 
                             [parent hp] 
                             [callback (λ (x y) 
                                         (forget-saved-bug-report item)
                                         (send-url
                                          (url->string
                                           (drracket:debug:bug-info->ticket-url item))))]
                             [label (string-constant bug-track-report)]))
         (void)))
     pref) ;; reverse list so first elements end up on top of list
    (send saved-bug-reports-window reflow-container)
    (send saved-bug-reports-window end-container-sequence))
  
  (define (forget-saved-bug-report item)
    (preferences:set 'drracket:saved-bug-reports 
                     (remove item (preferences:get 'drracket:saved-bug-reports))))
  
  (define (show-saved-bug-reports-window)
    (init-saved-bug-reports-window)
    (unless (send saved-bug-reports-window is-shown?)
      (refresh-saved-bug-reports-window (preferences:get 'drracket:saved-bug-reports)))
    (send saved-bug-reports-window show #t))
  
  
  
  ;                                                    
  ;                                                    
  ;                                                    
  ;                                                    
  ;   ;;;;                                   ;;    ;   
  ;  ;;;                                    ;  ;  ;    
  ;  ;;;; ;;; ;;;;;;;  ;;; ;; ;;;    ;;;;   ;  ;  ;    
  ;  ;;;; ;;;;;;;;;;;; ;;;;;;;;;;;  ;; ;;;  ;  ; ;     
  ;  ;;;  ;;;  ;;  ;;; ;;; ;;; ;;; ;;; ;;;   ;; ;; ;;  
  ;  ;;;  ;;;    ;;;;; ;;; ;;; ;;; ;;;;;;;      ; ;  ; 
  ;  ;;;  ;;;  ;;; ;;; ;;; ;;; ;;; ;;;         ;  ;  ; 
  ;  ;;;  ;;;  ;;; ;;; ;;; ;;; ;;;  ;;;;;;     ;  ;  ; 
  ;  ;;;  ;;;   ;;;;;; ;;; ;;; ;;;   ;;;;     ;    ;;  
  ;                                                    
  ;                                                    
  ;                                                    
  ;                                                    
  
  
  (define -frame%
    (drracket:module-language:module-language-online-expand-frame-mixin
     (frame-mixin
      (drracket:frame:mixin
       (drracket:frame:basics-mixin 
        (frame:size-pref-mixin
         (frame:searchable-text-mixin 
          (frame:searchable-mixin
           (frame:text-info-mixin 
            (frame:delegate-mixin
             (frame:status-line-mixin
              (frame:info-mixin
               (frame:text-mixin
                (frame:editor-mixin
                 (frame:standard-menus-mixin
                  (frame:register-group-mixin
                   (frame:focus-table-mixin
                    (frame:basic-mixin
                     frame%))))))))))))))))))
  
  (define-local-member-name enable-two-way-prefs)
  (define (make-two-way-prefs-dragable-panel% % pref-key)
    (class %
      (inherit get-percentages)

      (define save-prefs? #f)
      (define/public (enable-two-way-prefs) (set! save-prefs? #t))
      
      (define/augment (after-percentage-change)
        (when save-prefs?
          (let ([percentages (get-percentages)])
            (when (and (pair? percentages)
                       (pair? (cdr percentages))
                       (null? (cddr percentages)))
              (preferences:set pref-key (car percentages)))))
        (inner (void) after-percentage-change))
      (super-new)))
  
  (define drs-name-message%
    (class name-message%
      (define/override (on-choose-directory dir)
        (let ([file (finder:get-file dir
                                     (string-constant select-file)
                                     #f
                                     ""
                                     (send this get-top-level-window))])
          (when file
            (handler:edit-file file))))
      (super-new 
       [string-constant-untitled (string-constant untitled)]
       [string-constant-no-full-name-since-not-saved 
        (string-constant no-full-name-since-not-saved)])))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; lambda-snipclass is for backwards compatibility
  ;;
  (define lambda-snipclass
    (make-object (class snip-class%
                   (define/override (read p) (make-object string-snip% "λ"))
                   (super-new))))
  (send lambda-snipclass set-version 1)
  (send lambda-snipclass set-classname "drscheme:lambda-snip%")
  (send (get-the-snip-class-list) add lambda-snipclass)
  
  (define newest-frame 'nothing-yet)
  
  (define open-drscheme-window
    (case-lambda
      [() (open-drscheme-window #f)]
      [(name)
       (cond
         [(and newest-frame
               name
               (not (equal? newest-frame 'nothing-yet)) 
               (send newest-frame still-untouched?))
          (send newest-frame change-to-file name)
          (send newest-frame show #t)
          (begin0 newest-frame
                  (set! newest-frame #f))]
         [(and name ;; only open a tab if we have a filename
               (preferences:get 'drracket:open-in-tabs))
          (define frs (send (group:get-the-frame-group) get-frames))
          (let ([ac (send (group:get-the-frame-group) get-active-frame)])
            (when (and ac (send ac is-shown?))
              (set! frs (cons ac (remove ac frs)))))
          (define fr (let loop ([frs frs])
                       (cond
                         [(null? frs) #f]
                         [else (let ([fr (car frs)])
                                 (or (and (is-a? fr drracket:unit:frame<%>)
                                          fr)
                                     (loop (cdr frs))))])))
          (cond
            [fr
             (send fr open-in-new-tab name)
             (send fr show #t)
             fr]
            [else
             (create-new-drscheme-frame name)])]
         [else
          (create-new-drscheme-frame name)])]))
  
  (define (create-new-drscheme-frame filename)
    (let* ([drs-frame% (drracket:get/extend:get-unit-frame)]
           [frame (new drs-frame% (filename filename))])
      (send frame update-toolbar-visibility)
      (send frame initialize-module-language)
      (send frame show #t)
      (send (send frame get-interactions-text) initialize-console)
      frame)))

(define/contract (compute-label-string fn)
  (-> (or/c path? #f) label-string?)
  (cond
    [fn
     (define base-title (format (string-constant module-browser-in-file) ""))
     (define str (path->string fn))
     (define limit (- 200 (string-length base-title)))
     (define str-to-use
       (if (<= (string-length str) limit)
           str
           (string-append "..."
                          (substring str
                                     (+ (- (string-length str) limit) 3)
                                     (string-length str)))))
     (format (string-constant module-browser-in-file) str-to-use)]
    [else (string-constant module-browser-no-file)]))

(module+ test
  (require rackunit)
  (check-equal? (compute-label-string (string->path "x"))
                (format (string-constant module-browser-in-file) "x"))
  (check-equal? (compute-label-string #f)
                (string-constant module-browser-no-file))
  (check-equal? (string-length (compute-label-string (string->path (make-string 200 #\x))))
                200)
  (for ([i (in-range 100 300)])
    (let/ec k
      (parameterize ([error-escape-handler k])
        (check-true (string? 
                     (compute-label-string 
                      (string->path (make-string i #\x)))))))))
