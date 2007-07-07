
#|

closing:
  warning messages don't have frame as parent.....

tab panels new behavior:
  - save all tabs (pr 6689?)
  
module browser threading seems wrong.

|#

(module unit mzscheme
  (require (lib "contract.ss")
           (lib "unit.ss")
           (lib "class.ss")
           (lib "file.ss")
           (lib "etc.ss")
           (lib "list.ss")
           (lib "port.ss")
           (lib "string-constant.ss" "string-constants")
           (lib "framework.ss" "framework")
           (lib "name-message.ss" "mrlib")
           (lib "bitmap-label.ss" "mrlib")
           
           "drsig.ss"
           "auto-language.ss"
           
           (prefix drscheme:arrow: "../arrow.ss")
           
           (lib "mred.ss" "mred")
           (prefix mred: (lib "mred.ss" "mred"))

           (prefix mzlib:file: (lib "file.ss"))
           (prefix mzlib:date: (lib "date.ss")))

  (provide unit@)
  
  (define module-browser-progress-constant (string-constant module-browser-progress))
  (define status-compiling-definitions (string-constant module-browser-compiling-defns))
  (define show-lib-paths (string-constant module-browser-show-lib-paths/short))
  (define show-planet-paths (string-constant module-browser-show-planet-paths/short))
  (define refresh (string-constant module-browser-refresh))
  
  (define-unit unit@
    (import [prefix help-desk: drscheme:help-desk^]
            [prefix drscheme:app: drscheme:app^]
            [prefix drscheme:frame: drscheme:frame^]
            [prefix drscheme:text: drscheme:text^]
            [prefix drscheme:rep: drscheme:rep^]
            [prefix drscheme:language-configuration: drscheme:language-configuration/internal^]
            [prefix drscheme:language: drscheme:language^]
            [prefix drscheme:get/extend: drscheme:get/extend^]
            [prefix drscheme:module-overview: drscheme:module-overview^]
            [prefix drscheme:tools: drscheme:tools^]
            [prefix drscheme:eval: drscheme:eval^]
            [prefix drscheme:init: drscheme:init^]
            [prefix drscheme:module-language: drscheme:module-language^]
            [prefix drscheme:modes: drscheme:modes^])
    (export (rename drscheme:unit^
                    [-frame% frame%]
                    [-frame<%> frame<%>]))
    
      (define-local-member-name
        get-visible-defs
        set-visible-defs
        set-focus-d/i
        get-i
        set-i)
      (define tab<%>
        (interface (drscheme:rep:context<%>)
          get-frame
          get-defs
          get-ints
          get-visible-defs
          set-visible-defs
          set-visible-ints
          set-focus-d/i
          get-i
          set-i
          break-callback
          is-current-tab?
          get-enabled
          on-close
          can-close?))

      (define definitions-text<%> 
        (interface ()
          get-tab
	  get-next-settings
	  after-set-next-settings))
      
    (define-struct teachpack-callbacks 
      (get-names   ;; settings -> (listof string)
       add ;; settings path -> settings
       remove  ;; string[returned from teachpack-names] settings -> settings
       remove-all ;; settings -> settings
       ))
    
    ;; get rid of set-user-teachpack-cache method
    
      (keymap:add-to-right-button-menu
       (let ([old (keymap:add-to-right-button-menu)])
         (λ (menu text event)
           (old menu text event)
           (when (and (is-a? text text%)
                      (or (is-a? text (get-definitions-text%))
                          (is-a? text drscheme:rep:text%))
                      (is-a? event mouse-event%))
             (let* ([end (send text get-end-position)]
                    [start (send text get-start-position)])
               (unless (= 0 (send text last-position))
                 (let ([str (if (= end start)
                                (find-symbol
                                 text
                                 (call-with-values
                                  (λ ()
                                    (send text dc-location-to-editor-location
                                          (send event get-x)
                                          (send event get-y)))
                                  (λ (x y)
                                    (send text find-position x y))))
                                (send text get-text start end))]
                       [language
                        (let ([canvas (send text get-canvas)])
                          (and canvas
                               (let ([tlw (send canvas get-top-level-window)])
                                 (and (is-a? tlw -frame<%>)
                                      (send (send tlw get-definitions-text)
                                            get-next-settings)))))])
                   (unless (string=? str "")
                     (make-object separator-menu-item% menu)
                     (make-object menu-item%
                       (format (string-constant search-help-desk-for) 
                               (shorten-str 
                                str 
                                (- 200 (string-length (string-constant search-help-desk-for)))))
                       menu
                       (λ x (help-desk:help-desk str #f 'keyword+index 'contains language)))
                     (make-object menu-item%
                       (format (string-constant exact-lucky-search-help-desk-for) 
                               (shorten-str 
                                str 
                                (- 200 (string-length (string-constant exact-lucky-search-help-desk-for)))))
                       menu
                       (λ x (help-desk:help-desk str #t 'keyword+index 'exact language)))
		     (void)))))))))
      
      ;; find-symbol : number -> string
      ;; finds the symbol around the position `pos' (approx)
      (define (find-symbol text pos)
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
          (apply string (append before after))))
      
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
        (let* ([dlg (make-object dialog% (string-constant enter-fraction))]
               [hp (make-object horizontal-panel% dlg)]
               [_1 (make-object message% (string-constant whole-part) hp)]
               [whole (make-object text-field% #f hp void)]
               [vp (make-object vertical-panel% hp)]
               [hp2 (make-object horizontal-panel% vp)]
               [num (make-object text-field% #f hp2 void)]
               [num-m (make-object message% (string-constant numerator) hp2)]
               [hp3 (make-object horizontal-panel% vp)]
               [den (make-object text-field% #f hp3 void)]
               [den-m (make-object message% (string-constant denominator) hp3)]
               [bp (make-object horizontal-panel% dlg)]
               [ok? #f]
               [validate-number
                (λ ()
                  (let ([num-s (string->number (send num get-value))]
                        [den-s (string->number (send den get-value))]
                        [whole-s (if (string=? (send whole get-value) "")
                                     0
                                     (string->number (send whole get-value)))])
                    (if (and num-s den-s whole-s)
                        (let ([ans (+ whole-s (/ num-s den-s))])
                          (if (and (exact? ans)
                                   (real? ans)
                                   (not (integer? ans)))
                              ans
                              #f))
                        #f)))]
               [ok-callback
                (λ () 
                  (cond
                    [(validate-number)
                     (set! ok? #t)
                     (send dlg show #f)]
                    [else 
                     (message-box
                      (string-constant drscheme)
                      (string-constant invalid-number)
                      dlg)]))]
               [cancel-callback 
                (λ () (send dlg show #f))])
          (let-values ([(ok cancel) 
                        (gui-utils:ok/cancel-buttons
                         bp
                         (λ (x y) (ok-callback))
                         (λ (x y) (cancel-callback)))])
            (let ([mw (max (send den-m get-width) (send num-m get-width))])
              (send den-m min-width mw)
              (send num-m min-width mw))
            (send bp set-alignment 'right 'center)
            (send dlg show #t)
            (and ok? (validate-number)))))
      
      ;; create-executable : (instanceof drscheme:unit:frame<%>) -> void
      (define (create-executable frame)
        (let* ([definitions-text (send frame get-definitions-text)]
               [program-filename (send definitions-text get-filename)])
          (cond
            [(not program-filename)
             (message-box (string-constant create-executable-title)
                          (string-constant must-save-before-executable)
                          frame)]
            [else
             (when (or (not (send definitions-text is-modified?))
                       (gui-utils:get-choice
                        (string-constant definitions-not-saved)
                        (string-constant yes)
                        (string-constant no)
                        (string-constant drscheme)
                        #f
                        frame))
               (let ([settings (send definitions-text get-next-settings)])
                 (send (drscheme:language-configuration:language-settings-language settings)
                       create-executable
                       (drscheme:language-configuration:language-settings-settings settings)
                       frame
                       program-filename)))])))
      
      (define make-execute-bitmap 
        (bitmap-label-maker (string-constant execute-button-label) 
                            (build-path (collection-path "icons") "run.png")))
      (define make-save-bitmap 
        (bitmap-label-maker (string-constant save-button-label) 
                            (build-path (collection-path "icons") "save.png")))
      (define make-break-bitmap 
        (bitmap-label-maker (string-constant break-button-label) 
                            (build-path (collection-path "icons") "break.png")))
      
      (define-values (get-program-editor-mixin add-to-program-editor-mixin)
        (let* ([program-editor-mixin
                (mixin (editor:basic<%> (class->interface text%)) () 
                  (init-rest args) 
                  (inherit get-top-level-window) 
                  
                  (define/private (reset-highlighting) 
                    (let ([f (get-top-level-window)]) 
                      (when (and f 
                                 (is-a? f -frame<%>)) 
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
                  (drscheme:tools:only-in-phase 'drscheme:unit:get-program-editor-mixin 'phase2 'init-complete)
                  program-editor-mixin)]
               [add-to-program-editor-mixin
                (λ (mixin)
                  (drscheme:tools:only-in-phase 'drscheme:unit:add-to-program-editor-mixin 'phase1)
                  (set! program-editor-mixin (compose mixin program-editor-mixin)))])
          (values get-program-editor-mixin
                  add-to-program-editor-mixin)))
      
      
      ;; this sends a message to it's frame when it gets the focus
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
            (drscheme:tools:only-in-phase 'phase2 'init-complete)
            (unless definitions-text%
              (set! definitions-text% (make-definitions-text%)))
            definitions-text%)))
      
      (define (make-definitions-text%)
        (let ([definitions-super%
                ((get-program-editor-mixin)
                 (drscheme:module-language:module-language-put-file-mixin
                  (scheme:text-mixin
                   (color:text-mixin
                    (drscheme:rep:drs-bindings-keymap-mixin
                     (mode:host-text-mixin
                      (text:delegate-mixin
                       (text:foreground-color-mixin
                        text:info%))))))))])
          (class* definitions-super% (definitions-text<%>)
            (inherit get-top-level-window)
            
            (define interactions-text #f)
            (define/public (set-interactions-text it)
              (set! interactions-text it))
              
            (define tab #f)
            (define/public (get-tab) tab)
            (define/public (set-tab t) (set! tab t))
            
            (inherit get-surrogate set-surrogate)
            (define/public (set-current-mode mode)
              (let ([surrogate (drscheme:modes:mode-surrogate mode)])
                (set-surrogate surrogate)
                (when interactions-text
                  (send interactions-text set-surrogate surrogate)
                  (send interactions-text set-submit-predicate
                        (drscheme:modes:mode-repl-submit mode)))))

            (define/public (is-current-mode? mode)
              (let ([surrogate (drscheme:modes:mode-surrogate mode)])
                (eq? surrogate (get-surrogate))))
            
            (define/public (change-mode-to-match)
              (let* ([language-settings (get-next-settings)]
                     [language-name (and language-settings
                                         (send (drscheme:language-configuration:language-settings-language
                                                language-settings)
                                               get-language-position))])
                (let loop ([modes (drscheme:modes:get-modes)])
                  (cond
                    [(null? modes) (error 'change-mode-to-match
                                          "didn't find a matching mode")]
                    [else (let ([mode (car modes)])
                            (if ((drscheme:modes:mode-matches-language mode) language-name)
                                (unless (is-current-mode? mode)
                                  (set-current-mode mode))
                                (loop (cdr modes))))]))))
            
            (inherit begin-edit-sequence end-edit-sequence
                     delete insert last-position paragraph-start-position
                     get-character)
            
            (define save-file-metadata #f)
            (define/augment (on-save-file filename fmt)
              (inner (void) on-save-file filename fmt)
              (let* ([lang (drscheme:language-configuration:language-settings-language next-settings)]
                     [settings (drscheme:language-configuration:language-settings-settings next-settings)]
                     [name-mod (send lang get-reader-module)])
                (when name-mod ;; the reader-module method's result is used a test of whether or not the get-metadata method is used for this language
                  (let ([metadata (send lang get-metadata (filename->modname filename) settings)])
                    (begin-edit-sequence)
                    (set! save-file-metadata metadata)
                    (insert metadata 0 0)))))
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
                (let ([modified? (is-modified?)])
                  (delete 0 (string-length save-file-metadata))
                  (set! save-file-metadata #f)
                  (end-edit-sequence)
                  ;; restore modification status to where it was before the metadata is removed
                  (set-modified modified?)))
              (inner (void) after-save-file success?))
            
            (define/augment (on-load-file filename format)
              (inner (void) on-load-file filename format)
              (begin-edit-sequence))
            (define/augment (after-load-file success?)
              (when success?
                (let* ([module-language
                        (and (preferences:get 'drscheme:switch-to-module-language-automatically?)
                             (ormap 
                              (λ (lang)
                                (and (is-a? lang drscheme:module-language:module-language<%>)
                                     lang))
                              (drscheme:language-configuration:get-languages)))]
                       [module-language-settings
                        (let ([prefs-setting (preferences:get 
                                              drscheme:language-configuration:settings-preferences-symbol)])
                          (cond
                            [(eq? (drscheme:language-configuration:language-settings-language prefs-setting)
                                  module-language)
                             (drscheme:language-configuration:language-settings-settings prefs-setting)]
                            [else 
                             (and module-language
                                  (send module-language default-settings))]))])
                  (let-values ([(matching-language settings)
                                (pick-new-language
                                 this
                                 (drscheme:language-configuration:get-languages)
                                 module-language
                                 module-language-settings)])
                    (when matching-language
                      (set-next-settings
                       (drscheme:language-configuration:make-language-settings 
                        matching-language
                        settings)
                       #f))))
                (set-modified #f))
              
              (end-edit-sequence)
              (inner (void) after-load-file success?))
                              
            (inherit is-modified? run-after-edit-sequence)
            (define/override (set-modified mod?)
              (super set-modified mod?)
              (run-after-edit-sequence
               (λ ()
                 (let ([f (get-top-level-window)])
                   (when (and f
                              (is-a? f -frame<%>))
                     (send f update-save-button))))))
            (define/override set-filename
              (case-lambda
                [(fn) (set-filename fn #f)]
                [(fn tmp?)
                 (super set-filename fn tmp?)
                 (let ([f (get-top-level-window)])
                   (when (and f
                              (is-a? f -frame<%>))
                     (send f update-save-message)))]))
            
            (field
             [needs-execution-state #f]
             [already-warned-state #f]
             [execute-settings (preferences:get drscheme:language-configuration:settings-preferences-symbol)]
             [next-settings execute-settings])
            
            
            (define/pubment (get-next-settings) next-settings)
            (define/pubment set-next-settings
              (opt-lambda (_next-settings [update-prefs? #t])
                (when (or (send (drscheme:language-configuration:language-settings-language _next-settings)
                                get-reader-module)
                          (send (drscheme:language-configuration:language-settings-language next-settings)
                                get-reader-module))
                  (set-modified #t))
                (set! next-settings _next-settings)
                (change-mode-to-match)
                
                (let ([f (get-top-level-window)])
                  (when (and f
                             (is-a? f -frame<%>))
                    (send f language-changed)))
                
                (let ([lang (drscheme:language-configuration:language-settings-language next-settings)]
                      [sets (drscheme:language-configuration:language-settings-settings next-settings)])
                  (preferences:set
                   'drscheme:recent-language-names
                   (limit-length
                    (remove-duplicates
                     (cons (cons (send lang get-language-name)
                                 (send lang marshall-settings sets))
                           (preferences:get 'drscheme:recent-language-names)))
                    10)))
                
                (when update-prefs?
                  (preferences:set
                   drscheme:language-configuration:settings-preferences-symbol
                   next-settings))
                
                (after-set-next-settings _next-settings)))

	    (define/pubment (after-set-next-settings s)
              (inner (void) after-set-next-settings s))
            
            (define/public (needs-execution)
              (or needs-execution-state
                  (and (not (this-and-next-language-the-same?))
                       (string-constant needs-execute-language-changed))))
            
            (define/public (this-and-next-language-the-same?)
              (let ([execute-lang (drscheme:language-configuration:language-settings-language execute-settings)]
                    [next-lang (drscheme:language-configuration:language-settings-language next-settings)])
                (and (eq? execute-lang next-lang)
                     (equal?
                      (send execute-lang marshall-settings 
                            (drscheme:language-configuration:language-settings-settings execute-settings))
                      (send execute-lang marshall-settings 
                            (drscheme:language-configuration:language-settings-settings next-settings))))))
            
            (define/pubment (set-needs-execution-message msg)
              (set! needs-execution-state msg))
            (define/pubment (teachpack-changed)
              (set! needs-execution-state (string-constant needs-execute-teachpack-changed)))
            (define/pubment (just-executed)
              (set! execute-settings next-settings)
              (set! needs-execution-state #f)
              (set! already-warned-state #f))
            (define/pubment (already-warned?)
              already-warned-state)
            (define/pubment (already-warned)
              (set! already-warned-state #t))
            (define/augment (after-insert x y)
              (set! needs-execution-state (string-constant needs-execute-defns-edited))
              (inner (void) after-insert x y))
            (define/augment (after-delete x y)
              (set! needs-execution-state (string-constant needs-execute-defns-edited))
              (inner (void) after-delete x y))
            
            (inherit get-filename)
            (field
             [tmp-date-string #f])
            
            (inherit get-filename/untitled-name)
            (define/private (get-date-string)
              (string-append
               (mzlib:date:date->string (seconds->date (current-seconds)))
               " "
               (get-filename/untitled-name)))
            
            (define/override (on-paint before dc left top right bottom dx dy draw-caret)
              (when (and before
                         (or (is-a? dc post-script-dc%)
                             (is-a? dc printer-dc%)))
                (set! tmp-date-string (get-date-string))
                (let-values ([(w h d s) (send dc get-text-extent tmp-date-string)])
                  (send (current-ps-setup) set-editor-margin 0 (inexact->exact (ceiling h)))))
              (super on-paint before dc left top right bottom dx dy draw-caret)
              (when (and (not before)
                         (or (is-a? dc post-script-dc%)
                             (is-a? dc printer-dc%)))
                (send dc draw-text (get-date-string) 0 0)
                (void))
              
              ;; draw the arrows
              (when before
                (when error-arrows
                  (let ([old-pen (send dc get-pen)])
                    (send dc set-pen (send the-pen-list find-or-create-pen "red" 1 'solid))
                    (let loop ([pts error-arrows])
                      (cond
                        [(null? pts) (void)]
                        [(null? (cdr pts)) (void)]
                        [else (let ([pt1 (car pts)]
                                    [pt2 (cadr pts)])
                                (draw-arrow dc dx dy pt1 pt2)
                                (loop (cdr pts)))]))
                    (send dc set-pen old-pen)))))
            
            (define/private (draw-arrow dc dx dy pt1 pt2)
              (let-values ([(x1 y1) (find-poss (car pt1) (cadr pt1) (+ (cadr pt1) 1))]
                           [(x2 y2) (find-poss (car pt2) (cadr pt2) (+ (cadr pt2) 1))])
                (drscheme:arrow:draw-arrow dc x1 y1 x2 y2 dx dy)))
            
            (inherit dc-location-to-editor-location)
            (define/private (find-poss text left-pos right-pos)
              (let ([xlb (box 0)]
                    [ylb (box 0)]
                    [xrb (box 0)]
                    [yrb (box 0)])
                (send text position-location left-pos xlb ylb #t)
                (send text position-location right-pos xrb yrb #f)
                (let*-values ([(xl-off yl-off) (send text editor-location-to-dc-location (unbox xlb) (unbox ylb))]
                              [(xl yl) (dc-location-to-editor-location xl-off yl-off)]
                              [(xr-off yr-off) (send text editor-location-to-dc-location (unbox xrb) (unbox yrb))]
                              [(xr yr) (dc-location-to-editor-location xr-off yr-off)])
                  (values (/ (+ xl xr) 2)
                          (/ (+ yl yr) 2)))))

            (inherit invalidate-bitmap-cache)
            (define/public (set-error-arrows arrows)
              (set! error-arrows arrows)
              (invalidate-bitmap-cache))
            
            (field [error-arrows #f])
            
            (super-new)

	    (inherit set-max-undo-history)
	    (set-max-undo-history 'forever))))
      
      
                                                       
   ;      ;           ;; ;                             
  ;       ;          ;                                 
  ;       ;          ;                                 
 ;    ;;; ;   ;;;   ;;;  ;   ; ;;;    ;;;              
 ;   ;   ;;  ;   ;   ;   ;   ;;   ;  ;   ;             
 ;   ;    ;  ;   ;   ;   ;   ;    ;  ;   ;             
 ;   ;    ;  ;;;;;   ;   ;   ;    ;  ;;;;;             
 ;   ;    ;  ;       ;   ;   ;    ;  ;                 
 ;   ;   ;;  ;       ;   ;   ;    ;  ;                 
  ;   ;;; ;   ;;;;   ;   ;   ;    ;   ;;;;  ;   ;   ;  
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
               (when (is-a? frame -frame<%>)
                 (let* ([language-settings (send (send frame get-definitions-text) get-next-settings)]
                        [new-language (drscheme:language-configuration:language-settings-language language-settings)]
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
                              (label (format (string-constant jump-to-defn) (defn-name defn)))
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
                      [(memq (send editor get-character pos) '(#\space #\return #\newline #\( #\) #\[ #\] #\tab))
                       (offset pos)]
                      [else (loop (dir pos))])))]
               [before (search sub1 add1)]
               [after (search add1 (λ (x) x))])
          (send editor get-text before after)))
        
      (define func-defs-canvas%
        (class name-message%
	  (init-field frame)
	  
          (unless (is-a? frame -frame<%>)
            (error 'func-defs-canvas "frame is not a drscheme:unit:frame<%>"))
          
          (define sort-by-name? #f)
          (define sorting-name (string-constant sort-by-name))
          (define/private (change-sorting-order)
            (set! sort-by-name? (not sort-by-name?))
            (set! sorting-name (if sort-by-name?
                                   (string-constant sort-by-position) 
                                   (string-constant sort-by-name))))
          
          (define capability-info (drscheme:language:get-capability-default 'drscheme:define-popup))
          
          (inherit set-message set-hidden?)
          (define/public (language-changed new-language)
            (set! capability-info (send new-language capability-value 'drscheme:define-popup))
            (cond
              [capability-info
               (set-message #f (cdr capability-info))
               (set-hidden? #f)]
              [else
               (set-hidden? #t)]))
          (define/override (fill-popup menu reset)
            (when capability-info
              (let* ([text (send frame get-definitions-text)]
                     [unsorted-defns (get-definitions (car capability-info)
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
                                  (regexp-replace*
                                   #rx"&"
                                   (gui-utils:trim-string (defn-name defn) 200)
                                   "&&")
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
          
          (super-new (label "(define ...)"))))

      ;; defn = (make-defn number string number number)
      (define-struct defn (indent name start-pos end-pos))

      ;; get-definitions : boolean text -> (listof defn)
      (define (get-definitions tag-string indent? text)
        (let* ([min-indent 0]
               [defs (let loop ([pos 0])
                       (let ([defn-pos (send text find-string tag-string 'forward pos 'eof #t #f)])
                         (cond
                           [(not defn-pos) null]
                           [(in-semicolon-comment? text defn-pos)
                            (loop (+ defn-pos (string-length tag-string)))]
                           [else
                            (let ([indent (get-defn-indent text defn-pos)]
                                  [name (get-defn-name text (+ defn-pos (string-length tag-string)))])
                              (set! min-indent (min indent min-indent))
                              (cons (make-defn indent name defn-pos defn-pos)
                                    (loop (+ defn-pos (string-length tag-string)))))])))])
          
          ;; update end-pos's based on the start pos of the next defn
          (unless (null? defs)
            (let loop ([first (car defs)]
                       [defs (cdr defs)])
              (cond
                [(null? defs) 
                 (set-defn-end-pos! first (send text last-position))]
                [else (set-defn-end-pos! first (max (- (defn-start-pos (car defs)) 1)
                                                    (defn-start-pos first)))
                      (loop (car defs) (cdr defs))])))
          
          (when indent?
            (for-each (λ (defn)
                        (set-defn-name! defn
                                        (string-append
                                         (apply string
                                                (vector->list
                                                 (make-vector 
                                                  (- (defn-indent defn) min-indent) #\space)))
                                         (defn-name defn))))
                      defs))
          defs))

      ;; in-semicolon-comment: text number -> boolean
      ;; returns #t if `define-start-pos' is in a semicolon comment and #f otherwise
      (define (in-semicolon-comment? text define-start-pos)
        (let* ([para (send text position-paragraph define-start-pos)]
               [start (send text paragraph-start-position para)])
          (let loop ([pos start])
            (cond
              [(pos . >= . define-start-pos) #f]
              [(char=? #\; (send text get-character pos)) #t]
              [else (loop (+ pos 1))]))))
      
      ;; get-defn-indent : text number -> number
      ;; returns the amount to indent a particular definition
      (define (get-defn-indent text pos)
        (let* ([para (send text position-paragraph pos)]
               [para-start (send text paragraph-start-position para #t)])
          (let loop ([c-pos para-start]
                     [offset 0])
            (if (< c-pos pos)
                (let ([char (send text get-character c-pos)])
                  (cond
                    [(char=? char #\tab)
                     (loop (+ c-pos 1) (+ offset (- 8 (modulo offset 8))))]
                    [else
                     (loop (+ c-pos 1) (+ offset 1))]))
                offset))))
      
      ;; skip-to-whitespace/paren : text number -> number
      ;; skips to the next parenthesis or whitespace after `pos', returns that position.
      (define (skip-to-whitespace/paren text pos)
        (let loop ([pos pos])
          (if (>= pos (send text last-position))
              (send text last-position)
              (let ([char (send text get-character pos)])
                (cond
                  [(or (char=? #\) char)
                       (char=? #\( char)
                       (char=? #\] char)
                       (char=? #\[ char)
                       (char-whitespace? char))
                   pos]
                  [else (loop (+ pos 1))])))))
      
      ;; skip-whitespace/paren : text number -> number
      ;; skips past any parenthesis or whitespace
      (define (skip-whitespace/paren text pos)
        (let loop ([pos pos])
          (if (>= pos (send text last-position))
              (send text last-position)
              (let ([char (send text get-character pos)])
                (cond
                  [(or (char=? #\) char)
                       (char=? #\( char)
                       (char=? #\] char)
                       (char=? #\[ char)
                       (char-whitespace? char))
                   (loop (+ pos 1))]
                  [else pos])))))
      
      ;; get-defn-name : text number -> string
      ;; returns the name of the definition starting at `define-pos'
      (define (get-defn-name text define-pos)
        (if (>= define-pos (send text last-position))
            (string-constant end-of-buffer-define)
            (let* ([start-pos (skip-whitespace/paren text (skip-to-whitespace/paren text define-pos))]
                   [end-pos (skip-to-whitespace/paren text start-pos)])
              (send text get-text start-pos end-pos))))
      
      (define (set-box/f! b v) (when (box? b) (set-box! b v)))
      

                                    
                                    
                                    
   ;;                               
  ;                                 
  ;                                 
 ;;;  ; ;;  ;;;   ; ;;; ;;     ;;;  
  ;   ;;   ;   ;  ;;  ;;  ;   ;   ; 
  ;   ;        ;  ;   ;   ;   ;   ; 
  ;   ;     ;;;;  ;   ;   ;   ;;;;; 
  ;   ;    ;   ;  ;   ;   ;   ;     
  ;   ;    ;   ;  ;   ;   ;   ;     
  ;   ;     ;;;;; ;   ;   ;    ;;;; 
                                    
                                    
                                    
      (define vertical-dragable/def-int%
        (class panel:vertical-dragable%
          (init-field unit-frame)
          (inherit get-percentages)
          (define/augment (after-percentage-change)
            (let ([percentages (get-percentages)])
              (when (and (= 1
                            (length (send unit-frame get-definitions-canvases))
                            (length (send unit-frame get-interactions-canvases)))
                         (= 2 (length percentages)))
                (preferences:set 'drscheme:unit-window-size-percentage (car percentages))))
            (inner (void) after-percentage-change))
          (super-new)))
      
    (define super-frame%
      (drscheme:frame:mixin
       (drscheme:frame:basics-mixin 
        (frame:searchable-text-mixin 
         (frame:searchable-mixin
          (frame:text-info-mixin 
           (frame:delegate-mixin
            (frame:status-line-mixin
             (frame:info-mixin
              (frame:text-mixin
               (frame:open-here-mixin
                (frame:editor-mixin
                 (frame:standard-menus-mixin
                  (frame:register-group-mixin
                   (frame:basic-mixin
                    frame%)))))))))))))))
      
      (define tab%
        (class* object% (drscheme:rep:context<%> tab<%>)
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
            (send defs lock #t)
            (send ints lock #t)
            (send frame disable-evaluation-in-tab this))
          (define/public (enable-evaluation)
            (set! enabled? #t)
            (send defs lock #f)
            (send ints lock #f)
            (send frame enable-evaluation-in-tab this))
          (define/public (get-enabled) enabled?)
        
          (define/public (get-directory)
            (let ([filename (send defs get-filename)])
              (if (and (path? filename)
                       (file-exists? filename))
                  (let-values ([(base _1 _2) (split-path (mzlib:file:normalize-path filename))])
                    base)
                  #f)))
          (define/public (needs-execution)
            (send defs needs-execution))
          
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
          
          (define/public (update-running b?) (send frame update-running b?))
          
          (define/public-final (is-current-tab?) (eq? this (send frame get-current-tab)))
          
          (super-new)))
      
      ;; should only be called by the tab% object
      (define-local-member-name 
        disable-evaluation-in-tab
        enable-evaluation-in-tab)
      
      (define -frame<%>
        (interface (drscheme:frame:<%> frame:searchable-text<%> frame:delegate<%> frame:open-here<%>)
          get-special-menu
          get-interactions-text
          get-definitions-text
          get-interactions-canvas
          get-definitions-canvas
          get-button-panel
          execute-callback
          get-current-tab
          open-in-new-tab
          on-tab-change
          enable-evaluation
          disable-evaluation
          get-definitions/interactions-panel-parent
          register-capability-menu-item
          
          ensure-rep-shown
          ensure-rep-hidden
          ensure-defs-shown))
      
      (define frame-mixin
        (mixin (drscheme:frame:<%> frame:searchable-text<%> frame:delegate<%> frame:open-here<%>)
          (-frame<%>)
          (init filename)
          (inherit set-label-prefix get-show-menu
                   get-menu%
                   get-area-container
                   update-info
                   get-file-menu
                   file-menu:get-close-item
                   file-menu:get-save-item
                   file-menu:get-save-as-item
                   file-menu:get-revert-item
                   file-menu:get-print-item)

          ;; logging : (union #f string[directory-name])
          (field [logging #f]
                 [definitions-log-counter 0]  ;; number
                 [interactions-log-counter 0] ;; number
                 [logging-parent-panel #f]    ;; panel (unitialized short time only)
                 [logging-panel #f]           ;; panel (unitialized short time only)
                 [logging-menu-item #f])      ;; menu-item (unitialized short time only)
          ;; log-definitions : -> void
          (define/private (log-definitions)
            (when logging
              (set! definitions-log-counter (+ definitions-log-counter 1))
              (send definitions-text save-file 
                    (build-path logging (format "~a-definitions" (pad-two definitions-log-counter)))
                    'copy)))
          
          ;; log-ineractions : -> void
          (define/private (log-interactions)
            (when logging
              (set! interactions-log-counter (+ interactions-log-counter 1))
              (send interactions-text save-file 
                    (build-path logging (format "~a-interactions" (pad-two interactions-log-counter)))
                    'copy)))
          
          ;; pad-two : number -> string
          ;; pads a number to two digits?
          (define/private (pad-two n)
            (cond
              [(<= 0 n 9) (format "0~a" n)]
              [else (format "~a" n)]))
          
          ;; start-logging : -> void
          ;; turns on the logging and shows the logging gui
          (define/private (start-logging)
            (let ([log-directory (mred:get-directory
                                  (string-constant please-choose-a-log-directory)
                                  this)])
              (when (and log-directory
                         (ensure-empty log-directory))
                (send logging-menu-item set-label (string-constant stop-logging))
                (set! logging log-directory)
                (set! definitions-log-counter 0)
                (set! interactions-log-counter 0)
                (build-logging-panel)
                (log-definitions))))
          
          ;; stop-logging : -> void
          ;; turns off the logging procedure
          (define/private (stop-logging)
            (log-interactions)
            (send logging-menu-item set-label (string-constant log-definitions-and-interactions))
            (set! logging #f)
            (send logging-panel change-children (λ (l) null)))
          
          ;; build-logging-panel : -> void
          ;; builds the contents of the logging panel
          (define/private (build-logging-panel)
            (define hp (make-object horizontal-panel% logging-panel '(border)))
            (make-object message% (string-constant logging-to) hp)
            (send (make-object message% (path->string logging) hp) stretchable-width #t)
            (make-object button% (string-constant stop-logging) hp (λ (x y) (stop-logging))))
          
          ;; ensure-empty : string[directory] -> boolean
          ;; if the log-directory is empty, just return #t
          ;; if not, ask the user about emptying it. 
          ;;   if they say yes, try to empty it.
          ;;     if that fails, report the error and return #f.
          ;;     if it succeeds, return #t.
          ;;   if they say no, return #f.
          (define/private (ensure-empty log-directory)
            (let ([dir-list (directory-list log-directory)])
              (or (null? dir-list)
                  (let ([query (message-box 
                                (string-constant drscheme)
                                (format (string-constant erase-log-directory-contents) log-directory)
                                this
                                '(yes-no))])
                    (cond
                      [(eq? query 'no) 
                       #f]
                      [(eq? query 'yes)
                       (with-handlers ([exn:fail:filesystem?
                                        (λ (exn)
                                          (message-box 
                                           (string-constant drscheme)
                                           (format (string-constant error-erasing-log-directory)
                                                   (if (exn? exn)
                                                       (format "~a" (exn-message exn))
                                                       (format "~s" exn)))
                                           this)
                                          #f)])
                         (for-each (λ (file) (delete-file (build-path log-directory file)))
                                   dir-list)
                         #t)])))))

          (define/override (make-root-area-container cls parent)
            (let* ([outer-panel (super make-root-area-container module-browser-dragable-panel% parent)]
                   [saved-p (preferences:get 'drscheme:module-browser-size-percentage)]
                   [_module-browser-panel (new vertical-panel%
                                            (parent outer-panel)
                                            (alignment '(left center))
                                            (stretchable-width #f))]
                   [louter-panel (make-object vertical-panel% outer-panel)]
                   [root (make-object cls louter-panel)])
              (set! module-browser-panel _module-browser-panel)
              (set! module-browser-parent-panel outer-panel)
              (send outer-panel change-children (λ (l) (remq module-browser-panel l)))
              (preferences:set 'drscheme:module-browser-size-percentage saved-p)
              (set! logging-parent-panel (new horizontal-panel%
                                           (parent louter-panel)
                                           (stretchable-height #f)))
              (set! logging-panel (make-object horizontal-panel% logging-parent-panel))
              (unless toolbar-shown?
                (send logging-parent-panel change-children (λ (l) '())))
              root))

          (inherit show-info hide-info is-info-hidden?)
          (field [toolbar-shown? (preferences:get 'drscheme:toolbar-shown)]
                 [toolbar-menu-item #f])
          
          (define/override (on-toolbar-button-click) 
            (toggle-toolbar-visiblity))
          
          (define/private (toggle-toolbar-visiblity)
            (set! toolbar-shown? (not toolbar-shown?))
            (preferences:set 'drscheme:toolbar-shown toolbar-shown?)
            (update-toolbar-visiblity))

          (define/private (update-toolbar-visiblity)
            (cond
              [toolbar-shown?
               (show-info)
               (send top-outer-panel change-children (λ (l) (list top-panel)))
               (send logging-parent-panel change-children (λ (l) (list logging-panel)))
               (send toolbar-menu-item set-label (string-constant hide-toolbar))]
              [else
               (hide-info)
               (send top-outer-panel change-children (λ (l) '()))
               (send logging-parent-panel change-children (λ (l) '()))
               (send toolbar-menu-item set-label (string-constant show-toolbar))])
            (update-defs/ints-resize-corner))
          
          (field [remove-show-status-line-callback
                  (preferences:add-callback
                   'framework:show-status-line
                   (λ (p v)
                     (update-defs/ints-resize-corner/pref v)))])
          
          (define/private (update-defs/ints-resize-corner)
            (update-defs/ints-resize-corner/pref (preferences:get 'framework:show-status-line)))
          
          (define/private (update-defs/ints-resize-corner/pref si-pref)
            (let ([bottom-material? (and toolbar-shown? si-pref)])
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
          (define/override (get-text-to-search)
            (if search-canvas
                (send search-canvas get-editor)
                (get-editor)))
          
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
                  (unless (eq? mod? (send save-button is-shown?))
                    (send save-button show mod?))
                  (set! save-init-shown? mod?))
              (update-tab-label current-tab)))

          (define/public (language-changed)
            (let* ([settings (send definitions-text get-next-settings)]
                   [language (drscheme:language-configuration:language-settings-language settings)])
              (send func-defs-canvas language-changed language)
              (send language-message set-yellow/lang
                    (not (send definitions-text this-and-next-language-the-same?))
                    (string-append (send language get-language-name)
                                   (if (send language default-settings? 
                                             (drscheme:language-configuration:language-settings-settings settings))
                                       ""
                                       (string-append " " (string-constant custom)))))
              (let ([label (send scheme-menu get-label)]
                    [new-label (send language capability-value 'drscheme:language-menu-title)])
                (unless (equal? label new-label)
                  (send scheme-menu set-label new-label)))))
            
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
            (let ([label (get-defs-tab-label (send tab get-defs) tab)])
              (unless (equal? label (send tabs-panel get-item-label (send tab get-i)))
                (send tabs-panel set-item-label (send tab get-i) label))))
          
          (define/private (get-defs-tab-label defs tab)
            (let ([fn (send defs get-filename)])
              (add-modified-flag 
               defs
               (if fn
                   (get-tab-label-from-filename fn)
                   (send defs get-filename/untitled-name)))))
          
          (define/private (get-tab-label-from-filename fn)
            (let* ([take-n
                    (λ (n lst)
                      (let loop ([n n]
                                 [lst lst])
                        (cond
                          [(zero? n) null]
                          [(null? lst) null]
                          [else (cons (car lst) (loop (- n 1) (cdr lst)))])))]
                   [find-exp-diff
                    (λ (p1 p2)
                      (let loop ([p1 p1]
                                 [p2 p2]
                                 [i 1])
                        (cond
                          [(or (null? p1) (null? p2)) i]
                          [else (let ([f1 (car p1)]
                                      [f2 (car p2)])
                                  (if (equal? f1 f2)
                                      (loop (cdr p1) (cdr p2) (+ i 1))
                                      i))])))]
                   [exp (reverse (explode-path (normalize-path fn)))]
                   [other-exps
                    (filter
                     (λ (x) (and x 
                                 (not (equal? exp x))))
                     (map (λ (other-tab) 
                            (let ([fn (send (send other-tab get-defs) get-filename)])
                              (and fn 
                                   (reverse (explode-path (normalize-path fn))))))
                          tabs))]
                   [size
                    (let loop ([other-exps other-exps]
                               [size 1])
                      (cond
                        [(null? other-exps) size]
                        [else (let ([new-size (find-exp-diff (car other-exps) exp)])
                                (loop (cdr other-exps)
                                      (max new-size size)))]))])
              (path->string (apply build-path (reverse (take-n size exp))))))
          
          (define/private (add-modified-flag text string)
            (if (send text is-modified?)
                (let ([prefix (get-save-diamond-prefix)])
                  (if prefix
                      (string-append prefix string)
                      string))
                string))
          
          (define/private (get-save-diamond-prefix)
            (let ([candidate-prefixes (list 
                                       (case (system-type)
                                         [(windows) "• "]
                                         [else "◆ "])
                                       "* ")])
              (ormap
               (lambda (candidate)
                 (and (andmap (λ (x) (send normal-control-font screen-glyph-exists? x #t))
                              (string->list candidate))
                      candidate))
               candidate-prefixes)))
              
          [define/override get-canvas% (λ () (drscheme:get/extend:get-definitions-canvas))]
          
          (define/public (update-running running?)
            (send running-message set-label 
                  (if running?
                      (string-constant running)
                      (string-constant not-running))))
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
          
          (define/override (get-editor%) (drscheme:get/extend:get-definitions-text))
          (define/public (still-untouched?)
            (and (= (send definitions-text last-position) 0)
                 (not (send definitions-text is-modified?))
                 (not (send definitions-text get-filename))
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
	       (send definitions-text begin-edit-sequence)
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
                 (parent edit-menu)
                 (label (string-constant mode-submenu-label))
                 (demand-callback
                  (λ (menu)
                    (for-each (λ (item) (send item delete))
                              (send menu get-items))
                    (for-each (λ (mode) 
                                (let* ([item
                                        (new checkable-menu-item%
                                             (label (drscheme:modes:mode-name mode))
                                             (parent menu)
                                             (callback 
                                              (λ (_1 _2) (send definitions-text set-current-mode mode))))])
                                  (when (send definitions-text is-current-mode? mode)
                                    (send item check #t))))
                              (drscheme:modes:get-modes))))))
          
          

          
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
          
          (define/private (split)
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
                          (drscheme:get/extend:get-definitions-canvas)
                          definitions-text))
          
          (define/private (split-interactions canvas-to-be-split)
            (handle-split canvas-to-be-split
                          (λ (x) (set! interactions-canvases x))
                          interactions-canvases
                          (drscheme:get/extend:get-interactions-canvas)
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
          
          (define/private (collapse)
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
                        (if (eq? (car (get-canvases)) target)
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
                                 (if (eq? (car canvases) target)
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
		  [p (preferences:get 'drscheme:unit-window-size-percentage)])
	      
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
	      (send resizable-panel change-children (λ (l) new-children))
	      
	      (preferences:set 'drscheme:unit-window-size-percentage p)
	      
	      ;; restore preferred interactions/definitions sizes
	      (when (and (= 1 (length definitions-canvases))
			 (= 1 (length interactions-canvases))
			 (= 2 (length new-children)))
		(with-handlers ([exn:fail? (λ (x) (void))])
		  (send resizable-panel set-percentages
			(list p (- 1 p))))))
	    
	    (send resizable-panel end-container-sequence)
	    
	    (when (ormap (λ (child)
			   (and (is-a? child editor-canvas%)
				(not (send child has-focus?))))
			 (send resizable-panel get-children))
	      (let loop ([children (send resizable-panel get-children)])
		(cond
		  [(null? children) (void)]
		  [else (let ([child (car children)])
			  (if (is-a? child editor-canvas%)
			      (send child focus)
			      (loop (cdr children))))])))
	    
	    
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
	    (send file-menu:print-transcript-item enable interactions-shown?))
          
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
            (when logging
              (stop-logging))
            (remove-show-status-line-callback)
            (send interactions-text on-close))
          
          ;; execute-callback : -> void
          ;; uses the state of the button to determine if an execution is
          ;; already running. This function is called from many places, not
          ;; just the execute button.
          (define/public (execute-callback)
            (when (send execute-button is-enabled?)
              (check-if-save-file-up-to-date)
              (when (preferences:get 'drscheme:show-interactions-on-execute)
                (ensure-rep-shown interactions-text))
              (when logging
                (log-definitions)
                (log-interactions))
              (send definitions-text just-executed)
              (send language-message set-yellow #f)
              (send interactions-canvas focus)
              (send interactions-text reset-console)
              (send interactions-text clear-undos)
              (let ([start (if (and ((send definitions-text last-position) . >= . 2)
                                    (char=? (send definitions-text get-character 0) #\#)
                                    (char=? (send definitions-text get-character 1) #\!))
                               (send definitions-text paragraph-start-position 1)
                               0)])
                (send definitions-text split-snip start)
                (let ([text-port (open-input-text-editor definitions-text start)])
                  (port-count-lines! text-port)
                  (let* ([line (send definitions-text position-paragraph start)]
                         [column (- start (send definitions-text paragraph-start-position line))]
                         [relocated-port (relocate-input-port text-port 
                                                              (+ line 1)
                                                              column
                                                              (+ start 1))])
                    (port-count-lines! relocated-port)
                    (send interactions-text evaluate-from-port
                          relocated-port
                          #t
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
                      1)])
                (case user-choice
                  [(1) (void)]
                  [(2) (revert)]))))
          
          (inherit get-menu-bar get-focus-object get-edit-target-object)
          
          (define/override on-size
            (lambda (w h)
              (preferences:set 'drscheme:unit-window-width w)
              (preferences:set 'drscheme:unit-window-height h)
              (super on-size w h)))
          
          (define/override (get-editor) definitions-text)
          (define/override (get-canvas)
	    (initialize-definitions-canvas)
	    definitions-canvas)
          (define/private (initialize-definitions-canvas)
            (unless definitions-canvas
              (set! definitions-canvas
                    (new (drscheme:get/extend:get-definitions-canvas)
                         (parent resizable-panel)
                         (editor definitions-text)))))
          
          (define/override (get-delegated-text) definitions-text)
          (define/override (get-open-here-editor) definitions-text)
          
          ;; wire the definitions text to the interactions text and initialize it.
          (define/private (init-definitions-text tab)
            (let ([defs (send tab get-defs)]
                  [ints (send tab get-ints)])
              (send defs set-interactions-text ints)
              (send defs set-tab tab)
              (send ints set-definitions-text defs)
              (send defs change-mode-to-match)))


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
          (define/private create-new-tab
            (opt-lambda ([filename #f])
              (let* ([defs (new (drscheme:get/extend:get-definitions-text))]
                     [tab-count (length tabs)]
                     [new-tab (new (drscheme:get/extend:get-tab)
                                   (defs defs)
                                   (i tab-count)
                                   (frame this)
                                   (defs-shown? #t)
                                   (ints-shown? (not filename)))]
                     [ints (make-object (drscheme:get/extend:get-interactions-text) new-tab)])
                (send new-tab set-ints ints)
                (set! tabs (append tabs (list new-tab)))
                (send tabs-panel append (if filename
                                            (get-tab-label-from-filename filename)
                                            (get-defs-tab-label defs #f)))
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
          (define/private (change-to-tab tab)
            (let ([old-delegate (send definitions-text get-delegate)]
                  [old-tab current-tab])
              (save-visible-tab-regions)
              (set! current-tab tab)
              (set! definitions-text (send current-tab get-defs))
              (set! interactions-text (send current-tab get-ints))
              
              
              (begin-container-sequence)
              (for-each (λ (defs-canvas) (send defs-canvas set-editor definitions-text))
                        definitions-canvases)
              (for-each (λ (ints-canvas) (send ints-canvas set-editor interactions-text))
                        interactions-canvases)

              (restore-visible-tab-regions)
              (update-save-message)
              (update-save-button)
              (language-changed)
              
              (send definitions-text update-frame-filename)
              (send definitions-text set-delegate old-delegate)
              (on-tab-change old-tab current-tab)
              (end-container-sequence)))
          
          (define/pubment (on-tab-change from-tab to-tab)
            (let ([old-enabled (send from-tab get-enabled)]
                  [new-enabled (send to-tab get-enabled)])
              (unless (eq? old-enabled new-enabled)
                (if new-enabled
                    (enable-evaluation)
                    (disable-evaluation))))
            
            (let ([from-defs (send from-tab get-defs)]
                  [to-defs (send to-tab get-defs)])
              (let ([delegate (send from-defs get-delegate)])
                (send from-defs set-delegate #f)
                (send to-defs set-delegate delegate)))
            
            (inner (void) on-tab-change from-tab to-tab))
          
          (define/public (next-tab) (change-to-delta-tab +1))
          (define/public (prev-tab) (change-to-delta-tab -1))
          
          (define/private (change-to-delta-tab dt)
            (change-to-nth-tab (modulo (+ (send current-tab get-i) dt) (length tabs))))
          
          (define/private (close-current-tab)
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
                            (change-to-tab (cond
                                             [(< (send tab get-i) (length tabs))
                                              (list-ref tabs (send tab get-i))]
                                             [else (car (last-pair tabs))])))
                          (loop (cdr l-tabs))))]))]))
          
          (define/private (close-tab tab)
            (cond
              [(send tab can-close?)
               (send tab on-close)
               #t]
              [else #f]))
          
          (define/public (open-in-new-tab filename)
            (create-new-tab filename))
          
          (define/private (change-to-nth-tab n)
            (unless (< n (length tabs))
              (error 'change-to-nth-tab "number too big ~s" n))
            (change-to-tab (list-ref tabs n)))
          
          (define/private (save-visible-tab-regions)
            (define (get-visible-regions txt)
              (map (λ (canvas) 
                     (let-values ([(x y w h _) (get-visible-region canvas)])
                       (list x y w h)))
                   (send txt get-canvases)))
            
            (send current-tab set-visible-ints
                  (get-visible-regions interactions-text)
                  interactions-shown?)
            (send current-tab set-visible-defs 
                  (get-visible-regions definitions-text)
                  definitions-shown?)
            (send current-tab set-focus-d/i
                  (if (ormap (λ (x) (send x has-focus?)) interactions-canvases)
                      'ints
                      'defs)))
          
          (define/private (restore-visible-tab-regions)
            (define (set-visible-regions txt regions ints?)
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
                               (cdr canvases))))]))
                (for-each (λ (c r) (set-visible-region txt c r)) 
                          (send txt get-canvases)
                          regions)))
            (define (set-visible-region txt canvas region)
              (let ([admin (send txt get-admin)])
                (send admin scroll-to 
                      (first region)
                      (second region)
                      (third region)
                      (fourth region))))
            (let-values ([(vi is?) (send current-tab get-visible-ints)]
                         [(vd ds?) (send current-tab get-visible-defs)])
              (set! interactions-shown? is?)
              (set! definitions-shown? ds?)
              (update-shown)
              (set-visible-regions definitions-text vd #f)
              (set-visible-regions interactions-text vi #t))
            (case (send current-tab get-focus-d/i)
              [(defs) (send (car definitions-canvases) focus)]
              [(ints) (send (car interactions-canvases) focus)]))

          (define/private (pathname-equal? p1 p2)
            (with-handlers ([exn:fail:filesystem? (λ (x) #f)])
              (string=? (path->string (normalize-path p1))
                        (path->string (normalize-path p2)))))
          (define/override (make-visible filename)
            (let loop ([tabs tabs])
              (unless (null? tabs)
                (let* ([tab (car tabs)]
                       [tab-filename (send (send tab get-defs) get-filename)])
                  (if (and tab-filename
                           (pathname-equal? filename tab-filename))
                      (change-to-tab tab)
                      (loop (cdr tabs)))))))
          
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
                
          (define/private (update-menu-bindings)
            (when (preferences:get 'framework:menu-bindings)
              (when close-tab-menu-item
                (update-close-tab-menu-item-shortcut close-tab-menu-item))
              (update-close-menu-item-shortcut (file-menu:get-close-item))))
          
          (define/private (update-close-tab-menu-item-shortcut item)
            (let ([just-one? (and (pair? tabs) (null? (cdr tabs)))])
              (send item set-shortcut (if just-one? #f #\w))))
          
          (define/private (update-close-menu-item-shortcut item)
            (let ([just-one? (and (pair? tabs) (null? (cdr tabs)))])
              (send item set-shortcut (if just-one? #\w #f))))

          
          ;;
          ;; end tabs
          ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
          (define/public (get-definitions-text) definitions-text)
          (define/public (get-interactions-text) interactions-text)
          
          (define/public (get-definitions/interactions-panel-parent)
            (get-area-container))
          
          (inherit delegated-text-shown? hide-delegated-text show-delegated-text)
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
            (set! interactions-item
                  (make-object menu:can-restore-menu-item%
                    (string-constant show-interactions-menu-item-label)
                    (get-show-menu)
                    (λ (_1 _2) 
                      (toggle-show/hide-interactions)
                      (update-shown))
                    #\e
                    (string-constant interactions-menu-item-help-string)))
            
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
                       (show-delegated-text))))))
            
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
            
            (set! toolbar-menu-item
                  (new menu-item%
                       (label (string-constant show-toolbar))
                       (parent show-menu)
                       (callback
                        (λ (x y)
                          (toggle-toolbar-visiblity))))))

          
;                                                                                                       
;                                                                                                       
;                                                                                                       
;                           ;           ;              ;                                                
;                           ;           ;              ;                                                
;                           ;           ;              ;                                                
;   ; ;;  ;;     ;;;     ;; ;   ;   ;   ;    ;;;       ; ;;    ; ;   ;;;   ;   ;   ;   ;;;    ;;;   ; ; 
;   ;;  ;;  ;   ;   ;   ;  ;;   ;   ;   ;   ;   ;      ;;  ;   ;;   ;   ;  ;   ;   ;  ;      ;   ;  ;;  
;   ;   ;   ;  ;     ; ;    ;   ;   ;   ;  ;    ;      ;    ;  ;   ;     ;  ; ; ; ;   ;;    ;    ;  ;   
;   ;   ;   ;  ;     ; ;    ;   ;   ;   ;  ;;;;;;      ;    ;  ;   ;     ;  ; ; ; ;    ;;   ;;;;;;  ;   
;   ;   ;   ;  ;     ; ;    ;   ;   ;   ;  ;           ;    ;  ;   ;     ;  ; ; ; ;      ;  ;       ;   
;   ;   ;   ;   ;   ;   ;  ;;   ;  ;;   ;   ;          ;;  ;   ;    ;   ;    ;   ;       ;   ;      ;   
;   ;   ;   ;    ;;;     ;; ;    ;; ;   ;    ;;;;      ; ;;    ;     ;;;     ;   ;    ;;;     ;;;;  ;   
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
              (close-status-line 'plt:module-browser:mouse-over)
              (send module-browser-parent-panel change-children
                    (λ (l)
                      (remq module-browser-panel l)))))
          
          (define/private (can-browse-language?)
            (let* ([lang/config (preferences:get (drscheme:language-configuration:get-settings-preferences-symbol))]
                   [lang (drscheme:language-configuration:language-settings-language lang/config)]
                   [strs (send lang get-language-position)]
                   [can-browse?
                    (or (regexp-match #rx"module" (car (last-pair strs)))
                        (ormap (λ (x) (regexp-match #rx"PLT" x))
                               strs))])
              (unless can-browse?
                (message-box (string-constant drscheme)
                             (string-constant module-browser-only-in-plt-and-module-langs)))
              can-browse?))
          
          (define/private (update-module-browser-pane)
            (open-status-line 'plt:module-browser:mouse-over)
            (send module-browser-panel begin-container-sequence)
            (unless module-browser-ec 
              (set! module-browser-pb 
                    (drscheme:module-overview:make-module-overview-pasteboard
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
                        (preferences:set 'drscheme:module-browser:hide-paths (send module-browser-pb get-hidden-paths)))]
                     [mk-checkbox
                      (λ (key label)
                        (new check-box%
                             (parent module-browser-panel)
                             (label label)
                             (value (not (memq key (preferences:get 'drscheme:module-browser:hide-paths))))
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
                                        (string-constant module-browser-name-long)))
                         (selection (preferences:get 'drscheme:module-browser:name-length))
                         (callback
                          (λ (x y)
                            (let ([selection (send module-browser-name-length-choice get-selection)])
                              (preferences:set 'drscheme:module-browser:name-length selection)
                              (update-module-browser-name-length selection))))))
              (update-module-browser-name-length 
               (preferences:get 'drscheme:module-browser:name-length))
              
              (set! module-browser-button 
                    (new button%
                      (parent module-browser-panel)
                      (label refresh)
                      (callback (λ (x y) (update-module-browser-pane)))
                      (stretchable-width #t))))
            
            (let ([p (preferences:get 'drscheme:module-browser-size-percentage)])
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
                    [(2) 'long])))
            
          (define/private (mouse-currently-over snips)
            (if (null? snips)
                (update-status-line 'plt:module-browser:mouse-over #f)
                (let* ([snip (car snips)]
                       [lines (send snip get-lines)]
                       [name (or (send snip get-filename)
                                 (send snip get-word))]
                       [str (if lines
                                (format (string-constant module-browser-filename-format) name lines)
                                name)])
                  (update-status-line 'plt:module-browser:mouse-over str))))
            
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
                (drscheme:module-overview:fill-pasteboard 
                 module-browser-pb
                 (drscheme:language:make-text/pos
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
          
          ;; set-directory : text -> void
          ;; sets the current-directory and current-load-relative-directory
          ;; based on the file saved in the definitions-text
          (define/private (set-directory definitions-text)
            (let* ([tmp-b (box #f)]
                   [fn (send definitions-text get-filename tmp-b)])
              (unless (unbox tmp-b)
                (when fn
                  (let-values ([(base name dir?) (split-path fn)])
                    (current-directory base)
                    (current-load-relative-directory base))))))
          

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
          (define file-menu:print-transcript-item #f)
          (define file-menu:create-new-tab-item #f)
          
          (define/override (file-menu:between-new-and-open file-menu)
            (set! file-menu:create-new-tab-item
                  (new menu:can-restore-menu-item%
                       (label (string-constant new-tab))
                       (shortcut #\=)
                       (parent file-menu)
                       (callback
                        (λ (x y)
                          (create-new-tab))))))
          [define/override file-menu:between-open-and-revert
            (lambda (file-menu)
              (super file-menu:between-open-and-revert file-menu)
              (make-object separator-menu-item% file-menu))]
          (define close-tab-menu-item #f)
          (define/override (file-menu:between-close-and-quit file-menu)
            (set! close-tab-menu-item
                  (new (get-menu-item%)
                       (label (string-constant close-tab))
                       (demand-callback
                        (λ (item)
                          (send item enable (1 . < . (send tabs-panel get-number)))))
                       (parent file-menu)
                       (callback
                        (λ (x y)
                          (close-current-tab)))))
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
              (set! logging-menu-item
                    (make-object menu:can-restore-menu-item%
                      (string-constant log-definitions-and-interactions)
                      file-menu
                      (λ (x y)
                        (if logging
                            (stop-logging)
                            (start-logging)))))
              (make-object separator-menu-item% file-menu)
              (super file-menu:between-save-as-and-print file-menu)))
          
          [define/override file-menu:print-string (λ () (string-constant print-definitions))]
          (define/override (file-menu:between-print-and-close file-menu)
            (set! file-menu:print-transcript-item
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
            (add-modes-submenu edit-menu))
          
          ;; capability-menu-items : hash-table[menu -o> (listof (list menu-item number key)))
          (define capability-menu-items (make-hash-table))
          (define/public (register-capability-menu-item key menu)
            (let ([items (send menu get-items)])
              (when (null? items)
                (error 'register-capability-menu-item "menu ~e has no items" menu))
              (let* ([menu-item (car (last-pair items))]
                     [this-one (list menu-item (- (length items) 1) key)]
                     [old-ones (hash-table-get capability-menu-items menu (λ () '()))])
                (hash-table-put! capability-menu-items menu (cons this-one old-ones)))))
          
          (define/private (update-items/capability menu)
            (let ([new-items (get-items/capability menu)])
              (for-each (λ (i) (send i delete)) (send menu get-items))
              (for-each (λ (i) (send i restore)) new-items)))
          (define/private (get-items/capability menu)
            (let loop ([capability-items 
                        (reverse
                         (hash-table-get capability-menu-items menu (λ () '())))]
                       [all-items (send menu get-items)]
                       [i 0])
              (cond
                [(null? capability-items) all-items]
                [else
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
                             [(eq? (car all-items) cap-item)
                              (cons cap-item (loop (cdr capability-items) (cdr all-items) (+ i 1)))]
                             [else
                              (cons cap-item (loop (cdr capability-items) all-items (+ i 1)))])]
                          [else
                           (cond
                             [(null? all-items)
                              (loop (cdr capability-items) null (+ i 1))]
                             [(eq? (car all-items) cap-item)
                              (loop (cdr capability-items) (cdr all-items) (+ i 1))]
                             [else
                              (loop (cdr capability-items) all-items (+ i 1))])]))]
                     [else (cons (car all-items)
                                 (loop capability-items
                                       (cdr all-items)
                                       (+ i 1)))]))])))
          
          (define/private (get-current-capability-value key)
            (let* ([language-settings (send (get-definitions-text) get-next-settings)]
                   [new-language (drscheme:language-configuration:language-settings-language language-settings)])
              (send new-language capability-value key)))
          
          (define language-menu 'uninited-language-menu)
          (define scheme-menu 'scheme-menu-not-yet-init)
          (define special-menu 'special-menu-not-yet-init)
          (define/public (get-special-menu) special-menu)
          
          (define/public (choose-language-callback)
            (let ([new-settings (drscheme:language-configuration:language-dialog
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
                 (let* ([language (drscheme:language-configuration:language-settings-language
                                   (send (get-definitions-text) get-next-settings))]
                        [settings (drscheme:language-configuration:language-settings-settings
                                   (send (get-definitions-text) get-next-settings))]
                        [tp-names ((teachpack-callbacks-get-names tp-callbacks) settings)]
                        [update-settings
                         (λ (settings)
                           (send (get-definitions-text) set-next-settings 
                                 (drscheme:language-configuration:make-language-settings language settings))
                           (send (get-definitions-text) teachpack-changed))])
                   (set! teachpack-items
                         (list*
                          (make-object separator-menu-item% language-menu)
                          (new menu:can-restore-menu-item%
                               [label (string-constant add-teachpack-menu-item-label)]
                               [parent language-menu]
                               [callback
                                (λ (_1 _2)
                                  (update-settings ((teachpack-callbacks-add tp-callbacks) settings this)))])
                          (let ([mi (new menu:can-restore-menu-item% 
                                         [label (string-constant clear-all-teachpacks-menu-item-label)]
                                         [parent language-menu]
                                         [callback
                                          (λ (_1 _2) 
                                            (update-settings ((teachpack-callbacks-remove-all tp-callbacks) settings)))])])
                            
                            (send mi enable (not (null? tp-names)))
                            mi)
                          (map (λ (name)
                                 (new menu:can-restore-menu-item%
                                      [label (format (string-constant clear-teachpack) name)]
                                      [parent language-menu]
                                      [callback
                                       (λ (item evt)
                                         (update-settings ((teachpack-callbacks-remove tp-callbacks) settings name)))]))
                               tp-names))))]
                [else 
                 (set! teachpack-items '())])))
          
          (define/private (initialize-menus)
            (let* ([mb (get-menu-bar)]
                   [language-menu-on-demand (λ (menu-item) (update-teachpack-menu))]
                   [_ (set! language-menu (make-object (get-menu%) 
                                            (string-constant language-menu-name)
                                            mb
                                            #f
                                            language-menu-on-demand))]
                   [_ (set! scheme-menu (new (get-menu%) 
                                             [label (drscheme:language:get-capability-default
                                                     'drscheme:language-menu-title)]
                                             [parent mb]))]
                   [send-method
                    (λ (method)
                      (λ (_1 _2)
                        (let ([text (get-focus-object)])
                          (when (is-a? text scheme:text<%>)
                            (method text)))))]
                   [show/hide-capability-menus
                    (λ ()
                      (for-each (λ (menu) (update-items/capability menu)) (send (get-menu-bar) get-items)))])
              
              (make-object menu:can-restore-menu-item%
                (string-constant choose-language-menu-item-label)
                language-menu
                (λ (_1 _2) (choose-language-callback))
                #\l)
              
              (set! execute-menu-item
                    (make-object menu:can-restore-menu-item%
                      (string-constant execute-menu-item-label)
                      scheme-menu
                      (λ (_1 _2) (execute-callback))
                      #\t
                      (string-constant execute-menu-item-help-string)))
              (make-object menu:can-restore-menu-item%
                (string-constant break-menu-item-label)
                scheme-menu
                (λ (_1 _2) (send current-tab break-callback))
                #\b
                (string-constant break-menu-item-help-string))
              (make-object menu:can-restore-menu-item%
                (string-constant kill-menu-item-label)
                scheme-menu
                (λ (_1 _2) (send interactions-text kill-evaluation))
                #\k
                (string-constant kill-menu-item-help-string))
              (when (custodian-memory-accounting-available?)
                (new menu-item%
                     [label (string-constant limit-memory-menu-item-label)]
                     [parent scheme-menu]
                     [callback
                      (λ (item b)
                        (let ([num (get-mbytes this 
                                               (let ([limit (send interactions-text get-custodian-limit)])
                                                 (and limit
                                                      (floor (/ limit 1024 1024)))))])
                          (when num
                            (cond
                              [(eq? num #t)
                               (preferences:set 'drscheme:limit-memory #f)
                               (send interactions-text set-custodian-limit #f)]
                              [else
                               (preferences:set 'drscheme:limit-memory 
                                                (* 1024 1024 num))
                               (send interactions-text set-custodian-limit
                                     (* 1024 1024 num))]))))]))
              (new menu:can-restore-menu-item%
                   (label (string-constant clear-error-highlight-menu-item-label))
                   (parent scheme-menu)
                   (callback
                    (λ (_1 _2) 
                      (let ([ints (send (get-current-tab) get-ints)])
                        (send ints reset-error-ranges))))
                   (help-string (string-constant clear-error-highlight-item-help-string))
                   (demand-callback
                    (λ (item)
                      (let ([ints (send (get-current-tab) get-ints)])
                        (send item enable (send ints get-error-ranges))))))
              (make-object separator-menu-item% scheme-menu)
              (make-object menu:can-restore-menu-item%
                (string-constant create-executable-menu-item-label)
                scheme-menu
                (λ (x y) (create-executable this)))
              (make-object menu:can-restore-menu-item%
                (string-constant module-browser...)
                scheme-menu
                (λ (x y) (drscheme:module-overview:module-overview this)))
              (make-object separator-menu-item% scheme-menu)
              (make-object menu:can-restore-menu-item%
                (string-constant reindent-menu-item-label)
                scheme-menu
                (send-method (λ (x) (send x tabify-selection))))
              (make-object menu:can-restore-menu-item%
                (string-constant reindent-all-menu-item-label)
                scheme-menu
                (send-method (λ (x) (send x tabify-all)))
                #\i)
              (make-object menu:can-restore-menu-item%
                (string-constant box-comment-out-menu-item-label)
                scheme-menu
                (send-method (λ (x) (send x box-comment-out-selection))))
              (make-object menu:can-restore-menu-item%
                (string-constant semicolon-comment-out-menu-item-label)
                scheme-menu
                (send-method (λ (x) (send x comment-out-selection))))
              (make-object menu:can-restore-menu-item%
                (string-constant uncomment-menu-item-label)
                scheme-menu
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
                                      (when (is-a? ed scheme:text<%>)
                                        (send ed uncomment-box/selection)))))]
                               [else (send text uncomment-selection)]))]
                          [else (send text uncomment-selection)]))))))
            
              (set! special-menu
                    (new (get-menu%)
                         [label (string-constant special-menu)]
                         [parent mb]
                         [demand-callback
                          (λ (special-menu)
                            ;; just here for convience -- it actually works on all menus, not just the special menu
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
                           (let ([str (get-text-from-user (string-constant large-semicolon-letters)
                                                          (string-constant text-to-insert)
                                                          this)])
                             (when (and str
                                        (not (equal? str "")))
                               (let ()
                                 (define language-settings (send definitions-text get-next-settings))
                                 (define-values (comment-prefix comment-character)
                                   (if language-settings
                                       (send (drscheme:language-configuration:language-settings-language
                                              language-settings)
                                             get-comment-character)
                                       (values ";" #\;)))
                                 (define bdc (make-object bitmap-dc% (make-object bitmap% 1 1 #t)))
                                 (define the-font (send (send (editor:get-standard-style-list)
                                                              find-named-style
                                                              "Standard")
                                                        get-font))
                                 (define-values (tw th td ta) (send bdc get-text-extent str the-font))
                                 (define tmp-color (make-object color%))
                                 
                                 (define (get-char x y)
                                   (send bdc get-pixel x y tmp-color)
                                   (let ([red (send tmp-color red)])
                                     (if (= red 0)
                                         comment-character
                                         #\space)))
                                 (define bitmap
                                   (make-object bitmap% 
                                     (inexact->exact tw)
                                     (inexact->exact th) 
                                     #t))
                                 
                                 (define (fetch-line y)
                                   (let loop ([x (send bitmap get-width)]
                                              [chars null])
                                     (cond
                                       [(zero? x) (apply string chars)]
                                       [else (loop (- x 1) (cons (get-char (- x 1) y) chars))])))
                                 
                                 (send bdc set-bitmap bitmap)
                                 (send bdc clear)
                                 (send bdc set-font the-font)
                                 (send bdc draw-text str 0 0)
                                 
                                 (send edit begin-edit-sequence)
                                 (let ([start (send edit get-start-position)]
                                       [end (send edit get-end-position)])
                                   (send edit delete start end)
                                   (send edit insert "\n" start start)
                                   (let loop ([y (send bitmap get-height)])
                                     (unless (zero? y)
                                       (send edit insert (fetch-line (- y 1)) start start)
                                       (send edit insert comment-prefix start start)
                                       (send edit insert "\n" start start)
                                       (loop (- y 1)))))
                                 (send edit end-edit-sequence)))))))]
                    [c% (get-menu-item%)])
                
                (frame:add-snip-menu-items 
                 special-menu 
                 c%
                 (λ (item)
                   (let ([label (send item get-label)])
                     (cond
                       [(equal? label (string-constant insert-comment-box-menu-item-label))
                        (register-capability-menu-item 'drscheme:special:insert-comment-box special-menu)]
                       [(equal? label (string-constant insert-image-item))
                        (register-capability-menu-item 'drscheme:special:insert-image special-menu)]))))
                                           
                (make-object c% (string-constant insert-fraction-menu-item-label)
                  special-menu callback 
                  #f #f
                  has-editor-on-demand)
                (register-capability-menu-item 'drscheme:special:insert-fraction special-menu)
                
                (make-object c% (string-constant insert-large-letters...)
                  special-menu
                  (λ (x y) (insert-large-semicolon-letters))
                  #f #f
                  has-editor-on-demand)
                (register-capability-menu-item 'drscheme:special:insert-large-letters special-menu)
                
                (make-object c% (string-constant insert-lambda)
                  special-menu
                  (λ (x y) (insert-lambda))
                  #\\
                  #f
                  has-editor-on-demand)
                (register-capability-menu-item 'drscheme:special:insert-lambda special-menu))

              (make-object separator-menu-item% (get-show-menu))
              
              (new menu:can-restore-menu-item%
                   (shortcut (if (eq? (system-type) 'macosx) #f #\m))
                   (label (string-constant split-menu-item-label))
                   (parent (get-show-menu))
                   (callback (λ (x y) (split)))
                   (demand-callback (λ (item) (split-demand item))))
              (new menu:can-restore-menu-item% 
                   (shortcut #\r)
                   (label (string-constant collapse-menu-item-label))
                   (parent (get-show-menu))
                   (callback (λ (x y) (collapse)))
                   (demand-callback (λ (item) (collapse-demand item))))
              
              (frame:reorder-menus this)))

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

          (define definitions-text (new (drscheme:get/extend:get-definitions-text)))
                    
          ;; tabs : (listof tab)
          (define tabs (list (new (drscheme:get/extend:get-tab)
                                  (defs definitions-text)
                                  (frame this)
                                  (i 0)
                                  (defs-shown? #t)
                                  (ints-shown? #t))))
          (define/public-final (get-tabs) tabs)
          
          ;; current-tab : tab
          ;; corresponds to the tabs-panel's active button.
          (define current-tab (car tabs))
          
          (define interactions-text (new (drscheme:get/extend:get-interactions-text) 
                                         (context (car tabs))))
          (send (car tabs) set-ints interactions-text)
          
          (init-definitions-text (car tabs))
          
          (super-new
            (filename filename)
            (style '(toolbar-button))
            (width (preferences:get 'drscheme:unit-window-width))
            (height (preferences:get 'drscheme:unit-window-height)))
          
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

          
          ;; most contain only top-panel (or nothing)
          (define top-outer-panel (new horizontal-pane% 
                                       (parent (get-area-container))
                                       (stretchable-height #f)))
          
          [define top-panel (make-object horizontal-panel% top-outer-panel)]
          [define name-panel (new vertical-pane%
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
          [define resizable-panel (new vertical-dragable/def-int%
                                    (unit-frame this)
                                    (parent panel-with-tabs))]
          
          [define definitions-canvas #f]
          (initialize-definitions-canvas)
          [define definitions-canvases (list definitions-canvas)]
          [define interactions-canvas (new (drscheme:get/extend:get-interactions-canvas)
                                           (parent resizable-panel)
                                           (editor interactions-text))]
          [define interactions-canvases (list interactions-canvas)]
          

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
          
          (public get-definitions-canvas get-interactions-canvas)
          [define get-definitions-canvas (λ () definitions-canvas)]
          [define get-interactions-canvas (λ () interactions-canvas)]
          
          (set! save-button
                (make-object button% 
                  (make-save-bitmap this)
                  top-panel
                  (λ args
                    (when definitions-text
                      (save)
                      (send definitions-canvas focus)))))
          
          (set! name-message (new drs-name-message% [parent name-panel]))
          [define teachpack-items null]
          [define break-button (void)]
          [define execute-button (void)]
          [define button-panel (make-object horizontal-panel% top-panel)]
          [define/public get-execute-button (λ () execute-button)]
          [define/public get-break-button (λ () break-button)]
          [define/public get-button-panel (λ () button-panel)]
          
          (inherit get-info-panel)
          (define running-message
            (make-object message% (string-constant not-running) (get-info-panel)))

          
          [define func-defs-canvas (new func-defs-canvas% 
                                        (parent name-panel)
                                        (frame this))]
          
          (set! execute-button
                (make-object button%
                  (make-execute-bitmap this)
                  button-panel
                  (λ (button evt) (execute-callback))))
          (set! break-button
                (make-object button%
                  (make-break-bitmap this) 
                  button-panel
                  (λ (x y)
		    (send current-tab break-callback))))
          
          (send button-panel stretchable-height #f)
          (send button-panel stretchable-width #f) 
          
          (send top-panel change-children
                (λ (l)
                  (list name-panel save-button
                        (make-object vertical-panel% top-panel) ;; spacer
                        button-panel)))
          
          (send top-panel stretchable-height #f)
          (inherit get-label)
          (let ([m (send definitions-canvas get-editor)])
            (set-save-init-shown?
             (and m (send m is-modified?))))

          (define language-message
            (let* ([info-panel (get-info-panel)]
                   [vp (new vertical-panel% 
                            [parent info-panel]
                            [alignment '(left center)]
                            [stretchable-width #f]
                            [stretchable-height #f])]
                   [spacer (new horizontal-panel% [parent info-panel])]
                   [l-m-label (new language-label-message% [parent vp] [frame this])]
                   [language-message (new language-message% [parent vp])])
              (send info-panel change-children 
                    (λ (l)
                      (list* vp
                             spacer 
                             (remq* (list spacer vp) l))))
              language-message))
          
	  (update-save-message)
          (update-save-button)
          (language-changed)
          
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
                  (let ([p (preferences:get 'drscheme:unit-window-size-percentage)])
                    (list p (- 1 p)))))
          
          (set-label-prefix (string-constant drscheme))
          (update-toolbar-visiblity)
          (set! newest-frame this)
          (send definitions-canvas focus)))
    
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
      
      (define outer-hp (new horizontal-panel% [parent d] [alignment '(center bottom)]))
      (define rb (new radio-box%
                      [label #f]
                      [choices (list (string-constant limit-memory-unlimited) (string-constant limit-memory-limited))]
                      [callback (λ (a b) (grayizie))]
                      [parent outer-hp]))
      
      (define (grayizie)
        (case (send rb get-selection)
          [(0) 
           (send tb enable #f)
           (send msg2 enable #f)
           (background gray-foreground-sd)]
          [(1)
           (send tb enable #t)
           (send msg2 enable #t)
           (background black-foreground-sd)
           (let ([e (send tb get-editor)])
             (send e set-position 0 (send e last-position)))
           (send tb focus)])
        (update-ok-button-state))
      
      (define hp (new horizontal-panel% 
                      [parent outer-hp] 
                      [stretchable-height #f]
                      [stretchable-width #f]))
      
      (define tb
        (new text-field%
             [label #f]
             [parent hp]
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
                (update-ok-button-state))]))
      
      (define (update-ok-button-state)
        (case (send rb get-selection)
          [(0) (send ok-button enable #t)]
          [(1) (send ok-button enable (is-valid-number? (send tb get-editor)))]))
      
      (define msg2 (new message% [parent hp] [label (string-constant limit-memory-megabytes)]))
      (define bp (new horizontal-panel% [parent d]))
      (define-values (ok-button cancel-button)
        (gui-utils:ok/cancel-buttons
         bp 
         (λ (a b) 
           (case (send rb get-selection)
             [(0) (set! result #t)]
             [(1) (set! result (string->number (send (send tb get-editor) get-text)))])
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
               (integer? n)
               (100 . <= . n))))
      
      (define (background sd)
        (let ([txt (send tb get-editor)])
          (send txt change-style sd 0 (send txt last-position))))
      
      (send clear-sd set-delta-background "white")
      (send yellow-sd set-delta-background "yellow")
      (send black-foreground-sd set-delta-foreground "black")
      (send gray-foreground-sd set-delta-foreground "gray")
      (send d set-alignment 'left 'center)
      (send bp set-alignment 'right 'center)
      (when current-limit
        (send rb set-selection 1))
      (update-ok-button-state)
      (grayizie)
      (send tb focus)
      (let ([e (send tb get-editor)])
        (send e set-position 0 (send e last-position)))
      (send d show #t)
      result)


    
    (define (limit-length l n)
      (let loop ([l l]
                 [n n])
        (cond
          [(or (null? l) (zero? n))  null]
          [else (cons (car l) (loop (cdr l) (- n 1)))])))
    (define (remove-duplicates l)
      (reverse
       (let loop ([l (reverse l)])
         (cond
           [(null? l) l]
           [else
            (if (member (car l) (cdr l))
                (loop (cdr l))
                (cons (car l) (loop (cdr l))))]))))
    
    (define programming-language-label (string-constant programming-language-label))
    (define second-line-indent 6)
    (define language-message%
      (class canvas%
        (inherit get-dc get-client-size refresh)
        (define message "")
        (define/public (set-lang l)
          (unless (equal? l message)
            (set! message l)
            (update-min-widths)
            (refresh)))
        
        (define yellow? #f)
        
        (define/public (set-yellow/lang y? l)
          (unless (and (equal? y? yellow?)
                       (equal? l message))
            (set! yellow? y?)
            (set! message l)
            (update-min-widths)
            (refresh)))
        
        (define/public (set-yellow y?)
          (unless (equal? y? yellow?)
            (set! yellow? y?)
            (refresh)))
        
        (define/override (on-paint)
          (let ([dc (get-dc)])
            (let-values ([(w h) (get-client-size)])
              (send dc set-pen (get-panel-background) 1 'transparent)
              (send dc set-brush (get-panel-background) 'transparent)
              (send dc draw-rectangle 0 0 w h)
              (when yellow?
                (send dc set-pen "black" 1 'transparent)
                (send dc set-brush "yellow" 'solid)
                (send dc draw-rectangle (get-left-side-padding) 0 (- w (get-left-side-padding)) h))
              (send dc set-font small-control-font)
              (send dc draw-text message (get-left-side-padding) 0))))
        
        (super-new [style '(transparent)])
        (inherit stretchable-width stretchable-height)
        (stretchable-width #f)
        (stretchable-height #f)
        
        (inherit min-width min-height)
        (define (update-min-widths)
          (let ([dc (get-dc)])
            (let-values ([(w2 h2 _3 _4) (send dc get-text-extent message small-control-font)])
              (min-width (inexact->exact (floor (+ (get-left-side-padding) w2))))
              (min-height (inexact->exact (floor h2))))))))
    
    (define language-label-message%
      (class name-message%
        (init-field frame)
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
                             (drscheme:language-configuration:get-languages))])
                 (when lang
                   ;; this test can fail when a language has been added wrongly via the tools interface
                   ;; just ignore that menu item, in that case.
                   (let ([settings (send lang unmarshall-settings marshalled-settings)])
                     (when lang
                       (set! added-one? #t)
                       (new menu-item%
                            [parent menu]
                            [label (string-append "  "
                                                  (send lang get-language-name)
                                                  (if (send lang default-settings? settings)
                                                      ""
                                                      (string-append " " (string-constant custom))))]
                            [callback
                             (λ (x y)
                               (send (send frame get-definitions-text)
                                     set-next-settings
                                     (drscheme:language-configuration:make-language-settings
                                      lang
                                      settings)))]))))))
             (preferences:get 'drscheme:recent-language-names))
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
                  
        (super-new [label programming-language-label]
                   [font tiny-control-font])))
    
    (define -frame% (frame-mixin super-frame%))

      (define module-browser-dragable-panel%
        (class panel:horizontal-dragable%
          (inherit get-percentages)
          (define/augment (after-percentage-change)
            (let ([percentages (get-percentages)])
              (when (and (pair? percentages)
                         (pair? (cdr percentages))
                         (null? (cddr percentages)))
                (preferences:set 'drscheme:module-browser-size-percentage
                                 (car percentages))))
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
        (super-new)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;; lambda-snipclass is for backwards compatibility
      ;;
      (define lambda-snipclass
        (make-object (class snip-class% ()
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
                  (not (eq? newest-frame 'nothing-yet)) 
                  (send newest-frame still-untouched?))
             (send newest-frame change-to-file name)
             (send newest-frame show #t)
             (begin0 newest-frame
                     (set! newest-frame #f))]
            [(and name ;; only open a tab if we have a filename
                  (preferences:get 'drscheme:open-in-tabs))
             (let ([fr (let loop ([frs (cons (send (group:get-the-frame-group) get-active-frame)
                                             (send (group:get-the-frame-group) get-frames))])
                         (cond
                           [(null? frs) #f]
                           [else (let ([fr (car frs)])
                                   (or (and (is-a? fr -frame<%>)
                                            fr)
                                       (loop (cdr frs))))]))])
               (if fr
                   (begin (send fr open-in-new-tab name)
                          (send fr show #t)
                          fr)
                   (create-new-drscheme-frame name)))]
            [else
             (create-new-drscheme-frame name)])]))
      
      (define (create-new-drscheme-frame filename)
        (let* ([drs-frame% (drscheme:get/extend:get-unit-frame)]
               [frame (new drs-frame% (filename filename))])
          (send (send frame get-interactions-text) initialize-console)
          (send frame show #t)
          frame))))
