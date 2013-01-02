#lang racket/base

#|

TODO
- should a GC should happen on each execution? (or perhaps better, each kill?)
- front-end methods have new signature


|#
; =Kernel= means in DrRacket's thread and parameterization
; 
; =User= means the user's thread and parameterization
; 
; =Handler= means in the handler thread of some eventspace; it must
;  be combined with either =Kernel= or =User=

(require racket/class
         racket/path
         racket/pretty
         racket/unit
         racket/list
         racket/port
         
         string-constants
         setup/xref
         racket/gui/base
         framework
         browser/external
         "drsig.rkt"
         "local-member-names.rkt"
         "stack-checkpoint.rkt"
         "parse-logger-args.rkt"
         
         ;; the dynamic-require below loads this module, 
         ;; so we make the dependency explicit here, even
         ;; tho nothing is used from this module.
         planet/terse-info)

(provide rep@ with-stack-checkpoint)

(define orig-output-port (current-output-port))
(define (oprintf . args) (apply fprintf orig-output-port args))
 
(define no-breaks-break-parameterization
  (parameterize-break #f (current-break-parameterization)))

(define-unit rep@
  (import [prefix drracket:init: drracket:init^]
          [prefix drracket:language-configuration: drracket:language-configuration/internal^]
          [prefix drracket:language: drracket:language^]
          [prefix drracket:app: drracket:app^]
          [prefix drracket:frame: drracket:frame^]
          [prefix drracket:unit: drracket:unit^]
          [prefix drracket:text: drracket:text^]
          [prefix drracket:help-desk: drracket:help-desk^]
          [prefix drracket:debug: drracket:debug^]
          [prefix drracket:eval: drracket:eval^]
          [prefix drracket:module-language: drracket:module-language/int^]
          [prefix drracket: drracket:interface^])
  (export (rename drracket:rep^
                  [-text% text%]
                  [-text<%> text<%>]))
  
  (define -text<%>
    (interface ((class->interface text%)
                text:ports<%>
                editor:file<%>
                racket:text<%>
                color:text<%>)
      reset-highlighting
      highlight-errors
      highlight-errors/exn
      
      get-user-custodian
      get-user-eventspace
      get-user-thread
      get-user-namespace
      
      get-definitions-text
      
      kill-evaluation
      
      display-results
      
      run-in-evaluation-thread
      after-many-evals
      on-execute
      
      shutdown
      
      get-error-ranges
      reset-error-ranges
      
      reset-console
      
      copy-prev-previous-expr
      copy-next-previous-expr
      copy-previous-expr
      
      
      initialize-console
      
      get-prompt
      insert-prompt
      get-context))
  
  (define sized-snip<%>
    (interface ((class->interface snip%))
      ;; get-character-width : -> number
      ;; returns the number of characters wide the snip is,
      ;; for use in pretty printing the snip.
      get-character-width))
  
  ;; current-language-settings : (parameter language-setting)
  ;; set to the current language and its setting on the user's thread.
  (define current-language-settings (make-parameter #f))
  
  ;; current-rep : (parameter (union #f (instanceof rep:text%)))
  ;; the repl that controls the evaluation in this thread.
  (define current-rep (make-parameter #f))
  
  ;; a port that accepts values for printing in the repl
  (define current-value-port (make-parameter #f))
  
  ;; drracket-error-display-handler : (string (union #f exn) -> void
  ;; =User=
  ;; the timing is a little tricky here. 
  ;; the file icon must appear before the error message in the text, so that happens first.
  ;; the highlight must be set after the error message, because inserting into the text resets
  ;;     the highlighting.
  (define (drracket-error-display-handler msg exn)
    (drracket:debug:error-display-handler/stacktrace msg exn))
  
  (define (main-user-eventspace-thread?)
    (let ([rep (current-rep)])
      (and rep
           (eq? (eventspace-handler-thread (send rep get-user-eventspace))
                (current-thread)))))
  
  (define drs-bindings-keymap (make-object keymap:aug-keymap%))

  (let* ([get-frame
          (λ (obj)
            (and (is-a? obj editor<%>)
                 (let ([canvas (send obj get-canvas)])
                   (and canvas
                        (let ([frame (send canvas get-top-level-window)])
                          (and (is-a? frame drracket:unit:frame%)
                               frame))))))]
         [add-drs-function
          (λ (name f)
            (send drs-bindings-keymap add-function name
                  (λ (obj evt) (cond [(get-frame obj) => f]))))]
         [show-tab
          (λ (i)
            (λ (obj evt)
              (let ([fr (get-frame obj)])
                (and fr
                     (is-a? fr drracket:unit:frame<%>)
                     (< i (send fr get-tab-count))
                     (begin (send fr change-to-nth-tab i)
                            #t)))))])
    (for ([i (in-range 1 10)])
      (send drs-bindings-keymap add-function (format "show-tab-~a" i) (show-tab (- i 1))))
    (send drs-bindings-keymap add-function "search-help-desk"
          (λ (obj evt)
            (if (not (and (is-a? obj text%) (get-frame obj))) ; is `get-frame' needed?
              (drracket:help-desk:help-desk)
              (let* ([start (send obj get-start-position)]
                     [end (send obj get-end-position)]
                     [str (if (= start end)
                            (drracket:unit:find-symbol obj start)
                            (send obj get-text start end))])
                (if (or (not str) (equal? "" str))
                  (drracket:help-desk:help-desk)
                  (let* ([l (send obj get-canvas)]
                         [l (and l (send l get-top-level-window))]
                         [l (and l (is-a? l drracket:unit:frame<%>) (send l get-definitions-text))]
                         [l (and l (send l get-next-settings))]
                         [l (and l (drracket:language-configuration:language-settings-language l))]
                         [ctxt (and l (send l capability-value 'drscheme:help-context-term))]
                         [name (and l (send l get-language-name))])
                    (drracket:help-desk:help-desk
                     str (and ctxt (list ctxt name)))))))))
    
    ;; keep this in case people use it in their keymaps
    (add-drs-function "execute"  (λ (frame) (send frame execute-callback)))
    
    (add-drs-function "run"  (λ (frame) (send frame execute-callback)))
    (add-drs-function "next-tab" (λ (frame) (send frame next-tab)))
    (add-drs-function "prev-tab" (λ (frame) (send frame prev-tab)))
    (add-drs-function "collapse" (λ (frame) (send frame collapse)))
    (add-drs-function "split"    (λ (frame) (send frame split)))
    
    (add-drs-function "jump-to-previous-error-loc"
                      (λ (frame) (send frame jump-to-previous-error-loc)))
    (add-drs-function "jump-to-next-error-loc"
                      (λ (frame) (send frame jump-to-next-error-loc)))
    
    (add-drs-function "send-toplevel-form-to-repl" (λ (frame) (send frame send-toplevel-form-to-repl #f)))
    (add-drs-function "send-selection-to-repl" (λ (frame) (send frame send-selection-to-repl #f)))
    (add-drs-function "send-toplevel-form-to-repl-and-go" (λ (frame) (send frame send-toplevel-form-to-repl #t)))
    (add-drs-function "send-selection-to-repl-and-go" (λ (frame) (send frame send-selection-to-repl #t)))
    (add-drs-function "move-to-interactions" (λ (frame) (send frame move-to-interactions))))
  
  (send drs-bindings-keymap map-function "m:p" "jump-to-previous-error-loc")
  (send drs-bindings-keymap map-function "m:n" "jump-to-next-error-loc")
  (send drs-bindings-keymap map-function "esc;p" "jump-to-previous-error-loc")
  (send drs-bindings-keymap map-function "esc;n" "jump-to-next-error-loc")
  (send drs-bindings-keymap map-function "c:x;`" "jump-to-next-error-loc")
  
  (send drs-bindings-keymap map-function "f5" "run")
  (send drs-bindings-keymap map-function "f1" "search-help-desk")
  (send drs-bindings-keymap map-function "c:tab" "next-tab")
  (send drs-bindings-keymap map-function "c:s:tab" "prev-tab")
  (send drs-bindings-keymap map-function "c:pagedown" "next-tab")
  (send drs-bindings-keymap map-function "c:pageup" "prev-tab")
  
  (send drs-bindings-keymap map-function "c:x;0" "collapse")
  (send drs-bindings-keymap map-function "c:x;2" "split")

  (send drs-bindings-keymap map-function "c:c;c:z" "move-to-interactions")
  
  (for ([i (in-range 1 10)])
    (send drs-bindings-keymap map-function 
          (format "a:~a" i) 
          (format "show-tab-~a" i))
    (send drs-bindings-keymap map-function 
          (format "m:~a" i) 
          (format "show-tab-~a" i)))
  
  (define (get-drs-bindings-keymap) drs-bindings-keymap)
  
  ;; drs-bindings-keymap-mixin :
  ;;   ((implements editor:keymap<%>) -> (implements editor:keymap<%>))
  ;;   for any x that is an instance of the resulting class,
  ;;     (is-a? (send (send x get-canvas) get-top-level-frame) drracket:unit:frame%)
  (define drs-bindings-keymap-mixin
    (mixin (editor:keymap<%>) (editor:keymap<%>)
      (define/override (get-keymaps)
        (editor:add-after-user-keymap drs-bindings-keymap (super get-keymaps)))
      (super-new)))
  
  ;; Max length of output queue (user's thread blocks if the
  ;; queue is full):
  (define output-limit-size 2000)
  
  (define setup-scheme-interaction-mode-keymap
    (λ (keymap)
      (send keymap add-function "put-previous-sexp"
            (λ (text event) 
              (send text copy-prev-previous-expr)))
      (send keymap add-function "put-next-sexp"
            (λ (text event) 
              (send text copy-next-previous-expr)))
      
      (keymap:send-map-function-meta keymap "p" "put-previous-sexp")
      (keymap:send-map-function-meta keymap "n" "put-next-sexp")
      (send keymap map-function "c:up" "put-previous-sexp")
      (send keymap map-function "c:down" "put-next-sexp")))
  
  (define scheme-interaction-mode-keymap (make-object keymap:aug-keymap%))
  (setup-scheme-interaction-mode-keymap scheme-interaction-mode-keymap)
  
  (define drs-font-delta (make-object style-delta% 'change-family 'decorative))
  
  (define output-delta (make-object style-delta%)) ; used to be 'change-weight 'bold
  (define result-delta (make-object style-delta%)) ; used to be 'change-weight 'bold
  (define error-delta (make-object style-delta%
                        'change-style
                        'italic))
  (send error-delta set-delta-foreground (make-object color% 255 0 0))
  (send result-delta set-delta-foreground (make-object color% 0 0 175))
  (send output-delta set-delta-foreground (make-object color% 150 0 150))
  
  (define error-text-style-delta (make-object style-delta%))
  (send error-text-style-delta set-delta-foreground (make-object color% 200 0 0))
  
  (define grey-delta (make-object style-delta%))
  (send grey-delta set-delta-foreground "GREY")
  
  (define welcome-delta (make-object style-delta% 'change-family 'decorative))
  (define click-delta (gui-utils:get-clickback-delta))
  (define red-delta (make-object style-delta%))
  (define dark-green-delta (make-object style-delta%))
  (send* red-delta
    (copy welcome-delta)
    (set-delta-foreground "RED"))  
  (send* dark-green-delta
    (copy welcome-delta)
    (set-delta-foreground "dark green"))
  (define warning-style-delta (make-object style-delta% 'change-bold))
  (send* warning-style-delta
    (set-delta-foreground "BLACK")
    (set-delta-background "YELLOW"))
  (define (get-welcome-delta) welcome-delta)
  (define (get-dark-green-delta) dark-green-delta)
  
  ;; is-default-settings? : language-settings -> boolean
  ;; determines if the settings in `language-settings'
  ;; correspond to the default settings of the language.
  (define (is-default-settings? language-settings)
    (send (drracket:language-configuration:language-settings-language language-settings)
          default-settings?
          (drracket:language-configuration:language-settings-settings language-settings)))
  
  (define (extract-language-name language-settings defs-text)
    (cond
      [(is-a? (drracket:language-configuration:language-settings-language language-settings)
              drracket:module-language:module-language<%>)
       (send (drracket:language-configuration:language-settings-language language-settings)
             get-users-language-name defs-text)]
      [else
       (send (drracket:language-configuration:language-settings-language language-settings)
             get-language-name)]))
  (define (extract-language-style-delta language-settings)
    (send (drracket:language-configuration:language-settings-language language-settings)
          get-style-delta))
  (define (extract-language-url language-settings)
    (send (drracket:language-configuration:language-settings-language language-settings)
          get-language-url))
  
  (define-struct sexp (left right prompt))
  
  (let* ([list-of? (λ (p?)
                     (λ (l)
                       (and (list? l)
                            (andmap p? l))))]
         [snip/string? (λ (s) (or (is-a? s snip%) (string? s)))]
         [list-of-snip/strings? (list-of? snip/string?)]
         [list-of-lists-of-snip/strings? (list-of? list-of-snip/strings?)])
    (preferences:set-default
     'drracket:console-previous-exprs
     null
     list-of-lists-of-snip/strings?))
  (define (marshall-previous-exprs lls)
    (map (λ (ls)
           (list
            (apply
             string-append
             (reverse
              (map (λ (s)
                     (cond
                       [(is-a? s string-snip%)
                        (send s get-text 0 (send s get-count))]
                       [(string? s) s]
                       [else "'non-string-snip"]))
                   ls)))))
         lls))
  (let ([unmarshall (λ (x) x)])
    (preferences:set-un/marshall
     'drracket:console-previous-exprs
     marshall-previous-exprs unmarshall))
  
  (define color? ((get-display-depth) . > . 8))
  
  ;; instances of this interface provide a context for a rep:text%
  ;; its connection to its graphical environment (ie frame) for
  ;; error display and status infromation is all mediated
  ;; through an instance of this interface.
  
  (define file-icon
    (let ([bitmap
           (make-object bitmap%
             (collection-file-path "file.gif" "icons"))])
      (if (send bitmap ok?)
          (make-object image-snip% bitmap)
          (make-object string-snip% "[open file]"))))
  
  
  ;; insert/delta : (instanceof text%) (union snip string) (listof style-delta%) *-> (values number number)
  ;; inserts the string/snip into the text at the end and changes the
  ;; style of the newly inserted text based on the style deltas.
  (define (insert/delta text s . deltas)
    (define before (send text last-position))
    (send text insert s before before #f)
    (define after (send text last-position))
    (for ([delta (in-list deltas)])
      (when (is-a? delta style-delta%)
        (send text change-style delta before after)))
    (values before after))
  
  (define log-max-size 1000)
  (define log-entry-max-size 1000)
  
  (define after-expression (make-parameter #f))
  
  (define text-mixin
    (mixin ((class->interface text%)
            text:ports<%>
            editor:file<%>
            racket:text<%>
            color:text<%>
            text:ports<%>)
      (-text<%>)
      (init-field context)
      (inherit auto-wrap
               begin-edit-sequence
               change-style
               clear-box-input-port
               clear-undos
               clear-input-port
               clear-output-ports
               delete
               delete/io
               end-edit-sequence
               erase
               find-snip
               find-string
               freeze-colorer
               get-active-canvas
               get-admin
               get-can-close-parent
               get-canvases
               get-character
               get-end-position
               get-err-port
               get-extent
               get-focus-snip
               get-in-port
               get-in-box-port
               get-insertion-point
               get-out-port
               get-regions
               get-snip-position
               get-start-position
               get-styles-fixed
               get-style-list
               get-text
               get-top-level-window
               get-unread-start-point
               get-value-port
               in-edit-sequence?
               insert
               insert-before
               insert-between
               is-locked?
               last-position
               line-location
               lock
               paragraph-start-position
               position-line
               position-paragraph
               port-name-matches?
               release-snip
               reset-input-box
               reset-regions
               run-after-edit-sequence
               scroll-to-position
               send-eof-to-in-port 
               set-allow-edits
               set-caret-owner
               set-clickback
               set-insertion-point
               set-position
               set-styles-sticky
               set-styles-fixed
               set-unread-start-point
               split-snip
               thaw-colorer)
      
      (define definitions-text 'not-yet-set-definitions-text)
      (define/public (set-definitions-text dt) (set! definitions-text dt))
      (define/public (get-definitions-text) definitions-text)
      
      (unless (is-a? context drracket:rep:context<%>)
        (error 'drracket:rep:text% 
               "expected an object that implements drracket:rep:context<%> as initialization argument, got: ~e"
               context))
      
      (define/public (get-context) context)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                          ;;;
      ;;;            User -> Kernel                ;;;
      ;;;                                          ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; =User= (probably doesn't matter)
      (define/private queue-system-callback
        (λ (ut thunk [always? #f])
          (parameterize ([current-eventspace drracket:init:system-eventspace])
            (queue-callback 
             (λ ()
               (when (or always? (eq? ut (get-user-thread)))
                 (thunk)))
             #f))))
      
      ;; =User=
      (define/private queue-system-callback/sync
        (λ (ut thunk)
          (let ([s (make-semaphore 0)])
            (queue-system-callback 
             ut 
             (λ ()
               (when (eq? ut (get-user-thread))
                 (thunk))
               (semaphore-post s))
             #t)
            (semaphore-wait s))))
      
      ;; display-results : (listof TST) -> void
      ;; prints each element of anss that is not void as values in the REPL.
      (define/public (display-results anss) ; =User=, =Handler=, =Breaks=
        (display-results/void (filter (λ (x) (not (void? x))) anss)))
      
      ;; display-results : (listof TST) -> void
      ;; prints each element of anss in the REPL.
      (define/public (display-results/void anss) ; =User=, =Handler=, =Breaks=
        (for-each 
         (λ (v) 
           (let* ([ls (current-language-settings)]
                  [lang (drracket:language-configuration:language-settings-language ls)]
                  [settings (drracket:language-configuration:language-settings-settings ls)])
             (send lang render-value/format
                   v
                   settings
                   (get-value-port)
                   (get-repl-char-width))))
         anss))
      
      ;; get-repl-char-width : -> (and/c exact? integer?)
      ;; returns the width of the repl in characters, or 80 if the
      ;; answer cannot be found.
      (define/private (get-repl-char-width)
        (let ([admin (get-admin)]
              [standard (send (get-style-list) find-named-style "Standard")])
          (if (and admin standard)
              (let ([bw (box 0)])
                (send admin get-view #f #f bw #f)
                (let* ([dc (send admin get-dc)]
                       [standard-font (send standard get-font)]
                       [old-font (send dc get-font)])
                  (send dc set-font standard-font)
                  (let* ([char-width (send dc get-char-width)]
                         [answer (inexact->exact (floor (/ (unbox bw) char-width)))])
                    (send dc set-font old-font)
                    answer)))
              80)))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                            ;;;
      ;;;            Error Highlighting              ;;;
      ;;;                                            ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; error-ranges : (union false? (cons srcloc (listof srcloc)))
      (define error-ranges #f)
      (define/public (get-error-ranges) error-ranges)
      (define/public (set-error-ranges ranges)
        (set! error-ranges (and ranges 
                                (not (null? ranges))
                                (cleanup-locs ranges))))
      (define clear-error-highlighting void)
      (define/public (reset-error-ranges) 
        (set-error-ranges #f)
        (when definitions-text (send definitions-text set-error-arrows #f))
        (clear-error-highlighting))
      
      ;; highlight-error : file number number -> void
      (define/public (highlight-error file start end)
        (highlight-errors (list (make-srcloc file #f #f start (- end start))) #f))
      
      ;; highlight-errors/exn : exn -> void
      ;; highlights all of the errors associated with the exn (incl. arrows)
      (define/public (highlight-errors/exn exn)
        (let ([locs (cond
                      [(exn:srclocs? exn)
                       ((exn:srclocs-accessor exn) exn)]
                      [else '()])])
          (highlight-errors locs #f)))
      
      ;; =Kernel= =handler=
      ;; highlight-errors :    (listof srcloc)
      ;;                       (union #f (listof srcloc))
      ;;                    -> (void)
      (define/public (highlight-errors raw-locs [raw-error-arrows #f])
        (clear-error-highlighting)
        (when definitions-text (send definitions-text set-error-arrows #f))
        (set-error-ranges raw-locs)
        (define locs (or (get-error-ranges) '())) ;; calling set-error-range cleans up the locs
        (define error-arrows (and raw-error-arrows (cleanup-locs raw-error-arrows)))
        
        (for-each (λ (loc) (send (srcloc-source loc) begin-edit-sequence)) locs)
        
        (when color?
          (let ([resets
                 (map (λ (loc)
                        (let* ([file (srcloc-source loc)]
                               [start (- (srcloc-position loc) 1)]
                               [span (srcloc-span loc)]
                               [finish (+ start span)])
                          (send file highlight-range start finish (drracket:debug:get-error-color) #f 'high)))
                      locs)])
            (when (and definitions-text error-arrows)
              (let ([filtered-arrows
                     (remove-duplicate-error-arrows
                      (filter
                       (λ (arr) (embedded-in? (srcloc-source arr) definitions-text))
                       error-arrows))])
                (send definitions-text set-error-arrows filtered-arrows)))
            
            (set! clear-error-highlighting
                  (λ ()
                    (set! clear-error-highlighting void)
                    (for-each (λ (x) (x)) resets)))))

        (let* ([first-loc (and (pair? locs) (car locs))]
               [first-file (and first-loc (srcloc-source first-loc))]
               [first-start (and first-loc (- (srcloc-position first-loc) 1))]
               [first-span (and first-loc (srcloc-span first-loc))])
          
          (when (and first-loc first-start first-span)
            (let ([first-finish (+ first-start first-span)])
              (when (eq? first-file definitions-text) ;; only move set the cursor in the defs window
                (send first-file set-position first-start first-start))
              (send first-file scroll-to-position first-start #f first-finish)))
          
          (for-each (λ (loc) (send (srcloc-source loc) end-edit-sequence)) locs)
          
          (when first-loc
            
            (when (eq? first-file definitions-text)
              ;; when we're highlighting something in the defs window,
              ;; make sure it is visible
              (let ([tlw (send first-file get-top-level-window)]) 
                (when (is-a? tlw drracket:unit:frame<%>)
                  (send tlw ensure-defs-shown))))
            
            (send first-file set-caret-owner (get-focus-snip) 'global))))

      ;; unlike highlight-error just above, this function does not change 
      ;; what the currently noted errors locations are, it just highlights 
      ;; one of them.
      (define/public (highlight-a-single-error raw-loc)
        (define loc (car (cleanup-locs (list raw-loc))))
        (define source (srcloc-source loc))
        (when (and (is-a? source text%)
                   (srcloc-position loc)
                   (srcloc-span loc))
          (send source begin-edit-sequence)
          
          (clear-error-highlighting) ;; clear the 'highlight-range' from previous errors
          
          (define start (- (srcloc-position loc) 1))
          (define span (srcloc-span loc))
          (define finish (+ start span))
          
          (let ([reset (send source highlight-range start finish (drracket:debug:get-error-color) #f 'high)])
            (set! clear-error-highlighting
                  (λ ()
                    (set! clear-error-highlighting void)
                    (reset))))
          
          (when (and start span)
            (let ([finish (+ start span)])
              (when (eq? source definitions-text) ;; only move set the cursor in the defs window
                (send source set-position start span))
              (send source scroll-to-position start #f finish)))
          
          (send source end-edit-sequence)
          
          (when (eq? source definitions-text)
            ;; when we're highlighting something in the defs window,
            ;; make sure it is visible
            (let ([tlw (send source get-top-level-window)]) 
              (when (is-a? tlw drracket:unit:frame<%>)
                (send tlw ensure-defs-shown))))
          
          (send source set-caret-owner (get-focus-snip) 'global)))
        
      (define/private (cleanup-locs locs)
        (let ([ht (make-hasheq)])
          (filter (λ (loc) (and (is-a? (srcloc-source loc) text:basic<%>)
                                (number? (srcloc-position loc))
                                (number? (srcloc-span loc))))
                  (map (λ (srcloc)
                         (cond
                           [(hash-ref ht (srcloc-source srcloc) #f)
                            =>
                            (λ (e) 
                              (make-srcloc e
                                           (srcloc-line srcloc)
                                           (srcloc-column srcloc)
                                           (srcloc-position srcloc)
                                           (srcloc-span srcloc)))]
                           [(send definitions-text port-name-matches? (srcloc-source srcloc))
                            (hash-set! ht (srcloc-source srcloc) definitions-text)
                            (make-srcloc definitions-text
                                         (srcloc-line srcloc)
                                         (srcloc-column srcloc)
                                         (srcloc-position srcloc)
                                         (srcloc-span srcloc))]
                           [(port-name-matches? (srcloc-source srcloc))
                            (hash-set! ht (srcloc-source srcloc) this)
                            (make-srcloc this
                                         (srcloc-line srcloc)
                                         (srcloc-column srcloc)
                                         (srcloc-position srcloc)
                                         (srcloc-span srcloc))]
                           [(and (symbol? (srcloc-source srcloc))
                                 (text:lookup-port-name (srcloc-source srcloc)))
                            =>
                            (lambda (editor)
                              (make-srcloc editor
                                           (srcloc-line srcloc)
                                           (srcloc-column srcloc)
                                           (srcloc-position srcloc)
                                           (srcloc-span srcloc)))]
                           [else srcloc]))
                       locs))))
      
      (define highlights-can-be-reset (make-parameter #t))
      (define/public (reset-highlighting)
        (when (highlights-can-be-reset) (reset-error-ranges)))
      (define/public (call-without-reset-highlighting thunk)
        (parameterize ([highlights-can-be-reset #f])
          (thunk)))
      
      ;; remove-duplicate-error-arrows : (listof X) -> (listof X)
      ;; duplicate arrows point from and to the same place -- only
      ;; need one arrow for each pair of locations they point to.
      (define/private (remove-duplicate-error-arrows error-arrows)
        (let ([ht (make-hash)])
          (let loop ([arrs error-arrows]
                     [n 0])
            (unless (null? arrs)
              (hash-set! ht (car arrs) n)
              (loop (cdr arrs) (+ n 1))))
          (let* ([unsorted (hash-map ht list)]
                 [sorted (sort unsorted (λ (x y) (<= (cadr x) (cadr y))))]
                 [arrs (map car sorted)])
            arrs)))
      
      (define/private (embedded-in? txt-inner txt-outer)
        (let loop ([txt-inner txt-inner])
          (cond
            [(eq? txt-inner txt-outer) #t]
            [else (let ([admin (send txt-inner get-admin)])
                    (and (is-a? admin editor-snip-editor-admin<%>)
                         (loop (send (send (send admin get-snip) get-admin) get-editor))))])))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  specialization
      ;;
      
      (define/override (after-io-insertion)
        (super after-io-insertion)
        (let ([frame (get-frame)])
          (when frame
            (let ([tab (send definitions-text get-tab)])
              (when (eq? (send frame get-current-tab) tab)
                (send context ensure-rep-shown this))))))
      
      (define/augment (after-insert start len)
        (inner (void) after-insert start len)
        (cond
          [(in-edit-sequence?) 
           (set! had-an-insert (cons start had-an-insert))]
          [else (update-after-inserts (list start))]))
      
      ;; private field
      (define had-an-insert '())
      
      (define/augment (after-edit-sequence)
        (inner (void) after-edit-sequence)
        (unless (null? had-an-insert)
          (let ([to-clean had-an-insert])
            (set! had-an-insert '())
            (update-after-inserts to-clean))))
      
      (define/private (update-after-inserts starts)
        (unless inserting-prompt?
          (reset-highlighting))
        (when (and prompt-position 
                   (ormap (λ (start) (< start prompt-position))
                          starts))
          (set! prompt-position (get-unread-start-point))
          (reset-regions (append (all-but-last (get-regions))
                                 (list (list prompt-position 'end))))))
      
      (define/augment (after-delete x y)
        (unless inserting-prompt?
          (reset-highlighting))
        (inner (void) after-delete x y))
      
      (define/override (get-keymaps)
        (editor:add-after-user-keymap scheme-interaction-mode-keymap
                                      (super get-keymaps)))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                            ;;;
      ;;;                Evaluation                  ;;;
      ;;;                                            ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define/public (eval-busy?)
        (not (and (get-user-thread)
                  (thread-running? (get-user-thread)))))
      
      (field (user-language-settings #f)
             (user-custodian-parent #f)
             (memory-killed-cust-box #f)
             (user-custodian #f)
             (custodian-limit (and (custodian-memory-accounting-available?)
                                   (preferences:get 'drracket:child-only-memory-limit)))
             (user-eventspace-box (make-weak-box #f))
             (user-namespace-box (make-weak-box #f))
             (user-eventspace-main-thread #f)
             (user-break-parameterization #f)
             (user-logger drracket:init:system-logger) ;; for now, just let all messages be everywhere
             
             ;; user-exit-code (union #f byte?)
             ;; #f indicates that exit wasn't called. Integer indicates exit code
             (user-exit-code #f))
            
      (define/public (get-user-language-settings) user-language-settings)
      (define/public (get-user-custodian) user-custodian)
      (define/public (get-user-eventspace) (weak-box-value user-eventspace-box))
      (define/public (get-user-thread) user-eventspace-main-thread)
      (define/public (get-user-namespace) (weak-box-value user-namespace-box))
      (define/pubment (get-user-break-parameterization) user-break-parameterization) ;; final method
      (define/pubment (get-custodian-limit) custodian-limit)
      (define/pubment (set-custodian-limit c) (set! custodian-limit c))
      (field (in-evaluation? #f)
             (ask-about-kill? #f))
      (define/public (get-in-evaluation?) in-evaluation?)
      
      (define/public-final (insert-warning message)
        (let ([locked? (is-locked?)])
          (when locked? (lock #f))
          (begin-edit-sequence)
          (let ([start (get-unread-start-point)])
            (insert-before message)
            (let ([end (get-unread-start-point)])
              (change-style warning-style-delta start end)
              (insert-before "\n")))
          (end-edit-sequence)
          (when locked? (lock #t))))
      
      (field (show-no-user-evaluation-message? #t))
      
      ;; use this to be able to kill the evaluator without the popup dialog
      (define/public (set-show-no-user-evaluation-message? b)
        (set! show-no-user-evaluation-message? b))
      
      (define/private (cleanup)
        (set! in-evaluation? #f)
        (update-running #f)
        (unless (and (get-user-thread) (thread-running? (get-user-thread)))
          (lock #t)
          (when (and show-no-user-evaluation-message? (not shutting-down?))
            (no-user-evaluation-message
             (get-frame)
             user-exit-code
             (not (custodian-box-value memory-killed-cust-box))))
          (set! show-no-user-evaluation-message? #t)))
      
      (field (need-interaction-cleanup? #f))
      
      (define/private (no-user-evaluation-message frame exit-code memory-killed?)
        (define new-limit (and custodian-limit (+ custodian-limit custodian-limit)))
        (define-values (ans checked?)
          (if (preferences:get 'drracket:show-killed-dialog)
              (message+check-box/custom
               (string-constant evaluation-terminated)
               (string-append
                (string-constant evaluation-terminated-explanation)
                (if exit-code
                    (string-append
                     "\n\n"
                     (if (zero? exit-code)
                         (string-constant exited-successfully)
                         (format (string-constant exited-with-error-code) exit-code)))
                    "")
                (if memory-killed?
                    (string-append 
                     "\n\n"
                     (string-constant program-ran-out-of-memory))
                    ""))
               (string-constant evaluation-terminated-ask)
               (string-constant ok)
               #f
               (and memory-killed?
                    new-limit
                    (format "Increase memory limit to ~a megabytes" 
                            (floor (/ new-limit 1024 1024))))
               frame
               '(default=1 stop checked)
               #:dialog-mixin frame:focus-table-mixin)
              (values 1 #t)))
        (unless checked?
          (preferences:set 'drracket:show-killed-dialog #f))
        (when (equal? ans 3)
          (set-custodian-limit new-limit)
          (preferences:set 'drracket:child-only-memory-limit new-limit))
        (set-insertion-point (last-position))
        (insert-warning "\nInteractions disabled"))
      
      (define/private (cleanup-interaction) ; =Kernel=, =Handler=
        (set! need-interaction-cleanup? #f)
        (begin-edit-sequence)
        (set-caret-owner #f 'display)
        (cleanup)
        (end-edit-sequence)
        (send context set-breakables #f #f)
        (send context enable-evaluation))
      
      (define/augment (submit-to-port? key)
        (or (eq? (send key get-key-code) 'numpad-enter)
            (send key get-control-down)
            (send key get-alt-down)
            (and prompt-position
                 (let ([lang (drracket:language-configuration:language-settings-language user-language-settings)])
                   (cond
                     [(is-a? lang drracket:module-language:module-language<%>)
                      (let ([pred 
                             (send lang get-language-info 
                                   'drracket:submit-predicate
                                   (λ (port only-whitespace-afterwards?)
                                     (and only-whitespace-afterwards?
                                          (submit-predicate this prompt-position))))])
                        (pred 
                         ;; no good! giving away the farm here. need to hand
                         ;; over a proxy that is limited to just read access
                         (open-input-text-editor this prompt-position)
                         (only-whitespace-after-insertion-point)))]
                     [else
                      (and (only-whitespace-after-insertion-point)
                           (submit-predicate this prompt-position))])))))
      
      (define/private (only-whitespace-after-insertion-point)
        (let ([start (get-start-position)]
              [end (get-end-position)])
          (and (= start end)
               (let loop ([pos start])
                 (cond
                   [(= pos (last-position)) #t]
                   [else (and (char-whitespace? (get-character pos))
                              (loop (+ pos 1)))])))))
      
      (define/augment (on-submit)
        (inner (void) on-submit)
        (when (and (get-user-thread) 
                   (thread-running? (get-user-thread)))
          ;; the -2 drops the last newline from history (why -2 and not -1?!)
          (save-interaction-in-history prompt-position (- (last-position) 2))
          
          (let* ([old-regions (get-regions)]
                 [abl (all-but-last old-regions)]
                 [lst (last old-regions)])
            (reset-regions (append abl (list (list (list-ref lst 0) (last-position))))))
          
          (send context repl-submit-happened)
          
          ;; lets us know we are done with this one interaction
          ;; (since there may be multiple expressions at the prompt)
          (send-eof-to-in-port)
          
          (set! prompt-position #f)
          (evaluate-from-port
           (get-in-port) 
           #f
           (λ ()
             ;; clear out the eof object if it wasn't consumed
             (clear-input-port)))))
      
      (inherit get-backward-sexp)
      (define/override (on-local-char key)
        (let ([start (get-start-position)]
              [end (get-end-position)]
              [code (send key get-key-code)])
          (cond
            [(not (or (eq? code 'numpad-enter)
                      (equal? code #\return)
                      (equal? code #\newline)))
             (super on-local-char key)]
            [(not prompt-position) 
             ;; evaluating? just drop the keypress
             (void)] 
            [(and (< end prompt-position)
                  (= start end)
                  (get-backward-sexp end))
             =>
             (λ (sexp-start)
               (copy-down sexp-start end))]
            [(and (< end prompt-position)
                  (not (= start end)))
             (copy-down start end)]
            [else
             (super on-local-char key)])))
      
      (define/private (copy-down start end)
        (begin-edit-sequence)
        (split-snip start)
        (split-snip end)
        (let loop ([snip (find-snip start 'after-or-none)])
          (when snip
            (let ([pos (+ (get-snip-position snip)
                          (send snip get-count))])
              (when (<= pos end)
                (insert (send snip copy) (last-position) (last-position))
                (loop (send snip next))))))
        (set-position (last-position) (last-position))
        (end-edit-sequence))
      
      ;; prompt-position : (union #f integer)
      ;; the position just after the last prompt
      (field (prompt-position #f))
      (define inserting-prompt? #f)
      (define/public (get-prompt) "> ")
      (define/public (insert-prompt)
        (set! inserting-prompt? #t)
        (begin-edit-sequence)
        (reset-input-box)
        (let* ([pmt (get-prompt)]
               [prompt-space (string-length pmt)])
          
          ;; insert the prompt, possibly inserting a newline first
          (let* ([usp (get-unread-start-point)]
                 [usp-para (position-paragraph usp)]
                 [usp-para-start (paragraph-start-position usp-para)])
            (unless (equal? usp usp-para-start)
              (insert-between "\n")
              (set! prompt-space (+ prompt-space 1)))
            (insert-between pmt))
          
          (let ([sp (get-unread-start-point)])
            (set! prompt-position sp)
            (reset-regions (append (get-regions) (list (list sp 'end))))))
        (end-edit-sequence)
        (set! inserting-prompt? #f))
      
      (field [submit-predicate (λ (text prompt-position) #t)])
      (define/public (set-submit-predicate p)
        (set! submit-predicate p))
      
      (define/public (evaluate-from-port port complete-program? cleanup) ; =Kernel=, =Handler=
        (send context disable-evaluation)
        (send context reset-offer-kill)
        (send context set-breakables (get-user-thread) (get-user-custodian))
        (reset-pretty-print-width)
        (set! in-evaluation? #t)
        (update-running #t)
        (set! need-interaction-cleanup? #t)
        (define the-after-expression (after-expression))
        
        (run-in-evaluation-thread
         (λ () ; =User=, =Handler=, =No-Breaks=
           (let* ([settings (current-language-settings)]
                  [lang (drracket:language-configuration:language-settings-language settings)]
                  [settings (drracket:language-configuration:language-settings-settings settings)]
                  [dummy-value (box #f)]
                  [get-sexp/syntax/eof 
                   (if complete-program?
                       (send lang front-end/complete-program port settings)
                       (send lang front-end/interaction port settings))])
             
             ; Evaluate the user's expression. We're careful to turn on
             ;   breaks as we go in and turn them off as we go out.
             ;   (Actually, we adjust breaks however the user wanted it.)
             
             
             ;; this binding of last-results is to catch the results 
             ;; that come from throwing to the prompt instead of
             ;; a normal exit
             (define last-results
               (call-with-values
                (λ ()
                  (call-with-continuation-prompt
                   (λ ()
                     (call-with-break-parameterization
                      user-break-parameterization
                      (λ ()
                        (let loop ()
                          (define sexp/syntax/eof (with-stack-checkpoint (get-sexp/syntax/eof)))
                          (cond
                            [(eof-object? sexp/syntax/eof) (abort-current-continuation 
                                                            (default-continuation-prompt-tag)
                                                            (λ () (values)))]
                            [else
                             (define results
                               (call-with-values
                                (λ ()
                                  ;; we duplicate the 'expand-syntax-to-top-form' dance that eval-syntax
                                  ;; does here, so that we can put 'with-stack-checkpoint's in to limit
                                  ;; the amount of DrRacket code we see in stacktraces
                                  (let loop ([stx sexp/syntax/eof])
                                    (define top-expanded (with-stack-checkpoint (expand-syntax-to-top-form stx)))
                                    (syntax-case top-expanded (begin)
                                      [(begin a1 . args)
                                       (let lloop ([args (syntax->list #'(a1 . args))])
                                         (cond
                                           [(null? (cdr args))
                                            (loop (car args))]
                                           [else
                                            (loop (car args))
                                            (lloop (cdr args))]))]
                                      [_ 
                                       (let ([expanded (with-stack-checkpoint (expand-syntax top-expanded))])
                                         (call-with-continuation-prompt
                                          (λ ()
                                            (with-stack-checkpoint (eval-syntax expanded)))
                                          (default-continuation-prompt-tag)
                                          (λ args
                                            (apply
                                             abort-current-continuation 
                                             (default-continuation-prompt-tag)
                                             args))))])))
                                list))
                             (parameterize ([pretty-print-columns pretty-print-width])
                               (for ([x (in-list results)])
                                 ((current-print) x)))
                             (loop)])))))
                   (default-continuation-prompt-tag)
                   (letrec ([me
                             (λ args
                               (cond
                                 [(and (pair? args)
                                       (null? (cdr args))
                                       (procedure? (car args))
                                       (procedure-arity-includes? (car args) 0))
                                  (call-with-continuation-prompt (car args) 
                                                                 (default-continuation-prompt-tag)
                                                                 me)]
                                 [else
                                  (call-with-continuation-prompt
                                   (λ ()
                                     (call-with-continuation-prompt
                                      (λ ()
                                        (apply
                                         abort-current-continuation 
                                         (default-continuation-prompt-tag)
                                         args)))))]))])
                     me)))
                list))
             (parameterize ([pretty-print-columns pretty-print-width])
               (for ([x (in-list last-results)])
                 ((current-print) x)))
             
             (when complete-program?
               (call-with-continuation-prompt
                (λ ()
                  (call-with-break-parameterization
                   user-break-parameterization
                   (λ ()
                     (send lang front-end/finished-complete-program settings))))
                (default-continuation-prompt-tag)
                (λ args (void))))
             
             (when the-after-expression 
               (call-with-continuation-prompt
                (λ () 
                  (the-after-expression))))
             
             (set! in-evaluation? #f)
             (update-running #f)
             (cleanup)
             (flush-output (get-value-port))
             (queue-system-callback/sync
              (get-user-thread)
              (λ () ; =Kernel=, =Handler= 
                (after-many-evals)
                (cleanup-interaction)
                (insert-prompt)))))))
      
      ;; =User=, =Handler=
      (define/pubment (on-execute rout) (inner (void) on-execute rout))
      
      ;; =Kernel=, =Handler=
      (define/pubment (after-many-evals) (inner (void) after-many-evals))
      
      (define/private shutdown-user-custodian ; =Kernel=, =Handler=
        ; Use this procedure to shutdown when in the middle of other cleanup
        ;  operations, such as when the user clicks "Run".
        ; Don't use it to kill a thread where other, external cleanup
        ;  actions must occur (e.g., the exit handler for the user's
        ;  thread). In that case, shut down user-custodian directly.
        (λ ()
          (when user-custodian
            (custodian-shutdown-all user-custodian))
          (set! user-custodian #f)
          (set! user-eventspace-main-thread #f)))
      
      (define/public (kill-evaluation) ; =Kernel=, =Handler=
        (when user-custodian
          (custodian-shutdown-all user-custodian))
        (set! user-custodian #f))
      
      (field (eval-thread-thunks null)
             (eval-thread-state-sema 'not-yet-state-sema)
             (eval-thread-queue-sema 'not-yet-thread-sema)
             
             (cleanup-sucessful 'not-yet-cleanup-sucessful)
             (cleanup-semaphore 'not-yet-cleanup-semaphore)
             (thread-killed 'not-yet-thread-killed))
      (define/private (initialize-killed-thread) ; =Kernel=
        (when (thread? thread-killed)
          (kill-thread thread-killed))
        (set! thread-killed
              (thread
               (λ () ; =Kernel=
                 (let ([ut (get-user-thread)])
                   (thread-wait ut)
                   (queue-system-callback
                    ut
                    (λ () ; =Kernel=, =Handler=
                      (if need-interaction-cleanup?
                          (cleanup-interaction)
                          (cleanup))
                      ;; HACK: lock the interactions now; the reason for this
                      ;; is that `cleanup-interaction' invokes
                      ;; `enable-evaluation', and in "unit.rkt" this is defined
                      ;; to unlock the interactions which might make sense in
                      ;; that context.
                      (lock #t))))))))
      
      (define/public (run-in-evaluation-thread thunk) ; =Kernel=
        (semaphore-wait eval-thread-state-sema)
        (set! eval-thread-thunks (append eval-thread-thunks (list thunk)))
        (semaphore-post eval-thread-state-sema)
        (semaphore-post eval-thread-queue-sema))
      
      (define/private (init-evaluation-thread) ; =Kernel=
        (set! user-language-settings (send definitions-text get-next-settings))
        
        (set! user-custodian-parent (make-custodian))
        (set! user-custodian (parameterize ([current-custodian user-custodian-parent])
                               (make-custodian)))
        (set! memory-killed-cust-box (make-custodian-box user-custodian-parent #t))
        (when custodian-limit
          (custodian-limit-memory user-custodian-parent 
                                  custodian-limit
                                  user-custodian-parent))
        (let ([user-eventspace (parameterize ([current-custodian user-custodian])
                                 (make-eventspace))])
          (set! user-eventspace-box (make-weak-box user-eventspace))
          (set! user-break-parameterization (parameterize-break 
                                             #t 
                                             (current-break-parameterization)))
          (set! eval-thread-thunks null)
          (set! eval-thread-state-sema (make-semaphore 1))
          (set! eval-thread-queue-sema (make-semaphore 0))
          (set! user-exit-code #f)
          
          (reset-logger-messages)
          
          (let* ([init-thread-complete (make-semaphore 0)]
                 [goahead (make-semaphore)])
            
            ; setup standard parameters
            (let ([snip-classes
                   ; the snip-classes in the DrRacket eventspace's snip-class-list
                   (drracket:eval:get-snip-classes)]
                  [drs-eventspace (current-eventspace)])
              (queue-user/wait
               (λ () ; =User=, =No-Breaks=
                 ; No user code has been evaluated yet, so we're in the clear...
                 (break-enabled #f)
                 (set! user-eventspace-main-thread (current-thread))
                 
                 (current-logger user-logger)
                 
                 (initialize-parameters snip-classes)
                 (let ([drracket-exit-handler
                        (λ (x)
                          (parameterize-break
                           #f
                           (let ([s (make-semaphore)])
                             (parameterize ([current-eventspace drs-eventspace])
                               (queue-callback
                                (λ ()
                                  (set! user-exit-code 
                                        (if (exact-integer? x)
                                            (modulo x 256)
                                            0))
                                  (semaphore-post s))))
                             (semaphore-wait s)
                             (custodian-shutdown-all user-custodian))))])
                   (exit-handler drracket-exit-handler)))))
            

            (queue-user/wait
             (λ ()
               ;; register drscheme with the planet-terse-register for the user's namespace
               ;; must be called after 'initialize-parameters' is called (since it initializes
               ;; the user's namespace)
               (planet-terse-set-key (namespace-module-registry (current-namespace)))
               (planet-terse-register 
                (lambda (tag package)
                  (parameterize ([current-eventspace drracket:init:system-eventspace])
                    (queue-callback (λ () (new-planet-info tag package))))))))
               
            ;; disable breaks until an evaluation actually occurs
            (send context set-breakables #f #f)
            
            ;; initialize the language
            (send (drracket:language-configuration:language-settings-language user-language-settings)
                  on-execute
                  (drracket:language-configuration:language-settings-settings user-language-settings)
                  (let ([run-on-user-thread (lambda (t) 
                                              (queue-user/wait 
                                               (λ ()
                                                 (with-handlers ((exn? (λ (x) (oprintf "~s\n" (exn-message x)))))
                                                   (t)))))])
                    run-on-user-thread))
            
            ;; setup the special repl values
            (let ([raised-exn? #f]
                  [exn #f])
              (queue-user/wait
               (λ () ; =User=, =No-Breaks=
                 (with-handlers ((void (λ (x) 
                                         (set! exn x)
                                         (set! raised-exn? #t))))
                   (drracket:language:setup-setup-values))))
              (when raised-exn?
                (eprintf
                 (string-append
                  "copied exn raised when setting up snip values"
                  " (thunk passed as third argume to drracket:language:add-snip-value)\n"))
                (raise exn)))
            
            ;; allow extensions to this class to do some setup work
            (on-execute 
             (let ([run-on-user-thread (lambda (t) (queue-user/wait t))])
               run-on-user-thread))
            
            (parameterize ([current-eventspace user-eventspace])
              (queue-callback
               (λ ()
                 (set! in-evaluation? #f)
                 (update-running #f)
                 (send context set-breakables #f #f)
                 
                 ;; after this returns, future event dispatches
                 ;; will use the user's break parameterization
                 (initialize-dispatch-handler)
                 
                 ;; let init-thread procedure return,
                 ;; now that parameters are set
                 (semaphore-post init-thread-complete)
                 
                 ; We're about to start running user code.
                 
                 ; Pause to let killed-thread get initialized
                 (semaphore-wait goahead)
                 
                 (let loop () ; =User=, =Handler=, =No-Breaks=
                   ; Wait for something to do
                   (unless (semaphore-try-wait? eval-thread-queue-sema)
                     ; User event callbacks run here; we turn on
                     ;  breaks in the dispatch handler.
                     (yield eval-thread-queue-sema))
                   ; About to eval something
                   (semaphore-wait eval-thread-state-sema)
                   (let ([thunk (car eval-thread-thunks)])
                     (set! eval-thread-thunks (cdr eval-thread-thunks))
                     (semaphore-post eval-thread-state-sema)
                     ; This thunk evals the user's expressions with appropriate
                     ;   protections.
                     (thunk))
                   (loop)))))
            (semaphore-wait init-thread-complete)
            ; Start killed-thread
            (initialize-killed-thread)
            ; Let user expressions go...
            (semaphore-post goahead))))
      
      (define/private (queue-user/wait thnk)
        (let ([wait (make-semaphore 0)])
          (parameterize ([current-eventspace (get-user-eventspace)])
            (queue-callback
             (λ ()
               (thnk)
               (semaphore-post wait))))
          (semaphore-wait wait)))
      
      (field (shutting-down? #f))
      
      (define/override (allow-close-with-no-filename?) #t)
      (define/augment (can-close?)
        (and (cond
               [in-evaluation?
                (equal? (message-box/custom
                         (string-constant drscheme)
                         (string-constant program-is-still-running)
                         (string-constant close-anyway)
                         (string-constant cancel)
                         #f
                         (or (get-top-level-window) (get-can-close-parent))
                         '(default=1 caution)
                         2
                         #:dialog-mixin frame:focus-table-mixin)
                        1)]
               [(let ([user-eventspace (get-user-eventspace)])
                  (and user-eventspace
                       (parameterize ([current-eventspace user-eventspace])
                         (not (null? (get-top-level-windows))))))
                (equal? (message-box/custom
                         (string-constant drscheme)
                         (string-constant program-has-open-windows)
                         (string-constant close-anyway)
                         (string-constant cancel)
                         #f
                         (or (get-top-level-window) (get-can-close-parent))
                         '(default=1 caution)
                         2
                         #:dialog-mixin frame:focus-table-mixin)
                        1)]
               [else #t])
             (inner #t can-close?)))
      
      (define/augment (on-close)
        (shutdown)
        (preferences:set 'drracket:console-previous-exprs 
                         (trim-previous-exprs
                          (append 
                           (preferences:get 'drracket:console-previous-exprs)
                           local-previous-exprs)))
        (inner (void) on-close))
      
      (define/public (shutdown) ; =Kernel=, =Handler=
        (set! shutting-down? #t)
        (when (thread? thread-killed)
          (kill-thread thread-killed)
          (set! thread-killed #f))
        (shutdown-user-custodian))
      
      (define/private update-running ; =User=, =Handler=, =No-Breaks=
        (λ (bool)
          (queue-system-callback
           (get-user-thread)
           (λ ()
             (send context update-running bool)))))
      
      (define logger-editor #f)
      (define logger-messages '())
      (define user-log-receiver-args-str (preferences:get 'drracket:logger-receiver-string))
      (define/public (set-user-log-receiver-args str args) 
        (set! user-log-receiver-args-str str)
        (update-log-receiver-to-match-str))
      (define/public (get-user-log-receiver-args-str) user-log-receiver-args-str)
      (define/public (enable/disable-capture-log logging-on?)
        (cond
          [logging-on?
           (update-log-receiver-to-match-str)]
          [else
           (channel-put user-log-receiver-changed #f)]))
      (define/private (update-log-receiver-to-match-str)
        (define args (parse-logger-args user-log-receiver-args-str))
        (channel-put user-log-receiver-changed
                     (and args
                          (apply make-log-receiver user-logger args))))
      (define user-log-receiver-changed (make-channel))
      (thread
       (λ ()
         (struct gui-event (start end name) #:prefab)
         (define callback-running? #f)
         (define evts '())
         (define sema (make-semaphore 1))
         (define (user-event-handler-callback)
           (semaphore-wait sema)
           (define my-evts evts)
           (set! evts '())
           (set! callback-running? #f)
           (semaphore-post sema)
           (for ([vec (in-list (reverse my-evts))])
             (new-log-message vec)))
         (let loop ([user-log-receiver #f])
           (sync
            (handle-evt user-log-receiver-changed
                        (λ (user-log-receiver) 
                          (loop user-log-receiver)))
            (if user-log-receiver
                (handle-evt user-log-receiver
                            (λ (vec)
                              (unless (gui-event? (vector-ref vec 2))
                                (semaphore-wait sema)
                                (set! evts (cons vec evts))
                                (unless callback-running?
                                  (queue-callback user-event-handler-callback #f)
                                  (set! callback-running? #t))
                                (semaphore-post sema))
                              (loop user-log-receiver)))
                never-evt)))))
      (define/public (get-logger-messages) logger-messages)
      (define/private (new-log-message vec)
        (define str
          (cond
            [(<= (string-length (vector-ref vec 1)) log-entry-max-size)
             (vector-ref vec 1)]
            [else
             (substring (vector-ref vec 1) 0 log-entry-max-size)]))
        (cond
          [(< (length logger-messages) log-max-size)
           (set! logger-messages (cons str logger-messages))
           (update-logger-gui (cons 'add-line str))]
          [else
           (set! logger-messages
                 (cons
                  str
                  (let loop ([msgs logger-messages])
                    (cond
                      [(null? (cdr msgs)) null]
                      [else (cons (car msgs) (loop (cdr msgs)))]))))
           (update-logger-gui (cons 'clear-last-line-and-add-line str))]))
      
      (define/private (reset-logger-messages) 
        (set! logger-messages '())
        (update-logger-gui #f))
      
      (define/private (update-logger-gui command)
        (let ([tab (send definitions-text get-tab)])
          (send tab update-logger-window command)))
                        
      (define/private (new-planet-info tag package) 
        (let ([frame (get-frame)])
          (when frame
            (send (send frame get-current-tab) new-planet-status tag package))))
      
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                          ;;;
      ;;;                Execution                 ;;;
      ;;;                                          ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; initialize-paramters : (listof snip-class%) -> void
      (define/private (initialize-parameters snip-classes) ; =User=
        
        (current-language-settings user-language-settings)
        (error-print-source-location #f)
        (error-display-handler drracket-error-display-handler)
        (current-load-relative-directory #f)
        (current-custodian user-custodian)
        (current-load text-editor-load-handler)
        
        (drracket:eval:set-basic-parameters snip-classes)
        (current-rep this)
        (let ([dir (or (send context get-directory)
                       drracket:init:first-dir)])
          (current-directory dir))
        
        (set! user-namespace-box (make-weak-box (current-namespace)))
        
        (current-output-port (get-out-port))
        (current-error-port (get-err-port))
        (current-value-port (get-value-port))
        (current-input-port (get-in-box-port))

        (current-print (lambda (v) (display-results (list v)))))
      
      (define/private (initialize-dispatch-handler) ;;; =User=
        (let* ([primitive-dispatch-handler (event-dispatch-handler)])
          (event-dispatch-handler
           (letrec ([drscheme-event-dispatch-handler ; <= a name for #<...> printout
                     (λ (eventspace) ; =User=, =Handler=
                       ; Breaking is enabled if the user turned on breaks and
                       ;  is in a `yield'. If we get a break, that's ok, because
                       ;  the kernel never queues an event in the user's eventspace.
                       (cond
                         [(eq? eventspace (get-user-eventspace))
                          ; =User=, =Handler=
                          
                          ; We must distinguish between "top-level" events and
                          ;  those within `yield' in the user's program.
                          (cond
                            [(not in-evaluation?)
                             ;; at this point, we must not be in a nested dispatch, so we can
                             ;; just disable breaks and rely on call-with-break-parameterization
                             ;; to restore them to the user's setting.
                             (call-with-break-parameterization
                              no-breaks-break-parameterization
                              (λ ()
                                ; =No-Breaks=
                                (send context reset-offer-kill)
                                (send context set-breakables (get-user-thread) (get-user-custodian))
                                (call-with-continuation-prompt
                                 (λ () ; =User=, =Handler=, =No-Breaks=
                                   (call-with-break-parameterization
                                    user-break-parameterization
                                    (λ () (primitive-dispatch-handler eventspace)))))
                                
                                ;; in principle, the line below might cause
                                ;; "race conditions" in the GUI. That is, there might
                                ;; be many little events that the user won't quite
                                ;; be able to break.
                                (send context set-breakables #f #f)))]
                            [else
                             ; Nested dispatch; don't adjust interface
                             (primitive-dispatch-handler eventspace)])]
                         [else 
                          ; =User=, =Non-Handler=, =No-Breaks=
                          (primitive-dispatch-handler eventspace)]))])
             drscheme-event-dispatch-handler))))
      
      (define/public (reset-console)
        (when (thread? thread-killed)
          (kill-thread thread-killed))
        (send context clear-annotations)
        (drracket:debug:hide-backtrace-window)
        (shutdown-user-custodian)
        (clear-input-port)
        (clear-box-input-port)
        (clear-output-ports)
        (set-allow-edits #t)
        
        ;; in case the last evaluation thread was killed, clean up some state.
        (lock #f)
        (set! in-evaluation? #f)
        (update-running #f)
        
        ;; clear out repl first before doing any work.
        (begin-edit-sequence)
        (set! prompt-position #f)
        (reset-input-box)
        (delete (paragraph-start-position 1) (last-position))
        (end-edit-sequence)
        
        ;; must init-evaluation-thread before determining
        ;; the language's name, since this updates user-language-settings
        (init-evaluation-thread)
        
        (begin-edit-sequence)
        (set-position (last-position) (last-position))
        
        (set! setting-up-repl? #t)
        (insert/delta this (string-append (string-constant language) ": ") welcome-delta)
        (let-values (((before after)
                      (insert/delta
                       this
                       (extract-language-name user-language-settings definitions-text)
                       dark-green-delta
                       (extract-language-style-delta user-language-settings)))
                     ((url) (extract-language-url user-language-settings)))
          (when url
            (set-clickback before after (λ args (send-url url))
                           click-delta)))
        (unless (is-default-settings? user-language-settings)
          (insert/delta this (string-append " [" (string-constant custom) "]") dark-green-delta))
        (when custodian-limit
          (insert/delta this 
                        "; memory limit: "
                        welcome-delta)
          (insert/delta this
                        (format "~a MB" (floor (/ custodian-limit 1024 1024)))
                        dark-green-delta))
        (insert/delta this ".\n" welcome-delta)
        
        (let ([osf (get-styles-fixed)])
          (set-styles-fixed #f)
          (send (drracket:language-configuration:language-settings-language user-language-settings)
                extra-repl-information
                (drracket:language-configuration:language-settings-settings user-language-settings)
                (open-output-text-editor this 'end))
          (set-styles-fixed osf))
        
        (set! setting-up-repl? #f)
        
        (reset-regions (list (list (last-position) (last-position))))
        (set-unread-start-point (last-position))
        (set-insertion-point (last-position))
        (set! indenting-limit (last-position))
        (set-allow-edits #f)
        (set! repl-header-end #f)
        (end-edit-sequence))
      
      (define/public (initialize-console)
        (begin-edit-sequence)
        (freeze-colorer)
        (set! setting-up-repl? #t)
        (insert/delta this (string-append (string-constant welcome-to) " ") welcome-delta)
        (let-values ([(before after)
                      (insert/delta this 
                                    (string-constant drscheme)
                                    click-delta
                                    drs-font-delta)])
          (insert/delta this (format (string-append ", " (string-constant version) " ~a [~a].\n") 
                                     (version:version) (system-type 'gc))
                        welcome-delta)
          (set-clickback before after 
                         (λ args (drracket:app:about-drscheme))
                         click-delta))
        (set! setting-up-repl? #f)
        (send context disable-evaluation)
        (reset-console)
        (thaw-colorer)
        (insert-prompt)
        
        ;; call the first-opened method on the user's thread, but wait here for that to terminate
        (let ([lang (drracket:language-configuration:language-settings-language user-language-settings)]
              [drr-evtspace (current-eventspace)]
              [s (make-semaphore 0)])
          
          (define-values (sp-err-other-end sp-err) (make-pipe-with-specials))
          (define-values (sp-out-other-end sp-out) (make-pipe-with-specials))
          (define io-chan (make-channel))
          
          ;; collect the IO to replay later
          (thread
           (λ () 
             (let loop ([ports (list sp-err-other-end sp-out-other-end)]
                        [io '()])
               (cond
                 [(null? ports) (channel-put io-chan io)]
                 [else
                  (apply sync
                         (map (λ (port) (handle-evt 
                                         port 
                                         (λ (_) 
                                           (define byte/special (read-byte-or-special port))
                                           (if (eof-object? byte/special)
                                               (loop (remq port ports) io)
                                               (loop ports (cons (cons port byte/special) 
                                                                 io))))))
                              ports))]))))
             
          (run-in-evaluation-thread
           (λ ()
             (let/ec k
               ;; we set the io ports here to ones that just collect the data
               ;; since we're blocking the eventspace handler thread (and thus IO to 
               ;; the user's ports can deadlock)
               (parameterize ([error-escape-handler (λ () (k (void)))]
                              [current-output-port sp-out]
                              [current-error-port sp-err])
                 (cond
                   ;; this is for backwards compatibility; drracket used to
                   ;; expect this method to be a thunk (but that was a bad decision)
                   [(object-method-arity-includes? lang 'first-opened 1)
                    (send lang first-opened
                          (drracket:language-configuration:language-settings-settings user-language-settings))]
                   [else
                    ;; this is the backwards compatible case.
                    (send lang first-opened)])))
             (semaphore-post s)))
          
          ;; wait for the first-opened method to finish up
          (semaphore-wait s)
          
          ;; close the output ports to get the above thread to terminate
          (close-output-port sp-err)
          (close-output-port sp-out)
          
          ;; duplicate it over to the user's ports, now that there is
          ;; no danger of deadlock
          (for ([i (in-list (reverse (channel-get io-chan)))])
            (define obj (cdr i))
            
            ((if (byte? obj) write-byte write-special)
             obj
             (if (eq? (car i) sp-err-other-end)
                 (get-err-port)
                 (get-out-port))))
          (flush-output (get-err-port))
          (flush-output (get-out-port)))
        
        (send context enable-evaluation)
        (end-edit-sequence)
        (clear-undos))
      
      (define indenting-limit 0)
      (define/override (get-limit n) 
        (cond
          [(< n indenting-limit) 0]
          [else indenting-limit]))
      
      ;; avoid calling paragraph-start-position very often.
      (define repl-header-end #f)
      (define/private (get-repl-header-end)
        (if repl-header-end
            repl-header-end
            (begin (set! repl-header-end (paragraph-start-position 2))
                   repl-header-end)))
      
      (define setting-up-repl? #f)
      (define/augment (can-change-style? start len)
        (and (inner #t can-change-style? start len)
             (or setting-up-repl?
                 (start . >= . (get-repl-header-end)))))
      
      (define/private (last-str l)
        (if (null? (cdr l))
            (car l)
            (last-str (cdr l))))
      
      (field (previous-expr-pos -1))
      
      (define/public (copy-previous-expr)
        (when prompt-position
          (let ([snip/strings (list-ref (get-previous-exprs) previous-expr-pos)])
            (begin-edit-sequence)
            (delete prompt-position (last-position) #f)
            (for-each (λ (snip/string)
                        (insert (if (is-a? snip/string snip%)
                                    (send snip/string copy)
                                    snip/string)
                                prompt-position))
                      snip/strings)
            (set-position (last-position))
            (end-edit-sequence))))
      
      (define/public (copy-next-previous-expr)
        (let ([previous-exprs (get-previous-exprs)])
          (unless (null? previous-exprs)
            (set! previous-expr-pos
                  (if (< (add1 previous-expr-pos) (length previous-exprs))
                      (add1 previous-expr-pos)
                      0))
            (copy-previous-expr))))
      (define/public (copy-prev-previous-expr)
        (let ([previous-exprs (get-previous-exprs)])
          (unless (null? previous-exprs)
            (set! previous-expr-pos
                  (if (previous-expr-pos . <= . 0)
                      (sub1 (length previous-exprs))
                      (sub1 previous-expr-pos)))
            (copy-previous-expr))))
      
      ;; private fields
      (define global-previous-exprs (preferences:get 'drracket:console-previous-exprs))
      (define local-previous-exprs null)
      (define/private (get-previous-exprs)
        (append global-previous-exprs local-previous-exprs))
      (define/private (add-to-previous-exprs snips)
        (set! local-previous-exprs (append local-previous-exprs (list snips))))
      
      ; list-of-lists-of-snip/strings? -> list-of-lists-of-snip/strings?
      (define/private (trim-previous-exprs lst)
        (define max-size 10000)
        (define (expr-size expr)
          (for/fold ([s 0]) ([e expr]) (+ s (string-length e))))
        (define within-bound
          (let loop ([marshalled (reverse (marshall-previous-exprs lst))]
                     [keep 0]
                     [sum 0])
            (if (empty? marshalled)
                keep
                (let* ([size (expr-size (first marshalled))]
                       [w/another (+ size sum)])
                  (if (> w/another max-size)
                      keep
                      (loop (rest marshalled) (add1 keep) w/another))))))
        (take-right lst within-bound))
      
      (define/private (save-interaction-in-history start end)
        (split-snip start)
        (split-snip end)
        (let ([snips
               (let loop ([snip (find-snip start 'after-or-none)]
                          [snips null])
                 (cond
                   [(not snip) snips]
                   [((get-snip-position snip) . <= . end)
                    (loop (send snip next)
                          (cons (send snip copy) snips))]
                   [else snips]))])
          (set! previous-expr-pos -1)
          (add-to-previous-exprs snips)))
      
      (define pretty-print-width (pretty-print-columns))
      (define/private (reset-pretty-print-width)
        (let* ([standard (send (get-style-list) find-named-style "Standard")])
          (when standard
            (let* ([admin (get-admin)]
                   [width
                    (let ([bw (box 0)]
                          [b2 (box 0)])
                      (send admin get-view b2 b2 bw b2)
                      (unbox bw))]
                   [dc (send admin get-dc)]
                   [new-font (send standard get-font)]
                   [old-font (send dc get-font)])
              (send dc set-font new-font)
              (let* ([char-width (send dc get-char-width)]
                     [min-columns 50]
                     [new-columns (max min-columns 
                                       (floor (/ width char-width)))])
                (send dc set-font old-font)
                (set! pretty-print-width new-columns))))))
      
      ;; get-frame : -> (or/c #f (is-a?/c frame))
      (define/private (get-frame)
        (let ([c (get-any-canvas)])
          (and c (send c get-top-level-window))))
      

      ;; returns the most recently active canvas or, if no canvas
      ;; has ever been active, it returns just any canvas
      (define/private (get-any-canvas)
        (or (get-active-canvas)
            (let ([canvases (get-canvases)])
              (and (not (null? canvases))
                   (car canvases)))))

      (inherit paragraph-end-position)
      (define/override (get-start-of-line pos)
        (define para (position-paragraph pos))
        (define para-start (paragraph-start-position para))
        (define para-end (paragraph-end-position para))
        (define after-prompt-start 
          (let* ([prompt (get-prompt)]
                 [para-start-text (get-text para-start (+ para-start (string-length prompt)))])
            (cond
              [(equal? prompt para-start-text)
               (+ para-start (string-length prompt))]
              [else
               para-start])))
        (define first-non-whitespace 
          (let loop ([i after-prompt-start])
            (cond
              [(= i para-end) #f]
              [(char-whitespace? (get-character i))
               (loop (+ i 1))]
              [else i])))
        (define new-pos 
          (cond 
            [(not first-non-whitespace) after-prompt-start]
            [(< pos after-prompt-start) after-prompt-start]
            [(= pos after-prompt-start) first-non-whitespace]
            [(<= pos first-non-whitespace) after-prompt-start]
            [else first-non-whitespace]))
        new-pos)
      
      (super-new)
      (auto-wrap #t)
      (set-styles-sticky #f)
      
      (inherit set-max-undo-history)
      (set-max-undo-history 'forever)))
  
  (define (all-but-last lst)
    (let loop ([o lst])
      (cond
        [(null? o) null]
        [(null? (cdr o)) null]
        [else (cons (car o) (loop (cdr o)))])))
  
  (define input-delta (make-object style-delta%))
  (send input-delta set-delta-foreground (make-object color% 0 150 0))
  
  ;; insert-error-in-text : (is-a?/c text%)
  ;;                        (union #f (is-a?/c drracket:rep:text<%>))
  ;;                        string?
  ;;                        exn?
  ;;                        (union false? (and/c string? directory-exists?))
  ;;                        ->
  ;;                        void?
  (define (insert-error-in-text text interactions-text msg exn user-dir)
    (insert-error-in-text/highlight-errors
     text
     (λ (l) (send interactions-text highlight-errors l))
     msg
     exn
     user-dir))
  
  ;; insert-error-in-text/highlight-errors : (is-a?/c text%)
  ;;                                         ((listof (list text% number number)) -> void)
  ;;                                         string?
  ;;                                         exn?
  ;;                                         (union false? (and/c string? directory-exists?))
  ;;                                         ->
  ;;                                         void?
  (define (insert-error-in-text/highlight-errors text highlight-errors msg exn user-dir)
    (let ([locked? (send text is-locked?)]
          [insert-file-name/icon
           ;; insert-file-name/icon : string number number number number -> void
           (λ (source-name start span row col)
             (let ([range-spec
                    (cond
                      [(and row col)
                       (format ":~a:~a" row col)]
                      [start
                       (format "::~a" start)]
                      [else ""])])
               (cond
                 [(file-exists? source-name)
                  (let* ([normalized-name (normalize-path source-name)]
                         [short-name (if user-dir
                                         (find-relative-path user-dir normalized-name)
                                         source-name)])
                    (let-values ([(icon-start icon-end) (insert/delta text (send file-icon copy))]
                                 [(space-start space-end) (insert/delta text " ")]
                                 [(name-start name-end) (insert/delta text short-name)]
                                 [(range-start range-end) (insert/delta text range-spec)]
                                 [(colon-start colon-ent) (insert/delta text ": ")])
                      (when (number? start)
                        (send text set-clickback icon-start range-end
                              (λ (_1 _2 _3)
                                (open-file-and-highlight normalized-name
                                                         (- start 1) 
                                                         (if span
                                                             (+ start -1 span)
                                                             start)))))))]
                 [else
                  (insert/delta text source-name)
                  (insert/delta text range-spec)
                  (insert/delta text ": ")])))])
      (send text begin-edit-sequence)
      (send text lock #f)
      (cond
        [(exn:fail:syntax? exn)
         (for-each
          (λ (expr)
            (let ([src (and (syntax? expr) (syntax-source expr))]
                  [pos (and (syntax? expr) (syntax-position expr))]
                  [span (and (syntax? expr) (syntax-span expr))]
                  [col (and (syntax? expr) (syntax-column expr))]
                  [line (and (syntax? expr) (syntax-line expr))])
              (when (and (string? src)
                         (number? pos)
                         (number? span)
                         (number? line)
                         (number? col))
                (insert-file-name/icon src pos span line col))
              (insert/delta text (format "~a" (exn-message exn)) error-delta)
              (when (and (error-print-source-location)
                         (syntax? expr))
                (insert/delta text " in: ")
                (insert/delta text (format "~s" (syntax->datum expr)) error-text-style-delta))
              (insert/delta text "\n")
              (when (and (is-a? src text:basic<%>)
                         (number? pos)
                         (number? span))
                (highlight-errors (list (list src (- pos 1) (+ pos -1 span)))))))
          (exn:fail:syntax-exprs exn))]
        [(exn:fail:read? exn)
         '(let ([src (exn:read-source exn)]
                [pos (exn:read-position exn)]
                [span (exn:read-span exn)]
                [line (exn:read-line exn)]
                [col (exn:read-column exn)])
            (when (and (string? src)
                       (number? pos)
                       (number? span)
                       (number? line)
                       (number? col))
              (insert-file-name/icon src pos span line col))
            (insert/delta text (format "~a" (exn-message exn)) error-delta)
            (insert/delta text "\n")
            (when (and (is-a? src text:basic<%>)
                       (number? pos)
                       (number? span))
              (highlight-errors (list (list src (- pos 1) (+ pos -1 span))))))]
        [(exn? exn)
         (insert/delta text (format "~a" (exn-message exn)) error-delta)
         (insert/delta text "\n")]
        [else
         (insert/delta text "uncaught exception: " error-delta)
         (insert/delta text (format "~s" exn) error-delta)
         (insert/delta text "\n")])
      (send text lock locked?)
      (send text end-edit-sequence)))
  
  
  ;; open-file-and-highlight : string (union number #f) (union number #f)
  ;; =Kernel, =Handler=
  ;; opens the file named by filename. If position is #f,
  ;; doesn't highlight anything. If position is a number and other-position
  ;; is #f, highlights the range from position to the end of sexp.
  ;; if other-position is a number, highlights from position to 
  ;; other position.
  (define (open-file-and-highlight filename position other-position)
    (let ([file (handler:edit-file filename)])
      (when (and (is-a? file drracket:unit:frame%)
                 position)
        (if other-position
            (send (send file get-interactions-text)
                  highlight-error
                  (send file get-definitions-text)
                  position
                  other-position)
            (send (send file get-interactions-text)
                  highlight-error/forward-sexp
                  (send file get-definitions-text)
                  position)))))
  
  (define drs-autocomplete-mixin
    (λ (get-defs x)
      (class (text:autocomplete-mixin x)
        (define/override (get-all-words)
          (let* ([definitions-text (get-defs this)]
                 [settings (send definitions-text get-next-settings)]
                 [language (drracket:language-configuration:language-settings-language settings)])
            (send language capability-value 'drscheme:autocomplete-words)))
        (super-new))))
  
  (define -text% 
    (text-mixin 
     ;; drs-bindings-keymap-mixin has to come
     ;; before text-mixin so that the keymaps 
     ;; get added in the right order (specifically
     ;; so that esc;n and esc;p work right in the
     ;; repl (prev and next interaction) and in the defs
     ;; (previous and next error))
     (drs-bindings-keymap-mixin
      (text:ports-mixin
       (racket:text-mixin
        (color:text-mixin
         (text:info-mixin
          (editor:info-mixin
           (text:searching-mixin
            (mode:host-text-mixin
             (drs-autocomplete-mixin 
              (λ (txt) (send txt get-definitions-text))
              (text:foreground-color-mixin
               (text:normalize-paste-mixin
                text:clever-file-format%))))))))))))))
