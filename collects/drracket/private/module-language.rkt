#lang racket/base

(provide module-language@)
(require racket/unit
         racket/class
         racket/list
         racket/path
         racket/contract
         racket/sandbox
         racket/runtime-path
         racket/math
         racket/gui/base
         compiler/embed
         compiler/cm
         launcher
         framework
         string-constants
         planet/config
         setup/dirs
         racket/place
         "tooltip.rkt"
         "drsig.rkt"
         "rep.rkt"
         "eval-helpers.rkt"
         "local-member-names.rkt")

(define-runtime-path expanding-place.rkt "expanding-place.rkt")

(define sc-online-expansion-running (string-constant online-expansion-running))
(define sc-only-raw-text-files-supported (string-constant only-raw-text-files-supported))
(define sc-abnormal-termination (string-constant abnormal-termination))
(define sc-jump-to-error (string-constant jump-to-error))

(define op (current-output-port))
(define (oprintf . args) (apply fprintf op args))

(define-unit module-language@
  (import [prefix drracket:language-configuration: drracket:language-configuration/internal^]
          [prefix drracket:language: drracket:language^]
          [prefix drracket:unit: drracket:unit^]
          [prefix drracket:rep: drracket:rep^]
          [prefix drracket:init: drracket:init^]
          [prefix drracket:module-language-tools: drracket:module-language-tools^]
          [prefix drracket: drracket:interface^])
  (export drracket:module-language/int^)
  
  (define module-language<%>
    (interface ()
      get-users-language-name
      get-language-info))
  
  ;; add-module-language : -> void
  ;; adds the special module-only language to drscheme
  (define (add-module-language)
    (define module-language%
      (module-mixin
       ((drracket:language:get-default-mixin)
        (drracket:language:module-based-language->language-mixin
         (drracket:language:simple-module-based-language->module-based-language-mixin
          drracket:language:simple-module-based-language%)))))
    (drracket:language-configuration:add-language
     (new module-language%)))
  
  ;; collection-paths : (listof (union 'default string))
  ;; command-line-args : (vectorof string)
  ;; auto-text : string
  (define-struct (module-language-settings drracket:language:simple-settings)
    (collection-paths command-line-args auto-text compilation-on? full-trace?))
  
  (define (module-language-settings->prefab-module-settings settings)
    (prefab-module-settings (module-language-settings-command-line-args settings)
                            (module-language-settings-collection-paths settings)
                            (module-language-settings-compilation-on? settings)
                            (module-language-settings-full-trace? settings)
                            (drracket:language:simple-settings-annotations settings)))
  
  (define default-compilation-on? #t)
  (define default-full-trace? #t)
  (define default-auto-text "#lang racket\n")  
  
  ;; module-mixin : (implements drracket:language:language<%>)
  ;;             -> (implements drracket:language:language<%>)
  (define (module-mixin %)
    (class* % (drracket:language:language<%> module-language<%>)
      
      (define language-info #f) ;; a result from module-compiled-language-info
      (define sandbox #f)       ;; a sandbox for querying the language-info
      (define/public (get-language-info key default)
        (init-sandbox)
        (cond
          [(and language-info sandbox)
           (let ([mp (vector-ref language-info 0)]
                 [name (vector-ref language-info 1)]
                 [val (vector-ref language-info 2)])
             (call-in-sandbox-context
              sandbox
              (λ ()
                (parameterize ([current-security-guard drracket:init:system-security-guard])
                  (((dynamic-require mp name) val) key default)))))]
          [else default]))
      (define (init-sandbox)
        (unless sandbox
          (when language-info
            (set! sandbox (make-evaluator 'racket/base)))))
      
      (define/override (first-opened settings)
        (define ns (with-handlers ((exn:fail? (lambda (x) #f)))
                     ;; get-ns can fail in all kinds of strange ways;
                     ;; just give up if it does, since an error here
                     ;; means drracket won't start up.
                     (get-ns (get-auto-text settings))))
        (when ns (current-namespace ns)))
      
      (define/private (get-ns str)
        (define ev (make-evaluator 'racket/base))
        (ev `(parameterize ([read-accept-reader #t])
               (define stx (read-syntax "here" (open-input-string ,str)))
               (define modname
                 (syntax-case stx ()
                   [(module name . stuff)
                    `',(syntax->datum #'name)]
                   [_ #f]))
               (and modname
                    (eval stx)
                    (namespace-require modname)
                    (module->namespace modname)))))
      
      (inherit get-language-name)
      (define/public (get-users-language-name defs-text)
        (let* ([defs-port (open-input-text-editor defs-text)]
               [read-successfully?
                (with-handlers ((exn:fail? (λ (x) #f)))
                  (read-language defs-port (λ () #f))
                  #t)])
          (cond
            [read-successfully?
             (let* ([str (send defs-text get-text 0 (file-position defs-port))]
                    [pos (regexp-match-positions #rx"#(?:!|lang )" str)])
               (cond
                 [(not pos)
                  (get-language-name)]
                 [else
                  ;; newlines can break things (ie the language text won't 
                  ;; be in the right place in the interactions window, which
                  ;; at least makes the test suites unhappy), so get rid of 
                  ;; them from the name. Otherwise, if there is some weird formatting,
                  ;; so be it.
                  (regexp-replace* #rx"[\r\n]+"
                                   (substring str (cdr (car pos)) (string-length str))
                                   " ")]))]
            [else
             (get-language-name)])))
                
      (define/override (use-namespace-require/copy?) #f)
      
      (define/augment (capability-value key)
        (cond
          [(eq? key 'drscheme:autocomplete-words)
           (drracket:language-configuration:get-all-manual-keywords)]
          [else (drracket:language:get-capability-default key)]))
      
      ;; config-panel : as in super class
      ;; uses drracket:language:simple-module-based-language-config-panel
      ;; and adds a collection paths configuration to it.
      (define/override (config-panel parent)
        (module-language-config-panel parent))
      
      ;; NOTE: this method is also used in the super class's implementation
      ;; of default-settings?, which is why the super call is appropriate
      ;; there, even tho these settings are not the same as the defaults
      ;; in other languages (here 'none is the default annotations,
      ;; there you get errortrace annotations).
      (define/override (default-settings)
        (let ([super-defaults (super default-settings)])
          (make-module-language-settings
           #t 'print 'mixed-fraction-e #f #t 'debug;; simple settings defaults 
           
           '(default)
           #()
           default-auto-text
           default-compilation-on?
           default-full-trace?)))
      
      ;; default-settings? : -> boolean
      (define/override (default-settings? settings)
        
        (and (super default-settings? settings)
             
             (equal? (module-language-settings-collection-paths settings)
                     '(default))
             (equal? (module-language-settings-command-line-args settings)
                     #())
             ;; Never show that this is a "custom" language because of the
             ;; auto-text
             ;; (equal? (module-language-settings-auto-text settings)
             ;;         default-auto-text)
             (equal? (module-language-settings-compilation-on? settings)
                     default-compilation-on?)
             (equal? (module-language-settings-full-trace? settings)
                     default-full-trace?)))
      
      (define/override (marshall-settings settings)
        (let ([super-marshalled (super marshall-settings settings)])
          (list super-marshalled
                (module-language-settings-collection-paths settings)
                (module-language-settings-command-line-args settings)
                (module-language-settings-auto-text settings)
                (module-language-settings-compilation-on? settings)
                (module-language-settings-full-trace? settings))))
      
      (define/override (unmarshall-settings marshalled)
        (and (list? marshalled)
             (let ([marshalled-len (length marshalled)])
               ;; older formats had no auto-text or compilation disabling
               (and (<= 3 marshalled-len)
                    (let ([collection-paths (list-ref marshalled 1)]
                          [command-line-args (list-ref marshalled 2)]
                          [auto-text (if (<= marshalled-len 3)
                                         default-auto-text
                                         (list-ref marshalled 3))]
                          [compilation-on? (if (<= marshalled-len 4)
                                               default-compilation-on?
                                               (list-ref marshalled 4))]
                          [full-trace? (if (<= marshalled-len 5)
                                           default-full-trace?
                                           (list-ref marshalled 5))])
                      (and (list? collection-paths)
                           (andmap (λ (x) (or (string? x) (symbol? x)))
                                   collection-paths)
                           (vector? command-line-args)
                           (andmap string? (vector->list command-line-args))
                           (string? auto-text)
                           (boolean? compilation-on?)
                           (let ([super (super unmarshall-settings 
                                               (let ([p (car marshalled)])
                                                 ;; Convert 'write to 'print:
                                                 (if (eq? (vector-ref p 1) 'write)
                                                     (list->vector 
                                                      (list* (vector-ref p 0)
                                                             'print
                                                             (cddr (vector->list p))))
                                                     p)))])
                             (and super
                                  (apply make-module-language-settings
                                         (append
                                          (vector->list (drracket:language:simple-settings->vector super))
                                          (list collection-paths
                                                command-line-args
                                                auto-text
                                                
                                                ;; current versions of drscheme do not allow this combination
                                                ;; in the first place (compilation is only allowed in 'none
                                                ;; and 'debug mode), but older versions might.
                                                (and (memq (drracket:language:simple-settings-annotations super) 
                                                           '(none debug))
                                                     compilation-on?)
                                                
                                                full-trace?)))))))))))
      
      (define/override (on-execute settings run-in-user-thread)
        (super on-execute settings run-in-user-thread)
        
        ;; reset the language info so that if the module is illformed, 
        ;; we don't save the language info from the last run
        (set! language-info #f)
        (set! sandbox #f)
        
        (run-in-user-thread
         (λ ()
           (set-module-language-parameters (module-language-settings->prefab-module-settings settings)
                                           module-language-parallel-lock-client))))
      
      (define/override (get-one-line-summary)
        (string-constant module-language-one-line-summary))
      
      (define/public (get-auto-text settings)
        (module-language-settings-auto-text settings))
      
      ;; utility for the front-end method: return a function that will return
      ;; each of the given syntax values on each call, executing thunks when
      ;; included; when done with the list, send eof.
      (define (expr-getter . exprs/thunks)
        (define (loop)
          (if (null? exprs/thunks)
            eof
            (let ([x (car exprs/thunks)])
              (set! exprs/thunks (cdr exprs/thunks))
              (if (procedure? x) (begin (x) (loop)) x))))
        loop)
      
      (inherit get-reader)
      (define repl-init-thunk (make-thread-cell #f))
      
      (define/override (front-end/complete-program port settings)
        (define (super-thunk) 
          (define reader (get-reader))
          (reader (object-name port) port))
        (define path
          (cond [(get-filename port) => (compose simplify-path cleanse-path)]
                [else #f]))
        (define resolved-modpath (and path (module-path-index-resolve
                                            (module-path-index-join
                                             path
                                             #f))))
        (define-values (name lang module-expr)
          (let ([expr
                 ;; just reading the definitions might be a syntax error,
                 ;; possibly due to bad language (eg, no foo/lang/reader)
                 (with-handlers ([exn:fail? (λ (e)
                    ;; [Eli] FIXME: use `read-language' on `port' after calling
                    ;; `file-position' to reset it to the beginning (need to
                    ;; make sure that it's always a seekable port), then see
                    ;; the position that we're left at, re-read that part of
                    ;; the port (a second reset), construct a string holding
                    ;; the #lang, and read from it an empty module, and extract
                    ;; the base module from it (ask Matthew about this).
                                              (raise-hopeless-exception
                                               e "invalid module text"))])
                   (super-thunk))])
            (when (eof-object? expr)
              (raise-hopeless-syntax-error (string-append
                                            "There must be a valid module in the\n"
                                            "definitions window.  Try starting your program with\n"
                                            "\n"
                                            "  #lang racket\n"
                                            "\n"
                                            "and clicking ‘Run’.")))
            (let ([more (super-thunk)])
              (unless (eof-object? more)
                (raise-hopeless-syntax-error
                 "there can only be one expression in the definitions window"
                 more)))
            (transform-module path expr raise-hopeless-syntax-error)))
        (define modspec (or path `',(syntax-e name)))
        (define (check-interactive-language)
          (unless (memq '#%top-interaction (namespace-mapped-symbols))
            (raise-hopeless-exception
             #f #f ; no error message, just a suffix
             (format "~s does not support a REPL (no #%top-interaction)"
                     (syntax->datum lang)))))
        ;; We're about to send the module expression to drracket now, the rest
        ;; of the setup is done in `front-end/finished-complete-program' below,
        ;; so use `repl-init-thunk' to store an appropriate continuation for
        ;; this setup.  Once we send the expression, we'll be called again only
        ;; if it was evaluated (or expanded) with no errors, so begin with a
        ;; continuation that deals with an error, and if we're called again,
        ;; change it to a continuation that initializes the repl for the
        ;; module.  So the code is split among several thunks that follow.
        (define (*pre)
          (thread-cell-set! repl-init-thunk *error)
          (current-module-declare-name resolved-modpath)
          (current-module-declare-source path))
        (define (*post)
          (current-module-declare-name #f)
          (current-module-declare-source #f)
          (when path ((current-module-name-resolver) resolved-modpath))
          (thread-cell-set! repl-init-thunk *init))
        (define (*error)
          (current-module-declare-name #f)
          (current-module-declare-source #f)
          ;; syntax error => try to require the language to get a working repl
          (with-handlers ([void (λ (e)
                                  (raise-hopeless-syntax-error
                                   "invalid language specification"
                                   lang))])
            (namespace-require lang))
          (check-interactive-language))
        (define (*init)
          (parameterize ([current-namespace (current-namespace)])
            ;; the prompt makes it continue after an error
            (call-with-continuation-prompt
             (λ () (with-stack-checkpoint 
                    (begin
                      (*do-module-specified-configuration)
                      (namespace-require modspec))))))
          (current-namespace (module->namespace modspec))
          (check-interactive-language))
        (define (*do-module-specified-configuration)
          (let ([info (module->language-info modspec #t)])
            (parameterize ([current-eventspace drracket:init:system-eventspace])
              (queue-callback
               (λ () (set! language-info info))))
            (when info
              (let ([get-info
                     ((dynamic-require (vector-ref info 0)
                                       (vector-ref info 1))
                      (vector-ref info 2))])
                (let ([configs (get-info 'configure-runtime '())])
                  (for ([config (in-list configs)])
                    ((dynamic-require (vector-ref config 0)
                                      (vector-ref config 1))
                     (vector-ref config 2))))))))
        ;; here's where they're all combined with the module expression
        (expr-getter *pre module-expr *post))
      
      (define/override (front-end/finished-complete-program settings)
        (cond [(thread-cell-ref repl-init-thunk)
               => (λ (t) (thread-cell-set! repl-init-thunk #f) (t))]))
      
      (define/override (front-end/interaction port settings)
        (λ ()
          (let ([v (parameterize ([read-accept-reader #t]
                                  [read-accept-lang #f])
                     (with-stack-checkpoint
                      ((current-read-interaction) 
                       (object-name port)
                       port)))])
            (if (eof-object? v)
                v
                (let ([w (cons '#%top-interaction v)])
                  (if (syntax? v)
                      (namespace-syntax-introduce
                       (datum->syntax #f w v))
                      v))))))
      
      ;; printer settings are just ignored here.
      (define/override (create-executable setting parent program-filename)
        (let* ([executable-specs (drracket:language:create-executable-gui
                                  parent program-filename #t #t)])
          (when executable-specs
            (let ([executable-type (list-ref executable-specs 0)]
                  [gui? (eq? 'mred (list-ref executable-specs 1))]
                  [executable-filename (list-ref executable-specs 2)])
              (with-handlers ([(λ (x) #f) ;exn:fail?
                               (λ (x)
                                 (message-box
                                  (string-constant drscheme)
                                  (if (exn? x)
                                      (format "~a" (exn-message x))
                                      (format "uncaught exception: ~s" x))))])
                (let ([call-create-embedding-executable
                       (λ (exe-name)
                         (let ([short-program-name
                                (let-values ([(base name dir) (split-path program-filename)])
                                  (path-replace-suffix name #""))])
                           (create-embedding-executable
                            exe-name
                            #:gracket? gui?
                            #:verbose? #f
                            #:modules (list (list #f program-filename))
                            #:configure-via-first-module? #t
                            #:literal-expression
                            (parameterize ([current-namespace (make-base-empty-namespace)])
                              (namespace-require 'racket/base)
                              (compile
                               `(namespace-require '',(string->symbol (path->string short-program-name)))))
                            #:cmdline '("-U" "--"))))])
                  
                  (case executable-type
                    [(launcher)
                     (let ([make-launcher (if gui? make-mred-launcher make-mzscheme-launcher)])
                       (make-launcher (list "-qt-" (path->string program-filename))
                                      executable-filename))]
                    [(distribution)
                     (drracket:language:create-distribution-for-executable
                      executable-filename
                      gui?
                      call-create-embedding-executable)]
                    [(stand-alone)
                     (define c (make-custodian))
                     (define d (new dialog% [parent parent] [label (string-constant create-executable-title)]))
                     (new message% [parent d] [label (string-constant creating-executable-progress-status)])
                     (new button%
                          [parent d]
                          [label (string-constant abort)]
                          [callback (lambda (_1 _2)
                                      (custodian-shutdown-all c))])
                     (define thd
                       (parameterize ([current-custodian c])
                         (thread
                          (λ ()
                            (call-create-embedding-executable executable-filename)))))
                     (thread
                      (λ ()
                        (thread-wait thd)
                        (queue-callback (λ () (send d show #f)))))
                     (send d show #t)])))))))
      
      (super-new
       [module #f]
       [language-position (list (string-constant module-language-name))]
       [language-numbers (list -32768)])))
  
  ;; can be called with #f to just kill the repl (in case we want to kill it
  ;; but keep the highlighting of a previous error)
  (define (raise-hopeless-exception exn [prefix #f] [suffix #f])
    (define rep (drracket:rep:current-rep))
    ;; Throw an error as usual if we don't have the drscheme rep, then we just
    ;; raise the exception as normal.  (It can happen in some rare cases like
    ;; having a single empty scheme box in the definitions.)
    (unless rep (if exn (raise exn) (error "\nInteractions disabled")))
    (when prefix (fprintf (current-error-port) "Module Language: ~a\n" prefix))
    (when exn ((error-display-handler) (exn-message exn) exn))
    ;; these are needed, otherwise the warning can appear before the output
    (flush-output (current-output-port))
    (flush-output (current-error-port))
    ;; do the rep-related work carefully -- using drscheme's eventspace, and
    ;; wait for it to finish before we continue.
    (let ([s (make-semaphore 0)]
          [msg (string-append "\nInteractions disabled"
                              (if suffix (string-append ": " suffix) "."))])
      (parameterize ([current-eventspace drracket:init:system-eventspace])
        (queue-callback
         (λ ()
           (send rep call-without-reset-highlighting
             (λ ()
               (send* rep (insert-warning msg)
                          (set-show-no-user-evaluation-message? #f))))
           (semaphore-post s))))
      (semaphore-wait s))
    (custodian-shutdown-all (send rep get-user-custodian)))
  
  (define (raise-hopeless-syntax-error . error-args)
    (with-handlers ([exn:fail? raise-hopeless-exception])
      (apply raise-syntax-error '|Module Language|
             error-args)))
    
  ;; module-language-config-panel : panel -> (case-> (-> settings) (settings -> void))
  (define (module-language-config-panel parent)
    (define new-parent
      (new vertical-panel%
           [parent parent]
           [alignment '(center center)]
           [stretchable-height #f]
           [stretchable-width #f]))
    (define compilation-on-check-box #f)
    (define compilation-on? #t)
    (define save-stacktrace-on-check-box #f)
    (define left-debugging-radio-box #f)
    (define right-debugging-radio-box #f)
    (define simple-case-lambda
      (drracket:language:simple-module-based-language-config-panel
       new-parent
       #:case-sensitive #t
       
       #:get-debugging-radio-box (λ (rb-l rb-r) 
                                   (set! left-debugging-radio-box rb-l)
                                   (set! right-debugging-radio-box rb-r))
       
       #:debugging-radio-box-callback
       (λ (debugging-radio-box evt)
         (update-compilation-checkbox left-debugging-radio-box right-debugging-radio-box))

       #:dynamic-panel-extras
       (λ (dynamic-panel)
         (set! compilation-on-check-box
               (new check-box%
                    [label (string-constant automatically-compile)]
                    [parent dynamic-panel]
                    [callback
                     (λ (_1 _2) (set! compilation-on? (send compilation-on-check-box get-value)))]))
         (set! save-stacktrace-on-check-box (new check-box%
                                                 [label (string-constant preserve-stacktrace-information)]
                                                 [parent dynamic-panel])))))
    (define (update-compilation-checkbox left-debugging-radio-box right-debugging-radio-box)
      (case (send left-debugging-radio-box get-selection)
        [(0 1)
         (send compilation-on-check-box enable #t)
         (send compilation-on-check-box set-value compilation-on?)]
        [(#f)
         (send compilation-on-check-box enable #f)
         (send compilation-on-check-box set-value #f)]))
    
    (define cp-panel (new group-box-panel%
                          [parent new-parent]
                          [label (string-constant ml-cp-collection-paths)]))
    
    (define args-panel (new group-box-panel%
                            [parent new-parent]
                            [label (string-constant ml-command-line-arguments)]))
    (define args-text-box (new text-field%
                               [parent args-panel]
                               [label #f]
                               [init-value "#()"]
                               [callback void]))
    (define auto-text-panel (new group-box-panel%
                                 [parent new-parent]
                                 [label (string-constant module-language-auto-text)]))
    (define auto-text-text-box (new text-field%
                                    [parent auto-text-panel]
                                    [label #f]
                                    [init-value ""]
                                    [callback void]))
    
    ;; data associated with each item in listbox : boolean
    ;; indicates if the entry is the default paths.
    (define collection-paths-lb (new list-box%
                                     [parent cp-panel]
                                     [choices '("a" "b" "c")]
                                     [label #f]
                                     [callback (λ (x y) (update-buttons))]))
    (define button-panel (new horizontal-panel%
                              [parent cp-panel]
                              [alignment '(center center)]
                              [stretchable-height #f]))
    (define add-button
      (make-object button% (string-constant ml-cp-add) button-panel
        (λ (x y) (add-callback))))
    (define add-default-button
      (make-object button% (string-constant ml-cp-add-default) button-panel
        (λ (x y) (add-default-callback))))
    (define remove-button
      (make-object button% (string-constant ml-cp-remove) button-panel
        (λ (x y) (remove-callback))))
    (define raise-button
      (make-object button% (string-constant ml-cp-raise) button-panel
        (λ (x y) (move-callback -1))))
    (define lower-button
      (make-object button% (string-constant ml-cp-lower) button-panel
        (λ (x y) (move-callback +1))))
    
    (define (update-buttons)
      (let ([lb-selection (send collection-paths-lb get-selection)]
            [lb-tot (send collection-paths-lb get-number)])
        (send remove-button enable lb-selection)
        (send raise-button enable (and lb-selection (not (= lb-selection 0))))
        (send lower-button enable
              (and lb-selection (not (= lb-selection (- lb-tot 1)))))))
    
    (define (add-callback)
      (let ([dir (get-directory (string-constant ml-cp-choose-a-collection-path)
                                (send parent get-top-level-window))])
        (when dir
          (send collection-paths-lb append (path->string dir) #f)
          (update-buttons))))
    
    (define (add-default-callback)
      (cond [(has-default?)
             (message-box (string-constant drscheme)
                          (string-constant ml-cp-default-already-present)
                          (send parent get-top-level-window))]
            [else
             (send collection-paths-lb append (string-constant ml-cp-default-collection-path) #t)
             (update-buttons)]))
    
    ;; has-default? : -> boolean
    ;; returns #t if the `default' entry has already been added
    (define (has-default?)
      (let loop ([n (send collection-paths-lb get-number)])
        (cond [(= n 0) #f]
              [(send collection-paths-lb get-data (- n 1)) #t]
              [else (loop (- n 1))])))
    
    (define (remove-callback)
      (let ([to-delete (send collection-paths-lb get-selection)])
        (send collection-paths-lb delete to-delete)
        (unless (zero? (send collection-paths-lb get-number))
          (send collection-paths-lb set-selection (min to-delete (- (send collection-paths-lb get-number) 1))))
        (update-buttons)))
    
    (define (move-callback d)
      (let* ([sel (send collection-paths-lb get-selection)]
             [vec (get-lb-vector)]
             [new (+ sel d)]
             [other (vector-ref vec new)])
        (vector-set! vec new (vector-ref vec sel))
        (vector-set! vec sel other)
        (set-lb-vector vec)
        (send collection-paths-lb set-selection new)
        (update-buttons)))
    
    (define (get-lb-vector)
      (list->vector (for/list ([n (in-range (send collection-paths-lb get-number))])
                              (cons (send collection-paths-lb get-string n)
                                    (send collection-paths-lb get-data n)))))
    
    (define (set-lb-vector vec)
      (send collection-paths-lb clear)
      (for ([x (in-vector vec)] [n (in-naturals)])
           (send collection-paths-lb append (car x))
           (send collection-paths-lb set-data n (cdr x))))
    
    (define (get-collection-paths)
      (for/list ([n (in-range (send collection-paths-lb get-number))])
                (let ([data (send collection-paths-lb get-data n)])
                  (if data 'default (send collection-paths-lb get-string n)))))
    
    (define (install-collection-paths paths)
      (send collection-paths-lb clear)
      (for ([cp paths])
           (if (symbol? cp)
               (send collection-paths-lb append (string-constant ml-cp-default-collection-path) #t)
               (send collection-paths-lb append cp #f))))
    
    (define (get-command-line-args)
      (let* ([str (send args-text-box get-value)]
             [read-res (parameterize ([read-accept-graph #f])
                         (with-handlers ([exn:fail:read? (λ (x) #())])
                           (read (open-input-string str))))])
        (if (and (vector? read-res) (andmap string? (vector->list read-res)))
            read-res
            #())))
    
    (define (install-command-line-args vec)
      (send args-text-box set-value
            (parameterize ([print-vector-length #f])
              (format "~s" vec))))
    
    (define (get-auto-text)
      (let ([str (send auto-text-text-box get-value)])
        (cond
          [(equal? str "") ""]
          [else (string-append str "\n")])))
    
    (define (install-auto-text str)
      (send auto-text-text-box set-value (regexp-replace #rx"\n$" str "")))
    
    (install-collection-paths '(default))
    (update-buttons)
    (install-auto-text default-auto-text)
    (update-compilation-checkbox left-debugging-radio-box right-debugging-radio-box)
    
    (case-lambda
      [()
       (let ([simple-settings (simple-case-lambda)])
         (apply make-module-language-settings
                (append 
                 (vector->list (drracket:language:simple-settings->vector simple-settings))
                 (list (get-collection-paths)
                       (get-command-line-args)
                       (get-auto-text)
                       (case (send left-debugging-radio-box get-selection)
                         [(0 1) compilation-on?]
                         [(#f) #f])
                       (send save-stacktrace-on-check-box get-value)))))]
      [(settings)
       (simple-case-lambda settings)
       (install-collection-paths (module-language-settings-collection-paths settings))
       (install-command-line-args (module-language-settings-command-line-args settings))
       (install-auto-text (module-language-settings-auto-text settings))
       (set! compilation-on? (module-language-settings-compilation-on? settings))
       (send compilation-on-check-box set-value (module-language-settings-compilation-on? settings))
       (update-compilation-checkbox left-debugging-radio-box right-debugging-radio-box)
       (send save-stacktrace-on-check-box set-value (module-language-settings-full-trace? settings))
       (update-buttons)]))
  
  ;; get-filename : port -> (union string #f)
  ;; extracts the file the definitions window is being saved in, if any.
  (define (get-filename port)
    (let ([source (object-name port)])
      (cond
        [(path? source) source]
        [(is-a? source text%)
         (let ([canvas (send source get-canvas)])
           (and canvas
                (let ([frame (send canvas get-top-level-window)])
                  (and (is-a? frame drracket:unit:frame%)
                       (let* ([b (box #f)]
                              [filename (send (send frame get-definitions-text)
                                              get-filename
                                              b)])
                         (if (unbox b)
                             #f
                             filename))))))]
        [else #f])))
  
  (define-local-member-name
    show-bkg-running
    frame-show-bkg-running
    restart-place
    set-expand-error
    update-frame-expand-error
    expand-error-next
    expand-error-prev)
  
  (define module-language-online-expand-tab-mixin
    (mixin (drracket:unit:tab<%>) ()
      (inherit get-frame)
      (define bkg-label "")
      (define bkg-colors '())
      (define bkg-state 'nothing)
     
      (define/public (add-bkg-running-color id color label)
        (set! bkg-colors
              (sort
               (cons (list id color label) bkg-colors)
               string<=? #:key (compose symbol->string car))))
      
      (define/public (remove-bkg-running-color id)
        (set! bkg-colors (filter (λ (x) (not (eq? (car x) id))) bkg-colors))
        (send (get-frame) frame-show-bkg-running (get-colors) (get-label)))
      
      (define/public (get-bkg-running)
        (values (get-colors) (get-label)))
      
      (define/public (show-bkg-running state label)
        (set! bkg-state state)
        (set! bkg-label label)
        (send (get-frame) frame-show-bkg-running (get-colors) (get-label)))
      
      (define/private (get-colors)
        (case bkg-state
          [(reader-in-defs-error) 'parens]
          [(running) (list "blue")]
          [(nothing) (if (null? bkg-colors)
                         #f
                         (map (λ (x) (list-ref x 1)) bkg-colors))]
          [(failed) (list "red")]
          [else (error 'show-bkg-running "unknown state ~s\n" bkg-state)]))
      
      (define/private (get-label)
        (if (eq? bkg-state 'nothing)
            (if (null? bkg-colors)
                #f
                (map (λ (x) (list-ref x 2)) bkg-colors))
            (list bkg-label)))
      
      (super-new)))
  
  (define module-language-online-expand-frame-mixin
    (mixin (frame:basic<%> frame:info<%> drracket:unit:frame<%>) ()
      (inherit get-info-panel get-current-tab)
      
      (define expand-error-parent-panel #f)
      (define expand-error-panel #f)
      (define expand-error-message #f)
      (define expand-error-button-parent-panel #f)
      (define expand-error-single-child #f)
      (define expand-error-multiple-child #f)

      ;; colors : (or/c #f (listof string?) 'parens)
      (define colors #f)
      (define tooltip-labels #f)
      
      (super-new)
      
      (define/override (make-root-area-container cls parent)
        (set! expand-error-parent-panel
              (super make-root-area-container vertical-panel% parent))
        (define root (make-object cls expand-error-parent-panel))
        (set! expand-error-panel
              (new horizontal-panel% 
                   [stretchable-height #f]
                   [parent expand-error-parent-panel]))
        
        (set! expand-error-message (new error-message% [parent expand-error-panel]
                                        [stretchable-width #t]
                                        [msg "hi"]))
        (set! expand-error-button-parent-panel
              (new vertical-panel% 
                   [stretchable-width #f]
                   [stretchable-height #f]
                   [parent expand-error-panel]))
        (set! expand-error-single-child
              (new button% 
                   [parent expand-error-button-parent-panel]
                   [stretchable-width #t]
                   [label sc-jump-to-error]
                   [font small-control-font]
                   [callback (λ (b evt) (send (send (get-current-tab) get-defs) expand-error-next))]))
        (set! expand-error-multiple-child
              (new horizontal-panel% [parent expand-error-button-parent-panel]))
        (new button% 
             [label "<"]
             [font small-control-font]
             [callback (λ (b evt) (send (send (get-current-tab) get-defs) expand-error-prev))]
             [parent expand-error-multiple-child])
        (new message% 
             [parent expand-error-multiple-child]
             [label sc-jump-to-error])
        (new button% 
             [label ">"]
             [font small-control-font]
             [callback (λ (b evt) (send (send (get-current-tab) get-defs) expand-error-next))]
             [parent expand-error-multiple-child])
        (send expand-error-button-parent-panel change-children (λ (l) (list expand-error-single-child)))
        (send expand-error-parent-panel change-children (λ (l) (remq expand-error-panel l)))
        root)
      
      (define expand-error-msg #f)
      (define expand-error-srcloc-count 0)
      
      (define/public (set-expand-error msg srcloc-count)
        (unless (and (equal? expand-error-msg msg)
                     (equal? expand-error-srcloc-count srcloc-count))
          (set! expand-error-msg msg)
          (set! expand-error-srcloc-count srcloc-count)
          (cond
            [expand-error-msg
             (send expand-error-message set-msg expand-error-msg)
             (send expand-error-parent-panel change-children
                   (λ (l) (append (remq expand-error-panel l) (list expand-error-panel))))
             (send expand-error-button-parent-panel change-children
                   (λ (l) (cond
                            [(= srcloc-count 0) '()]
                            [(= srcloc-count 1)
                             (list expand-error-single-child)]
                            [else
                             (list expand-error-multiple-child)])))]
            [else
             (send expand-error-parent-panel change-children
                   (λ (l) (remq expand-error-panel l)))])))
      
      (define/augment (on-tab-change from-tab to-tab)
        (send (send to-tab get-defs) restart-place)
        (send (send to-tab get-defs) update-frame-expand-error)
        (inner (void) on-tab-change from-tab to-tab))

      (define/override (on-activate active?)
        (define defs (send (get-current-tab) get-defs))
        (when active? 
          (send defs restart-place))
        (super on-activate active?))
      
      (define/public (frame-show-bkg-running new-colors labels)
        (unless (equal? tooltip-labels labels)
          (set! tooltip-labels labels)
          (update-tooltip))
        (unless (equal? new-colors colors)
          (set! colors new-colors)
          (send running-canvas refresh)))
     
      (define tooltip-frame #f)
      (define/private (show-tooltip)
        (cond
          [tooltip-labels
           (unless tooltip-frame
             (set! tooltip-frame (new tooltip-frame%)))
           (send tooltip-frame set-tooltip tooltip-labels)
           (define-values (rx ry) (send running-canvas client->screen 0 0))
           (define-values (cw ch) (send running-canvas get-client-size))
           (send tooltip-frame show-over rx ry cw ch #:prefer-upper-left? #t)]
          [else
           (when tooltip-frame
             (send tooltip-frame show #f))]))
      (define/private (update-tooltip)
        (when tooltip-frame
          (cond
            [tooltip-labels
             (when (send tooltip-frame is-shown?)
               ;; just call this, as it updates the tooltip label already
               (show-tooltip))]
            [else
             (send tooltip-frame show #f)])))
      (define/private (hide-tooltip)
        (when tooltip-frame
          (send tooltip-frame show #f)))

      (define parens-mismatch-str "())")
      (define ball-size 10)
      (define parens-mismatch-font
        (send the-font-list find-or-create-font
              (send small-control-font get-point-size)
              (send small-control-font get-face)
              (send small-control-font get-family)
              (send small-control-font get-style)
              'bold
              (send small-control-font get-underlined)
              (send small-control-font get-smoothing)
              (send small-control-font get-size-in-pixels)))
              
      (define running-canvas
        (new (class canvas%
               (inherit get-dc popup-menu refresh get-client-size)
               (define/override (on-paint)
                 (let ([dc (get-dc)])
                   (when colors
                     (send dc set-smoothing 'aligned)
                     (send dc set-pen "black" 1 'transparent)
                     (send dc set-text-foreground "darkred")
                     (send dc set-font parens-mismatch-font)
                     (define-values (tw th td ta) (send dc get-text-extent parens-mismatch-str))
                     (define-values (cw ch) (get-client-size))
                     (cond
                       [(list? colors)
                        (define len (length colors))
                        (for ([color (in-list colors)]
                              [i (in-naturals)])
                          (send dc set-brush color 'solid)
                          (send dc draw-arc 
                                (- (/ cw 2) (/ ball-size 2))
                                (- (/ ch 2) (/ ball-size 2))
                                ball-size ball-size
                                (* 2 pi (/ i len))
                                (* 2 pi (/ (+ i 1) len))))]
                       [(eq? colors 'parens)
                        (send dc draw-text parens-mismatch-str 
                              (- (/ cw 2) (/ tw 2))
                              (- (/ ch 2) (/ th 2)))]))))
               (define cb-proc (λ (sym new-val)
                                 (set! colors #f)
                                 (refresh)))
               (preferences:add-callback 'drracket:online-compilation cb-proc #t)
               (define/override (on-event evt) 
                 (cond
                   [(send evt button-down?)
                    (define menu (new popup-menu%))
                    (define on? (preferences:get 'drracket:online-compilation))
                    (new menu-item% 
                         [parent menu]
                         [label (if on?
                                    "Disable online compilation"
                                    "Enable online compilation")]
                         [callback
                          (λ args
                            (preferences:set 'drracket:online-compilation (not on?)))])
                    (popup-menu menu (send evt get-x) (send evt get-y))]
                   [(send evt entering?)
                    (show-tooltip)]
                   [(send evt leaving?)
                    (hide-tooltip)]))
               (super-new [style '(transparent)]
                          [parent (get-info-panel)]
                          [stretchable-width #f]
                          [stretchable-height #f]
                          [min-width 10]
                          [min-height 10])
               
               (inherit min-width min-height)
               (let ([dc (get-dc)])
                 (send dc set-font parens-mismatch-font)
                 (define-values (w h d a) (send dc get-text-extent parens-mismatch-str))
                 (min-width (ceiling (inexact->exact (max w ball-size))))
                 (min-height (ceiling (inexact->exact (max h ball-size))))))))))
  
  (define error-message%
    (class canvas%
      (init-field msg)
      (inherit refresh get-dc get-client-size)
      (define/public (set-msg m) 
        (set! msg m)
        (refresh))
      (define/override (on-paint)
        (define dc (get-dc))
        (define-values (cw ch) (get-client-size))
        (send dc set-font error-font)
        (define-values (tw th td ta) (send dc get-text-extent msg))
        (send dc set-text-foreground "firebrick")
        (send dc draw-text msg 2 (- (/ ch 2) (/ th 2))))
      (super-new [style '(transparent)])
      
      (inherit min-height)
      (let ()
        (send (get-dc) set-font error-font)
        (define-values (tw th td ta) (send (get-dc) get-text-extent msg))
        (min-height (inexact->exact (ceiling th))))))
  
  (define error-font
    (let ([base-font normal-control-font])
      (send the-font-list find-or-create-font
            (send normal-control-font get-point-size)
            (send normal-control-font get-family)
            'italic
            (send normal-control-font get-weight)
            (send normal-control-font get-underlined)
            (send normal-control-font get-smoothing)
            (send normal-control-font get-size-in-pixels))))
        
  (define yellow-message%
    (class canvas%
      (inherit get-dc refresh get-client-size
               min-width min-height
               get-parent)
      (define labels '(""))
      (define/public (set-lab _ls) 
        (unless (equal? labels _ls)
          (set! labels _ls)
          (update-size)
          (refresh)))
      (define/private (update-size)
        (define dc (get-dc))
        (send dc set-font small-control-font)
        (define-values (w h _1 _2) (send dc get-text-extent (car labels)))
        (send (get-parent) begin-container-sequence)
        (min-width (+ 5 (inexact->exact (ceiling w))))
        (min-height (+ 5 (* (length labels) (inexact->exact (ceiling h)))))
        (send (get-parent) end-container-sequence)
        (send (get-parent) reflow-container))
      (define/override (on-paint)
        (define dc (get-dc))
        (send dc set-font small-control-font)
        (define-values (w h) (get-client-size))
        (define-values (tw th _1 _2) (send dc get-text-extent (car labels)))
        (send dc set-pen "black" 1 'transparent)
        (send dc set-brush "LemonChiffon" 'solid)
        (send dc set-pen "black" 1 'solid)
        (send dc draw-rectangle 0 0 (- w 1) (- h 1))
        (for ([label (in-list labels)]
              [i (in-naturals)])
          (send dc draw-text label 2 (+ 2 (* i th)))))
      (super-new [stretchable-width #f] [stretchable-height #f])))

  (define expanding-place
    (and (>= (processor-count) 1)
         (dynamic-place expanding-place.rkt 'start)))
  (define place-initialized? #f)
  (define pending-thread #f)
  
  (define (send-to-place editor-contents filename prefab-module-settings show-results)
    (when expanding-place
      (unless place-initialized?
        (set! place-initialized? #t)
        (place-channel-put expanding-place module-language-compile-lock)
        (place-channel-put
         expanding-place 
         (for/list ([o-e-h (in-list (drracket:module-language-tools:get-online-expansion-handlers))])
           (list (drracket:module-language-tools:online-expansion-handler-mod-path o-e-h)
                 (drracket:module-language-tools:online-expansion-handler-id o-e-h)))))
      (set! pending-thread
            (thread (λ () 
                      (define-values (pc-in pc-out) (place-channel))
                      (define to-send
                        (vector-immutable editor-contents
                                          filename
                                          pc-in 
                                          prefab-module-settings))
                      (place-channel-put expanding-place to-send)
                      (define res (place-channel-get pc-out))
                      (when res
                        (let ([t (current-thread)])
                          (queue-callback
                           (λ ()
                             (when (eq? t pending-thread)
                               (set! pending-thread #f)
                               (show-results res)))))))))))
  
  (define (stop-place-running)
    (when expanding-place
      (when pending-thread
        (place-channel-put expanding-place 'abort)
        (set! pending-thread #f))))
  
  (struct error-range (start end [clear-highlight #:mutable]))
  
  (define online-compilation-error-pen-width 8)
  
  (define module-language-online-expand-text-mixin 
    (mixin (text:basic<%> 
            drracket:unit:definitions-text<%>
            drracket:module-language-tools:definitions-text<%>) ()
      (inherit last-position find-first-snip get-top-level-window get-filename
               get-tab get-canvas invalidate-bitmap-cache 
               set-position get-start-position get-end-position
               highlight-range dc-location-to-editor-location)
      
      (define compilation-out-of-date? #f)
      
      (define tmr (new timer% [notify-callback (lambda () (send-off))]))
      
      (define cb-proc (λ (sym new-val) 
                        (when new-val
                          (queue-callback (λ () (buffer-modified))))))
      (preferences:add-callback 'drracket:online-compilation cb-proc #t)
      
      (define/private (buffer-modified)
        (clear-old-error)
        (reset-frame-expand-error)
        (let ([tlw (get-top-level-window)])
          (when expanding-place
            (when (in-module-language tlw)
              (send (get-tab) show-bkg-running 'nothing #f)
              (stop-place-running)
              (set! compilation-out-of-date? #t)
              (when (preferences:get 'drracket:online-compilation)
                (when (eq? (send tlw get-current-tab) (get-tab))
                  (send tmr stop)
                  (send tmr start 250 #t)))))))
      
      (define/public (restart-place)
        (stop-place-running)
        (when compilation-out-of-date?
          (send tmr start 250 #t)))
      
      (define/private (send-off)
        (define tlw (get-top-level-window))
        ;; make sure the frame's current tab is still this one
        ;; (we may get #f for the tlw when the tab has been switched)
        (when (and tlw (eq? (send tlw get-current-tab) (get-tab)))
          (define settings (in-module-language tlw))
          (when settings
            (define-values (editor-contents filename) (fetch-data-to-send))
            (when editor-contents
              (send-to-place editor-contents
                             filename
                             (module-language-settings->prefab-module-settings settings)
                             (λ (res) (show-results res)))
              (when status-line-open?
                (clear-old-error)
                (reset-frame-expand-error))
              (send (get-tab) show-bkg-running 'running sc-online-expansion-running)))))
      
      (define/private (fetch-data-to-send)
        (define str (make-string (last-position) #\space))
        (let/ec k
          (let loop ([s (find-first-snip)]
                     [i 0])
            (cond
              [(not s) (void)]
              [(is-a? s string-snip%)
               (define size (send s get-count))
               (send s get-text! str 0 size i)
               (loop (send s next) (+ i size))]
              [else
               (send (get-tab) show-bkg-running 'failed sc-only-raw-text-files-supported)
               (k #f #f)]))
          (define fn (let* ([b (box #f)]
                            [n (get-filename b)])
                       (and (not (unbox b))
                            n)))
          (values str fn)))
      
      (define status-line-open? #f)
      
      (define error-message-str #f)
      (define error-message-srclocs '())
      (define/private (reset-frame-expand-error)
        (unless (and (eq? error-message-str #f)
                     (eq? error-message-srclocs '()))
          (set! error-message-str #f)
          (set! error-message-srclocs '())
          (update-frame-expand-error)))
      (define/public (update-frame-expand-error)
        (send (send (get-tab) get-frame) set-expand-error
              error-message-str 
              (length error-message-srclocs)))
      
      (define/public (expand-error-next) 
        (define candidates (filter (λ (error-message-srcloc) 
                                     (> (- (vector-ref error-message-srcloc 0) 1) 
                                        (get-end-position)))
                                   error-message-srclocs))
        (cond
          [(null? candidates)
           (unless (null? error-message-srclocs)
             (jump-to (car error-message-srclocs)))]
          [else
           (jump-to (car candidates))]))
      
      (define/public (expand-error-prev) 
        (define candidates (filter (λ (error-message-srcloc) 
                                     (< (+ (vector-ref error-message-srcloc 0)
                                           (vector-ref error-message-srcloc 1)
                                           -1)
                                        (get-start-position)))
                                   error-message-srclocs))
        (cond
          [(null? candidates)
           (unless (null? error-message-srclocs)
             (jump-to (last error-message-srclocs)))]
          [else
           (jump-to (last candidates))]))
      
      (define/private (jump-to vec)
        (set-position (- (vector-ref vec 0) 1))
        (define cnvs (get-canvas))
        (when cnvs (send cnvs focus)))
      
      (define/private (show-results res)
        (set! compilation-out-of-date? #f)
        (case (vector-ref res 0)
          [(exn)
           (define tlw (send (get-tab) get-frame))
           (send (get-tab) show-bkg-running 'nothing #f)
           (set! error-message-str (vector-ref res 1))
           (set! error-message-srclocs (vector-ref res 2))
           (set! error-ranges 
                 (for/list ([range (in-list (vector-ref res 2))])
                   (define pos (vector-ref range 0))
                   (define span (vector-ref range 1))
                   (error-range (- pos 1) (+ pos span -1) #f)))
           ;; should really only invalidate the appropriate region here (and in clear-error-ranges)
           (invalidate-bitmap-cache 0 0 'display-end 'display-end)
           (update-frame-expand-error)]
          [(access-violation)
           (send (get-tab) show-bkg-running 'failed (gui-utils:format-literal-label "~a" (vector-ref res 1)))
           (clear-old-error)
           (reset-frame-expand-error)]
          [(reader-in-defs-error)
           (send (get-tab) show-bkg-running 'reader-in-defs-error
                 (gui-utils:format-literal-label "~a" (vector-ref res 1)))
           (clear-old-error)
           (reset-frame-expand-error)]
          [(abnormal-termination)
           (send (get-tab) show-bkg-running 'failed sc-abnormal-termination)
           (clear-old-error)
           (reset-frame-expand-error)]
          [(no-errors)
           (send (get-tab) show-bkg-running 'nothing #f)
           (clear-old-error)
           (reset-frame-expand-error)]
          [(handler-results)
           (clear-old-error)
           (reset-frame-expand-error)
           ;; inform the installed handlers that something has come back
           (for ([key-val (in-list (vector-ref res 1))])
             (define that-key (list-ref key-val 0))
             (define val (list-ref key-val 1))
             (for ([o-e-h (in-list (drracket:module-language-tools:get-online-expansion-handlers))])
               (define this-key (list (drracket:module-language-tools:online-expansion-handler-mod-path o-e-h)
                                      (drracket:module-language-tools:online-expansion-handler-id o-e-h)))
               (when (equal? this-key that-key)
                 ((drracket:module-language-tools:online-expansion-handler-local-handler o-e-h) this val))))
           (send (get-tab) show-bkg-running 'nothing #f)]
          [else
           (error 'module-language.rkt "unknown response from the expanding place: ~s\n" res)]))
      
      
      (define error-ranges '())
      (define/private (clear-old-error)
        (for ([an-error-range (in-list error-ranges)])
          (when (error-range-clear-highlight an-error-range)
            ((error-range-clear-highlight an-error-range))
            (set-error-range-clear-highlight! an-error-range #f)))
        (set! error-ranges '())
        (invalidate-bitmap-cache 0 0 'display-end 'display-end))
      
      (define byt (box 0.0))
      (define byb (box 0.0))
      (define vbx (box 0.0))
      (define vby (box 0.0))
      (define vbw (box 0.0))
      (define vbh (box 0.0))
    
      (inherit position-location get-admin)
      
      (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
        (super on-paint before? dc left top right bottom dx dy draw-caret)
        (unless before?
          (define saved-brush (send dc get-brush))
          (define saved-pen (send dc get-pen))
          (define smoothing (send dc get-smoothing))
          (send dc set-smoothing 'smoothed)
          
          (define path (new dc-path%))
          (send dc set-brush "black" 'transparent)
          (send dc set-pen (send the-pen-list find-or-create-pen "red"
                                 online-compilation-error-pen-width
                                 'solid 'butt 'miter))
          (send dc set-alpha 
                (if (preferences:get 'framework:white-on-black?)
                    .5
                    .25))
          
          (for ([an-error-range (in-list error-ranges)])
            (define-values (x1 y1 x2 y2 x3 y3 x4 y4) (get-box an-error-range))
            
            (send path move-to (+ dx x2) (+ dy y2))
            (send path line-to (+ dx x3) (+ dy y3))
            (send path close)
            
            (send path ellipse (+ dx (- x2 4)) (+ dy y2) 4 4)
            (send path ellipse (+ dx (- x2 4)) (+ dy y3) 4 4))
          
          (send dc draw-path path)
          (send dc set-alpha 1)
          (send dc set-brush saved-brush)
          (send dc set-pen saved-pen)
          (send dc set-smoothing smoothing)))
      
      (define/override (on-event evt)
        (define-values (mx my) 
          (dc-location-to-editor-location 
           (send evt get-x)
           (send evt get-y)))
        (cond
          [(or (send evt moving?)
               (send evt entering?))
           (for ([an-error-range (in-list error-ranges)])
             (define-values (x1 y1 x2 y2 x3 y3 x4 y4) (get-box an-error-range))
             (cond
               [(and (<= x1 mx x2)
                     (<= y2 my y3))
                (unless (error-range-clear-highlight an-error-range)
                  (set-error-range-clear-highlight! 
                   an-error-range
                   (highlight-range (error-range-start an-error-range)
                                    (error-range-end an-error-range)
                                    "pink")))]
               [else
                (when (error-range-clear-highlight an-error-range)
                  ((error-range-clear-highlight an-error-range))
                  (set-error-range-clear-highlight! an-error-range #f))]))
           (super on-event evt)]
          [(send evt leaving?)
           (for ([an-error-range (in-list error-ranges)])
             (when (error-range-clear-highlight an-error-range)
               ((error-range-clear-highlight an-error-range))
               (set-error-range-clear-highlight! an-error-range #f)))
           (super on-event evt)]
          [(send evt button-down? 'left)
           (define used-click? #f)
           (for ([an-error-range (in-list error-ranges)])
             (define-values (x1 y1 x2 y2 x3 y3 x4 y4) (get-box an-error-range))
             (when (and (<= x1 mx x2)
                        (<= y2 my y3))
               (set! used-click? #t)
               (set-position (error-range-start an-error-range))))
           (unless used-click?
             (super on-event evt))]
          [else
           (super on-event evt)]))
      
      (define/private (get-box an-error-range)
        (define start-pos (error-range-start an-error-range))
        (define end-pos (error-range-end an-error-range))
        (position-location start-pos #f byt)
        (position-location end-pos #f byb #f)
        (send (get-admin) get-view vbx vby vbw vbh)
        
        (define x2 (+ (unbox vbx) (unbox vbw) 
                      (- (/ online-compilation-error-pen-width 2))
                      (- (/ online-compilation-error-pen-width 1))))
        (define y2 (+ (unbox byt)))
        
        (define x1 (+ x2 (- (/ online-compilation-error-pen-width 1))))
        (define y1 y2)
        
        (define x3 x2)
        (define y3 (unbox byb))
        
        (define x4 x1)
        (define y4 y3)
        
        (values x1 y1 x2 y2 x3 y3 x4 y4))
        
      (define/override (move-to-new-language)
        ;; this is here to get things running for the initital tab in a new frame
        (super move-to-new-language)
        (buffer-modified))
      
      (define/augment (after-insert start end) 
        (buffer-modified)
        (inner (void) after-insert start end))
      
      (define/augment (after-delete start end) 
        (buffer-modified)
        (inner (void) after-delete start end))
      
      (define/augment (after-load-file success?)
        (buffer-modified)
        (inner (void) after-load-file success?))
      
      (super-new)))

    
  (define module-language-put-file-mixin
    (mixin (text:basic<%>) ()
      (inherit get-text last-position get-character get-top-level-window)

      (define/override (put-file directory default-name)
        (let ([tlw (get-top-level-window)])
          (if (in-module-language tlw)
              (let ([module-default-filename (get-module-filename)])
                (super put-file directory module-default-filename))
              (super put-file directory default-name))))
      
      ;; returns the name after "(module " suffixed with .scm
      ;; in the beginning of the editor
      ;; or #f if the beginning doesn't match "(module "
      (define/private (get-module-filename)
        (let ([open-paren (skip-whitespace 0)])
          (or (match-paren open-paren "(")
              (match-paren open-paren "[")
              (match-paren open-paren "{"))))
      
      (define/private (match-paren open-paren paren)
        (and (matches open-paren paren)
             (let ([module (skip-whitespace (+ open-paren 1))])
               (and (matches module "module")
                    (let* ([end-module (+ module (string-length "module"))]
                           [filename-start (skip-whitespace end-module)]
                           [filename-end (skip-to-whitespace filename-start)])
                      (and (not (= filename-start end-module))
                           (string-append (get-text filename-start filename-end)
                                          ".rkt")))))))
      
      
      (define/private (matches start string)
        (let ([last-pos (last-position)])
          (let loop ([i 0])
            (cond
              [(and (i . < . (string-length string))
                    ((+ i start) . < . last-pos))
               (and (char=? (string-ref string i)
                            (get-character (+ i start)))
                    (loop (+ i 1)))]
              [(= i (string-length string)) #t]
              [else #f]))))
      
      (define/private (skip-whitespace start)
        (let ([last-pos (last-position)])
          (let loop ([pos start])
            (cond
              [(pos . >= . last-pos) last-pos]
              [else
               (let ([char (get-character pos)])
                 (cond
                   [(char-whitespace? char)
                    (loop (+ pos 1))]
                   [else pos]))]))))
      
      (define/private (skip-to-whitespace start)
        (let ([last-pos (last-position)])
          (let loop ([pos start])
            (cond 
              [(pos . >= . last-pos)
               last-pos]
              [(char-whitespace? (get-character pos))
               pos]
              [else
               (loop (+ pos 1))]))))
      
      (super-new)))
  
  (define module-language-compile-lock (make-compile-lock))
  
  (define module-language-parallel-lock-client
    (compile-lock->parallel-lock-client
     module-language-compile-lock
     (current-custodian)))

  ;; in-module-language : top-level-window<%> -> module-language-settings or #f
  (define (in-module-language tlw)
    (and tlw
         (is-a? tlw drracket:unit:frame<%>)
         (let ([settings (send (send tlw get-definitions-text) get-next-settings)])
           (and (is-a? (drracket:language-configuration:language-settings-language settings)
                       module-language<%>)
                (drracket:language-configuration:language-settings-settings settings))))))
