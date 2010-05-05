#lang racket/base

(provide module-language@)
(require scheme/unit
         racket/class
         racket/list
         racket/path
         racket/contract
         mred
         compiler/embed
         compiler/cm
         launcher
         framework
         string-constants
         planet/config
         "drsig.rkt"
         "rep.rkt")

(define op (current-output-port))
(define (oprintf . args) (apply fprintf op args))

(define-unit module-language@
  (import [prefix drracket:language-configuration: drracket:language-configuration/internal^]
          [prefix drracket:language: drracket:language^]
          [prefix drracket:unit: drracket:unit^]
          [prefix drracket:rep: drracket:rep^]
          [prefix drracket:init: drracket:init^])
  (export drracket:module-language^)
  
  (define module-language<%>
    (interface ()
      get-users-language-name))
  
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
  
  (define default-compilation-on? #t)
  (define default-full-trace? #t)
  (define default-auto-text "#lang racket\n")  
  
  ;; module-mixin : (implements drracket:language:language<%>)
  ;;             -> (implements drracket:language:language<%>)
  (define (module-mixin %)
    (class* % (drracket:language:language<%> module-language<%>)
      
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
                  ;; them from the name. Otherwise, if there is some wierd formatting,
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
           #t 'write 'mixed-fraction-e #f #t 'debug;; simple settings defaults 
           
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
                (module-language-settings-compilation-on? settings))))
      
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
                           (let ([super (super unmarshall-settings (car marshalled))])
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
        (run-in-user-thread
         (λ ()
           (current-command-line-arguments
            (module-language-settings-command-line-args settings))
           (let* ([default (current-library-collection-paths)]
                  [cpaths (append-map (λ (x) (if (symbol? x) default (list x)))
                                      (module-language-settings-collection-paths
                                       settings))])
             (when (null? cpaths)
               (fprintf (current-error-port)
                        "WARNING: your collection paths are empty!\n"))
             (current-library-collection-paths cpaths))
           
           (compile-context-preservation-enabled (module-language-settings-full-trace? settings))
           
           (when (module-language-settings-compilation-on? settings)
             
             (let ([annotations (drracket:language:simple-settings-annotations settings)])
               (case annotations
                 [(none)
                  (use-compiled-file-paths
                   (cons (build-path "compiled" "drscheme")
                         (use-compiled-file-paths)))]
                 [(debug)
                  (use-compiled-file-paths
                   (cons (build-path "compiled" "drscheme" "errortrace")
                         (use-compiled-file-paths)))]))
             
             (current-load/use-compiled (make-compilation-manager-load/use-compiled-handler))
             (manager-skip-file-handler
              (λ (p) (file-stamp-in-paths 
                      p
                      (cons (CACHE-DIR) (current-library-collection-paths)))))))))
      
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
        (define (super-thunk) ((get-reader) (object-name port) port))
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
                 (with-handlers ([exn? (λ (e)
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
            (when (eof-object? expr) (raise-hopeless-syntax-error))
            (let ([more (super-thunk)])
              (unless (eof-object? more)
                (raise-hopeless-syntax-error
                 "there can only be one expression in the definitions window"
                 more)))
            (transform-module path expr)))
        (define modspec (or path `',(syntax-e name)))
        (define (check-interactive-language)
          (unless (memq '#%top-interaction (namespace-mapped-symbols))
            (raise-hopeless-exception
             #f #f ; no error message, just a suffix
             (format "~s does not support a REPL (no #%top-interaction)"
                     (syntax->datum lang)))))
        ;; We're about to send the module expression to drscheme now, the rest
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
                      (*do-module-specified-configuration modspec)
                      (namespace-require modspec))))))
          (current-namespace (module->namespace modspec))
          (check-interactive-language))
        (define (*do-module-specified-configuration modspec)
          (let ([info (module->language-info modspec #t)])
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
          (let ([v (parameterize ([read-accept-reader #t])
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
            (let ([launcher? (eq? 'launcher (car executable-specs))]
                  [gui? (eq? 'mred (cadr executable-specs))]
                  [executable-filename (caddr executable-specs)])
              (with-handlers ([(λ (x) #f) ;exn:fail?
                               (λ (x)
                                 (message-box
                                  (string-constant drscheme)
                                  (if (exn? x)
                                      (format "~a" (exn-message x))
                                      (format "uncaught exception: ~s" x))))])
                (if (not launcher?)
                    (let ([short-program-name
                           (let-values ([(base name dir) (split-path program-filename)])
                             (path-replace-suffix name #""))])
                      ((if (eq? 'distribution (car executable-specs))
                           drracket:language:create-distribution-for-executable
                           (lambda (executable-filename gui? make)
                             (make executable-filename)))
                       executable-filename
                       gui?
                       (lambda (exe-name)
                         (create-embedding-executable
                          exe-name
                          #:mred? gui?
                          #:verbose? #f ;; verbose?
                          #:modules (list (list #f program-filename))
                          #:configure-via-first-module? #t
                          #:literal-expression
                          (begin
                            (parameterize ([current-namespace (make-base-empty-namespace)])
                              (namespace-require 'racket/base)
                              (compile 
                               `(namespace-require '',(string->symbol (path->string short-program-name))))))
                          #:cmdline '("-U" "--")))))
                    (let ([make-launcher (if gui? make-mred-launcher make-mzscheme-launcher)])
                      (make-launcher (list "-qt-" (path->string program-filename))
                                     executable-filename))))))))
      
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
    (with-handlers ([exn? raise-hopeless-exception])
      (apply raise-syntax-error '|Module Language|
             (if (null? error-args)
               (list (string-append
                      "There must be a valid module in the\n"
                      "definitions window.  Try starting your program with\n"
                      "\n"
                      "  #lang scheme\n"
                      "\n"
                      "and clicking ‘Run’."))
               error-args))))
  
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
       
       #:get-debugging-radio-box (λ (rb-l rb-r) (set! left-debugging-radio-box rb-l) (set! right-debugging-radio-box rb-r))
       
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
                              (cons (send collection-paths-lb get-string n) (send collection-paths-lb get-data n)))))
    
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
  
  ;; transform-module : (union #f path) syntax
  ;;   -> (values syntax[name-of-module] syntax[lang-of-module] syntax[module])
  ;; = User =
  (define (transform-module filename stx)
    (define-values (mod name lang body)
      (syntax-case stx ()
        [(module name lang . body)
         (eq? 'module (syntax-e #'module))
         (values #'module #'name #'lang #'body)]
        [_ (raise-hopeless-syntax-error
            (string-append "only a module expression is allowed, either\n"
                           "    #lang <language-name>\n or\n"
                           "    (module <name> <language> ...)\n")
            stx)]))
    (define name* (syntax-e name))
    (unless (symbol? name*)
      (raise-hopeless-syntax-error "bad syntax in name position of module"
                                   stx name))
    (when filename (check-filename-matches filename name* stx))
    (let* (;; rewrite the module to use the racket/base version of `module'
           [mod  (datum->syntax #'here 'module mod)]
           [expr (datum->syntax stx `(,mod ,name ,lang . ,body) stx stx)])
      (values name lang expr)))
  
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
  
  ;; check-filename-matches : path datum syntax -> void
  (define (check-filename-matches filename datum unexpanded-stx)
    (let-values ([(base name dir?) (split-path filename)])
      (let ([expected (string->symbol
                       (path->string (path-replace-suffix name #"")))])
        (unless (equal? expected datum)
          (raise-hopeless-syntax-error
           (format
            "module name doesn't match saved filename, got ~s and expected ~s"
            datum
            expected)
           unexpanded-stx)))))
  
  (define module-language-put-file-mixin
    (mixin (text:basic<%>) ()
      (inherit get-text last-position get-character get-top-level-window)
      (define/override (put-file directory default-name)
        (let ([tlw (get-top-level-window)])
          (if (and tlw
                   (is-a? tlw drracket:unit:frame<%>))
              (let* ([definitions-text (send tlw get-definitions-text)]
                     [module-language?
                      (is-a? (drracket:language-configuration:language-settings-language
                              (send definitions-text get-next-settings))
                             module-language<%>)]
                     [module-default-filename
                      (and module-language? (get-module-filename))])
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
      
      (super-new))))
