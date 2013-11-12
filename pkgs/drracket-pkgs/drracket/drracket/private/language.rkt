#lang racket/unit

(require drracket/private/drsig
         string-constants
         
         ;; NOTE: this module instantiates stacktrace itself, so we have
         ;; to be careful to not mix that instantiation with the one
         ;; drracket/private/debug.rkt does. errortrace-lib's is for the
         ;; compilation handling, DrRacket's is for profiling and test coverage
         ;; (which do not do compilation)
         (prefix-in el: errortrace/errortrace-lib) 
         
         (prefix-in image-core: mrlib/image-core)
         
         mzlib/pconvert
         racket/pretty
         mzlib/struct
         racket/class
         racket/file
         racket/list
         compiler/embed
         launcher
         mred
         framework
         mrlib/syntax-browser
         compiler/distribute
         compiler/bundle-dist
         (prefix-in file: file/convertible)
         "rep.rkt"
         "local-member-names.rkt"
         (prefix-in pict-snip: "pict-snip.rkt"))
  
  (import [prefix drracket:debug: drracket:debug^]
          [prefix drracket:tools: drracket:tools^]
          [prefix drracket:rep: drracket:rep^]
          [prefix drracket:init: drracket:init^]
          [prefix drracket:help-desk: drracket:help-desk^])
  (export drracket:language/int^)
  
  (define original-output-port (current-output-port))
  (define (oprintf . args) (apply fprintf original-output-port args)) 

(define text/pos-text drracket:language:text/pos-text)
(define text/pos-start drracket:language:text/pos-start)
(define text/pos-end drracket:language:text/pos-end)
(define make-text/pos make-drracket:language:text/pos)
(define text/pos? drracket:language:text/pos?)
(define struct:text/pos struct:drracket:language:text/pos)
(define text/pos make-text/pos)

  (define language<%>
    (interface ()
      marshall-settings
      unmarshall-settings
      default-settings
      default-settings?
      
      front-end/complete-program
      front-end/finished-complete-program
      front-end/interaction
      config-panel
      on-execute
      extra-repl-information
      
      first-opened
      render-value/format
      render-value
      
      capability-value
      
      create-executable
      
      get-reader-module
      get-metadata
      metadata->settings
      get-metadata-lines
      
      get-language-position
      get-language-name
      get-style-delta
      get-language-numbers
      get-one-line-summary
      get-language-url
      get-comment-character))
  
  (define module-based-language<%>
    (interface ()
      marshall-settings
      unmarshall-settings
      default-settings
      default-settings?
      
      get-module
      get-transformer-module
      use-namespace-require/copy?
      use-namespace-require/copy-from-setting?
      config-panel
      
      get-reader
      
      on-execute
      get-init-code
      use-mred-launcher
      
      render-value/format
      render-value
      
      get-language-position 
      get-language-numbers
      get-one-line-summary
      get-language-url))
  
  (define simple-module-based-language<%>
    (interface ()
      get-module
      get-language-position
      get-language-numbers
      get-one-line-summary
      get-language-url
      get-reader))
  
  
  (define simple-module-based-language%
    (class* object% (simple-module-based-language<%>)
      (init-field module
                  language-position
                  (language-numbers (map (λ (x) 0) language-position))
                  (one-line-summary #f)
                  (language-url #f)
                  (documentation-reference #f)
                  (reader (λ (src port)
                            (let ([v (parameterize ([read-accept-reader #t])
                                        (with-stack-checkpoint
                                         (read-syntax src port)))])
                              (if (eof-object? v)
                                  v
                                  (namespace-syntax-introduce v)))))
                  (language-id (if (pair? language-position)
                                   (car (last-pair language-position))
                                   (error 'simple-module-based-language<%>
                                          "expected non-empty list of strings, got ~e" language-position))))
      (define/public (get-module) module)
      (define/public (get-language-position) language-position)
      (define/public (get-language-numbers) language-numbers)
      (define/public (get-one-line-summary) one-line-summary)
      (define/public (get-language-url) language-url)
      (define/public (get-reader) reader)
      (super-new)))
  
  
  
  ;; simple-module-based-language->module-based-language : module-based-language<%>
  ;; transforms a simple-module-based-language into a module-based-language<%>
  (define simple-module-based-language->module-based-language-mixin
    (mixin (simple-module-based-language<%>) (module-based-language<%>)
      (define/public (get-transformer-module) 'mzscheme)
      (define/public (use-namespace-require/copy?) #f)
      (define/public (use-namespace-require/copy-from-setting? setting)
        (use-namespace-require/copy?))
      (define/public (use-mred-launcher) #t)
      
      (inherit get-module)
      (define/public (marshall-settings settings)
        (simple-settings->vector settings))
      (define/public (unmarshall-settings printable)
        (and (vector? printable)
             (= (vector-length printable)
                (procedure-arity make-simple-settings))
             (boolean? (vector-ref printable 0))
             (memq (vector-ref printable 1) '(constructor quasiquote write trad-write print))
             (memq (vector-ref printable 2) 
                   '(mixed-fraction 
                     mixed-fraction-e
                     repeating-decimal 
                     repeating-decimal-e))
             (boolean? (vector-ref printable 3))
             (boolean? (vector-ref printable 4))
             (memq (vector-ref printable 5) '(none debug debug/profile test-coverage))
             (apply make-simple-settings (vector->list printable))))
      (define/public (default-settings) 
        (make-simple-settings #t 'print 'mixed-fraction-e #f #t 'debug))
      (define/public (default-settings? x)
        (equal? (simple-settings->vector x)
                (simple-settings->vector (default-settings))))
      (define/public (config-panel parent)
        (simple-module-based-language-config-panel parent))
      
      (define/public (on-execute setting run-in-user-thread)
        (initialize-simple-module-based-language setting run-in-user-thread))
      (define/public (get-init-code setting)
        (simple-module-based-language-get-init-code setting))
      
      (define/public (render-value/format value settings port width)
        (simple-module-based-language-render-value/format value settings port width))
      (define/public (render-value value settings port)
        (simple-module-based-language-render-value/format value settings port 'infinity))
      (super-new)))
  
(define simple-settings-case-sensitive drracket:language:simple-settings-case-sensitive)
(define simple-settings-fraction-style drracket:language:simple-settings-fraction-style)
(define simple-settings-show-sharing drracket:language:simple-settings-show-sharing)
(define simple-settings-insert-newlines drracket:language:simple-settings-insert-newlines)
(define simple-settings-annotations drracket:language:simple-settings-annotations)
(define simple-settings-printing-style drracket:language:simple-settings-printing-style)
(define struct:simple-settings struct:drracket:language:simple-settings)
(define simple-settings? drracket:language:simple-settings?)
(define make-simple-settings make-drracket:language:simple-settings)
(define simple-settings make-simple-settings)

  (define simple-settings->vector (make-->vector drracket:language:simple-settings))
  
  ;; simple-module-based-language-config-panel :
  ;;   parent [#:case-sensitive (union #f #t '?)]
  ;;     -> (case-> (-> settings) (settings -> void))
  (define (simple-module-based-language-config-panel
           _parent
           #:case-sensitive [*case-sensitive '?]
           #:dynamic-panel-extras [dynamic-panel-extras void]
           #:get-debugging-radio-box [get-debugging-radio-box void]
           #:debugging-radio-box-callback [debugging-radio-box-callback void])
    (letrec ([parent (instantiate vertical-panel% ()
                       (parent _parent)
                       (alignment '(center center)))]
             
             [input-panel (and (eq? *case-sensitive '?)
                               (instantiate group-box-panel% ()
                                 (label (string-constant input-syntax))
                                 (parent parent)
                                 (alignment '(left center))))]
             
             [dynamic-panel (instantiate group-box-panel% ()
                              (label (string-constant dynamic-properties))
                              (parent parent)
                              (alignment '(left center)))]
             
             [output-panel (instantiate group-box-panel% ()
                             (label (string-constant output-syntax))
                             (parent parent)
                             (alignment '(left center)))]
             
             [case-sensitive (and input-panel
                                  (make-object check-box%
                                    (string-constant case-sensitive-label)
                                    input-panel
                                    void))]
             [debugging-panel (new horizontal-panel%
                                   [parent dynamic-panel]
                                   [stretchable-height #f]
                                   [alignment '(left center)])]
             [debugging-left (new radio-box%
                                  (label #f)
                                  (choices 
                                   (list (string-constant no-debugging-or-profiling)
                                         (string-constant debugging)))
                                  (parent debugging-panel)
                                  (callback
                                   (λ (a b)
                                     (send debugging-right set-selection #f)
                                     (debugging-radio-box-callback a b))))]
             [debugging-right (new radio-box%
                                   (label #f)
                                   (choices 
                                    (list (string-constant debugging-and-profiling)
                                          (string-constant test-coverage)))
                                   (parent debugging-panel)
                                   (callback
                                    (λ (a b)
                                      (send debugging-left set-selection #f)
                                      (debugging-radio-box-callback a b))))]
             [output-style (make-object radio-box%
                             (string-constant output-style-label)
                             (list (string-constant constructor-printing-style)
                                   (string-constant quasiquote-printing-style)
                                   (string-constant write-printing-style)
                                   (string-constant print-printing-style))
                             output-panel
                             (λ (rb evt) (enable-fraction-style))
                             '(horizontal vertical-label))]
             [enable-fraction-style 
              (lambda ()
                (let ([on? (member (send output-style get-selection) '(0 1))])
                  (send fraction-style enable on?)))]
             [show-sharing (make-object check-box%
                             (string-constant sharing-printing-label)
                             output-panel
                             void)]
             [insert-newlines (make-object check-box%
                                (string-constant use-pretty-printer-label)
                                output-panel
                                void)]
             [fraction-style
              (make-object check-box% (string-constant decimal-notation-for-rationals)
                output-panel
                void)])
      (get-debugging-radio-box debugging-left debugging-right)
      (dynamic-panel-extras dynamic-panel)
      
      (case-lambda
        [()
         (make-simple-settings
          (if case-sensitive
            (send case-sensitive get-value)
            (and *case-sensitive #t))
          (case (send output-style get-selection)
            [(0) 'constructor]
            [(1) 'quasiquote]
            [(2) 'trad-write]
            [(3) 'print])
          (if (send fraction-style get-value)
              'repeating-decimal-e
              'mixed-fraction-e)
          (send show-sharing get-value)
          (send insert-newlines get-value)
          (case (send debugging-left get-selection)
            [(0) 'none]
            [(1) 'debug]
            [(#f)
             (case (send debugging-right get-selection)
               [(0) 'debug/profile]
               [(1) 'test-coverage])]))]
        [(settings)
         (when case-sensitive
           (send case-sensitive set-value
                 (simple-settings-case-sensitive settings)))
         (send output-style set-selection
               (case (simple-settings-printing-style settings)
                 [(constructor) 0]
                 [(quasiquote) 1]
                 [(write trad-write) 2]
                 [(print) 3]))
         (enable-fraction-style)
         (send fraction-style set-value (eq? (simple-settings-fraction-style settings)
                                             'repeating-decimal-e))
         (send show-sharing set-value (simple-settings-show-sharing settings))
         (send insert-newlines set-value (simple-settings-insert-newlines settings))
         (case (simple-settings-annotations settings)
           [(none) (send debugging-right set-selection #f) (send debugging-left set-selection 0)]
           [(debug) (send debugging-right set-selection #f) (send debugging-left set-selection 1)]
           [(debug/profile) (send debugging-left set-selection #f) (send debugging-right set-selection 0)]
           [(test-coverage) (send debugging-left set-selection #f) (send debugging-right set-selection 1)])])))
  
  ;; simple-module-based-language-render-value/format : TST settings port (union #f (snip% -> void)) (union 'infinity number) -> void
  (define (simple-module-based-language-render-value/format value settings port width)
    (let-values ([(converted-value write?)
                  (call-with-values
                      (lambda ()
                        (simple-module-based-language-convert-value value settings))
                    (case-lambda
                     [(converted-value) (values converted-value #t)]
                     [(converted-value write?) (values converted-value write?)]))])
      (let ([pretty-out (if write? pretty-write pretty-print)])
        (setup-printing-parameters 
         (λ ()
            (cond
             [(simple-settings-insert-newlines settings)
              (if (number? width)
                  (parameterize ([pretty-print-columns width])
                    (pretty-out converted-value port))
                  (pretty-out converted-value port))]
             [else
              (parameterize ([pretty-print-columns 'infinity])
                (pretty-out converted-value port))
              (newline port)]))
         settings
         width))))
  
  (define default-pretty-print-current-style-table (pretty-print-current-style-table))
  
  
  (define (setup-printing-parameters thunk settings width) ((make-setup-printing-parameters) thunk settings width))
  
  ;; make-setup-printing-parameters : -> (-> (-> void) simple-settings number void)
  (define (make-setup-printing-parameters)
    (define-syntax-rule 
      (dyn name)
      (define name (if gave-up?
                       (string->symbol (format "~a-gave-up" 'name))
                       (dynamic-require 'pict 'name))))
    (define gave-up? #f)
    (define pict:convertible?
      (with-handlers ((exn:fail? (λ (exn)
                                   (set! gave-up? #t)
                                   (log-error (exn-message exn))
                                   (λ (val) #f))))
        (dynamic-require 'pict/convert 'pict-convertible?)))
    (define pict-convert (if gave-up?
                             'pict-convert-gave-up
                             (dynamic-require 'pict/convert 'pict-convert)))
    (dyn pict-width)
    (dyn pict-height)
    (dyn pict-ascent)
    (dyn pict-descent)
    (dyn draw-pict)
      
    (define (mk-pict-snip convertible)
      (define pict (pict-convert convertible))
      (define w (pict-width pict))
      (define h (pict-height pict))
      (define a (pict-ascent pict))
      (define d (pict-descent pict))
      (define rdc (new record-dc%))
      (send rdc set-smoothing 'aligned)
      (send rdc set-clipping-rect 0 0 w h)
      (draw-pict pict rdc 0 0)
      (define recorded-datum (send rdc get-recorded-datum))
      (new pict-snip:pict-snip% [w w] [h h] [d d] [a a] [recorded-datum recorded-datum]))
    (λ (thunk settings width)
      
      (let ([use-number-snip?
             (λ (x)
               (and (number? x)
                    (exact? x)
                    (real? x)
                    (not (integer? x))))])
        (define convert-table (make-hasheq))
        (parameterize ([pretty-print-pre-print-hook (λ (val port) (void))]
                       [pretty-print-post-print-hook (λ (val port) (void))]
                       [pretty-print-exact-as-decimal #f]
                       [pretty-print-depth #f]
                       [pretty-print-.-symbol-without-bars #f]
                       [pretty-print-show-inexactness #f]
                       [pretty-print-abbreviate-read-macros #t]
                       [pretty-print-current-style-table default-pretty-print-current-style-table]
                       [pretty-print-remap-stylable (λ (x) #f)]
                       [pretty-print-print-line
                        (lambda (line port offset width)
                          (when (and (number? width)
                                     (not (eq? 0 line)))
                            (newline port))
                          0)]
                       
                       [pretty-print-columns width]
                       [pretty-print-size-hook
                        (let ([oh (pretty-print-size-hook)])
                          (λ (value display? port)
                            (cond
                              [(not (port-writes-special? port)) (oh value display? port)]
                              [(is-a? value snip%) 1]
                              [(pict:convertible? value) 1]
                              [(use-number-snip? value) 1]
                              [(syntax? value) 1]
                              [(to-snip-value? value) 1]
                              [(hash-ref convert-table value #f) 
                               ;; this handler can be called multiple times per value
                               ;; avoid building the png bytes more than once
                               1]
                              [(and (file:convertible? value)
                                    (file:convert value 'png-bytes #f))
                               =>
                               (λ (converted)
                                 (hash-set! convert-table value converted)
                                 1)]
                              [else (oh value display? port)])))]
                       [pretty-print-print-hook
                        (let ([oh (pretty-print-print-hook)])
                          (λ (value display? port)
                            (cond
                              [(not (port-writes-special? port)) (oh value display? port)]
                              [(is-a? value snip%)
                               (cond
                                 [(image-core:image? value)
                                  
                                  ;; do this computation here so that any failures
                                  ;; during drawing happen under the user's custodian
                                  (image-core:compute-image-cache value) 
                                  (write-special (send value copy) port)
                                  1]
                                 [else
                                  (write-special (send value copy) port)
                                  1])]
                              [(pict:convertible? value)
                               (write-special (mk-pict-snip value))]
                              [(use-number-snip? value)
                               (write-special
                                (case (simple-settings-fraction-style settings)
                                  [(mixed-fraction) 
                                   (number-snip:make-fraction-snip value #f)]
                                  [(mixed-fraction-e)
                                   (number-snip:make-fraction-snip value #t)]
                                  [(repeating-decimal)
                                   (number-snip:make-repeating-decimal-snip value #f)]
                                  [(repeating-decimal-e)
                                   (number-snip:make-repeating-decimal-snip value #t)])
                                port)
                               1]
                              [(syntax? value)
                               (write-special (render-syntax/snip value) port)]
                              [(to-snip-value? value)
                               (write-special (value->snip value) port)]
                              [(hash-ref convert-table value #f)
                               =>
                               (λ (bytes)
                                 (write-special
                                  (make-object image-snip%
                                    (read-bitmap (open-input-bytes bytes)))
                                  port))]
                              [else (oh value display? port)])))]
                       [print-graph
                        ;; only turn on print-graph when using `write' or `print' printing 
                        ;; style, because the sharing is being taken care of
                        ;; by the print-convert sexp construction when using
                        ;; other printing styles.
                        (and (memq (simple-settings-printing-style settings) '(write print))
                             (simple-settings-show-sharing settings))])
          (thunk)))))
      
  ;; drscheme-inspector : inspector
  (define drscheme-inspector (current-inspector))
  
  ;; simple-module-based-language-convert-value : TST settings -> TST
  (define (simple-module-based-language-convert-value value settings)
    (case (simple-settings-printing-style settings)
      [(print) (values value #f)]
      [(write trad-write) value]
      [(constructor)
       (parameterize ([constructor-style-printing #t]
                      [show-sharing (simple-settings-show-sharing settings)]
                      [current-print-convert-hook (leave-snips-alone-hook (current-print-convert-hook))])
         (print-convert value))]
      [(quasiquote)
       (parameterize ([constructor-style-printing #f]
                      [show-sharing (simple-settings-show-sharing settings)]
                      [current-print-convert-hook (leave-snips-alone-hook (current-print-convert-hook))])
         (print-convert value))]))
  
  ;; leave-snips-alone-hook : any? (any? -> printable) any? -> printable
  (define ((leave-snips-alone-hook sh) expr basic-convert sub-convert)
    (if (or (is-a? expr snip%)
            (is-a? expr bitmap%)
            (to-snip-value? expr))
        expr
        (sh expr basic-convert sub-convert)))
  
  ;; initialize-simple-module-based-language : setting ((-> void) -> void)
  (define (initialize-simple-module-based-language setting run-in-user-thread)
    (run-in-user-thread
     (λ ()
       
       (let ([annotations (simple-settings-annotations setting)])
         (case annotations
           [(debug)
            (current-compile (el:make-errortrace-compile-handler))
            (error-display-handler 
             (drracket:debug:make-debug-error-display-handler
              (error-display-handler)))
            (use-compiled-file-paths
             (cons (build-path "compiled" "errortrace")
                   (use-compiled-file-paths)))]
           
           [(debug/profile)
            (drracket:debug:profiling-enabled #t)
            (error-display-handler 
             (drracket:debug:make-debug-error-display-handler
              (error-display-handler)))
            (current-eval (drracket:debug:make-debug-eval-handler (current-eval)))]
           
           [(debug/profile test-coverage)
            (drracket:debug:test-coverage-enabled #t)
            (current-eval (drracket:debug:make-debug-eval-handler (current-eval)))]))
       
       (define my-setup-printing-parameters (make-setup-printing-parameters))
       (global-port-print-handler
        (λ (value port [depth 0])
          (let-values ([(converted-value write?)
                        (call-with-values 
                            (lambda () (simple-module-based-language-convert-value value setting))
                          (case-lambda
                           [(converted-value) (values converted-value #t)]
                           [(converted-value write?) (values converted-value write?)]))])
            (my-setup-printing-parameters 
             (λ ()
               (parameterize ([pretty-print-columns 'infinity])
                 (if write?
                     (pretty-write converted-value port)
                     (pretty-print converted-value port depth))))
             setting
             'infinity))))
       (current-inspector (make-inspector))
       (read-case-sensitive (simple-settings-case-sensitive setting)))))
  
  ;; simple-module-based-language-get-init-code : setting -> sexp[module]
  (define (simple-module-based-language-get-init-code setting)
    `(module mod-name mzscheme
       (require mzlib/pconvert
                mzlib/pretty)
       
       (provide init-code)
       
       (define (executable-error-value->string-handler val size)
         (let ([o (open-output-string)])
           (render-value val o)
           (let ([s (get-output-string o)])
             (if ((string-length s) . <= . size)
                 s
                 (string-append
                  (substring s 0 (- size 3))
                  "...")))))
       
       (define (render-value value port)
         (parameterize ([pretty-print-columns 'infinity])
           ,(case (simple-settings-printing-style setting)
              [(print) `(pretty-print value port)]
              [(write trad-write) `(pretty-write value port)]
              [(constructor)
               `(parameterize ([constructor-style-printing #t]
                               [show-sharing ,(simple-settings-show-sharing setting)])
                  (pretty-write (print-convert value) port))]
              [(quasiquote)
               `(parameterize ([constructor-style-printing #f]
                               [show-sharing ,(simple-settings-show-sharing setting)])
                  (pretty-write (print-convert value) port))])))
         
       ,(if (memq (simple-settings-annotations setting) '(debug debug/profile test-coverage))
            `(require errortrace)
            `(void))
       
       (define (init-code)
         (current-inspector (make-inspector))
         (error-value->string-handler executable-error-value->string-handler)
         (read-case-sensitive ,(simple-settings-case-sensitive setting)))))
  
  
  ;; module-based-language->language : module-based-language -> language<%>
  ;; given a module-based-language, implements a language
  (define module-based-language->language-mixin
    (mixin (module-based-language<%>) (language<%>)
      (inherit get-module get-transformer-module use-namespace-require/copy-from-setting?
               get-init-code use-mred-launcher get-reader)
      
      (define/public (front-end/finished-complete-program settings) (void))
      (define/public (module-based-language->language-mixin settings) (void))
      
      (define/pubment (capability-value s) 
        (inner (get-capability-default s) capability-value s))
      
      (define/public (first-opened) (void))
      (define/public (get-comment-character) (values ";  " #\;))
      
      (inherit get-language-position)
      (define/public (get-language-name)
        (let ([pos (get-language-position)])
          (if (null? pos)
              "<<unknown>>"
              (car (last-pair pos)))))
      (define/public (get-style-delta) #f)
      (define/override (on-execute setting run-in-user-thread)
        (super on-execute setting run-in-user-thread)
        (initialize-module-based-language (use-namespace-require/copy-from-setting? setting)
                                          (get-module)
                                          (get-transformer-module)
                                          run-in-user-thread))
      (define/public (front-end/complete-program port settings)
        (module-based-language-front-end port (get-reader)))
      (define/public (front-end/interaction port settings)
        (module-based-language-front-end port (get-reader)))
      (define/public (create-executable setting parent program-filename) (void))
      (define/public (extra-repl-information _1 _2) (void))
      (define/public (get-reader-module) #f)
      (define/public (get-metadata a b) #f)
      (define/public (metadata->settings m) #f)
      (define/public (get-metadata-lines) #f)
      
      (super-new)))
  
  ;; create-module-based-language-executable :  
  ;; (is-a?/c area-container<%>) string (or #f module-spec) module-spec sexp (union boolean? 'ask) boolean?
  ;;  -> void
  (define (create-module-based-language-executable parent
                                                   program-filename
                                                   module-language-spec
                                                   transformer-module-language-spec
                                                   init-code
                                                   mred-launcher
                                                   use-copy?)
    (let ([executable-specs (create-executable-gui parent
                                                   program-filename
                                                   #t 
                                                   (if (boolean? mred-launcher)
                                                       (if mred-launcher
                                                           'mred
                                                           'mzscheme)
                                                       #t))])
      (when executable-specs
        (let* ([type (car executable-specs)]
               [base (cadr executable-specs)]
               [executable-filename (caddr executable-specs)]
               [create-executable
                (case type
                  [(launcher) create-module-based-launcher]
                  [(stand-alone) create-module-based-stand-alone-executable]
                  [(distribution) create-module-based-distribution])])
          (with-handlers ((exn:fail? (λ (msg)
                                       (define sp (open-output-string))
                                       (parameterize ([current-error-port sp])
                                         (drracket:init:original-error-display-handler
                                          (exn-message exn)
                                          exn))
                                       (message-box 
                                        (string-constant drscheme)
                                        (string-append
                                         (string-constant error-creating-executable)
                                         "\n\n"
                                         (get-output-string sp))))))
            (create-executable
             program-filename
             executable-filename
             module-language-spec
             transformer-module-language-spec
             init-code
             (if (boolean? mred-launcher)
                 mred-launcher
                 (eq? base 'mred))
             use-copy?))))))
  
  
  ;; create-executable-gui : (union #f (is-a?/c top-level-area-container<%>))
  ;;                         (union #f string?)
  ;;                         (union #t 'launcher 'stand-alone 'distribution)
  ;;                         (union #t 'mzscheme 'mred)
  ;;                      -> (union #f (list (union 'no-show 'launcher 'stand-alone 'distribution)
  ;;                                         (union 'no-show 'mzscheme 'mred)
  ;;                                         string[filename]))
  (define (create-executable-gui parent program-filename show-type show-base)
    (define dlg (make-object dialog% (string-constant create-executable-title) parent))
    (define filename-panel (make-object horizontal-panel% dlg))
    (define filename-text-field (new text-field% 
                                     [label (string-constant filename)]
                                     [parent filename-panel]
                                     [init-value (path->string 
                                                  (default-executable-filename 
                                                    program-filename 
                                                    (if (eq? show-type #t) 'launcher show-type)
                                                    #f))]
                                     [min-width 400]
                                     [callback void]))
    (define filename-browse-button (instantiate button% ()
                                     (label (string-constant browse...))
                                     (parent filename-panel)
                                     (callback
                                      (λ (x y) (browse-callback)))))
    (define type/base-panel (instantiate vertical-panel% ()
                              (parent dlg)
                              (stretchable-width #f)))
    (define type-panel (make-object horizontal-panel% type/base-panel))
    (define type-rb (and (boolean? show-type)
                         (instantiate radio-box% ()
                           (label (string-constant executable-type))
                           (choices (list (string-constant launcher-explanatory-label)
                                          (string-constant stand-alone-explanatory-label)
                                          (string-constant distribution-explanatory-label)))
                           (parent type-panel)
                           (callback (lambda (rb e) 
                                       (preferences:set 'drracket:create-executable-gui-type
                                                        (case (send rb get-selection)
                                                          [(0) 'launcher]
                                                          [(1) 'stand-alone]
                                                          [(2) 'distribution]))
                                       (reset-filename-suffix))))))
    (define base-panel (make-object horizontal-panel% type/base-panel))
    (define base-rb (and (boolean? show-base)
                         (instantiate radio-box% ()
                           (label (string-constant executable-base))
                           (choices (list "Racket" "GRacket"))
                           (parent base-panel)
                           (callback (lambda (rb e)
                                       (preferences:set 'drracket:create-executable-gui-base
                                                        (case (send rb get-selection)
                                                          [(0) 'racket]
                                                          [(1) 'gracket]))
                                       (reset-filename-suffix))))))
    
    (define (reset-filename-suffix)
      (let ([s (send filename-text-field get-value)])
        (unless (string=? s "")
          (let ([new-s (default-executable-filename 
                         (string->path s)
                         (current-mode)
                         (not (currently-mzscheme-binary?)))])
            (send filename-text-field set-value (path->string new-s))))))
    
    (define button-panel (instantiate horizontal-panel% ()
                           (parent dlg)
                           (alignment '(right center))))
    
    (define-values (ok-button cancel-button)
      (gui-utils:ok/cancel-buttons
       button-panel
       (λ (x y)
         (when (check-filename)
           (set! cancelled? #f)
           (send dlg show #f)))
       (λ (x y) (send dlg show #f))
       (string-constant create)
       (string-constant cancel)))
    
    (define (browse-callback)
      (let ([ftf (send filename-text-field get-value)])
        (let-values ([(base name _) 
                      (if (path-string? ftf)
                          (split-path ftf)
                          (values (current-directory) "" #f))])
          (let* ([mzscheme? (currently-mzscheme-binary?)]
                 [mode (current-mode)]
                 [filename 
                  (put-executable/defaults
                   dlg
                   base
                   name
                   mode
                   (not mzscheme?)
                   (case mode
                     [(launcher)
                      (if mzscheme?
                          (string-constant save-a-mzscheme-launcher)
                          (string-constant save-a-mred-launcher))]
                     [(stand-alone)
                      (if mzscheme?
                          (string-constant save-a-mzscheme-stand-alone-executable)
                          (string-constant save-a-mred-stand-alone-executable))]
                     [(distribution)
                      (if mzscheme?
                          (string-constant save-a-mzscheme-distribution)
                          (string-constant save-a-mred-distribution))]))])
            (when filename
              (send filename-text-field set-value (path->string filename)))))))
    
    (define (currently-mzscheme-binary?)
      (cond
        [base-rb
         (= 0 (send base-rb get-selection))]
        [else (eq? show-base 'mzscheme)]))
    
    (define (current-mode)
      (cond
        [type-rb
         (let ([s (send type-rb get-item-label (send type-rb get-selection))])
           (cond
             [(equal? s (string-constant launcher-explanatory-label)) 'launcher]
             [(equal? s (string-constant stand-alone-explanatory-label)) 'stand-alone]
             [(equal? s (string-constant distribution-explanatory-label)) 'distribution]))]
        [else show-type]))
    
    (define (check-filename)
      (let ([filename-str (send filename-text-field get-value)]
            [mred? (not (currently-mzscheme-binary?))]
            [mode (current-mode)])
        (let-values ([(extension style filters)
                      (mode->put-file-extension+style+filters mode mred?)])
          (cond
            [(string=? "" filename-str)
             (message-box (string-constant drscheme)
                          (string-constant please-specify-a-filename)
                          dlg
                          #:dialog-mixin frame:focus-table-mixin)
             #f]
            [(not (users-name-ok? mode extension dlg (string->path filename-str)))
             #f]
            [(or (directory-exists? filename-str)
                 (file-exists? filename-str))
             (ask-user-can-clobber? filename-str)]
            [else #t]))))
    
    ;; ask-user-can-clobber-directory? : (is-a?/c top-level-window<%>) string -> boolean
    (define (ask-user-can-clobber? filename)
      (eq? (message-box (string-constant drscheme)
                        (format (string-constant are-you-sure-replace?) filename)
                        dlg
                        '(yes-no)
                        #:dialog-mixin frame:focus-table-mixin)
           'yes))
    
    (define cancelled? #t)
    (when type-rb
      (send type-rb set-selection
            (case (preferences:get 'drracket:create-executable-gui-type)
              [(launcher) 0]
              [(stand-alone) 1]
              [(distribution) 2])))
    (when base-rb
      (send base-rb set-selection
            (case (preferences:get 'drracket:create-executable-gui-base)
              [(racket) 0]
              [(gracket) 1])))
    (reset-filename-suffix)
    (send dlg show #t)
    (cond
      [cancelled? #f]
      [else
       (list
        (if type-rb
            (current-mode)
            'no-show)
        (if base-rb
            (case (send base-rb get-selection)
              [(0) 'mzscheme]
              [(1) 'mred])
            'no-show)
        (send filename-text-field get-value))]))
  
  (define (normalize-mode mode)
    (case mode
      [(launcher stand-alone distribution) mode]
      ;; Backward compatibility: interpret a boolean
      [else (if mode 'launcher 'stand-alone)]))
  
  ;; put-executable : parent string (union boolean 'launcher 'stand-alone 'distribution) boolean -> (union false? string)
  ;; invokes the put-file dialog with arguments specific to building executables
  (define (put-executable parent program-filename mode mred? title)
    (let-values ([(base name dir) (split-path program-filename)])
      (let ([mode (normalize-mode mode)])
        (let ([default-name (default-executable-filename name mode mred?)])
          (put-executable/defaults
           parent
           base
           default-name
           mode
           mred? 
           title)))))
  
  ;; put-executable/defaults : parent string string symbol boolean -> (union false? string)
  (define (put-executable/defaults parent default-dir default-name mode mred? title)
    (let-values ([(extension style filters)
                  (mode->put-file-extension+style+filters mode mred?)])
      (let* ([dir? (case mode
                     [(launcher)
                      (if mred?
                          (mred-launcher-is-directory?)
                          (mzscheme-launcher-is-directory?))]
                     [(stand-alone)
                      (embedding-executable-is-directory? mred?)]
                     [(distribution) #f])]
             [users-name
              (if dir?
                  (get-directory title
                                 parent
                                 default-dir
                                 style)
                  (put-file title
                            parent
                            default-dir
                            default-name
                            extension
                            style
                            filters))])
        (and users-name
             (users-name-ok? mode extension parent users-name)
             (or (not dir?)
                 (gui-utils:get-choice
                  (format (string-constant warning-directory-will-be-replaced)
                          users-name)
                  (string-constant yes)
                  (string-constant no)
                  (string-constant drscheme)
                  #f
                  parent))
             users-name))))
  
  ;; users-name-ok? : symbol string (union #f frame% dialog%) path? -> boolean
  ;; returns #t if the string is an acceptable name for
  ;; a saved executable, and #f otherwise.
  (define (users-name-ok? mode extension parent name)
    (or (not extension)
        (let ([suffix-m (regexp-match #rx"[.][^.]*$" (path->string name))])
          (or (and suffix-m
                   (string=? (substring (car suffix-m) 1) extension))
              (and
               (message-box (string-constant drscheme)
                            (format
                             (string-constant ~a-must-end-with-~a)
                             (case mode
                               [(launcher) (string-constant launcher)]
                               [(stand-alone) (string-constant stand-alone)]
                               [(distribution) (string-constant distribution)])
                             name
                             extension)
                            parent
                            #:dialog-mixin frame:focus-table-mixin)
               #f)))))
  
  ;; default-executable-filename : path symbol boolean -> path
  (define (default-executable-filename program-filename mode mred?)
    (let ([ext (let-values ([(extension style filters)
                             (mode->put-file-extension+style+filters mode mred?)])
                 (if extension
                     (string->bytes/utf-8 (string-append "." extension))
                     #""))])
      (path-replace-suffix program-filename ext)))
  
  (define (mode->put-file-extension+style+filters mode mred?)
    (case mode
      [(launcher)
       (if mred?
           (mred-launcher-put-file-extension+style+filters)
           (mzscheme-launcher-put-file-extension+style+filters))]
      [(stand-alone)
       (embedding-executable-put-file-extension+style+filters mred?)]
      [(distribution)
       (bundle-put-file-extension+style+filters)]))
  
  ;; create-module-based-stand-alone-executable : ... -> void (see docs)
  (define (create-module-based-stand-alone-executable program-filename 
                                                      executable-filename
                                                      module-language-spec
                                                      transformer-module-language-spec
                                                      init-code
                                                      gui?
                                                      use-copy?)
    
    (with-handlers ([(λ (x) #f) ;exn:fail?
                     (λ (x)
                       (message-box 
                        (string-constant drscheme)
                        (format "~a" (exn-message x))
                        #:dialog-mixin frame:focus-table-mixin)
                       (void))])
      (define init-code-tmp-filename (make-temporary-file "drs-standalone-exectable-init~a"))
      (define bootstrap-tmp-filename (make-temporary-file "drs-standalone-exectable-bootstrap~a"))
      (let ([init-code-mod-name
             (let-values ([(base name dir?) 
                           (split-path init-code-tmp-filename)])
               (string->symbol (path->string name)))])
        
        (call-with-output-file bootstrap-tmp-filename
          (λ (port)
            (write `(let () ;; cannot use begin, since it gets flattened to top-level (and re-compiled!)
                      ,@(if module-language-spec
                            (if use-copy?
                                (list 
                                 `(namespace-require/copy ',module-language-spec))
                                (list
                                 `(namespace-require/constant ',module-language-spec)))
                            '())
                      ,@(if transformer-module-language-spec
                            (list `(namespace-require `(for-syntax ,transformer-module-language-spec)))
                            (list))
                      ((dynamic-require ',init-code-mod-name 'init-code)))
                   port))
          #:exists 'truncate
          #:mode 'text)
        
        (let ([new-init-code 
               (list*
                (car init-code)
                init-code-mod-name
                (cddr init-code))])
          (call-with-output-file init-code-tmp-filename
            (λ (port)
              (write new-init-code port))
            #:exists 'truncate #:mode 'text)))
      
      (let* ([pre-to-be-embedded-module-specs0
              (cond
                [(and module-language-spec transformer-module-language-spec)
                 (if (equal? module-language-spec transformer-module-language-spec)
                     (list module-language-spec)
                     (list module-language-spec transformer-module-language-spec))]
                [module-language-spec 
                 (list module-language-spec)]
                [transformer-module-language-spec
                 (list transformer-module-language-spec)]
                [else '()])]
             [pre-to-be-embedded-module-specs1
              (if gui?
                  (cons '(lib "mred/mred.rkt")
                        pre-to-be-embedded-module-specs0)
                  pre-to-be-embedded-module-specs0)]
             [pre-to-be-embedded-module-specs2
              (cons `(file ,(path->string init-code-tmp-filename))
                    pre-to-be-embedded-module-specs1)]
             [pre-to-be-embedded-module-specs3
              (filter (λ (x) (not (eq? x 'mzscheme)))
                      pre-to-be-embedded-module-specs2)]
             [to-be-embedded-module-specs
              (map (λ (x) (list #f x))
                   pre-to-be-embedded-module-specs3)])
        
        (create-embedding-executable 
         executable-filename
         #:mred? gui?
         #:verbose? #f ;; verbose?
         #:modules to-be-embedded-module-specs
         #:literal-files (list 
                          bootstrap-tmp-filename
                          program-filename)
         #:cmdline (if gui?
                       (list "-mvqZ")
                       (list "-mvq"))))
      (delete-file init-code-tmp-filename)
      (delete-file bootstrap-tmp-filename)
      (void)))
  
  ;; create-module-based-distribution : ... -> void (see docs)
  (define (create-module-based-distribution program-filename 
                                            distribution-filename
                                            module-language-spec
                                            transformer-module-language-spec
                                            init-code
                                            gui?
                                            use-copy?)
    (create-distribution-for-executable
     distribution-filename
     gui?
     (lambda (exe-name)
       (create-module-based-stand-alone-executable program-filename 
                                                   exe-name
                                                   module-language-spec
                                                   transformer-module-language-spec
                                                   init-code
                                                   gui?
                                                   use-copy?))))
  
  ;; create-distribution-for-executable : ... -> void (see docs)
  (define (create-distribution-for-executable distribution-filename
                                              gui?
                                              make-executable)
    ;; Delete old file, if it exists:
    (when (file-exists? distribution-filename)
      (delete-file distribution-filename))
    ;; Figure out base name, and create working temp directory:
    (let* ([base-name (let-values ([(base name dir?) (split-path distribution-filename)])
                        (path-replace-suffix name #""))]
           [temp-dir
            (make-temporary-file "drscheme-tmp-~a" 'directory)]
           [c (make-custodian)]
           [dialog (new dialog%
                        [label (string-constant distribution-progress-window-title)]
                        [width 400])]
           [status-message
            (new message%
                 [label (string-constant creating-executable-progress-status)]
                 [parent dialog]
                 [stretchable-width #t])]
           [pane (new vertical-pane%
                      [parent dialog])]
           [abort-button
            (new button%
                 [parent pane]
                 [label (string-constant abort)]
                 [callback (lambda (_1 _2)
                             (custodian-shutdown-all c))])]
           
           [exn #f]
           
           [worker-thread
            (parameterize ([current-custodian c])
              (thread
               (λ ()
                 (with-handlers ([exn? (λ (e) (set! exn e))])
                   ;; Build the exe:
                   (make-directory (build-path temp-dir "exe"))
                   (let ([exe-name (build-path temp-dir "exe" (default-executable-filename base-name 'stand-alone gui?))])
                     (make-executable exe-name)
                     (when (or (file-exists? exe-name)
                               (directory-exists? exe-name))
                       (let ([dist-dir (build-path temp-dir base-name)])
                         ;; Assemble the bundle directory:
                         (queue-callback 
                          (λ ()
                            (send status-message set-label (string-constant assembling-distribution-files-progress-status))))
                         (assemble-distribution dist-dir (list exe-name))
                         ;; Pack it:
                         (queue-callback
                          (λ ()
                            (send status-message set-label (string-constant packing-distribution-progress-status))))
                         (bundle-directory distribution-filename dist-dir #t))))))))])
      
      ;; create a thread that will trigger hiding the dialog and the return from `show'
      ;; when things are done (no matter if there was a kill, or just normal terminiation)
      (thread
       (λ ()
         (thread-wait worker-thread)
         (queue-callback (λ () (send dialog show #f)))))
      
      (send dialog show #t)
      
      ;; Clean up:
      (custodian-shutdown-all c)
      (delete-directory/files temp-dir)
      
      (when exn
        (raise exn))))
  
  
  (define (condense-scheme-code-string s)
    (let ([i (open-input-string s)]
          [o (open-output-string)])
      (let loop ()
        (let ([c (read-char i)])
          (unless (eof-object? c)
            (let ([next (λ ()
                          (display c o)
                          (loop))])
              (case c
                [(#\space)
                 (if (char=? #\( (peek-char i))
                     (loop)
                     (next))]
                [(#\))
                 (if (eq? #\space (peek-char i))
                     (begin
                       (display #\) o)
                       (read-char i)
                       (loop))
                     (next))]
                [(#\\)
                 (begin
                   (display #\\ o)
                   (display (read-char i) o)
                   (loop))]
                [(#\" #\|)
                 (display c o)
                 (let loop ()
                   (let ([v (read-char i)])
                     (cond
                       [(eq? v c) (next)]
                       [(eq? v #\\)
                        (display v o)
                        (display (read-char i) o)
                        (loop)]
                       [else (display v o)
                             (loop)])))]
                [else (next)])))))
      (get-output-string o)))
  
  (define (create-module-based-launcher program-filename 
                                        executable-filename
                                        module-language-spec
                                        transformer-module-language-spec
                                        init-code
                                        gui?
                                        use-copy?)
    
    (with-handlers ([(λ (x) #f) ;exn:fail?
                     (λ (x)
                       (message-box 
                        (string-constant drscheme)
                        (format "~a" (exn-message x))
                        #:dialog-mixin frame:focus-table-mixin)
                       (void))])
      
      ((if gui? make-mred-launcher make-mzscheme-launcher)
       (list
        (path->string
         (collection-file-path (if gui? 
                                   "launcher-mred-bootstrap.rkt"
                                   "launcher-mz-bootstrap.rkt")
                               "drracket" "private"))
        (condense-scheme-code-string (format "~s" init-code))
        (path->string program-filename)
        (format "~s" module-language-spec)
        (format "~s" transformer-module-language-spec)
        (format "~s" use-copy?))
       (if (path? executable-filename)
           (path->string executable-filename)
           executable-filename))))
  
  ;; initialize-module-based-language : boolean (or #f module-spec) module-spec ((-> void) -> void)
  (define (initialize-module-based-language use-copy?
                                            module-spec
                                            transformer-module-spec
                                            run-in-user-thread)
    (run-in-user-thread
     (λ ()
       (with-handlers ([(λ (x) #t)
                        (λ (x)
                          (display (if (exn? x) 
                                       (exn-message x) 
                                       (format "~s" x)))
                          (newline))])
         (when module-spec
           (if use-copy?
               (namespace-require/copy module-spec)
               (namespace-require/constant module-spec)))
         (when transformer-module-spec
           (namespace-require `(for-syntax ,transformer-module-spec)))))))
  
  ;; module-based-language-front-end : (port reader -> (-> (union sexp syntax eof)))
  ;; type reader = type-spec-of-read-syntax (see mz manual for details)
  (define (module-based-language-front-end port reader)
    (λ () 
      (let ([s (reader (object-name port) port)])
        (if (syntax? s)
            (namespace-syntax-introduce
             (datum->syntax 
              #f 
              (cons '#%top-interaction s)
              s))
            s))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  snip/value extensions
  ;;
  
  (define to-snips null)
  (define-struct to-snip (predicate? >value setup-thunk))
  (define add-snip-value
    (lambda (predicate constructor [setup-thunk void])
      (set! to-snips (cons (make-to-snip predicate constructor setup-thunk) to-snips))))
  
  (define (value->snip v)
    (ormap (λ (to-snip) (and ((to-snip-predicate? to-snip) v)
                             ((to-snip->value to-snip) v)))
           to-snips))
  (define (to-snip-value? v)
    (ormap (λ (to-snip) ((to-snip-predicate? to-snip) v)) to-snips))
  (define (setup-setup-values) 
    (for-each (λ (t) ((to-snip-setup-thunk t))) to-snips))
  
  
  (define capabilities '())
  (define (capability-registered? x) (and (assoc x capabilities) #t))
  (define (register-capability name contract default)
    (when (capability-registered? name)
      (error 'register-capability "already registered capability ~s" name))
    (set! capabilities (cons (list name default contract) capabilities)))
  (define (get-capability-default name)
    (let ([l (assoc name capabilities)])
      (unless l
        (error 'get-capability-default "name not bound ~s" name))
      (cadr l)))
  (define (get-capability-contract name)
    (let ([l (assoc name capabilities)])
      (unless l
        (error 'get-capability-contract "name not bound ~s" name))
      (caddr l)))
  

;                                                                        
;                                                                        
;  ;;;;;;                                       ;                        
;   ;   ;          ;                                                     
;   ; ;   ;;  ;;  ;;;;;   ;;;  ;; ;;    ;;;;  ;;;     ;;;  ;; ;;    ;;;; 
;   ;;;    ;  ;    ;     ;   ;  ;;  ;  ;   ;    ;    ;   ;  ;;  ;  ;   ; 
;   ; ;     ;;     ;     ;;;;;  ;   ;   ;;;     ;    ;   ;  ;   ;   ;;;  
;   ;       ;;     ;     ;      ;   ;      ;    ;    ;   ;  ;   ;      ; 
;   ;   ;  ;  ;    ;   ; ;      ;   ;  ;   ;    ;    ;   ;  ;   ;  ;   ; 
;  ;;;;;; ;;  ;;    ;;;   ;;;; ;;; ;;; ;;;;   ;;;;;   ;;;  ;;; ;;; ;;;;  
;                                                                        
;                                                                        
;                                                                        
;                                                                        
  
  
  
  (define language-extensions null)
  (define (get-language-extensions) 
    (drracket:tools:only-in-phase
     'drracket:language:get-default-mixin
     'phase2)
    language-extensions)
  
  (define (default-mixin x) x)
  (define (get-default-mixin)
    (drracket:tools:only-in-phase
     'drracket:language:get-default-mixin
     'phase2)
    default-mixin)
  
  (define (extend-language-interface extension<%> default-impl)
    (drracket:tools:only-in-phase
     'drracket:language:extend-language-interface
     'phase1)
    (set! default-mixin (compose default-impl default-mixin))
    (set! language-extensions (cons extension<%> language-extensions)))
