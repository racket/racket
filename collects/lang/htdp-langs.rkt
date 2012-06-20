#lang scheme
(require string-constants
         framework
         (prefix-in et: errortrace/stacktrace)
         mzlib/pretty
         (prefix-in pc: mzlib/pconvert)
         mzlib/file
         mzlib/unit
         mzlib/class
         mzlib/list
         mzlib/struct
         mzlib/compile
         mzlib/struct
         drscheme/tool
         mred
         framework/private/bday
         syntax/moddep
         mrlib/cache-image-snip
         (prefix-in ic: mrlib/image-core)
         setup/dirs
         test-engine/racket-tests

         ;; this module is shared between the drscheme's namespace (so loaded here) 
         ;; and the user's namespace in the teaching languages
         "private/set-result.rkt"
         "private/rewrite-error-message.rkt"

         "private/continuation-mark-key.rkt"
         "private/create-htdp-executable.rkt"
         
         "stepper-language-interface.rkt"
         "debugger-language-interface.rkt"
         "run-teaching-program.rkt"
         "htdp-langs-save-file-prefix.rkt"

         (only-in test-engine/scheme-gui make-formatter)
         (only-in test-engine/scheme-tests
                  scheme-test-data error-handler test-format test-execute display-results
                  build-test-engine)
         (lib "test-engine/test-display.scm")
         deinprogramm/signature/signature
         )
  
  
  (provide tool@)
  
  (define o (current-output-port))
  (define (oprintf . args) (apply fprintf o args))
  
  (define user-installed-teachpacks-collection "installed-teachpacks")
  (define teachpack-installation-dir (build-path (find-user-collects-dir) user-installed-teachpacks-collection))
  
  
  (define tool@
    (unit 
      (import drscheme:tool^)
      (export drscheme:tool-exports^)
      
      (define drs-eventspace (current-eventspace))
      
      ;; tracing? : boolean
      ;; teachpacks : (listof require-spec)
      (define-struct (htdp-lang-settings drscheme:language:simple-settings) (tracing? teachpacks))
      (define htdp-lang-settings->vector (make-->vector htdp-lang-settings))
      
      (define image-string "<image>")
      
      (define htdp-language<%>
        (interface ()
          get-module
          get-language-position
          get-sharing-printing
          get-abbreviate-cons-as-list
          get-allow-sharing?
          get-use-function-output-syntax?
          get-accept-quasiquote?
          get-read-accept-dot))
      
      ;; module-based-language-extension :    (implements drscheme:language:module-based-language<%>) 
      ;;                                   -> (implements drscheme:language:module-based-language<%>)
      ;; changes the default settings and sets a few more paramters during `on-execute'
      (define (module-based-language-extension super%)
        (class* super% ()
          
          (inherit get-sharing-printing get-abbreviate-cons-as-list)
          
          (define/override (default-settings)
            (make-htdp-lang-settings 
             #t
             'constructor
             'repeating-decimal
             (get-sharing-printing)
             #t
             'none
             #f 
             (preferences:get 'drracket:htdp:last-set-teachpacks)))
          
          (define/override (default-settings? s)
            (and (super default-settings? s)
                 (not (htdp-lang-settings-tracing? s))
                 (null? (htdp-lang-settings-teachpacks s))))
          
          (define/override (marshall-settings x)
            (list (super marshall-settings x)
                  (htdp-lang-settings-tracing? x)
                  (htdp-lang-settings-teachpacks x)))
          
          (define/override (unmarshall-settings x)
            (if (and (list? x)
                     (= (length x) 3)
                     (boolean? (list-ref x 1))
                     (list-of-require-specs? (list-ref x 2)))
                (let ([drs-settings (super unmarshall-settings (first x))])
                  (make-htdp-lang-settings
                   (drscheme:language:simple-settings-case-sensitive drs-settings)
                   (drscheme:language:simple-settings-printing-style  drs-settings)
                   (drscheme:language:simple-settings-fraction-style  drs-settings)
                   (drscheme:language:simple-settings-show-sharing  drs-settings)
                   (drscheme:language:simple-settings-insert-newlines  drs-settings)
                   (drscheme:language:simple-settings-annotations drs-settings)
                   (cadr x)
                   (caddr x)))
                (default-settings)))
          
          (define/private (list-of-require-specs? l)
            (and (list? l)
                 (andmap (λ (x)
                           (and (list? x)
                                (andmap (λ (x) (or (string? x) (symbol? x))) x)))
                         l)))
          
          (inherit get-allow-sharing? get-use-function-output-syntax? 
                   get-accept-quasiquote? get-read-accept-dot)
          (define/override (config-panel parent)
            (sharing/not-config-panel (get-allow-sharing?) (get-accept-quasiquote?) parent))
          
          (define/override (on-execute settings run-in-user-thread)
            (let ([drs-namespace (current-namespace)]
                  [set-result-module-name 
                   ((current-module-name-resolver) '(lib "lang/private/set-result.ss") #f #f)]
                  [scheme-test-module-name
                   ((current-module-name-resolver) '(lib "test-engine/scheme-tests.ss") #f #f)]
                  [scheme-signature-module-name
                   ((current-module-name-resolver) '(lib "deinprogramm/signature/signature-english.rkt") #f #f)]
                  [tests-on? (preferences:get 'test-engine:enable?)])
              (run-in-user-thread
               (lambda ()
                 (when (getenv "PLTDRHTDPNOCOMPILED") (use-compiled-file-paths '()))
                 (read-accept-quasiquote (get-accept-quasiquote?))
                 (namespace-attach-module drs-namespace ''drscheme-secrets)
                 (namespace-attach-module drs-namespace set-result-module-name)                 
                 (error-display-handler teaching-languages-error-display-handler)
                 (error-value->string-handler (λ (x y) (teaching-languages-error-value->string settings x y)))
                 (current-eval (add-annotation (htdp-lang-settings-tracing? settings) (current-eval)))
                 (error-print-source-location #f)
                 (read-decimal-as-inexact #f)
                 (read-accept-dot (get-read-accept-dot))
                 (namespace-attach-module drs-namespace scheme-test-module-name)
                 (namespace-require scheme-test-module-name)
                 (namespace-attach-module drs-namespace scheme-signature-module-name)
                 (namespace-require scheme-signature-module-name)
		 ;; hack: the test-engine code knows about the test~object name; we do, too
		 (namespace-set-variable-value! 'test~object (build-test-engine))
		 ;; record signature violations with the test engine
                 (signature-violation-proc
                  (lambda (obj signature message blame)
                    (cond
                      ((namespace-variable-value 'test~object #f (lambda () #f))
                       => (lambda (engine)
                            (send (send engine get-info) signature-failed
                                  obj signature message blame))))))
                 (scheme-test-data (list (drscheme:rep:current-rep) drs-eventspace test-display%))
                 (test-execute tests-on?)
		 (signature-checking-enabled? (get-preference 'signatures:enable-checking? (lambda () #t)))
                 (test-format (make-formatter (lambda (v o) (render-value/format v settings o 40)))))))
            (super on-execute settings run-in-user-thread)
            
            ;; set the global-port-print-handler after the super class because the super sets it too
            (run-in-user-thread
             (lambda ()
               (define my-setup-printing-parameters (drscheme:language:make-setup-printing-parameters))
               (global-port-print-handler
                (λ (value port [depth 0])
                  (teaching-language-render-value/format my-setup-printing-parameters value settings port 'infinity))))))
          
          (define/private (teaching-languages-error-value->string settings v len)
            (let ([sp (open-output-string)])
              (set-printing-parameters settings (λ () (print v sp)))
              (flush-output sp)
              (let ([s (get-output-string sp)])
                (cond
                  [(<= (string-length s) len) s]
                  [else (string-append (substring s 0 (- len 3)) "...")]))))

	  ;; set-printing-parameters : settings ( -> TST) -> TST
	  ;; is implicitly exposed to the stepper.  watch out!  --  john
          (define/public (set-printing-parameters settings thunk)
            (define img-str "#<image>")
            (define (is-image? val)
              (or (is-a? val ic:image%)         ;; 2htdp/image
                  (is-a? val cache-image-snip%) ;; htdp/image
                  (is-a? val image-snip%)       ;; literal image constant
                  (is-a? val bitmap%)))         ;; works in other places, so include it here too
            (parameterize ([pc:booleans-as-true/false #t]
                           [pc:add-make-prefix-to-constructor #t]
                           [pc:abbreviate-cons-as-list (get-abbreviate-cons-as-list)]
                           [pc:current-print-convert-hook
                            (let ([ph (pc:current-print-convert-hook)])
                              (lambda (val basic sub)
                                (cond
                                  [(equal? val set!-result) '(void)]
                                  [else (ph val basic sub)])))]
                           [pretty-print-show-inexactness #t]
                           [pretty-print-exact-as-decimal #t]
                           [pretty-print-print-hook
                            (let ([oh (pretty-print-print-hook)])
                              (λ (val display? port)
                                (if (and (not (port-writes-special? port))
                                         (is-image? val))
                                    (begin (display img-str port)
                                           (string-length img-str))
                                    (oh val display? port))))]
                           [pretty-print-size-hook
                            (let ([oh (pretty-print-size-hook)])
                              (λ (val display? port)
                                (if (and (not (port-writes-special? port))
                                         (is-image? val))
                                    (string-length img-str)
                                    (oh val display? port))))]
                           [pc:use-named/undefined-handler
                            (lambda (x)
                              (and (get-use-function-output-syntax?)
                                   (procedure? x)
                                   (object-name x)))]
                           [pc:named/undefined-handler
                            (lambda (x)
                              (string->symbol
                               (format "function:~a" (object-name x))))])
              (thunk)))
          
          (define/override (render-value/format value settings port width)
            (teaching-language-render-value/format drscheme:language:setup-printing-parameters value settings port width))
          (define/override (render-value value settings port)
            (teaching-language-render-value/format drscheme:language:setup-printing-parameters value settings port 'infinity))
          
          (define/private (teaching-language-render-value/format setup-printing-parameters value settings port width)
            ;; set drscheme's printing parameters
            (setup-printing-parameters
             (λ ()
               ;; then adjust the settings for the teaching languages
               (set-printing-parameters
                settings
                (λ () 
                  (let-values ([(converted-value write?)
                                (call-with-values
                                    (lambda ()
                                      (drscheme:language:simple-module-based-language-convert-value value settings))
                                  (case-lambda
                                   [(converted-value) (values converted-value #t)]
                                   [(converted-value write?) (values converted-value write?)]))])
                    (let ([pretty-out (if write? pretty-write pretty-print)])
                      (cond
                       [(drscheme:language:simple-settings-insert-newlines settings)
                        (if (number? width)
                            (parameterize ([pretty-print-columns width])
                              (pretty-out converted-value port))
                            (pretty-out converted-value port))]
                       [else
                        (parameterize ([pretty-print-columns 'infinity])
                          (pretty-out converted-value port))
                        (newline port)]))))))
             settings
             width))
          
          (super-new)))
      
      ;; sharing/not-config-panel :  boolean boolean parent -> (case-> (-> settings) (settings -> void))
      ;; constructs the config-panel for a language without a sharing option.
      (define (sharing/not-config-panel allow-sharing-config? accept-quasiquote? _parent)
        (let* ([parent (make-object vertical-panel% _parent)]
               
               [input-panel (instantiate group-box-panel% ()
                              (parent parent)
                              (label (string-constant input-syntax))
                              (alignment '(left center)))]
               
               [output-panel (instantiate group-box-panel% ()
                               (parent parent)
                               (label (string-constant output-syntax))
                               (alignment '(left center)))]
               
               [tp-group-box (instantiate group-box-panel% ()
                               (label (string-constant teachpacks))
                               (parent parent)
                               (alignment '(center top)))]
               [tp-panel (new vertical-panel%
                              [parent tp-group-box]
                              [alignment '(center center)]
                              [stretchable-width #f]
                              [stretchable-height #f])]
               
               [case-sensitive (make-object check-box%
                                 (string-constant case-sensitive-label)
                                 input-panel
                                 void)]
               [output-style (make-object radio-box%
                               (string-constant output-style-label)
                               (if accept-quasiquote?
                                   (list (string-constant constructor-printing-style)
                                         (string-constant quasiquote-printing-style)
                                         (string-constant write-printing-style))
                                   (list (string-constant constructor-printing-style)
                                         (string-constant write-printing-style)))
                               output-panel
                               void)]
               [show-sharing #f]
               [insert-newlines (make-object check-box%
                                  (string-constant use-pretty-printer-label)
                                  output-panel
                                  void)]
               [tracing (new check-box%
                             (parent output-panel)
                             (label (string-constant tracing-enable-tracing))
                             (callback void))]
               [fraction-style
                (make-object radio-box% (string-constant fraction-style)
                  (list (string-constant use-mixed-fractions)
                        (string-constant use-repeating-decimals))
                  output-panel
                  void)]
               
               [tps '()])
          
          (when allow-sharing-config?
            (set! show-sharing
                  (instantiate check-box% ()
                    (parent output-panel)
                    (label (string-constant sharing-printing-label))
                    (callback void))))
          
          ;; set the characteristics of the GUI
          (send _parent set-alignment 'center 'center)
          (send parent stretchable-height #f)
          (send parent stretchable-width #f)
          (send parent set-alignment 'center 'center)
          
          (case-lambda
            [()
             (make-htdp-lang-settings
              (send case-sensitive get-value)
              (if accept-quasiquote?
                  (case (send output-style get-selection)
                    [(0) 'constructor]
                    [(1) 'quasiquote]
                    [(2) 'write])
                  (case (send output-style get-selection)
                    [(0) 'constructor]
                    [(1) 'write]))
              (case (send fraction-style get-selection)
                [(0) 'mixed-fraction]
                [(1) 'repeating-decimal])
              (and allow-sharing-config? (send show-sharing get-value))
              (send insert-newlines get-value)
              'none
              (send tracing get-value)
              tps)]
            [(settings)
             (send case-sensitive set-value (drscheme:language:simple-settings-case-sensitive settings))
             (send output-style set-selection
                   (if accept-quasiquote?
                       (case (drscheme:language:simple-settings-printing-style settings)
                         [(constructor) 0]
                         [(quasiquote) 1]
                         [(print trad-write write) 2])
                       (case (drscheme:language:simple-settings-printing-style settings)
                         [(constructor) 0]
                         [(quasiquote) 0]
                         [(print trad-write write) 1])))
             (send fraction-style set-selection
                   (case (drscheme:language:simple-settings-fraction-style settings)
                     [(mixed-fraction) 0]
                     [(repeating-decimal) 1]))
             (when allow-sharing-config?
               (send show-sharing set-value (drscheme:language:simple-settings-show-sharing settings)))
             (send insert-newlines set-value 
                   (drscheme:language:simple-settings-insert-newlines settings))
             (set! tps (htdp-lang-settings-teachpacks settings))
             (send tp-panel change-children (λ (l) '()))
             (if (null? tps)
                 (new message%
                      [parent tp-panel]
                      [label (string-constant teachpacks-none)])
                 (for-each
                  (λ (tp) (new message% 
                               [parent tp-panel]
                               [label (format "~s" tp)]))
                  tps))
             (send tracing set-value (htdp-lang-settings-tracing? settings))
             (void)])))
      
      (define simple-htdp-language%
        (class* drscheme:language:simple-module-based-language% (htdp-language<%>)
          (init-field sharing-printing
                      abbreviate-cons-as-list
                      allow-sharing?
                      manual
                      reader-module
                      (use-function-output-syntax? #f)
                      (accept-quasiquote? #t)
                      (read-accept-dot #f)
                      (style-delta #f))
          (define/public (get-sharing-printing) sharing-printing)
          (define/public (get-abbreviate-cons-as-list) abbreviate-cons-as-list)
          (define/public (get-allow-sharing?) allow-sharing?)
          (define/public (get-manual) manual)
          (define/public (get-use-function-output-syntax?) use-function-output-syntax?)
          (define/public (get-accept-quasiquote?) accept-quasiquote?)
          (define/public (get-read-accept-dot) read-accept-dot)
          ;(define/override (get-one-line-summary) one-line-summary)
          (define/public (get-htdp-style-delta) style-delta)
          
          (super-new [language-url "http://www.htdp.org/"])))
      
      (define (language-extension %)
        (class %
          (inherit get-manual)
          
          (define/override (extra-repl-information settings port) 
            (define welcome (drscheme:rep:get-welcome-delta))
            (define (go str sd)
              (let* ([s (make-object string-snip% str)]
                     [sl (editor:get-standard-style-list)]
                     [std (send sl find-named-style "Standard")]
                     [style (send sl find-or-create-style std sd)])
                (send s set-style style)
                (write-special s port)))
            
            (define tps (htdp-lang-settings-teachpacks settings))
            
            (unless (null? tps)
              (go "Teachpack" welcome)
              (cond
                [(= 1 (length tps))
                 (go ": " welcome)
                 (go (cadr (car tps)) (drscheme:rep:get-dark-green-delta))]
                [(= 2 (length tps))
                 (go "s: " welcome)
                 (go (cadr (car tps)) (drscheme:rep:get-dark-green-delta))
                 (go " and " welcome)
                 (go (cadr (cadr tps)) (drscheme:rep:get-dark-green-delta))]
                [else
                 (go "s: " welcome)
                 (go (cadr (car tps)) (drscheme:rep:get-dark-green-delta))
                 (let loop ([these-tps (cdr tps)])
                   (cond
                     [(null? (cdr these-tps))
                      (go ", and " welcome)
                      (go (cadr (car these-tps)) (drscheme:rep:get-dark-green-delta))]
                     [else
                      (go ", " welcome)
                      (go (cadr (car these-tps)) (drscheme:rep:get-dark-green-delta))
                      (loop (cdr these-tps))]))])
              (go "." welcome)
              (newline port)))
          
          (define/override (first-opened settings)
            (for ([tp (in-list (htdp-lang-settings-teachpacks settings))])
              (namespace-require/constant tp)))
          
          (inherit get-module get-transformer-module get-init-code
                   use-namespace-require/copy?)
          (define/override (create-executable setting parent program-filename)
            (let ([dist-filename
		   (drscheme:language:put-executable
		    parent program-filename
		    'distribution
		    #t 
		    (string-constant save-a-mred-distribution))])
              (when dist-filename
                (drscheme:language:create-distribution-for-executable 
                 dist-filename
                 #t
                 (λ (exe-name)
                   (create-htdp-lang-executable program-filename exe-name reader-module))))))

          (define/private (get-export-names sexp)
            (let* ([sym-name ((current-module-name-resolver) sexp #f #f)]
                   [no-ext-name (substring (symbol->string sym-name)
                                           1
                                           (string-length (symbol->string sym-name)))]
                   [full-name
                    (cond
                      [(file-exists? (string-append no-ext-name ".ss"))
                       (string-append no-ext-name ".ss")]
                      [(file-exists? (string-append no-ext-name ".scm"))
                       (string-append no-ext-name ".scm")]
                      [(file-exists? no-ext-name)
                       no-ext-name]
                      [else (error 'htdp-lang.rkt "could not find language filename ~s" no-ext-name)])]
                   [base-dir (let-values ([(base _1 _2) (split-path full-name)]) base)]
                   [stx
                    (call-with-input-file full-name
                      (lambda (port)
                        (read-syntax full-name port)))]
                   [code
                    (parameterize ([current-load-relative-directory base-dir]
                                   [current-directory base-dir])
                      (expand stx))]
                   [find-name
                    (lambda (p)
                      (cond
                        [(symbol? p) p]
                        [(and (pair? p) (pair? (cdr p)))
                         (cadr p)]
                        [else (car p)]))])
              (append
               (map find-name (syntax-property code 'module-variable-provides))
               (map find-name (syntax-property code 'module-syntax-provides)))))

          (define/private (symbol-append x y)
            (string->symbol
             (string-append
              (symbol->string x)
              (symbol->string y))))
          
          (inherit get-htdp-style-delta)
          (define/override (get-style-delta)
            (get-htdp-style-delta))
          
          (inherit get-reader set-printing-parameters)
          
          (define/override (front-end/complete-program port settings)
            (expand-teaching-program port  
                                     (get-reader)
                                     (get-module)
                                     (htdp-lang-settings-teachpacks settings)))
          
          (define/override (front-end/interaction port settings)
            (let ([t (super front-end/interaction port settings)]
		  [start? #t]
                  [done? #f])
              (λ ()
                (cond
		  [start?
		   (set! start? #f)
		   #'(#%plain-app reset-tests)]
                  [done? eof]
                  [else
                   (let ([ans (parameterize ([read-accept-lang #f])
                                (t))])
                     (cond
                       [(eof-object? ans)
                        (set! done? #t)
                        #`(test)]
                       [else
                        ans]))]))))


          (define keywords #f)
          (define/augment (capability-value key)
            (case key
              [(drscheme:autocomplete-words)
               (unless keywords 
                 (set! keywords (text:get-completions/manuals #f))) ;; complete with everything, which is wrong ..
               keywords]
              [(drscheme:teachpack-menu-items) htdp-teachpack-callbacks]
              [(drscheme:special:insert-lambda) #f]
              [(drscheme:help-context-term)
               (let* ([m (get-module)]
                      [m (and m (pair? m) (pair? (cdr m)) (cadr m))]
                      [m (and m (regexp-match #rx"^(lang/[^/.]+).ss$" m))]
                      [m (and m (cadr m))])
                 (if m
                   (format "O:{ L:~a T:teachpack T:picturing-programs }" m)
                   (error 'drscheme:help-context-term
                          "internal error: unexpected module spec")))]
              [(tests:test-menu tests:dock-menu) #t]
              [else (inner (drscheme:language:get-capability-default key) 
                           capability-value
                           key)]))
          
          (define htdp-teachpack-callbacks
            (drscheme:unit:make-teachpack-callbacks
             (λ (settings) 
               (map cadr (htdp-lang-settings-teachpacks settings)))
             (λ (settings parent) 
               (let ([teachpack (get-teachpack-from-user parent)])
                 (if teachpack
                     (let ([old-tps (htdp-lang-settings-teachpacks settings)])
                       (if (member teachpack old-tps)
                           (begin
                             (message-box (string-constant drscheme)
                                          (format (string-constant already-added-teachpack)
                                                  (cadr teachpack))
                                          #:dialog-mixin frame:focus-table-mixin)
                             settings)
                           
                           (let ([new-tps (append old-tps (list teachpack))])
                             (preferences:set 'drracket:htdp:last-set-teachpacks new-tps)
                             (make-htdp-lang-settings
                              (drscheme:language:simple-settings-case-sensitive settings)
                              (drscheme:language:simple-settings-printing-style settings)
                              (drscheme:language:simple-settings-fraction-style settings)
                              (drscheme:language:simple-settings-show-sharing settings)
                              (drscheme:language:simple-settings-insert-newlines settings)
                              (drscheme:language:simple-settings-annotations settings)
                              (htdp-lang-settings-tracing? settings)
                              new-tps))))
                     settings)))
             (λ (settings name) 
               (let ([new-tps (filter (λ (x) (not (equal? (cadr x) name)))
                                      (htdp-lang-settings-teachpacks settings))])
                 (preferences:set 'drracket:htdp:last-set-teachpacks new-tps)
                 (make-htdp-lang-settings
                  (drscheme:language:simple-settings-case-sensitive settings)
                  (drscheme:language:simple-settings-printing-style settings)
                  (drscheme:language:simple-settings-fraction-style settings)
                  (drscheme:language:simple-settings-show-sharing settings)
                  (drscheme:language:simple-settings-insert-newlines settings)
                  (drscheme:language:simple-settings-annotations settings)
                  (htdp-lang-settings-tracing? settings)
                  new-tps)))
             (λ (settings) 
               (preferences:set 'drracket:htdp:last-set-teachpacks '())
               (make-htdp-lang-settings
                (drscheme:language:simple-settings-case-sensitive settings)
                (drscheme:language:simple-settings-printing-style settings)
                (drscheme:language:simple-settings-fraction-style settings)
                (drscheme:language:simple-settings-show-sharing settings)
                (drscheme:language:simple-settings-insert-newlines settings)
                (drscheme:language:simple-settings-annotations settings)
                (htdp-lang-settings-tracing? settings)
                '()))))
        
          (inherit-field reader-module)
          (define/override (get-reader-module) reader-module)
          (define/override (get-metadata modname settings)
            (string-append
             (apply string-append
                    (map (λ (x) (string-append x "\n"))
                         htdp-save-file-prefix))
             (format "#reader~s~s\n"
                     reader-module
                     `((modname ,modname)
                       (read-case-sensitive ,(drscheme:language:simple-settings-case-sensitive settings))
                       (teachpacks ,(htdp-lang-settings-teachpacks settings))
                       (htdp-settings ,(htdp-lang-settings->vector settings))))))
          
          (inherit default-settings)
          (define/override (metadata->settings metadata)
            (let* ([table (massage-metadata (metadata->table metadata))] ;; extract the table
                   [ssv (assoc 'htdp-settings table)])
              (if ssv
                  (let ([settings-list (vector->list (cadr ssv))])
                    (if (equal? (length settings-list)
                                (procedure-arity make-htdp-lang-settings))
                        (apply make-htdp-lang-settings settings-list)
                        (default-settings)))
                  (default-settings))))
          
          (define/private (massage-metadata md)
            (if (and (list? md)
                     (andmap (λ (x) (and (pair? x) (symbol? (car x)))) md))
                md
                '()))
          
          (define/private (metadata->table metadata)
            (with-handlers ((exn:fail:read? (λ (x) #f)))
              (let ([p (open-input-string metadata)])
                (regexp-match #rx"\n#reader" p) ;; skip to reader
                (read p) ;; skip module
                (read p))))
          
          (define/override (get-metadata-lines) 3)
          
          (super-new)))
      
      (define (get-teachpack-from-user parent)
        (define tp-dirs (list "htdp" "2htdp"))
        (define labels (list (string-constant teachpack-pre-installed/htdp)
                             (string-constant teachpack-pre-installed/2htdp)))
        (define tpss (map (λ (tp-dir)
                            (let ([base (collection-path "teachpack" tp-dir)])
                              (filter
                               (λ (x) (file-exists? (build-path base x)))
                               (directory-list base))))
                          tp-dirs))
        (define sort-order (λ (x y) (string<=? (path->string x) (path->string y))))
        (define pre-installed-tpss (map (λ (tps) (sort tps sort-order)) tpss))
        (define dlg (new (frame:focus-table-mixin dialog%) [parent parent] [label (string-constant drscheme)]))
        (define hp (new horizontal-panel% [parent dlg]))
        (define answer #f)
        (define compiling? #f)
        
        (define pre-installed-gbs (map (λ (tps label)
                                         (new group-box-panel%
                                              [label label]
                                              [parent hp]))
                                       tpss labels))
        (define user-installed-gb (new group-box-panel%
                                       [label (string-constant teachpack-user-installed)]
                                       [parent hp]))
        
        (define pre-installed-lbs
          (map (λ (pre-installed-gb pre-installed-tps)
                 (new list-box%
                      [label #f]
                      [choices (map path->string pre-installed-tps)]
                      [stretchable-height #t]
                      [min-height 300]
                      [min-width 200]
                      [callback
                       (λ (this evt)
                         (case (send evt get-event-type)
                           [(list-box-dclick) (selected this)]
                           [else
                            (for-each (λ (x) (unless (eq? x this) (clear-selection x)))
                                      (cons user-installed-lb
                                            pre-installed-lbs))
                            (update-button)]))]
                      [parent pre-installed-gb]))
               pre-installed-gbs
               pre-installed-tpss))
        
        (define user-installed-lb
          (new list-box%
               [label #f]
               [choices '()]
               [stretchable-height #t]
               [min-width 200]
               [callback
                (λ (x evt)
                  (case (send evt get-event-type)
                    [(list-box-dclick) (selected user-installed-lb)]
                    [else
                     (for ([pre-installed-lb (in-list pre-installed-lbs)])
                       (clear-selection pre-installed-lb))
                     (update-button)]))]
               [parent user-installed-gb]))
        
        (define (selected lb)
          (when (send lb get-selection)
            (unless compiling?
              (set! answer (figure-out-answer))
              (send dlg show #f))))
        
        (define (clear-selection lb)
          (for-each
           (λ (x) (send lb select x #f))
           (send lb get-selections)))
        
        (define add-button (new button%
                                [parent user-installed-gb]
                                [label (string-constant add-teachpack-to-list...)]
                                [callback (λ (x y) (install-teachpack))]))
        
        (define (install-teachpack)
          (let ([file (get-file (string-constant select-a-teachpack) dlg)])
            (when file
              (let-values ([(base name dir) (split-path file)])
                (let ([dest-file (build-path teachpack-installation-dir name)])
                  (when (or (not (file-exists? dest-file))
                            (equal? 1
                                    (message-box/custom
                                     (string-constant drscheme)
                                     (format
                                      (string-constant teachpack-already-installed)
                                      (path->string name))
                                     (string-constant overwrite)
                                     (string-constant cancel)
                                     #f
                                     dlg
                                     '(default=2 caution))))
                    (make-directory* teachpack-installation-dir)
                    (when (file-exists? dest-file)
                      (delete-file dest-file))
                    (copy-file file dest-file)
                    
                    ;; compiling the teachpack should be the last thing in this GUI callback
                    (compile-new-teachpack dest-file)))))))
        
        (define (compile-new-teachpack filename)
          (let-values ([(_1 short-name _2) (split-path filename)])
            (cond
              [(cannot-compile? filename)
               (post-compilation-gui-cleanup short-name)]
              [else
               (send compiling-message set-label
                     (format (string-constant compiling-teachpack) 
                             (path->string short-name)))
               (starting-compilation)
               (let ([nc (make-custodian)]
                     [exn #f])
                 (let ([t 
                        (parameterize ([current-custodian nc])
                          (thread (λ () 
                                    (with-handlers ((exn? (λ (x) (set! exn x))))
                                      (parameterize ([current-namespace (make-base-namespace)])
                                        (with-module-reading-parameterization
                                         (lambda ()
                                           (compile-file filename))))))))])
                   (thread
                    (λ ()
                      (thread-wait t)
                      (queue-callback
                       (λ () 
                         (cond
                           [exn
                            (message-box (string-constant drscheme)
                                         (exn-message exn))
                            (delete-file filename)
                            (update-user-installed-lb)]
                           [else
                            (post-compilation-gui-cleanup short-name)])
                         (done-compilation)
                         (send compiling-message set-label "")))))))])))
        
        (define (post-compilation-gui-cleanup short-name)
          (update-user-installed-lb)
          (for ([pre-installed-lb (in-list pre-installed-lbs)])
            (clear-selection pre-installed-lb))
          (send user-installed-lb set-string-selection (path->string short-name)))
        
        (define (starting-compilation)
          (set! compiling? #t)
          (update-button)
          (send cancel-button enable #f))
        
        (define (done-compilation)
          (set! compiling? #f)
          (update-button)
          (send cancel-button enable #t))
        
        (define (update-user-installed-lb)
          (let ([files
                 (if (directory-exists? teachpack-installation-dir)
                     (map path->string 
                          (filter 
                           (λ (x) (file-exists? (build-path teachpack-installation-dir x)))
                           (directory-list teachpack-installation-dir)))
                     '())])
            (send user-installed-lb set (sort files string<=?))))
        
        
        (define (update-button)
          (send ok-button enable 
                (and (not compiling?)
                     (or (pair? (send user-installed-lb get-selections))
                         (ormap (λ (pre-installed-lb)
                                  (pair? (send pre-installed-lb get-selections)))
                                pre-installed-lbs)))))
        
        (define button-panel (new horizontal-panel% 
                                  [parent dlg]
                                  [alignment '(right center)]
                                  [stretchable-height #f]))
        (define compiling-message (new message% [parent button-panel] [label ""] [stretchable-width #t]))
        (define-values (ok-button cancel-button)
          (gui-utils:ok/cancel-buttons button-panel
                                       (λ (b e)
                                         (set! answer (figure-out-answer))
                                         (send dlg show #f))
                                       (λ (b e) 
                                         (send dlg show #f))
                                       (string-constant ok) (string-constant cancel)))
        
        (define (figure-out-answer)
          (cond
            [(ormap (λ (pre-installed-lb tp-dir) 
                      (and (send pre-installed-lb get-selection)
                           (list tp-dir (send pre-installed-lb get-string (send pre-installed-lb get-selection)))))
                    pre-installed-lbs
                    tp-dirs)
             =>
             (λ (pr)
               (define-values (tp-dir f) (apply values pr))
               `(lib ,f "teachpack" ,tp-dir))]
            [(send user-installed-lb get-selection)
             =>
             (λ (i) `(lib ,(send user-installed-lb get-string i)
                          ,user-installed-teachpacks-collection))]
            [else (error 'figure-out-answer "no selection!")]))
        
        
        (send ok-button enable #f)
        (update-user-installed-lb)
        
        (send dlg show #t)
        answer)

      (define (stepper-settings-language %)
        (if (implementation? % stepper-language<%>)
            (class* % (stepper-language<%>)
              (init-field stepper:supported)
              (init-field stepper:enable-let-lifting)
	      (init-field stepper:show-lambdas-as-lambdas)
              (define/override (stepper:supported?) stepper:supported)
              (define/override (stepper:enable-let-lifting?) stepper:enable-let-lifting)
              (define/override (stepper:show-lambdas-as-lambdas?) stepper:show-lambdas-as-lambdas)
              (super-new))
            (class* % ()
              (init stepper:supported)
              (init stepper:enable-let-lifting)
              (init stepper:show-lambdas-as-lambdas)
              (super-new))))

      (define (debugger-settings-language %)
        (if (implementation? % debugger-language<%>)
            (class* % (debugger-language<%>)
              (init-field [debugger:supported #f])
              (define/override (debugger:supported?) debugger:supported)
              (super-new))
            (class %
              (init [debugger:supported #f])
              (super-new))))

      ;; filter/hide-ids : syntax[list] -> listof syntax
      (define (filter/hide-ids ids)
        ;; When a `define-values' or `define-syntax' declaration
        ;; is macro-generated, if the defined name also originates
        ;; from a macro, then the name is hidden to anything
        ;; that wasn't generated by the same macro invocation. This
        ;; hiding relies on renaming at the symbol level, and it's
        ;; exposed by the fact that `syntax-e' of the identifier 
        ;; returns a different name than `identifier-binding'.
        (filter
         (lambda (id)
           (let ([ib (identifier-binding id)])
             ;; ib should always be a 4-elem list, but
             ;; check, just in case:
             (or (not (pair? ib)) 
                 (eq? (syntax-e id)
                      (cadr ib)))))
         (syntax->list ids)))
                 
      
      ;                                                                                              
      ;                                                                                              
      ;                                                                                              
      ;   ;                                                             ;                     ;      
      ;   ;                                                             ;                     ;      
      ;   ;                                                             ;                     ;      
      ;   ; ;;    ;;;    ; ;;     ;; ;   ; ;;  ;;    ;;;    ; ;;        ; ;;    ;;;     ;;;   ;   ;  
      ;   ;;  ;  ;   ;   ;;  ;   ;  ;;   ;;  ;;  ;  ;   ;   ;;  ;       ;;  ;  ;   ;   ;   ;  ;  ;   
      ;   ;   ;      ;   ;   ;  ;    ;   ;   ;   ;      ;   ;   ;       ;   ;      ;  ;       ; ;    
      ;   ;   ;   ;;;;   ;   ;  ;    ;   ;   ;   ;   ;;;;   ;   ;       ;   ;   ;;;;  ;       ;;;    
      ;   ;   ;  ;   ;   ;   ;  ;    ;   ;   ;   ;  ;   ;   ;   ;       ;   ;  ;   ;  ;       ;  ;   
      ;   ;   ;  ;   ;   ;   ;   ;  ;;   ;   ;   ;  ;   ;   ;   ;       ;   ;  ;   ;   ;   ;  ;   ;  
      ;   ;   ;   ;;;;;  ;   ;    ;; ;   ;   ;   ;   ;;;;;  ;   ;       ;   ;   ;;;;;   ;;;   ;    ; 
      ;                              ;                                                               
      ;                         ;    ;                                                               
      ;                          ;;;;                                                                
      

      ;; this inspector should be powerful enough to see
      ;; any structure defined in the user's namespace
      (define drscheme-inspector (current-inspector))
      (eval `(,#'module drscheme-secrets mzscheme
                        (provide drscheme-inspector)
                        (define drscheme-inspector ,drscheme-inspector)))
      (namespace-require ''drscheme-secrets)
      
      
      
      ;                                                               
      ;                                                               
      ;                                                               
      ;                                                               
      ;                                                               
      ;                                 ;                             
      ;    ;;;   ; ;  ; ;   ;;;    ; ; ;;;;  ; ;  ;;;     ;;;    ;;;  
      ;   ;   ;  ;;   ;;   ;   ;   ;;   ;    ;;  ;   ;   ;   ;  ;   ; 
      ;  ;    ;  ;    ;   ;     ;  ;    ;    ;       ;  ;      ;    ; 
      ;  ;;;;;;  ;    ;   ;     ;  ;    ;    ;    ;;;;  ;      ;;;;;; 
      ;  ;       ;    ;   ;     ;  ;    ;    ;   ;   ;  ;      ;      
      ;   ;      ;    ;    ;   ;   ;    ;    ;   ;   ;   ;   ;  ;     
      ;    ;;;;  ;    ;     ;;;    ;     ;;  ;    ;;;;;   ;;;    ;;;; 
      ;                                                               
      ;                                                               
      ;                                                               
      
      
      
      
      (define mf-note
        (let ([bitmap
               (make-object bitmap%
                 (build-path (collection-path "icons") "mf.gif"))])
          (and (send bitmap ok?)
               (make-object image-snip% bitmap))))
      
      ;; teaching-languages-error-display-handler : 
      ;;    (string (union TST exn) -> void) -> string exn -> void
      ;; adds in the bug icon, if there are contexts to display
      (define (teaching-languages-error-display-handler msg exn)
          (if (exn? exn)
              (display (get-rewriten-error-message exn) (current-error-port))
              (eprintf "uncaught exception: ~e" exn))
          (eprintf "\n")

          ;; need to flush here so that error annotations inserted in next line
          ;; don't get erased if this output were to happen after the insertion
          (flush-output (current-error-port))
          
          (let ([rep (drscheme:rep:current-rep)])
            (when (and (is-a? rep drscheme:rep:text<%>)
                       (eq? (send rep get-err-port) (current-error-port)))
              (let ([to-highlight 
                     (cond
                       [(exn:srclocs? exn) 
                        ((exn:srclocs-accessor exn) exn)]
                       [(exn? exn) 
                        (let ([cms (continuation-mark-set->list 
                                    (exn-continuation-marks exn)
                                    teaching-languages-continuation-mark-key)])
			  (cond
			   ((not cms) '())
			   ((findf (lambda (mark)
				     (and mark
					  (or (path? (car mark))
					      (symbol? (car mark)))))
				   cms)
			    => (lambda (mark)
				 (apply (lambda (source line col pos span)
					  (list (make-srcloc source line col pos span)))
					mark)))
			   (else '())))]
		       [else '()])])

                (parameterize ([current-eventspace drs-eventspace])
                  (queue-callback
                   (lambda ()
                     ;; need to make sure that the user's eventspace is still the same
                     ;; and still running here?
                     (send rep highlight-errors to-highlight #f))))))))
      
      ;; with-mark : syntax syntax exact-nonnegative-integer -> syntax
      ;; a member of stacktrace-imports^
      ;; guarantees that the continuation marks associated with teaching-languages-continuation-mark-key are
      ;; members of the debug-source type
      (define (with-mark source-stx expr phase)
        (let ([source (syntax-source source-stx)]
              [line (syntax-line source-stx)]
              [col (syntax-column source-stx)]
              [start-position (syntax-position source-stx)]
              [span (syntax-span source-stx)])
          (if (and (or (symbol? source) (path? source))
                   (number? start-position)
                   (number? span))
              (with-syntax ([expr expr]
                            [mark (list source line col start-position span)]
                            [teaching-languages-continuation-mark-key teaching-languages-continuation-mark-key])
                #`(with-continuation-mark 'teaching-languages-continuation-mark-key
                    'mark
                    expr))
              expr)))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  profiling infrastructure. Not used.
      ;;
      
      (define profile-key (gensym))
      (define (profiling-enabled) #f)
      (define (initialize-profile-point . x) (void))
      (define (register-profile-start . x) #f)
      (define (register-profile-done . x) (void))
      
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  test coverage
      ;;
      
      (define test-coverage-enabled (make-parameter #t))
      (define current-test-coverage-info (make-thread-cell #f))
      
      (define (initialize-test-coverage-point expr)
        (unless (thread-cell-ref current-test-coverage-info)
          (let ([ht (make-hasheq)])
            (thread-cell-set! current-test-coverage-info ht)
            (let ([rep (drscheme:rep:current-rep)])
              (when rep
                (parameterize ([current-eventspace drs-eventspace])
                  (queue-callback
                   (λ ()
                     (define sl (editor:get-standard-style-list))
                     (define on-s (send sl find-named-style test-coverage-on-style-name))
                     (define off-s (send sl find-named-style test-coverage-off-style-name))
                     (send rep set-test-coverage-info ht on-s off-s #f))))))))
        (let ([ht (thread-cell-ref current-test-coverage-info)])
          (when ht
            (hash-set! ht expr #;(box #f) (mcons #f #f)))))
      
      (define (test-covered expr)
        (let* ([ht (or (thread-cell-ref current-test-coverage-info)
                       (error 'htdp-langs
                              "internal-error: no test-coverage table"))]
               [v (hash-ref ht expr
                    (lambda ()
                      (error 'htdp-langs
                             "internal-error: expression not found: ~.s"
                             expr)))])
          #; (lambda () (set-box! v #t))
          (with-syntax ([v v]) #'(#%plain-app set-mcar! v #t))))
      
      (define-values/invoke-unit et:stacktrace@
        (import et:stacktrace-imports^) (export (prefix et: et:stacktrace^)))
     
      ;; add-annotation : boolean (sexp -> value) -> sexp -> value
      ;; adds debugging and test coverage information to `sexp' and calls `oe'
      (define (add-annotation tracing? oe)
        (let ([teaching-language-eval-handler
               (lambda (exp)
                 (let* ([is-compiled? (compiled-expression? (if (syntax? exp) (syntax-e exp) exp))]
                        [annotated
                         (if is-compiled? 
                             exp
                             (let* ([et-annotated (et:annotate-top (expand exp) 
                                                                   (namespace-base-phase))]
                                    [tr-annotated
                                     (if tracing?
                                         (drscheme:tracing:annotate (expand et-annotated))
                                         et-annotated)])
                               tr-annotated))])
                   (oe annotated)))])
          teaching-language-eval-handler))
      
     
      
;                                                                                                                
;                                                                                                                
;                                                                                                                
;                            ;                   ;   ;                                        ;                  
;                                                ;   ;                                        ;                  
;                   ;            ;               ;   ;       ;                           ;    ;                  
;   ; ;;    ;   ;  ;;;;      ;  ;;;;      ;;;    ;   ;      ;;;;   ;;;     ;; ;    ;;;  ;;;;  ; ;;     ;;;   ; ; 
;   ;;  ;   ;   ;   ;        ;   ;       ;   ;   ;   ;       ;    ;   ;   ;  ;;   ;   ;  ;    ;;  ;   ;   ;  ;;  
;   ;    ;  ;   ;   ;        ;   ;           ;   ;   ;       ;   ;     ; ;    ;  ;    ;  ;    ;   ;  ;    ;  ;   
;   ;    ;  ;   ;   ;        ;   ;        ;;;;   ;   ;       ;   ;     ; ;    ;  ;;;;;;  ;    ;   ;  ;;;;;;  ;   
;   ;    ;  ;   ;   ;        ;   ;       ;   ;   ;   ;       ;   ;     ; ;    ;  ;       ;    ;   ;  ;       ;   
;   ;;  ;   ;  ;;   ;        ;   ;       ;   ;   ;   ;       ;    ;   ;   ;  ;;   ;      ;    ;   ;   ;      ;   
;   ; ;;     ;; ;    ;;      ;    ;;      ;;;;;  ;   ;        ;;   ;;;     ;; ;    ;;;;   ;;  ;   ;    ;;;;  ;   
;   ;                                                                         ;                                  
;   ;                                                                    ;    ;                                  
;   ;                                                                     ;;;;                                   
      
      
      ;; add-htdp-language : (instanceof htdp-language<%>) -> void
      (define (add-htdp-language o)
        (drscheme:language-configuration:add-language o))
      
      (define (phase1) (void))

      ;; phase2 : -> void
      (define (phase2)
        (define htdp-language%
          (stepper-settings-language
           (debugger-settings-language
            ((drscheme:language:get-default-mixin)
             (language-extension
              (drscheme:language:module-based-language->language-mixin
               (module-based-language-extension
                (drscheme:language:simple-module-based-language->module-based-language-mixin
                 simple-htdp-language%))))))))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant advanced-one-line-summary))
           (module '(lib "lang/htdp-advanced.ss"))
           (manual #"advanced")
           (language-position
            (list (string-constant teaching-languages)
                  (string-constant how-to-design-programs)
                  (string-constant advanced-student)))
           (language-id "plt:advanced-student")
           (language-numbers '(-500 -500 5))
           (sharing-printing #t)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #t)
           (reader-module '(lib "htdp-advanced-reader.ss" "lang"))
           (debugger:supported #t)
	   (stepper:supported #f)
	   (stepper:enable-let-lifting #t)
	   (stepper:show-lambdas-as-lambdas #t)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant intermediate/lambda-one-line-summary))
           (module '(lib "lang/htdp-intermediate-lambda.ss"))
           (manual #"intermediate-lambda")
           (language-position
            (list (string-constant teaching-languages)
                  (string-constant how-to-design-programs)
                  (string-constant intermediate-student/lambda)))
           (language-id "plt:intermediate-student/lambda")
           (style-delta (let ([match (regexp-match-positions
                                      "lambda"
                                      (string-constant intermediate-student/lambda))])
                          (if match
                              (let ([pos (car match)])
                                (list (list (make-object style-delta% 'change-family 'modern)
                                            (car pos)
                                            (cdr pos))))
                              #f)))
           (language-numbers '(-500 -500 4))
           (sharing-printing #f)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #f)
           (reader-module '(lib "htdp-intermediate-lambda-reader.ss" "lang"))
	   (stepper:supported #t)
           (stepper:enable-let-lifting #t)
	   (stepper:show-lambdas-as-lambdas #t)))
        
	(add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant intermediate-one-line-summary))
           (module '(lib "lang/htdp-intermediate.ss"))
           (manual #"intermediate")
           (language-position
            (list (string-constant teaching-languages)
                  (string-constant how-to-design-programs)
                  (string-constant intermediate-student)))
           (language-id "plt:intermediate-student")
           (language-numbers '(-500 -500 3))
           (sharing-printing #f)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #f)
           (use-function-output-syntax? #t)
           (reader-module '(lib "htdp-intermediate-reader.ss" "lang"))
	   (stepper:supported #t)
           (stepper:enable-let-lifting #t)
	   (stepper:show-lambdas-as-lambdas #f)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant beginning/abbrev-one-line-summary))
           (module '(lib "lang/htdp-beginner-abbr.ss"))
           (manual #"beginning-abbr")
           (language-position
            (list (string-constant teaching-languages)
                  (string-constant how-to-design-programs)
                  (string-constant beginning-student/abbrev)))
           (language-id "plt:beginning-student/abbrev")
           (language-numbers '(-500 -500 2))
           (sharing-printing #f)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #f)
           (reader-module '(lib "htdp-beginner-abbr-reader.ss" "lang"))
	   (stepper:supported #t)
           (stepper:enable-let-lifting #t)
	   (stepper:show-lambdas-as-lambdas #f)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant beginning-one-line-summary))
           (module '(lib "lang/htdp-beginner.ss"))
           (manual #"beginning")
           (language-position
            (list (string-constant teaching-languages)
                  (string-constant how-to-design-programs)
                  (string-constant beginning-student)))
           (language-numbers '(-500 -500 1))
           (language-id "plt:beginning-student")
           (sharing-printing #f)
           (abbreviate-cons-as-list #f)
           (allow-sharing? #f)
           (accept-quasiquote? #f)
           (reader-module '(lib "htdp-beginner-reader.ss" "lang"))
	   (stepper:supported #t)
           (stepper:enable-let-lifting #t)
	   (stepper:show-lambdas-as-lambdas #f))))
      
      (define test-coverage-on-style-name "plt:htdp:test-coverage-on")
      (define test-coverage-off-style-name "plt:htdp:test-coverage-off")
      (define test-coverage-on-style-pref (string->symbol test-coverage-on-style-name))
      (define test-coverage-off-style-pref (string->symbol test-coverage-off-style-name))
      
      (color-prefs:register-color-preference test-coverage-on-style-pref
                                             test-coverage-on-style-name
                                             (send the-color-database find-color "black")
                                             (send the-color-database find-color "white"))
      (color-prefs:register-color-preference test-coverage-off-style-pref
                                             test-coverage-off-style-name
                                             (send the-color-database find-color "orange")
                                             (send the-color-database find-color "indianred")
                                             #:background (send the-color-database find-color "black"))
      (color-prefs:add-to-preferences-panel 
       "HtDP Languages"
       (λ (parent)
         (color-prefs:build-color-selection-panel parent
                                                  test-coverage-on-style-pref
                                                  test-coverage-on-style-name
                                                  (string-constant test-coverage-on))
         (color-prefs:build-color-selection-panel parent
                                                  test-coverage-off-style-pref
                                                  test-coverage-off-style-name
                                                  (string-constant test-coverage-off)
                                                  #:background? #t)))
      
      (define (update-sds white-on-black?)
        (define sl (editor:get-standard-style-list))
        (define on-s (send sl find-named-style test-coverage-on-style-name))
        (define off-s (send sl find-named-style test-coverage-off-style-name))
        (define on-sd (make-object style-delta%))
        (define off-sd (make-object style-delta%))
        (send on-s get-delta on-sd)
        (send off-s get-delta off-sd)
        (cond
          [white-on-black?
           (send on-sd set-delta-foreground "white")
           (send off-sd set-delta-foreground "indianred")
           (send off-sd set-delta-background "black")]
          [else
           (send on-sd set-delta-foreground "black")
           (send off-sd set-delta-foreground "orange")
           (send off-sd set-delta-background "black")])
        (preferences:set test-coverage-on-style-pref on-sd)
        (preferences:set test-coverage-off-style-pref off-sd))
      
      (preferences:add-callback 'framework:white-on-black?
                                (λ (p v) (update-sds v)))))
