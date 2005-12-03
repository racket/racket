#|

tracing todo:
 - shorten lines

;; we don't use the built in debugging, use our own
;; version here that has no bug icon and only
;; annotates code that comes from editors.

|#

(module htdp-langs mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "framework.ss" "framework")
           (prefix et: (lib "stacktrace.ss" "errortrace"))
           (prefix tr: (lib "stacktrace.ss" "trace"))
           (lib "pretty.ss")
           (prefix pc: (lib "pconvert.ss"))
           (lib "unitsig.ss")
           (lib "class.ss")
           (lib "list.ss")
           (lib "file.ss")
           (lib "port.ss")
           (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "bday.ss" "framework" "private")
           (lib "moddep.ss" "syntax")
           (lib "cache-image-snip.ss" "mrlib")
           
           ;; this module is shared between the drscheme's namespace (so loaded here) 
           ;; and the user's namespace in the teaching languages
           "private/set-result.ss")
  
  (provide tool@)
  
  (define sc-tracing (string-constant tracing-enable-tracing))
  (define sc-show-tracing-window (string-constant tracing-show-tracing-window))
  (define sc-hide-tracing-window (string-constant tracing-hide-tracing-window))
  (define sc-tracing-nothing-to-show (string-constant tracing-tracing-nothing-to-show))
  
  (define ellipses-cutoff 200)
  
  (define o (current-output-port))
  (define (oprintf . args) (apply fprintf o args))
  
  (define init-eventspace (current-eventspace))
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      (define-local-member-name
        get-tracing-text
        show-tracing
        tracing:add-line
        tracing:rest)
      (define tab-tracing<%>
        (interface ()
          get-tracing-text
          get-any-results?
          tracing:add-line
          tracing:reset))
      
      
      (define drs-eventspace (current-eventspace))
      
      (define-struct (htdp-lang-settings drscheme:language:simple-settings) (tracing?))
      
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
             #f))
          
          (define/override (default-settings? s)
            (and (super default-settings? s)
                 (not (htdp-lang-settings-tracing? s))))
          
          (define/override (marshall-settings x)
            (list (super marshall-settings x)
                  (htdp-lang-settings-tracing? x)))
          
          (define/override (unmarshall-settings x)
            (if (and (pair? x)
                     (pair? (cdr x))
                     (null? (cddr x))
                     (boolean? (cadr x)))
                (let ([drs-settings (super unmarshall-settings (first x))])
                  (make-htdp-lang-settings
                   (drscheme:language:simple-settings-case-sensitive drs-settings)
                   (drscheme:language:simple-settings-printing-style  drs-settings)
                   (drscheme:language:simple-settings-fraction-style  drs-settings)
                   (drscheme:language:simple-settings-show-sharing  drs-settings)
                   (drscheme:language:simple-settings-insert-newlines  drs-settings)
                   (drscheme:language:simple-settings-annotations drs-settings)
                   (cadr x)))
                (default-settings)))
          
          (inherit get-allow-sharing? get-use-function-output-syntax? 
                   get-accept-quasiquote? get-read-accept-dot)
          (define/override (config-panel parent)
            (sharing/not-config-panel (get-allow-sharing?) parent))
          
          (define/override (on-execute settings run-in-user-thread)
            (let ([drs-namespace (current-namespace)]
                  [set-result-module-name 
                   ((current-module-name-resolver) '(lib "set-result.ss" "lang" "private") #f #f)])
              (run-in-user-thread
               (lambda ()
                 (read-accept-quasiquote (get-accept-quasiquote?))
                 (namespace-attach-module drs-namespace 'drscheme-secrets)
                 (namespace-attach-module drs-namespace set-result-module-name)
                 (error-display-handler teaching-languages-error-display-handler)
                 (current-eval (add-annotation (htdp-lang-settings-tracing? settings) (current-eval)))
                 (error-print-source-location #f)
                 (read-decimal-as-inexact #f)
                 (read-accept-dot (get-read-accept-dot)))))
            (super on-execute settings run-in-user-thread))

	  ;; set-printing-parameters : settings ( -> TST) -> TST
	  ;; is implicitly exposed to the stepper.  watch out!  --  john
          (define/public (set-printing-parameters settings thunk)
            (parameterize ([pc:booleans-as-true/false #t]
                           [pc:abbreviate-cons-as-list (get-abbreviate-cons-as-list)]
                           [pc:current-print-convert-hook
                            (let ([ph (pc:current-print-convert-hook)])
                              (lambda (val basic sub)
                                (cond
                                  [(equal? val set!-result) '(void)]
                                  [else (ph val basic sub)])))]
                           [pretty-print-show-inexactness #t]
                           [pretty-print-exact-as-decimal #t]
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
            (set-printing-parameters
             settings
             (lambda ()
               (super render-value/format value settings port width))))
          
          (define/override (render-value value settings port)
            (set-printing-parameters
             settings
             (lambda ()
               (super render-value value settings port))))
          
          (super-instantiate ())))
      
      ;; sharing/not-config-panel :  boolean parent -> (case-> (-> settings) (settings -> void))
      ;; constructs the config-panel for a language without a sharing option.
      (define (sharing/not-config-panel allow-sharing-config? _parent)
        (let* ([parent (make-object vertical-panel% _parent)]
               
               [input-panel (instantiate group-box-panel% ()
                              (parent parent)
                              (label (string-constant input-syntax))
                              (alignment '(left center)))]
               
               [output-panel (instantiate group-box-panel% ()
                               (parent parent)
                               (label (string-constant output-syntax))
                               (alignment '(left center)))]
               
               [case-sensitive (make-object check-box%
                                 (string-constant case-sensitive-label)
                                 input-panel
                                 void)]
               [output-style (make-object radio-box%
                               (string-constant output-style-label)
                               (list (string-constant constructor-printing-style)
                                     (string-constant quasiquote-printing-style)
                                     (string-constant write-printing-style))
                               output-panel
                               void)]
               [fraction-style
                (make-object radio-box% (string-constant fraction-style)
                  (list (string-constant use-mixed-fractions)
                        (string-constant use-repeating-decimals))
                  output-panel
                  void)]
               [show-sharing #f]
               [insert-newlines (make-object check-box%
                                  (string-constant use-pretty-printer-label)
                                  output-panel
                                  void)]
               [tracing (new check-box%
                             (parent output-panel)
                             (label sc-tracing)
                             (callback void))])
          
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
              (case (send output-style get-selection)
                [(0) 'constructor]
                [(1) 'quasiquote]
                [(2) 'write])
              (case (send fraction-style get-selection)
                [(0) 'mixed-fraction]
                [(1) 'repeating-decimal])
              (and allow-sharing-config? (send show-sharing get-value))
              (send insert-newlines get-value)
              'none
              (send tracing get-value))]
            [(settings)
             (send case-sensitive set-value (drscheme:language:simple-settings-case-sensitive settings))
             (send output-style set-selection
                   (case (drscheme:language:simple-settings-printing-style settings)
                     [(constructor) 0]
                     [(quasiquote) 1]
                     [(write) 2]
                     [(print) 2]))
             (send fraction-style set-selection
                   (case (drscheme:language:simple-settings-fraction-style settings)
                     [(mixed-fraction) 0]
                     [(repeating-decimal) 1]))
             (when allow-sharing-config?
               (send show-sharing set-value (drscheme:language:simple-settings-show-sharing settings)))
             (send insert-newlines set-value 
                   (drscheme:language:simple-settings-insert-newlines settings))
             (send tracing set-value (htdp-lang-settings-tracing? settings))])))
      
      (define simple-htdp-language%
        (class* drscheme:language:simple-module-based-language% (htdp-language<%>)
          (init-field sharing-printing
                      abbreviate-cons-as-list
                      allow-sharing?
                      manual
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
          
          (super-instantiate ()
            (language-url "http://www.htdp.org/"))))
      
      (define (language-extension %)
        (class %
          (inherit get-manual)
          
          (define/override (order-manuals x) 
            (values (list (get-manual) #"teachpack" #"drscheme" #"help") #f))
          
          (inherit get-module get-transformer-module get-init-code
                   use-namespace-require/copy?)
          (define/override (create-executable setting parent program-filename teachpack-cache)
            (let ([executable-filename
		   (drscheme:language:put-executable
		    parent program-filename
		    #f 
		    #t
		    (string-constant save-a-mred-stand-alone-executable))])
              (when executable-filename
                (let ([wrapper-filename (make-temporary-file "drs-htdp-lang-executable~a.ss")]
                      [teachpack-specs
                       (map (lambda (x) `(file ,(path->string x)))
                            (drscheme:teachpack:teachpack-cache-filenames teachpack-cache))])
                  (call-with-output-file wrapper-filename
                    (lambda (outp)
                      (write
                       `(module #%htdp-lang-language mzscheme
                          (require (prefix #%htdp: ,(get-module)))
                          (provide ,@(map (lambda (x) `(rename ,(symbol-append '#%htdp: x) ,x))
                                          (get-export-names (get-module))))
                          (require ,@teachpack-specs)
                          ,@(map (lambda (x) `(provide ,@(get-export-names x)))
                                 teachpack-specs))
                       outp)
                      (newline outp)
                      (newline outp)
                      (fprintf outp "(module #%htdp-lang-executable #%htdp-lang-language\n")
                      (call-with-input-file program-filename
                        (lambda (inp)
                          (copy-port inp outp)))
                      (fprintf outp "\n)\n\n")
                      (write `(require #%htdp-lang-executable) outp)
                      (newline outp))
                    'truncate)
                  (drscheme:language:create-module-based-stand-alone-executable
                   wrapper-filename
                   executable-filename
                   (get-module)
                   (get-transformer-module)
                   (get-init-code setting teachpack-cache)
                   #t
                   (use-namespace-require/copy?))
                  (delete-file wrapper-filename)))))
          
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
                      [else (error 'htdp-lang.ss "could not find language filename ~s" no-ext-name)])]
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
          
          (define/override (front-end/complete-program port settings teachpacks)
            (let ([state 'init]
                  ;; state : 'init => 'require => 'done
                  [reader (get-reader)])
              
              (lambda ()
                (case state
                  [(init)
                   (set! state 'require)
                   (let ([body-exps 
                          (let loop ()
                            (let ([result (reader (object-name port) port)])
                              (if (eof-object? result)
                                  null
                                  (cons result (loop)))))]
                         [language-module (get-module)]
                         [require-specs 
                          (drscheme:teachpack:teachpack-cache-require-specs teachpacks)])
                     (rewrite-module 
                      (expand
                       (datum->syntax-object
                        #f
                        `(,#'module #%htdp ,language-module 
                           (,#'require ,@require-specs)
                           ,@body-exps)))))]
                  [(require) 
                   (set! state 'done)
                   (syntax
                    (let ([done-already? #f])
                      (dynamic-wind
                       void
                       (lambda () 
                         ;(dynamic-require '#%htdp #f)
                         (eval #'(require #%htdp)))  ;; work around a bug in dynamic-require
                       (lambda () 
                         (unless done-already?
                           (set! done-already? #t)
                           (current-namespace (module->namespace '#%htdp)))))))]
                  [(done) eof]))))

          (super-new)))

      ;; rewrite-module : syntax -> syntax
      ;; rewrites te module to provide all definitions and 
      ;; print out all results.
      (define (rewrite-module stx)
        (syntax-case stx (module #%plain-module-begin)
          [(module name lang (#%plain-module-begin bodies ...))
           (with-syntax ([(rewritten-bodies ...) 
                          (rewrite-bodies (syntax->list (syntax (bodies ...))))])
             (syntax (module name lang
                       (#%plain-module-begin 
                        rewritten-bodies ...))))]
          [else
           (raise-syntax-error 'htdp-languages "internal error .1")]))
      
      ;; rewrite-bodies : (listof syntax) -> syntax
      (define (rewrite-bodies bodies)
        (let loop ([bodies bodies])
          (cond
            [(null? bodies) null]
            [else
             (let ([body (car bodies)])
               (syntax-case body (require define-values define-syntaxes require-for-syntax provide)
                 [(define-values (new-vars ...) e)
                  (cons body (loop (cdr bodies)))]
                 [(define-syntaxes (new-vars ...) e)
                  (cons body (loop (cdr bodies)))]
                 [(require specs ...)
                  (cons body (loop (cdr bodies)))]
                 [(require-for-syntax specs ...)
                  (cons body (loop (cdr bodies)))]
                 [(provide specs ...)
                  (loop (cdr bodies))]
                 [else 
                  (let ([new-exp
                         (with-syntax ([body body]
                                       [print-results
                                        (lambda results
                                          (let ([rep (drscheme:rep:current-rep)])
                                            (when rep
                                              (send rep display-results/void results))))])
                           (syntax 
                            (call-with-values
                             (lambda () body)
                             print-results)))])
                    (cons new-exp (loop (cdr bodies))))]))])))
      
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
      
      (eval `(module drscheme-secrets mzscheme
               (provide drscheme-inspector)
               (define drscheme-inspector ,drscheme-inspector)))
      (namespace-require 'drscheme-secrets)
      
      
      
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
      
      
      
      
      ;; cm-key : symbol
      ;; the key used to put information on the continuation
      (define cm-key (gensym 'teaching-languages-continuation-mark-key))
      
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
              (display (exn-message exn) (current-error-port))
              (fprintf (current-error-port) "uncaught exception: ~e" exn))
          (fprintf (current-error-port) "\n")
          
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
                        (let ([cms (continuation-mark-set->list (exn-continuation-marks exn) cm-key)])
                          (if cms
                              (let loop ([cms cms])
                                (cond
                                  [(null? cms) '()]
                                  [else (let* ([cms (car cms)]
                                               [source (car cms)]
                                               [pos (cadr cms)]
                                               [span (cddr cms)])
                                          (if (is-a? source text%)
                                              (list (make-srcloc source #f #f pos span))
                                              (loop (cdr cms))))]))
                              '()))]
                       [else '()])])
                
                (parameterize ([current-eventspace init-eventspace])
                  (queue-callback
                   (lambda ()
                     ;; need to make sure that the user's eventspace is still the same
                     ;; and still running here?
                     (send rep highlight-errors to-highlight #f))))))))
      
      ;; with-mark : syntax syntax -> syntax
      ;; a member of stacktrace-imports^
      ;; guarantees that the continuation marks associated with cm-key are
      ;; members of the debug-source type
      (define (with-mark source-stx expr)
        (let ([source (syntax-source source-stx)]
              [start-position (syntax-position source-stx)]
              [span (syntax-span source-stx)])
          (if (and (is-a? source text:basic<%>)
                   (number? start-position)
                   (number? span))
              (with-syntax ([expr expr]
                            [mark (list* source start-position span)]
                            [cm-key cm-key])
                #`(with-continuation-mark 'cm-key
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
      
      (define (initialize-test-coverage-point key expr)
        (unless (thread-cell-ref current-test-coverage-info)
          (let ([ht (make-hash-table)])
            (thread-cell-set! current-test-coverage-info ht)
            (let ([rep (drscheme:rep:current-rep)])
              (when rep
                (send rep set-test-coverage-info
                      ht
                      (let ([s (make-object style-delta%)])
                        (send s set-delta-foreground "black")
                        s)
                      (let ([s (make-object style-delta%)])
                        (send s set-delta-foreground "firebrick")
                        s)
                      #f)))))
        (hash-table-put! (thread-cell-ref current-test-coverage-info)
                         key
                         (list #f expr)))
      
      (define (test-covered key)
        (let ([v (hash-table-get (thread-cell-ref current-test-coverage-info) key)])
          (set-car! v #t)))
      
      (define-values/invoke-unit/sig et:stacktrace^ et:stacktrace@ et et:stacktrace-imports^)

      (define calltrace-key #`(quote #,(gensym 'drscheme-calltrace-key)))
      
      (define (print-call-trace inferred-name original? src args improper? depth)
        (when inferred-name
          (let ([name (cond
                        [(identifier? inferred-name) (syntax-e inferred-name)]
                        [else (object-name inferred-name)])]
                [rep (drscheme:rep:current-rep)])
            (when (and name rep)
              (let ([canvas (send rep get-canvas)])
                (when canvas
                  (let* ([frame (send canvas get-top-level-window)]
                         [tab (send frame get-current-tab)])
                    (when (is-a? tab tab-tracing<%>)
                      (let ([sp (open-output-string)])
                        (let loop ([i depth])
                          (unless (zero? i)
                            (display " " sp)
                            (loop (- i 1))))
                        (fprintf sp "(")
                        (fprintf sp "~a" name)
                        (let loop ([args args])
                          (cond
                            [(null? args) (void)]
                            [(and (null? (cdr args)) improper?)
                             (fprintf sp " . ")
                             (fprintf sp "~v" (car args))]
                            [else
                             (let ([arg (car args)])
                               (fprintf sp " ")
                               (fprintf sp "~v" arg))
                             (loop (cdr args))]))
                        (fprintf sp ")")
			(let ([sema (make-semaphore)])
			  ;; Disable breaks, so an exn handler can't
			  ;;  grab the DrScheme eventspacae:
			  (parameterize-break #f
			    ;; Queue callback to write trace line ---
			    ;; low priority, so that infinite loops don't stop the user
			    ;;  from clicking "Break"
			    (parameterize ([current-eventspace drs-eventspace])
			      (queue-callback
			       (lambda ()
				 (send tab tracing:add-line (get-output-string sp))
				 (semaphore-post sema))
			       #f)))
			  ;; Wait for th eline to get written, so that the
			  ;;  trace output doesn't get too far behind (which
			  ;;  matters, again, for infinite loops)
			  (semaphore-wait sema)))))))))))

      (define-values/invoke-unit/sig tr:stacktrace^ tr:stacktrace@ tr tr:stacktrace-imports^)
      
      ;; add-annotation : boolean (sexp -> value) -> sexp -> value
      ;; adds debugging and test coverage information to `sexp' and calls `oe'
      (define (add-annotation tracing? oe)
        (let ([teaching-language-eval-handler
               (lambda (exp)
                 (let* ([is-compiled? (compiled-expression? (if (syntax? exp) (syntax-e exp) exp))]
                        [annotated
                         (if is-compiled?
                             exp
                             (let* ([et-annotated (et:annotate-top (expand exp) #f)]
                                    [tr-annotated
                                     (if tracing?
                                         (tr:annotate (expand et-annotated))
                                         et-annotated)])
                               tr-annotated))])
                   (oe annotated)))])
          teaching-language-eval-handler))
      
      (define tab-tracing-mixin
        (mixin (drscheme:unit:tab<%> drscheme:rep:context<%>) (tab-tracing<%>)
          (inherit get-frame)
          
          (define tracing-visible? #f)
          (define/public (set-tracing-visible? v?) (set! tracing-visible? v?))
          (define/public (get-tracing-visible?) tracing-visible?)
          
          (define/augment (clear-annotations)
            (tracing:reset)
            (inner (void) clear-annotations))
          
          (define any-results? #f)
          (define/public (get-any-results?) any-results?)
          (define/public (tracing:reset)
            (set! any-results? #f)
            (send show-tracing-text lock #f)
            (send show-tracing-text erase)
	    (send show-tracing-text auto-wrap #t)
	    (send show-tracing-text insert sc-tracing-nothing-to-show)
            (send show-tracing-text lock #t))
          
          (define show-tracing-text (new text:hide-caret/selection%))
          (define/public (get-tracing-text) show-tracing-text)
          (send show-tracing-text lock #t)
          
          (define/public (tracing:add-line s)
            (let ([old-any? any-results?])
              (set! any-results? #t)
              (unless old-any?
                (send (get-frame) show-tracing))
              (send show-tracing-text begin-edit-sequence)
              (send show-tracing-text lock #f)
	      (unless old-any?
		(send show-tracing-text erase)
		(send show-tracing-text auto-wrap #f))
              (let ([insert
                     (lambda (s)
                       (send show-tracing-text insert s (send show-tracing-text last-position) 'same #f))])
                (cond
                  [(<= (string-length s) ellipses-cutoff)
                   (insert s)
                   (insert "\n")]
                  [else
                   (insert (substring s 0 ellipses-cutoff))
                   (insert " ")
                   (let ([ell-start (send show-tracing-text last-position)])
                     (insert "...")
                     (let ([ell-end (send show-tracing-text last-position)])
                       (let ([para (send show-tracing-text last-paragraph)])
                         (insert "\n")
                         (send show-tracing-text change-style clickback-delta ell-start ell-end)
                         (send show-tracing-text set-clickback ell-start ell-end 
                               (lambda (t x y)
                                 (send show-tracing-text begin-edit-sequence)
                                 (send show-tracing-text lock #f)
                                 (let ([line-start (send show-tracing-text paragraph-start-position para)]
                                       [line-end (send show-tracing-text paragraph-end-position para)])
                                   (send show-tracing-text delete line-start line-end #f)
                                   (send show-tracing-text insert s line-start 'same #f))
                                 (send show-tracing-text lock #t)
                                 (send show-tracing-text end-edit-sequence))))))]))
              (send show-tracing-text lock #t)
              (send show-tracing-text end-edit-sequence)))
          
          (super-new)))
          
      
      (define frame-tracing-mixin 
        (mixin (drscheme:frame:<%> drscheme:unit:frame<%>) ()
          (inherit get-current-tab)
          (define show-tracing-menu-item #f)
          (define tracing-visible? #f)
          
          (define/augment (on-tab-change old new)
            (inner (void) on-tab-change old new)
            (send show-tracing-canvas set-editor (send new get-tracing-text))
            (cond
              [(eq? tracing-visible? (send new get-tracing-visible?))
               (void)]
              [(send new tracing-visible?)
               (show-tracing)]
              [else
               (hide-tracing)]))
          
          (define/override (add-show-menu-items show-menu)
            (super add-show-menu-items show-menu)
            (set! show-tracing-menu-item
                  (new menu-item%
                       (parent show-menu)
                       (label sc-show-tracing-window)
                       (callback (lambda (x y) (toggle-tracing))))))
          
          (define/public (show-tracing)
	    (set! tracing-visible? #t)
	    (send show-tracing-menu-item set-label sc-hide-tracing-window)
	    (send dragable-parent begin-container-sequence)
	    (send dragable-parent change-children
		  (lambda (l)
		    (let ([without (remq show-tracing-canvas l)])
		      (append without (list show-tracing-canvas)))))
	    (send dragable-parent set-percentages '(3/4 1/4))
	    (send dragable-parent end-container-sequence))
          
          (define/private (hide-tracing)
	    (set! tracing-visible? #f)
	    (send show-tracing-menu-item set-label sc-show-tracing-window)
	    (send dragable-parent change-children
		  (lambda (l)
		    (remq show-tracing-canvas l))))
          
          (define/private (toggle-tracing)
            (if tracing-visible?
                (hide-tracing)
                (show-tracing)))
          
          (define dragable-parent #f)
          (define show-tracing-parent-panel #f)
          (define show-tracing-canvas #f)
          
          (define/override (make-root-area-container cls parent)
            (set! dragable-parent (super make-root-area-container panel:horizontal-dragable% parent))
            (let ([root (make-object cls dragable-parent)])
              (set! show-tracing-canvas (new editor-canvas% 
                                             (parent dragable-parent)
                                             (editor (send (get-current-tab) get-tracing-text))))
              (send dragable-parent change-children (lambda (l) (remq show-tracing-canvas l)))
              root))
          
          (super-new)))
      
      (define clickback-delta (make-object style-delta%))
      (send clickback-delta set-delta-foreground "BLUE")
      (send clickback-delta set-delta 'change-underline #t)

      
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
          ((drscheme:language:get-default-mixin)
           (language-extension
            (drscheme:language:module-based-language->language-mixin
             (module-based-language-extension
              (drscheme:language:simple-module-based-language->module-based-language-mixin
               simple-htdp-language%))))))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant advanced-one-line-summary))
           (module '(lib "htdp-advanced.ss" "lang"))
           (manual #"advanced")
           (language-position
            (list (string-constant teaching-languages)
                  (string-constant how-to-design-programs)
                  (string-constant advanced-student)))
           (language-numbers '(-500 -500 5))
           (sharing-printing #t)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #t)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant intermediate/lambda-one-line-summary))
           (module '(lib "htdp-intermediate-lambda.ss" "lang"))
           (manual #"intermediate-lambda")
           (language-position
            (list (string-constant teaching-languages)
                  (string-constant how-to-design-programs)
                  (string-constant intermediate-student/lambda)))
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
           (allow-sharing? #f)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant intermediate-one-line-summary))
           (module '(lib "htdp-intermediate.ss" "lang"))
           (manual #"intermediate")
           (language-position
            (list (string-constant teaching-languages)
                  (string-constant how-to-design-programs)
                  (string-constant intermediate-student)))
           (language-numbers '(-500 -500 3))
           (sharing-printing #f)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #f)
           (use-function-output-syntax? #t)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant beginning/abbrev-one-line-summary))
           (module '(lib "htdp-beginner-abbr.ss" "lang"))
           (manual #"beginning-abbr")
           (language-position
            (list (string-constant teaching-languages)
                  (string-constant how-to-design-programs)
                  (string-constant beginning-student/abbrev)))
           (language-numbers '(-500 -500 2))
           (sharing-printing #f)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #f)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant beginning-one-line-summary))
           (module '(lib "htdp-beginner.ss" "lang"))
           (manual #"beginning")
           (language-position
            (list (string-constant teaching-languages)
                  (string-constant how-to-design-programs)
                  (string-constant beginning-student)))
           (language-numbers '(-500 -500 1))
           (sharing-printing #f)
           (abbreviate-cons-as-list #f)
           (allow-sharing? #f)
           (accept-quasiquote? #f)))
        
        (drscheme:get/extend:extend-unit-frame frame-tracing-mixin)
        (drscheme:get/extend:extend-tab tab-tracing-mixin)))))
