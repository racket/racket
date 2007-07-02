#|

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
           (lib "file.ss")
           (lib "unit.ss")
           (lib "class.ss")
           (lib "list.ss")
           (lib "struct.ss")
           (lib "compile.ss")
           (lib "struct.ss")
           (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "bday.ss" "framework" "private")
           (lib "moddep.ss" "syntax")
           (lib "cache-image-snip.ss" "mrlib")
           (lib "embed.ss" "compiler")
           (lib "wxme.ss" "wxme")
           (lib "dirs.ss" "setup")
           
           ;; this module is shared between the drscheme's namespace (so loaded here) 
           ;; and the user's namespace in the teaching languages
           "private/set-result.ss"
           
           (lib "stepper-language-interface.ss" "stepper"))
  
  (provide tool@)
  
  (define sc-tracing (string-constant tracing-enable-tracing))
  (define sc-show-tracing-window (string-constant tracing-show-tracing-window))
  (define sc-hide-tracing-window (string-constant tracing-hide-tracing-window))
  (define sc-tracing-nothing-to-show (string-constant tracing-tracing-nothing-to-show))
  
  (define ellipses-cutoff 200)
  
  (define o (current-output-port))
  (define (oprintf . args) (apply fprintf o args))
  
  (define init-eventspace (current-eventspace))
  
  (define user-installed-teachpacks-collection "installed-teachpacks")
  (define teachpack-installation-dir (build-path (find-user-collects-dir) user-installed-teachpacks-collection))
  
  (define tool@
    (unit 
      (import drscheme:tool^)
      (export drscheme:tool-exports^)
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
             '()))
          
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
                                (andmap string? x)))
                         l)))
          
          (inherit get-allow-sharing? get-use-function-output-syntax? 
                   get-accept-quasiquote? get-read-accept-dot)
          (define/override (config-panel parent)
            (sharing/not-config-panel (get-allow-sharing?) (get-accept-quasiquote?) parent))
          
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
                             (callback void))]
               
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
                         [(write) 2]
                         [(print) 2])
                       (case (drscheme:language:simple-settings-printing-style settings)
                         [(constructor) 0]
                         [(quasiquote) 0]
                         [(write) 1]
                         [(print) 1])))
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
             (send tracing set-value (htdp-lang-settings-tracing? settings))])))
      
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
          
          (super-instantiate ()
            (language-url "http://www.htdp.org/"))))
      
      (define (language-extension %)
        (class %
          (inherit get-manual)
          
          (define/override (extra-repl-information settings port) 
            (define (go str sd)
              (let* ([s (make-object string-snip% str)]
                     [sl (editor:get-standard-style-list)]
                     [std (send sl find-named-style "Standard")]
                     [style (send sl find-or-create-style std sd)])
                (send s set-style style)
                (write-special s port)))
            
            (define tps (htdp-lang-settings-teachpacks settings))
            
            (unless (null? tps)
              (go "Teachpack" (drscheme:rep:get-welcome-delta))
              (cond
                [(= 1 (length tps))
                 (go ": " (drscheme:rep:get-welcome-delta))
                 (go (cadr (car tps)) (drscheme:rep:get-dark-green-delta))]
                [(= 2 (length tps))
                 (go "s: " (drscheme:rep:get-welcome-delta))
                 (go (cadr (car tps)) (drscheme:rep:get-dark-green-delta))
                 (go " and " (drscheme:rep:get-welcome-delta))
                 (go (cadr (cadr tps)) (drscheme:rep:get-dark-green-delta))]
                [else
                 (go "s: " (drscheme:rep:get-welcome-delta))
                 (go (cadr (car tps)) (drscheme:rep:get-dark-green-delta))
                 (let loop ([these-tps (cdr tps)])
                   (cond
                     [(null? (cdr these-tps))
                      (go ", and " (drscheme:rep:get-welcome-delta))
                      (go (cadr (car these-tps)) (drscheme:rep:get-dark-green-delta))]
                     [else
                      (go ", " (drscheme:rep:get-welcome-delta))
                      (go (cadr (car these-tps)) (drscheme:rep:get-dark-green-delta))
                      (loop (cdr these-tps))]))])
              (go "." (drscheme:rep:get-welcome-delta))
              (newline port)))
 
          (define/override (order-manuals x) 
            (values (list (get-manual) #"teachpack" #"drscheme" #"help") #f))
          
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
                   (create-embedding-executable 
                    exe-name
                    #:modules `((#f ,program-filename))
                    #:literal-expression `(require ,(filename->require-symbol program-filename))
                    #:cmdline '("-Zmvq")
                    #:src-filter
                    (λ (path) (cannot-compile? path))
                    #:get-extra-imports
                    (λ (path cm)
                      (call-with-input-file path
                        (λ (port)
                          (cond
                            [(is-wxme-stream? port)
                             (let-values ([(snip-class-names data-class-names)
                                           (extract-used-classes port)])
                               (list*
                                '(lib "read.ss" "wxme")
                                '(lib "mred.ss" "mred")
                                reader-module
                                (filter
                                 values
                                 (map (λ (x) (string->lib-path x #t))
                                      (append
                                       snip-class-names
                                       data-class-names)))))]
                            [else
                             '()]))))
                    #:mred? #t))))))

          (define/private (filename->require-symbol fn)
            (let-values ([(base name dir) (split-path fn)])
              (string->symbol
               (path->string
                (path-replace-suffix name #"")))))
          
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
          
          (define/override (front-end/complete-program port settings)
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
                         [language-module (get-module)])
                     (for-each
                      (λ (tp)
                        (with-handlers ((exn:fail? (λ (x) (error 'teachpack (missing-tp-message tp)))))
                          (unless (file-exists? (build-path (apply collection-path (cddr tp))
                                                            (cadr tp)))
                            (error))))
                      (htdp-lang-settings-teachpacks settings))
                     (rewrite-module
                      settings
                      (expand
                       (datum->syntax-object
                        #f
                        `(,#'module #%htdp ,language-module 
                                    ,@(map (λ (x) `(require ,x))
                                           (htdp-lang-settings-teachpacks settings))
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
          
          (define/private (missing-tp-message x)
            (let* ([m (regexp-match #rx"/([^/]*)$" (cadr x))]
                   [name (if m
                             (cadr m)
                             (cadr x))])
              (format "the teachpack '~a' was not found" name)))

          (define/augment (capability-value key)
            (case key
              [(drscheme:teachpack-menu-items) htdp-teachpack-callbacks]
              [(drscheme:special:insert-lambda) #f]
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
                                                  (cadr teachpack)))
                             settings)
                           
                           (make-htdp-lang-settings
                            (drscheme:language:simple-settings-case-sensitive settings)
                            (drscheme:language:simple-settings-printing-style settings)
                            (drscheme:language:simple-settings-fraction-style settings)
                            (drscheme:language:simple-settings-show-sharing settings)
                            (drscheme:language:simple-settings-insert-newlines settings)
                            (drscheme:language:simple-settings-annotations settings)
                            (htdp-lang-settings-tracing? settings)
                            (append old-tps (list teachpack)))
                           
                           #;
                           (copy-struct htdp-lang-settings settings
                                        [htdp-lang-settings-teachpacks 
                                         (append old-tps (list teachpack))])))
                     settings)))
             (λ (settings name) 
               (make-htdp-lang-settings
                (drscheme:language:simple-settings-case-sensitive settings)
                (drscheme:language:simple-settings-printing-style settings)
                (drscheme:language:simple-settings-fraction-style settings)
                (drscheme:language:simple-settings-show-sharing settings)
                (drscheme:language:simple-settings-insert-newlines settings)
                (drscheme:language:simple-settings-annotations settings)
                (htdp-lang-settings-tracing? settings)
                (filter (λ (x) (not (equal? (cadr x) name)))
                        (htdp-lang-settings-teachpacks settings))))
             (λ (settings) 
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
             ";; The first three lines of this file were inserted by DrScheme. They record metadata\n"
             ";; about the language level of this file in a form that our tools can easily process.\n"
             (format "#reader~s~s\n"
                     reader-module
                     `((modname ,modname)
                       (read-case-sensitive ,(drscheme:language:simple-settings-case-sensitive settings))
                       (teachpacks ,(htdp-lang-settings-teachpacks settings))
                       (htdp-settings ,(htdp-lang-settings->vector settings))))))
          
          (inherit default-settings)
          (define/override (metadata->settings metadata)
            (let* ([table (metadata->table metadata)] ;; extract the table
                   [ssv (assoc 'htdp-settings table)])
              (if ssv
                  (let ([settings-list (vector->list (cadr ssv))])
                    (if (equal? (length settings-list)
                                (procedure-arity make-htdp-lang-settings))
                        (apply make-htdp-lang-settings settings-list)
                        (default-settings)))
                  (default-settings))))
          
          (define/private (metadata->table metadata)
            (let ([p (open-input-string metadata)])
              (regexp-match #rx"\n#reader" p) ;; skip to reader
              (read p) ;; skip module
              (read p)))
          
          (define/override (get-metadata-lines) 3)
          
          (super-new)))
      
      ;; cannot-compile? : path -> boolean
      ;; returns #t if the file cannot be compiled, #f otherwise
      (define (cannot-compile? path)
        (call-with-input-file path
          (λ (port) 
            (let ([ok-to-compile-names 
                   (map (λ (x) (format "~s" x))
                        '(wxtext
                          (lib "comment-snip.ss" "framework")
                          (lib "xml-snipclass.ss" "xml")
                          (lib "scheme-snipclass.ss" "xml")))])
              (and (is-wxme-stream? port)
                   (let-values ([(snip-class-names data-class-names)
                                 (extract-used-classes port)])
                     (not (and (andmap
                                (λ (used-name) (member used-name ok-to-compile-names))
                                snip-class-names)
                               (andmap
                                (λ (used-name) (member used-name ok-to-compile-names))
                                data-class-names)))))))))
      
      (define (get-teachpack-from-user parent)
        (define tp-dir (collection-path "teachpack" "htdp"))
        (define columns 2)
        (define tps (filter
                     (λ (x) (file-exists? (build-path tp-dir x)))
                     (directory-list tp-dir)))
        (define sort-order (λ (x y) (string<=? (path->string x) (path->string y))))
        (define pre-installed-tps (sort tps sort-order))
        (define dlg (new dialog% [parent parent] [label (string-constant drscheme)]))
        (define hp (new horizontal-panel% [parent dlg]))
        (define answer #f)
        (define compiling? #f)
        
        (define pre-installed-gb (new group-box-panel%
                                      [label (string-constant teachpack-pre-installed)]
                                      [parent hp]))
        (define user-installed-gb (new group-box-panel%
                                       [label (string-constant teachpack-user-installed)]
                                       [parent hp]))
        
        (define pre-installed-lb
          (new list-box%
               [label #f]
               [choices (map path->string pre-installed-tps)]
               [stretchable-height #t]
               [min-height 300]
               [min-width 200]
               [callback
                (λ (x evt)
                  (case (send evt get-event-type)
                    [(list-box-dclick) (selected pre-installed-lb)]
                    [else
                     (clear-selection user-installed-lb)
                     (update-button)]))]
               [parent pre-installed-gb]))
        
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
                     (clear-selection pre-installed-lb)
                     (update-button)]))]
               [parent user-installed-gb]))
        
        (define (selected lb)
          (unless compiling?
            (set! answer (figure-out-answer))
            (send dlg show #f)))
        
        (define (clear-selection lb)
          (for-each
           (λ (x) (send lb select x #f))
           (send lb get-selections)))
        
        (define add-button (new button%
                                [parent user-installed-gb]
                                [label (string-constant install-teachpack...)]
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
                                      (parameterize ([read-accept-reader #t])
                                        (compile-file filename))))))])
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
          (clear-selection pre-installed-lb)
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
                         (pair? (send pre-installed-lb get-selections))))))
        
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
            [(send pre-installed-lb get-selection)
             =>
             (λ (i) `(lib ,(send pre-installed-lb get-string i) 
                          "teachpack"
                          "htdp"))]
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
        (class* % (stepper-language<%>)
          (init-field stepper:enable-let-lifting)
          (inherit [dontcare stepper:enable-let-lifting?])
          (define/override (stepper:enable-let-lifting?) stepper:enable-let-lifting)
          (super-new)))

      ;; rewrite-module : settings syntax -> syntax
      ;; rewrites te module to print out results of non-definitions
      (define (rewrite-module settings stx)
        (syntax-case stx (module #%plain-module-begin)
          [(module name lang (#%plain-module-begin bodies ...))
           (with-syntax ([(rewritten-bodies ...) 
                          (rewrite-bodies (syntax->list (syntax (bodies ...))))])
             #`(module name lang
                 (#%plain-module-begin 
                  rewritten-bodies ...)))]
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
        (let ([ht (thread-cell-ref current-test-coverage-info)])
          (when ht
            (hash-table-put! ht key (list #f expr)))))
      
      (define (test-covered key)
        (let ([ht (thread-cell-ref current-test-coverage-info)])
          (when ht
            (let ([v (hash-table-get ht key)])
              (set-car! v #t)))))
      
      (define-values/invoke-unit et:stacktrace@
        (import et:stacktrace-imports^) (export (prefix et: et:stacktrace^)))

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
			  ;; Wait for the line to get written, so that the
			  ;;  trace output doesn't get too far behind (which
			  ;;  matters, again, for infinite loops)
			  (semaphore-wait sema)))))))))))

      (define-values/invoke-unit tr:stacktrace@
        (import tr:stacktrace-imports^) (export (prefix tr: tr:stacktrace^)))
      
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
              [(send new get-tracing-visible?)
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
          (stepper-settings-language
           ((drscheme:language:get-default-mixin)
            (language-extension
             (drscheme:language:module-based-language->language-mixin
              (module-based-language-extension
               (drscheme:language:simple-module-based-language->module-based-language-mixin
                simple-htdp-language%)))))))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant advanced-one-line-summary))
           (module '(lib "htdp-advanced.ss" "lang"))
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
           (stepper:enable-let-lifting #t)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant intermediate/lambda-one-line-summary))
           (module '(lib "htdp-intermediate-lambda.ss" "lang"))
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
           (stepper:enable-let-lifting #t)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant intermediate-one-line-summary))
           (module '(lib "htdp-intermediate.ss" "lang"))
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
           (stepper:enable-let-lifting #t)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant beginning/abbrev-one-line-summary))
           (module '(lib "htdp-beginner-abbr.ss" "lang"))
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
           (stepper:enable-let-lifting #t)))
        
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
           (language-id "plt:beginning-student")
           (sharing-printing #f)
           (abbreviate-cons-as-list #f)
           (allow-sharing? #f)
           (accept-quasiquote? #f)
           (reader-module '(lib "htdp-beginner-reader.ss" "lang"))
           (stepper:enable-let-lifting #t)))
        
        (drscheme:get/extend:extend-unit-frame frame-tracing-mixin)
        (drscheme:get/extend:extend-tab tab-tracing-mixin)))))
