(module model-settings mzscheme
  (require (lib "contract.ss")
           (lib "pconvert.ss"))
  
  ; there are two separate reasons to use units here, but it's just too painful.
  ; reason 1) the drscheme:language procedures are linked at runtime into the
  ; stepper-tool unit, and we need to get them down here somehow.  Making the
  ; render-settings a unit would solve this.
  ; reason 2) the render-settings should be recomputed once for each stepper 
  ; invocation.  invoke-unit is a nice way of doing this without dropping back
  ; to linking-by-position, which is what happens with a simple closure 
  ; implementatian.
  
  ; HOWEVER, like I said, it's just too painful. Once this is a unit, then 
  ; everything else wants to be a unit too. For instance, to make sure that 
  ; the reconstructor gets the right invocation of the unit, it needs to be a 
  ; unit as well.  Pretty soon, everything is units.

  (define-struct render-settings (true-false-printed? constructor-style-printing? abbreviate-cons-as-list? render-to-sexp lifting?))
  
  (provide/contract [check-global-defined (-> symbol? boolean?)]
                    [global-lookup (-> any/c any)]

                    [struct render-settings 
                            ([true-false-printed? boolean?]
                             [constructor-style-printing? boolean?]
                             [abbreviate-cons-as-list? boolean?]
                             [render-to-sexp (any/c . -> . any)]
                             [lifting? boolean?])]
                    
                    
                    [get-render-settings ((any/c . -> . string?) ; render-to-string
                                          (any/c . -> . any) ; render-to-sexp
                                          boolean? ; lifting?
                                          . -> .
                                          render-settings?)]
                    
                    ;; the 'fake' render-settings structures are used for testing, so that the test suite
                    ;; can be run without access to a drscheme frame.
                    [fake-beginner-render-settings render-settings?]
                    [fake-beginner-wla-render-settings render-settings?]
                    [fake-intermediate-render-settings render-settings?]
                    [fake-intermediate/lambda-render-settings render-settings?]
                    [fake-mz-render-settings render-settings?])
  
  (define (make-fake-render-to-sexp true/false constructor-style abbreviate)
    (lambda (val)
      (parameterize ([booleans-as-true/false true/false]
                     [constructor-style-printing constructor-style]
                     [abbreviate-cons-as-list abbreviate])
        ;; duplication of hack inserted by matthew flatt for 201 release:
        (or (and (procedure? val)
                 (object-name val))
            (print-convert val)))))
    
  ; FIXME : #f totally unacceptable as 'render-to-string'
  (define fake-beginner-render-settings
    (make-render-settings #t #t #f (make-fake-render-to-sexp #t #t #f) #t))
  
  (define fake-beginner-wla-render-settings
    (make-render-settings #t #t #t (make-fake-render-to-sexp #t #t #t) #t))
  
  (define fake-intermediate-render-settings
    fake-beginner-wla-render-settings)
  
  (define fake-intermediate/lambda-render-settings
    fake-beginner-wla-render-settings)
  
  (define fake-mz-render-settings
    (make-render-settings (booleans-as-true/false) 
                          (constructor-style-printing) 
                          (abbreviate-cons-as-list) 
                          print-convert
                          #f))
  
  (define-struct test-struct () (make-inspector))
  
  ;; get-render-settings : infer aspects of the current language's print conversion by explicitly testing 
  ;;  assorted test expressions
  (define (get-render-settings render-to-string render-to-sexp lifting?)
    (let* ([true-false-printed? (string=? (render-to-string #t) "true")]
           [constructor-style-printing? (string=? (render-to-string (make-test-struct)) "(make-test-struct)")]
           [rendered-list (render-to-string '(3))]
           [rendered-list-substring (substring rendered-list 
                                               0 
                                               (min 5 (string-length rendered-list)))]
           [abbreviate-cons-as-list? (and constructor-style-printing?
                                              (string=? rendered-list-substring "(list"))])
      (make-render-settings
       true-false-printed?
       constructor-style-printing?
       abbreviate-cons-as-list?
       render-to-sexp
       lifting?)))
  
  (define (check-global-defined identifier)
    (with-handlers
        ([exn:fail:contract:variable? (lambda args #f)])
      (global-lookup identifier)
      #t))
  
  (define (global-lookup identifier)
    (namespace-variable-value identifier)))