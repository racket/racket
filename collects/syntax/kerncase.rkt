
(module kerncase racket/base
  (require (for-syntax racket/base)
           (for-template racket/base))

  (define-syntax kernel-syntax-case-internal
    (lambda (stx)
      (syntax-case stx ()
	[(_ stxv phase rel? (extras ...) kernel-context clause ...)
	 (quasisyntax/loc
	  stx
	  (syntax-case* stxv (extras ...
                                     #,@(map
                                         syntax-local-introduce
                                         (syntax-e
                                          (quote-syntax
                                           (quote 
                                            quote-syntax #%top
                                            #%plain-lambda case-lambda
                                            let-values letrec-values
                                            begin begin0 set!
                                            with-continuation-mark
                                            if #%plain-app #%expression
                                            define-values define-syntaxes define-values-for-syntax
                                            module 
                                            #%plain-module-begin 
                                            #%require #%provide 
                                            #%variable-reference)))))
                        (let ([p phase])
                          (cond
                           [(and #,(syntax-e #'rel?) (= p 0)) 
                            free-identifier=?]
                           [(and #,(syntax-e #'rel?) (= p 1)) 
                            free-transformer-identifier=?]
                           [else (let ([id (namespace-module-identifier p)])
                                   (lambda (a b)
                                     (free-identifier=? (datum->syntax id 
                                                                       (let ([s (syntax-e b)])
                                                                         (case s
                                                                           [(#%plain-app) '#%app]
                                                                           [(#%plain-lambda) 'lambda]
                                                                           [else s])))
                                                        a
                                                        p)))]))
	    clause ...))])))
  
  (define-syntax kernel-syntax-case
    (lambda (stx)
      (syntax-case stx ()
	[(_ stxv trans? clause ...)
         (quasisyntax/loc stx
           (kernel-syntax-case-internal stxv (if trans? 1 0) #t () #,stx clause ...))])))

  (define-syntax kernel-syntax-case*
    (lambda (stx)
      (syntax-case stx ()
	[(_ stxv trans? (extras ...) clause ...)
         (quasisyntax/loc stx
           (kernel-syntax-case-internal stxv (if trans? 1 0) #t (extras ...) #,stx clause ...))])))

  (define-syntax kernel-syntax-case/phase
    (lambda (stx)
      (syntax-case stx ()
	[(_ stxv phase clause ...)
         (quasisyntax/loc stx
           (kernel-syntax-case-internal stxv phase #f () #,stx clause ...))])))

  (define-syntax kernel-syntax-case*/phase
    (lambda (stx)
      (syntax-case stx ()
	[(_ stxv phase (extras ...) clause ...)
         (quasisyntax/loc stx
           (kernel-syntax-case-internal stxv phase #f (extras ...) #,stx clause ...))])))

  (define (kernel-form-identifier-list)
    (syntax-e (quote-syntax
               (begin
                begin0
                define-values
                define-syntaxes
                define-values-for-syntax
                set!
                let-values
                letrec-values
                #%plain-lambda
                case-lambda
                if
                quote
                letrec-syntaxes+values
                with-continuation-mark
                #%expression
                #%plain-app
                #%top
                #%datum
                #%variable-reference
                module #%provide #%require))))

  (provide kernel-syntax-case
           kernel-syntax-case*
           kernel-syntax-case/phase
           kernel-syntax-case*/phase
           kernel-form-identifier-list))
