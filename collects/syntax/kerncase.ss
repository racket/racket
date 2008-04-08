
(module kerncase scheme/base
  (require (for-syntax scheme/base)
           (for-template scheme/base))

  (define-syntax kernel-syntax-case-internal
    (lambda (stx)
      (syntax-case stx ()
	[(_ stxv trans? (extras ...) kernel-context clause ...)
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
                        (if trans? free-transformer-identifier=? free-identifier=?)
	    clause ...))])))
  
  (define-syntax kernel-syntax-case
    (lambda (stx)
      (syntax-case stx ()
	[(_ stxv trans? clause ...)
         (quasisyntax/loc stx
           (kernel-syntax-case-internal stxv trans? () #,stx clause ...))])))

  (define-syntax kernel-syntax-case*
    (lambda (stx)
      (syntax-case stx ()
	[(_ stxv trans? (extras ...) clause ...)
         (quasisyntax/loc stx
           (kernel-syntax-case-internal stxv trans? (extras ...) #,stx clause ...))])))

  (define (kernel-form-identifier-list)
    (syntax-e (quote-syntax
               (begin
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
                #%datum ; should this be here?
                #%variable-reference))))

  (provide kernel-syntax-case
           kernel-syntax-case*
           kernel-form-identifier-list))
