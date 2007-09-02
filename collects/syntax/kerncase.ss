
(module kerncase mzscheme

  (define-syntax kernel-syntax-case-internal
    (lambda (stx)
      (syntax-case stx ()
	[(_ stxv trans? (extras ...) kernel-context clause ...)
	 (quasisyntax/loc
	  stx
	  (syntax-case* stxv #,(datum->syntax-object
				#'kernel-context
                                (append (syntax->list #'(extras ...))
                                        '(quote 
                                          quote-syntax #%datum #%top
                                          lambda case-lambda
                                          let-values letrec-values
                                          begin begin0 set!
                                          with-continuation-mark
                                          if #%app #%expression
                                          define-values define-syntaxes define-values-for-syntax
                                          module #%plain-module-begin require provide 
                                          require-for-syntax require-for-template
                                          require-for-label
                                          provide-for-syntax provide-for-label
                                          #%variable-reference)))
                        (if trans? module-transformer-identifier=? module-identifier=?)
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

  (define (kernel-form-identifier-list stx)
    (map (lambda (s)
	   (datum->syntax-object stx s #f))
	 '(begin
	    define-values
	    define-syntaxes
	    define-values-for-syntax
	    set!
	    let-values
	    letrec-values
	    lambda
	    case-lambda
	    if
	    quote
	    letrec-syntaxes+values
	    with-continuation-mark
            #%expression
	    #%app
	    #%top
	    #%datum
	    #%variable-reference)))
  
  (provide kernel-syntax-case
           kernel-syntax-case*
           kernel-form-identifier-list))
