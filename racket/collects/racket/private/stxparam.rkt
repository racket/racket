
(module stxparam '#%kernel
  (#%require "define.rkt"
             (for-syntax '#%kernel 
                         "stx.rkt" "stxcase-scheme.rkt" 
                         "small-scheme.rkt" 
                         "stxloc.rkt" "stxparamkey.rkt"))

  (#%provide (for-syntax do-syntax-parameterize))

  (define-for-syntax (do-syntax-parameterize stx let-syntaxes-id empty-body-ok? keep-orig?)
    (syntax-case stx ()
      [(_ ([id val] ...) body ...)
       (let ([ids (syntax->list #'(id ...))])
	 (with-syntax ([((gen-id must-be-renamer?) ...)
                    (map (lambda (id)
                           (unless (identifier? id)
                             (raise-syntax-error
                              #f
                              "not an identifier"
                              stx
                              id))
                           (let ([sp (syntax-parameter-local-value id)])
                             (unless (syntax-parameter? sp)
                               (raise-syntax-error
                                #f
                                "not bound as a syntax parameter"
                                stx
                                id))
                             (list
                              (syntax-local-get-shadower
                               (syntax-local-introduce (syntax-parameter-target sp))
                               #t)
                              (rename-transformer-parameter? sp))))
                         ids)])
	   (let ([dup (check-duplicate-identifier ids)])
	     (when dup
	       (raise-syntax-error
		#f
		"duplicate binding"
		stx
		dup)))
           (unless empty-body-ok?
             (when (null? (syntax-e #'(body ...)))
               (raise-syntax-error
                #f
                "missing body expression(s)"
                stx)))
           (with-syntax ([let-syntaxes let-syntaxes-id]
                         [(orig ...) (if keep-orig?
                                         (list ids)
                                         #'())])
             (syntax/loc stx
               (let-syntaxes ([(gen-id)
                               (convert-renamer
                                (if must-be-renamer? (quote-syntax val) #f)
                                val)]
                              ...)
                 orig ...
                 body ...)))))])))
