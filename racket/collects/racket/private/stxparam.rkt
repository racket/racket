
(module stxparam '#%kernel
  (#%declare #:require=define)

  (#%require "define.rkt"
             (for-syntax '#%kernel 
                         "stx.rkt" "stxcase-scheme.rkt" 
                         "core-macros.rkt" "core-macros.rkt"
                         "stxloc.rkt" "stxparamkey.rkt"))

  (#%provide (for-syntax do-syntax-parameterize)
             let-local-keys)

  (define-for-syntax (do-syntax-parameterize stx finish-k)
    (syntax-case stx ()
      [(-syntax-parameterize ([id val] ...) body ...)
       (let ([ids (syntax->list #'(id ...))])
	 (with-syntax ([((gen-id local-key who/must-be-renamer) ...)
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
                              (car (generate-temporaries '(stx-param)))
                              (syntax-parameter-key sp)
                              (and (rename-transformer-parameter? sp)
                                   #'-syntax-parameterize))))
                         ids)])
	   (let ([dup (check-duplicate-identifier ids)])
	     (when dup
	       (raise-syntax-error
		#f
		"duplicate binding"
		stx
		dup)))
           (if finish-k
               (finish-k #'(id ...)
                         #'(gen-id ...)
                         #'((wrap-parameter-value 'who/must-be-renamer val) ...)
                         #'([local-key gen-id] ...)
                         #'(body ...))
               (begin
                 (when (null? (syntax-e #'(body ...)))
                   (raise-syntax-error
                    #f
                    "missing body expression(s)"
                    stx))
                 (syntax/loc stx
                   (letrec-syntaxes+values
                       ([(gen-id) (wrap-parameter-value 'who/must-be-renamer val)] ...)
                       ()
                     (let-local-keys ([local-key gen-id] ...)
                       (let-values () body ...))))))))]))

  (define-syntax (let-local-keys stx)
    (if (eq? 'expression (syntax-local-context))
        (let-values ([(expr opaque-expr)
                      (syntax-case stx ()
                        [(_ ([local-key id] ...) expr)
                         (with-continuation-mark
                          current-parameter-environment
                          (extend-parameter-environment
                           (current-parameter-environment)
                           #'([local-key id] ...))
                          (syntax-local-expand-expression
                           #'expr
                           #t))])])
          opaque-expr)
        (with-syntax ([stx stx])
          #'(#%expression stx)))))
