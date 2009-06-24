
(module stxparam '#%kernel
  (#%require "private/more-scheme.ss"
             "private/letstx-scheme.ss"
             "private/define.ss"
             "private/stxparam.ss"
             (for-syntax '#%kernel 
                         "stxparam-exptime.ss"
                         "private/stx.ss" "private/stxcase-scheme.ss" 
                         "private/small-scheme.ss" 
                         "private/stxloc.ss" "private/stxparamkey.ss"))

  (#%provide define-syntax-parameter
             syntax-parameterize
             (for-syntax syntax-parameter-value
                         make-parameter-rename-transformer))
  
  (define-syntax (define-syntax-parameter stx)
    (syntax-case stx ()
      [(_ id init-val)
       (with-syntax ([gen-id (car (generate-temporaries (list #'id)))])
	 #'(begin
	     (define-syntax gen-id (convert-renamer init-val))
	     (define-syntax id 
	       (let ([gen-id ((syntax-local-certifier) #'gen-id)])
		 (make-set!-transformer
		  (make-syntax-parameter 
		   (lambda (stx)
		     (let ([v (syntax-parameter-target-value gen-id)])
		       (apply-transformer v stx #'set!)))
		   gen-id))))))]))

  (define-syntax (syntax-parameterize stx)
    (do-syntax-parameterize stx #'let-syntaxes #f)))
