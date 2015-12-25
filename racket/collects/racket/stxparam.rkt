
(module stxparam '#%kernel
  (#%require "private/letstx-scheme.rkt"
             "private/define.rkt"
             "private/stxparam.rkt"
             (for-syntax '#%kernel 
                         "stxparam-exptime.rkt"
                         "private/stxcase-scheme.rkt" 
                         "private/small-scheme.rkt" 
                         "private/stxloc.rkt" "private/stxparamkey.rkt"))

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
	       (let ([gen-id #'gen-id])
		 (make-set!-transformer
		  (make-syntax-parameter 
		   (lambda (stx)
		     (let ([v (syntax-parameter-target-value gen-id)])
		       (apply-transformer v stx #'set!)))
		   gen-id))))))]))

  (define-syntax (syntax-parameterize stx)
    (do-syntax-parameterize stx #'let-syntaxes #f #f))
  
  (define-syntax syntax-parameterize-rename
    (syntax-rules ()
      [(_ ([stx-param id] ...) body0 body ...)
       (syntax-parameterize ([stx-param (make-rename-transformer id)] ...) body0 body ...)])))
