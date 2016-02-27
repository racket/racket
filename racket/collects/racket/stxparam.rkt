
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
             define-rename-transformer-parameter
             syntax-parameterize
             (for-syntax syntax-parameter-value
                         make-parameter-rename-transformer))
  
  (define-syntax (define-syntax-parameter stx)
    (syntax-case stx ()
      [(_ id init-val)
       (with-syntax ([gen-id (car (generate-temporaries (list #'id)))])
         #'(begin
             (define-syntax gen-id (convert-renamer #f init-val))
             (define-syntax id 
               (let ([gen-id #'gen-id])
                 (make-set!-transformer
                  (make-syntax-parameter 
                   (lambda (stx)
                     (let ([v (syntax-parameter-target-value gen-id)])
                       (apply-transformer v stx #'set!)))
                   gen-id))))))]))

  (define-syntax (define-rename-transformer-parameter stx)
    (syntax-case stx ()
      [(_ id init-val)
       (with-syntax ([gen-id (car (generate-temporaries (list #'id)))])
         #'(begin
             (define-syntax gen-id (convert-renamer #'init-val init-val))
             (define-syntax id 
               (let ([gen-id #'gen-id])
                 (make-rename-transformer-parameter 
                  #f
                  gen-id)))))]))

  (define-syntax (syntax-parameterize stx)
    (do-syntax-parameterize stx #'let-syntaxes #f #f)))
