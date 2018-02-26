
(module stxparam '#%kernel
  (#%require "private/letstx-scheme.rkt"
             "private/define.rkt"
             "private/stxparam.rkt"
             (for-syntax '#%kernel 
                         "stxparam-exptime.rkt"
                         "private/stxcase-scheme.rkt" 
                         "private/small-scheme.rkt" 
                         "private/stxloc.rkt"
                         "private/stxparamkey.rkt"))

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
             (define-syntax gen-id (wrap-parameter-value #f init-val))
             (define-syntax id
               (let ([key (gensym)])
                 (make-syntax-parameter
                  (quote-syntax gen-id)
                  key)))))]))

  (define-syntax (define-rename-transformer-parameter stx)
    (syntax-case stx ()
      [(_ id init-val)
       (with-syntax ([gen-id (car (generate-temporaries (list #'id)))])
         #'(begin
             (define-syntax gen-id (wrap-parameter-value 'define-rename-transformer-parameter init-val))
             (define-syntax id 
               (let ([key (gensym)])
                 (make-rename-transformer-parameter 
                  #'gen-id ; needed if `key` is not set
                  key)))))]))

  (define-syntax (syntax-parameterize stx)
    (do-syntax-parameterize stx #'letrec-syntaxes #f #f)))
