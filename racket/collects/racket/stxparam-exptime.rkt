
(module stxparam-exptime '#%kernel
  (#%require "private/stxcase-scheme.rkt"
             "private/small-scheme.rkt"
             "private/stxparamkey.rkt")

  (#%provide syntax-parameter-value
             make-parameter-rename-transformer)
  
  (define-values (syntax-parameter-value)
    (lambda (id)
      (let* ([v (syntax-local-value id (lambda () #f))])
        (unless (syntax-parameter? v)
          (raise-argument-error 'syntax-parameter-value "syntax-parameter?" v))
        (syntax-parameter-key-value (syntax-parameter-key v)
                                    (syntax-parameter-default-id v)))))

  (define-values (make-parameter-rename-transformer)
    (lambda (id)
      (make-set!-transformer
       (lambda (stx)
         (let ([v (syntax-parameter-value (syntax-local-introduce id))])
           (apply-transformer v stx #'set!)))))))
