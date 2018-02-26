(define-syntax define-syntax-rule
  (syntax-rules ()
    [(_ (name arg ...) e ...)
     (define-syntax name
       (syntax-rules ()
         [(_ arg ...) e ...]))]))
