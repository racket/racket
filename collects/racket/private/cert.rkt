(module base "pre-base.rkt"

  ;; Backward compatibility
  (#%provide syntax-recertify
             syntax-local-certifier)

  (define syntax-recertify (lambda (a b c d) a))
  (define syntax-local-certifier
    (case-lambda
     [() (syntax-local-certifier 'a)]
     [(a) (case-lambda 
           [(a) a]
           [(a b) a]
           [(a b c) a])])))
