(module define-config racket/base
  (require (for-syntax racket/base))

  (provide define-parameters)

  (define-syntax (define-parameters stx)
    (syntax-case stx ()
      [(_ (name val) ...)
       (andmap identifier? (syntax-e #'(name ...)))
       #'(begin
           (provide name ...)
           (define name (make-parameter val)) ...)])))
