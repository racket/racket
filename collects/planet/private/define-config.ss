(module define-config scheme/base
  (require (for-syntax scheme/base))

  (provide define-parameters)

  (define-syntax (define-parameters stx)
    (syntax-case stx ()
      [(_ (name val) ...)
       (andmap identifier? (syntax->list #'(name ...)))
       #'(begin
           (provide name ...)
           (define name (make-parameter val)) ...)])))
