(module ellipses '#%kernel
  (#%require (for-syntax '#%kernel))

  (#%provide ... _ ~? ~@ ~indexed)

  (define-syntaxes (...)
    (lambda (stx)
      (raise-syntax-error #f "ellipses not allowed as an expression" stx)))

  (define-syntaxes (~?)
    (lambda (stx) (raise-syntax-error #f "not allowed as an expression" stx)))
  (define-syntaxes (~@)
    (lambda (stx) (raise-syntax-error #f "not allowed as an expression" stx)))
  (define-syntaxes (~indexed)
    (lambda (stx) (raise-syntax-error #f "not allowed as an expression" stx)))

  (define-syntaxes (_)
    (lambda (stx)
      (raise-syntax-error #f "wildcard not allowed as an expression" stx))))
