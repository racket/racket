(module require-transform '#%kernel
  (#%declare #:unsafe-cross-phase-persistent)
  (#%require "core-syntax.rkt")

  (#%provide syntax-local-lift-require-definition-param)

  (define syntax-local-lift-require-definition-param
    (make-parameter #f)))
