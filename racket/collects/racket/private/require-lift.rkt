(module require-transform '#%kernel
  (#%declare #:require=define)

  (#%require "define.rkt")

  (#%provide syntax-local-lift-require-definition-param)

  (define syntax-local-lift-require-definition-param
    (make-parameter #f)))
