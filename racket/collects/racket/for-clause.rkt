(module for-clause racket/kernel
  (#%declare #:unsafe-cross-phase-persistent)
  (#%require (for-template (submod "private/for.rkt" expand)))
  (#%provide syntax-local-splicing-for-clause-introduce))
