(module for-clause racket/kernel
  (#%require (for-template (submod "private/for.rkt" expand)))
  (#%provide syntax-local-splicing-for-clause-introduce))
