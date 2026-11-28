(module define racket/base
  (#%declare #:unsafe-cross-phase-persistent)
  (require racket/private/norm-define)
  (provide normalize-definition normalize-definition/mk-rhs))
