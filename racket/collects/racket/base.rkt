(module base "private/base.rkt"
  (#%declare #:unsafe-cross-phase-persistent)

  (provide (all-from-out "private/base.rkt"))

  (module reader syntax/module-reader
    racket/base))
