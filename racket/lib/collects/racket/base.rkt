(module base "private/base.rkt"
  (provide (all-from-out "private/base.rkt"))

  (module reader syntax/module-reader
    racket/base))
