(module base "private/base.rkt"
  (#%declare #:flatten-requires)

  (provide (all-from-out "private/base.rkt"))

  (module reader syntax/module-reader
    racket/base))
