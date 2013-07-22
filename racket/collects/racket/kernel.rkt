(module kernel '#%kernel
  (#%provide (all-from '#%kernel))

  (#%declare #:cross-phase-persistent)

  (module reader syntax/module-reader
    #:language 'racket/kernel))
