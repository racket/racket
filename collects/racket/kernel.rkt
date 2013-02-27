(module kernel '#%kernel
  (#%provide (all-from '#%kernel))

  (module reader syntax/module-reader
    #:language 'racket/kernel))
