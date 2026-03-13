#lang racket/base
(require (for-label (except-in racket/base
                               struct)
                    ffi2))

(provide ref-doc
         (for-label
          (all-from-out racket/base
                        ffi2)))

(define ref-doc '(lib "scribblings/reference/reference.scrbl"))
