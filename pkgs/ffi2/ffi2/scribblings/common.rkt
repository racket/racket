#lang racket/base
(require (for-label (except-in racket/base
                               struct)
                    (rename-in racket/contract/base
                               [-> ->/c])
                    ffi2))

(provide ref-doc
         guide-doc
         ffi-unsafe-doc
         (for-label
          (all-from-out racket/base
                        racket/contract/base
                        ffi2)))

(define ref-doc '(lib "scribblings/reference/reference.scrbl"))
(define guide-doc '(lib "scribblings/guide/guide.scrbl"))
(define ffi-unsafe-doc '(lib "scribblings/foreign/foreign.scrbl"))
