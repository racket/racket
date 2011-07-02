#lang racket/base

(require scribble/manual
         scribble/struct
         scribble/decode
         (only-in "../inside/utils.rkt" cpp)
         (for-syntax racket/base)
         (for-label racket/base
                    racket/contract
                    (except-in ffi/unsafe ->)
                    ffi/unsafe/cvector
                    ffi/vector))

(provide cpp
         InsideRacket
         guide.scrbl
         (all-from-out scribble/manual)
         (for-label (all-from-out racket/base
                                  racket/contract
                                  ffi/unsafe
                                  ffi/unsafe/cvector
                                  ffi/vector)))

(define InsideRacket
  (other-manual '(lib "scribblings/inside/inside.scrbl")))

(define guide.scrbl
  '(lib "scribblings/guide/guide.scrbl"))
