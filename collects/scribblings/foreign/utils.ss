#lang racket/base

(require scribble/manual
         scribble/struct
         scribble/decode
         (only-in "../inside/utils.ss" cpp)
         (for-syntax racket/base)
         (for-label racket/base
                    racket/contract
                    (except-in racket/unsafe/ffi ->)))

(provide cpp
         InsideMzScheme
         (all-from-out scribble/manual)
         (for-label (all-from-out racket/base
                                  racket/contract
                                  racket/unsafe/ffi)))


(define InsideMzScheme
  (other-manual '(lib "scribblings/inside/inside.scrbl")))
