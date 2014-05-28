#lang racket/base

(require '#%extfl
         '#%unsafe
         "private/vector-wraps.rkt"
         (for-syntax racket/base))

(provide (all-from-out '#%extfl)
         pi.t
         in-extflvector for/extflvector for*/extflvector
         extflvector-copy)

(define pi.t 3.1415926535897932385t0)

(define-vector-wraps "extflvector"
  "extflonum?" extflonum?
  extflvector? extflvector-length extflvector-ref extflvector-set! make-extflvector
  unsafe-extflvector-ref unsafe-extflvector-set! unsafe-extflvector-length
  in-extflvector*
  in-extflvector
  for/extflvector
  for*/extflvector
  extflvector-copy
  0.0T0)
