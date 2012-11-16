#lang typed/racket/base

(require "../../../flonum.rkt"
         "../../../base.rkt")

(provide box-muller-transform standard-flnormal-random)

(: box-muller-transform (Float Float -> Float))
(define (box-muller-transform x y)
  (cond [(and (fl= x 0.0) (fl= y 0.0))  0.0]
        [else  (fl* (flsqrt (fl* -2.0 (fllog x)))
                    (flsin (fl* (fl* 2.0 pi) y)))]))

(: standard-flnormal-random (-> Float))
;; The Box-Muller method has an bad reputation, but undeservedly:
;;  1. There's nothing unstable about the floating-point implementation of the transform
;;  2. It has good tail behavior (i.e. it can return very unlikely numbers)
;;  3. With today's RNGs, there's no need to worry about generating two random numbers
;;  4. With today's FPUs, there's no need to worry about computing `log' and `sin' (sheesh)
;; Points in favor: it's simple and fast
(define (standard-flnormal-random)
  (let loop ()
    (define r (box-muller-transform (random) (random)))
    (if (rational? r) r (loop))))
