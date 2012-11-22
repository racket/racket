#lang typed/racket/base

(require "../../../flonum.rkt"
         "../../../base.rkt"
         "../../../vector.rkt")

(provide flnormal-sample)

(: box-muller-transform (Float Float -> Float))
(define (box-muller-transform x y)
  (cond [(and (fl= x 0.0) (fl= y 0.0))  0.0]
        [else  (fl* (flsqrt (fl* -2.0 (fllog x)))
                    (flsin (fl* (fl* 2.0 pi) y)))]))

(: flnormal-sample (Flonum Flonum Integer -> FlVector))
;; The Box-Muller method has an bad reputation, but undeservedly:
;;  1. There's nothing unstable about the floating-point implementation of the transform
;;  2. It has good tail behavior (i.e. it can return very unlikely numbers)
;;  3. With today's RNGs, there's no need to worry about generating two random numbers
;;  4. With today's FPUs, there's no need to worry about computing `log' and `sin' (sheesh)
;; Points in favor: it's simple and fast
(define (flnormal-sample c s n)
  (cond [(n . < . 0)  (raise-argument-error 'flnormal-sample "Natural" 2 c s n)]
        [else
         (build-flvector
          n (λ (_)
              (let loop ()
                (define r (box-muller-transform (random) (random)))
                (if (rational? r) (fl+ c (fl* s r)) (loop)))))]))

