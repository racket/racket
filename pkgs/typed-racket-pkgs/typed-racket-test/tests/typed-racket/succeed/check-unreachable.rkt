#lang racket/load


(require (for-syntax typed-racket/utils/tc-utils))

(begin-for-syntax
  (check-unreachable-code? #t))

(require typed/racket)



(require racket/flonum
         math/private/flonum/flvector
         math/private/flonum/flonum-functions
         math/private/flonum/flonum-more-functions)

(define-predicate float-complex? Float-Complex)
(: atanh (case-> (Zero -> Zero)
                 (Float -> Float)
                 (Real -> Real)
                 (Float-Complex -> Float-Complex)
                 (Number -> Number)))
(define (atanh x)
  (cond [(flonum? x) (flatanh x)]
        [(eqv? x 0)  0]
        [(real? x)  (flatanh (fl x))]
        [(float-complex? x)  (* 0.5 (- (log (+ 1.0 x)) (log (- 1.0 x))))]
        [else  (* 1/2 (- (log (+ 1 x)) (log (- 1 x))))]))
