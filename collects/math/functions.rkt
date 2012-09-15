#lang typed/racket/base

(require racket/flonum
         "private/functions/factorial.rkt"
         "private/functions/hyperbolic.rkt"
         "private/functions/inverse-hyperbolic.rkt"
         "constants.rkt")

(provide (all-from-out "private/functions/factorial.rkt"
                       "private/functions/hyperbolic.rkt"
                       "private/functions/inverse-hyperbolic.rkt")
         hypot
         power-of-two?)

(: hypot (Real Real -> Real))
(define (hypot x y)
  (define xa (abs x))
  (define ya (abs y))
  (let ([xa  (min xa ya)]
        [ya  (max xa ya)])
    (cond [(zero? xa)  ya]
          [else  (define u (/ xa ya))
                 (define h (* ya (sqrt (add1 (* u u)))))
                 (with-asserts ([h  real?]) h)])))

;; Returns #t if x is an integer power of 2
(: power-of-two? (Exact-Rational -> Boolean))
(define (power-of-two? x)
  (cond [(not (positive? x))  #f]
        [(integer? x)  (= x (expt 2 (- (integer-length x) 1)))]
        [else  (and (= 1 (numerator x))
                    (power-of-two? (denominator x)))]))
