#lang typed/racket/base

(require racket/flonum
         "private/functions/factorial.rkt"
         "private/functions/hyperbolic.rkt"
         "private/functions/inverse-hyperbolic.rkt"
         "constants.rkt")

(provide (all-from-out "private/functions/factorial.rkt"
                       "private/functions/hyperbolic.rkt"
                       "private/functions/inverse-hyperbolic.rkt")
         flhypot hypot
         fllog/base
         power-of-two?)

(: flhypot (Float Float -> Float))
(define (flhypot x y)
  (define xa (abs x))
  (define ya (abs y))
  (let ([xa  (min xa ya)]
        [ya  (max xa ya)])
    (cond [(= xa 0.0)  ya]
          [else  (define u (/ xa ya))
                 (* ya (flsqrt (+ 1.0 (* u u))))])))

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

;; todo: overflow not likely; underflow likely
(: fllog/base (Float Float -> Float))
(define (fllog/base b x)
  (/ (fllog x) (fllog b)))

;; Returns #t if x is an integer power of 2
(: power-of-two? (Exact-Rational -> Boolean))
(define (power-of-two? x)
  (cond [(not (positive? x))  #f]
        [(integer? x)  (= x (expt 2 (- (integer-length x) 1)))]
        [else  (and (= 1 (numerator x))
                    (power-of-two? (denominator x)))]))

;; from plot:
;; floor-log/base
;; ceiling-log/base

