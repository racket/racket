#lang typed/racket/base

#|
TODO

Move racket/math functions here; racket/math reexports
|#

(require racket/flonum
         "private/factorial.rkt"
         "private/hyperbolic.rkt"
         "private/inverse-hyperbolic.rkt"
         "constants.rkt")

(provide (all-from-out "private/factorial.rkt"
                       "private/hyperbolic.rkt"
                       "private/inverse-hyperbolic.rkt")
         flhypot hypot
         fllog/base)

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

;; from plot:
;; floor-log/base
;; ceiling-log/base
