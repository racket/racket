#lang typed/racket

(require racket/flonum
         typed/rackunit)

(: neg (Real -> Real))
(define (neg x) (- x))

(: flneg (Flonum -> Flonum))
(define (flneg x) (fl* -1.0 x))

(check-eqv? (neg +inf.0) -inf.0)
(check-eqv? (neg -inf.0) +inf.0)
(check-eqv? (neg +nan.0) +nan.0)
(check-eqv? (neg -0.0) +0.0)
(check-eqv? (neg +0.0) -0.0)

(check-eqv? (flneg +inf.0) (neg +inf.0))
(check-eqv? (flneg -inf.0) (neg -inf.0))
(check-eqv? (flneg +nan.0) (neg +nan.0))
(check-eqv? (flneg -0.0)   (neg -0.0))
(check-eqv? (flneg +0.0)   (neg +0.0))

(check-eqv? (- +inf.0) (neg +inf.0))
(check-eqv? (- -inf.0) (neg -inf.0))
(check-eqv? (- +nan.0) (neg +nan.0))
(check-eqv? (- -0.0)   (neg -0.0))
(check-eqv? (- +0.0)   (neg +0.0))
