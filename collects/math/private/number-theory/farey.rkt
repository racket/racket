#lang typed/racket/base

(require "types.rkt")

(provide farey mediant)

(: mediant : Exact-Rational Exact-Rational -> Exact-Rational)
(define (mediant x y)
  (/ (+ (numerator x) (numerator y))
     (+ (denominator x) (denominator y))))

(: farey : Integer -> (Listof Exact-Rational))
(define (farey n)
  (cond [(n . <= . 0)  (raise-argument-error 'farey "Positive-Integer" n)]
        [else
         (let loop ([a 1] [b 1] [c (sub1 n)] [d n] [#{fs : (Listof Exact-Rational)} '()])
           (let ([fs  (cons (/ a b) fs)])
             (cond [(positive? a)
                    (define k (quotient (+ n b) d))
                    (loop c d (- (* k c) a) (- (* k d) b) fs)]
                   [else
                    fs])))]))
