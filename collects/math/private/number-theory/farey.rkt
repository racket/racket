#lang typed/racket/base

(require "../exception.rkt"
         "types.rkt")

(provide farey mediant)

(: mediant : Q Q -> Q)
(define (mediant x y)
  (/ (+ (numerator x) (numerator y))
     (+ (denominator x) (denominator y))))

(: farey : Z -> (Listof Q))
(define (farey n)
  (cond [(n . <= . 0)  (raise-argument-error 'farey "Positive-Integer" n)]
        [else
         (let: loop : (Listof Q) ([a : Integer 1]
                                  [b : Integer 1]
                                  [c : Integer (sub1 n)]
                                  [d : Integer n]
                                  [fs : (Listof Q)  '()])
           (let ([fs  (cons (/ a b) fs)])
             (cond [(positive? a)
                    (define k (quotient (+ n b) d))
                    (loop c d (- (* k c) a) (- (* k d) b) fs)]
                   [else
                    fs])))]))
