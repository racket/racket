#lang typed/racket
(provide farey mediant)

(define-type Q Exact-Rational)
(define-predicate natural? Natural)

(: mediant : Q Q -> Q)
(define (mediant x y)
  (/ (+ (numerator x) (numerator y))
     (+ (denominator x) (denominator y))))

(: farey : Positive-Integer -> (Listof Q))
(define (farey n)
  (define: fs : (Listof Q) '())
  (let: loop : (Listof Q)
    ([a : Integer 1]
     [b : Integer 1]
     [c : Integer (sub1 n)]
     [d : Integer n])
    (set! fs (cons (/ a b) fs))
    (when (positive? a)
      (define k (quotient (+ n b) d))
      (loop c d (- (* k c) a) (- (* k d) b)))
    fs))
