#lang typed/racket
(provide bernoulli/tangent)

(require "tangent-number.rkt")

(define-predicate natural? Natural)

(: bernoulli/tangent : Natural -> Exact-Rational)
;   compute the n'th Bernoulli number
(define (bernoulli/tangent n)
  ; Implementation note:
  ;   The Bernoulli number is computed on the basis
  ;   of the corresponding tangent number. Since
  ;   the tangent number computation uses bignum
  ;   integers only, this may or may not be faster
  ;   for a one-of computation than the (bernoulli n)
  ;   function. [Which caches computations]
  (cond
    [(= n 0)   1]
    [(= n 1)  -1/2]
    [(= n 2)   1/6]
    [(odd? n)  0]
    [else      (let ([m (/ n 2)])
                 (* (tangent-number (assert (sub1 n) natural?))
                    (/ n
                       (let ([4^m (expt 4 m)])
                         (* 4^m (- 4^m 1))))
                    (if (even? m)
                        -1
                        1)))]))