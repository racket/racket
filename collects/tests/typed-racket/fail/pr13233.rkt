#;
(exn-pred "Expected Zero, but got")
#lang typed/racket

;; test odd? filter

(: foo (Integer -> Zero))
(define (foo n)
  (if (odd? n)
      0
      n))

(: ZERO Zero)
(define ZERO (foo 2))

ZERO

