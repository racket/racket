#;
(exn-pred "expected: Zero\n  given:")
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

