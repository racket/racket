#;
(exn:pred #rx"Expected Zero, but got (U Negative-Integer Positive-Integer)")
#lang typed/racket

;; test even? filter

(: foo (Integer -> String))
(define (foo n)
  (if (even? n)
      "dummy"
      (z n)))

(: z (Zero -> String))
(define (z x) "dummy")

