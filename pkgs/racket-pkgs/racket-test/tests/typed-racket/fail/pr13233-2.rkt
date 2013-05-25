#;
(exn:pred #rx"Expected Zero, but got (U Nonpositive-Integer Byte-Larger-Than-One Positive-Index-Not-Byte Positive-Fixnum-Not-Index Positive-Integer-Not-Fixnum)")
#lang typed/racket

;; test odd? filter

(: foo (Integer -> String))
(define (foo n)
  (if (odd? n)
      "dummy"
      (o n)))

(: o (One -> String))
(define (o x) "dummy")

