#;
(exn:pred #rx"Expected One, but got (U Nonpositive-Integer Byte-Larger-Than-One Positive-Index-Not-Byte Positive-Fixnum-Not-Index Positive-Integer-Not-Fixnum)")
#lang typed/racket

;; test even? filter

(: foo (Integer -> String))
(define (foo n)
  (if (even? n)
      (o n)
      "dummy"))

(: o (One -> String))
(define (o x) "dummy")

