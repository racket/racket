#;
(exn-pred #rx"Type Checker: Declaration for `n' provided, but `n' has no definition")
#lang typed/racket

;; Similar to pr14246.rkt, but for local definitions
(: foo : Number -> Number)
(define (foo a)
  (: n : Number)
  (: i : Integer)
  (: s : String)
  a)

