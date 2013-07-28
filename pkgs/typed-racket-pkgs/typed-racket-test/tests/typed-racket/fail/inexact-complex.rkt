#;
(exn-pred 2)
#lang typed/scheme

(ann 1+2i Inexact-Complex)

(: f (Real -> Inexact-Complex))
(define (f x)
  (* x 2.0)) ; x can be exact 0
