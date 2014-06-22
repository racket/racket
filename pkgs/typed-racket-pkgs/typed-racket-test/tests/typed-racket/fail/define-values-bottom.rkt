#;
(exn-pred "runtime")
#lang typed/racket

(define-values (a b)
  (error 'runtime))

b
