#;
(exn-pred "runtime")
#lang typed/racket

;; Make sure -Bottom and multiple values play nice together at the module level.
(define-values (a b)
  (error 'runtime))

b
