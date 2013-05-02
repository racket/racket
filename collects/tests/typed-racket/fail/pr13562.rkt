#;
(exn-pred #rx"struct:: expected the literal")

#lang typed/racket

;; Check that #:methods is ruled out
(struct: foo ([a : Integer]) #:methods gen:dict [])

