#;
(exn-pred "x:.*cannot use before initialization")
#lang typed/racket

;; This test previously tested TR's static analysis of undefined
;; variables, but it now just delegates to Racket's dynamic checks.

(+ (letrec: ([x : Float x]) x) 1) ; PR 11511

(letrec: ([x : Float (+ x 1)]) 0) ; error in rhs
