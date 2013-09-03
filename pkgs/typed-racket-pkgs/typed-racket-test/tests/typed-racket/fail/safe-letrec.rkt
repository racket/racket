#;
(exn-pred 2)
#lang typed/racket

;; make sure letrec takes into account that some bindings may be undefined

(+ (letrec: ([x : Float x]) x) 1) ; PR 11511

(letrec: ([x : Float (+ x 1)]) 0) ; error in rhs
