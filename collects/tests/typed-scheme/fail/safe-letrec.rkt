#;
(exn-pred 1)
#lang typed/racket

;; make sure letrec takes into account that some bidings may be undefined

(+ (letrec: ([x : Float x]) x) 1) ; PR 11511
