#;
(exn-pred 3)
#lang typed/racket

;; make sure letrec takes into account that some bidings may be undefined

(+ (letrec: ([x : Float x]) x) 1) ; PR 11511

(letrec: ([x : Number 3]
          [y : Number z] ; bad
          [z : Number x])
         z)

(letrec: ([x : Number 3]
          [y : (Number -> Number) (lambda (x) z)] ; bad
          [z : Number x]
          [w : (Number -> Number) (lambda (x) (y x))]) ; bad too
         z)
