#lang typed/racket
(: f (case->
  (Symbol -> Symbol)
  (Symbol Symbol -> Symbol)))
(define f (case-lambda
            ((x) x)
             (w w)
            ((x y) x)
        ))

(f 'x 'y)
