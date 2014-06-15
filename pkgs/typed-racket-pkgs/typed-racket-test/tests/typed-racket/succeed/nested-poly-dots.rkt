#lang typed/racket

(: foo (All (b ...) ((List (b ... b -> b) ... b) -> Void)))
(define (foo x)
  (void))

(foo (list (λ: ([x : String] [y : Symbol]) x) (λ ([x : String] [y : Symbol]) y)))
