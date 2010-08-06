#lang typed/racket

(: f (Boxof (Number -> Number)))
(define f (box (lambda: ([x : Number]) x)))

(provide f)
