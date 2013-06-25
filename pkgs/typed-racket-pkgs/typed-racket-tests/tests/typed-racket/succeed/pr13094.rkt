#lang typed/racket

;; Test alpha equivalent types

(: x (All (A) (A -> A)))
(define x (plambda: (C) ((f : C)) f))

(: y (All (A) (A A -> A)))
(define y (plambda: (C) ((f : C) (g : A)) f))

(: z (All (B) (B (B -> B) -> B)))
(define z (plambda: (C) ((x : C) (f : (B -> B))) (f x)))
