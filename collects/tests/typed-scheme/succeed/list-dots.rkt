#lang typed/racket

(: f (All (a ...) ((List a ...) -> (List a ... a))))
(define (f x) x)

(: g (All (a ...) (a ... -> (List a ...))))
(define (g . x) x)

(g 7 7 7)

(: h (All (a ...) (a ... -> (Listof Any))))
(define (h . x) x)

(: h2 (All (a ...) ((Pair String a) ... -> (Listof (Pair String Any)))))
(define (h2 . x) x)

(: h3 (All (a ...) ((Pair String a) ... -> (Listof Any))))
(define (h3 . x) x)
