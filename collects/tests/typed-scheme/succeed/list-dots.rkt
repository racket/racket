#lang typed/racket

;; tests for the new iteration of ...

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

(: h4 (All (a ...) (a ... -> Number)))
(define (h4 . x) (length x))

(: i (All (a ...) (List a ...) (a ... -> Number) -> Number))
(define (i xs f) (apply f xs))

(: i2 (All (a ...) (List a ...) (Any * -> Number) -> Number))
(define (i2 xs f) (apply f xs))

(: i3 (All (a ...) (List a ...) (List a ...) ((Pairof a a) ... -> Number) -> Number))
(define (i3 xs ys f) (apply f (map cons xs ys)))

(: i4 (All (a ...) (List a ...) (Listof Number) ((Pairof a Number) ... -> Number) -> Number))
(define (i4 xs ys f) (apply f (map cons xs ys)))
