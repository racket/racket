#lang typed-scheme

(: f (All (a) (a -> a)))
(define (f x) x)

(define: x : (Number -> Number) f)

#;
((lambda: ([f : (All (a ...) (a ... a -> Number))]) 12)
 +)

#;(Lambda (a ...)
  ((lambda: ([f : (a .. a -> Number)]) 12) +))

#|
(: g (All (a ...) ((a ... a -> Number) -> Number)))

(define (g x) 3)

|#
