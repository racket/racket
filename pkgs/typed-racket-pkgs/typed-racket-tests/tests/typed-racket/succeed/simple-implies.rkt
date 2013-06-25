#lang typed/scheme

;; Example 13
(define: x : Any 7)
(define: z : (U Number String) 7)
(if (and (number? x) (string? z))
    (add1 x)
    (if (number? x)
        (add1 z)
        0))

;(and (number? x) (string? z))
