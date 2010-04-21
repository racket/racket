#lang typed/scheme

;; Example 13
(define: x : Any 7)
(define: z : (U Number String) 7)
(cond [(and (number? x) (string? z)) (add1 x)]
      [(number? x) (add1 z)]
      [else 0])

;(and (number? x) (string? z))
