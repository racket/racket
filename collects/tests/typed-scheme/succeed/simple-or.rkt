#lang typed/scheme
(define: x : Any 7)
(define: (f [x : (U String Number)]) : Number 0)

(let ([tmp (number? x)]) (if tmp tmp (string? x)))

(if (let ([tmp (number? x)])
      (if tmp tmp (string? x)))
    (f x)
    0)

(: strnum? (Any -> Boolean : (U String Number)))
(define (strnum? x)
  (or (string? x) (number? x)))
