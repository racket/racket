#lang typed/scheme

(define: x : Any 7)
(define: (f [x : (U String Number)]) : Number
  (if (number? x) (add1 x) (string-length x)))
(if (let ([tmp (number? x)]) (if tmp tmp (string? x))) (f x) 0)