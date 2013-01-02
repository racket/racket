#lang typed/racket/base

(: f : Integer -> (Listof Any))
(define (f x)
  (let: loop ([i : Integer 0] [l '()])
        (if (> i x)
            l
            (loop (add1 i) (cons i l)))))
(f 10)
