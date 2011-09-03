#lang typed/racket

;; test for optional annotation on for:-bound variables

(for: ([i (in-range 10)] ; no annotation
       [j : Integer (in-range 10 20)])
      (display (+ i j)))

(for/fold: : Integer ([acc 0])
           ([i (in-range 10)])
           (+ i acc))

(let ((x '(1 3 5 7 9)))
  (do: : Number ((x x (cdr x))
                 (sum 0 (+ sum (car x))))
       ((null? x) sum)))
