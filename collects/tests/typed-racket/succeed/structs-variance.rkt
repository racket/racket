#lang typed/racket

(struct: (A) x ([f : (A -> A)]))
(struct: (B) y ([f : (x B)]))

(: f : (U Integer (y Integer)) -> Integer)
(define (f v)
  (if (y? v)
      ((x-f (y-f v)) 0)
      v))

(f 17)

(f (y (x (lambda: ([n : Integer]) (+ 2 n)))))
