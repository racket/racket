#lang typed/racket

(: f2 (Boolean Integer Integer -> Integer))
(define (f2 add? i1 i2)
  (if add?
      (+ i1 i2)
      (- i1 i2)))

(: f3 (Boolean Integer Integer Integer -> Integer))
(define (f3 add? i1 i2 i3)
  (if add?
      (+ i1 i2 i3)
      (- i1 i2 i3)))

(define f1a (curry f2 #t))
(define f1b (curry f3 #t))
(define f2a (curry f2))
(define f2b (curry f3))

(f2 #t 2 3)
(f2 #f 2 1)
(f1a 10 20)
(f1b 10 20 30)
((f2a #t) 10 20)
((f2b #t) 10 20 30)
