#lang racket/base
(provide rectangles-intersect?)

(define (rectangles-intersect? l1 t1 r1 b1 l2 t2 r2 b2)
  (or (point-in-rectangle? l1 t1 l2 t2 r2 b2)
      (point-in-rectangle? r1 t1 l2 t2 r2 b2)
      (point-in-rectangle? l1 b1 l2 t2 r2 b2)
      (point-in-rectangle? r1 b1 l2 t2 r2 b2)))

(define (point-in-rectangle? x y l t r b)
  (and (<= l x r)
       (<= t y b)))
