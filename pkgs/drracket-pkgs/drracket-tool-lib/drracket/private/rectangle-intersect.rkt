#lang racket/base
(provide rectangles-intersect?)

(define (rectangles-intersect? l1 t1 r1 b1 l2 t2 r2 b2)
  (or (rectangles-intersect-one-way? l1 t1 r1 b1 l2 t2 r2 b2)
      (rectangles-intersect-one-way? l2 t2 r2 b2 l1 t1 r1 b1)))

(define (rectangles-intersect-one-way? l1 t1 r1 b1 l2 t2 r2 b2)
  (or (horizontal-segment-in-rectangle? l1 r1 t1 
                                        l2 t2 r2 b2)
      (horizontal-segment-in-rectangle? l1 r1 b1 
                                        l2 t2 r2 b2)
      (vertical-segment-in-rectangle?   l1 t1 b1
                                        l2 t2 r2 b2)
      (vertical-segment-in-rectangle?   r1 t1 b1
                                        l2 t2 r2 b2)))

(define (vertical-segment-in-rectangle? x y1 y2 l t r b)
  (and (<= l x r)
       (or (<= t y1 b)
           (<= t y2 b)
           (and (<= y1 t)
                (<= b y2)))))

(define (horizontal-segment-in-rectangle? x1 x2 y l t r b)
  (and (<= t y b)
       (or (<= l x1 r)
           (<= l x2 r)
           (and (<= x1 l)
                (<= r x2)))))

(module+ test
  (require rackunit)
  (check-equal? (rectangles-intersect? 0 0 10 10
                                       2 2 8 8)
                #t)
  (check-equal? (rectangles-intersect? 2 2 8 8
                                       0 0 10 10)
                #t)
  (check-equal? (rectangles-intersect? 0 0 10 10
                                       0 0 10 10)
                #t)
  (check-equal? (rectangles-intersect? 10 10 20 20
                                       0 15 30 17)
                #t)
  (check-equal? (rectangles-intersect? 0 15 30 17
                                       10 10 20 20)
                #t)
  (check-equal? (rectangles-intersect? 0 0 10 10
                                       20 20 40 40)
                #f)
  (check-equal? (rectangles-intersect? 0 0 10 10
                                       25 0 27 10)
                #f))
