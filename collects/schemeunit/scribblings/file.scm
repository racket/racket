#lang scheme/base

(define (my-+ a b)
  (if (zero? a)
      b
      (my-+ (sub1 a) (add1 b))))

(define (my-* a b)
  (if (zero? a)
      b
      (my-* (sub1 a) (my-+ b b))))

(provide my-+
         my-*)
