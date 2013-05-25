#lang typed/scheme/base

(require typed/rackunit)
(: my-+ : Integer Integer -> Integer)

(define (my-+ a b)
  (if (zero? a)
      b
      (my-+ (sub1 a) (add1 b))))

(: my-* : Integer Integer -> Integer)
(define (my-* a b)
  (if (= 1 a)
      b
      (my-* (sub1 a) (my-+ b b))))

(check-equal? (my-+ 1 1) 2 "Simple addition")

(test-begin
 (check-equal? (my-+ 1 1) 2 "Simple addition")
 (check-equal? (my-* 2 2) 4 "Simple multiplication"))
