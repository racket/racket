#lang racket/base

(require racket/fixnum
         rackunit)

(test-case "bitwise-and on a positive fixnum"
  (define (f a)
    (fixnum? (bitwise-and a (most-positive-fixnum))))

  (check-true (f (- (random 1) 1))))

(test-case "bitwise-and on a large positive big-integer"
  (define (f a)
    (fixnum? (bitwise-and a (add1 (most-positive-fixnum)))))

  (check-false (f (- (random 1) 1))))

(test-case "bitwise-ior on a negative fixnum"
  (define (g a)
    (fixnum? (bitwise-ior (most-negative-fixnum) a)))

  (check-true (g (random 1))))

(test-case "bitwise-ior on a large negative big-integer"
  (define (g a)
    (fixnum? (bitwise-ior (sub1 (most-negative-fixnum)) a)))

  (check-false (g (random 1))))
