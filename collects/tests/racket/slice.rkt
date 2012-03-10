#lang racket/base

(module fac1 racket/base
  (printf "fac1 running\n")
  (require racket/slice)  
  (define (! n)
    (if (zero? n)
      1
      (* n (! (sub1 n)))))
  (slice test
    (printf "fac1 testing\n")
    (require rackunit)
    (check-equal? (! 0) 1))
  (slice test
    (check-equal? (! 1) 1)
    (check-equal? (! 5) 120)))

(require (submod 'fac1 test))
