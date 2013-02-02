#lang racket/base

(require rackunit rackunit/text-ui unstable/sequence)

(run-tests
 (test-suite "sequence.rkt"
  (check-equal? (for/list ([x (sequence-lift add1 (in-range 10))])
                  x)
                '(1 2 3 4 5 6 7 8 9 10))
  (check-equal? (for/list ([x (sequence-lift 
                               + (in-parallel (in-range 10) (in-range 10)))])
                  x)
                '(0 2 4 6 8 10 12 14 16 18))
  (check-equal? (for/list ([x (in-sequence-forever (in-range 5) 5)]
                           [y (in-range 10)])
                  x)
                '(0 1 2 3 4 5 5 5 5 5))
  (check-equal? (for/list ([(x y) (in-pairs '((1 . 1) (2 . 2) (3 . 3)))])
                  (+ x y))
                '(2 4 6))   
  (check-true (sequence? (in-slice 1 '())))
  (check-equal? '() (for/list ([v (in-slice 1 '())]) v))
  (check-equal? '((0 1)) (for/list ([v (in-slice 3 (in-range 2))]) v))
  (check-equal? '((0 1 2) (3 4 5))
                (for/list ([v (in-slice 3 (in-range 6))]) v))
  (check-equal? '((0 1 2) (3 4 5) (6 7))
                (for/list ([v (in-slice 3 (in-range 8))]) v))
  (check-exn exn:fail:contract?
             (lambda () (for/list ([x (in-slice 0 (in-range 8))]) x)))))
