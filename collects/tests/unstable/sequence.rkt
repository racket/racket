#lang racket/base

(require rackunit rackunit/text-ui unstable/sequence)

(run-tests
 (test-suite "sequence.rkt"
   (check-true (sequence? (in-slice 1 '())))
   (check-equal? '() (for/list ([v (in-slice 1 '())]) v))
   (check-equal? '((0 1)) (for/list ([v (in-slice 3 (in-range 2))]) v))
   (check-equal? '((0 1 2) (3 4 5))
                 (for/list ([v (in-slice 3 (in-range 6))]) v))
   (check-equal? '((0 1 2) (3 4 5) (6 7))
                 (for/list ([v (in-slice 3 (in-range 8))]) v))
   (check-exn exn:fail:contract?
              (lambda () (for/list ([x (in-slice 0 (in-range 8))]) x)))))
