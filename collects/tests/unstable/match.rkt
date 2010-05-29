#lang racket

(require unstable/match rackunit rackunit/text-ui "helpers.rkt")

(run-tests
 (test-suite "match.ss"
   (test-suite "match?"
     (test
      (check-true (match? (list 1 2 3)
                    (list a b c)
                    (vector x y z))))
     (test
      (check-true (match? (vector 1 2 3)
                    (list a b c)
                    (vector x y z))))
     (test
      (check-false (match? (+ 1 2 3)
                     (list a b c)
                     (vector x y z)))))
   (test-suite "as"
     (test
      (match (list 1 2 3)
        [(as ([a 0]) (list b c d)) (list a b c d)])
      (list 0 1 2 3)))))
