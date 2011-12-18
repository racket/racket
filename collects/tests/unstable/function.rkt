#lang racket
(require rackunit rackunit/text-ui unstable/function
         "helpers.rkt")

(run-tests
 (test-suite "function.rkt"

   (test-suite "Higher Order Predicates"

     (test-suite "conjoin"
       (test-case "no functions"
         (check-true ((conjoin) 'x #:y 'z)))
       (test-case "true"
         (check-true ((conjoin integer? exact?) 1)))
       (test-case "false"
         (check-false ((conjoin integer? exact?) 1.0)))
       (test-case "false"
         (check-false ((conjoin integer? exact?) 0.5))))

     (test-suite "disjoin"
       (test-case "no functions"
         (check-false ((disjoin) 'x #:y 'z)))
       (test-case "true"
         (check-true ((disjoin integer? exact?) 1)))
       (test-case "true"
         (check-true ((disjoin integer? exact?) 1/2)))
       (test-case "false"
         (check-false ((disjoin integer? exact?) 0.5)))))
   ))
