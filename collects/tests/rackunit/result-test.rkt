#lang racket/base

(require rackunit
         rackunit/private/result)

(provide result-tests)
  
(define result-tests
  (test-suite
   "All tests for result"

   (test-equal?
    "fold-test-results returns seed"
    (fold-test-results
     (lambda (result seed) seed)
     'hello
     (delay-test (test-case "Demo" (check = 1 1)))
     #:fdown (lambda (name seed) seed)
     #:run run-test-case)
    'hello)

   (test-equal?
    "fold-test-results return values of run to result-fn"
    (fold-test-results
     (lambda (v1 v2 seed)
       (check-equal? v1 'v1)
       (check-equal? v2 'v2)
       seed)
     'hello
     (delay-test (test-case "Demo" (check = 1 1)))
     #:run (lambda (name action) (values 'v1 'v2)))
    'hello)

   (test-equal?
    "fold-test-results calls run with name and action"
    (fold-test-results
     (lambda (result seed) seed)
     'hello
     (delay-test (test-case "Demo" 'boo))
     #:run (lambda (name action)
             (check string=? name "Demo")
             (check-equal? (action) 'boo)))
    'hello)
   ))
