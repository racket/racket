#lang racket
(require rackunit rackunit/text-ui "3.rkt")

(define d0 (initialize (flat-contract integer?) =))
(define d (put (put (put d0 'a 2) 'b 2) 'c 1))

(run-tests
 (test-suite
  "dictionaries"
  (test-equal? "value for" 2 (value-for d 'b))
  (test-false "has?" (has? (rem d 'b) 'b))
  (test-equal? "count" 3 (count d))
  (test-case "contract check for put: symbol?"
             (define d0 (initialize (flat-contract integer?) =))
             (check-exn exn:fail:contract? (lambda () (put d0 "a" 2))))))
