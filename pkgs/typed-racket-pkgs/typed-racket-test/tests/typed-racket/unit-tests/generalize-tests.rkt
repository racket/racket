#lang racket/base

(require "test-utils.rkt"
         racket/format
         rackunit
         (rep rep-utils)
         (types generalize abbrev union)
         (for-syntax racket/base syntax/parse))

(provide tests)
(gen-test-main)

(define-syntax check-generalize
  (syntax-parser
    [(_ t*:expr exp*:expr)
     #'(test-case (~a `(t* => exp*))
         (define actual (generalize t*))
         (define expected exp*)
         (with-check-info (['actual actual]
                           ['expected expected])
           (unless (type-equal? actual expected)
             (fail-check "Didn't generalize to expected type."))))]))


(define tests
  (test-suite "Generalize Tests"
    (check-generalize -Null (-lst Univ))
    (check-generalize
      (-pair -Symbol (-lst -Symbol))
      (-lst -Symbol))
    (check-generalize
      (-pair -Symbol (Un -Null (-pair -Symbol (-lst -Symbol))))
      (-lst -Symbol))
    ))
