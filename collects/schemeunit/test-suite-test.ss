#lang scheme/base

(require "check.ss"
         "test.ss")

(define run? #f)

(define-test-suite define-test
  (check = 1 1))

(define/provide-test-suite test-suite-define-provide-test
  (check = 1 1))

(define test-suite-tests
  (test-suite
   "test-suite-tests"

   ;; We rely on order of evaluation to test that checks are
   ;; converted to test cases
   
   (test-begin
    (check-false run?))
   
   (check-not-exn (lambda () (begin (set! run? #t) run?)))

   (test-begin
    (check-true run?))

   (test-case
    "define-test"
    (check-pred test-suite? define-test))

   (test-case
    "test-suite name must be string"
    (check-exn exn:fail:contract?
               (lambda ()
                 (test-suite (check = 1 1)))))
   ))



(provide test-suite-tests)