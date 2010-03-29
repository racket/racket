#lang scheme/base

(require schemeunit
         "check-test.ss"
         "check-info-test.ss"
         "format-test.ss"
         "test-case-test.ss"
         "test-suite-test.ss"
         "base-test.ss"
         "location-test.ss"
         "result-test.ss"
         "test-test.ss"
         "util-test.ss"
         "text-ui-test.ss"
         "monad-test.ss"
         "hash-monad-test.ss"
         "counter-test.ss"
         "text-ui-util-test.ss")

(provide all-schemeunit-tests
         success-and-failure-tests)

(define all-schemeunit-tests
  (test-suite
   "All SchemeUnit Tests"
   check-tests
   base-tests
   check-info-tests
   test-case-tests
   test-suite-tests
   test-suite-define-provide-test
   location-tests
   result-tests
   test-tests
   util-tests
   text-ui-tests
   monad-tests
   hash-monad-tests
   counter-tests
   text-ui-util-tests
   format-tests
   ))

;; These tests fail. The are intended to do this so a human can manually check the output they produce. They should not be run by DrDr as they will generate bogus warnings.
(define success-and-failure-tests
  (test-suite
   "Successes and Failures"
   all-schemeunit-tests
   (test-case "Intended to fail" (fail))
   (test-case "Also intended to fail" (check-eq? 'apples 'orange))
   (test-equal? "Yet again intended to fail" "apples" "oranges")
   (test-case "Intended to throw error" (error 'testing "<<This is an error message>>"))
   (test-case "Error within a check" (check error 'foo 'bar))
   ))

