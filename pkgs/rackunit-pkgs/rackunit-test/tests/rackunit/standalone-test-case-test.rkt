;; Here we check the standalone (not within a test-suite)
;; semantics of checks.  These tests are not part of the
;; standard test suite and must be run separately.

#lang racket/base

(require rackunit/private/check
         rackunit/private/test-case)

;; These tests should succeeds
(test-begin (check-eq? 1 1))
(test-case "succeed" (check-eq? 1 1))

;; These should raise errors
(test-begin (error "First Outta here!"))
(test-case "error" (error "Second Outta here!"))

;; Thesse should raise failures
(test-begin (check-eq? 1 2))
(test-case "failure" (check-eq? 1 2))
