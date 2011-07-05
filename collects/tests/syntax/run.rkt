#lang racket/base

(require rackunit rackunit/text-ui racket/runtime-path)

;; Runs all the files in the tests subdirectory.
;; A test fails if it throws an exception.

(define-runtime-path tests-dir "./tests")

(define tests
  (make-test-suite
   "syntax tests"
   (for/list ([t (in-directory tests-dir)])
     (test-suite
      (path->string t)
      (check-not-exn (lambda () (dynamic-require t #f)))))))

(run-tests tests)
