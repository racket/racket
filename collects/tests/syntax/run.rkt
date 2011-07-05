#lang racket/base

(require rackunit rackunit/text-ui racket/runtime-path)

;; Runs all the files in the tests subdirectory.
;; A test fails if it throws an exception.

(define-runtime-path tests-dir "./tests")

(define tests
  (make-test-suite
   "syntax tests"
   (for/list ([t (directory-list tests-dir)]
              #:when (regexp-match ".*rkt$" t))
     (test-suite
      (path->string t)
      (check-not-exn (lambda ()
                       (dynamic-require (build-path tests-dir t) #f)))))))

(run-tests tests)
