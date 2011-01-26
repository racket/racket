#lang racket/base

(require tests/eli-tester lazy)

;; tests for lazy language constructs

(define (test-take)
  (test (take 0 '(1 2 3)) => '())) ; test for push#22080

(provide langimpl-tests)
(define (langimpl-tests)
  (test do (test-take)))