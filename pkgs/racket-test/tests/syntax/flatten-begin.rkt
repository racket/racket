#lang racket/base

;; Tests for syntax/flatten-begin

(require rackunit
         rackunit/text-ui
         syntax/flatten-begin
         (for-meta -2 (only-in racket/base begin)))

(define-binary-check (check-equal-datum? actual expected)
  (check-equal? (map syntax->datum actual)
                (map syntax->datum expected)))

(define-test-suite flatten-begin-tests
  (check-exn exn:fail:syntax? (λ () (flatten-begin #'())))
  (check-exn exn:fail:syntax? (λ () (flatten-begin #'(1))))
  (check-exn exn:fail:syntax? (λ () (flatten-begin #'(1 2))))
  (check-exn exn:fail:syntax? (λ () (flatten-begin #'(1 . 2))))
  (check-equal-datum? (flatten-begin #'(begin))
                      (list))
  (check-equal-datum? (flatten-begin #'(begin 1 2 3))
                      (list #'1 #'2 #'3))
  (check-equal-datum? (flatten-begin #'(+ 1 2 3))
                      (list #'1 #'2 #'3))
  (check-equal-datum? (flatten-begin #'(begin (begin 1 2) 3))
                      (list #'(begin 1 2) #'3)))

(define-test-suite flatten-all-begins-tests
  (check-exn exn:fail:syntax? (λ () (flatten-all-begins #'())))
  (check-exn exn:fail:syntax? (λ () (flatten-all-begins #'(1))))
  (check-exn exn:fail:syntax? (λ () (flatten-all-begins #'(1 . 2))))
  (check-exn exn:fail:syntax? (λ () (flatten-all-begins #'(1 2 3))))
  (check-exn exn:fail:syntax? (λ () (flatten-all-begins #'(begin . 1))))
  (check-equal-datum? (flatten-all-begins #'(begin 1 2 3))
                      (list #'1 #'2 #'3))
  (check-equal-datum? (flatten-all-begins #'(begin (1 2) 2 3))
                      (list #'(1 2) #'2 #'3))
  (check-equal-datum? (flatten-all-begins (syntax-shift-phase-level #'(begin 1 2 3) 2))
                      (list #'1 #'2 #'3))
  (check-equal-datum? (flatten-all-begins #'(begin (begin 1 2) 3))
                      (list #'1 #'2 #'3))
  (check-equal-datum? (flatten-all-begins #'(begin (begin 1 2) (+ 3 4) 5))
                      (list #'1 #'2 #'(+ 3 4) #'5))
  (check-equal-datum? (flatten-all-begins #'(begin (begin 1 (begin 2) 3) 4))
                      (list #'1 #'2 #'3 #'4))
  (check-equal-datum? (flatten-all-begins #'(begin (begin 1 2) (begin 3) 4))
                      (list #'1 #'2 #'3 #'4)))

(run-tests flatten-begin-tests)
(run-tests flatten-all-begins-tests)
