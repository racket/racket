#lang racket

(provide test
         test-ok check-ok
         test-bad check-bad)

(require rackunit racket/pretty)

(define-syntax-rule (test e ...)
  (test-case (parameterize ([pretty-print-columns 50])
               (pretty-format/write '(test e ...)))
    e ...))
(define-syntax-rule (test-ok e ...) (test (check-ok e ...)))
(define-syntax-rule (test-bad e ...) (test (check-bad e ...)))
(define-syntax-rule (check-ok e ...) (check-not-exn (lambda () e ...)))
(define-syntax-rule (check-bad e ...) (check-exn exn:fail? (lambda () e ...)))

(define (pretty-format/write x)
  (with-output-to-string
    (lambda () (pretty-write x))))
