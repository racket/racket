#lang racket/base

;; Test syntax lifting in `with-type`

(require rackunit typed/racket)

(with-type #:result Number
  (define-syntax (m stx)
    (syntax-local-lift-expression #'(+ 1 2)))
  (m))

(define-syntax (m2 stx)
  (syntax-local-lift-expression #'(+ 1 2)))

(with-type #:result Number (m2))

(with-type ([val Number]) (define val (m2)))
(check-equal? val 3)
