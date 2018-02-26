#lang racket/base
(require (for-syntax racket/base))

(provide check)

(define-syntax-rule (check who pred arg)
  (unless (pred arg)
    (raise-argument-error who (as-string pred) arg)))

(define-syntax (as-string stx)
  (syntax-case stx ()
    [(_ id)
     (datum->syntax stx (symbol->string (syntax-e #'id)) stx)]))
