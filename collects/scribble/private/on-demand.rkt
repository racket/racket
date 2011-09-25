#lang racket/base
(require racket/promise
         (for-syntax racket/base))
(provide define-on-demand)

(define-syntax-rule (define-on-demand id rhs)
  (begin
    (define val (delay rhs))
    (define-syntax (id stx)
      (if (identifier? stx)
          #'(force val)
          (raise-syntax-error #f "bad syntax" stx)))))
