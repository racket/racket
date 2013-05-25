#lang racket/base
(require racket/promise
         (for-syntax racket/base))
(provide define-on-demand)

(define-syntax-rule (define-on-demand id rhs)
  (begin
    (define define-on-demand-bound-promise (delay rhs))
    (define-syntax (id stx)
      (if (identifier? stx)
          #'(force define-on-demand-bound-promise)
          (raise-syntax-error #f "bad syntax" stx)))))
