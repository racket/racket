#lang scheme/base
(require (for-template racket/private/shared-body
                       (only-in "teachprims.rkt" advanced-cons)))

(provide shared/proc)

(define code-insp
  (variable-reference->module-declaration-inspector
   (#%variable-reference)))

(define shared/proc
  (lambda (stx make-check-cdr)
    (shared-body stx #'advanced-cons code-insp make-check-cdr)))
