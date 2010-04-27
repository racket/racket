#lang scheme/base

(require (for-syntax scheme/base
                     syntax/stx
                     syntax/kerncase
                     syntax/struct
                     scheme/include))

(provide shared)

(define undefined (letrec ([x x]) x))
(require (only-in scheme/base [cons the-cons]))

(define-syntax shared
  (lambda (stx)
    (define make-check-cdr #f)
    ;; Include the implementation.
    ;; See private/shared-body.ss.
    (include "private/shared-body.rkt")))
