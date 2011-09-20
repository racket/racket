#lang scheme/base

(require (for-syntax scheme/base
                     syntax/stx
                     syntax/kerncase
                     syntax/struct
                     racket/struct-info
                     scheme/include))

(provide shared)

(define-for-syntax code-insp (variable-reference->module-declaration-inspector
                              (#%variable-reference)))

(define undefined (letrec ([x x]) x))
(require (only-in scheme/base [cons the-cons]))

(define-syntax shared
  (lambda (stx)
    (define make-check-cdr #f)
    ;; Include the implementation.
    ;; See private/shared-body.rkt.
    (include "private/shared-body.rkt")))
