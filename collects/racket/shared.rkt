#lang racket/base
(require (for-syntax racket/base
                     syntax/kerncase
                     syntax/struct
                     racket/struct-info
                     racket/include))

(provide shared)

(define-for-syntax code-insp (variable-reference->module-declaration-inspector
                              (#%variable-reference)))

(define undefined (letrec ([x x]) x))
(require (only-in racket/base [cons the-cons]))

(define-syntax shared
  (lambda (stx)
    (define make-check-cdr #f)
    ;; Include the implementation.
    ;; See private/shared-body.rkt.
    (include "private/shared-body.rkt")))
