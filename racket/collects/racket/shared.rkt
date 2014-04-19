#lang racket/base
(require (for-syntax racket/base)
         "private/shared-body.rkt")

(provide shared)

(define-for-syntax code-insp
  (variable-reference->module-declaration-inspector
   (#%variable-reference)))

(define-syntax shared
  (lambda (stx)
    (shared-body stx #'cons code-insp #f)))
