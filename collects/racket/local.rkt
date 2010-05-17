#lang scheme/base
(require (for-syntax scheme/base)
         "private/local.rkt")
(provide local)

(define-syntax (local stx)
  (do-local stx #'letrec-syntaxes+values))
