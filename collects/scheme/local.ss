#lang scheme/base
(require (for-syntax scheme/base)
         "private/local.ss")
(provide local)

(define-syntax (local stx)
  (do-local stx #'letrec-syntaxes+values))
