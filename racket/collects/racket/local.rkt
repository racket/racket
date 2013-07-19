#lang racket/base

(require (for-syntax racket/base)
         "private/local.rkt")
(provide local)

(define-syntax (local stx)
  (do-local stx #'letrec-syntaxes+values))
