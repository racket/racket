#lang racket/base
(require "../syntax/syntax.rkt")

(provide (struct-out portal-syntax)
         make-portal-syntax)

(struct portal-syntax (content))

(define (make-portal-syntax stx)
  (unless (syntax? stx)
    (raise-argument-error 'make-portal-syntax "syntax?" stx))
  (portal-syntax stx))
