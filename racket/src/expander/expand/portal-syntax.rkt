#lang racket/base
(require "../common/contract.rkt"
         "../syntax/syntax.rkt")

(provide (struct-out portal-syntax)
         portal-syntax-content
         make-portal-syntax)

(struct portal-syntax (get-content ; syntax or (int -> syntax)
                       pos)        ; #f implies that `get-content` is syntax
  #:authentic)

(define (make-portal-syntax stx)
  (unless (syntax? stx)
    (raise-argument-error 'make-portal-syntax "syntax?" stx))
  (portal-syntax stx #f))

(define/who (portal-syntax-content p)
  (unless (portal-syntax? p)
    (raise-argument-error who "portal-syntax?" p))
  (define pos (portal-syntax-pos p))
  (if pos
      ((portal-syntax-get-content p) pos)
      (portal-syntax-get-content p)))
