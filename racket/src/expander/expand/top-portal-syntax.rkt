#lang racket/base
(require "../namespace/namespace.rkt"
         "bind-top.rkt"
         "portal-syntax.rkt")

(provide make-top-add-defined-portal)

(define (make-top-add-defined-portal ns ctx generated-syms)
  (lambda (id phase portal-stx orig-s)
    (define-values (ids syms) (as-expand-time-top-level-bindings (list id) orig-s ctx))
    (define sym (car syms))
    (when phase
      (define t (portal-syntax portal-stx))
      (namespace-set-transformer! ns phase sym t))
    (set-box! generated-syms (cons sym (unbox generated-syms)))
    sym))
