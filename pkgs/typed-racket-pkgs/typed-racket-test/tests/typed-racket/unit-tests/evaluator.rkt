#lang racket/base
;; Allow evaluation at phase1
(require (for-syntax racket/base syntax/parse))
(provide phase1-eval phase1-phase0-eval phase1-phase0-run)

(define-namespace-anchor anchor)
(define namespace (namespace-anchor->empty-namespace anchor))

(define-syntax phase1-phase0-run
  (syntax-parser
    [(_ form:expr ...)
     #'(let-syntax ([go (lambda (stx) form ...)]) (go))]))

(define-syntax phase1-phase0-eval
  (syntax-parser
    [(_ form:expr ...)
     #'(eval-syntax (quote-syntax (phase1-phase0-run form ...)) namespace)]))

(define-syntax phase1-eval
  (syntax-parser
    [(_ form:expr ...)
     #'(phase1-phase0-eval form ... #'(void))]))
