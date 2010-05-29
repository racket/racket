#lang racket/base

(require racket/match (for-syntax racket/base))

(provide == match? as)

(define-match-expander
  ==
  (lambda (stx)
    (syntax-case stx ()
      [(_ val comp)
       #'(? (lambda (x) (comp val x)))]
      [(_ val) #'(== val equal?)])))

(define-syntax-rule (match? e p ...)
  (match e [p #t] ... [_ #f]))

(define-match-expander as
  (syntax-rules ()
    [(as ([x e] ...) p ...) (and (app (lambda (y) e) x) ... p ...)]))
