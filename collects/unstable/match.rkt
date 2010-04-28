#lang racket/base

(require racket/match (for-syntax racket/base))

(provide ==)

(define-match-expander
  ==
  (lambda (stx)
    (syntax-case stx ()
      [(_ val comp)
       #'(? (lambda (x) (comp val x)))]
      [(_ val) #'(== val equal?)])))
