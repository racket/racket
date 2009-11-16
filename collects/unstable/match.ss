#lang scheme/base

(require scheme/match (for-syntax scheme/base))

(provide ==)

(define-match-expander
  ==
  (lambda (stx)
    (syntax-case stx ()
      [(_ val comp)
       #'(? (lambda (x) (comp val x)))]
      [(_ val) #'(== val equal?)])))
