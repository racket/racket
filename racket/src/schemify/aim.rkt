#lang racket/base
(require (for-syntax racket/base))
(provide aim?)

;; macro statically ensures that the second argument is a valid target
(define-syntax (aim? stx)
  (syntax-case stx (quote)
    [(_ e 'target)
     (and (identifier? #'target)
          (memq (syntax->datum #'target) '(cify interp system)))
     #'(eq? e 'target)]))
