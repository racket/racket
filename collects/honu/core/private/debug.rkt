#lang scheme/base

(require (for-syntax scheme/base))

(provide debug)

(define-for-syntax verbose? #f)
(define-syntax (debug stx)
  (if verbose?
    (syntax-case stx ()
      [(_ str x ...)
       #'(printf str x ...)])
    #'(void)))

