#lang scheme/base

(require (for-template scheme/base))

(provide internal)

(define (internal stx)
  (quasisyntax/loc stx
    (define-values ()
      (begin
        (quote-syntax #,stx)
        (#%plain-app values)))))
