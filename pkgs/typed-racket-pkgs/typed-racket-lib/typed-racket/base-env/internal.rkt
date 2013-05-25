#lang racket/base

(require (for-template racket/base))

(provide internal)

(define (internal stx)
  (quasisyntax/loc stx
    (define-values ()
      (begin
        (quote-syntax #,stx)
        (#%plain-app values)))))
