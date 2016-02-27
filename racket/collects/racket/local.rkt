#lang racket/base

(require (for-syntax racket/base)
         "private/local.rkt")
(provide local)

(define-syntax (local stx)
  (do-local stx (lambda (def-ctx expand-context sbindings vbindings bodys)
                  (quasisyntax/loc stx
                    (letrec-syntaxes+values
                     #,sbindings
                     #,vbindings
                     #,@bodys)))))
