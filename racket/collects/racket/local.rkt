#lang racket/base

(require (for-syntax racket/base syntax/transformer)
         "private/local.rkt")
(provide local)

(define-syntax local
  (make-expression-transformer
   (lambda (stx)
     (do-local stx (lambda (def-ctx expand-context sbindings vbindings bodys)
                     (quasisyntax/loc stx
                       (letrec-syntaxes+values
                           #,sbindings
                         #,vbindings
                         #,@bodys)))))))
