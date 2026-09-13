#lang racket/base
(require syntax/parse/pre
         "provide.rkt"
         syntax/contract
         (only-in "../private/residual.rkt"
                  this-context-syntax
                  this-role)
         racket/contract/base)

(define not-given (gensym))

(define-syntax-class (expr/c ctc-stx)
  (pattern 10))

(provide-syntax-class/contract
 [expr/c (syntax-class/c (syntax?)
                         ())])
