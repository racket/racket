#lang racket/base

(require
  "../utils/utils.rkt"
  syntax/parse
  (for-template racket/base)
  racket/match
  unstable/sequence
  (types type-table tc-result)
  (typecheck typechecker)
  (env lexical-env)
  (optimizer utils))

(provide binary-expand)

;; Converts (op a1 a2 a3 ...+) into (let ([ni ai] ...) ((op (op n1 n2) n3) ...+))
(define (binary-expand orig-stx)
  (syntax-parse orig-stx
    #:literal-sets (kernel-literals)
    [(#%plain-app op:id (~between args 3 +inf.0) ...)
     #:with (names ...) (generate-temporaries #'(args ...))
     #:with (n1 n2 ns ...) #'(names ...)
     #:with inner
       (let ([stx (for/fold ([acc (quasisyntax/loc orig-stx
                                    (#%plain-app . #,(syntax/loc orig-stx (op n1 n2))))])
                            ([new-n (in-syntax #'(ns ...))])
                    (quasisyntax/loc orig-stx
                      (#%plain-app op #,acc #,new-n)))])
         (with-lexical-env/extend 
           (syntax->list #'(names ...))
           (for/list ([arg (in-syntax #'(args ...))])
              (match (type-of arg)
                [(tc-result1: t) t]))
           (tc-expr/check stx (type-of orig-stx))
         stx))
     #'(let-values ([(names) args] ...) inner)]))
