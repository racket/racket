#lang racket/base

(require
  "../utils/utils.rkt"
  syntax/parse
  (for-template racket/base)
  racket/match
  racket/syntax
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
     #:with (names ...)
       (for/list ([arg (in-syntax #'(args ...))])
         (define id (generate-temporary arg))
         (datum->syntax id (syntax->datum id) arg))
     #:with (n1 n2 ns ...) #'(names ...)
     (define result
       (for/fold ([acc (quasisyntax/loc orig-stx
                         (#%plain-app . #,(syntax/loc orig-stx (op n1 n2))))])
                 ([new-n (in-syntax #'(ns ...))])
         (quasisyntax/loc orig-stx
           (#%plain-app op #,acc #,new-n))))
     (with-lexical-env/extend
       (syntax->list #'(names ...))
       (for/list ([arg (in-syntax #'(args ...))])
          (match (type-of arg)
            [(tc-result1: t) t]))
       (tc-expr/check result (type-of orig-stx)))
     (define bindings
       (for/list ([arg (in-syntax #'(args ...))]
                  [name (in-syntax #'(names ...))])
         (quasisyntax/loc arg [(#,name) #,arg])))
     #`(let-values #,bindings #,result)]))
