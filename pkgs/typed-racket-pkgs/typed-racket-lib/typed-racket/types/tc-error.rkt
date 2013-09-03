#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep)
         (utils tc-utils)
         "base-abbrev.rkt"
         (prefix-in c: (contract-req))
         racket/match)

(provide/cond-contract
 [tc-error/expr ((string?) (#:return c:any/c #:stx syntax?) #:rest (c:listof c:any/c)
                 . c:->* . c:any/c)]

 [lookup-fail (identifier? . c:-> . Type/c)]
 [lookup-type-fail (identifier? . c:-> . Type/c)])

(define (tc-error/expr msg
                       #:return [return -Bottom]
                       #:stx [stx (current-orig-stx)]
                       . rest)
  (apply tc-error/delayed #:stx stx msg rest)
  return)

;; error for unbound variables
(define (lookup-fail e)
  (match (identifier-binding e)
    ['lexical (tc-error/expr "untyped identifier ~a" (syntax-e e))]
    [#f (tc-error/expr "untyped top-level identifier ~a" (syntax-e e))]
    [(list _ _ nominal-source-mod nominal-source-id _ _ _)
     (let-values ([(x y) (module-path-index-split nominal-source-mod)])
       (cond [(and (not x) (not y))
              (tc-error/expr "untyped identifier ~a" (syntax-e e))]
             [else
              (tc-error/expr "untyped identifier ~a imported from module <~a>"
                             (syntax-e e) x)]))]))

(define (lookup-type-fail i)
  (tc-error/expr "~a is not bound as a type" (syntax-e i)))
