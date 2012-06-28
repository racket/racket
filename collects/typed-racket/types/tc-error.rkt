#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep filter-rep object-rep rep-utils)
         (utils tc-utils)         
         racket/match)

(provide tc-error/expr lookup-type-fail lookup-fail)

(define (tc-error/expr msg
                       #:return [return (make-Union null)]
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
