#lang racket/base

(require "../structures.rkt" "../constraints.rkt"
         racket/list racket/match
         unstable/contract
         (except-in racket/contract recursive-contract)
         (for-template racket/base racket/contract/parametric)
         (for-syntax racket/base syntax/parse))

(provide
  (contract-out
    [parametric->/sc ((listof identifier?) static-contract? . -> . static-contract?)])
  parametric->/sc:)


(struct parametric-combinator combinator (vars)
  #:transparent
  #:property prop:combinator-name "parametric->/sc"
  #:methods gen:sc
    [(define (sc-map v f)
       (match v
        [(parametric-combinator (list arg) vars)
         (parametric-combinator (list (f arg 'covariant)) vars)]))
     (define (sc->contract v f)
       (match v
        [(parametric-combinator (list arg) vars)
         #`(parametric->/c #,vars #,(f arg))]))
     (define (sc->constraints v f)
       (match v
        [(parametric-combinator (list arg) vars)
         (merge-restricts* 'impersonator  (list (f arg)))]))])

(define (parametric->/sc vars body)
  (parametric-combinator (list body) vars))

(define-match-expander parametric->/sc:
  (syntax-parser
    [(_ vars body)
     #'(parametric-combinator (list body) vars)]))
