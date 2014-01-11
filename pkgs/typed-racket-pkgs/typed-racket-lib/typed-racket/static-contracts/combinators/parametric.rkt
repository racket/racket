#lang racket/base

;; Static contract for parametric->/c.

(require
  "../structures.rkt"
  "../constraints.rkt"
  "../terminal.rkt"
  racket/list racket/match
  unstable/contract
  racket/contract
  (for-template racket/base racket/contract/parametric)
  (for-syntax racket/base syntax/parse))

(provide
  (contract-out
    [parametric->/sc ((listof identifier?) static-contract? . -> . static-contract?)]
    [parametric-var/sc (identifier? . -> . static-contract?)])
  parametric->/sc:
  (rename-out
    [parametric-var/sc parametric-var/sc:]))


(struct parametric-combinator combinator (vars)
  #:transparent
  #:property prop:combinator-name "parametric->/sc"
  #:methods gen:sc
    [(define (sc-map v f)
       (match v
        [(parametric-combinator (list arg) vars)
         (parametric-combinator (list (f arg 'covariant)) vars)]))
     (define (sc-traverse v f)
       (match v
        [(parametric-combinator (list arg) vars)
         (f arg 'covariant)
         (void)]))
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

(define-terminal-sc parametric-var/sc (id) #:impersonator
  #:printer (v p mode) (display (syntax-e (parametric-var/sc-id v)) p)
   id)
