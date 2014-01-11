#lang racket/base

;; Static contract for struct/c.

(require "../structures.rkt" "../constraints.rkt"
         racket/list racket/match
         unstable/contract
         racket/contract
         (for-template racket/base racket/contract/base)
         (for-syntax racket/base syntax/parse))

(provide
  (contract-out
    [struct/sc (identifier? boolean? (listof static-contract?) . -> . static-contract?)])
  struct/sc:)


(struct struct-combinator combinator (name mut?)
  #:transparent
  #:property prop:combinator-name "struct/sc"
  #:methods gen:sc
    [(define (sc-map v f)
       (match v
        [(struct-combinator args name mut?)
         (struct-combinator (map (λ (a) (f a (if mut? 'invariant 'covariant))) args)
                            name mut?)]))
     (define (sc-traverse v f)
       (match v
        [(struct-combinator args name mut?)
         (for-each (λ (a) (f a (if mut? 'invariant 'covariant))) args)
         (void)]))
     (define (sc->contract v f)
       (match v
        [(struct-combinator args name _)
         #`(struct/c #,name #,@(map f args))]))
     (define (sc->constraints v f)
       (match v
        [(struct-combinator args _ mut?)
         (merge-restricts*
           (if mut? 'chaperone 'flat)
           (map (lambda (a) (if (not mut?) (add-constraint (f a) 'chaperone) (f a))) args))]))])

(define (struct/sc name mut? fields)
  (struct-combinator fields name mut?))

(define-match-expander struct/sc:
  (syntax-parser
    [(_ name fields)
     #'(struct-combinator fields name _)]))
