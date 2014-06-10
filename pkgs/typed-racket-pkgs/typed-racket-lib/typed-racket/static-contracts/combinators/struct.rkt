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
    [struct/sc (identifier? (listof identifier?) boolean? (listof static-contract?) . -> . static-contract?)])
  struct/sc:)


(struct struct-combinator combinator (name acc-ids mut?)
  #:transparent
  #:property prop:combinator-name "struct/sc"
  #:methods gen:sc
    [(define (sc-map v f)
       (match v
        [(struct-combinator args name acc-ids mut?)
         (struct-combinator (map (λ (a) (f a (if mut? 'invariant 'covariant))) args)
                            name acc-ids mut?)]))
     (define (sc-traverse v f)
       (match v
        [(struct-combinator args name acc-ids mut?)
         (for-each (λ (a) (f a (if mut? 'invariant 'covariant))) args)
         (void)]))
     (define (sc->contract v f)
       (match v
        [(struct-combinator args name acc-ids _)
         #`(struct/dc #,name #,@(for/list ([arg args] [acc-id acc-ids])
                                  #`((#:selector #,acc-id) () #:lazy #,(f arg))))]))
     (define (sc->constraints v f)
       (match v
        [(struct-combinator args _ acc-ids mut?)
         (merge-restricts*
           'chaperone
           (map (lambda (a) (if (not mut?) (add-constraint (f a) 'chaperone) (f a))) args))]))])

(define (struct/sc name acc-ids mut? fields)
  (struct-combinator fields name acc-ids mut?))

(define-match-expander struct/sc:
  (syntax-parser
    [(_ name fields)
     #'(struct-combinator fields name _ _)]))
