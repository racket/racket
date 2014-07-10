#lang racket/base

;; Static contract for struct/c.

(require "../structures.rkt" "../constraints.rkt"
         racket/list racket/match
         unstable/contract
         racket/contract
         (for-template racket/base racket/contract/base "../../utils/struct-type-c.rkt")
         (for-syntax racket/base syntax/parse))



(provide
  (contract-out
    [struct/sc (identifier? boolean? (listof static-contract?) . -> . static-contract?)]
    ;; #f as argument indicates StructTypeTop, which should fail on
    ;; all reflective operations.
    [struct-type/sc (any/c . -> . static-contract?)])
  struct/sc:
  struct-type/sc:)


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

;; FIXME: Currently ignores the structure type and fails on all
;; reflective use.  
(struct struct-type/sc combinator ()
  #:transparent
  #:property prop:combinator-name "struct-type/sc"
  #:methods gen:sc
    [(define (sc-map v f)
       (match v
         [(struct-type/sc args)
          (struct-type/sc (map (λ (a) (f a 'covariant)) args))]))
     (define (sc-traverse v f)
       (match v
         [(struct-type/sc args)
          (for-each (λ (a) (f a 'covariant)) args)
          (void)]))
     (define (sc->contract v f)
       (match v
         [(struct-type/sc args)
          #`(struct-type/c #f)]))
     (define (sc->constraints v f)
       (match v
         [(struct-type/sc args) (simple-contract-restrict 'chaperone)]))])

(define-match-expander struct-type/sc:
  (syntax-parser
    [(_ args)
     #'(struct-type/sc args)]))
