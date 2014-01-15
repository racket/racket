#lang racket/base

;; Static contract for case->.
;; Like case-> doesn't support keyword arguments.

(require "../structures.rkt" "../constraints.rkt"
         racket/list racket/match
         unstable/contract
         racket/contract
         (for-template racket/base racket/contract/base)
         (for-syntax racket/base syntax/parse))

(provide
  (contract-out
    [case->/sc ((listof arr-combinator?) . -> . static-contract?)]
    [arr/sc (-> (listof static-contract?)
                (maybe/c static-contract?)
                (maybe/c (listof static-contract?))
                static-contract?)])
  case->/sc:
  arr/sc:)


(define (case->/sc arrs)
  (case-combinator arrs))

(define (arr/sc args rest range)
  (arr-combinator (arr-seq args rest range)))

(struct case-combinator combinator ()
  #:transparent
  #:property prop:combinator-name "case->/sc"
  #:methods gen:sc
    [(define (sc-map v f)
       (case-combinator (map (λ (a) (f a 'covariant)) (combinator-args v))))
     (define (sc-traverse v f)
       (for-each (λ (a) (f a 'covariant)) (combinator-args v))
       (void))
     (define (sc->contract v f)
       #`(case-> #,@(map f (combinator-args v))))
     (define (sc->constraints v f)
       (merge-restricts* 'chaperone (map f (combinator-args v))))])
(struct arr-combinator combinator ()
  #:transparent
  #:property prop:combinator-name "arr/sc"
  #:methods gen:sc
    [(define (sc-map v f)
       (arr-combinator (arr-seq-sc-map f (combinator-args v))))
     (define (sc-traverse v f)
       (arr-seq-sc-map f (combinator-args v))
       (void))
     (define (sc->contract v f)
       (match v
        [(arr-combinator (arr-seq args rest range))
         (with-syntax ([(arg-stx ...) (map f args)]
                       [(rest-stx ...) (if rest #`(#:rest #,(f rest)) #'())]
                       [range-stx (if range #`(values #,@(map f range)) #'any)])
           #'(arg-stx ... rest-stx ... . -> . range-stx))]))
     (define (sc->constraints v f)
       (merge-restricts* 'chaperone (map f (arr-seq->list (combinator-args v)))))])

(define-match-expander case->/sc:
  (syntax-parser
    [(_ args ...)
     #'(case-combinator (list args ...))]))

(define-match-expander arr/sc:
  (syntax-parser
    [(_ args rest range)
     #'(arr-combinator (arr-seq args rest range))]))


(define (arr-seq-sc-map f seq)
  (match seq
    [(arr-seq args rest range)
     (arr-seq
       (map (λ (a) (f a 'contravariant)) args)
       (and rest (f rest 'contravariant))
       (and range (map (λ (a) (f a 'covariant)) range)))]))

(define (arr-seq->list seq)
  (match seq
    [(arr-seq args rest range)
     (append
       args
       (if rest (list rest) empty)
       (or range empty))]))


(struct arr-seq (args rest range)
   #:transparent
   #:property prop:sequence
     (match-lambda
       [(arr-seq args rest range)
        (append
          args
          (if rest (list rest) empty)
          (if range range empty))]))
