#lang racket/base

;; Functionalityt otoptimize a static contract to provide faster checking.
;; Also supports droping one side's obligations.

(require "combinators.rkt"
         "structures.rkt"
         racket/set
         racket/list
         (except-in racket/contract recursive-contract)
         racket/match)



(provide 
  (contract-out
    [optimize (static-contract? (or/c 'covariant 'contravariant 'invariant ) . -> . static-contract?)]))


(define (any/sc-reduce sc)
  (match sc
    [(listof/sc: (any/sc:)) list?/sc]
    [(vectorof/sc: (any/sc:)) vector?/sc]
    [(set/sc: (any/sc:)) set?/sc]
    [(box/sc: (any/sc:)) box?/sc]
    [(syntax/sc: (any/sc:)) syntax?/sc]
    [(promise/sc: (any/sc:)) promise?/sc]
    [(hash/sc: (any/sc:) (any/sc:)) hash?/sc]
    [(any/sc:) sc]
    [else sc]))


(define (covariant-any/sc-reduce sc)
  (match sc
    [(->/sc: mand-args opt-args mand-kw-args opt-kw-args rest-arg (list (any/sc:) ...))
     (function/sc mand-args opt-args mand-kw-args opt-kw-args rest-arg #f)]
    [(arr/sc: args rest (list (any/sc:) ...))
     (arr/sc args rest #f)]
    [else sc]))

(define (flat-reduce sc)
  (match sc
    [(? flat/sc?)
     any/sc]
    [sc sc]))

(define (invert-variance v)
  (case v
    [(covariant) 'contravariant]
    [(contravariant) 'covariant]
    [(invariant) 'invariant]))

(define (combine-variance var1 var2)
  (case var1
    [(covariant) var2]
    [(contravariant) (invert-variance var2)]
    [(invariant) 'invariant]))

;; If the variance is 'covariant, drops the parts ensuring that server behaves
;; If the variance is 'contrvariant, drops the parts ensuring that client behaves
;; If the variance is 'invariant, only optimizes the contract.
(define (optimize sc variance)
  (define (single-step sc variance)
    (define ((maybe/co reduce) sc)
      (case variance
        [(covariant) (reduce sc)]
        [(contravariant invariant) sc]
        [else (error 'maybe/co "Bad variance ~a" variance)]))

    ((maybe/co flat-reduce) ((maybe/co covariant-any/sc-reduce) (any/sc-reduce sc))))

  (define ((recur current-variance) sc variance)
    (define new-variance (combine-variance current-variance variance))
    (single-step (sc-map sc (recur new-variance)) new-variance))
  ((recur variance) sc 'covariant))


