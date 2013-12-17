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

(define (none/sc-reduce sc)
  (match sc
    [(listof/sc: (none/sc:)) empty-list/sc]
    [(list/sc: sc1 ... (none/sc:) sc2 ...) none/sc]
    [(vectorof/sc: (none/sc:)) empty-vector/sc]
    [(vector/sc: sc1 ... (none/sc:) sc2 ...) none/sc]
    [(set/sc: (none/sc:)) empty-set/sc]
    [(box/sc: (none/sc:)) none/sc]
    [(syntax/sc: (none/sc:)) none/sc]
    [(promise/sc: (none/sc:)) none/sc]
    [(hash/sc: (none/sc:) value/sc) empty-hash/sc]
    [(hash/sc: key/sc (none/sc:)) empty-hash/sc]
    [else sc]))


(define (any/sc-reduce sc)
  (match sc
    [(listof/sc: (any/sc:)) list?/sc]
    [(list/sc: (and scs (any/sc:)) ...) (list-length/sc (length scs))]
    [(vectorof/sc: (any/sc:)) vector?/sc]
    [(vector/sc: (and scs (any/sc:)) ...) (vector-length/sc (length scs))]
    [(set/sc: (any/sc:)) set?/sc]
    [(box/sc: (any/sc:)) box?/sc]
    [(syntax/sc: (any/sc:)) syntax?/sc]
    [(promise/sc: (any/sc:)) promise?/sc]
    [(hash/sc: (any/sc:) (any/sc:)) hash?/sc]
    [else sc]))


(define (covariant-any/sc-reduce sc)
  (match sc
    [(->/sc: mand-args opt-args mand-kw-args opt-kw-args rest-arg (list (any/sc:) ...))
     (function/sc mand-args opt-args mand-kw-args opt-kw-args rest-arg #f)]
    [(arr/sc: args rest (list (any/sc:) ...))
     (arr/sc args rest #f)]
    [else sc]))

(define (covariant-none/sc-reduce sc)
  (match sc
    [(none/sc:) any/sc]
    [else sc]))

(define (or/sc-reduce sc)
  (match sc
    [(or/sc:) none/sc]
    [(or/sc: sc) sc]
    [(or/sc: sc1 ... (any/sc:) sc2 ...)
     any/sc]
    [(or/sc: sc1 ... (none/sc:) sc2 ...)
     (or/sc-reduce (apply or/sc (append sc1 sc2)))]
    [else sc]))

(define (and/sc-reduce sc)
  (match sc
    [(and/sc:) any/sc]
    [(and/sc: sc) sc]
    [(and/sc: sc1 ... (none/sc:) sc2 ...)
     none/sc]
    [(and/sc: sc1 ... (any/sc:) sc2 ...)
     (and/sc-reduce (apply and/sc (append sc1 sc2)))]
    [else sc]))



(define (covariant-flat-reduce sc)
  (match sc
    [(? flat/sc?) any/sc]
    [(none/sc:) any/sc]
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

    ((compose
       (maybe/co covariant-flat-reduce)
       (maybe/co covariant-any/sc-reduce)
       (maybe/co covariant-none/sc-reduce)
       and/sc-reduce
       or/sc-reduce
       none/sc-reduce
       any/sc-reduce)
     sc))

  (define (full-pass sc)
    (define ((recur current-variance) sc variance)
      (define new-variance (combine-variance current-variance variance))
      (single-step (sc-map sc (recur new-variance)) new-variance))
    ((recur variance) sc 'covariant))
  (let loop ((sc sc))
    (define new-sc (full-pass sc))
    (if (equal? sc new-sc)
        new-sc
        (loop new-sc))))
