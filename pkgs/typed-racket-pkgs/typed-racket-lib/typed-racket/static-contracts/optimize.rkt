#lang racket/base

;; Functionality to optimize a static contract to provide faster checking.
;; Also supports droping checks on either side.

(require "combinators.rkt"
         "structures.rkt"
         racket/set
         racket/list
         (except-in racket/contract recursive-contract)
         racket/match)



(provide
  (contract-out
    [optimize ((static-contract?) (#:trusted-positive boolean? #:trusted-negative boolean?)
               . ->* . static-contract?)]))

;; Reduce a static contract to a smaller simpler one that protects in the same way
(define (reduce sc)
  (match sc
    ;; none/sc cases
    [(listof/sc: (none/sc:)) empty-list/sc]
    [(list/sc: sc1 ... (none/sc:) sc2 ...) none/sc]
    [(set/sc: (none/sc:)) empty-set/sc]
    [(syntax/sc: (none/sc:)) none/sc]
    ;; The following are unsound because chaperones allow operations on these data structures to 
    ;; can call continuations and thus be useful even if they cannot return values.
    ;[(vectorof/sc: (none/sc:)) empty-vector/sc]
    ;[(vector/sc: sc1 ... (none/sc:) sc2 ...) none/sc]
    ;[(box/sc: (none/sc:)) none/sc]
    ;[(promise/sc: (none/sc:)) none/sc]
    ;[(hash/sc: (none/sc:) value/sc) empty-hash/sc]
    ;[(hash/sc: key/sc (none/sc:)) empty-hash/sc]

    ;; any/sc cases
    [(listof/sc: (any/sc:)) list?/sc]
    [(list/sc: (and scs (any/sc:)) ...) (list-length/sc (length scs))]
    [(vectorof/sc: (any/sc:)) vector?/sc]
    [(vector/sc: (and scs (any/sc:)) ...) (vector-length/sc (length scs))]
    [(set/sc: (any/sc:)) set?/sc]
    [(box/sc: (any/sc:)) box?/sc]
    [(syntax/sc: (any/sc:)) syntax?/sc]
    [(promise/sc: (any/sc:)) promise?/sc]
    [(hash/sc: (any/sc:) (any/sc:)) hash?/sc]

    ;; or/sc cases
    [(or/sc: scs ...)
     (match scs
      [(list) none/sc]
      [(list sc) sc]
      [(? (λ (l) (member any/sc l))) any/sc]
      [(? (λ (l) (member none/sc l)))
       (apply or/sc (remove* (list none/sc) scs))]
      [else sc])]

    ;; and/sc cases
    [(and/sc: scs ...)
     (match scs
      [(list) any/sc]
      [(list sc) sc]
      [(? (λ (l) (member none/sc l))) none/sc]
      [(? (λ (l) (member any/sc l)))
       (apply and/sc (remove* (list any/sc) scs))]
      [else sc])]



    [else sc]))


;; Reduce a static contract assuming that we trusted the current positive side
(define (trusted-side-reduce sc)
  (match sc
    [(->/sc: mand-args opt-args mand-kw-args opt-kw-args rest-arg (list (any/sc:) ...))
     (function/sc mand-args opt-args mand-kw-args opt-kw-args rest-arg #f)]
    [(arr/sc: args rest (list (any/sc:) ...))
     (arr/sc args rest #f)]
    [(none/sc:) any/sc]
    [(? flat/sc?) any/sc]
    [else sc]))



(define (invert-side v)
  (case v
    [(positive) 'negative]
    [(negative) 'positive]
    [(both) 'both]))

(define (combine-variance side var)
  (case var
    [(covariant) side]
    [(contravariant) (invert-side side)]
    [(invariant) 'both]))

;; If we trust a specific side then we drop all contracts protecting that side.
(define (optimize sc #:trusted-positive [trusted-positive #f] #:trusted-negative [trusted-negative #f])
  ;; single-step: reduce and trusted-side-reduce if appropriate
  (define (single-step sc side)
    (define trusted
      (case side
        [(positive) trusted-positive]
        [(negative) trusted-negative]
        [(both) (and trusted-positive trusted-negative)]))

    (reduce
      (if trusted
          (trusted-side-reduce sc)
          sc)))

  ;; full-pass: single-step at every static contract subpart
  (define (full-pass sc)
    (define ((recur side) sc variance)
      (define new-side (combine-variance side variance))
      (single-step (sc-map sc (recur new-side)) new-side))
    ((recur 'positive) sc 'covariant))

  ;; Do full passes until we reach a fix point
  (let loop ((sc sc))
    (define new-sc (full-pass sc))
    (if (equal? sc new-sc)
        new-sc
        (loop new-sc))))
