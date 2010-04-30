#lang scheme/base

(require unstable/debug "../utils/utils.rkt" (rep type-rep) (only-in (types abbrev utils) tc-results?) scheme/contract)

(define table (make-hasheq))

(define (reset-type-table) (set! table (make-hasheq)))

(define (add-typeof-expr e t) 
  (when (optimize?)
    (hash-set! table e t)))

(define (type-of e) (hash-ref table e))

(p/c [add-typeof-expr (syntax? tc-results? . -> . any/c)]
     [type-of (syntax? . -> . tc-results?)]
     [reset-type-table (-> any/c)])