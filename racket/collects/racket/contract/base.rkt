#lang racket/base

(require "private/arrow.rkt"
         "private/case-arrow.rkt"
         "private/arr-i.rkt"
         "private/base.rkt"
         "private/box.rkt"
         "private/hash.rkt"
         "private/vector.rkt"
         "private/struct-dc.rkt"
         "private/struct-prop.rkt"
         "private/misc.rkt"
         "private/provide.rkt"
         "private/guts.rkt"
         "private/opters.rkt"       ;; required for effect to install the opters
         "private/basic-opters.rkt" ;; required for effect to install the opters
         "private/opt.rkt"
         "private/out.rkt"
         "private/arrow-val-first.rkt"
         "private/orc.rkt")

(provide
 base->?
 ->d
 base->-rngs/c
 base->-doms/c
 unconstrained-domain->
 the-unsupplied-arg
 unsupplied-arg?
 method-contract?
 matches-arity-exactly?
 keywords-match
 bad-number-of-results
 (for-syntax check-tail-contract
             make-this-parameters
             parse-leftover->*)
 tail-marks-match?
 values/drop
 arity-checking-wrapper
 unspecified-dom
 blame-add-range-context
 blame-add-nth-arg-context
 
 (rename-out [->2 ->] [->*2 ->*])
 dynamic->*
 predicate/c

 ->i
 box-immutable/c 
 box/c
 hash/c
 hash/dc
 vectorof
 vector/c
 vector-immutable/c
 vector-immutableof
 struct/dc
 struct/c
 struct-type-property/c
 
 contract
 recursive-contract
 invariant-assertion
 
 flat-murec-contract
 and/c
 not/c
 =/c >=/c <=/c </c >/c between/c
 integer-in
 char-in
 real-in
 natural-number/c
 string-len/c
 false/c
 printable/c
 listof list*of non-empty-listof cons/c list/c cons/dc
 promise/c
 syntax/c
 
 parameter/c
 procedure-arity-includes/c
 
 any/c
 any
 none/c
 make-none/c
 
 prompt-tag/c
 continuation-mark-key/c
 
 channel/c
 evt/c
 
 flat-contract
 flat-contract-predicate
 flat-named-contract
 
 blame-add-car-context
 blame-add-cdr-context
 raise-not-cons-blame-error
 
 rename-contract
 if/c
 
 symbols or/c first-or/c one-of/c
 flat-rec-contract
 provide/contract
 ;(for-syntax make-provide/contract-transformer) ;; not documented!
 contract-out
 recontract-out
 define-module-boundary-contract
 
 ;; from private/opt.rkt:
 opt/c define-opt/c

 ;; from private/guts.rkt
 has-contract?
 value-contract
 has-blame?
 value-blame
 contract-continuation-mark-key
 list-contract?
 
 ;; from private/case-arrow.rkt
 case->

 ;; from here (needs `->`, so can't be deeper)
 failure-result/c

 contract?
 chaperone-contract?
 impersonator-contract?
 flat-contract?
 
 contract-late-neg-projection
 contract-name
 contract-projection
 contract-val-first-projection
 get/build-late-neg-projection
 get/build-val-first-projection
 
 ;; not documented.... (ie unintentional export)
 n->th)


;; failure-result/c : contract
;; Describes the optional failure argument passed to hash-ref, for example.
;; If the argument is a procedure, it must be a thunk, and it is applied. Otherwise
;; the argument is simply the value to return.
(define failure-result/c
  (if/c procedure? (-> any) any/c))
