#lang racket/base
(provide struct-type/c)
(require racket/contract/combinator)

;; struct-type/c generates contracts which protect structure type
;; descriptors. These descriptors can be used reflectively to create
;; constructors, accessors, mutators, predicates, and other
;; structure-related functions.

;; Currently, this is a very simple implentation which always rejects
;; all reflective access. A better implementation would check that the
;; procedures created by reflective access to the structure obey
;; appropriate invariants.

(define (val-first-projection b)
  (define (fail neg-party v)
    (raise-blame-error 
     (blame-swap b) #:missing-party neg-party
     v 
     "Attempted to use a struct type reflectively in untyped code: ~v" v))
  (λ (v)
    (λ (neg-party)
      (chaperone-struct-type 
       v
       ;; the below interposition functions could be improved to fail later, 
       ;; when the functions they produce are actually used.

       ;; interposition for `struct-type-info`
       (λ _ (fail neg-party v))
       ;; interposition for `struct-type-make-constructor`
       (λ _ (fail neg-party v))
       ;; guard for interposition on subtypes
       (λ _ (fail neg-party v))))))

(define (struct-type/c sty) ;; currently ignores sty
  (make-chaperone-contract
   #:name "struct-type/c"
   #:first-order struct-type?
   #:projection (λ (blame) (λ (val) (((val-first-projection blame) val) #f)))
   #:val-first-projection val-first-projection))
