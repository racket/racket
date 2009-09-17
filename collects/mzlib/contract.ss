#lang scheme/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  provide arrow contracts from our local copy
;;

(require "private/contract-arrow.ss")
(provide (all-from-out "private/contract-arrow.ss"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  provide contracts for objects
;;
(require "private/contract-object.ss")
(provide (all-from-out "private/contract-object.ss"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; old-style define/contract
;;

(require "private/contract-define.ss")
(provide (all-from-out "private/contract-define.ss"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; provide everything from the scheme/ implementation
;; except the arrow contracts
;;

(require scheme/private/contract-base
         scheme/private/contract-misc
         scheme/private/contract-provide
         scheme/private/contract-guts
         scheme/private/contract-ds
         scheme/private/contract-opt
         scheme/private/contract-basic-opters)

(provide 
 opt/c define-opt/c ;(all-from "private/contract-opt.ss")
 (except-out (all-from-out scheme/private/contract-ds)
             lazy-depth-to-look)
 
 (all-from-out scheme/private/contract-base)
 (all-from-out scheme/private/contract-provide)
 (except-out (all-from-out scheme/private/contract-misc)
             check-between/c
             string-len/c
             check-unary-between/c)
 (rename-out [string-len/c string/len]))

;; from contract-guts.ss

(provide any
         and/c
         any/c
         none/c
         make-none/c 
         
         guilty-party
         contract-violation->string
         
         contract?
         contract-name
         contract-proc
         
         flat-contract?
         flat-contract
         flat-contract-predicate
         flat-named-contract
         
         contract-first-order-passes?
         
         ;; below need docs
         
         make-proj-contract
         
         contract-stronger?
         
         coerce-contract 
         flat-contract/predicate?
         
         build-compound-type-name
         raise-contract-error
         
         proj-prop proj-pred? proj-get
         name-prop name-pred? name-get
         stronger-prop stronger-pred? stronger-get
         flat-prop flat-pred? flat-get
         first-order-prop first-order-get
         (rename-out [or/c union]))


;; copied here because not provided by scheme/contract anymore
(define (flat-contract/predicate? pred)
  (or (flat-contract? pred)
      (and (procedure? pred)
           (procedure-arity-includes? pred 1))))
