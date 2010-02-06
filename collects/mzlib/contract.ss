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

(require scheme/contract/private/base
         scheme/contract/private/misc
         scheme/contract/private/provide
         scheme/contract/private/guts
         scheme/contract/private/ds
         scheme/contract/private/opt
         scheme/contract/private/basic-opters)

(provide 
 opt/c define-opt/c ;(all-from "private/contract-opt.ss")
 (except-out (all-from-out scheme/contract/private/ds)
             lazy-depth-to-look)
 
 (all-from-out scheme/contract/private/base)
 (all-from-out scheme/contract/private/provide)
 (except-out (all-from-out scheme/contract/private/misc)
             check-between/c
             string-len/c
             check-unary-between/c)
 (rename-out [or/c union])
 (rename-out [string-len/c string/len])
 (except-out (all-from-out scheme/contract/private/guts)
             check-flat-contract
             check-flat-named-contract))


;; copied here because not provided by scheme/contract anymore
(define (flat-contract/predicate? pred)
  (or (flat-contract? pred)
      (and (procedure? pred)
           (procedure-arity-includes? pred 1))))
