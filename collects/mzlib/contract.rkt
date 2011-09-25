#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  provide arrow contracts from our local copy (mostly)
;;

(require "private/contract-arrow.rkt")
(provide (all-from-out "private/contract-arrow.rkt"))
(require (only-in racket/contract/base unconstrained-domain->))
(provide unconstrained-domain->)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  provide contracts for objects
;;
(require "private/contract-object.rkt")
(provide (all-from-out "private/contract-object.rkt"))

(require (only-in racket/class
                  is-a?/c
                  implementation?/c
                  subclass?/c
                  mixin-contract
                  make-mixin-contract))
(provide is-a?/c
         implementation?/c
         subclass?/c
         mixin-contract
         make-mixin-contract)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; old-style define/contract
;;

(require "private/contract-define.rkt")
(provide (all-from-out "private/contract-define.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; old-style flat mutable contracts
;;
(require "private/contract-mutable.rkt")
(provide (all-from-out "private/contract-mutable.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; old-style flat struct contracts
;;
(require "private/contract-struct.rkt")
(provide (all-from-out "private/contract-struct.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; provide everything from the racket/ implementation
;; except the arrow contracts
;;

(require racket/contract/private/base
         racket/contract/private/misc
         racket/contract/private/provide
         racket/contract/private/guts
         racket/contract/private/prop
         racket/contract/private/blame
         racket/contract/private/ds
         racket/contract/private/opt
         racket/contract/private/basic-opters
         racket/contract/combinator)


(define (build-flat-contract name pred) (make-predicate-contract name pred))

(provide 
 opt/c define-opt/c ;(all-from "private/contract-opt.rkt")
 (except-out (all-from-out racket/contract/private/ds)
             contract-struct)
 
 (all-from-out racket/contract/private/base
               racket/contract/private/provide)
 (except-out (all-from-out racket/contract/private/misc)
             check-between/c
             string-len/c
             check-unary-between/c)
 (rename-out [or/c union])
 (rename-out [string-len/c string/len])
 (except-out (all-from-out racket/contract/private/guts)
             check-flat-contract
             check-flat-named-contract
             make-predicate-contract)
 (except-out (all-from-out racket/contract/private/blame)
             make-blame)
 (except-out (all-from-out racket/contract/private/prop)
             chaperone-contract-struct?
             contract-struct-first-order
             contract-struct-name
             contract-struct-projection
             contract-struct-stronger?
             contract-struct?
             flat-contract-struct?)
 (all-from-out racket/contract/combinator)
 build-flat-contract)
