#lang racket/base

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
;; provide everything from the racket/ implementation
;; except the arrow contracts
;;

(require racket/contract/private/base
         racket/contract/private/misc
         racket/contract/private/provide
         racket/contract/private/guts
         racket/contract/private/ds
         racket/contract/private/opt
         racket/contract/private/basic-opters)

(provide 
 opt/c define-opt/c ;(all-from "private/contract-opt.ss")
 (except-out (all-from-out racket/contract/private/ds)
             lazy-depth-to-look)
 
 (all-from-out racket/contract/private/base)
 (all-from-out racket/contract/private/provide)
 (except-out (all-from-out racket/contract/private/misc)
             check-between/c
             string-len/c
             check-unary-between/c)
 (rename-out [or/c union])
 (rename-out [string-len/c string/len])
 (except-out (all-from-out racket/contract/private/guts)
             check-flat-contract
             check-flat-named-contract))


;; copied here because not provided by racket/contract anymore
(define (flat-contract/predicate? pred)
  (or (flat-contract? pred)
      (and (procedure? pred)
           (procedure-arity-includes? pred 1))))
