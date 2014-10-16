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
 (except-out (all-from-out "private/arrow.rkt")
             making-a-method
             procedure-accepts-and-more?
             check-procedure
             check-procedure/more

             contracted-function?
             contracted-function-proc
             contracted-function-ctc
             make-contracted-function 
             contract-key
             
             ;; these two are provided for-syntax
             ;check-tail-contract
             ;make-this-parameters
             
             -> ->*)
 (rename-out [->2 ->] [->*2 ->*])
 (all-from-out "private/arr-i.rkt"
               "private/box.rkt"
               "private/hash.rkt"
               "private/vector.rkt"
               "private/struct-dc.rkt"
               "private/struct-prop.rkt")
 (except-out (all-from-out "private/base.rkt")
             current-contract-region)
 (except-out (all-from-out "private/misc.rkt")
             check-between/c
             check-unary-between/c
             random-any/c)
 symbols or/c one-of/c
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
 case->)
