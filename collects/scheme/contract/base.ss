#lang scheme/base

;; A stripped down version of scheme/contract for use in
;; the PLT code base where appropriate.

(require "private/arrow.ss"
         "private/base.ss"
         "private/misc.ss"
         "private/provide.ss"
         "private/guts.ss"
         "private/ds.ss"
         "private/opt.ss")

(provide 
 opt/c define-opt/c ;(all-from-out "private/opt.ss")
 (except-out (all-from-out "private/ds.ss")
             lazy-depth-to-look)
 
 (except-out (all-from-out "private/arrow.ss")
             making-a-method
             procedure-accepts-and-more?
             check-procedure
             check-procedure/more)
 (except-out (all-from-out "private/misc.ss")
             check-between/c
             check-unary-between/c)
 (all-from-out "private/provide.ss")
 (all-from-out "private/base.ss"))

;; from private/guts.ss

(provide any
         and/c
         any/c
         none/c
         make-none/c 
         
         guilty-party
         exn:fail:contract2?
         exn:fail:contract2-srclocs
                
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
         
         coerce-contract/f
         coerce-contract
         coerce-contracts
         coerce-flat-contract
         coerce-flat-contracts
         
         build-compound-type-name
         raise-contract-error
         
         proj-prop proj-pred? proj-get
         name-prop name-pred? name-get
         stronger-prop stronger-pred? stronger-get
         flat-prop flat-pred? flat-get
         first-order-prop first-order-get)
