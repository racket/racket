#lang scheme/base

;; A stripped down version of scheme/contract for use in
;; the PLT code base where appropriate.

(require scheme/private/contract-arrow
         scheme/private/contract-base
         scheme/private/contract-misc
         scheme/private/contract-provide
         scheme/private/contract-guts
         scheme/private/contract-ds
         scheme/private/contract-opt)

(provide 
 opt/c define-opt/c ;(all-from-out "private/contract-opt.ss")
 (except-out (all-from-out scheme/private/contract-ds)
             lazy-depth-to-look)
 
 (except-out (all-from-out scheme/private/contract-arrow) 
             making-a-method
             procedure-accepts-and-more?
             check-procedure
             check-procedure/more)
 (except-out (all-from-out scheme/private/contract-misc)
             check-between/c
             check-unary-between/c)
 (all-from-out scheme/private/contract-provide)
 (all-from-out scheme/private/contract-base))

;; from contract-guts.ss

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