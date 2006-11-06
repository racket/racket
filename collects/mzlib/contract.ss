(module contract mzscheme
  (require "private/contract.ss"
           "private/contract-arrow.ss"
           "private/contract-guts.ss"
           "private/contract-ds.ss"
           "private/contract-opt-guts.ss"
           "private/contract-opt.ss"
           "private/contract-basic-opters.ss")
  
  (provide 
   ;; opt is not ready yet
   #;(all-from "private/contract-opt.ss")
   #;(all-from-except "private/contract-opt-guts.ss"
                    make-opt-contract
                    orig-ctc-prop
                    orig-ctc-pred?
                    orig-ctc-get)
   (all-from "private/contract-ds.ss")
   (all-from-except "private/contract-arrow.ss"
                    check-procedure)
   (all-from-except "private/contract-guts.ss"
                    build-compound-type-name
                    first-order-prop
                    first-order-get
                    check-flat-contract
                    check-flat-named-contract)
   (all-from-except "private/contract.ss"
                    check-between/c
                    check-unary-between/c)))