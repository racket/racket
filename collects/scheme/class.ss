(module class mzscheme

  (require "private/contract-object.ss")
  (provide (all-from "private/contract-object.ss"))
  
  ;; All of the implementation is actually in private/class-internal.ss,
  ;;  which provides extra (private) functionality to contract.ss.
  (require "private/class-internal.ss")
  
  (provide-public-names)
  (provide generic?))
