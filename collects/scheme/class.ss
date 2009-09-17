(module class mzscheme

  (require "contract/private/object.ss")
  (provide (all-from "contract/private/object.ss"))
  
  ;; All of the implementation is actually in private/class-internal.ss,
  ;;  which provides extra (private) functionality to contract.ss.
  (require "private/class-internal.ss")
  
  (provide-public-names)
  (provide generic?))
