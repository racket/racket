(module mred-unit mzscheme
  (require mzlib/unit
           "mred-sig.rkt"
           "mred.rkt")
  (provide standard-mred@)
  (define-unit-from-context standard-mred@ mred^))
