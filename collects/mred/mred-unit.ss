(module mred-unit mzscheme
  (require mzlib/unit
           "mred-sig.ss"
           "mred.ss")
  (provide standard-mred@)
  (define-unit-from-context standard-mred@ mred^))