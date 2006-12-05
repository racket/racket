(module mred-unit mzscheme
  (require (lib "unit.ss")
           "mred-sig.ss"
           "mred.ss")
  (provide standard-mred@)
  (define-unit-from-context standard-mred@ mred^))