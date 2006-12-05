(module plt-installer mzscheme
  (require (lib "unit.ss")
           "plt-installer-sig.ss"
           "plt-installer-unit.ss"
           (lib "mred-unit.ss" "mred")
           (lib "mred-sig.ss" "mred"))
  
  (provide-signature-elements setup:plt-installer^)
  (define-compound-unit/infer plt-installer+mred@ 
    (import) (export setup:plt-installer^)
    (link standard-mred@ plt-installer@))
  (define-values/invoke-unit/infer plt-installer+mred@))

