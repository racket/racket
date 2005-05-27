(module plt-installer mzscheme
  (require (lib "unitsig.ss")
           "plt-installer-sig.ss"
           "plt-installer-unit.ss"
           (lib "mred.ss" "mred")
           (lib "mred-sig.ss" "mred"))
  
  (provide-signature-elements setup:plt-installer^)
  (define-values/invoke-unit/sig setup:plt-installer^ plt-installer@ #f mred^))

