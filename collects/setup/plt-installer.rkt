(module plt-installer mzscheme
  (require mzlib/unit
           "plt-installer-sig.rkt"
           "plt-installer-unit.rkt"
           mred/mred-unit
           mred/mred-sig)

  (provide-signature-elements setup:plt-installer^)
  (define-compound-unit/infer plt-installer+mred@ 
    (import) (export setup:plt-installer^)
    (link standard-mred@ plt-installer@))
  (define-values/invoke-unit/infer plt-installer+mred@))
