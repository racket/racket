
(module mrpict-sig mzscheme
  (require (lib "unitsig.ss"))

  (provide mrpict-extra^)
  (define-signature mrpict-extra^
    (dc-for-text-size
     show-pict
     text caps-text current-expected-text-scale
     dc
     linewidth
     
     draw-pict
     make-pict-drawer)))
