
(module mrpict-sig mzscheme
  (require mzlib/unit)

  (provide mrpict-extra^)
  (define-signature mrpict-extra^
    (dc-for-text-size
     convert-bounds-padding
     show-pict
     text caps-text current-expected-text-scale
     dc
     linewidth
     linestyle
     
     draw-pict
     make-pict-drawer)))
