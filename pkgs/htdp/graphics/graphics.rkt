(module graphics mzscheme
  (require mzlib/unit
           mred/mred-sig
           mred/mred-unit
           "graphics-sig.rkt"
           "graphics-unit.rkt")
  (provide-signature-elements graphics^ graphics:posn^)

  (define-values/invoke-unit/infer
    (export graphics^ graphics:posn^)
    (link standard-mred@ graphics@)))
