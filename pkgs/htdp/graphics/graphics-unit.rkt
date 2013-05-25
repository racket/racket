(module graphics-unit mzscheme
  (require mzlib/unit
           mred/mred-sig
           "graphics-sig.rkt"
           "graphics-posn-less-unit.rkt")
  (provide graphics@)

  (define-unit p@
    (import)
    (export graphics:posn^)
    (define-struct posn (x y)))
  
  (define-compound-unit/infer graphics@
    (import mred^)
    (export graphics:posn^ graphics^)
    (link p@ graphics-posn-less@)))
