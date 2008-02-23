(module graphics-unit mzscheme
  (require mzlib/unit
	   (lib "mred-sig.ss" "mred")
	   "graphics-sig.ss"
	   "graphics-posn-less-unit.ss")
  (provide graphics@)

  (define-unit p@
    (import)
    (export graphics:posn^)
    (define-struct posn (x y)))
  
  (define-compound-unit/infer graphics@
    (import mred^)
    (export graphics:posn^ graphics^)
    (link p@ graphics-posn-less@)))
