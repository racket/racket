(module graphics-unit mzscheme
  (require (lib "unit.ss")
	   (lib "mred-sig.ss" "mred")
	   "graphics-sig.ss"
	   "graphics-posn-less-unit.ss")
  (provide graphics@)

  (define-unit posn@ (import) (export graphics:posn^)
    (define-struct posn (x y)))

  (define-compound-unit/infer graphics@
    (import mred^)
    (export graphics:posn^ graphics:posn-less^) 
    (link posn@ graphics-posn-less@)))

