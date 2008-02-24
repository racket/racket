(module graphics mzscheme
  (require mzlib/unit
	   (lib "mred-sig.ss" "mred")
	   (lib "mred-unit.ss" "mred")
	   "graphics-sig.ss"
	   "graphics-unit.ss")
  (provide-signature-elements graphics^ graphics:posn^)

  (define-compound-unit/infer graphics+mred@
    (import)
    (export graphics^ graphics:posn^)
    (link standard-mred@ graphics@))
  
  (define-values/invoke-unit/infer graphics+mred@))
