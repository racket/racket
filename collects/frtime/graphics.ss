(module graphics mzscheme
  (require (lib "unit.ss")
	   (lib "mred-sig.ss" "mred")
	   (lib "mred.ss" "mred")
	   "graphics-sig.ss"
	   "graphics-unit.ss")
  (provide-signature-elements graphics:posn^ graphics:posn-less^)

  (define-values/invoke-unit/infer graphics@))
