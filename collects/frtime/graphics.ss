(module graphics mzscheme
  (require (lib "unitsig.ss")
	   (lib "mred-sig.ss" "mred")
	   (lib "mred.ss" "mred")
	   "graphics-sig.ss"
	   "graphics-unit.ss")
  (provide-signature-elements graphics^)

  (define-values/invoke-unit/sig graphics^ graphics@ #f mred^))