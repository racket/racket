
(module mrpict mzscheme
  (require (lib "unitsig.ss"))

  (require (lib "mred-sig.ss" "mred")
	   (lib "mred.ss" "mred"))

  (require "mrpict-sig.ss"
	   "mrpict-unit.ss")

  (define-values/invoke-unit/sig mrpict^
    mrpict@
    #f
    mred^)

  (provide-signature-elements mrpict^))
