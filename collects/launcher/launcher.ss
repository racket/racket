
(module launcher mzscheme
  (require (lib "unit.ss"))

  (require "launcher-sig.ss"
	   "launcher-unit.ss")
  
  (define-values/invoke-unit/infer launcher@)

  (provide-signature-elements launcher^))
