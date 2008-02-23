
(module launcher mzscheme
  (require mzlib/unit)

  (require "launcher-sig.ss"
	   "launcher-unit.ss")
  
  (define-values/invoke-unit/infer launcher@)

  (provide-signature-elements launcher^))
