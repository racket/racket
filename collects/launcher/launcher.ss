
(module launcher mzscheme
  (require (lib "unit.ss"))

  (require (lib "compile-sig.ss" "dynext")
	   (lib "compile.ss" "dynext")
	   (lib "link-sig.ss" "dynext")
	   (lib "link.ss" "dynext"))
  
  (require "launcher-sig.ss"
	   "launcher-unit.ss")
  
  (define-values/invoke-unit/infer launcher@)

  (provide-signature-elements launcher^))
