
(module xml mzscheme
  (require (lib "unitsig.ss"))

  (require "xml-sig.ss"
	   "xml-unit.ss")
  
  (define-values/invoke-unit/sig xml^ xml@)

  (provide-signature-elements xml^))
