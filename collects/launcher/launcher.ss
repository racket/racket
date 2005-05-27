
(module launcher mzscheme
  (require (lib "unitsig.ss"))

  (require (lib "compile-sig.ss" "dynext")
	   (lib "compile.ss" "dynext")
	   (lib "link-sig.ss" "dynext")
	   (lib "link.ss" "dynext"))
  
  (require "launcher-sig.ss"
	   "launcher-unit.ss")
  
  (define-values/invoke-unit/sig launcher^ 
    launcher@
    #f
    dynext:compile^
    dynext:link^)

  (provide-signature-elements launcher^))
