
(module collection mzscheme
  (require (lib "unitsig.ss"))

  (require (lib "file-sig.ss" "dynext")
	  (lib "file.ss" "dynext")
	  (lib "sig.ss" "compiler")
	  (lib "compiler.ss" "compiler")
	  (lib "option.ss" "compiler"))

  (require "make-sig.ss"
	  "make.ss"
	  "collection-sig.ss"
	  "collection-unit.ss")
  
  (define-values/invoke-unit/sig make:collection^ 
    make:collection@
    #f
    make^
    dynext:file^
    compiler:option^
    compiler^)

  (provide-signature-elements make:collection^))
