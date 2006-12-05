
(module collection mzscheme
  (require (lib "unit.ss"))

  (require (lib "file-sig.ss" "dynext")
	  (lib "file.ss" "dynext")
	  (lib "sig.ss" "compiler")
	  (lib "compiler.ss" "compiler")
	  (lib "option.ss" "compiler"))

  (require "make-sig.ss"
	  "make.ss"
	  "collection-sig.ss"
	  "collection-unit.ss")
  
  (define-values/invoke-unit/infer make:collection@)

  (provide-signature-elements make:collection^))
