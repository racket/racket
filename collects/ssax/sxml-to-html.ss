(module sxml-to-html mzscheme

  (provide SXML->HTML
	   enattr
	   entag
	   string->goodHTML)
  
  (require "coutputs.ss")
  (require "assertions.ss")
  (require "oleg-utils.ss")
  (require "sxml-tree-trans.ss")

  (require (lib "include.ss"))
  (include "SXML-to-HTML.scm"))
