(module sxml-to-html-ext mzscheme

  (provide make-header
	   make-navbar
	   make-footer
	   universal-conversion-rules
	   universal-protected-rules
	   alist-conv-rules)

  (require (lib "23.ss" "srfi"))
  (require "oleg-utils.ss")
  (require "coutputs.ss")
  (require "assertions.ss")
  (require "crementing.ss")
  (require "lookup-def.ss")
  (require "sxml-to-html.ss")
  (require "sxml-tree-trans.ss")

  (define OS:file-length file-size)

  (require (lib "include.ss"))
  (include "SXML-to-HTML-ext.scm"))


  