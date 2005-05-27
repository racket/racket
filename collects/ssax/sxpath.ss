(module sxpath mzscheme

  (provide nodeset?
	   node-typeof?
	   map-union
	   sxpath)

  (require (lib "pretty.ss"))
  (require (lib "23.ss" "srfi")) ; ERROR
  (require "crementing.ss")
  (require "assertions.ss")
  (require "coutputs.ss")

  (require (lib "include.ss"))
  (include "SXPath-old.scm"))


  