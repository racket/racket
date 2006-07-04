(module check mzscheme
  (require (lib "include.ss")
	   (lib "23.ss" "srfi")
	   (lib "42.ss" "srfi")
	   (lib "pretty.ss"))

  (include "check-reference.scm")
  
  (provide check
	   check-ec
	   check-report
	   check-set-mode!
	   check-reset!
	   check-passed?))
