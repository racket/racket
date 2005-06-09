(module oleg-utils mzscheme

  (provide any?
	   list-intersperse list-intersperse!
	   list-tail-diff
	   string-rindex
	   substring?
	   string->integer
	   string-split
	   make-char-quotator)
  
  (require (only (lib "13.ss" "srfi")
                 string-index-right string-contains string-null?))
  (require (lib "23.ss" "srfi"))
  (require "crementing.ss")

  (require (lib "include.ss"))
  (include "util.scm"))


	   
