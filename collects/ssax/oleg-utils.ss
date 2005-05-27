(module oleg-utils mzscheme

  (provide any?
	   list-intersperse list-intersperse!
	   list-tail-diff
	   string-rindex
	   substring?
	   string->integer
	   string-split
	   make-char-quotator)
  
  (require (rename (lib "13.ss" "srfi") string-index-right string-index-right)
	   (rename (lib "13.ss" "srfi") string-contains string-contains)
	   (rename (lib "13.ss" "srfi") string-null? string-null?))
  (require (lib "23.ss" "srfi"))
  (require "crementing.ss")

  (require (lib "include.ss"))
  (include "util.scm"))


	   