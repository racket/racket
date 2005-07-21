;; module loader for SRFI-13
(module |13| mzscheme
  (require (lib "string.ss" "srfi" "13"))
  (provide (all-from-except (lib "string.ss" "srfi" "13")
			    s:string-upcase s:string-downcase s:string-titlecase)
	   (rename s:string-upcase string-upcase)
	   (rename s:string-downcase string-downcase)
	   (rename s:string-titlecase string-titlecase)))


