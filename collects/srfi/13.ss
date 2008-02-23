;; module loader for SRFI-13
(module |13| mzscheme
  (require srfi/13/string)
  (provide (all-from-except srfi/13/string
			    s:string-upcase s:string-downcase s:string-titlecase)
	   (rename s:string-upcase string-upcase)
	   (rename s:string-downcase string-downcase)
	   (rename s:string-titlecase string-titlecase)))


