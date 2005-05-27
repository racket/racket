;; module loader for SRFI-1
(module |1| mzscheme
  
  (require (lib "list.ss" "srfi" "1"))
  
  (provide (all-from-except (lib "list.ss" "srfi" "1")
		       s:append! s:reverse!
		       s:map s:for-each
		       s:member
		       s:assoc)
	   (rename s:append! append!)
	   (rename s:reverse! reverse!)
	   (rename s:map map)
	   (rename s:for-each for-each)
	   (rename s:member member)
	   (rename s:assoc assoc)))
			    
