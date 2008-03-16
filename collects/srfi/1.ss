;; module loader for SRFI-1
(module |1| mzscheme
  
  (require srfi/1/list)
  
  (provide (all-from-except srfi/1/list
                            s:map s:for-each
                            s:member
                            s:assoc)
	   (rename s:map map)
	   (rename s:for-each for-each)
	   (rename s:member member)
	   (rename s:assoc assoc)))
			    
