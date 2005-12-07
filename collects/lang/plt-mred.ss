(module plt-mred mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss"))

  (define argv (current-command-line-arguments))
  (define program (find-system-path 'exec-file))
  
  (provide argv 
           program
           (all-from mzscheme)
           (all-from (lib "class.ss"))
	   (all-from (lib "mred.ss" "mred"))))
