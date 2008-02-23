(module plt-mred mzscheme
  (require mred
           mzlib/class)

  (define argv (current-command-line-arguments))
  (define program (find-system-path 'exec-file))
  
  (provide argv 
           program
           (all-from mzscheme)
           (all-from mzlib/class)
	   (all-from mred)))
