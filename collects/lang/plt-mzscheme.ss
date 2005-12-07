(module plt-mzscheme mzscheme
  (define argv (current-command-line-arguments))
  (define program (find-system-path 'exec-file))
  (provide argv 
           program
           (all-from mzscheme)))
