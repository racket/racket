(module internal-hp mzscheme
  (provide internal-host internal-port addon-host)
  
  (define internal-host "helpdesk.plt-scheme.org") ;; should not exist.
  (define addon-host "addon-helpdesk.plt-scheme.org") ;; ditto
  (define internal-port 8000))
