;; This installer module just adds a bin/mred script
;;  under OS X
(module script-installer mzscheme
  (provide post-installer)
  (require (lib "launcher.ss" "launcher"))

  (define post-installer
    (lambda (path)
      (when (eq? 'macosx (system-type))	
	(let ([install
	       (lambda (variant)
		 (parameterize ([current-launcher-variant variant])
		   (make-mred-launcher null 
				       (mred-program-launcher-path "MrEd")
				       '((exe-name . "MrEd")))))])
	  (install 'script)
	  (when (memq 'script-3m (available-mred-variants))
	    (install 'script-3m)))))))
