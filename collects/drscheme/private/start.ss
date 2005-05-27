
(module start mzscheme
  (require "link.ss"
	   "drsig.ss"
	   (lib "class.ss")
	   (lib "unitsig.ss")
           (lib "framework.ss" "framework")
	   (lib "splash.ss" "framework"))
  
  (shutdown-splash)
  (define-values/invoke-unit/sig drscheme^ drscheme@)
  (close-splash))
