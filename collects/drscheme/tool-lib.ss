
#|

This first time this is loaded, it loads all of drscheme and invokes
the main unit, starting up drscheme. After that, it just provides
all of the names in the tools library, for use defining keybindings

|#

(module tool-lib mzscheme
  (require "private/link.ss"
	   "private/drsig.ss"
	   (lib "class.ss")
	   (lib "unit.ss")
           (lib "framework.ss" "framework")
	   (lib "splash.ss" "framework"))
  
  (shutdown-splash)
  (define-values/invoke-unit/infer drscheme@)
  (close-splash)
  (provide-signature-elements drscheme:tool^))
