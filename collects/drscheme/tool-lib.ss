
#|

This first time this is loaded, it loads all of drscheme and invokes
the main unit, starting up drscheme. After that, it just provides
all of the names in the tools library, for use defining keybindings

|#

(module tool-lib mzscheme
  (require "private/link.ss"
	   "private/drsig.ss"
	   mzlib/class
	   mzlib/unit
           framework
	   (lib "splash.ss" "framework"))

  (shutdown-splash)
  (define-values/invoke-unit/infer drscheme@)
  (close-splash)
  (provide-signature-elements drscheme:tool^)
  
  (provide drscheme:unit:program-editor-mixin)
  (define-syntax (drscheme:unit:program-editor-mixin stx)
    (syntax-case stx ()
      [(_ a ...)
       #'((drscheme:unit:get-program-editor-mixin) a ...)]
      [_ #'(drscheme:unit:get-program-editor-mixin)])))

      
