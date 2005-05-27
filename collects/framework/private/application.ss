(module application mzscheme
  (require (lib "unitsig.ss")
	   "sig.ss"
	   (lib "mred-sig.ss" "mred"))

  (provide application@)

  (define application@
    (unit/sig framework:application^
      (import)

      (define current-app-name (make-parameter
				"MrEd"
				(λ (x)
				  (unless (string? x)
				    (error 'current-app-name
					   "the app name must be a string"))
				  x))))))