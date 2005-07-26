
(module library-repl mzscheme
  (require (all-except "library-module.ss" #%module-begin)
	   (prefix r6rs: "reader.ss"))
  (require-for-syntax "private/uri.ss")
  (provide import export indirect-export library)

  (current-readtable r6rs:r6rs-readtable)

  (define-syntax (library stx)
    (syntax-case stx ()
      [(_ name . rest)
       (unless (string? (syntax-e #'name))
	 (raise-syntax-error
	  #f
	  "expected a string for the library name"
	  stx
	  #'name))
       (let ([modname (uri->symbol (syntax-e #'name))])
	 #`(begin
	     (module #,modname (lib "library-module.ss" "r6rs")
	       #,(datum->syntax-object
		  #f
		  (list '#%module-begin stx)))
	     ;; Notify module-name resolver that we defined something that
	     ;;  might otherwise be loaded.
	     ((current-module-name-resolver) #f '#,modname #f)))])))




