
(unless (with-handlers ([exn:fail? (lambda (x) #f)])
	  (namespace-variable-binding 'SECTION)
	  #t)
  (load-relative "testing.rktl"))
