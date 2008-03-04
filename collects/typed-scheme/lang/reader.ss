(module reader scheme/base
  (require (prefix-in r: "../typed-reader.ss")
	   (only-in syntax/module-reader wrap-read-all))
  
  (define (*read in)
    (wrap-read-all 'typed-scheme in r:read))
  
  (define (*read-syntax src in)
    (wrap-read-all 'typed-scheme
		   in
		   (lambda (in)
		     (r:read-syntax src in))))
  
  (provide (rename-out [*read read] [*read-syntax read-syntax])))
