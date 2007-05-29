
(module compile mzscheme
  (require "file.ss" 
	   "port.ss")
  (provide compile-file)
  
  ;; (require (lib "src2src.ss" "compiler"))

  (define compile-file
    (case-lambda
     [(src)
      (let-values ([(base name dir?) (split-path src)])
	(let ([cdir (build-path
		     (if (symbol? base)
			 'same
			 base)
		     "compiled")])
	  (unless (directory-exists? cdir)
	    (make-directory cdir))
	  (compile-file src (build-path cdir (path-replace-suffix name #".zo")))))]
     [(src dest) (compile-file src dest values)]
     [(src dest filter)
      (let ([in (open-input-file src)])
	(dynamic-wind
	 void
	 (lambda ()
	   (port-count-lines! in)
	   (with-handlers ([void
			    (lambda (exn)
			      (with-handlers ([void void])
				(delete-file dest))
			      (raise exn))])
	     (let ([out (open-output-file dest 'truncate/replace)]
		   [ok? #f])
	       (let ([dir (let-values ([(base name dir?) (split-path src)])
			    (if (eq? base 'relative)
				(current-directory)
				(path->complete-path base (current-directory))))])
		 (parameterize ([current-load-relative-directory dir]
				[current-write-relative-directory dir])
		   (dynamic-wind
		       void
		       (lambda ()
			 (let loop ()
			   (let ([r (read-syntax src in)])
			     (unless (eof-object? r)
			       (write (compile-syntax (filter (namespace-syntax-introduce r))) out)
			       (loop))))
			 (set! ok? #t))
		       (lambda () 
			 (close-output-port out)
			 (unless ok?
			   (with-handlers ([void void])
			     (delete-file dest))))))))))
	 (lambda () (close-input-port in))))
      dest])))

