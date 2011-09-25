(module path-spec racket/base
  (require (for-template racket/base))

  (provide resolve-path-spec)

  (define (resolve-path-spec fn loc stx)
    (let ([file
	   (syntax-case fn (lib file)
	     [_
	      (string? (syntax-e fn))
	      (let ([s (syntax-e fn)])
		(unless (module-path? s)
		  (raise-syntax-error
		   #f
		   "bad relative pathname string"
		   stx
		   fn))
                (apply build-path
                       (regexp-split #rx"/" s)))]
	     [(file . _)
	      (let ([l (syntax->datum fn)])
		(unless (module-path? l)
		  (raise-syntax-error
		   #f
		   "bad `file' path"
		   stx
		   fn))
                (string->path (cadr l)))]
	     [(lib . _)
	      (let ([l (syntax->datum fn)])
		(unless (module-path? l)
		  (raise-syntax-error
		   #f
		   "bad `lib' path"
		   stx
		   fn))
                (let ([s (resolved-module-path-name
                          (module-path-index-resolve
                           (module-path-index-join l #f)))])
                  (if (path? s)
                      s
                      (raise-syntax-error
                       #f
                       "`lib' path produced symbolic module name"
                       stx
                       fn))))]
	     [else
	      (raise-syntax-error
	       #f
	       "not a pathname string, `file' form, or `lib' form for file"
	       stx
	       fn)])])
      (if (complete-path? file)
	  file
	  (path->complete-path
	   file
	   (cond
	    ;; Src of include expression is a path?
	    [(and (path? (syntax-source loc))
		  (complete-path? (syntax-source loc)))
	     (let-values ([(base name dir?) 
			   (split-path (syntax-source loc))])
	       (if dir?
		   (syntax-source loc)
		   base))]
	    ;; Load relative?
	    [(current-load-relative-directory)]
	    ;; Current directory
	    [(current-directory)]))))))
