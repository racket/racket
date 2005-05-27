(module path-spec mzscheme
  (require "stx.ss")

  (provide resolve-path-spec)

  (define (resolve-path-spec fn loc stx build-path-stx)
    (let ([file
	   (syntax-case* fn (lib) module-or-top-identifier=?
	     [_
	      (string? (syntax-e fn))
	      (let ([s (syntax-e fn)])
		(unless (or (relative-path? s)
			    (absolute-path? s))
		  (raise-syntax-error
		   #f
		   "bad pathname string"
		   stx
		   fn))
		(string->path s))]
	     [(-build-path elem ...)
	      (module-or-top-identifier=? #'-build-path build-path-stx)
	      (let ([l (syntax-object->datum (syntax (elem ...)))])
		(when (null? l)
		  (raise-syntax-error
		   #f
		   "`build-path' keyword is not followed by at least one string"
		   stx
		   fn))
		(apply build-path l))]
	     [(lib filename ...)
	      (let ([l (syntax-object->datum (syntax (filename ...)))])
		(unless (or (andmap string? l)
			    (pair? l))
		  (raise-syntax-error
		   #f
		   "`lib' keyword is not followed by a sequence of string datums"
		   stx
		   fn))
		(build-path (if (null? (cdr l))
				(collection-path "mzlib")
				(apply collection-path (cdr l)))
			    (car l)))]
	     [else
	      (raise-syntax-error
	       #f
	       "not a pathname string, `build-path' form, or `lib' form for file"
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
