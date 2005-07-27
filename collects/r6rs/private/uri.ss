
(module uri mzscheme
  (require (lib "string.ss")
	   (lib "list.ss"))
  (provide uri->symbol
	   uri->module-path)

  (define rx:scheme-uri #rx"^[sS][cC][hH][eE][mM][eE]://([^/]+/+[^/]+.*)$")

  (define (uri->scheme-path s)
    (let ([m (regexp-match rx:scheme-uri s)])
      (and m
	   (let ([l (filter (lambda (s)
			      (not (string=? s "")))
			    (regexp-split #rx"/" (cadr m)))])
	     (let loop ([l l][accum null])
	       (cond
		[(null? (cdr l))
		 (let ([s (car l)])
		   (cons (if (regexp-match #rx"[.]" s)
			     s
			     (string-append s ".scm"))
			 (reverse accum)))]
		[else (loop (cdr l) (cons (car l) accum))]))))))
	   

  (define (uri->symbol s)
    (let ([p (uri->scheme-path s)])
      (cond
       [p (string->symbol
	   (string-append
	    ","
	    (let ([collpath
		   ;; Try to get real collection; if it doesn't exist,
		   ;;  make one up relative to mzlib.
		   (with-handlers ([exn:fail:filesystem?
				    (lambda (exn)
				      (simplify-path
				       (apply build-path (collection-path "mzlib")
					      'up
					      (cdr p))))])
		     (apply collection-path (cdr p)))])
	      (path->string (build-path collpath
					(path-replace-suffix (car p) #""))))))]
       [else (string->symbol
	      (string-append "," 
			     (path->string
			      (apply build-path 
				     (simplify-path
				      (expand-path
				       ;; Don't use (current-load-relative-directory)
				       (current-directory)))
				     (filter 
				      (lambda (x)
					(not (string=? x "")))
				      (regexp-split #rx"/" s))))))])))
  
  (define (uri->module-path s)
    (let ([p (uri->scheme-path s)])
      (cond
       [p 
	;; If the collection exists, build a `lib' path. Otherwise, assume
	;;  that we're in REPL mode, and make up a symbol using uri->symbol
	(if (with-handlers ([exn:fail:filesystem? (lambda (x) #f)])
	      (apply collection-path (cdr p)))
	    `(lib ,@p)
	    (uri->symbol s))]
       [else s]))))
