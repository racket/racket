
(module collects-path mzscheme

  (provide collects-path->bytes
	   check-collects-path
	   set-collects-path)

  (define (collects-path->bytes collects-path)
    (and collects-path
	 (cond
	  [(path? collects-path) (path->bytes collects-path)]
	  [(string? collects-path) (string->bytes/locale collects-path)]
	  [(and (list? collects-path)
		(pair? collects-path))
	   (let ([l (map (lambda (p)
			   (cond
			    [(path? p) (path->bytes p)]
			    [(string? p) (string->bytes/locale p)]
			    [else #""]))
			 collects-path)])
	     (let loop ([l l])
	       (if (null? (cdr l))
		   (car l)
		   (bytes-append (car l) #"\0" (loop (cdr l))))))]
	  [else #""])))
  
  (define (check-collects-path who collects-path collects-path-bytes)
    (when collects-path
      (unless (or (path-string? collects-path)
		  (and (list? collects-path)
		       (andmap path-string? collects-path)))
	(raise-type-error who "path, string, list of paths and strings, or #f" 
			  collects-path))
      (unless ((bytes-length collects-path-bytes) . <= . 1024)
	(error who "collects path list is too long"))))

  (define (find-cmdline rx)
    (let ([m (regexp-match-positions rx (current-input-port))])
      (if m
	  (caar m)
	  (error 
	   'create-embedding-executable
	   "can't find collection-path position in executable"))))

  (define (set-collects-path dest-exe collects-path-bytes)
    (when collects-path-bytes
      (let ([libpos (let ([tag #"coLLECTs dIRECTORy:"])
		      (+ (with-input-from-file dest-exe 
			   (lambda () (find-cmdline tag)))
			 (bytes-length tag)))])
	(with-output-to-file dest-exe
	  (lambda ()
	    (let ([out (current-output-port)])
	      (file-position out libpos)
	      (write-bytes collects-path-bytes out)
	      (write-bytes #"\0\0" out)))
	  'update)))))
