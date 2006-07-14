(module file mzscheme
  (provide find-relative-path
	   explode-path
	   normalize-path
	   build-absolute-path
	   build-relative-path
	   filename-extension
	   file-name-from-path
	   path-only
	   delete-directory/files
	   copy-directory/files
	   make-directory*
	   make-temporary-file
	   find-library

	   get-preference
	   put-preferences

	   call-with-input-file*
	   call-with-output-file*

	   fold-files
	   find-files
	   pathlist-closure)

  (require "list.ss"
	   "etc.ss")

  (define build-relative-path
    (lambda (p . args)
      (if (relative-path? p)
	  (apply build-path p args)
	  (error 'build-relative-path "base path ~s is absolute" p))))

  (define build-absolute-path
    (lambda (p . args)
      (if (relative-path? p)
	  (error 'build-absolute-path "base path ~s is relative" p)
	  (apply build-path p args))))

  ;; Note that normalize-path does not normalize the case
  (define normalize-path
    (letrec ([resolve-all
	      (lambda (path wrt)
		(let ([orig-path (if (and wrt (not (complete-path? path)))
				     (path->complete-path path wrt)
				     path)])
		  (let loop ([full-path orig-path][seen-paths (list orig-path)])
		    (let ([resolved (resolve-path full-path)])
		      (if (equal? resolved full-path)
			  (do-normalize-path resolved #f)
			  (let ([path (if (relative-path? resolved)
					  (build-path
					   (let-values ([(base name dir?) (split-path full-path)])
					     base)
					   resolved)
					  resolved)])
			    (if (member path seen-paths)
				(error 'normalize-path "circular reference at ~s" path)
				(let ([spath
				       ;; Use simplify-path to get rid of ..s, which can
				       ;;  allow the path to grow indefinitely in a cycle.
				       ;; An exception must mean a cycle of links.
				       (with-handlers ([exn:fail:filesystem?
							(lambda (x)
							  (error 'normalize-path "circular reference at ~s" path))])
					 (simplify-path path))])
				  (loop spath (cons path seen-paths))))))))))]
	     [resolve
	      (lambda (path)
		(if (equal? path (resolve-path path))
		    path
		    (resolve-all path #f)))]
	     [normalize-path
	      (case-lambda 
	       [(orig-path) (do-normalize-path orig-path (current-directory))]
	       [(orig-path wrt) 
		(unless (complete-path? wrt)
		  (raise-type-error 'normalize-path "complete path" wrt))
		(do-normalize-path orig-path wrt)])]
	     [error-not-a-dir
	      (lambda (path)
		(error 'normalize-path 
		       "~s (within the input path) is not a directory or does not exist"
		       path))]
	     [do-normalize-path
	      (lambda (orig-path wrt)
		(let normalize ([path (expand-path orig-path)])
		  (let-values ([(base name dir?) (split-path path)])
		    (cond
		     [(eq? name 'up)
		      (let up ([base (if (eq? base 'relative)
					 wrt
					 (resolve-all base wrt))])
			(if (directory-exists? base)
			    (let-values ([(prev name dir?) (split-path base)])
			      (cond
			       [(not prev) 
				(error 'normalize-path
				       "root has no parent directory: ~s"
				       orig-path)]
			       [else
				(let ([prev
				       (if (eq? prev 'relative)
					   wrt
					   (normalize prev))])
				  (cond
				   [(eq? name 'same) (up prev)]
				   [(eq? name 'up) (up (up prev))]
				   [else prev]))]))
			    (error-not-a-dir base)))]
		     [(eq? name 'same)
		      (cond
		       [(eq? base 'relative) wrt]
		       [else (let ([n (normalize base)])
			       (if (directory-exists? n)
				   n
				   (error-not-a-dir n)))])]
		     [else
		      (cond
		       [(not base) (path->complete-path path)]
		       [else (let* ([base (if (eq? base 'relative)
					      (normalize wrt)
					      (normalize base))]
				    [path (if (directory-exists? base)
					      (build-path base name)
					      (error-not-a-dir base))]
				    [resolved (expand-path (resolve path))])
			       (cond
				[(relative-path? resolved)
				 (normalize (build-path base resolved))]
				[(complete-path? resolved)
				 resolved]
				[else (path->complete-path resolved base)]))])]))))])
      normalize-path))

  ;; Argument must be in normal form
  (define do-explode-path
    (lambda (who orig-path)
      (let loop ([path orig-path][rest '()])
	(let-values ([(base name dir?) (split-path path)])
	  (when (or (and base
			 (not (path? base)))
		    (not (path? name)))
	    (raise-type-error who "path in normal form" orig-path))
	  (if base
	      (loop base (cons name rest))
	      (cons name rest))))))

  (define explode-path
    (lambda (orig-path)
      (unless (path-string? orig-path)
	(raise-type-error 'explode-path "path or string" orig-path))
      (do-explode-path 'explode-path orig-path)))

  ;; Arguments must be in normal form
  (define find-relative-path
    (lambda (directory filename)
      (let ([dir (do-explode-path 'find-relative-path directory)]
	    [file (do-explode-path 'find-relative-path filename)])
	(if (equal? (car dir) (car file))
	    (let loop ([dir (cdr dir)]
		       [file (cdr file)])
	      (cond
	       [(null? dir) (if (null? file) filename (apply build-path file))]
	       [(null? file) (apply build-path (map (lambda (x) 'up) dir))]
	       [(equal? (car dir) (car file))
		(loop (cdr dir) (cdr file))]
	       [else
		(apply build-path 
		       (append (map (lambda (x) 'up) dir)
			       file))]))
	    filename))))

  (define (file-name who name)
    (unless (path-string? name)
      (raise-type-error who "path or string" name))
    (let-values ([(base file dir?) (split-path name)])
      (and (not dir?) (path? file) file)))

  (define (file-name-from-path name)
    (file-name 'file-name-from-path name))

  (define (path-only name)
    (unless (path-string? name)
      (raise-type-error 'path-only "path or string" name))
    (let-values ([(base file dir?) (split-path name)])
      (cond [dir? name]
            [(path? base) base]
            [else #f])))

  ;; name can be any string; we just look for a dot
  (define (filename-extension name)
    (let* ([name (file-name 'filename-extension name)]
           [name (and name (path->bytes name))])
      (cond [(and name (regexp-match #rx#"[.]([^.]+)$" name)) => cadr]
            [else #f])))

  (define (delete-directory/files path)
    (unless (path-string? path)
      (raise-type-error 'delete-directory/files "path or string" path))
    (cond
     [(or (link-exists? path) (file-exists? path))
      (delete-file path)]
     [(directory-exists? path)
      (for-each (lambda (e) (delete-directory/files (build-path path e)))
		(directory-list path))
      (delete-directory path)]
     [else (error 'delete-directory/files
		  "encountered ~a, neither a file nor a directory"
		  path)]))

  (define (copy-directory/files src dest)
    (cond
     [(file-exists? src)
      (copy-file src dest)]
     [(directory-exists? src)
      (make-directory dest)
      (for-each (lambda (e) (copy-directory/files (build-path src e)
						  (build-path dest e)))
		(directory-list src))]
     [else (error 'copy-directory/files
		  "encountered ~a, neither a file nor a directory"
		  src)]))

  (define (make-directory* dir)
    (let-values ([(base name dir?) (split-path dir)])
      (when (and (path? base)
		 (not (directory-exists? base)))
	(make-directory* base))
      (unless (directory-exists? dir)
        (make-directory dir))))

  (define make-temporary-file
    (case-lambda
     [(template copy-from base-dir)
      (with-handlers ([exn:fail:contract?
		       (lambda (x)
			 (raise-type-error 'make-temporary-file
					   "format string for 1 argument"
					   template))])
	(format template void))
      (unless (or (not copy-from) (path-string? copy-from) (eq? copy-from 'directory))
	(raise-type-error 'make-temporary-file "path, valid-path string, 'directory, or #f" copy-from))
      (unless (or (not base-dir) (path-string? base-dir))
	(raise-type-error 'make-temporary-file "path, valid-path, string, or #f" base-dir))
      (let ([tmpdir (find-system-path 'temp-dir)])
	(let loop ([s (current-seconds)][ms (current-milliseconds)])
	  (let ([name (let ([n (format template (format "~a~a" s ms))])
			(cond
			 [base-dir (build-path base-dir n)]
			 [(relative-path? n) (build-path tmpdir n)]
			 [else n]))])
	    (with-handlers ([exn:fail:filesystem:exists? (lambda (x) 
							   ;; try again with a new name
							   (loop (- s (random 10))
								 (+ ms (random 10))))])
	      (if copy-from
		  (if (eq? copy-from 'directory)
		      (make-directory name)
		      (copy-file copy-from name))
		  (close-output-port (open-output-file name)))
	      name))))]
     [(template copy-from) (make-temporary-file template copy-from #f)]
     [(template) (make-temporary-file template #f #f)]
     [() (make-temporary-file "mztmp~a" #f #f)]))
  
  (define find-library
    (case-lambda 
     [(name) (find-library name "mzlib")]
     [(name collection . cp)
      (let ([dir (with-handlers ([exn:fail:filesystem? (lambda (exn) #f)])
		   (apply collection-path collection cp))])
	(if dir
	    (let ([file (build-path dir name)])
	      (if (file-exists? file)
		  file
		  #f))
	    #f))]))

  (define (with-pref-params thunk)
    (parameterize ((read-case-sensitive #f)
		   (read-square-bracket-as-paren #t)
		   (read-curly-brace-as-paren #t)
		   (read-accept-box #t)
		   (read-accept-compiled #f)
		   (read-accept-bar-quote #t)
		   (read-accept-graph #t)
		   (read-decimal-as-inexact #t)
		   (read-accept-dot #t)
		   (read-accept-quasiquote #t)
		   (read-accept-reader #f)
		   (print-struct #f)
		   (print-graph #t)
		   (print-box #t)
		   (print-vector-length #t)
		   (current-readtable #f))
      (thunk)))


  (define pref-box (make-weak-box #f)) ; non-weak box => need to save
  (define (get-prefs flush? filename)
    (let ([f (and (not flush?)
		  (not filename)
		  (weak-box-value pref-box))])
      (or f
	  (let ([f (let ([v (with-handlers ([exn:fail:filesystem? (lambda (x) null)])
			      (let ([pref-file (or filename
						   (let ([f (find-system-path 'pref-file)])
						     (if (file-exists? f)
							 ;; Using `file-exists?' means there's technically
							 ;; a race condition, but something
							 ;; has gone really wrong if the file disappears.
							 f
							 ;; Error here bails out through above `with-handlers'
							 (build-path (collection-path "defaults")
								     "plt-prefs.ss"))))])
				(with-pref-params
				 (lambda ()
				   (with-input-from-file pref-file
				     read)))))])
		     ;; Make sure file content had the right shape:
		     (if (and (list? v) 
			      (andmap (lambda (x) 
					(and (pair? x) 
					     (pair? (cdr x))
					     (null? (cddr x))))
				      v))
			 v
			 null))])
	    (unless filename
	      (set! pref-box (make-weak-box f)))
	    f))))

  (define get-preference
    (case-lambda 
     [(name fail-thunk refresh-cache? filename)
      (unless (symbol? name)
	(raise-type-error
	 'get-preference
	 "symbol"
	 name))
      (unless (and (procedure? fail-thunk)
		   (procedure-arity-includes? fail-thunk 0))
	(raise-type-error
	 'get-preference
	 "procedure (arity 0)"
	 fail-thunk))
      (let ([f (get-prefs refresh-cache? filename)])
	(let ([m (assq name f)])
	  (if m
	      (cadr m)
	      (fail-thunk))))]
     [(name fail-thunk refresh-cache?) (get-preference name fail-thunk refresh-cache? #f)]
     [(name fail-thunk) (get-preference name fail-thunk #t #f)]
     [(name) (get-preference name (lambda () #f) #t #f)]))

  (define put-preferences
    (case-lambda
     [(names vals lock-there filename)
      (unless (and (list? names)
		   (andmap symbol? names))
	(raise-type-error
	 'put-preferences
	 "list of symbols"
	 names))
      (unless (list? vals)
	(raise-type-error
	 'put-preferences
	 "list"
	 vals))
      (unless (= (length names) (length vals))
	(raise-mismatch-error
	 'put-preferences
	 (format "the size of the name list (~a) does not match the size of the value list (~a): "
		 (length names) (length vals))
	 vals))
      (let-values ([(pref-file lock-file pref-dir)
		    (let ([filename (or filename (find-system-path 'pref-file))])
		      (let-values ([(base name dir?) (split-path filename)])
			(let ([dir (if (symbol? base)
				       (current-directory)
				       base)])
			  (unless (directory-exists? dir)
			    (make-directory* dir))
			  (values filename
				  (build-path dir (bytes->path
						   (bytes-append
						    (if (eq? 'windows (system-type))
							#"_"
							#".")
						    #"LOCK"
						    (path->bytes name))))
				  dir))))])
	(with-handlers ([exn:fail:filesystem:exists?
			 (lambda (x)
			   (lock-there lock-file))])
	  ;; Grab lock:
	  (close-output-port (open-output-file lock-file 'error))
	  (dynamic-wind
	      void
	      (lambda ()
		(let ([f (get-prefs #t filename)])
		  (for-each
		   (lambda (name val)
		     (let ([m (assq name f)])
		       (if m
			   (set-car! (cdr m) val)
			   (set! f (cons (list name val) f)))))
		   names vals)
		  (unless filename
		    (set! pref-box (make-weak-box f)))
		  ;; To write the file, copy the old one to a temporary name
		  ;; (preserves permissions, etc), write to the temp file,
		  ;; then move (atomicly) the temp file to the normal name.
		  (let* ([tmp-file (make-temporary-file
				    "TMPPREF~a"
				    (and (file-exists? pref-file) pref-file)
				    pref-dir)])
		    ;; If something goes wrong, try to delete the temp file.
		    (with-handlers ([exn:fail? (lambda (exn)
						 (with-handlers ([exn:fail:filesystem? void])
						   (delete-file tmp-file))
						 (raise exn))])
		      ;; Write to temp file...
		      (with-output-to-file tmp-file
			(lambda ()
			  (with-pref-params
			   (lambda ()
			     ;; If a pref value turns out to be unreadable, raise
			     ;;  an exception instead of creating a bad pref file.
			     (parameterize ([print-unreadable #f])
			       ;; Poor man's pretty-print: one line per entry.
			       (printf "(~n")
			       (for-each (lambda (a)
					   (if (list? (cadr a))
					       (begin
						 (printf " (~s~n  (~n" (car a))
						 (for-each (lambda (i) (printf "  ~s~n" i)) (cadr a))
						 (printf "  ))~n"))
					       (printf " ~s~n" a)))
					 f)
			       (printf ")~n")))))
			'truncate/replace)
		      (rename-file-or-directory tmp-file pref-file #t)))))
	      (lambda ()
		;; Release lock:
		(delete-file lock-file)))))]
     [(names vals lock-there) 
      (put-preferences names vals lock-there #f)]
     [(names vals) 
      (put-preferences
       names vals 
       (lambda (lock-file) 
	 (error 'put-preferences
		"some other process has the preference-file lock, as indicated by the existence of the lock file: ~e"
		lock-file)))]))

  (define call-with-input-file*
    (lambda (file thunk . flags)
      (let ([p (apply open-input-file file flags)])
	(dynamic-wind
	    void
	    (lambda () (thunk p))
	    (lambda () (close-input-port p))))))	  

  (define call-with-output-file*
    (lambda (file thunk . flags)
      (let ([p (apply open-output-file file flags)])
	(dynamic-wind
	    void
	    (lambda () (thunk p))
	    (lambda () (close-output-port p))))))

  ;; fold-files : (pathname sym alpha -> alpha) alpha pathname/#f -> alpha
  (define fold-files
    (opt-lambda (f init [path #f] [follow-links? #t])
      (define (do-path path acc)
        (cond [(and (not follow-links?) (link-exists? path)) (f path 'link acc)]
              [(directory-exists? path)
               (call-with-values (lambda () (f path 'dir acc))
                 (letrec ([descend
                           (case-lambda
                             [(acc)
                              (do-paths (map (lambda (p) (build-path path p))
                                             (sort
                                              (directory-list path) void))
                                        acc)]
                             [(acc descend?)
                              (if descend? (descend acc) acc)])])
                   descend))]
              [(file-exists? path) (f path 'file acc)]
              [(link-exists? path) (f path 'link acc)] ; dangling links
              [else (error 'fold-files "path disappeared: ~e" path)]))
      (define (do-paths paths acc)
        (cond [(null? paths) acc]
              [else (do-paths (cdr paths) (do-path (car paths) acc))]))
      (if path (do-path path init) (do-paths (directory-list) init))))

  (define find-files
    (opt-lambda (f [path #f])
      (reverse!
       (fold-files (lambda (path kind acc) (if (f path) (cons path acc) acc))
                   null path))))

  (define (pathlist-closure paths)
    (let loop ([paths (map (lambda (p) (simplify-path (resolve-path p) #f))
                           paths)]
               [r '()])
      (if (null? paths)
        (reverse! r)
        (let loop2 ([path (car paths)]
                    [new (cond [(file-exists? (car paths))
                                (list (car paths))]
                               [(directory-exists? (car paths))
                                (find-files void (car paths))]
                               [else (error 'pathlist-closure
                                            "file/directory not found: ~a"
                                            (car paths))])])
          (let-values ([(base name dir?) (split-path path)])
            (if (path? base)
              (loop2 base (if (or (member base r) (member base paths))
                            new (cons base new)))
              (loop (cdr paths) (append! (reverse! new) r))))))))

  )
