(module cm mzscheme
  (require (lib "modcode.ss" "syntax")
           (lib "modresolve.ss" "syntax")
	   (lib "main-collects.ss" "setup")
	   (lib "file.ss"))

  (provide make-compilation-manager-load/use-compiled-handler
	   managed-compile-zo
	   make-caching-managed-compile-zo
	   trust-existing-zos
	   manager-compile-notify-handler
	   (rename trace manager-trace-handler))
  
  (define manager-compile-notify-handler (make-parameter void))
  (define trace (make-parameter void))
  (define indent (make-parameter ""))
  (define trust-existing-zos (make-parameter #f))

  (define (trace-printf fmt . args) 
    ((trace) (string-append (indent) (apply format fmt args))))
  
  (define my-max
    (case-lambda
      (() 0)
      (x (apply max x))))
  
  (define (get-deps code path)
    (let-values ([(imports fs-imports ft-imports fl-imports) (module-compiled-imports code)])
      (map path->bytes
	   (map (lambda (x)
		  (resolve-module-path-index x path))
		;; Filter symbols:
		(let loop ([l (append imports fs-imports ft-imports fl-imports)])
		  (cond
		   [(null? l) null]
		   [(symbol? (car l)) (loop (cdr l))]
		   [else (cons (car l) (loop (cdr l)))]))))))

  (define (get-compilation-dir+name mode path)
    (let-values (((base name-suffix must-be-dir?) (split-path path)))
      (let ((name (path-replace-suffix name-suffix #"")))
	(values
	 (cond
	  ((eq? 'relative base) mode)
	  (else (build-path base mode)))
	 name))))

  (define (get-compilation-path mode path)
    (let-values ([(dir name) (get-compilation-dir+name mode path)])
      (path->bytes (build-path dir name))))

  (define (get-code-dir mode path)
    (let-values (((base name-suffix must-be-dir?) (split-path path)))
      (cond
        ((eq? 'relative base) mode)
        (else (build-path base mode)))))

  (define (try-delete-file path)
    ;; Attempt to delete, but give up if it
    ;;  doesn't work:
    (with-handlers ([exn:fail:filesystem? void])
      (trace-printf "deleting: ~a" path)
      (delete-file path)))

  ;; with-compile-output : path (output-port -> alpha) -> alpha
  ;;  Open path for writing, and arranges to delete path if there's
  ;;  an exception. Breaks are managed so that the port is reliably
  ;;  closed and the file is reliably deleted if there's a break
  (define (with-compile-output path proc)
    (let ([bp (current-break-parameterization)])
      (with-handlers ([void (lambda (exn)
			      (try-delete-file path)
			      (raise exn))])
	(let ([out (open-output-file path 'truncate/replace)])
	  (dynamic-wind
	      void
	      (lambda ()
		(call-with-break-parameterization
		 bp
		 (lambda ()
		   (proc out))))
	      (lambda ()
		(close-output-port out)))))))

  (define (write-deps code mode path external-deps)
    (let ((dep-path (bytes->path
		     (bytes-append (get-compilation-path mode path) #".dep")))
          (deps (get-deps code path)))
      (with-compile-output 
       dep-path
       (lambda (op)
	 (write (cons (version)
		      (append (map path->main-collects-relative deps)
			      (map (lambda (x) (path->main-collects-relative (cons 'ext x)))
				   external-deps)))
		op)
	 (newline op)))))

  (define (touch path)
    (close-output-port (open-output-file path 'append)))

  (define (compilation-failure mode path zo-name date-path reason)
    (with-handlers ((exn:fail:filesystem? void))
      (delete-file zo-name))
    (let ([fail-path (bytes->path
		      (bytes-append (get-compilation-path mode path) #".fail"))])
      (with-compile-output 
       fail-path
       (lambda (p)
	 (display reason p))))
    (trace-printf "failure"))

  (define (compile-zo mode path)
    ((manager-compile-notify-handler) path)
    (trace-printf "compiling: ~a" path)
    (parameterize ([indent (string-append "  " (indent))])
      (let ([zo-name (bytes->path (bytes-append (get-compilation-path mode path) #".zo"))])
        (cond
         [(and (file-exists? zo-name) (trust-existing-zos)) (touch zo-name)]
         [else
          (when (file-exists? zo-name) (delete-file zo-name))
          (with-handlers ([exn:get-module-code?
                           (lambda (ex)
                             (compilation-failure
                              mode path zo-name (exn:get-module-code-path ex)
			      (exn-message ex)))])
            (let* ([param
                    ;; Avoid using cm while loading cm-ctime:
                    (parameterize ([use-compiled-file-paths null])
                      (dynamic-require '(lib "cm-ctime.ss" "mzlib" "private")
                                       'current-external-file-registrar))]
                   [external-deps null]
                   [code (parameterize ([param (lambda (ext-file)
                                                 (set! external-deps
                                                       (cons (path->bytes ext-file)
                                                             external-deps)))])
                           (get-module-code path mode))]
                   [code-dir (get-code-dir mode path)])
              (if (not (directory-exists? code-dir))
                (make-directory* code-dir))
	      (with-compile-output
	       zo-name
	       (lambda (out)
		 (with-handlers ((exn:fail?
				  (lambda (ex)
				    (close-output-port out)
				    (try-delete-file zo-name)
				    (compilation-failure mode path zo-name #f (exn-message ex)))))
		   (parameterize ([current-write-relative-directory
				   (let-values ([(base name dir?) (split-path path)])
				     (if (eq? base 'relative)
					 (current-directory)
					 (path->complete-path base (current-directory))))])
		     (write code out)))
		 ;; redundant, but close as early as possible:
		 (close-output-port out)
		 ;; Note that we check time and write .deps before returning from with-compile-output...
		 (let ([ss-sec (file-or-directory-modify-seconds path)]
		       [zo-sec (if (file-exists? zo-name)
				   (file-or-directory-modify-seconds zo-name)
				   +inf.0)])
		   (when (< zo-sec ss-sec)
		     (error 'compile-zo
			    "date for newly created .zo file (~a @ ~a) is before source-file date (~a @ ~a)~a"
			    zo-name
			    (format-date (seconds->date zo-sec))
			    path
			    (format-date (seconds->date ss-sec))
			    (if (> ss-sec (current-seconds))
				", which appears to be in the future"
				""))))
		 (write-deps code mode path external-deps)))))])))
    (trace-printf "end compile: ~a" path))

  (define (format-date date)
    (format "~a:~a:~a:~a:~a:~a"
	    (date-year date)
	    (date-month date)
	    (date-day date)
	    (date-hour date)
	    (date-minute date)
	    (date-second date)))
  
  (define (append-object-suffix f)
    (path-replace-suffix f (case (system-type)
			     [(windows) #".dll"]
			     [else #".so"])))

  (define _loader-path (append-object-suffix (bytes->path #"_loader")))

  (define (get-compiled-time mode path w/fail?)
    (let*-values  ([(dir name) (get-compilation-dir+name mode path)])
      (first-date
       (lambda () (build-path dir "native" (system-library-subpath) _loader-path))
       (lambda () (build-path dir "native" (system-library-subpath) (append-object-suffix name)))
       (lambda () (build-path dir (path-replace-suffix name #".zo")))
       (and w/fail? (lambda () (build-path dir (path-replace-suffix name #".fail")))))))

  (define first-date
    (case-lambda
     [() -inf.0]
     [(f . l)
      (if f
	  (with-handlers ([exn:fail:filesystem?
			   (lambda (ex)
			     (apply first-date l))])
            (let ([name (f)])
              (file-or-directory-modify-seconds name)))
	  (apply first-date l))]))
  
  (define (compile-root mode path up-to-date)
    (let ([path (simplify-path (expand-path path))])
      (let ((stamp (and up-to-date
			(hash-table-get up-to-date path #f))))
	(cond
          (stamp stamp)
          (else
           (trace-printf "checking: ~a" path)
           (let ((path-zo-time (get-compiled-time mode path #f))
                 (path-time 
                  (with-handlers ((exn:fail:filesystem? 
                                   (lambda (ex)
                                     (trace-printf "~a does not exist" path)
                                     #f)))
                    (file-or-directory-modify-seconds path))))
             (cond
               ((not path-time) path-zo-time)
               (else
                (cond
                  ((> path-time path-zo-time) 
		   (trace-printf "newer src...")
		   (compile-zo mode path))
                  (else
                   (let ((deps (with-handlers ((exn:fail:filesystem? (lambda (ex) (list (version)))))
                                 (call-with-input-file (bytes->path 
							(bytes-append (get-compilation-path mode path) #".dep"))
                                   read))))
                     (cond
                       ((or (not (pair? deps))
                            (not (equal? (version) (car deps))))
			(trace-printf "newer version...")
                        (compile-zo mode path))
                       ((ormap (lambda (d)
				 ;; str => str is a module file name (check transitive dates)
				 ;; (cons 'ext str) => str is an non-module file (check date)
				 (let ([t (cond
					   [(bytes? d) (compile-root mode (bytes->path d) up-to-date)]
					   [(path? d) (compile-root mode d up-to-date)]
					   [(and (pair? d) 
						 (eq? (car d) 'ext)
						 (or (bytes? (cdr d))
						     (path? (cdr d))))
					    (with-handlers ((exn:fail:filesystem?
							     (lambda (ex) +inf.0)))
					      (file-or-directory-modify-seconds (if (bytes? (cdr d))
										    (bytes->path (cdr d))
										    (cdr d))))]
					   [else +inf.0])])
				   (when (> t path-zo-time)
				     (trace-printf "newer: ~a (~a > ~a)..." d t path-zo-time))
				   (> t path-zo-time)))
			       (map main-collects-relative->path (cdr deps)))
                        (compile-zo mode path))))))
                (let ((stamp (get-compiled-time mode path #t)))
                  (hash-table-put! up-to-date path stamp)
                  stamp)))))))))
  
  (define (managed-compile-zo zo)
    ((make-caching-managed-compile-zo) zo))
  
  (define (make-caching-managed-compile-zo)
    (let ([cache (make-hash-table 'equal)])
      (lambda (zo)
	(parameterize ([current-load/use-compiled (make-compilation-manager-load/use-compiled-handler/table cache)])
	  (compile-root (car (use-compiled-file-paths)) (path->complete-path zo) cache)))))

  (define (make-compilation-manager-load/use-compiled-handler)
    (make-compilation-manager-load/use-compiled-handler/table (make-hash-table 'equal)))

  (define (make-compilation-manager-load/use-compiled-handler/table cache)
    (let ([orig-eval (current-eval)]
	  [orig-load (current-load)]
	  [orig-namespace (current-namespace)]
	  [default-handler (current-load/use-compiled)]
	  [modes (use-compiled-file-paths)])
      (when (null? modes)
	(raise-mismatch-error 'make-compilation-manager-... 
			      "empty use-compiled-file-paths list: "
			      modes))
      (letrec ([compilation-manager-load-handler
		(lambda (path mod-name)
		  (cond
                    [(not mod-name)
                     (trace-printf "skipping:  ~a mod-name ~s" path mod-name)
                     (default-handler path mod-name)]
                    [(not (member (car modes) (use-compiled-file-paths)))
                     (trace-printf "skipping:  ~a compiled-paths ~s" path (use-compiled-file-paths))
                     (default-handler path mod-name)]
                    [(not (eq? compilation-manager-load-handler (current-load/use-compiled)))
                     (trace-printf "skipping:  ~a current-load/use-compiled changed ~s" 
                                   path
                                   (current-load/use-compiled))
                     (default-handler path mod-name)]
                    [(not (eq? orig-eval (current-eval)))
                     (trace-printf "skipping:  ~a orig-eval ~s current-eval ~s" 
                                   path orig-eval 
                                   (current-eval))
                     (default-handler path mod-name)]
                    [(not (eq? orig-load (current-load)))
                     (trace-printf "skipping:  ~a orig-load ~s current-load ~s" 
                                   path
                                   orig-load
                                   (current-load))
                     (default-handler path mod-name)]
                    [(not (eq? orig-namespace (current-namespace)))
                     (trace-printf "skipping:  ~a orig-namespace ~s current-namespace ~s" 
                                   path
                                   orig-namespace
                                   (current-namespace))
                     (default-handler path mod-name)]
                    [else 
                     (trace-printf "processing: ~a" path)
                     (compile-root (car modes) path cache)
		     (trace-printf "done: ~a" path)
                     (default-handler path mod-name)]))])
	compilation-manager-load-handler))))
