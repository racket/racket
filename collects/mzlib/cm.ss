(module cm scheme/base
  (require syntax/modcode
           syntax/modresolve
	   (lib "main-collects.ss" "setup")
	   scheme/file)

  (provide make-compilation-manager-load/use-compiled-handler
	   managed-compile-zo
	   make-caching-managed-compile-zo
	   trust-existing-zos
	   manager-compile-notify-handler
	   (rename-out [trace manager-trace-handler]))
  
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
    (let-values ([(imports) (apply append (map cdr (module-compiled-imports code)))])
      (map path->bytes
           (let ([l (map (lambda (x)
                           (resolve-module-path-index x path))
                         imports)])
             ;; Filter symbols:
             (let loop ([l l])
               (cond
                [(null? l) null]
                [(symbol? (car l)) (loop (cdr l))]
                [else (cons (car l) (loop (cdr l)))]))))))

  (define (get-compilation-dir+name mode path)
    (let-values (((base name must-be-dir?) (split-path path)))
      (values
       (cond
        ((eq? 'relative base) mode)
        (else (build-path base mode)))
       name)))

  (define (get-compilation-path mode path)
    (let-values ([(dir name) (get-compilation-dir+name mode path)])
      (build-path dir name)))

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
	(let ([out (open-output-file path #:exists 'truncate/replace)])
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
    (let ((dep-path (path-add-suffix (get-compilation-path mode path) #".dep"))
          (deps (get-deps code path)))
      (with-compile-output 
       dep-path
       (lambda (op)
	 (write (cons (version)
		      (append (map path->main-collects-relative deps)
			      (map (lambda (x) (cons 'ext (path->main-collects-relative x)))
				   external-deps)))
		op)
	 (newline op)))))

  (define (touch path)
    (close-output-port (open-output-file path #:exists 'append)))

  (define (compilation-failure mode path zo-name date-path reason)
    (with-handlers ((exn:fail:filesystem? void))
      (delete-file zo-name))
    (trace-printf "failure"))

  (define (compile-zo mode path read-src-syntax)
    ((manager-compile-notify-handler) path)
    (trace-printf "compiling: ~a" path)
    (parameterize ([indent (string-append "  " (indent))])
      (let ([zo-name (path-add-suffix (get-compilation-path mode path) #".zo")])
        (cond
         [(and (file-exists? zo-name) (trust-existing-zos)) (touch zo-name)]
         [else
          (when (file-exists? zo-name) (delete-file zo-name))
          (with-handlers ([exn:get-module-code?
                           (lambda (ex)
                             (compilation-failure
                              mode path zo-name (exn:get-module-code-path ex)
			      (exn-message ex))
                             (raise ex))])
            (let* ([param
                    ;; Avoid using cm while loading cm-ctime:
                    (parameterize ([use-compiled-file-paths null])
                      (dynamic-require 'mzlib/private/cm-ctime
                                       'current-external-file-registrar))]
                   [external-deps null]
                   [code (parameterize ([param (lambda (ext-file)
                                                 (set! external-deps
                                                       (cons (path->bytes ext-file)
                                                             external-deps)))]
                                        [current-reader-guard
                                         (let ([rg (current-reader-guard)])
                                           (lambda (d)
                                             (let ([d (rg d)])
                                               (when (module-path? d)
                                                 (let ([p (resolved-module-path-name
                                                           (module-path-index-resolve
                                                            (module-path-index-join d #f)))])
                                                   (when (path? p)
                                                     (set! external-deps
                                                           (cons (path->bytes p)
                                                                 external-deps)))))
                                               d)))])
                           (get-module-code path mode
                                            compile
                                            (lambda (a b) #f) ; extension handler
                                            #:source-reader read-src-syntax))]
                   [code-dir (get-code-dir mode path)])
              (when code
                (when (not (directory-exists? code-dir))
                  (make-directory* code-dir))
                (with-compile-output
                 zo-name
                 (lambda (out)
                   (with-handlers ((exn:fail?
                                    (lambda (ex)
                                      (close-output-port out)
                                      (compilation-failure mode path zo-name #f (exn-message ex))
                                      (raise ex))))
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
                   (write-deps code mode path external-deps))))))])))
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
    (path-add-suffix f (system-type 'so-suffix)))

  (define (get-compiled-time mode path)
    (let*-values  ([(dir name) (get-compilation-dir+name mode path)])
      (first-date
       (lambda () (build-path dir "native" (system-library-subpath) (append-object-suffix name)))
       (lambda () (build-path dir (path-add-suffix name #".zo"))))))

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
  
  (define (compile-root mode path up-to-date read-src-syntax)
    (let ([path (simplify-path (cleanse-path path))])
      (let ((stamp (and up-to-date
			(hash-ref up-to-date path #f))))
	(cond
          (stamp stamp)
          (else
           (trace-printf "checking: ~a" path)
           (let ((path-zo-time (get-compiled-time mode path))
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
		   (compile-zo mode path read-src-syntax))
                  (else
                   (let ((deps (with-handlers ((exn:fail:filesystem? (lambda (ex) (list (version)))))
                                 (call-with-input-file (path-add-suffix (get-compilation-path mode path) #".dep")
                                   read))))
                     (cond
                       ((or (not (pair? deps))
                            (not (equal? (version) (car deps))))
			(trace-printf "newer version...")
                        (compile-zo mode path read-src-syntax))
                       ((ormap (lambda (d)
				 ;; str => str is a module file name (check transitive dates)
				 ;; (cons 'ext str) => str is an non-module file (check date)
				 (let ([t (cond
					   [(bytes? d) (compile-root mode (bytes->path d) up-to-date read-src-syntax)]
					   [(path? d) (compile-root mode d up-to-date read-src-syntax)]
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
			       (map (lambda (p)
                                      (if (and (pair? p)
                                               (eq? 'ext (car p)))
                                          (cons 'ext (main-collects-relative->path (cdr p)))
                                          (main-collects-relative->path p)))
                                    (cdr deps)))
                        (compile-zo mode path read-src-syntax))))))
                (let ((stamp (get-compiled-time mode path)))
                  (hash-set! up-to-date path stamp)
                  stamp)))))))))
  
  (define (managed-compile-zo zo [read-src-syntax read-syntax])
    ((make-caching-managed-compile-zo read-src-syntax) zo))
  
  (define (make-caching-managed-compile-zo [read-src-syntax read-syntax])
    (let ([cache (make-hash)])
      (lambda (zo)
	(parameterize ([current-load/use-compiled (make-compilation-manager-load/use-compiled-handler/table cache)])
	  (compile-root (car (use-compiled-file-paths)) (path->complete-path zo) cache read-src-syntax)
          (void)))))

  (define (make-compilation-manager-load/use-compiled-handler)
    (make-compilation-manager-load/use-compiled-handler/table (make-hash)))

  (define (make-compilation-manager-load/use-compiled-handler/table cache)
    (let ([orig-eval (current-eval)]
	  [orig-load (current-load)]
	  [orig-registry (namespace-module-registry (current-namespace))]
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
                    [(not (eq? orig-registry (namespace-module-registry (current-namespace))))
                     (trace-printf "skipping:  ~a orig-rgistry ~s current-registry ~s" 
                                   path
                                   orig-registry
                                   (namespace-module-registry (current-namespace)))
                     (default-handler path mod-name)]
                    [else 
                     (trace-printf "processing: ~a" path)
                     (compile-root (car modes) path cache read-syntax)
		     (trace-printf "done: ~a" path)
                     (default-handler path mod-name)]))])
	compilation-manager-load-handler))))
