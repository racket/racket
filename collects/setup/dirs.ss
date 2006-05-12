(module dirs mzscheme
  (require (lib "winutf16.ss" "compiler" "private"))

  (define main-collects-dir
    (delay
      (let ([d (find-system-path 'collects-dir)])
	(cond
	 [(complete-path? d) d]
	 [(absolute-path? d)
	  ;; This happens only under Windows; add a drive
	  ;;  specification to make the path complete
	  (let ([exec (find-system-path 'exec-file)])
	    (if (complete-path? exec)
		(let-values ([(base name dir?) (split-path exec)])
		  (path->complete-path d base))
		(path->complete-path d (find-system-path 'orig-dir))))]
	 [else
	  ;; Relative to executable...
	  (parameterize ([current-directory (find-system-path 'orig-dir)])
	    (let ([p (or (find-executable-path (find-system-path 'exec-file) d #t)
			 ;; If we get here, then we can't find the directory
			 #f)])
	      (and p
		   (simplify-path p))))]))))

  (provide find-main-collects-dir)
  (define (find-main-collects-dir)
    (force main-collects-dir))

  (define-syntax define-finder
    (syntax-rules ()
      [(_ provide id default)
       (begin
	 (provide id)
	 (define dir
	   (delay
	     (let ([p (find-main-collects-dir)])
	       (and p
		    (simplify-path (build-path p
					       'up
					       default))))))
	 (define (id)
	   (force dir)))]))

  (define-finder provide find-include-dir "include")
  (define-finder provide find-lib-dir "lib")

  (define-finder provide find-console-bin-dir (case (system-type)
						[(windows) 'same]
						[(macosx unix) "bin"]))

  (define-finder provide find-gui-bin-dir (case (system-type)
					    [(windows macosx) 'same]
					    [(unix) "bin"]))

  (provide find-dll-dir)
  (define dll-dir
    (delay (case (system-type)
	     [(windows)
	      ;; Extract "lib" location from binary:
	      (let ([exe (parameterize ([current-directory (find-system-path 'orig-dir)])
			   (find-executable-path (find-system-path 'exec-file)))])
		(with-input-from-file exe
		  (lambda ()
		    (let ([m (regexp-match (byte-regexp 
					    (bytes-append
					     #"("
					     (bytes->utf-16-bytes #"dLl dIRECTORy:")
					     #".*?)\0\0"))
					   (current-input-port))])
		      (unless m (error "cannot find \"dLl dIRECTORy\" tag in binary"))
		      (let-values ([(dir name dir?) (split-path exe)])
			(if (regexp-match #rx#"^<" (cadr m))
			    ;; no DLL dir in binary, so assume exe dir:
			    dir
			    ;; resolve relative directory:
			    (let ([p (bytes->path (utf-16-bytes->bytes (cadr m)))])
			      (path->complete-path p dir))))))))]
	     [else
	      (find-lib-dir)])))
  (define (find-dll-dir)
    (force dll-dir))

  )
