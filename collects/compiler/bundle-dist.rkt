
(module bundle-dist mzscheme
  (require mzlib/etc
	   mzlib/file
	   mzlib/process
	   mzlib/zip
	   mzlib/tar)

  (provide bundle-put-file-extension+style+filters
	   bundle-directory)
  
  (define (bundle-file-suffix)
    (case (system-type)
      [(macosx) "dmg"]
      [(windows) "zip"]
      [(unix) "tgz"]))

  (define (bundle-put-file-extension+style+filters)
    (values (bundle-file-suffix)
	    null
	    (case (system-type)
	      [(windows) '(("Zip file" "*.zip"))]
	      [(macosx) '(("Disk image" "*.dmg"))]
	      [(unix) '(("Gzipped tar file" "*.tgz"))])))

  (define (add-suffix name suffix)
    (if (filename-extension name)
	name
	(path-replace-suffix name
			     (string->bytes/utf-8 (string-append "." suffix)))))

  (define (with-prepared-directory dir for-exe? k)
    ;; If `dir' contains multiple files, create a new
    ;;  directory that contains a copy of `dir'
    (if (and for-exe?
	     (= 1 (length (directory-list dir))))
	(k dir)
	(let ([temp-dir (make-temporary-file "bundle-tmp-~a" 'directory)])
	  (dynamic-wind
	      void
	      (lambda ()
		(let ([dest
		       (let-values ([(base name dir?) (split-path dir)])
			 (build-path temp-dir name))])
		  (make-directory dest)
		  (let loop ([src dir][dest dest])
		    (for-each (lambda (f)
				(let ([src (build-path src f)]
				      [dest (build-path dest f)])
				  (cond
				   [(directory-exists? src)
				    (make-directory dest)
				    (loop src dest)]
				   [(file-exists? src)
				    (copy-file src dest)
				    (file-or-directory-modify-seconds 
				     dest
				     (file-or-directory-modify-seconds src))])))
			      (directory-list src))))
		(k temp-dir))
	      (lambda () (delete-directory/files temp-dir))))))

  (define bundle-directory
    (opt-lambda (target dir [for-exe? #f])
      (let ([target (add-suffix target (bundle-file-suffix))])
	(case (system-type)
	  [(macosx)
	   (with-prepared-directory
	    dir for-exe?
	    (lambda (dir)
	      (let* ([cout (open-output-bytes)]
		     [cerr (open-output-bytes)]
		     [cin (open-input-bytes #"")]
		     [p (process*/ports
			 cout cin cerr
			 "/usr/bin/hdiutil"
			 "create" "-format" "UDZO"
			 "-imagekey" "zlib-level=9"
			 "-mode" "555" 
			 "-volname" (path->string
				     (path-replace-suffix (file-name-from-path target) #""))
			 "-srcfolder" (path->string (expand-path (path->complete-path dir)))
			 (path->string (expand-path (path->complete-path target))))])
		((list-ref p 4) 'wait)
		(unless (eq? ((list-ref p 4) 'status) 'done-ok)
		  (error 'bundle-directory
			 "error bundling: ~a"
			 (regexp-replace #rx"[\r\n]*$" (get-output-string cerr) ""))))))]
	  [(windows unix)
	   (let-values ([(base name dir?) (split-path (path->complete-path dir))])
	     (parameterize ([current-directory base])
	       ((if (eq? 'unix (system-type)) tar-gzip zip) target name)))]
	  [else (error 'bundle-directory "don't know how")])))))
