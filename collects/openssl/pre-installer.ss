
(module pre-installer mzscheme
  (require (lib "setup-extension.ss" "make")
	   (lib "compile.ss" "dynext")
	   (lib "link.ss" "dynext")
	   (lib "file.ss"))

  (define (pre-installer plthome openssl-dir)
    (pre-install plthome
		 openssl-dir
		 "mzssl.c"
		 (build-path openssl-dir
			     "openssl")
		 ;; header subdirs
		 (list "openssl")
		 ;; unix libs
		 (list "ssl" "crypto")
		 ;; windows libs
		 (let* ([default-paths
			  (list (build-path openssl-dir "openssl"))]
			[paths
			 (let ([v (getenv "PLT_EXTENSION_LIB_PATHS")])
			   (if v
			       (path-list-string->path-list v default-paths)
			       default-paths))])
		   (if (ormap (lambda (path)
				(and (file-exists? (build-path path "lib" "libeay32xxxxxxx.lib"))
				     (file-exists? (build-path path "lib" "ssleay32xxxxxxx.lib"))))
			      paths)
		       ;; Use mangleable names:
		       (list "libeay32xxxxxxx" "ssleay32xxxxxxx")
		       ;; Use simple names:
		       (list "libeay32" "ssleay32")))
		 ;; unix extra libs (assume always there)
		 null
		 ;; Windows extra libs (assume always there)
		 (list "wsock32.lib")
		 ;; Extra depends:
		 (list "mzssl.ss")
		 ;; Last-chance k:
		 (lambda (k) (k))
		 ;; 3m, too:
		 #t)

    ;; Under windows, put "{lib,sll}eay32" into the system folder when
    ;; they're in a "precompiled" dir.
    (when (eq? 'windows (system-type))
      (let ([dir (build-path openssl-dir
			     "precompiled"
			     "native"
			     (system-library-subpath #f))])
	(when (directory-exists? dir)
	  (let ([l (directory-list dir)])
	    (let ([libeay (ormap (lambda (f)
				   (regexp-match #rx"^libeay32.*[.]dll$" f))
				 l)]
		  [ssleay (ormap (lambda (f)
				   (regexp-match #rx"^ssleay32.*[.]dll$" f))
				 l)])
	      (when (and libeay ssleay)
		(let ([sys-dir (find-system-path 'sys-dir)])
		  (let ([move-over
			 (lambda (f)
			   (unless (file-exists? (build-path sys-dir f))
			     (printf "  Installing ~a into system directory~n" f)
			     (copy-file (build-path dir f)
					(build-path sys-dir f))))])
		    (move-over (car libeay))
		    (move-over (car ssleay)))))))))))

  (provide pre-installer))
