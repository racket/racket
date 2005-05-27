
(module pre-installer mzscheme
  (require (lib "setup-extension.ss" "make")
	   (lib "compile.ss" "dynext")
	   (lib "link.ss" "dynext")
	   (lib "restart.ss")
	   (lib "launcher.ss" "launcher")
	   (lib "file.ss"))

  (define (pre-installer plthome openssl-dir)
    (define (go file)
      (pre-install plthome
		   openssl-dir
		   file
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
		   (lambda (k) (k))))
    (go "mzssl.c")

    ;; Build for 3m when it looks like we can/should.
    ;; This is a hack --- hopefully temporary!
    (let ([3m-dir (build-path "compiled" "native" (system-library-subpath #f) "3m")]
	  [mzssl.so (case (system-type)
		      [(windows) "mzssl.dll"]
		      [(macosx) "mzssl.dylib"]
		      [else "mzssl.so"])])
      (parameterize ([current-directory openssl-dir])
	(when (and (memq (system-type) '(unix macosx windows))
		   (memq '3m (available-mzscheme-variants))
		   (directory-exists? (build-path 'up 'up "src" "mzscheme" "gc2")))
	  (when (or (not (file-exists? (build-path 3m-dir mzssl.so)))
		    (not (file-exists? (build-path 3m-dir "mzssl.c")))
		    ((file-or-directory-modify-seconds (build-path 3m-dir 'up mzssl.so))
		     . > .
		     (file-or-directory-modify-seconds (build-path 3m-dir mzssl.so))))
	    (make-directory* 3m-dir)
	    (restart-mzscheme #() 
			      (lambda (x) x)
			      (list->vector 
			       (list
				"-qr"
				(path->string
				 (build-path 'up 'up "src" "mzscheme" "gc2" "xform.ss"))
				(let* ([inc (build-path 'up 'up "include")]
				       [fix-path (lambda (s)
						   (regexp-replace* " " (path->string s) "\" \""))]
				       [extras (cond ((getenv "PLT_EXTENSION_LIB_PATHS") =>
						      (lambda (ext)
							(apply string-append
							       (map (lambda (p)
								      (format 
								       " ~a~s"
								       (if (eq? 'windows (system-type))
									   " /I"
									   " -I")
								       (fix-path
									(build-path p "include"))))
								    (path-list-string->path-list ext '())))))
						     (else ""))])
				  (if (eq? 'windows (system-type))
				      (format "cl.exe /MT /E /I~a /I~a~a" 
					      (fix-path inc)
					      (fix-path
					       (build-path openssl-dir "openssl" "include"))
					      extras)
				      (format "gcc -E ~a-I~a~a" 
					      (if (eq? 'macosx (system-type))
						  "-DOS_X "
						  "")
					      (fix-path inc) 
					      extras)))
				"mzssl.c"
				(path->string 
				 (build-path 3m-dir "mzssl.c"))))
			      void))
	  (parameterize ([link-variant '3m])
	    (with-new-flags current-extension-compiler-flags
			    (if (eq? 'windows (system-type))
				'("/Zi")
				null)
	      (with-new-flags current-extension-linker-flags
			      (if (eq? 'windows (system-type))
				  '("/Zi")
				  null)
	        (go (build-path 3m-dir "mzssl.c"))))))))

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
