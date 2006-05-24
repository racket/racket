
(module distribute mzscheme
  (require (lib "kw.ss")
	   (lib "file.ss")
	   (lib "dirs.ss" "setup")
	   (lib "list.ss")
	   (prefix config: (lib "config.ss" "config"))
	   (lib "filename-version.ss" "dynext")
	   "private/macfw.ss"
	   "private/windlldir.ss"
	   "private/collects-path.ss")

  (provide assemble-distribution)

  (define/kw (assemble-distribution dest-dir 
				    binaries
				    #:key
				    [collects-path #f] ; relative to dest-dir
				    [copy-collects null])
    (let* ([types (map get-binary-type binaries)]
	   [_ (unless (directory-exists? dest-dir)
		(make-directory dest-dir))]
	   [sub-dirs (map (lambda (b type)
			   (case (system-type)
			     [(windows) #f]
			     [(unix) "bin"]
			     [(macosx) (if (memq type '(mred mred3m))
					   #f
					   "bin")]))
			  binaries
			  types)]
	   ;; Copy binaries into place:
	   [binaries
	    (map (lambda (b sub-dir type)
		   (let ([dest-dir (if sub-dir
				       (build-path dest-dir sub-dir)
				       dest-dir)])
		     (unless (directory-exists? dest-dir)
		       (make-directory dest-dir))
		     (let-values ([(base name dir?) (split-path b)])
		       (let ([dest (build-path dest-dir name)])
			 (if (and (memq type '(mred mred3m))
				  (eq? 'macosx (system-type)))
			     (begin
			       (copy-app b dest)
			       (app-to-file dest))
			     (begin
			      (copy-file* b dest)
			      dest))))))
		 binaries
		 sub-dirs
		 types)]
	   [single-mac-app? (and (eq? 'macosx (system-type))
				 (= 1 (length types))
				 (memq (car types) '(mred mred3m)))])
      ;; Create directories for libs and collects:
      (let-values ([(lib-dir collects-dir relative-collects-dir)
		    (if single-mac-app?
			;; Special case: single Mac OS X MrEd app:
			(let-values ([(base name dir?)
				      (split-path (car binaries))])
			  (values
			   (simplify-path (build-path base 'up "Frameworks"))
			   (if collects-path
			       (build-path dest-dir collects-path)
			       (simplify-path (build-path base 
							  'up
							  "Resources"
							  "collects")))
			   (if collects-path
			       (build-path 'up 'up 'up collects-path)
			       (build-path 'up "Resources" "collects"))))
			;; General case:
			(let ([relative-collects-dir 
			       (or collects-path
				   (build-path "lib"
					       "plt"
					       (let-values ([(base name dir?) 
							     (split-path (car binaries))])
						 (path-replace-suffix name #""))
					       "collects"))])
			  (values (build-path dest-dir "lib")
				  (build-path dest-dir relative-collects-dir)
				  relative-collects-dir)))])
	(make-directory* lib-dir)
	(make-directory* collects-dir)
	;; Copy libs into place
	(install-libs lib-dir types)
	;; Copy collections into place
	(for-each (lambda (dir)
		    (for-each (lambda (f)
				(copy-directory/files*
				 (build-path dir f)
				 (build-path collects-dir f)))
			      (directory-list dir)))
		  copy-collects)
	;; Patch binaries to find libs
	(patch-binaries binaries types)
	;; Patch binaries to find collects
	(for-each (lambda (b type sub-dir)
		    (set-collects-path 
		     b 
		     (collects-path->bytes 
		      (cond
		       [sub-dir
			(build-path 'up relative-collects-dir)]
		       [(and (eq? 'macosx (system-type))
			     (memq type '(mred mredx))
			     (not single-mac-app?))
			(build-path 'up 'up 'up relative-collects-dir)]
		       [else
			relative-collects-dir]))))
		  binaries types sub-dirs))
      ;; Done!
      (void)))

  (define (install-libs lib-dir types)
    (case (system-type)
      [(windows)
       (let ([copy-dll (lambda (name)
			 (copy-file* (search-dll (find-dll-dir) name)
				     (build-path lib-dir name)))]
	     [versionize (lambda (template)
			   (let ([f (search-dll (find-dll-dir)
						(format template filename-version-part))])
			     (if (file-exists? f)
				 (format template filename-version-part)
				 (format template "xxxxxxx"))))])
	 (map copy-dll
	      (list
	       "iconv.dll"
	       "UnicoWS.dll"))
	 (when (or (memq 'mzscheme types)
		   (memq 'mred types))
	   (map copy-dll
		(list
		 (versionize "libmzsch~a.dll")
		 (versionize "libmzgc~a.dll"))))
	 (when (or (memq 'mzscheme3m types)
		   (memq 'mred3m types))
	   (map copy-dll
		(list
		 (versionize "libmzsch3m~a.dll"))))
	 (when (memq 'mred types)
	   (map copy-dll
		(list
		 (versionize "libmred~a.dll"))))
	 (when (memq 'mred3m types)
	   (map copy-dll
		(list
		 (versionize "libmred3m~a.dll")))))]
      [(macosx)
       (when (memq 'mzscheme types)
	 (copy-framework "MzScheme" #f lib-dir))
       (when (memq 'mzscheme3m types)
	 (copy-framework "MzScheme" #t lib-dir))
       (when (memq 'mred types)
	 (copy-framework "MrEd" #f lib-dir))
       (when (memq 'mred3m types)
	 (copy-framework "MrEd" #t lib-dir))]
      [(unix)
       (let ([lib-plt-dir (build-path lib-dir "plt")])
	 (unless (directory-exists? lib-plt-dir)
	   (make-directory lib-plt-dir))
	 (let ([copy-bin
		(lambda (name)
		  (copy-file* (build-path (find-console-bin-dir) name)
			      (build-path lib-plt-dir 
					  (format "~a-~a" name (version)))))])
	   (when (memq 'mzscheme types)
	     (copy-bin "mzscheme"))
	   (when (memq 'mzscheme3m types)
	     (copy-bin "mzscheme3m"))
	   (when (memq 'mred types)
	     (copy-bin "mred"))
	   (when (memq 'mred3m types)
	     (copy-bin "mred3m")))
	 (when (shared-libraries?)
	   (when (or (memq 'mzscheme types)
		     (memq 'mred types))
	     (copy-shared-lib "mzscheme" lib-dir)
	     (copy-shared-lib "mzgc" lib-dir))
	   (when (or (memq 'mzscheme3m types)
		     (memq 'mred3m types))
	     (copy-shared-lib "mzscheme3m" lib-dir))
	   (when (memq 'mred types)
	     (copy-shared-lib "mred" lib-dir))
	   (when (memq 'mred types)
	     (copy-shared-lib "mred3m" lib-dir))))]))

  (define (search-dll dll-dir dll)
    (if dll-dir
	(build-path dll-dir dll)
	(let* ([exe-dir
		(let ([exec (path->complete-path 
			     (find-executable-path (find-system-path 'exec-file))
			     (find-system-path 'orig-dir))])
		  (let-values ([(base name dir?) (split-path exec)])
		    base))]
	       [paths (cons
		       exe-dir
		       (path-list-string->path-list
			(or (getenv "PATH") "")
			(list (find-system-path 'sys-dir))))])
	  (or (ormap (lambda (p)
		       (let ([p (build-path p dll)])
			 (and (file-exists? p)
			      p)))
		     paths)
	      ;; Can't find it, so just use executable's dir:
	      (build-path exe-dir dll)))))

  (define (copy-framework name 3m? lib-dir)
    (let* ([fw-name (format "PLT_~a.framework" name)]
	   [sub-dir (build-path fw-name "Versions"
				(if 3m?
				    (format "~a_3m" (version))
				    (version)))])
      (make-directory* (build-path lib-dir sub-dir))
      (let* ([fw-name (build-path sub-dir (format "PLT_~a" name))]
	     [dll-dir (find-framework fw-name)])
	(copy-file* (build-path dll-dir fw-name)
		    (build-path lib-dir fw-name))
	(let ([rsrc-src (build-path dll-dir sub-dir "Resources")])
	  (when (directory-exists? rsrc-src)
	    (copy-directory/files* 
	     rsrc-src 
	     (build-path lib-dir sub-dir "Resources")))))))

  (define (find-framework fw-name)
    (let ([dll-dir (find-dll-dir)])
      (or dll-dir
	  (ormap (lambda (p)
		   (let ([f (build-path p fw-name)])
		     (and (file-exists? f)
			  p)))
		 '("/System/Library/Frameworks"
		   "/Library/Frameworks"
		   "~/Library/Frameworks"))
	  ;; Can't find it, so just use relative path:
	  (build-path 'same))))

  ;; cache:
  (define avail-lib-files #f)

  (define (copy-shared-lib name lib-dir)
    (unless avail-lib-files
      (set! avail-lib-files (directory-list (find-dll-dir))))
    (let* ([rx (byte-regexp (string->bytes/latin-1
			     (format "lib~a-~a.*[.](?:so|dylib)$" name (version))))]
	   [files (filter (lambda (f)
			    (regexp-match rx (path->bytes f)))
			  avail-lib-files)])
      (when (null? files)
	(error 'copy-shared-lib "cannot find shared library for ~a"
	       name))
      (unless (null? (cdr files))
	(error 'copy-shared-lib 
	       "found multiple shared-library candidates for ~a: ~e"
	       name
	       files))
      (copy-file* (build-path (find-dll-dir) (car files))
		  (build-path lib-dir (car files)))))

  (define (patch-binaries binaries types)
    (case (system-type)
      [(windows)
       (for-each (lambda (b)
		   (update-dll-dir b "lib"))
		 binaries)]
      [(macosx)
       (if (and (= 1 (length types))
		(memq (car types) '(mred mred3m)))
	   ;; Special case for single MrEd app:
	   (update-framework-path "@executable_path/../Frameworks/"
				  (car binaries)
				  #t)
	   ;; General case:
	   (for-each (lambda (b type)
		       (update-framework-path (if (memq type '(mzscheme mzscheme3m))
						  "@executable_path/../lib/" 
						  "@executable_path/../../../lib/" )
					      b
					      (memq type '(mred mred3m))))
		     binaries types))]
      [(unix)
       (for-each (lambda (b type)
		   (patch-stub-exe-paths b
					 (build-path 
					  "../lib/plt"
					  (format "~a-~a" type (version)))
					 (and (shared-libraries?)
					      "../lib")))
		 binaries
		 types)]))

  (define (patch-stub-exe-paths b exe shared-lib-dir)
    (let-values ([(config-pos start end prog-len dll-len rest)
		  (with-input-from-file b
		    (lambda ()
		      (let* ([i (current-input-port)]
			     [m (regexp-match-positions #rx#"cOnFiG:" i)])
			(unless m
			  (error 'patch-stub-exe-paths
				 "cannot find config info"))
			(read-byte i)
			(read-one-int i) ; start of prog data
			(let ([start (read-one-int i)]
			      [end (read-one-int i)])
			  (file-position i start)
			  (let ([prog-len (next-bytes-length i)]
				[dll-len (next-bytes-length i)])
			    (values (+ (cdar m) 1)
				    start
				    end
				    prog-len
				    dll-len
				    (read-bytes (- (- end start) prog-len dll-len))))))))])
      (let ([exe-bytes (path->bytes (to-path exe))]
	    [shared-lib-bytes (if shared-lib-dir
				  (path->bytes (to-path shared-lib-dir))
				  #"")])
	(let ([delta (- (+ prog-len dll-len)
			(bytes-length exe-bytes)
			(bytes-length shared-lib-bytes))])
	  (with-output-to-file b
	    (lambda ()
	      (let ([o (current-output-port)])
		(file-position o (+ config-pos 8))
		(write-one-int (+ end delta) o)
		(flush-output o)
		(file-position o start)
		(write-bytes exe-bytes o)
		(write-bytes #"\0" o)
		(write-bytes shared-lib-bytes o)
		(write-bytes #"\0" o)
		(write-bytes rest o)
		(flush-output o)))
	    'update)))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Utilities

  (define (shared-libraries?)
    (eq? 'shared (system-type 'link)))

  (define (to-path s)
    (if (string? s)
	(string->path s)
	s))

  (define (get-binary-type b)
    ;; Since this is called first, we also check that the executable
    ;; is a stub binary for Unix.
    (with-input-from-file (app-to-file b)
      (lambda ()
	(let ([m (regexp-match #rx#"bINARy tYPe:(e?)(.)(.)(.)" (current-input-port))])
	  (if m
	      (begin
		(when (eq? 'unix (system-type))
		  (unless (equal? (cadr m) #"e")
		    (error 'assemble-distribution
			   "file is an original PLT executable, not a stub binary: ~e"
			   b)))
		(let ([3m? (equal? (list-ref m 4) #"3")])
		  (if (equal? (caddr m) #"r")
		      (if 3m?
			  'mred3m
			  'mred)
		      (if 3m?
			  'mzscheme3m
			  'mzscheme))))
	      (error 'assemble-distribution
		     "file is not a PLT executable: ~e"
		     b))))))

  (define (write-one-int n out)
    (write-bytes (integer->integer-bytes n 4 #t #f) out))

  (define (read-one-int in)
    (integer-bytes->integer (read-bytes 4 in) #t #f))

  (define (next-bytes-length in)
    (let ([m (regexp-match-positions #rx#"\0" in)])
      (cdar m)))

  (define (copy-file* src dest)
    (when (or (file-exists? dest)
	      (link-exists? dest))
      (delete-file dest))
    (copy-file src dest)
    (let ([t (file-or-directory-modify-seconds src)])
      (file-or-directory-modify-seconds dest t)))

  (define (copy-directory/files* src dest)
    (cond
     [(directory-exists? src)
      (unless (directory-exists? dest)
	(make-directory dest))
      (for-each (lambda (f)
		  (copy-directory/files* (build-path src f)
					 (build-path dest f)))
		(directory-list src))]
     [else
      (copy-file* src dest)]))

  (define (copy-app src dest)
    (when (or (file-exists? dest)
	      (directory-exists? dest)
	      (link-exists? dest))
      (delete-directory/files dest))
    (copy-directory/files src dest))

  (define (app-to-file b)
    (if (and (eq? 'macosx (system-type))
	     (regexp-match #rx#"[.][aA][pP][pP]$" 
			   (path->bytes (if (string? b)
					    (string->path b)
					    b))))
	(let ([no-app
	       (let-values ([(base name dir?) (split-path b)])
		 (path-replace-suffix name #""))])
	  (build-path b "Contents" "MacOS" no-app))
	b)))
