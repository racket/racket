
(module embed-unit mzscheme
  (require (lib "unitsig.ss")
	   (lib "file.ss")
	   (lib "list.ss")
	   (lib "etc.ss")
	   (lib "port.ss")
	   (lib "moddep.ss" "syntax")
	   (lib "plist.ss" "xml")
	   (lib "process.ss")
	   "embed-sig.ss"
	   "private/winicon.ss"
           "private/winsubsys.ss")

  (provide compiler:embed@)

  (define compiler:embed@
    (unit/sig compiler:embed^
      (import)

      (define (embedding-executable-is-directory? mred?)
	#f)
      
      (define (embedding-executable-is-actually-directory? mred?)
	(and mred? (eq? 'macosx (system-type))))
      
      (define (embedding-executable-put-file-extension+style+filters mred?)
	(case (system-type)
	  [(windows) (values "exe" null '(("Executable" "*.exe")))]
	  [(macosx) (if mred?
			(values "app" '(enter-packages) '(("App" "*.app")))
			(values #f null null))]
	  [else (values #f null null)]))

      (define (embedding-executable-add-suffix path mred?)
	(let* ([path (if (string? path)
			 (string->path path)
			 path)]
	       [fixup (lambda (re sfx)
			(if (regexp-match re (path->bytes path))
			    path
			    (path-replace-suffix path sfx)))])
	  (case (system-type)
	    [(windows) (fixup #rx#"[.][eE][xX][eE]$" #".exe")]
	    [(macosx) (if mred?
			  (fixup #rx#"[.][aA][pP][pP]$" #".app")
			  path)]
	    [else path])))
      
      ;; Find executable relative to the "mzlib"
      ;; collection.
      (define (find-exe mred? variant)
	(let* ([c-path (collection-path "mzlib")]
	       [base (build-path c-path 'up 'up)]
	       [fail
		(lambda ()
		  (error 'make-embedding-executable
			 "can't find ~a executable"
			 (if mred? "MrEd" "MzScheme")))]
	       [variant-suffix (case variant
				 [(normal) ""]
				 [(3m) "3m"])])
	  (let ([exe (build-path
		      base
		      (case (system-type)
			[(macosx)
			 (cond
			  [(not mred?)
			   ;; Need MzScheme:
			   (build-path "bin" (string-append 
					      "mzscheme"
					      variant-suffix))]
			  [mred?
			   ;; Need MrEd:
			   (build-path (format "MrEd~a.app" variant-suffix)
				       "Contents" "MacOS" 
				       (format "MrEd~a" variant-suffix))])]
			[(windows)
			 (format "~a~a.exe" (if mred?
						"mred"
						"mzscheme")
				 variant-suffix)]
			[(unix)
			 (build-path "bin"
				     (format "~a~a" (if mred?
							"mred"
							"mzscheme")
					     variant-suffix))]
			[(macos)
			 (format "~a~a" (if mred?
					    "MrEd"
					    "MzScheme")
				 variant-suffix)]))])
	    (unless (or (file-exists? exe)
			(directory-exists? exe))
	      (fail))
	    exe)))

      ;; Find the magic point in the binary:
      (define (find-cmdline what rx)
	(let ([m (regexp-match-positions rx (current-input-port))])
	  (if m
	      (caar m)
	      (error 
	       'make-embedding-executable
	       (format
		"can't find ~a position in executable"
		what)))))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (prepare-macosx-mred exec-name dest aux variant)
	(let* ([name (let-values ([(base name dir?) (split-path dest)])
		       (path-replace-suffix name #""))]
	       [variant-suffix (case variant
				 [(normal) ""]
				 [(3m) "3m"])]
	       [src (build-path (collection-path "launcher")
				(format "Starter~a.app" variant-suffix))]
	       [creator (let ([c (assq 'creator aux)])
			  (or (and c
				   (cdr c))
			      "MrSt"))]
	       [file-types (let ([m (assq 'file-types aux)])
			     (and m
				  (pair? (cdr m))
				  (cdr m)))]
	       [resource-files (let ([m (assq 'resource-files aux)])
				 (and m
				      (cdr m)))])
	  (when creator
	    (unless (and (string? creator) (= 4 (string-length creator)))
	      (error 'make-executable "creator is not a 4-character string: ~e" creator)))
	  (when file-types
	    (unless (and (list? file-types)
			 (andmap list? file-types)
			 (andmap (lambda (spec)
				   (andmap (lambda (p)
					     (and (list? p)
						  (= 2 (length p))
						  (string? (car p))))
					   spec))
				 file-types))
	      (error 'make-executable "bad file-types spec: ~e" file-types)))
	  (when resource-files
	    (unless (and (list? resource-files)
			 (andmap path-string?
				 resource-files))
	      (error 'make-executable "resource-files is not a list of paths: ~e" resource-files)))

	  (when (or (directory-exists? dest)
		    (file-exists? dest)
		    (link-exists? dest))
	    (delete-directory/files dest))
	  (make-directory* (build-path dest "Contents" "Resources"))
	  (make-directory* (build-path dest "Contents" "MacOS"))
	  (copy-file exec-name (build-path dest "Contents" "MacOS" name))
	  (copy-file (build-path src "Contents" "PkgInfo")
		     (build-path dest "Contents" "PkgInfo"))
	  (let ([icon (or (let ([icon (assq 'icns aux)])
			    (and icon
				 (cdr icon)))
			  (build-path src "Contents" "Resources" 
				      (format "Starter~a.icns" variant-suffix)))])
	    (copy-file icon
		       (build-path dest "Contents" "Resources" 
				   (format "Starter~a.icns" variant-suffix))))
	  (let ([orig-plist (call-with-input-file (build-path src
							      "Contents"
							      "Info.plist")
			      read-plist)]
		[plist-replace (lambda (plist . l)
				 (let loop ([plist plist][l l])
				   (if (null? l)
				       plist
				       (let ([key (car l)]
					     [val (cadr l)])
					 (loop `(dict
						 ,@(let loop ([c (cdr plist)])
						     (cond
						      [(null? c) (list (list 'assoc-pair key val))]
						      [(string=? (cadar c) key)
						       (cons (list 'assoc-pair key val)
							     (cdr c))]
						      [else
						       (cons (car c)
							     (loop (cdr c)))])))
					       (cddr l))))))])
	    (let* ([new-plist (plist-replace
			       orig-plist
			       
			       "CFBundleExecutable" 
			       (path->string name)
			       
			       "CFBundleSignature"
			       creator
			       
			       "CFBundleIdentifier" 
			       (format "org.plt-scheme.~a" (path->string name)))]
		   [new-plist (if file-types
				  (plist-replace
				   new-plist
				   
				   "CFBundleDocumentTypes"
				   (cons 'array
					 (map (lambda (spec)
						(cons
						 'dict
						 (map (lambda (p)
							(list
							 'assoc-pair
							 (car p)
							   (cadr p)))
						      spec)))
					      file-types)))
				  
				  new-plist)])
	      (call-with-output-file (build-path dest 
						 "Contents" 
						 "Info.plist")
		(lambda (port)
		  (write-plist new-plist port))
		'truncate)))
	  (call-with-output-file (build-path dest 
					     "Contents" 
					     "PkgInfo")
	    (lambda (port)
	      (fprintf port "APPL~a" creator))
	    'truncate)
	  (when resource-files
	    (for-each (lambda (p)
			(let-values ([(base name dir?) (split-path p)])
			  (copy-file p (build-path dest
						   "Contents" 
						   "Resources"
						   name))))
		      resource-files))
	  (build-path dest "Contents" "MacOS" name)))

      (define (finish-osx-mred dest flags exec-name keep-exe?)
	(call-with-output-file (build-path dest 
					   "Contents" 
					   "Resources" 
					   "starter-info")
	  (lambda (port)
	    (write-plist 
	     `(dict ,@(if keep-exe?
			  `((assoc-pair "executable name"
					,(path->string exec-name)))
			  null)
		    (assoc-pair "stored arguments"
				(array ,@flags)))
	     port))
	  'truncate))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; Represent modules with lists starting with the filename, so we
      ;; can use assoc:
      (define (make-mod normal-file-path normal-module-path code name prefix full-name relative-mappings)
	(list normal-file-path normal-module-path code
	      name prefix full-name relative-mappings))

      (define (mod-file m) (car m))
      (define (mod-mod-path m) (cadr m))
      (define (mod-code m) (caddr m))
      (define (mod-name m) (list-ref m 3))
      (define (mod-prefix m) (list-ref m 4))
      (define (mod-full-name m) (list-ref m 5))
      (define (mod-mappings m) (list-ref m 6))
      
      (define (generate-prefix)
	(format "#%embedded:~a:" (gensym)))
      
      (define (normalize filename)
	(simplify-path (expand-path filename)))

      ;; Loads module code, using .zo if there, compiling from .scm if not
      (define (get-code filename module-path codes prefixes verbose?)
	(when verbose?
	  (fprintf (current-error-port) "Getting ~s~n" filename))
	(let ([a (assoc filename (unbox codes))])
	  (if a
	      ;; Already have this module. Make sure that library-referenced
	      ;;  modules are consistently referenced through library paths:
	      (let ([found-lib? (and (pair? (mod-mod-path a))
				     (eq? 'lib (car (mod-mod-path a))))]
		    [look-lib? (and (pair? module-path)
				    (eq? 'lib (car module-path)))])
		(cond
		 [(and found-lib? look-lib?)
		  'ok]
		 [(or found-lib? look-lib?)
		  (error 'find-module
			 "module referenced both as a library and through a path: ~a"
			 filename)]
		 [else 'ok]))
	      ;; First use of the module. Get code and then get code for imports.
	      (let ([code (get-module-code filename)])
		(let-values ([(imports fs-imports ft-imports) (module-compiled-imports code)])
		  (let ([name (let-values ([(base name dir?) (split-path filename)])
				(path->string (path-replace-suffix name #"")))]
			[prefix (let ([a (assoc filename prefixes)])
				  (if a
				      (cdr a)
				      (generate-prefix)))]
			[all-file-imports (filter (lambda (x) (not (symbol? x)))
						  (append imports fs-imports ft-imports))])
		    (let ([sub-files (map (lambda (i) (normalize (resolve-module-path-index i filename)))
					  all-file-imports)]
			  [sub-paths (map (lambda (i) (collapse-module-path-index i module-path))
					  all-file-imports)])
		      ;; Get code for imports:
		      (for-each (lambda (sub-filename sub-path)
				  (get-code sub-filename
					    sub-path
					    codes
					    prefixes
					    verbose?))
				sub-files sub-paths)
		      ;; Build up relative module resolutions, relative to this one,
		      ;; that will be requested at run-time.
		      (let ([mappings (map (lambda (sub-i sub-filename)
					     (let-values ([(path base) (module-path-index-split sub-i)])
					       ;; Assert: base should refer to this module:
					       (let-values ([(path2 base2) (module-path-index-split base)])
						 (when (or path2 base2)
						   (error 'embed "unexpected nested module path index")))
					       (let ([m (assoc sub-filename (unbox codes))])
						 (cons path (mod-full-name m)))))
					   all-file-imports sub-files)])
			;; Record the module
			(set-box! codes
				  (cons (make-mod filename module-path code 
						  name prefix (string->symbol
							       (format "~a~a" prefix name))
						  mappings)
					(unbox codes)))))))))))
	    
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (make-module-name-resolver code-l)
	`(let ([orig (current-module-name-resolver)]
	       [ns (current-namespace)]
	       [mapping-table (quote
			       ,(map
				 (lambda (m)
				   `(,(mod-full-name m)
				     ,(mod-mappings m)))
				 code-l))]
	       [library-table (quote
			       ,(filter values
					(map (lambda (m)
					       (let ([path (mod-mod-path m)])
						 (if (and (pair? path)
							  (eq? 'lib (car path)))
						     (cons path (mod-full-name m))
						     #f)))
					     code-l)))])
	   (current-module-name-resolver
	    (lambda (name rel-to stx)
	      (if (or (not name)
		      (not (eq? (current-namespace) ns)))
		  ;; a notification,or wrong namespace
		  (orig name rel-to stx)
		  ;; Have a relative mapping?
		  (let ([a (assoc rel-to mapping-table)])
		    (if a
			(let ([a2 (assoc name (cadr a))])
			  (if a2
			      (cdr a2)
			      (error 'embedding-module-name-resolver
				     "unexpected relative mapping request: ~e in ~e"
				     name rel-to)))
			;; A library mapping that we have? 
			(let ([a3 (and (pair? name)
				       (eq? (car name) 'lib)
				       (ormap (lambda (lib-entry)
						(with-handlers ([exn:fail? (lambda (x) #f)])
						  ;; To check equality of library references,
						  ;; we have to consider relative paths in the
						  ;; filename part of the name.
						  (let loop ([a (build-path
								 (apply build-path 
									'same
									(cddar lib-entry))
								 (cadar lib-entry))]
							     [b (build-path
								 (apply build-path 
									'same
									(let ([d (cddr name)])
									  (if (null? d)
									      '("mzlib")
									      d)))
								 (cadr name))])
						    (if (equal? a b)
							lib-entry
							(let-values ([(abase aname d?) (split-path a)])
							  (if (eq? aname 'same)
							      (loop abase b)
							      (let-values ([(bbase bname a?) (split-path b)])
								(if (eq? bname 'same)
								    (loop a bbase)
								    (if (equal? aname bname)
									(loop abase bbase)
									#f)))))))))
					      library-table))])
			  (if a3
			      ;; Have it:
			      (cdr a3)
			      ;; Let default handler try:
			      (orig name rel-to stx))))))))))

      ;; Write a module bundle that can be loaded with 'load' (do not embed it
      ;; into an executable). The bundle is written to the current output port.
      (define (write-module-bundle verbose? modules literal-files literal-expression)
	(let* ([module-paths (map cadr modules)]
	       [files (map
		       (lambda (mp)
			 (let ([f (resolve-module-path mp #f)])
			   (unless f
			     (error 'write-module-bundle "bad module path: ~e" mp))
			   (normalize f)))
		       module-paths)]
	       [collapsed-mps (map
			       (lambda (mp)
				 (collapse-module-path mp "."))
			       module-paths)]
	       [prefix-mapping (map (lambda (f m)
				      (cons f (let ([p (car m)])
						(cond
						 [(symbol? p) (symbol->string p)]
						 [(eq? p #t) (generate-prefix)]
						 [(not p) ""]
						 [else (error
							'write-module-bundle
							"bad prefix: ~e"
							p)]))))
				    files modules)]
	       ;; Each element is created with `make-mod'.
	       ;; As we descend the module tree, we append to the front after
	       ;; loasing imports, so the list in the right order.
	       [codes (box null)])
	  (for-each (lambda (f mp) (get-code f mp codes prefix-mapping verbose?))
		    files
		    collapsed-mps)
	  ;; Install a module name resolver that redirects
	  ;; to the embedded modules
	  (write (make-module-name-resolver (unbox codes)))
	  (let ([l (unbox codes)])
	    (for-each
	     (lambda (nc)
	       (when verbose?
		 (fprintf (current-error-port) "Writing module from ~s~n" (mod-file nc)))
	       (write `(current-module-name-prefix ',(string->symbol (mod-prefix nc))))
	       (write (mod-code nc)))
	     l))
	  (write '(current-module-name-prefix #f))
	  (newline)
	  (for-each (lambda (f)
		      (when verbose?
			(fprintf (current-error-port) "Copying from ~s~n" f))
		      (call-with-input-file*
		       f
		       (lambda (i)
			 (copy-port i (current-output-port)))))
		    literal-files)
	  (when literal-expression
	    (write literal-expression))))

      ;; Use `write-module-bundle', but figure out how to put it into an executable
      (define make-embedding-executable
	(opt-lambda (dest mred? verbose? 
			  modules 
			  literal-files literal-expression
			  cmdline
			  [aux null]
			  [launcher? #f]
			  [variant 'normal])
	  (define keep-exe? (and launcher?
				 (let ([m (assq 'forget-exe? aux)])
				   (or (not m)
				       (not (cdr m))))))
	  (define long-cmdline? (or (eq? (system-type) 'windows)
				    (and mred? (eq? 'macosx (system-type)))))
	  (unless (or long-cmdline?
		      ((apply + (length cmdline) (map (lambda (s)
							(bytes-length (string->bytes/utf-8 s)))
						      cmdline)) . < . 50))
	    (error 'make-embedding-executable "command line too long"))
	  (let ([exe (find-exe mred? variant)])
	    (when verbose?
	      (fprintf (current-error-port) "Copying to ~s~n" dest))
	    (let-values ([(dest-exe osx?)
			  (if (and mred? (eq? 'macosx (system-type)))
			      (values (prepare-macosx-mred exe dest aux variant) #t)
			      (begin
				(when (or (file-exists? dest)
					  (directory-exists? dest)
					  (link-exists? dest))
				  ;; Delete-file isn't enough if the target
				  ;;  is supposed to be a directory. But
				  ;;  currently, that happens only for MrEd 
				  ;;  on Mac OS X, which is handles above.
				  (delete-file dest))
				(copy-file exe dest)
				(values dest #f)))])
	      (with-handlers ([void (lambda (x)
				      (if osx?
					  (when (directory-exists? dest)
					    (delete-directory/files dest))
					  (when (file-exists? dest)
					    (delete-file dest)))
				      (raise x))])
		(let ([m (and (eq? 'macosx (system-type))
			      (assq 'framework-root aux))])
		  (when m
		    (for-each (lambda (p)
				(system* "/usr/bin/install_name_tool"
					 "-change"
					 (format "~a.framework/Versions/~a/~a" p (version) p)
					 (format "~a~a.framework/Versions/~a/~a" 
						 (cdr m)
						 p (version) p)
					 (let ([dest (if mred?
							 (let-values ([(base name dir?) (split-path dest)])
							   (build-path dest
								       "Contents" "MacOS"
								       (path-replace-suffix name #"")))
							 dest)])
					   (if (path? dest)
					       (path->string dest)
					       dest))))
			      (if mred?
				  '("PLT_MzScheme" "PLT_MrEd")
				  '("PLT_MzScheme")))))
		(let ([start (file-size dest-exe)])
		  (with-output-to-file dest-exe
		    (lambda ()
		      (write-module-bundle verbose? modules literal-files literal-expression))
		    'append)
		  (let ([end (file-size dest-exe)])
		    (when verbose?
		      (fprintf (current-error-port) "Setting command line~n"))
		    (let ([start-s (number->string start)]
			  [end-s (number->string end)])
		      (let ([full-cmdline (append
					   (if launcher?
					       (if (and (eq? 'windows (system-type))
							keep-exe?)
						   (list (path->string exe)) ; argv[0]
						   null)
					       (list "-k" start-s end-s))
					   cmdline)])
			(if osx?
			    (finish-osx-mred dest full-cmdline exe keep-exe?)
			    (let ([cmdpos (with-input-from-file dest-exe 
					    (lambda () (find-cmdline 
							"cmdline"
							#"\\[Replace me for EXE hack")))]
				  [anotherpos (and mred?
						   (eq? 'windows (system-type))
						   (let ([m (assq 'single-instance? aux)])
						     (and m (not (cdr m))))
						   (with-input-from-file dest-exe 
						     (lambda () (find-cmdline 
								 "instance-check"
								 #"yes, please check for another"))))]
				  [out (open-output-file dest-exe 'update)])
			      (dynamic-wind
				  void
				  (lambda ()
				    (when anotherpos
				      (file-position out anotherpos)
				      (write-bytes #"no," out))
				    (if long-cmdline?
					;; write cmdline at end:
					(file-position out end)
					(begin
					  ;; write (short) cmdline in the normal position:
					  (file-position out cmdpos)
					  (display "!" out)))
				    (for-each
				     (lambda (s)
				       (fprintf out "~a~a~c"
						(integer->integer-bytes 
						 (add1 (bytes-length (string->bytes/utf-8 s)) )
						 4 #t #f)
						s
						#\000))
				     full-cmdline)
				    (display "\0\0\0\0" out)
				    (when long-cmdline?
				      ;; cmdline written at the end;
				      ;; now put forwarding information at the normal cmdline pos
				      (let ([new-end (file-position out)])
					(file-position out cmdpos)
					(fprintf out "~a...~a~a"
						 (if keep-exe? "*" "?")
						 (integer->integer-bytes end 4 #t #f)
						 (integer->integer-bytes (- new-end end) 4 #t #f)))))
				  (lambda ()
				    (close-output-port out)))
			      (let ([m (and (eq? 'windows (system-type))
					    (assq 'ico aux))])
				(when m
				  (install-icon dest-exe (cdr m))))
			      (let ([m (and (eq? 'windows (system-type))
					    (assq 'subsystem aux))])
				(when m
				  (set-subsystem dest-exe (cdr m))))))))))))))))))
