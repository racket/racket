
(module setup-extension mzscheme
  (require (lib "make.ss" "make")
	   (lib "link.ss" "dynext")
	   (lib "compile.ss" "dynext")
	   (lib "file.ss" "dynext")
	   (lib "file.ss")
	   (lib "list.ss")
	   (lib "process.ss")
	   (rename (lib "plthome.ss" "setup") plthome* plthome))

  (provide pre-install
	   with-new-flags)

  ;; Syntax used to add a command-line flag:
  (define-syntax with-new-flags
    (syntax-rules ()
      [(_ param flags body0 body ...)
       (parameterize ([param (append
			      (param)
			      flags)])
	 body0 body ...)]))

  (define (extract-base-filename file.c)
    (let-values ([(base name dir?) (split-path (extract-base-filename/c file.c 'pre-install))])
      name))

  (define (string-path->string s)
    (if (string? s) s (path->string s)))

  (define (pre-install plthome collection-dir file.c . rest)
    (let* ([base-dir (build-path collection-dir
				 "precompiled"
				 "native"
				 (system-library-subpath #f))]
	   [variant-dir (case (link-variant)
			  [(3m) (build-path base-dir "3m")]
			  [else base-dir])]
	   [base-file (extract-base-filename file.c)]
	   [file.so (build-path variant-dir (append-extension-suffix base-file))])
      (if (file-exists? file.so)
	  ;; Just copy pre-compiled file:
	  (let* ([base-dir (build-path collection-dir
				       "compiled"
				       "native"
				       (system-library-subpath #f))]
		 [variant-dir (case (link-variant)
				[(3m) (build-path base-dir "3m")]
				[else base-dir])]
		 [dest-file.so (build-path variant-dir (append-extension-suffix base-file))])
	    (make-directory* variant-dir)
	    (printf "  Copying ~a~n       to ~a~n" file.so dest-file.so)
	    (when (file-exists? dest-file.so) 
	      (delete-file dest-file.so))
	    (copy-file file.so dest-file.so))
	  ;; Normal build...
	  (apply pre-install/normal plthome collection-dir file.c rest))))

  (define (pre-install/normal plthome
			      collection-dir
			      file.c
			      default-lib-dir
			      include-subdirs
			      find-unix-libs
			      find-windows-libs
			      unix-libs
			      windows-libs
			      extra-depends
			      last-chance-k)
    (parameterize ([current-directory collection-dir])
      (define mach-id (string->symbol (path->string (system-library-subpath #f))))
      (define is-win? (eq? mach-id 'win32\\i386))

      ;; We look for libraries and includes in the 
      ;;  following places:
      (define search-path
	(append
	 (let ([v (getenv "PLT_EXTENSION_LIB_PATHS")])
	   (if v 
	       (path-list-string->path-list v (list default-lib-dir))
	       (list default-lib-dir)))
	 (list "/usr"
	       "/usr/local"
	       "/usr/local/gnu"
	       ;; OS X fink location:
	       "/sw"
	       ;; Hack for NU PLT's convenience:
	       "/arch/gnu/packages/readline-4.2")))
      
      (define sys-path
	(ormap (lambda (x)
		 (and (andmap
		       (lambda (sub)
			 (directory-exists? (build-path x "include" sub)))
		       include-subdirs)
		      (andmap (lambda (lib)
				(ormap (lambda (suffix)
					 (file-exists? 
					  (build-path x 
						      "lib"
						      (format "~a~a.~a" 
							      (if is-win? 
								  ""
								  "lib")
							      lib 
							      suffix))))
				       '("a" "so" "dylib" "lib")))
			      (if is-win?
				  find-windows-libs
				  find-unix-libs))
		      (if (string? x)
			  (string->path x)
			  x)))
	       search-path))

      (unless sys-path
	(error 'extension-installer
	       "can't find needed include files and/or library; try setting the environment variable PLT_EXTENSION_LIB_PATHS"))

      (parameterize ([make-print-checking #f])

	;; Used as make dependencies:
	(define mz-inc-dir (build-path plthome* "include"))
	(define headers (map (lambda (name)
			       (build-path mz-inc-dir name))
			     '("scheme.h" "schvers.h" "schemef.h" "sconfig.h" "stypes.h")))
	
	(define dir (let ([std (build-path "compiled" "native" (system-library-subpath #f))])
		      (case (link-variant)
			[(3m) (build-path std "3m")]
			[else std])))
	(define base-file (extract-base-filename file.c))
	(define file.so (build-path dir (append-extension-suffix base-file)))
	(define file.o (build-path dir (append-object-suffix base-file)))

	(with-new-flags 
	 current-extension-compiler-flags
	 (list (format "-I~a" (path->string (build-path sys-path "include"))))
	  
	 ;; Add -L and -l for Unix:
	 (with-new-flags
	  current-extension-linker-flags 
	  (if is-win?
	      null
	      (list (format "-L~a/lib" (path->string sys-path))))
	  
	  ;; Add libs for Windows:
	  (with-new-flags
	   current-standard-link-libraries
	   (if is-win?
	       (append (map 
			(lambda (l)
			  (build-path sys-path "lib" (format "~a.lib" l)))
			find-windows-libs)
		       windows-libs)
	       null)

	   ;; Extra stuff:
	   (with-new-flags
	    current-extension-linker-flags 
	    (case mach-id
	      [(rs6k-aix) (list "-lc")]
	      [else null])
	    
	    (define (delete/continue x)
	      (with-handlers ([(lambda (x) #t) void])
		(delete-file x)))
	    
	    (make-directory* dir)
	    
	    (last-chance-k
	     (lambda ()
	       (make/proc
		(list (list file.so 
			    (list file.o)
			    (lambda ()
			      (link-extension #f (append (list file.o) 
							 (if is-win?
							     null
							     (map (lambda (l)
								    (string-append "-l" (string-path->string l)))
								  (append find-unix-libs unix-libs))))
					      file.so)))
		      
		      (list file.o 
			    (append (list file.c)
				    (filter (lambda (x)
					      (regexp-match #rx#"mzdyn[a-z0-9]*[.]o" 
							    (if (string? x)
								x
								(path->string x))))
					    (expand-for-link-variant (current-standard-link-libraries)))
				    headers
				    extra-depends)
			    (lambda ()
			      (compile-extension #f file.c file.o ()))))
		#())))))))))))
