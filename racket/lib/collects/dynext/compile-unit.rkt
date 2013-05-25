(module compile-unit mzscheme
  (require mzlib/unit
           mzlib/process
           mzlib/sendevent
           "private/dirs.rkt"
           "private/stdio.rkt"
           "private/cmdargs.rkt")

  (require "compile-sig.rkt")

  (provide dynext:compile@)

  (define-unit dynext:compile@
      (import)
      (export dynext:compile^)
      
      (define (get-unix-compile)
	(or (find-executable-path "gcc" #f)
	    (find-executable-path "cc" #f)))
      
      (define (get-windows-compile)
	(or (find-executable-path "cl.exe" #f)
	    (find-executable-path "gcc.exe" #f)
	    (find-executable-path "bcc32.exe" #f)))
      
      (define current-extension-compiler 
	(make-parameter 
	 (or (let ([p (or (getenv "MZSCHEME_DYNEXT_COMPILER")
			  (getenv "CC"))])
	       (and p
		    (if (absolute-path? p)
			(string->path p)
			(find-executable-path p #f))))
	     (case (system-type) 
	       [(unix macosx) (get-unix-compile)]
	       [(windows) (get-windows-compile)]
	       [else #f]))
	 (lambda (v)
	   (when v 
	     (if (path-string? v)
		 (unless (and (file-exists? v)
			      (memq 'execute (file-or-directory-permissions v)))
		   (error 'current-extension-compiler 
			  "compiler not found or not executable: ~s" v))
		 (raise-type-error 'current-extension-compiler "path, valid-path string, or #f" v)))
	   v)))

      (define win-gcc?
	(let ([c (current-extension-compiler)])
	  (and c (regexp-match #"gcc.exe$" (path->bytes c)))))
      (define win-borland?
	(let ([c (current-extension-compiler)])
	  (and c (regexp-match #"bcc32.exe$" (path->bytes c)))))
      (define unix-cc?
	(let ([c (current-extension-compiler)])
	  (and c (regexp-match #"[^g]cc$" (path->bytes c)))))

      (define (add-variant-flags l)
	(append l (list (lambda ()
			  (if (eq? '3m (specific-compile-variant))
			      '("-DMZ_PRECISE_GC")
			      null)))))

      (define gcc-cpp-flags 
	(add-variant-flags (case (string->symbol (path->string (system-library-subpath #f)))
			     [(parisc-hpux) '("-D_HPUX_SOURCE")]
			     [(ppc-macosx x86_64-macosx) '("-DOS_X")]
			     [(i386-macosx) '("-DOS_X" "-m32")]
			     [(ppc-darwin x86_64-darwin) '("-DOS_X" "-DXONX")]
			     [(i386-darwin) '("-DOS_X" "-DXONX" "-m32")]
			     [else null])))

      (define gcc-compile-flags (append '("-c" "-O2" "-fPIC")
					(case (string->symbol (path->string (system-library-subpath #f)))
					  [(i386-macosx i386-darwin) '("-m32" "-fno-common")]
					  [(ppc-macosx ppc-darwin x86_64-macosx x86_64-darwin) '("-fno-common")]
					  [(win32\\i386) '("-DAS_MSVC_EXTENSION")]
					  [else null])
					gcc-cpp-flags))

      (define unix-cpp-flags 
	(add-variant-flags (case (string->symbol (path->string (system-library-subpath #f)))
			     [(parisc-hpux) '("-D_HPUX_SOURCE")]
			     [else gcc-cpp-flags])))

      (define unix-compile-flags (case (string->symbol (path->string (system-library-subpath #f)))
				   [(parisc-hpux) (append '("-c" "-O2" "-Aa" "+z" "+e")
							  unix-cpp-flags)]
				   [else gcc-compile-flags]))

      (define msvc-compile-flags 
	(add-variant-flags '("/c" "/MT" "/O2")))

      (define (make-flags-guard who)
	(lambda (l)
	  (unless (and (list? l) (andmap (lambda (s) (or (path-string? s)
							 (and (procedure? s) (procedure-arity-includes? s 0))))
					 l))
	    (raise-type-error who "list of paths/strings and thunks" l))
	  l))

      (define (get-env-compile-flags)
	(let ([v (or (getenv "MZSCHEME_DYNEXT_COMPILER_FLAGS")
		     (getenv "CFLAGS"))])
	  (if v
	      (split-command-line-args v)
	      null)))

      (define current-extension-compiler-flags
	(make-parameter
	 (append 
	  (get-env-compile-flags)
	  (case (system-type)
	    [(unix macosx) (if unix-cc?
			       unix-compile-flags
			       gcc-compile-flags)]
	    [(windows) (if (or win-gcc? win-borland?)
			   gcc-compile-flags
			   msvc-compile-flags)]
	    [(macos) '()]))
	 (make-flags-guard 'current-extension-compiler-flags)))

      (define current-extension-preprocess-flags
	(make-parameter
	 (case (system-type)
	   [(unix macosx) (cons "-E" (if unix-cc?
					 unix-cpp-flags
					 gcc-cpp-flags))]
	   [(windows) (if (or win-gcc? win-borland?)
			  (cons "-E" gcc-cpp-flags)
			  '("/E"))]
	   [(macos) '()])
	 (make-flags-guard 'current-extension-preprocess-flags)))

      (define compile-variant (make-parameter 
			       'normal
			       (lambda (s)
				 (unless (memq s '(normal cgc 3m))
				   (raise-type-error 'compile-variant "'normal, 'cgc, or '3m" s))
				 s)))

      (define (specific-compile-variant)
        (let ([v (compile-variant)])
          (if (eq? v 'normal)
              (system-type 'gc)
              v)))

      (define (expand-for-compile-variant l)
	(apply append (map (lambda (s) (if (path-string? s) (list s) (s))) l)))

      (define current-make-extra-extension-compiler-flags
	(make-parameter
	 (lambda () (case (specific-compile-variant)
		      [(3m) '("-DMZ_PRECISE_GC")]
		      [else null]))
	 (lambda (p)
	   (unless (and (procedure? p) (procedure-arity-includes? p 0))
	     (raise-type-error 'current-make-extra-extension-compiler-flags "procedure (arity 0)" p))
	   p)))
      
      (define (path-string->string s)
	(if (string? s) s (path->string s)))

      (define unix-compile-include-strings (lambda (s) 
					     (list (string-append "-I" (path-string->string s)))))
      (define msvc-compile-include-strings (lambda (s) 
					     (list (string-append "/I" (path-string->string s)))))

      (define current-make-compile-include-strings
	(make-parameter
	 (case (system-type)
	   [(unix macosx) unix-compile-include-strings]
	   [(windows) (if (or win-gcc? win-borland?)
			  unix-compile-include-strings
			  msvc-compile-include-strings)]
	   [(macos) unix-compile-include-strings])
	 (lambda (p)
	   (unless (procedure-arity-includes? p 1)
	     (raise-type-error 'current-make-compile-include-strings "procedure of arity 1" p))
	   p)))
      
      (define current-make-compile-input-strings
	(make-parameter
	 (lambda (s) (list (path-string->string s)))
	 (lambda (p)
	   (unless (procedure-arity-includes? p 1)
	     (raise-type-error 'current-make-compile-input-strings "procedure of arity 1" p))
	   p)))
      
      (define unix-compile-output-strings (lambda (s) (list "-o" (path-string->string s))))
      (define msvc-compile-output-strings (lambda (s) (list (string-append "/Fo" (path-string->string s)))))

      (define current-make-compile-output-strings
	(make-parameter
	 (case (system-type)
	   [(unix macosx) unix-compile-output-strings]
	   [(windows) (if (or win-gcc? win-borland?)
			  unix-compile-output-strings
			  msvc-compile-output-strings)]
	   [(macos) unix-compile-output-strings])
	 (lambda (p)
	   (unless (procedure-arity-includes? p 1)
	     (raise-type-error 'current-make-compile-output-strings "procedure of arity 1" p))
	   p)))
      
      (define (get-standard-compilers)
	(case (system-type)
	  [(unix macosx) '(gcc cc)]
	  [(windows) '(gcc msvc borland)]
	  [(macos) '(cw)]))

      (define (use-standard-compiler name)
	(define (bad-name name)
	  (error 'use-standard-compiler "unknown compiler: ~a" name))
	(case (system-type)
	  [(unix macosx) 
	   (case name
	     [(cc gcc) (let* ([n (if (eq? name 'gcc) "gcc" "cc")]
			      [f (find-executable-path n n)])
			 (unless f
			   (error 'use-standard-compiler "cannot find ~a" n))
			 (current-extension-compiler f))
	      (current-extension-compiler-flags (add-variant-flags
						 (if (eq? name 'gcc)
						     gcc-compile-flags
						     unix-compile-flags)))
	      (current-make-compile-include-strings unix-compile-include-strings)
	      (current-make-compile-input-strings (lambda (s) (list (path-string->string s))))
	      (current-make-compile-output-strings unix-compile-output-strings)]
	     [else (bad-name name)])]
	  [(windows) 
	   (case name
	     [(gcc) (let ([f (find-executable-path "gcc.exe" #f)])
		      (unless f
			(error 'use-standard-compiler "cannot find gcc.exe"))
		      (current-extension-compiler f))
	      (current-extension-compiler-flags (add-variant-flags gcc-compile-flags))
	      (current-make-compile-include-strings unix-compile-include-strings)
	      (current-make-compile-input-strings (lambda (s) (list (path-string->string s))))
	      (current-make-compile-output-strings unix-compile-output-strings)]
	     [(borland) (let ([f (find-executable-path "bcc32.exe" #f)])
			  (unless f
			    (error 'use-standard-compiler "cannot find bcc32.exe"))
			  (current-extension-compiler f))
	      (current-extension-compiler-flags (add-variant-flags gcc-compile-flags))
	      (current-make-compile-include-strings unix-compile-include-strings)
	      (current-make-compile-input-strings (lambda (s) (list (path-string->string s))))
	      (current-make-compile-output-strings unix-compile-output-strings)]
	     [(msvc) (let ([f (find-executable-path "cl.exe" #f)])
		       (unless f
			 (error 'use-standard-compiler "cannot find MSVC's cl.exe"))
		       (current-extension-compiler f))
	      (current-extension-compiler-flags (add-variant-flags msvc-compile-flags))
	      (current-make-compile-include-strings msvc-compile-include-strings)
	      (current-make-compile-input-strings (lambda (s) (list (path-string->string s))))
	      (current-make-compile-output-strings msvc-compile-output-strings)]
	     [else (bad-name name)])]
	  [(macos) 
	   (case name
	     [(cw) (current-extension-compiler #f)
	      (current-extension-compiler-flags (add-variant-flags unix-compile-flags))
	      (current-make-compile-include-strings unix-compile-include-strings)
	      (current-make-compile-input-strings (lambda (s) (list (path-string->string s))))
	      (current-make-compile-output-strings unix-compile-output-strings)]
	     [else (bad-name name)])]))
      
      (define-values (my-process* stdio-compile)
	(let-values ([(p* do-stdio) (get-stdio)])
	  (values
	   p*
	   (lambda (start-process quiet?)
	     (do-stdio start-process quiet? (lambda (s) (error 'compile-extension "~a" s)))))))
      
      (define (make-compile-extension current-extension-compiler-flags)
	(lambda (quiet? in out includes)
	  (let ([c (current-extension-compiler)])
	    (if c
		(stdio-compile (lambda (quiet?) 
				 (let ([command (append 
						 (list c)
						 (expand-for-compile-variant
						  (current-extension-compiler-flags))
						 (apply append 
							(map 
							 (lambda (s) 
							   ((current-make-compile-include-strings) s)) 
							 includes))
						 ((current-make-compile-include-strings) (include-dir))
						 ((current-make-compile-input-strings) in)
						 ((current-make-compile-output-strings) out))])
				   (unless quiet? 
				     (printf "compile-extension: ~a\n" command))
				   (apply my-process* command)))
			       quiet?)
		(error 'compile-extension "can't find an installed C compiler")))))

      (define compile-extension (make-compile-extension
				 current-extension-compiler-flags))
      (define preprocess-extension (make-compile-extension
				    current-extension-compiler-flags))))

