(module link-unit mzscheme
  (require mzlib/unit
           mzlib/process
           mzlib/sendevent
           "private/dirs.rkt"
           "private/stdio.rkt"
           "private/cmdargs.rkt"
           "filename-version.rkt")

  (require "link-sig.rkt")

  (provide dynext:link@)

  (define-unit dynext:link@
      (import)
      (export dynext:link^)

      (define (path-string->string s)
	(if (string? s) s (path->string s)))

      ;; ---- Find a linker for this platform --------------------

      (define (get-windows-linker)
	(or (find-executable-path "cl.exe" #f)
	    (find-executable-path "ld.exe" #f)
	    (find-executable-path "ilink32.exe" #f)))

      (define (get-unix-linker)
	(let ([l (case (string->symbol (path->string (system-library-subpath #f)))
                   [(sparc-solaris i386-solaris 
                                   sparc-sunos4
                                   i386-freebsd-2.x
                                   parisc-hpux
                                   i386-cygwin)
                    '("ld")]
		   [else '("gcc" "cc")])])
          (ormap (lambda (s)
                   (find-executable-path s #f))
                 l)))
      
      (define (check-valid-linker-path v)
	(unless (and (file-exists? v)
		     (memq 'execute (file-or-directory-permissions v)))
	  (error 'current-extension-linker 
		 "linker not found or not executable: ~s" v)))

      ;; See manual:
      (define current-extension-linker 
	(make-parameter 
	 (or (let ([p (getenv "MZSCHEME_DYNEXT_LINKER")])
	       (and p
		    (if (absolute-path? p)
			(string->path p)
			(find-executable-path p #f))))
	     (case (system-type) 
	       [(unix macosx) (get-unix-linker)]
	       [(windows) (get-windows-linker)]
	       [else #f]))
	 (lambda (v)
	   (when v 
	     (if (path-string? v)
		 (check-valid-linker-path v)
		 (raise-type-error 'current-extension-linker "path, valid-path string, or #f" v)))
	   v)))

      ;; Helpers to tell us about the selected linker in Windows:
      
      (define (still-win-gcc?)
	(or (and (eq? 'windows (system-type))
		 (let ([c (current-extension-linker)])
		   (and c (regexp-match #"ld.exe$" (path-string->string c)))))
	    (and (eq? 'unix (system-type))
		 (string=? "i386-cygwin"
			   (path->string (system-library-subpath #f))))))
      (define (still-win-borland?)
	(and (eq? 'windows (system-type))
	     (let ([c (current-extension-linker)])
	       (and c (regexp-match #"ilink32.exe$" (path-string->string c))))))

      (define win-gcc? (still-win-gcc?))
      (define win-borland? (still-win-borland?))
      
      ;; ---- The right flags for this platform+linker --------------------
      
      ;; We need 
      ;;   1) the basic flags
      ;;   2) a way to wrap inputs on the command line
      ;;   3) a way to wrap the output on the command line
      ;;   4) needed base libraries and objects

      (define link-variant (make-parameter 
			    'normal
			    (lambda (s)
			      (unless (memq s '(normal cgc 3m))
				(raise-type-error 'link-variant "'normal, 'cgc, or '3m" s))
			      s)))

      (define (specific-link-variant)
        (let ([v (link-variant)])
          (if (eq? v 'normal)
              (system-type 'gc)
              v)))

      (define (wrap-3m s)
	(lambda ()
	  (list (format s (if (eq? '3m (specific-link-variant)) "3m" "")))))

      (define (drop-3m s)
	(lambda ()
	  (if (eq? '3m (specific-link-variant))
	      null
	      (list s))))

      (define (expand-for-link-variant l)
	(apply append (map (lambda (s) (if (path-string? s) (list (path-string->string s)) (s))) l)))

      (define current-use-mzdyn (make-parameter #t))
      (define (mzdyn-maybe s)
        (lambda ()
          (if (current-use-mzdyn) (s) null)))

      (define msvc-linker-flags (list "/LD"))
      (define win-gcc-linker-flags (list "--dll"))
      (define borland-linker-flags (list "/Tpd" "/c"))

      (define mac-link-flags (list "-bundle" "-flat_namespace" "-undefined" "suppress"))

      (define (get-unix-link-flags)
	(case (string->symbol (path->string (system-library-subpath #f)))
	  [(sparc-solaris i386-solaris) (list "-G")]
	  [(sparc-sunos4) (list "-Bdynamic")]
	  [(i386-freebsd-2.x) (list "-Bshareable")]
	  [(rs6k-aix) (list "-bM:SRE"
			    "-brtl"
			    (lambda () 
			      (map (lambda (mz-exp)
				     (format "-bI:~a/~a" (include-dir) mz-exp))
				   ((wrap-3m "mzscheme~a.exp"))))
			    (format "-bE:~a/ext.exp" (include-dir))
			    "-bnoentry")]
	  [(parisc-hpux) (list "-b")]
	  [(ppc-macosx ppc-darwin x86_64-macosx x86_64-darwin) mac-link-flags]
          [(i386-macosx i386-darwin) (append mac-link-flags '("-m32"))]
	  [(i386-cygwin) win-gcc-linker-flags]
	  [else (list "-fPIC" "-shared")]))

      (define (get-env-link-flags)
	(let ([v (or (getenv "MZSCHEME_DYNEXT_LINKER_FLAGS")
		     (getenv "LDFLAGS"))])
	  (if v
	      (split-command-line-args v)
	      null)))
      
      ;; See manual:
      (define current-extension-linker-flags
	(make-parameter
	 (append (get-env-link-flags)
		 (case (system-type)
		   [(unix macosx) (get-unix-link-flags)]
		   [(windows) (cond
			       [win-gcc? win-gcc-linker-flags]
			       [win-borland? borland-linker-flags]
			       [else msvc-linker-flags])]
		   [(macos) null]))
	 (lambda (l)
	   (unless (and (list? l) (andmap string? l))
	     (raise-type-error 'current-extension-linker-flags "list of strings" l))
	   l)))

      ;; See manual:
      (define current-make-link-input-strings
	(make-parameter
	 (lambda (s) (list (path-string->string s)))
	 (lambda (p)
	   (unless (procedure-arity-includes? p 1)
	     (raise-type-error 'current-make-link-input-strings "procedure of arity 1" p))
	   p)))
            
      (define win-gcc-link-output-strings (lambda (s) (list "--base-file"
							    (path->string (make-win-gcc-temp "base"))
							    "-e" "_dll_entry@12" 
							    "-o" (path-string->string s))))
      (define msvc-link-output-strings (lambda (s) (list (string-append "/Fe" (path-string->string s)))))
      (define borland-link-output-strings (lambda (s) (list* "," (path-string->string s)
                                                             "," "," "c0d32.obj" "cw32.lib" "import32.lib"
                                                             (if (current-use-mzdyn)
                                                               (list "," (path->string
                                                                          (build-path (std-library-dir)
                                                                                      "bcc" 
                                                                                      "mzdynb.def")))
                                                               null))))
      
      ;; See manual:
      (define current-make-link-output-strings
	(make-parameter
	 (case (system-type)
	   [(unix macosx) 
	    (case (string->symbol (path->string (system-library-subpath #f)))
	      [(i386-cygwin) win-gcc-link-output-strings]
	      [else (lambda (s) (list "-o" (path-string->string s)))])]
	   [(windows) (cond
		       [win-gcc? win-gcc-link-output-strings]
		       [win-borland? borland-link-output-strings]
		       [else msvc-link-output-strings])]
	   [(macos) (lambda (s) (list "-o" (path-string->string s)))])
	 (lambda (p)
	   (unless (procedure-arity-includes? p 1)
	     (raise-type-error 'current-make-link-output-strings "procedure of arity 1" p))
	   p)))

      (define (make-win-link-libraries win-gcc? win-borland? unix?)
	(let* ([file (lambda (f)
		       (path->string
			(build-path (std-library-dir)
				    (cond
                                     [win-gcc? "gcc"]
                                     [win-borland? "bcc"]
                                     [else "msvc"])
				    f)))]
	       [dllfile (lambda (f)
			  (path->string
			   (build-path (std-library-dir) f)))]
               [filethunk (lambda (f)
                            (lambda ()
			      (map file (f))))]
	       [wrap-xxxxxxx
                (lambda (file f)
                  (lambda ()
                    (map (lambda (s)
                           (if (file-exists?
                                (file (format s filename-version-part)))
                             (file (format s filename-version-part))
                             (file (format s "xxxxxxx"))))
                         (f))))])
	  (cond
	   [win-gcc? (append
		      (if unix?
			  null
			  (list (wrap-xxxxxxx dllfile (wrap-3m "libracket~a~~a.dll"))
				(wrap-xxxxxxx dllfile (drop-3m "libmzgc~a.dll"))))
		      (list
		       (mzdyn-maybe (filethunk (wrap-3m "mzdyn~a.exp")))
		       (mzdyn-maybe (filethunk (wrap-3m 
						;; mzdyn.o is for Unix build, mzdynw.o for Windows
						(format "mzdyn~a~~a.o"
							(if unix? "" "w")))))
		       (file "init.o")
		       (file "fixup.o")))]
	   [win-borland? (map file (if (current-use-mzdyn)
                                     (list "mzdynb.obj")
                                     null))]
	   [else (list (wrap-xxxxxxx file (wrap-3m "libracket~a~~a.lib"))
		       (wrap-xxxxxxx file (drop-3m "libmzgc~a.lib"))
		       (mzdyn-maybe (filethunk (wrap-3m "mzdyn~a.exp")))
		       (mzdyn-maybe (filethunk (wrap-3m "mzdyn~a.obj"))))])))

      (define (get-unix/macos-link-libraries)
	(case (string->symbol (path->string (system-library-subpath #f)))
	  [(i386-cygwin)
	   (make-win-link-libraries #t #f #t)]
	  [else
	   (list (lambda ()
		   (if (current-use-mzdyn)
		       (map (lambda (mz.o)
			      (path->string (build-path (std-library-dir) mz.o)))
			    ((wrap-3m "mzdyn~a.o")))
		       null)))]))

      ;; See manual:
      (define current-standard-link-libraries
	(make-parameter
	 (case (system-type)
	   [(unix macos macosx) (get-unix/macos-link-libraries)]
	   [(windows) (make-win-link-libraries win-gcc? win-borland? #f)])
	 (lambda (l)
	   (unless (and (list? l) 
			(andmap (lambda (s) (or (path-string? s)
						(and (procedure? s) (procedure-arity-includes? s 0))))
				l))
	     (raise-type-error 'current-stand-link-libraries "list of paths/strings and thunks" l))
	   l)))
      
      ;; ---- Function to install standard linker parameters --------------------

      ;; see manual
      (define (use-standard-linker name)
	(define (bad-name name)
	  (error 'use-standard-linker "unknown linker: ~a" name))
	(case (system-type)
	  [(unix macosx) 
	   (case name
	     [(cc gcc) (current-extension-linker (get-unix-linker))
	      (current-extension-linker-flags (get-unix-link-flags))
	      (current-make-link-input-strings (lambda (s) (list (path-string->string s))))
	      (current-make-link-output-strings (lambda (s) (list "-o" (path-string->string s))))
	      (current-standard-link-libraries (get-unix/macos-link-libraries))]
	     [else (bad-name name)])]
	  [(windows)
	   (case name
	     [(gcc) (let ([f (find-executable-path "ld.exe" #f)])
		      (unless f
			(error 'use-standard-linker "cannot find gcc's ld.exe"))
		      (current-extension-linker f)
		      (current-extension-linker-flags win-gcc-linker-flags)
		      (current-make-link-input-strings (lambda (s) (list (path-string->string s))))
		      (current-make-link-output-strings win-gcc-link-output-strings)
		      (current-standard-link-libraries (make-win-link-libraries #t #f #f)))]
	     [(borland) (let ([f (find-executable-path "ilink32.exe" #f)])
			  (unless f
			    (error 'use-standard-linker "cannot find ilink32.exe"))
			  (current-extension-linker f)
			  (current-extension-linker-flags borland-linker-flags)
			  (current-make-link-input-strings (lambda (s) (list (path-string->string s))))
			  (current-make-link-output-strings borland-link-output-strings)
			  (current-standard-link-libraries (make-win-link-libraries #f #t #f)))]
	     [(msvc) (let ([f (find-executable-path "cl.exe" #f)])
		       (unless f
			 (error 'use-standard-linker "cannot find MSVC's cl.exe"))
		       (current-extension-linker f)
		       (current-extension-linker-flags msvc-linker-flags)
		       (current-make-link-input-strings (lambda (s) (list (path-string->string s))))
		       (current-make-link-output-strings msvc-link-output-strings)
		       (current-standard-link-libraries (make-win-link-libraries #f #f #f)))]
	     [else (bad-name name)])]
	  [(macos)
	   (case name
	     [(cw) (current-extension-linker #f)
	      (current-extension-linker-flags null)
	      (current-make-link-input-strings (lambda (s) (list (path-string->string s))))
	      (current-make-link-output-strings (lambda (s) (list "-o" (path-string->string s))))
	      (current-standard-link-libraries (get-unix/macos-link-libraries))]
	     [else (bad-name name)])]))

      ;; ---- The link driver for each platform --------------------

      (define unix/windows-link
	(lambda (quiet? in out)
	  (let ([c (current-extension-linker)])
	    (if c
		(let* ([output-strings
			((current-make-link-output-strings) out)]
		       [libs (expand-for-link-variant (current-standard-link-libraries))]
		       [command 
			(append 
			 (list c)
			 (expand-for-link-variant (current-extension-linker-flags))
			 (apply append (map (lambda (s) ((current-make-link-input-strings) s))
					    in))
			 libs
			 output-strings)])
		  (unless #f ; quiet? 
		    (printf "link-extension: ~a\n" command))
		  (stdio-link (lambda (quiet?)
				(apply my-process* command))
			      quiet?)

		  ;; Stange Cygwin system for relocatable DLLs: we run dlltool twice and
		  ;;  ld three times total
		  (when (still-win-gcc?)
		    (let ([dlltool (find-executable-path "dlltool.exe" "dlltool.exe")]
			  ;; Find base-file name we already made up:
			  [basefile (let ([m (member "--base-file" output-strings)])
				      (and m (cadr m)))]
			  ;; Make new exp file name:
			  [expfile (make-win-gcc-temp "exp")])
		      (when (and dlltool basefile)
			(let* ([dll-command
				;; Generate DLL link information
				`("--dllname" ,(if (path? out) (path->string out) out)
				  ,@(if (current-use-mzdyn)
                                      `("--def" ,(path->string (build-path (std-library-dir) "gcc" "mzdyn.def")))
                                      `())
				  "--base-file" ,basefile
				  "--output-exp" ,(path->string expfile))]
			       ;; Command to link with new .exp, re-create .base:
			       [command1
				(map (lambda (s)
				       (let ([s (if (path? s)
						    (path->string s)
						    s)])
					 (if (regexp-match "[.]exp$" s)
					     (path->string expfile)
					     s)))
				     command)]
			       ;; Command to link with new .exp file, no .base needed:
			       [command2
				(let loop ([l command1])
				  (cond
				   [(null? l) null]
				   [(and (string? (car l))
					 (string=? (car l) "--base-file"))
				    (cddr l)]
				   [else (cons (car l) (loop (cdr l)))]))])
			  (unless quiet?
			    (printf "link-extension, dlltool phase: ~a\n" 
				    (cons dlltool dll-command)))
			  (stdio-link (lambda (quiet?) 
					(apply my-process* dlltool dll-command))
				      quiet?)
			  (unless quiet?
			    (printf "link-extension, re-link phase: ~a\n" 
				    command1))
			  (stdio-link (lambda (quiet?) 
					(apply my-process* command1))
				      quiet?)
			  (unless quiet?
			    (printf "link-extension, re-dlltool phase: ~a\n" 
				    (cons dlltool dll-command)))
			  (stdio-link (lambda (quiet?)
					(apply my-process* dlltool dll-command))
				      quiet?)
			  (unless quiet?
			    (printf "link-extension, last re-link phase: ~a\n" 
				    command2))
			  (stdio-link (lambda (quiet?)
					(apply my-process* command2))
				      quiet?)
			  (delete-file basefile)
			  (delete-file expfile))))))
		(error 'link-extension "can't find an installed linker")))))
      
      (define link-extension
	(case (system-type)
	  [(unix windows macosx) unix/windows-link]))

      ;; ---- some helpers:
      
      (define-values (my-process* stdio-link)
	(let-values ([(p* do-stdio) (get-stdio)])
	  (values
	   p*
	   (lambda (start-process quiet?)
	     (do-stdio start-process quiet? (lambda (s) (error 'link-extension "~a" s)))))))
      
      (define (make-win-gcc-temp suffix)
	(let ([d (find-system-path 'temp-dir)])
	  (let loop ([n 1])
	    (let ([f (build-path d (format "tmp~a.~a" n suffix))])
	      (if (file-exists? f)
		  (loop (add1 n))
		  f)))))))
