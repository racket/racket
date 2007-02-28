
(module launcher-unit (lib "a-unit.ss")
  (require (lib "file.ss")
	   (lib "string.ss")
	   (lib "etc.ss")

	   (lib "compile-sig.ss" "dynext")
	   (lib "link-sig.ss" "dynext")
	   (lib "embed.ss" "compiler")
	   (lib "dirs.ss" "setup")
           (lib "variant.ss" "setup")

	   "launcher-sig.ss"

	   (lib "winutf16.ss" "compiler" "private"))

      (import (prefix c: dynext:compile^)
	      (prefix l: dynext:link^))
      (export launcher^)

      (define current-launcher-variant
	(make-parameter (system-type 'gc)
			(lambda (v)
			  (unless (memq v '(3m script-3m cgc script-cgc))
			    (raise-type-error
			     'current-launcher-variant
			     "variant symbol"
			     v))
			  v)))

      (define (variant-available? kind cased-kind-name  variant)
        (cond
         [(or (eq? 'unix (system-type))
              (and (eq? 'macosx (system-type))
                   (eq? kind 'mzscheme)))
          (let ([bin-dir (find-console-bin-dir)])
            (and bin-dir
                 (file-exists? (build-path 
                                bin-dir 
                                (format "~a~a" kind (variant-suffix variant #f))))))]
         [(eq? 'macosx (system-type))
          ;; kind must be mred, because mzscheme case is caught above
          (directory-exists? (build-path (find-gui-bin-dir) 
                                         (format "~a~a.app" 
                                                 cased-kind-name 
                                                 (variant-suffix variant #f))))]
         [(eq? 'windows (system-type))
          (file-exists? (build-path (if (eq? kind 'mzscheme)
                                        (find-console-bin-dir)
                                        (find-gui-bin-dir))
                                    (format "~a~a.exe" 
                                            cased-kind-name 
                                            (variant-suffix variant #t))))]
         [else (error "unknown system type")]))

      (define (available-variants kind)
	(let* ([cased-kind-name (if (eq? kind 'mzscheme)
                                    "MzScheme"
                                    "MrEd")]
               [normal-kind (system-type 'gc)]
               [alt-kind (if (eq? '3m normal-kind)
                             'cgc
                             '3m)]
               [normal (if (variant-available? kind cased-kind-name normal-kind)
			   (list normal-kind)
                           null)]
	       [alt (if (variant-available? kind cased-kind-name alt-kind)
                        (list alt-kind)
                        null)]
	       [script (if (and (eq? 'macosx (system-type))
				(eq? kind 'mred)
				(pair? normal))
			   (if (eq? normal-kind '3m)
                               '(script-3m)
                               '(script-cgc))
			   null)]
	       [script-alt (if (and (memq alt-kind alt)
                                    (pair? script))
                               (if (eq? alt-kind '3m)
                                   '(script-3m)
                                   '(script-cgc))
                               null)])
	  (append normal alt script script-alt)))

      (define (available-mred-variants)
	(available-variants 'mred))

      (define (available-mzscheme-variants)
	(available-variants 'mzscheme))

      (define (install-template dest kind mz mr)
	(define src (build-path (collection-path "launcher")
				(if (eq? kind 'mzscheme) mz mr)))
	(when (or (file-exists? dest)
		  (directory-exists? dest)
		  (link-exists? dest))
	  (delete-directory/files dest))
	(copy-file src dest))

      (define (script-variant? v)
        (memq v '(script-3m script-cgc)))

      (define (add-file-suffix path variant mred?)
	(let ([s (variant-suffix variant (case (system-type)
                                           [(unix) #f]
                                           [(windows) #t]
                                           [(macosx) (and mred?
                                                          (not (script-variant? variant)))]))])
	  (if (string=? "" s)
	      path
	      (if (and (eq? 'windows (system-type))
		       (regexp-match #rx#"[.]exe$" (path->bytes path)))
		  (path-replace-suffix path (string->bytes/utf-8
					     (format "~a.exe" s)))
		  (path-replace-suffix path (string->bytes/utf-8 s))))))
      
      (define (string-append/spaces f flags)
	(if (null? flags)
	    ""
	    (string-append
	     (f (car flags))
	     " "
	     (string-append/spaces f (cdr flags)))))
      
      (define (str-list->sh-str flags)
	(letrec ([trans
		  (lambda (s)
		    (cond
		     [(regexp-match "(.*)'(.*)" s)
		      => (lambda (m)
			   (string-append (trans (cadr m))
					  "\"'\""
					  (trans (caddr m))))]
		     [else (format "'~a'" s)]))])
	  (string-append/spaces trans flags)))
      
      (define (str-list->dos-str flags)
	(letrec ([trans
		  (lambda (s)
		    (if (or (regexp-match (string #\[ #\space #\newline #\tab #\return #\vtab #\]) s)
			    (regexp-match "\"" s)
			    (regexp-match "\\\\" s))
			(list->string
			 (let loop ([l (string->list s)][wrote-slash 0])
			   (cond
			    [(null? l) null]
			    [(char-whitespace? (car l))
			     (append
			      (string->list (make-string wrote-slash #\\))
			      (list #\" (car l) #\")
			      (loop (cdr l) 0))]
			    [else
			     (case (car l)
			       [(#\\) (cons #\\ (loop (cdr l) (add1 wrote-slash)))]
			       [(#\") (append
				       (string->list (make-string wrote-slash #\\))
				       `(#\" #\\ #\" #\")
				       (loop (cdr l) 0))]
			       [else (cons (car l) (loop (cdr l) 0))])])))
			s))])
	  (string-append/spaces trans flags)))

      (define one-arg-x-flags '((xa "-display")
				(xb "-geometry")
				(xc "-bg" "-background") 
				(xd "-fg" "-foregound") 
				(xe "-font")
				(xf "-name")
				(xg "-selectionTimeout")
				(xh "-title")
				(xi "-xnllanguage")
				(xj "-xrm")))
      (define no-arg-x-flags '((xk "-iconic") 
			       (xl "-rv" "-reverse") 
			       (xm "+rv") 
			       (xn "-synchronous")
                               (xo "-singleInstance")))

      (define (skip-x-flags flags)
	(let ([xfmem (lambda (flag) (lambda (xf) (member flag (cdr xf))))])
	  (let loop ([f flags])
	    (if (null? f)
		null
		(if (ormap (xfmem (car f)) one-arg-x-flags)
		    (if (null? (cdr f))
			null
			(loop (cddr f)))
		    (if (ormap (xfmem (car f)) no-arg-x-flags)
			(loop (cdr f))
			f))))))

      (define (output-x-arg-getter exec args)
	(let ([or-flags
               (lambda (l)
                 (if (null? (cdr l))
                   (car l)
                   (string-append
                    (car l)
                    (apply
                     string-append
                     (map (lambda (s) (string-append " | " s)) (cdr l))))))])
	  (apply
	   string-append
	   (append
	    (list "# Find X flags and shift them to the front\n"
		  "findxend() {\n"
		  " oneargflag=''\n"
		  " case \"$1\" in\n")
	    (map
	     (lambda (f)
	       (format (string-append
			"  ~a)\n"
			"     oneargflag=\"$1\"\n"
			"     ~a=\"$2\"\n"
			"   ;;\n")
		       (or-flags (cdr f))
		       (car f)))
	     one-arg-x-flags)
	    (map
	     (lambda (f)
	       (format "  ~a)\n    ~a=yes\n  ;;\n" (or-flags (cdr f)) (car f)))
	     no-arg-x-flags)
	    (list
	     (format (string-append
		      "  *)\n    ~a~a ~a  ;;\n"
		      " esac\n"
		      " shift\n"
		      " if [ \"$oneargflag\" != '' ] ; then\n"
		      "   if [ \"${1+n}\" != 'n' ] ; then echo $0: missing argument for standard X flag $oneargflag ; exit 1 ; fi\n"
		      "   shift\n"
		      " fi\n"
		      " findxend ${1+\"$@\"}\n"
		      "}\nfindxend ${1+\"$@\"}\n")
		     exec 
		     (apply
		      string-append
		      (append
		       (map
			(lambda (f) (format " ${~a+\"~a\"} ${~a+\"$~a\"}" (car f) (cadr f) (car f) (car f)))
			one-arg-x-flags)
		       (map
			(lambda (f) (format " ${~a+\"~a\"}" (car f) (cadr f)))
			no-arg-x-flags)))
		     args))))))

      (define (protect-shell-string s)
	(regexp-replace*
         #rx"[\"`'$\\]" (if (path? s) (path->string s) s) "\\\\&"))

      (define (normalize+explode-path p)
	(explode-path (normal-case-path (normalize-path p))))

      (define (relativize bindir-explode dest-explode)
	(let loop ([b bindir-explode] [d dest-explode])
	  (if (and (pair? b) (equal? (car b) (car d)))
	      (loop (cdr b) (cdr d))
	      (let ([p (append (map (lambda (x) 'up) (cdr d)) b)])
		(if (null? p) 
		    #f
		    (apply build-path p))))))

      (define (make-relative-path-header dest bindir)
        ;; rely only on binaries in /usr/bin:/bin
        (define (has-exe? exe)
          (or (file-exists? (build-path "/usr/bin" exe))
              (file-exists? (build-path "/bin" exe))))
        (let* ([has-readlink?  (and (not (eq? 'macosx (system-type)))
                                    (has-exe? "readlink"))]
               [dest-explode   (normalize+explode-path dest)]
               [bindir-explode (normalize+explode-path bindir)])
          (if (and (has-exe? "dirname") (has-exe? "basename")
                   (or has-readlink? (and (has-exe? "ls") (has-exe? "sed")))
                   (equal? (car dest-explode) (car bindir-explode)))
            (string-append
             "# Make this PATH-independent\n"
             "saveP=\"$PATH\"\n"
             "PATH=\"/usr/bin:/bin\"\n"
             "\n"
             (if has-readlink? ""
                 (string-append
                  "# imitate possibly-missing readlink\n"
                  "readlink() {\n"
                  "  ls -l -- \"$1\" | sed -e \"s/^.* -> //\"\n"
                  "}\n"
                  "\n"))
             "# Remember current directory\n"
             "saveD=`pwd`\n"
             "\n"
             "# Find absolute path to this script,\n"
             "# resolving symbolic references to the end\n"
             "# (changes the current directory):\n"
             "D=`dirname \"$0\"`\n"
             "F=`basename \"$0\"`\n"
             "cd \"$D\"\n"
             "while test "
             ;; On solaris, Edward Chrzanowski from Waterloo says that the man
             ;; page says that -L is not supported, but -h is; on other systems
             ;; (eg, freebsd) -h is listed as a compatibility feature
             (if (regexp-match #rx"solaris" (path->string
                                             (system-library-subpath)))
               "-h" "-L")
             " \"$F\"; do\n"
             "  P=`readlink \"$F\"`\n"
             "  D=`dirname \"$P\"`\n"
             "  F=`basename \"$P\"`\n"
             "  cd \"$D\"\n"
             "done\n"
             "D=`pwd`\n"
             "\n"
             "# Restore current directory\n"
             "cd \"$saveD\"\n"
             "\n"
             "bindir=\"$D"
	     (let ([s (relativize bindir-explode dest-explode)])
	       (if s
		   (string-append "/" 
				  (protect-shell-string s))
		   ""))
             "\"\n"
             "PATH=\"$saveP\"\n")
            ;; fallback to absolute path header
            (make-absolute-path-header bindir))))

      (define (make-absolute-path-header bindir)
	(string-append "bindir=\""(protect-shell-string bindir)"\"\n"))

      (define (make-unix-launcher kind variant flags dest aux)
	(install-template dest kind "sh" "sh") ; just for something that's executable
	(let* ([alt-exe (let ([m (and (eq? kind 'mred)
				      (script-variant? variant)
				      (assq 'exe-name aux))])
			  (and m
			       (format "~a~a.app/Contents/MacOS/~a~a" 
				       (cdr m) (variant-suffix variant #t)
				       (cdr m) (variant-suffix variant #t))))]
	       [x-flags? (and (eq? kind 'mred)
			      (eq? (system-type) 'unix)
			      (not (script-variant? variant)))]
	       [post-flags (cond
			    [x-flags? (skip-x-flags flags)]
			    [alt-exe null]
			    [else flags])]
	       [pre-flags (cond
			   [(not x-flags?) null]
			   [else
			    (let loop ([f flags])
			      (if (eq? f post-flags)
				  null
				  (cons (car f) (loop (cdr f)))))])]
	       [pre-str (str-list->sh-str pre-flags)]
	       [post-str (str-list->sh-str post-flags)]
	       [header (string-append
                        "#!/bin/sh\n"
                        "# This script was created by make-"
                        (symbol->string kind)"-launcher\n")]
	       [dir-finder
		(let ([bindir (if alt-exe
				  (find-gui-bin-dir)
				  (find-console-bin-dir))])
		  (if (let ([a (assq 'relative? aux)])
			(and a (cdr a)))
		      (make-relative-path-header dest bindir)
		      (make-absolute-path-header bindir)))]
	       [exec (format
		      "exec \"${bindir}/~a~a\" ~a"
		      (or alt-exe kind)
		      (if alt-exe "" (variant-suffix variant #f))
		      pre-str)]
	       [args (format
		      "~a~a ${1+\"$@\"}\n"
		      (if alt-exe "" "-N \"$0\" ")
		      post-str)]
	       [assemble-exec (if (and (eq? kind 'mred)
				       (not (script-variant? variant))
				       (not (null? post-flags)))
				  output-x-arg-getter
				  string-append)])
	  (unless (find-console-bin-dir)
	    (error 'make-unix-launcher "unable to locate bin directory"))
          (with-output-to-file dest
            (lambda ()
              (display header)
              (newline)
              ;; comments needed to rehack launchers when paths change
              ;; (see setup/unixstyle-install.ss)
              (display "# {{{ bindir\n")
              (display dir-finder)
              (display "# }}} bindir\n")
              (newline)
              (display (assemble-exec exec args)))
            'truncate)))

      (define (utf-16-regexp b)
	(byte-regexp (bytes-append (bytes->utf-16-bytes b)
				   #"[^>]*"
				   (bytes->utf-16-bytes #">"))))

      (define (make-windows-launcher kind variant flags dest aux)
	(if (not (and (let ([m (assq 'independent? aux)])
			(and m (cdr m)))))
	    ;; Normal launcher: 
	    (make-embedding-executable dest (eq? kind 'mred) #f
				       null null null
				       flags
				       aux
				       #t
				       variant)
	    ;; Independent launcher (needed for Setup PLT):
	    (begin
	      (install-template dest kind "mzstart.exe" "mrstart.exe")
	      (let ([bstr (bytes->utf-16-bytes
			   (string->bytes/utf-8 (str-list->dos-str flags)))]
		    [p (open-input-file dest)]
		    [m (utf-16-regexp #"<Command Line: Replace This")]
		    [x (utf-16-regexp #"<Executable Directory: Replace This")]
		    [v (utf-16-regexp #"<Executable Variant: Replace This")])
		(let* ([exedir (bytes->utf-16-bytes
				(bytes-append
				 (path->bytes (let ([bin-dir (if (eq? kind 'mred)
								 (find-gui-bin-dir)
								 (find-console-bin-dir))])
					       (if (let ([m (assq 'relative? aux)])
						    (and m (cdr m)))
						  (or (relativize (normalize+explode-path bin-dir)
								  (normalize+explode-path dest))
						      (build-path 'same))
						  bin-dir)))
				 ;; null wchar marks end of executable directory
				 #"\0\0"))]
		       [find-it ; Find the magic start
			(lambda (magic s)
			  (file-position p 0)
			  (let ([m (regexp-match-positions magic p)])
			    (if m
				(car m)
				(begin
				  (close-input-port p)
				  (when (file-exists? dest)
				    (delete-file dest))
				  (error 
				   'make-windows-launcher 
				   (format
				    "Couldn't find ~a position in template" s))))))]
		       [exedir-poslen (find-it x "executable path")]
		       [command-poslen (find-it m "command-line")]
		       [variant-poslen (find-it v "variant")]
		       [pos-exedir (car exedir-poslen)]
		       [len-exedir (- (cdr exedir-poslen) (car exedir-poslen))]
		       [pos-command (car command-poslen)]
		       [len-command (- (cdr command-poslen) (car command-poslen))]
		       [pos-variant (car variant-poslen)]
		       [space (char->integer #\space)]
		       [write-magic
			(lambda (p s pos len)
			  (file-position p pos)
			  (display s p)
			  (display (make-bytes (- len (bytes-length s)) space) p))]
		       [check-len
			(lambda (len s es)
			  (when (> (bytes-length s) len)
				(when (file-exists? dest)
				  (delete-file dest))
				(error
				 (format
				  "~a exceeds limit of ~a characters with ~a characters: ~a"
				  es len (string-length s) s))))])
		  (close-input-port p)
		  (check-len len-exedir exedir "executable home directory")
		  (check-len len-command bstr "collection/file name")
		  (let ([p (open-output-file dest 'update)])
		    (write-magic p exedir pos-exedir len-exedir)
		    (write-magic p (bytes-append bstr #"\0\0") pos-command len-command)
		    (let* ([suffix (variant-suffix (current-launcher-variant) #t)]
                           [suffix-bytes (bytes-append
                                          (list->bytes
                                           (apply append
                                                  (map (lambda (c) (list c 0))
                                                       (bytes->list (string->bytes/latin-1 suffix)))))
                                          #"\0\0")])
                      (write-magic p suffix-bytes pos-variant (bytes-length suffix-bytes)))
		    (close-output-port p)))))))

      ;; OS X launcher code:

      ; make-macosx-launcher : symbol (listof str) pathname ->
      (define (make-macosx-launcher kind variant flags dest aux)
	(if (or (eq? kind 'mzscheme)
                (script-variant? variant))
	    ;; MzScheme or script launcher is the same as for Unix
	    (make-unix-launcher kind variant flags dest aux)
	    ;; MrEd "launcher" is a stand-alone executable
	    (make-embedding-executable dest (eq? kind 'mred) #f
				       null null null
				       flags
				       aux
				       #t
				       variant)))

      (define (make-macos-launcher kind variant flags dest aux)
	(install-template dest kind "GoMr" "GoMr")
	(let ([p (open-input-file dest)])
	  (let ([m (regexp-match-positions #rx#"<Insert offset here>" p)])
	    ;; fast-forward to the end:
	    (let ([s (make-bytes 4096)])
	      (let loop ()
		(if (eof-object? (read-bytes! s p))
		    (file-position p)
		    (loop))))
	    (let ([data-fork-size (file-position p)])
	      (close-input-port p)
	      (let ([p (open-output-file dest 'update)]
		    [str (str-list->sh-str (append
					    (if (eq? kind 'mred)
						null
						'("-Z"))
					    flags))])
		(file-position p (caar m))
		(display (integer->integer-bytes (string-length str) 4 #t #t) p)
		(display (integer->integer-bytes data-fork-size 4 #t #t) p)
		(file-position p data-fork-size)
		(display str p)
		(close-output-port p))))))

      (define (get-maker)
	(case (system-type)
	  [(unix)    make-unix-launcher]
	  [(windows) make-windows-launcher]
	  [(macos)   make-macos-launcher]
	  [(macosx)  make-macosx-launcher]))

      (define make-mred-launcher
	(opt-lambda (flags dest [aux null])
	  (let ([variant (current-launcher-variant)])
	    ((get-maker) 'mred variant flags dest aux))))

      (define make-mzscheme-launcher
	(opt-lambda (flags dest [aux null])
	  (let ([variant (current-launcher-variant)])
	    ((get-maker) 'mzscheme variant flags dest aux))))

      (define (strip-suffix s)
	(path-replace-suffix s #""))

      (define (build-aux-from-path aux-root)
	(let ([aux-root (if (string? aux-root)
			    (string->path aux-root)
			    aux-root)])
	  (let ([try (lambda (key suffix)
		       (let ([p (path-replace-suffix aux-root suffix)])
			 (if (file-exists? p)
			     (list (cons key p))
			     null)))])
	    (append
	     (try 'icns #".icns")
	     (try 'ico #".ico")
	     (try 'independent? #".lch")
	     (let ([l (try 'creator #".creator")])
	       (if (null? l)
		   l
		   (with-handlers ([exn:fail:filesystem? (lambda (x) null)])
		     (with-input-from-file (cdar l)
		       (lambda () 
			 (let ([s (read-string 4)])
			   (if s
			       (list (cons (caar l) s))
			       null)))))))
	     (let ([l (try 'file-types #".filetypes")])
	       (if (null? l)
		   l
		   (with-handlers ([exn:fail:filesystem? (lambda (x) null)])
		     (with-input-from-file (cdar l)
		       (lambda () 
			 (let ([d (read)])
			   (let-values ([(local-dir base dir?) (split-path aux-root)])
			     (let ([icon-files
				    (apply
				     append
				     (map (lambda (spec)
					    (let ([m (assoc "CFBundleTypeIconFile" spec)])
					      (if m
						  (list (build-path 
							 (path->complete-path local-dir)
							 (format "~a.icns" (cadr m))))
						  null)))
					  d))])
			       (list
				(cons 'file-types d)
				(cons 'resource-files icon-files))))))))))))))

      (define (make-mred-program-launcher file collection dest)
	(make-mred-launcher (list "-mqvL" file collection "--") 
			    dest
			    (build-aux-from-path
			     (build-path (collection-path collection)
					 (strip-suffix file)))))
      
      (define (make-mzscheme-program-launcher file collection dest)
	(make-mzscheme-launcher (list "-mqvL" file collection "--") 
				dest
				(build-aux-from-path
				 (build-path (collection-path collection)
					     (strip-suffix file)))))

      (define (unix-sfx file)
	(list->string
	 (map
	  (lambda (c)
	    (if (char-whitespace? c)
		#\-
		(char-downcase c)))
	  (string->list file))))

      (define (sfx file) (case (system-type) 
			   [(unix) (unix-sfx file)]
			   [(windows) (string-append file ".exe")]
			   [else file]))

      (define (program-launcher-path name mred?)
	(let* ([variant (current-launcher-variant)]
	       [mac-script? (and (eq? (system-type) 'macosx) 
                                 (script-variant? variant))])
	  (let ([p (add-file-suffix 
		    (build-path 
		     (if (or mac-script? (not mred?))
			 (find-console-bin-dir)
			 (find-gui-bin-dir))
		     ((if mac-script? unix-sfx sfx) name))
		    variant
                    mred?)])
	   (if (and (eq? (system-type) 'macosx) 
		    (not (script-variant? variant)))
	       (path-replace-suffix p #".app")
	       p))))

      (define (mred-program-launcher-path name)
	(program-launcher-path name #t))
      
      (define (mzscheme-program-launcher-path name)
	(case (system-type)
	  [(macosx) (add-file-suffix 
		     (build-path (find-console-bin-dir) (unix-sfx name))
		     (current-launcher-variant)
                     #f)]
	  [else (program-launcher-path name #f)]))
      
      (define (mred-launcher-is-directory?)
	#f)
      (define (mzscheme-launcher-is-directory?)
	#f)

      (define (mred-launcher-is-actually-directory?)
	(and (eq? 'macosx (system-type))
	     (not (script-variant? (current-launcher-variant)))))
      (define (mzscheme-launcher-is-actually-directory?)
	#f)

      ;; Helper:
      (define (put-file-extension+style+filters type)
	(case type
	  [(windows) (values "exe" null '(("Executable" "*.exe")))]
	  [(macosx) (values "app" '(packages) '(("App" "*.app")))]
	  [else (values #f null null)]))
      
      (define (mred-launcher-add-suffix path)
	(embedding-executable-add-suffix path #t))

      (define (mzscheme-launcher-add-suffix path)
	(embedding-executable-add-suffix path #f))

      (define (mred-launcher-put-file-extension+style+filters)
	(put-file-extension+style+filters 
	 (if (and (eq? 'macosx (system-type))
                  (script-variant? (current-launcher-variant)))
	     'unix
	     (system-type))))

      (define (mzscheme-launcher-put-file-extension+style+filters)
	(put-file-extension+style+filters 
	 (if (eq? 'macosx (system-type))
	     'unix
	     (system-type))))

      (define mred-launcher-up-to-date?
	(opt-lambda (dest [aux null])
          (mzscheme-launcher-up-to-date? dest aux)))

      (define mzscheme-launcher-up-to-date?
	(opt-lambda (dest [aux null])
           (cond
	    ;; When running Setup PLT under Windows, the
	    ;;  launcher process stays running until MzScheme
	    ;;  completes, which means that it cannot be
	    ;;  overwritten at that time. So we assume
	    ;;  that a Setup-PLT-style independent launcher
	    ;;  is always up-to-date.
	    [(eq? 'windows (system-type))
	     (and (let ([m (assq 'independent? aux)])
		    (and m (cdr m)))
		  (file-exists? dest))]
	    ;; For any other setting, we could implement
	    ;;  a fancy check, but for now always re-create
	    ;;  launchers.
	    [else #f])))

      (define (install-mred-program-launcher file collection name)
	(make-mred-program-launcher file collection (mred-program-launcher-path name)))
      
      (define (install-mzscheme-program-launcher file collection name)
	(make-mzscheme-program-launcher file collection (mzscheme-program-launcher-path name))))
