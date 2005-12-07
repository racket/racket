
(module launcher-unit mzscheme
  (require (lib "unitsig.ss")
	   (lib "file.ss")
	   (lib "string.ss")
	   (lib "etc.ss")

	   (lib "compile-sig.ss" "dynext")
	   (lib "link-sig.ss" "dynext")
	   (lib "embed.ss" "compiler")
	   (lib "plthome.ss" "setup")

	   "launcher-sig.ss")

  (provide launcher@)

  (define launcher@
    (unit/sig launcher^
      (import [c : dynext:compile^]
	      [l : dynext:link^])

      (define current-launcher-variant
	(make-parameter 'normal
			(lambda (v)
			  (unless (memq v '(normal 3m script script-3m))
			    (raise-type-error
			     'current-launcher-variant
			     "variant symbol"
			     v))
			  v)))

      (define (available-variants kind)
	(let* ([3m (cond
		    [(or (eq? 'unix (system-type))
			 (and (eq? 'macosx (system-type))
			      (eq? kind 'mzscheme)))
		     (if (and plthome
			      (file-exists? (build-path plthome "bin" (format "~a3m" kind))))
			 '(3m)
			 null)]
		    [(eq? 'macosx (system-type))
		     ;; kind must be mred, because mzscheme case caught above
		     (if (directory-exists? (build-path plthome "MrEd3m.app"))
			 '(3m)
			 null)]
		    [(eq? 'windows (system-type))
		     (if (file-exists? (build-path plthome (format "~a3m.exe" kind)))
			 '(3m)
			 null)]
		    [else
		     ;; 3m launchers not yet supported for other platforms:
		     null])]
	       [normal (if (eq? kind 'mzscheme)
			   '(normal) ; MzScheme is always available
			   (if (and plthome
				    (cond
				     [(eq? 'unix (system-type))
				      (file-exists? (build-path plthome "bin" (format "~a" kind)))]
				     [(eq? 'macosx (system-type))
				      (directory-exists? (build-path plthome "MrEd.app"))]
				     [(eq? 'windows (system-type))
				      (file-exists? (build-path plthome (format "~a.exe" kind)))]
				     [else #t]))
			       '(normal)
			       null))]
	       [script (if (and (eq? 'macosx (system-type))
				(eq? kind 'mred)
				(pair? normal))
			   '(script)
			   null)]
	       [script-3m (if (and (memq '3m 3m)
				   (memq 'script script))
			      '(script-3m)
			      null)])
	  (append 3m normal script script-3m)))

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

      (define (variant-suffix variant)
	(case variant
	  [(normal script) ""]
	  [(3m script-3m) "3m"]))

      (define (add-file-suffix path variant)
	(let ([s (variant-suffix variant)])
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
			       (xn "-synchronous")))

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
	(let* ([newline (string #\newline)]
	       [or-flags
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
	    (list "# Find X flags and shift them to the front" newline
		  "findxend()" newline
		  "{" newline
		  " oneargflag=''" newline
		  " case \"$1\" in" newline)
	    (map
	     (lambda (f)
	       (format (string-append
			"  ~a)" newline
			"     oneargflag=\"$1\"" newline
			"     ~a=\"$2\"" newline
			"   ;;" newline)
		       (or-flags (cdr f))
		       (car f)))
	     one-arg-x-flags)
	    (map
	     (lambda (f)
	       (format "  ~a)~n    ~a=yes~n  ;;~n" (or-flags (cdr f)) (car f)))
	     no-arg-x-flags)
	    (list
	     (format (string-append
		      "  *)~n    ~a~a~a  ;;~n"
		      " esac~n"
		      " shift~n"
		      " if [ \"$oneargflag\" != '' ] ; then~n"
		      "   if [ \"${1+n}\" != 'n' ] ; then echo $0: missing argument for standard X flag $oneargflag ; exit 1 ; fi~n"
		      "   shift~n"
		      " fi~n"
		      " findxend ${1+\"$@\"}~n"
		      "}~nfindxend ${1+\"$@\"}~n")
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

      (define (make-unix-launcher kind variant flags dest aux)
	(install-template dest kind "sh" "sh") ; just for something that's executable
	(let* ([newline (string #\newline)]
	       [alt-exe (let ([m (and (eq? kind 'mred)
				      (memq variant '(script script-3m))
				      (assq 'exe-name aux))])
			  (and m
			       (format "~a~a.app/Contents/MacOS/~a~a" 
				       (cdr m) (variant-suffix variant)
				       (cdr m) (variant-suffix variant))))]
	       [post-flags (if (and (eq? kind 'mred)
				    (not (memq variant '(script script-3m))))
			       (skip-x-flags flags)
			       null)]
	       [pre-flags (cond
			   [alt-exe null]
			   [(null? post-flags) flags]
			   [else
			    (let loop ([f flags])
			      (if (eq? f post-flags)
				  null
				  (cons (car f) (loop (cdr f)))))])]
	       [pre-str (str-list->sh-str pre-flags)]
	       [post-str (str-list->sh-str post-flags)]
	       [header (format
			(string-append
			 "#!/bin/sh" newline
			 "# This script was created by make-~a-launcher" newline
			 newline
			 "if [ \"$PLTHOME\" = '' ] ; then" newline
			 "  PLTHOME=\"~a\"" newline
			 "  export PLTHOME" newline
			 "fi" newline
			 newline)
			kind (regexp-replace* "\"" 
					      (path->string plthome)
					      "\\\\\""))]
	       [exec (format
		      "exec \"${PLTHOME}/~a~a~a\" ~a"
		      (if alt-exe "" "bin/")
		      (or alt-exe kind)
		      (if alt-exe "" (variant-suffix variant)) pre-str)]
	       [args (format
		      " ~a ${1+\"$@\"}~n"
		      post-str)]
	       [assemble-exec (if (and (eq? kind 'mred)
				       (not (memq variant '(script scrip-3m)))
				       (not (null? post-flags)))
				  output-x-arg-getter
				  string-append)])
	  (unless plthome
	    (error 'make-unix-launcher "unable to locate PLTHOME"))
	  (let ([p (open-output-file dest 'truncate)])
	    (fprintf p "~a~a"
		     header
		     (assemble-exec exec args))
	    (close-output-port p))))
      
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
	      (let ([bstr (string->bytes/utf-8 (str-list->dos-str flags))]
		    [p (open-input-file dest)]
		    [m #rx#"<Command Line: Replace This[^>]*>"]
		    [x #rx#"<Executable Directory: Replace This[^>]*>"]
		    [v #rx#"<Executable Variant: Replace This>"])
		(let* ([exedir (bytes-append
				(path->bytes plthome)
				;; null character marks end of executable directory
				#"\0")]
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
		    (write-magic p bstr pos-command len-command)   
		    (when (eq? '3m (current-launcher-variant))
		      (write-magic p #"3" pos-variant 1))
		    (close-output-port p)))))))
      
      ;; OS X launcher code:
     
      ; make-macosx-launcher : symbol (listof str) pathname ->  
      (define (make-macosx-launcher kind variant flags dest aux)
	(if (or (eq? kind 'mzscheme) 
		(eq? variant 'script) 
		(eq? variant 'script-3m))
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
	  (let ([m (regexp-match-positions "<Insert offset here>" p)])
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
	  [(unix) make-unix-launcher]
	  [(windows) make-windows-launcher]
	  [(macos) make-macos-launcher]
	  [(macosx) make-macosx-launcher]))
      
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
      
      (define l-home (if (memq (system-type) '(unix))
			 (build-path plthome "bin")
			 plthome))

      (define l-home-macosx-mzscheme (build-path plthome "bin"))

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

      (define (mred-program-launcher-path name)
	(let* ([variant (current-launcher-variant)]
	       [mac-script? (and (eq? (system-type) 'macosx) 
				 (memq variant '(script script-3m)))])
	  (let ([p (add-file-suffix 
		    (build-path 
		     (if mac-script?
			 l-home-macosx-mzscheme
			 l-home)
		     ((if mac-script? unix-sfx sfx) name))
		    variant)])
	   (if (and (eq? (system-type) 'macosx) 
		    (not (memq variant '(script script-3m))))
	       (path-replace-suffix p #".app")
	       p))))
      
      (define (mzscheme-program-launcher-path name)
	(case (system-type)
	  [(macosx) (add-file-suffix 
		     (build-path l-home-macosx-mzscheme (unix-sfx name))
		     (current-launcher-variant))]
	  [else (mred-program-launcher-path name)]))
      
      (define (mred-launcher-is-directory?)
	#f)
      (define (mzscheme-launcher-is-directory?)
	#f)

      (define (mred-launcher-is-actually-directory?)
	(and (eq? 'macosx (system-type))
	     (not (memq (current-launcher-variant) '(script script-3m)))))
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
		  (memq (current-launcher-variant) '(script script-3m)))
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
	(make-mzscheme-program-launcher file collection (mzscheme-program-launcher-path name))))))
