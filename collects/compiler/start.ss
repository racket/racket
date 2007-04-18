;; Starts up the compiler according to command-line flags.
;; (c) 1997-2001 PLT

;; Scheme->C compilation is the only mode really handled
;;  by the code in this collection. Other modes are handled
;;  by other collections, such as MzLib and dynext.
;; If you are interested Scheme->C part of mzc, look in
;;  driver.ss, which is the `main' file for the compiler.

;; Different compilation modes are driven by dynamically
;;  linking in appropriate libraries. This is handled
;;  by compiler.ss.

;; See doc.txt for information about the Scheme-level interface
;;  provided by this collection.

(module start mzscheme

  ;; On error, exit with 1 status code
  (error-escape-handler (lambda () (exit 1)))

  (error-print-width 512)

  (require (prefix compiler:option: "option.ss"))
  (require "compiler.ss")

  ;; Read argv array for arguments and input file name
  (require (lib "cmdline.ss")
	   (lib "list.ss")
	   (lib "file.ss" "dynext")
	   (lib "compile.ss" "dynext")
	   (lib "link.ss" "dynext")
	   (lib "pack.ss" "setup")
	   (lib "getinfo.ss" "setup")
	   (lib "dirs.ss" "setup"))

  (define dest-dir (make-parameter #f))
  (define auto-dest-dir (make-parameter #f))

  (define ld-output (make-parameter #f))

  (define exe-output (make-parameter #f))
  (define exe-embedded-flags (make-parameter '("-m" "-v" "-U" "-q" "--")))
  (define exe-embedded-libraries (make-parameter null))
  (define exe-aux (make-parameter null))
  (define exe-embedded-collects-path (make-parameter #f))
  (define exe-embedded-collects-dest (make-parameter #f))
  (define exe-dir-add-collects-dirs (make-parameter null))

  (define exe-dir-output (make-parameter #f))

  (define module-mode (make-parameter #f))

  (define default-plt-name "archive")

  (define plt-output (make-parameter #f))
  (define plt-name (make-parameter default-plt-name))
  (define plt-files-replace (make-parameter #f))
  (define plt-files-plt-relative? (make-parameter #f))
  (define plt-files-plt-home-relative? (make-parameter #f))
  (define plt-force-install-dir? (make-parameter #f))
  (define plt-setup-collections (make-parameter null))
  (define plt-include-compiled (make-parameter #f))

  (define stop-at-source (make-parameter #f))

  (define (extract-suffix appender)
    (bytes->string/latin-1
     (subbytes
      (path->bytes (appender (bytes->path #"x")))
      1)))


  ;; Returns (values mode files prefixes)
  ;;  where mode is 'compile, 'link, or 'zo
  (define (parse-options argv)
    (parse-command-line
     "mzc"
     argv
     `([help-labels
	"-------------------------------- mode flags ---------------------------------"]
       [once-any
	[("-e" "--extension")
	 ,(lambda (f) 'compile)
	 (,(format "Output ~a file(s) from Scheme source(s) (default)" (extract-suffix append-extension-suffix)))]
	[("-c" "--c-source")
	 ,(lambda (f) 'compile-c)
	 (,(format "Output ~a file(s) from Scheme source(s)" (extract-suffix append-c-suffix)))]
	[("-o" "--object")
	 ,(lambda (f) 'compile-o)
	 (,(format "Output ~a/~a from Scheme source(s) for a multi-file extension" 
		   (extract-suffix append-object-suffix)
		   (extract-suffix append-constant-pool-suffix)))]
	[("-l" "--link-extension")
	 ,(lambda (f) 'link)
	 (,(format "Link ~a and ~a from Scheme sources into a ~a file"
		   (extract-suffix append-object-suffix)
		   (extract-suffix append-constant-pool-suffix)
		   (extract-suffix append-extension-suffix)))]
	[("-g" "--link-glue")
	 ,(lambda (f) 'link-glue)
	 (,(format "Create the ~a glue for --link-extension, but do not link"
		   (extract-suffix append-object-suffix)))]
	[("-z" "--zo")
	 ,(lambda (f) 'zo)
	 (,(format "Output ~a file(s) from Scheme source(s)" (extract-suffix append-zo-suffix)))]
	[("-k" "--make")
	 ,(lambda (f) 'make-zo)
	 ("Like --zo, but uses .dep, recurs for imports, implies --auto-dir")]
	[("--collection-extension")
	 ,(lambda (f) 'collection-extension)
	 ("Compile specified collection to extension")]
	[("--collection-zos")
	 ,(lambda (f) 'collection-zos)
	 (,(format "Compile specified collection to ~a files" (extract-suffix append-zo-suffix)))]
	[("--cc")
	 ,(lambda (f) 'cc)
	 (,(format "Compile arbitrary file(s) for an extension: ~a -> ~a" 
		   (extract-suffix append-c-suffix)
		   (extract-suffix append-object-suffix)))]
	[("--ld")
	 ,(lambda (f name) (ld-output name) 'ld)
	 (,(format "Link arbitrary file(s) to create <extension>: ~a -> ~a" 
		   (extract-suffix append-object-suffix)
		   (extract-suffix append-extension-suffix))
	  "extension")]
	[("--xform")
	 ,(lambda (f) 'xform)
	 (,(format "Convert for 3m compilation: ~a -> ~a" 
		   (extract-suffix append-c-suffix)
		   (extract-suffix append-c-suffix)))]
	[("--exe")
	 ,(lambda (f name) (exe-output name) 'exe)
	 (,(format "Embed module in MzScheme to create <exe>")
	  "exe")]
	[("--gui-exe")
	 ,(lambda (f name) (exe-output name) 'gui-exe)
	 (,(format "Embed module in MrEd to create <exe>")
	  "exe")]
	[("--exe-dir")
	 ,(lambda (f name) (exe-dir-output name) 'exe-dir)
	 (,(format "Combine executables with support files in <dir>")
	  "dir")]
	[("--collection-plt")
	 ,(lambda (f name) (plt-output name) 'plt-collect)
	 (,(format "Create .plt <archive> containing collections")
	  "archive")]
	[("--plt")
	 ,(lambda (f name) (plt-output name) 'plt)
	 (,(format "Create .plt <archive> containing relative files/dirs")
	  "archive")]]
       [once-any
	[("--3m")
	 ,(lambda (f) (compiler:option:3m #t))
	 (,(format "Compile/link for 3m, with -e/-c/-o/--exe/etc.~a"
                   (if (eq? '3m (system-type 'gc)) " [current default]" "")))]
	[("--cgc")
	 ,(lambda (f) (compiler:option:3m #f))
	 (,(format "Compile/link for CGC, with -e/-c/-o/--exe/etc.~a"
                   (if (eq? 'cgc (system-type 'gc)) " [current default]" "")))]]
       [once-each
	[("-m" "--module")
	 ,(lambda (f) (module-mode #t))
	 ("Skip eval of top-level syntax, etc. for -e/-c/-o/-z")]
	[("--embedded")
	 ,(lambda (f) (compiler:option:compile-for-embedded #t))
	 ("Compile for embedded run-time engine, with -c/-o/-g")]
	[("--source")
	 ,(lambda (f) (stop-at-source #t))
	 (,(format "Stop at ~a instead of ~a for -o/-g"
		   (extract-suffix append-c-suffix)
		   (extract-suffix append-object-suffix)))]
	[("-p" "--prefix") 
	 ,(lambda (f v) v)
	 ("Add elaboration-time prefix file for -e/-c/-o/-z" "file")]
	[("-n" "--name") 
	 ,(lambda (f name) (compiler:option:setup-prefix name))
	 ("Use <name> as extra part of public low-level names" "name")]]
       [once-any
	[("-d" "--destination") 
	 ,(lambda (f d)
	    (unless (directory-exists? d)
	      (error 'mzc "the destination directory does not exist: ~s" d))
	    (dest-dir d))
	 ("Output -e/-c/-o/-l/-g/-z file(s) to <dir>" "dir")]
	[("--auto-dir")
	 ,(lambda (f)
	    (auto-dest-dir #t))
	 (,(format "Output -z to \"compiled\", -e to ~s"
		   (path->string
		    (build-path "compiled" "native" (system-library-subpath #f)))))]]

       [help-labels
	"------------------- compiler/linker configuration flags ---------------------"]
       [once-each
	[("--tool") 
	 ,(lambda (f v) 
	    (let ([v (string->symbol v)])
	      (use-standard-compiler v)
	      (use-standard-linker v)))
	 (,(format "Use pre-defined <tool> as C compiler/linker:~a" 
		   (apply string-append
			  (apply append
				 (map (lambda (t)
					(list " " (symbol->string t)))
				      (get-standard-compilers)))))
	  "tool")]
	[("--compiler") 
	 ,(lambda (f v) (current-extension-compiler v))
	 ("Use <compiler-path> as C compiler" "compiler-path")]]
       [multi
	[("++ccf") 
	 ,(lambda (f v) (current-extension-compiler-flags
			 (append (current-extension-compiler-flags)
				 (list v))))
	 ("Add C compiler flag" "flag")]
	[("--ccf") 
	 ,(lambda (f v) (current-extension-compiler-flags
			 (remove v (current-extension-compiler-flags))))
	 ("Remove C compiler flag" "flag")]
	[("--ccf-clear") 
	 ,(lambda (f) (current-extension-compiler-flags null))
	 ("Clear C compiler flags")]
	[("--ccf-show") 
	 ,(lambda (f) 
	    (printf "C compiler flags: ~s~n" (expand-for-link-variant
					      (current-extension-compiler-flags))))
	 ("Show C compiler flags")]]
       [once-each
	[("--linker") 
	 ,(lambda (f v) (current-extension-linker v))
	 ("Use <linker-path> as C linker" "linker-path")]]
       [multi
	[("++ldf") 
	 ,(lambda (f v) (current-extension-linker-flags
			 (append (current-extension-linker-flags)
				 (list v))))
	 ("Add C linker flag" "flag")]
	[("--ldf") 
	 ,(lambda (f v) (current-extension-linker-flags
			 (remove v (current-extension-linker-flags))))
	 ("Remove C linker flag" "flag")]
	[("--ldf-clear") 
	 ,(lambda (f) (current-extension-linker-flags null))
	 ("Clear C linker flags")]
	[("--ldf-show") 
	 ,(lambda (f) 
	    (printf "C linker flags: ~s~n" (expand-for-link-variant
					    (current-extension-linker-flags))))
	 ("Show C linker flags")]
	[("++ldl") 
	 ,(lambda (f v) (current-standard-link-libraries
			 (append (current-standard-link-libraries)
				 (list v))))
	 ("Add C linker library" "lib")]
	[("--ldl-show") 
	 ,(lambda (f) 
	    (printf "C linker libraries: ~s~n" (expand-for-link-variant
						(current-standard-link-libraries))))
	 ("Show C linker libraries")]]
       [multi
	[("++cppf") 
	 ,(lambda (f v) (current-extension-preprocess-flags
			 (append (current-extension-preprocess-flags)
				 (list v))))
	 ("Add C preprocess (xform) flag" "flag")]
	[("--cppf") 
	 ,(lambda (f v) (current-extension-preprocess-flags
			 (remove v (current-extension-preprocess-flags))))
	 ("Remove C preprocess (xform) flag" "flag")]
	[("--cppf-clear") 
	 ,(lambda (f) (current-extension-preprocess-flags null))
	 ("Clear C preprocess (xform) flags")]
	[("--cppf-show") 
	 ,(lambda (f) 
	    (printf "C compiler flags: ~s~n" (expand-for-link-variant
					      (current-extension-preprocess-flags))))
	 ("Show C preprocess (xform) flags")]]
       [help-labels
	"--------------------- executable configuration flags ------------------------"]
       [once-each
	[("--collects-path")
	 ,(lambda (f i)
	    (exe-embedded-collects-path i))
	 ("Set <path> main collects in --[gui-]exe/--exe-dir" "path")]
	[("--collects-dest")
	 ,(lambda (f i)
	    (exe-embedded-collects-dest i))
	 ("Add --[gui-]exe collection code to <dir>" "dir")]
	[("--ico")
	 ,(lambda (f i) (exe-aux
			 (cons (cons 'ico i)
			       (exe-aux))))
	 ("Windows icon for --[gui-]exe executable" ".ico-file")]
	[("--icns")
	 ,(lambda (f i) (exe-aux
			 (cons (cons 'icns i)
			       (exe-aux))))
	 ("Mac OS X icon for --[gui-]exe executable" ".icns-file")]
	[("--orig-exe")
	 ,(lambda (f) (exe-aux 
		       (cons (cons 'original-exe? #t)
			     (exe-aux))))
	 ("Use original executable for --[gui-]exe instead of stub")]]
       [multi
	[("++lib")
	 ,(lambda (f l c) (exe-embedded-libraries
			   (append (exe-embedded-libraries)
				   (list (list l c)))))
	 ("Embed <lib> from <collect> in --[gui-]exe executable" "lib" "collect")]
	[("++collects-copy")
	 ,(lambda (f d) (exe-dir-add-collects-dirs
			 (append (exe-dir-add-collects-dirs)
				 (list d))))
	 ("Add collects in <dir> to --exe-dir" "dir")]
	[("++exf") 
	 ,(lambda (f v) (exe-embedded-flags
			 (append (exe-embedded-flags)
				 (list v))))
	 ("Add flag to embed in --[gui-]exe executable" "flag")]
	[("--exf") 
	 ,(lambda (f v) (exe-embedded-flags
			 (remove v (exe-embedded-flags))))
	 ("Remove flag to embed in --[gui-]exe executable" "flag")]
	[("--exf-clear") 
	 ,(lambda (f) (exe-embedded-flags null))
	 ("Clear flags to embed in --[gui-]exe executable")]
	[("--exf-show") 
	 ,(lambda (f) 
	    (printf "Flags to embed: ~s~n" (exe-embedded-flags)))
	 ("Show flag to embed in --[gui-]exe executable")]]
       [help-labels
	"----------------------------- .plt archive flags ----------------------------"]
       [once-each
	[("--plt-name")
	 ,(lambda (f n) (plt-name n))
	 ("Set the printed <name> describing the archive" "name")]
	[("--replace")
	 ,(lambda (f) (plt-files-replace #t))
	 ("Files in archive replace existing files when unpacked")]
	[("--at-plt")
	 ,(lambda (f) (plt-files-plt-relative? #t))
	 ("Files/dirs in archive are relative to user's add-ons directory")]]
       [once-any
	[("--all-users")
	 ,(lambda (f) (plt-files-plt-home-relative? #t))
	 ("Files/dirs in archive go to PLT installation if writable")]
	[("--force-all-users")
	 ,(lambda (f) 
	    (plt-files-plt-home-relative? #t)
	    (plt-force-install-dir? #t))
	 ("Files/dirs forced to PLT installation")]]
       [once-each
	[("--include-compiled")
	 ,(lambda (f) (plt-include-compiled #t))
	 ("Include \"compiled\" subdirectories in the archive")]]
       [multi
	[("++setup")
	 ,(lambda (f c) (plt-setup-collections
			 (append (plt-setup-collections)
				 (list c))))
	 ("Setup <collect> after the archive is unpacked" "collect")]]
       [help-labels
	"----------------------- compiler optimization flags -------------------------"]

       [once-each
	[("--no-prop")
	 ,(lambda (f) (compiler:option:propagate-constants #f))
	 ("Don't propagate constants")]
	[("--inline")
	 ,(lambda (f d) (compiler:option:max-inline-size 
			 (with-handlers ([void
					  (lambda (x)
					    (error 'mzc "bad size for --inline: ~a" d))])
			   (let ([v (string->number d)])
			     (unless (and (not (negative? v)) (exact? v) (real? v))
			       (error 'bad))
			     v))))
	 ("Set the maximum inlining size" "size")]
	[("--prim")
	 ,(lambda (f) (compiler:option:assume-primitives #t))
	 ("Assume primitive bindings at top level")]
	[("--stupid")
	 ,(lambda (f) (compiler:option:stupid #t))
	 ("Compile despite obvious non-syntactic errors")]
	[("--unsafe-disable-interrupts")
	 ,(lambda (f) (compiler:option:disable-interrupts #t))
	 ("Ignore threads, breaks, and stack overflow")]
	[("--unsafe-skip-tests")
	 ,(lambda (f) (compiler:option:unsafe #t))
	 ("Skip run-time tests for some primitive operations")]
	[("--unsafe-fixnum-arithmetic")
	 ,(lambda (f) (compiler:option:fixnum-arithmetic #t))
	 ("Assume fixnum arithmetic yields a fixnum")]]
       [help-labels
	"-------------------------- miscellaneous flags ------------------------------"]
       [once-each
	[("-v") 
	 ,(lambda (f) (compiler:option:verbose #t))
	 ("Verbose mode")]
	[("--save-temps")
	 ,(lambda (f) (compiler:option:clean-intermediate-files #f))
	 ("Keep intermediate files")]
	[("--debug")
	 ,(lambda (f) (compiler:option:debug #t))
	 ("Write debugging output to dump.txt")]])
     (lambda (accum file . files)
       (let ([mode (let ([l (filter symbol? accum)])
		     (if (null? l)
			 'compile
			 (car l)))])
	 (values 
	  mode
	  (cons file files)
	  (let ([prefixes (filter string? accum)])
	    (unless (memq mode '(compile compile-c compile-o zo))
	      (unless (null? prefixes)
		(error 'mzc "prefix files are not useful in ~a mode" mode)))
	    (if (module-mode)
		(begin
		  (when (compiler:option:assume-primitives)
		    (error 'mzc "--prim is not useful with -m or --module"))
		  (unless (null? prefixes)
		    (error 'mzc "prefix files not allowed with -m or --module"))
		  #f)
		`(begin
		   ,(if (compiler:option:assume-primitives) '(require mzscheme) '(void))
		   (require (lib "cffi.ss" "compiler"))
		   (require-for-syntax mzscheme)
		   ,@(map (lambda (s) `(load ,s)) prefixes)
		   (void)))))))
     (list "file/directory/collection" "file/directory/sub-collection")))

  (printf "mzc v~a [~a], Copyright (c) 2004-2007 PLT Scheme Inc.~n"
	  (version)
          (system-type 'gc))

  (define-values (mode source-files prefix)
    (parse-options (current-command-line-arguments)))

  (when (auto-dest-dir)
    (unless (memq mode '(zo compile))
      (error 'mzc "--auto-dir works only with -z, --zo, -e, or --extension (or default mode)")))

  (define (never-embedded action)
    (when (compiler:option:compile-for-embedded)
      (error 'mzc "cannot ~a an extension for an embedded MzScheme" action)))
  
  (if (compiler:option:3m)
      (begin
        (link-variant '3m)
        (compile-variant '3m))
      (begin
        (link-variant 'cgc)
        (compile-variant 'cgc)))
  
  (case mode
    [(compile)
     (never-embedded "compile")
     ((compile-extensions prefix) source-files (if (auto-dest-dir)
						   'auto
						   (dest-dir)))]
    [(compile-c)
     ((compile-extensions-to-c prefix) source-files (dest-dir))]
    [(compile-o)
     (((if (stop-at-source) compile-extension-parts-to-c compile-extension-parts) prefix)
      source-files (dest-dir))]
    [(link)
     (never-embedded "link")
     (link-extension-parts source-files (or (dest-dir) (current-directory)))]
    [(link-glue)
     ((if (stop-at-source) glue-extension-parts-to-c glue-extension-parts)
      source-files (or (dest-dir) (current-directory)))]
    [(zo)
     ((compile-zos prefix) source-files (if (auto-dest-dir)
					    'auto
					    (dest-dir)))]
    [(make-zo)
     (let ([n (make-namespace)]
	   [mc (dynamic-require '(lib "cm.ss")
				'managed-compile-zo)]
	   [cnh (dynamic-require '(lib "cm.ss")
				 'manager-compile-notify-handler)]
	   [did-one? #f])
       (parameterize ([current-namespace n]
		      [cnh (lambda (p)
			     (set! did-one? #t)
			     (printf "  making ~s~n" (path->string p)))])
	   (map (lambda (file)
		  (unless (file-exists? file)
		    (error 'mzc "file does not exist: ~a" file))
		  (set! did-one? #f)
		  (let ([name (extract-base-filename/ss file 'mzc)])
		    (printf "\"~a\":~n" file)
		    (mc file)
		    (let ([dest (append-zo-suffix 
				 (let-values ([(base name dir?) (split-path name)])
				   (build-path (if (symbol? base) 'same base)
					       "compiled" name)))])
		      (printf " [~a \"~a\"]~n"
			      (if did-one?
				  "output to"
				  "already up-to-date at")
			      dest))))
		source-files)))]
    [(collection-extension)
     (apply compile-collection-extension source-files)]
    [(collection-zos)
     (apply compile-collection-zos source-files)]
    [(cc)
     (for-each
      (lambda (file)
	(let* ([base (extract-base-filename/c file 'mzc)]
	       [dest (append-object-suffix 
		      (let-values ([(base name dir?) (split-path base)])
			(build-path (or (dest-dir) 'same) name)))])
	  (printf "\"~a\":~n" file)
	  (compile-extension (not (compiler:option:verbose))
			     file
			     dest
			     null)
	  (printf " [output to \"~a\"]~n" dest)))
      source-files)]
    [(ld)
     (extract-base-filename/ext (ld-output) 'mzc)
     ;; (for-each (lambda (file) (extract-base-filename/o file 'mzc)) source-files)
     (let ([dest (if (dest-dir)
		     (build-path (dest-dir) (ld-output))
		     (ld-output))])
       (printf "~a:~n" (let ([s (apply string-append
				       (map (lambda (n) (format " \"~a\"" n)) source-files))])
			 (substring s 1 (string-length s))))
       (link-extension (not (compiler:option:verbose))
		       source-files
		       dest)
       (printf " [output to \"~a\"]~n" dest))]
    [(xform)
     (for-each (lambda (file)
		 (let ([out-file (path-replace-suffix file ".3m.c")])
		   ((dynamic-require '(lib "xform.ss" "compiler") 'xform)
		    (not (compiler:option:verbose))
		    file
		    out-file
		    (list (find-include-dir)))
		   (printf " [output to \"~a\"]~n" out-file)))
	       source-files)]
    [(exe gui-exe)
     (unless (= 1 (length source-files))
       (error 'mzc "expected a single module source file to embed; given: ~e"
	      source-files))
     (let ([dest ((dynamic-require '(lib "embed.ss" "compiler" "private") 
				   'mzc:embedding-executable-add-suffix)
		  (exe-output)
		  (eq? mode 'gui-exe))])
       ((dynamic-require '(lib "embed.ss" "compiler" "private") 
			 'mzc:create-embedding-executable)
	dest
	#:mred? (eq? mode 'gui-exe) 
	#:variant (if (compiler:option:3m) '3m 'cgc)
	#:verbose? (compiler:option:verbose)
	#:modules (cons
		   `(#%mzc: (file ,(car source-files)))
		   (map (lambda (l)
			  `(#t (lib ,@l)))
			(exe-embedded-libraries)))
	#:literal-expression `(require ,(string->symbol
					 (format
					  "#%mzc:~a"
					  (let-values ([(base name dir?) (split-path (car source-files))])
					    (path->bytes (path-replace-suffix name #""))))))
	#:cmdline (let ([flags (exe-embedded-flags)])
		    (if (eq? mode 'gui-exe) 
			(cons "-Z" flags)
			flags))
	#:collects-path (exe-embedded-collects-path)
	#:collects-dest (exe-embedded-collects-dest)
	#:aux (exe-aux))
       (printf " [output to \"~a\"]~n" dest))]
    [(exe-dir)
     ((dynamic-require '(lib "distribute.ss" "compiler") 
		       'assemble-distribution)
      (exe-dir-output)
      source-files
      #:collects-path (exe-embedded-collects-path)
      #:copy-collects (exe-dir-add-collects-dirs))
     (printf " [output to \"~a\"]~n" (exe-dir-output))]
    [(plt)
     (for-each (lambda (fd)
		 (unless (relative-path? fd)
		   (error
		    'mzc
		    "file/directory is not relative to the current directory: \"~a\""
		    fd)))
	       source-files)
     (pack-plt (plt-output) (plt-name)
	       source-files
	       #:collections (map list (plt-setup-collections))
	       #:file-mode (if (plt-files-replace)
			       'file-replace
			       'file)
	       #:plt-relative? (or (plt-files-plt-relative?)
				   (plt-files-plt-home-relative?))
	       #:at-plt-home? (plt-files-plt-home-relative?)
	       #:test-plt-dirs (if (or (plt-force-install-dir?)
				       (not (plt-files-plt-home-relative?)))
				   #f
				   (list "collects" "doc" "include" "lib"))
	       #:requires
	       ;; Get current version of mzscheme for require:
	       (let ([i (get-info '("mzscheme"))])
		 (let ([v (and i (i 'version (lambda () #f)))])
		   (list (list '("mzscheme") v)))))
     (printf " [output to \"~a\"]~n" (plt-output))]
    [(plt-collect)
     (pack-collections-plt
      (plt-output)
      (if (eq? default-plt-name (plt-name))
	  #f
	  (plt-name))
      (map (lambda (sf)
	     (let loop ([sf sf])
	       (let ([m (regexp-match "^([^/]*)/(.*)$" sf)])
		 (if m
		     (cons (cadr m) (loop (caddr m)))
		     (list sf)))))
	   source-files)
      #:replace? (plt-files-replace)
      #:extra-setup-collections (map list (plt-setup-collections))
      #:file-filter (if (plt-include-compiled)
			(lambda (path)
			  (or (regexp-match #rx"compiled$" path)
			      (std-filter path)))
			std-filter)
      #:at-plt-home? (plt-files-plt-home-relative?)
      #:test-plt-collects? (not (plt-force-install-dir?)))
     (printf " [output to \"~a\"]~n" (plt-output))]
    [else (printf "bad mode: ~a~n" mode)]))
