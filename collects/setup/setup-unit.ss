 
; Expects parameters to be set before invocation.
; Calls `exit' when done.

(module setup-unit mzscheme
  (require (lib "unitsig.ss")
	   (lib "unit.ss")
	   (lib "file.ss")
	   (lib "list.ss")
	   (lib "cm.ss")
	   (lib "port.ss")
           (lib "match.ss")
           (lib "planet-archives.ss" "planet")
           
	   "option-sig.ss"
	   (lib "sig.ss" "compiler")
	   (lib "launcher-sig.ss" "launcher")
           
	   "unpack.ss"
	   "getinfo.ss"
	   "plthome.ss")
  
  (provide setup@)
  
  (define setup@
    (unit/sig ()
      (import setup-option^
	      compiler^
	      (compiler:option : compiler:option^)
	      launcher^)
      
      (define setup-fprintf
	(lambda (p s . args)
	  (apply fprintf p (string-append "setup-plt: " s "~n") args)))
      
      (define setup-printf
	(lambda (s . args)
	  (apply setup-fprintf (current-output-port) s args)))
      
      (setup-printf "Setup version is ~a" (version))
      (setup-printf "PLT home directory is ~a" (path->string plthome))
      (setup-printf "Collection paths are ~a" (if (null? (current-library-collection-paths))
						  "empty!"
						  ""))
      (for-each (lambda (p)
		  (setup-printf "  ~a" (path->string p)))
		(current-library-collection-paths))
      
      (define (warning s x)
	(setup-printf s
		      (if (exn? x)
			  (exn-message x)
			  x)))
      
      (define (call-info info flag mk-default test)
	(if info
	    (let ([v (info flag mk-default)])
	      (test v)
	      v)
	    (mk-default)))
      
      (define mode-dir
	(if (compile-mode)
	    (build-path "compiled" (compile-mode))
	    (build-path "compiled")))
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;               Archive Unpacking               ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define x-specific-collections
	(apply 
	 append
	 (specific-collections)
	 (map (lambda (x) (unpack 
			   x 
			   plthome 
			   (lambda (s) (setup-printf "~a" s)) 
			   (current-target-directory-getter)
			   (force-unpacks)
			   (current-target-plt-directory-getter)))
	      (archives))))
      
      (define x-specific-planet-dirs (specific-planet-dirs))
      
      (define (done)
	(setup-printf "Done setting up"))
            
      (unless (null? (archives))
	(when (and (null? x-specific-collections) (null? x-specific-planet-dirs))
	  (done)
	  (exit 0))) ; done
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;              Find Collections                 ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define-struct cc (collection path name info info-path shadowing-policy) (make-inspector))
      
      (define (warning-handler v)
	(lambda (exn) 
	  (setup-printf 
	   "Warning: ~a"
	   (if (exn? exn)
	       (exn-message exn)
	       exn))
	  v))
      
      ;; collection->cc : listof path -> cc
      (define (collection->cc collection-p)
        (let ([root-dir (ormap (lambda (p)
                                 (parameterize ([current-library-collection-paths
                                                 (list p)])
                                   (and (with-handlers ([exn:fail? (lambda (x) #f)])
                                          (apply collection-path collection-p))
                                        p)))
                               (current-library-collection-paths))])
          (let* ([info (with-handlers ([exn:fail? (warning-handler #f)])
                         (get-info collection-p))]
                 [name (call-info info 'name (lambda () #f)
                                  (lambda (x)
                                    (when x
                                      (unless (string? x)
                                        (error 
                                         (format 
                                          "'name' result from collection ~s is not a string:"
                                          collection-p)
                                         x)))))])
            (and
             name
             (make-cc
              collection-p
              (apply collection-path collection-p)
              name
              info
              (build-path root-dir "info-domain" "compiled" "cache.ss")
              ;; by convention, all collections have "version" 1 0. This forces them
              ;; to conflict with each other.
              (list (cons 'lib (map path->string collection-p)) 1 0))))))
      
      ;; remove-falses : listof (union X #f) -> listof X
      ;; returns the non-false elements of l in order
      (define (remove-falses l) (filter (lambda (x) x) l))
      
      (define (planet->cc path owner pkg-file extra-path maj min)
        (unless (path? path)
            (error 'path->cc "non-path when building package ~a" pkg-file))
        (let/ec return
          (let* ([info (with-handlers ([exn:fail? (warning-handler #f)])
                         (get-info/full path))]
                 [name (call-info info 'name (lambda () (return #f))
                                  (lambda (x)
                                    (when x
                                      (unless (string? x)
                                        (error 
                                         (format 
                                          "'name' result from directory ~s is not a string:"
                                          path)
                                         x)))))])
            (make-cc
             #f
             path
             name
             info
             (get-planet-cache-path)
             (list `(planet ,owner ,pkg-file ,@extra-path) maj min)))))
      
      ;; planet-cc->sub-cc : cc (listof bytes [encoded path]) -> cc
      ;; builds a compilation job for the given subdirectory of the given cc
      ;; this is an awful hack
      (define (planet-cc->sub-cc cc subdir)
        (match-let ([(('planet owner pkg-file extra-path ...) maj min) (cc-shadowing-policy cc)])
          (planet->cc
           (apply build-path (cc-path cc) (map bytes->path subdir))
           owner
           pkg-file
           (append extra-path subdir)
           maj
           min)))

      (define (cannot-compile c)
	(error 'setup-plt "don't know how to compile collection: ~a" 
	       (if (= (length c) 1)
		   (car c)
		   c)))
      
      (define planet-dirs-to-compile 
        (remove-falses
         (map (lambda (spec) (apply planet->cc spec))
              (if (and (null? x-specific-collections) (null? x-specific-planet-dirs))
                  (get-installed-planet-archives)
                  x-specific-planet-dirs))))
      
      (define collections-to-compile
	(quicksort
	 (if (and (null? x-specific-collections) (null? x-specific-planet-dirs))
	     (let ([ht (make-hash-table 'equal)])
	       (let loop ([collection-paths (current-library-collection-paths)])
		 (cond
                   [(null? collection-paths) 
                    (hash-table-map ht (lambda (k v) v))]
                   [else (let* ([cp (car collection-paths)]
                                [cp-contents
                                 (if (directory-exists? cp)
                                     (directory-list cp)
                                     null)])
                           (let loop ([collections (filter
                                                    (lambda (x)
                                                      (directory-exists?
                                                       (build-path cp x)))
                                                    cp-contents)])
                             (cond
                               [(null? collections) (void)]
                               [else (let* ([collection (car collections)])
                                       (hash-table-get
                                        ht
                                        collection
                                        (lambda ()
                                          (let ([cc (collection->cc (list collection))])
                                            (when cc
                                              (hash-table-put! 
                                               ht
                                               collection
                                               cc))))))
                                     (loop (cdr collections))])))
                         (loop (cdr collection-paths))])))
	     (map
	      (lambda (c)
		(or (collection->cc (map string->path c))
		    (cannot-compile c)))
	      x-specific-collections))
	 (lambda (a b) (string-ci<? (cc-name a) (cc-name b)))))
      
      ;; Close over sub-collections
      (set! collections-to-compile
	    (let loop ([l collections-to-compile])
	      (if (null? l)
		  null
		  (let* ([cc (car l)]
			 [info (cc-info cc)])
		    (append
		     (map
		      (lambda (subcol)
			(or
			 (collection->cc (map string->path subcol))
			 (cannot-compile subcol)))
		      (call-info info 'compile-subcollections
				 ;; Default: subdirs with info.ss files
				 (lambda ()
				   (map
				    (lambda (l) (map path->string l))
				    (map (lambda (x) (append (cc-collection cc) (list x)))
					 (filter
					  (lambda (p)
                                            (let ((d (build-path (cc-path cc) p)))
                                              (and (directory-exists? d)
                                                   (file-exists? (build-path d "info.ss")))))
                                          (directory-list (cc-path cc))))))
				 ;; Result checker:
				 (lambda (x)
				   (unless (and (list? x)
						(andmap
						 (lambda (x)
						   (and (list? x)
							(andmap
							 (lambda (x)
							   (and (path-string? x)
								(relative-path? x)))
							 x)))
						 x))
				     (error "result is not a list of relative path string lists:" x)))))
		     (list cc)
		     (loop (cdr l)))))))

      (set! planet-dirs-to-compile
	    (let loop ([l planet-dirs-to-compile])
	      (if (null? l)
		  null
		  (let* ([cc (car l)]
			 [info (cc-info cc)])
		    (append
		     (remove-falses
                      (map
                       (lambda (p) 
                         (planet-cc->sub-cc 
                          cc 
                          (cond
                            [(path? p) (list (path->bytes p))]
                            [(and (list? p) (andmap bytes? p)) p]
                            [else (map (λ (s) (path->bytes (string->path s))) p)])))
                       (call-info info 'compile-subcollections
                                  (lambda ()
                                    (map (λ (p) (list (path->bytes p)))
                                         (filter
                                          (lambda (p)
                                            (let ((d (build-path (cc-path cc) p)))
                                              (and (directory-exists? d)
                                                   (file-exists? (build-path d "info.ss")))))
                                          (directory-list (cc-path cc)))))
                                  ;; Result checker:
                                  (λ (p)
                                    (match p
                                      [(((? (λ (v) (or (string? v) (bytes? v)))) ...) ...)
                                       (void)]
                                      [_ (error "result is not a list of lists of strings: " p)])))))
                     (list cc)
                     (loop (cdr l)))))))

      (define ccs-to-compile (append collections-to-compile planet-dirs-to-compile))
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;                  Helpers                      ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define (control-io-apply print-doing f args)
        (if (make-verbose)
            (begin
              (apply f args)
              #t)
            (let* ([oop (current-output-port)]
                   [printed? #f]
                   [on? #f]
                   [dir-table (make-hash-table 'equal)]
                   [line-accum #""]
                   [op (if (verbose)
                           (current-output-port)
                           (open-output-nowhere))] 
                   [doing-path (lambda (path)
                                 (unless printed?
                                   (set! printed? #t)
                                   (print-doing oop))
                                 (unless (verbose)
                                   (let ([path (normal-case-path (path-only path))])
                                     (unless (hash-table-get dir-table path (lambda () #f))
                                       (hash-table-put! dir-table path #t)
                                       (print-doing oop path)))))])
              (parameterize ([current-output-port op]
                             [compile-notify-handler doing-path])
                (apply f args)
                printed?))))
      
      (define errors null)
      (define (record-error cc desc go)
	(with-handlers ([exn:fail?
			 (lambda (x)
			   (if (exn? x)
			       (begin
				 (fprintf (current-error-port) "~a~n" (exn-message x)))
			       (fprintf (current-error-port) "~s~n" x))
			   (set! errors (cons (list cc desc x) errors)))])
	  (go)))
      (define (show-errors port)
	(for-each
	 (lambda (e)
	   (let ([cc (car e)]
		 [desc (cadr e)]
		 [x (caddr e)])
	     (setup-fprintf port
			    "Error during ~a for ~a (~a)"
			    desc (cc-name cc) (path->string (cc-path cc)))
	     (if (exn? x)
		 (setup-fprintf port "  ~a" (exn-message x))
		 (setup-fprintf port "  ~s" x))))
	 (reverse errors)))
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;                  Clean                        ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define (delete-file/record-dependency path dependencies)
	(when (regexp-match-positions #rx"[.]dep$" (path->bytes path))
	  (let ([deps (with-handlers ([exn:fail? (lambda (x) null)])
			(with-input-from-file path read))])
	    (when (and (pair? deps) (list? deps))
              (for-each (lambda (s)
			  (when (path-string? s)
			    (hash-table-put! dependencies s #t)))
			(map un-plthome-ify (cdr deps))))))
	(delete-file path))
      
      (define (delete-files-in-directory path printout dependencies)
	(for-each
	 (lambda (end-path)
	   (let ([path (build-path path end-path)])
	     (cond
               [(directory-exists? path)
                (void)]
               [(file-exists? path)
                (printout)
                (delete-file/record-dependency path dependencies)]
               [else (error 'delete-files-in-directory
                            "encountered ~a, neither a file nor a directory"
                            path)])))
	 (directory-list path)))
      
      (define (clean-collection cc dependencies)
        (record-error 
         cc
         "Cleaning"
         (lambda ()
           (let* ([info (cc-info cc)]
                  [default (box 'default)]
                  [paths (call-info
                          info
                          'clean
                          (lambda ()
                            (list mode-dir
                                  (build-path mode-dir "native")
                                  (build-path mode-dir "native" (system-library-subpath))))
                          (lambda (x)
                            (unless (or (eq? x default)
                                        (and (list? x)
                                             (andmap path-string? x)))
                              (error 'setup-plt "expected a list of path strings for 'clean, got: ~s"
                                     x))))]
                  [printed? #f]
                  [print-message
                   (lambda ()
                     (unless printed?
                       (set! printed? #t)
                       (setup-printf "Deleting files for ~a at ~a" (cc-name cc) (path->string (cc-path cc)))))])
             (for-each (lambda (path)
                         (let ([full-path (build-path (cc-path cc) path)])
                           (when (or (file-exists? full-path) (directory-exists? full-path))
                             (let loop ([path (find-relative-path (normalize-path (cc-path cc))
                                                                  (normalize-path full-path))])
                               (let loop ([path path])
                                 (let-values ([(base name dir?) (split-path path)])
                                   (cond
                                     [(path? base)
                                      (loop base)]
                                     [(eq? base 'relative)
                                      (when (eq? name 'up)
                                        (error 'clean
                                               "attempted to clean files in ~s which is not a subdirectory of ~s"
                                               full-path
                                               (cc-path cc)))]
                                     [else
                                      (error 'clean
                                             "attempted to clean files in ~s which is not a subdirectory of ~s"
                                             full-path
                                             (cc-path cc))]))))
                             
                             (cond
                               [(directory-exists? full-path)
                                (delete-files-in-directory
                                 full-path
                                 print-message
                                 dependencies)]
                               [(file-exists? full-path)
                                (delete-file/record-dependency full-path dependencies)
                                (print-message)]
                               [else (void)]))))
                       paths)))))
      
      (when (clean)
        (let ([dependencies (make-hash-table 'equal)])
          ;; Main deletion:
          (for-each (lambda (cc)
                      (clean-collection cc dependencies))
                    ccs-to-compile)
          ;; Unless specific collections were named, also
          ;;  delete .zos for referenced modules and delete
          ;;  info-domain cache
          (when (and (null? x-specific-collections) (null? x-specific-planet-dirs))
            (setup-printf "Checking dependencies")
            (let loop ([old-dependencies dependencies])
              (let ([dependencies (make-hash-table 'equal)]
                    [did-something? #f])
                (hash-table-for-each
                 old-dependencies
                 (lambda (file _)
                   (let-values ([(dir name dir?) (split-path file)])
                     (let ([base-name (path-replace-suffix name #"")])
                       (let ([zo (build-path dir mode-dir (format "~a.zo" base-name))]
                             [dep (build-path dir mode-dir (format "~a.dep" base-name))])
                         (when (and (file-exists? dep)
                                    (file-exists? zo))
                           (set! did-something? #t)
                           (setup-printf "  deleting ~a" zo)
                           (delete-file/record-dependency zo dependencies)
                           (delete-file/record-dependency dep dependencies)))))))
                (when did-something?
                  (loop dependencies))))
            (setup-printf "Clearing info-domain caches")
            (for-each (lambda (p)
                        (let ([fn (build-path p "info-domain" "compiled" "cache.ss")])
                          (when (file-exists? fn)
                            (with-handlers ([exn:fail:filesystem? (warning-handler (void))])
                              (with-output-to-file fn void 'truncate/replace)))))
                      (current-library-collection-paths)))))
      
      (when (or (make-zo) (make-so))
        (compiler:option:verbose (compiler-verbose))
        (compiler:option:compile-subcollections #f))
      
      (define (do-install-part part)
        (when (or (call-install) (eq? part 'post))
          (for-each
           (lambda (cc)
             (let/ec k
               (record-error
                cc
                (case part
                  [(pre)     "Early Install"]
                  [(general) "General Install"]
                  [(post)    "Post Install"])
                (lambda ()
                  (let ([fn (call-info
                             (cc-info cc)
                             (case part
                               [(pre)     'pre-install-collection]
                               [(general) 'install-collection]
                               [(post)    'post-install-collection])
                             (lambda () (k #f))
                             (lambda (v)
                               (unless (and (path-string? v)
                                            (relative-path? v))
                                 (error "result is not a relative path string: " v))
                               (let ([p (build-path (cc-path cc) v)])
                                 (unless (file-exists? p)
                                   (error "installer file does not exist: " p)))))])
                    (let ([installer
                           (with-handlers ([exn:fail? (lambda (exn)
                                                        (error 'setup-plt
                                                               "error loading installer: ~a"
                                                               (if (exn? exn) (exn-message exn) exn)))])
                             (dynamic-require (build-path (cc-path cc) fn)
                                              (case part
                                                [(pre)     'pre-installer]
                                                [(general) 'installer]
                                                [(post)    'post-installer])))])
                      (setup-printf "~aInstalling ~a"
                                    (case part [(pre) "Pre-"] [(post) "Post-"] [else ""])
                                    (cc-name cc))
		      (if (procedure-arity-includes? installer 2)
			  (installer plthome (cc-path cc))
			  (installer plthome))))))))
           ccs-to-compile)))
      
      (do-install-part 'pre)

      (define (make-it desc compile-directory get-namespace)
        ;; To avoid polluting the compilation with modules that are
        ;; already loaded, create a fresh namespace before calling
        ;; this function.
	;; To avoid keeping modules in memory across collections,
	;; pass `make-namespace' as `get-namespace', otherwise use
	;; `current-namespace' for `get-namespace'.
        (for-each (lambda (cc)
		    (parameterize ([current-namespace (get-namespace)])
		      (record-error
		       cc
		       (format "Compiling ~a" desc)
		       (lambda ()
			 (unless (control-io-apply
				  (case-lambda 
				   [(p) 
				    ;; Main "doing something" message
				    (setup-fprintf p "Compiling ~a used by ~a" 
						   desc (cc-name cc))]
				   [(p where)
				    ;; Doing something specifically in "where"
				    (setup-fprintf p "  in ~a" 
						   (path->string
						    (path->complete-path
						     where
						     (cc-path cc))))])
				  compile-directory
				  (list (cc-path cc) (cc-info cc)))
			   (setup-printf "No more ~a to compile for ~a" 
					 desc (cc-name cc))))))
                    (collect-garbage))
                  ccs-to-compile))
      
      (define orig-namespace (current-namespace))
      
      (define (with-specified-mode thunk)
        (if (not (compile-mode))
            (thunk)
            ;; Use the indicated mode
            (let ([zo-compile (with-handlers ([exn:fail?
                                               (lambda (exn)
                                                 (error 'setup-plt
                                                        "error loading compiler for mode ~s: ~s"
                                                        (compile-mode)
                                                        (if (exn? exn)
                                                            (exn-message exn)
                                                            exn)))])
                                (dynamic-require `(lib "zo-compile.ss" ,(compile-mode)) 'zo-compile))]
                  [orig-kinds (use-compiled-file-paths)]
                  [orig-compile (current-compile)])
              (parameterize ([current-namespace (make-namespace)]
                             [current-compile zo-compile]
                             [use-compiled-file-paths (list mode-dir)]
                             [current-compiler-dynamic-require-wrapper
                              (lambda (thunk)
                                (parameterize ([current-namespace orig-namespace]
                                               [use-compiled-file-paths orig-kinds]
                                               [current-compile orig-compile])
                                  (thunk)))])
                (thunk)))))
      
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;                  Make zo                      ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (when (make-zo) 
        (with-specified-mode
         (lambda ()
           (make-it ".zos" compile-directory-zos make-namespace))))
      (when (make-so) (make-it "extensions" compile-directory-extension current-namespace))
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;               Info-Domain Cache               ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (when (make-info-domain)
        ;; Each ht maps a collection root dir to an
        ;; info-domain table. Even when `collections-to-compile'
        ;; is a subset of all collections, we only care about
        ;; those collections that exist in the same root as
        ;; the ones in `collections-to-compile'.
        (let ([ht (make-hash-table 'equal)]
              [ht-orig (make-hash-table 'equal)])
          (for-each (lambda (cc)
                      (let ([domain (with-handlers ([exn:fail? (lambda (x)
                                                                 (lambda () null))])
                                      (dynamic-require
                                       (build-path (cc-path cc) "info.ss")
                                       '#%info-domain))])
                        ;; Check whether we have a table for this cc's info-domain cache:
                        (let ([t (hash-table-get ht 
                                                 (cc-info-path cc)
                                                 (lambda ()
                                                   ;; No table for this root, yet. Build one.
                                                   (let ([l (let ([p (cc-info-path cc)])
                                                              (if (file-exists? p)
                                                                  (with-handlers ([exn:fail? (warning-handler null)])
                                                                    (with-input-from-file p
                                                                      read))
                                                                  null))])
                                                     ;; Convert list to hash table. Incluse only well-formed
                                                     ;;  list elements, and only elements whose corresponding
                                                     ;;  collection exists.
                                                     (let ([t (make-hash-table 'equal)]
                                                           [all-ok? #f])
                                                       (when (list? l)
                                                         (set! all-ok? #t)
                                                         (for-each 
                                                          (lambda (i)
                                                            (match i
                                                              [((? (lambda (a) 
                                                                       (and (bytes? a)
                                                                            (file-exists? (build-path 
                                                                                           (bytes->path a)
                                                                                           "info.ss"))))
                                                                   a)
                                                                ((? symbol? b) ...) 
                                                                c
                                                                (? integer? d)
                                                                (? integer? e))
                                                               (hash-table-put! t a (list b c d e))]
                                                              [_ 
                                                               (set! all-ok? #f)]))
                                                          l))
                                                       ;; Record the table loaded for this collection root
                                                       ;; in the all-roots table:
                                                       (hash-table-put! ht (cc-info-path cc) t)
                                                       ;; If anything in the "cache.ss" file was bad,
                                                       ;; then claim that the old table was empty,
                                                       ;; so that we definitely write the new table.
                                                       (hash-table-put! ht-orig (cc-info-path cc) 
                                                                        (and all-ok? (hash-table-copy t)))
                                                       t))))])
                          ;; Add this collection's info to the table, replacing
                          ;; any information already there.
                          (hash-table-put! t 
                                           (path->bytes (cc-path cc))
                                           (cons (domain) (cc-shadowing-policy cc))))))
                    ccs-to-compile)
          ;; Write out each collection-root-specific table to a "cache.ss" file:
          (hash-table-for-each ht
                               (lambda (info-path ht)
                                 (unless (equal? ht (hash-table-get ht-orig info-path))
                                   (let-values ([(base name must-be-dir?) (split-path info-path)])
                                     (unless (path? base)
                                       (error 'make-info-domain "Internal error: cc had invalid info-path: ~s" info-path))
                                     (make-directory* base)
                                     (let ([p info-path])
                                       (setup-printf "Updating ~a" p)
                                       (with-handlers ([exn:fail? (warning-handler (void))])
                                         (with-output-to-file p
                                           (lambda ()
                                             (write (hash-table-map ht cons))
                                             (newline))
                                           'truncate/replace)))))))))
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;                  Make Launchers               ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (when (make-launchers)
        (let ([name-list
               (lambda (l)
                 (unless (and (list? l) (andmap (lambda (x) (and (path-string? x) (relative-path? x))) l))
                   (error "result is not a list of relative path strings:" l)))]
              [flags-list
               (lambda (l)
                 (unless (and (list? l) (andmap (lambda (fs) (andmap string? fs)) l))
                   (error "result is not a list of strings:" l)))]
              [or-f (lambda (f) (lambda (x) (when x (f x))))])
          (for-each
           (lambda (cc)
             (record-error
              cc
              "Launcher Setup"
              (lambda ()
                (let* ([info (cc-info cc)]
                       [make-launcher
                        (lambda (kind
                                 launcher-names
                                 launcher-libraries
                                 launcher-flags
                                 program-launcher-path
                                 make-launcher
                                 up-to-date?)
                          (let ([mzlns (call-info info launcher-names (lambda () null) name-list)]
                                [mzlls (call-info info launcher-libraries (lambda () #f) (or-f name-list))]
                                [mzlfs (call-info info launcher-flags (lambda () #f) (or-f flags-list))])
                            (cond
                              [(null? mzlns) (void)]
                              [(not (or mzlls mzlfs))
                               (unless (null? mzlns)
                                 (setup-printf
                                  "Warning: ~a launcher name list ~s has no matching library/flags lists"
                                  kind mzlns))]
                              [(and (or (not mzlls) (= (length mzlns) (length mzlls)))
                                    (or (not mzlfs) (= (length mzlns) (length mzlfs))))
                               (for-each
                                (lambda (mzln mzll mzlf)
                                  (let ([p (program-launcher-path mzln)]
                                        [aux (cons `(exe-name . ,mzln)
                                                   (build-aux-from-path
                                                    (build-path (cc-path cc)
                                                                (path-replace-suffix (or mzll mzln) #""))))])
                                    (unless (up-to-date? p aux)
                                      (setup-printf "Installing ~a~a launcher ~a"
                                                    kind (if (eq? (current-launcher-variant) 'normal)
                                                             ""
                                                             (current-launcher-variant))
                                                    (path->string p))
                                      (make-launcher
                                       (or mzlf
                                           (if (and (cc-collection cc)
                                                    (= 1 (length (cc-collection cc))))
                                               ;; Common case (simpler parsing for Windows to
                                               ;; avoid cygwin bug):
                                               (list "-qmvL-" mzll (path->string (car (cc-collection cc))))
                                               (list "-qmvt-" (format "~a" (path->string (build-path (cc-path cc) mzll))))))
                                       p
                                       aux))))
                                mzlns
                                (or mzlls (map (lambda (_) #f) mzlns))
                                (or mzlfs (map (lambda (_) #f) mzlns)))]
                              [else
                               (let ([fault (if (or (not mzlls) (= (length mzlns) (length mzlls))) 'f 'l)])
                                 (setup-printf
                                  "Warning: ~a launcher name list ~s doesn't match ~a list; ~s"
                                  kind mzlns
                                  (if (eq? 'l fault) "library" "flags")
                                  (if (eq? fault 'l) mzlls mzlfs)))])))])
                  (for-each
                   (lambda (variant)
                     (parameterize ([current-launcher-variant variant])
                       (make-launcher
                        "MrEd"
                        'mred-launcher-names
                        'mred-launcher-libraries
                        'mred-launcher-flags
                        mred-program-launcher-path
                        make-mred-launcher
                        mred-launcher-up-to-date?)))
                   (available-mred-variants))
                  (for-each
                   (lambda (variant)
                     (parameterize ([current-launcher-variant variant])
                       (make-launcher
                        "MzScheme"
                        'mzscheme-launcher-names
                        'mzscheme-launcher-libraries
                        'mzscheme-launcher-flags
                        mzscheme-program-launcher-path
                        make-mzscheme-launcher
                        mzscheme-launcher-up-to-date?)))
                   (available-mzscheme-variants))))))
           ccs-to-compile)))
      
      (do-install-part 'general)
      (do-install-part 'post)
      
      (done)
      
      (unless (null? errors)
        (setup-printf "")
        (show-errors (current-error-port))
        (when (pause-on-errors)
          (fprintf (current-error-port)
                   "INSTALLATION FAILED.~nPress Enter to continue...~n")
          (read-line))
        (exit 1))
      
      (exit 0))))
