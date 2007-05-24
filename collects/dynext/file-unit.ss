
(module file-unit mzscheme
  (require (lib "unit.ss")
           (lib "include.ss")
           (lib "string.ss"))

  (require "file-sig.ss")

  (provide dynext:file@)

  (define-unit dynext:file@
      (import)
      (export dynext:file^)

      (define (append-zo-suffix s)
	(path-replace-suffix s #".zo"))

      (define (append-c-suffix s)
	(path-replace-suffix s #".c"))

      (define (append-constant-pool-suffix s)
	(path-replace-suffix s #".kp"))

      (define (append-object-suffix s)
	(path-replace-suffix
	 s
	 (case (system-type)
	   [(unix beos macos macosx) #".o"]
	   [(windows) #".obj"])))

      (define (append-extension-suffix s)
	(path-replace-suffix s (system-type 'so-suffix)))

      (define (extract-suffix appender)
	(subbytes
	 (path->bytes (appender (bytes->path #"x")))
	 1))

      (define-values (extract-base-filename/ss
		      extract-base-filename/c
		      extract-base-filename/kp
		      extract-base-filename/o
		      extract-base-filename/ext)
	(let ([mk
	       (lambda (who pat kind simple)
		 (let ([rx (byte-pregexp
			    (string->bytes/latin-1 (format "^(.*)\\.(~a)$" pat)))])
		   (letrec ([extract-base-filename
			     (case-lambda
			      [(s p)
			       (unless (path-string? s)
				 (raise-type-error who "path or valid-path string" s))
			       (let ([m (regexp-match rx (path->bytes (if (path? s)
									  s
									  (string->path s))))])
				 (cond
				  [m (bytes->path (cadr m))]
				  [p (if simple
                                         (error p "not a ~a filename (doesn't end with ~a): ~a" kind simple s)
                                         (path-replace-suffix s #""))]
				  [else #f]))]
			      [(s) (extract-base-filename s #f)])])
		     extract-base-filename)))])
	  (values
	   (mk 'extract-base-filename/ss 
	       #"[sS][sS]|[sS][cC][mM]" "Scheme" #f)
	   (mk 'extract-base-filename/c
	       #"[cC]|[cC][cC]|[cC][xX][xX]|[cC][pP][pP]|[cC][+][+]|[mM]" "C" ".c, .cc, .cxx, .cpp, .c++, or .m")
	   (mk 'extract-base-filename/kp
	       #"[kK][pP]" "constant pool" ".kp")
	   (mk 'extract-base-filename/o
	       (case (system-type)
		 [(unix beos macos macosx) #"[oO]"]
		 [(windows) #"[oO][bB][jJ]"])
	       "compiled object"
	       (extract-suffix append-object-suffix))
	   (mk 'extract-base-filename/ext
               (regexp-quote (subbytes (system-type 'so-suffix) 1) #f)
	       "MzScheme extension"
	       (extract-suffix append-extension-suffix)))))))

