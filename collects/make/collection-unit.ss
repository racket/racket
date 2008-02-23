
(module collection-unit mzscheme
  (require mzlib/unit
	   mzlib/list
	   mzlib/file)

  (require "collection-sig.ss")
  (require "make-sig.ss")

  (require compiler/sig
	   dynext/file-sig)

  (provide make:collection@)

  (define-unit make:collection@
	(import make^
	      dynext:file^
	      (prefix compiler:option: compiler:option^)
	      compiler^)
        (export make:collection^)

      (define (make-collection
	       collection-name
	       collection-files
	       argv)
	(printf "building collection ~a: ~a~n" collection-name collection-files)
	(let* ([zo-compiler #f]
	       [ext-compiler #f]
	       [dest-dir (build-path "compiled" "native" (system-library-subpath))]
	       [src-dir (current-directory)]
	       [sses (sort collection-files
                           (lambda (a b) (string-ci<? (path->string a) (path->string b))))]
	       [bases (map (lambda (src)
			     (extract-base-filename/ss src 'make-collection-extension))
			   sses)]
	       [cs (map 
		    (lambda (base)
		      (build-path
		       "compiled"
		       "native"
		       (append-c-suffix base)))
		    bases)]
	       [zos (map 
		     (lambda (base)
		       (build-path
			"compiled"
			(append-zo-suffix base)))
		     bases)]
	       [kps (map 
		     (lambda (base)
		       (build-path
			"compiled"
			"native"
			(append-constant-pool-suffix base)))
		     bases)]
	       [objs (map
		      (lambda (base)
			(build-path
			 dest-dir
			 (append-object-suffix base)))
		      bases)]
	       [ss->zo-list
		(map (lambda (ss zo)
		       `(,zo (,ss)
			     ,(lambda ()
				(unless zo-compiler
				  (set! zo-compiler (compile-zos #f)))
				(zo-compiler (list ss) "compiled"))))
		     sses zos)])
	  (unless (directory-exists? "compiled") (make-directory "compiled"))
	  (unless (or (equal? argv #("zo"))
		      (directory-exists? dest-dir))
	    (make-directory* dest-dir))
	  (make/proc
	   (append
	    `(("zo" ,zos))
	    ss->zo-list)
	   argv)))))
