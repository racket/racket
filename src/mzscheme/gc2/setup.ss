
(when (directory-exists? "xform-collects")
  (printf "Removing old xform-collects tree...\n")
  (let loop ([dir "xform-collects"])
    (for-each (lambda (x)
		(let ([x (build-path dir x)])
		  (when (file-exists? x)
		    (delete-file x))
		  (when (directory-exists? x)
		    (loop x))))
	      (directory-list dir))))

(printf "Copying tree...\n")
  
(use-compiled-file-paths null)

(unless (with-handlers ([exn:fail:filesystem? (lambda (x) #f)])
	  (collection-path "mzlib"))
  (let ([p (build-path (current-load-relative-directory)
		       'up
		       'up
		       'up
		       "collects")])
    (printf "Setting collection path: ~s~n" p)
    (current-library-collection-paths 
     (list p))))

(require (lib "moddep.ss" "syntax")
	 (lib "cm.ss"))

(define (go mod-path rel-to target)
  (let ([path (if target
		  mod-path
		  (if (module-path-index? mod-path)
		      (resolve-module-path-index mod-path rel-to)
		      (resolve-module-path mod-path rel-to)))])
    ;; Copy file to here:
    (let ([target 
	   (or target
	       (let-values ([(src-base rel-path)
			     (let loop ([path (simplify-path path)][accum null])
			       (let-values ([(base name dir?) (split-path path)])
				 (if (string=? (path->string name) "collects")
				     (values base (cons "xform-collects" accum))
				     (loop base (cons name accum)))))])
		 (let loop ([place (current-directory)][rel-path rel-path])
		   (if (null? (cdr rel-path))
		       (build-path place (car rel-path))
		       (let ([next (build-path place (car rel-path))])
			 (unless (directory-exists? next)
			   (make-directory next))
			 (loop next (cdr rel-path)))))))])
      (unless (file-exists? target)
	(printf "Copying ~a to ~a~n" path target)
	(copy-file path target)
	(let ([code (get-module-code path "no-such-dir")])
	  (let-values ([(a b c d) (module-compiled-imports code)])
	    (map (lambda (x)
		   (unless (symbol? x)
		     (go x path #f)))
		 (append a b c d))))))))

(unless (directory-exists? "xform-collects")
  (make-directory "xform-collects"))
(unless (directory-exists? "xform-collects/xform")
  (make-directory "xform-collects/xform"))

(go (build-path (current-load-relative-directory) "xform-mod.ss")
    #f
    "xform-collects/xform/xform-mod.ss")
;; Needed for cm:
(go '(lib "cm-ctime.ss" "mzlib" "private") #f #f)

(current-library-collection-paths 
 (list (build-path (current-directory) "xform-collects")))

(printf "Compiling xform support...~n")

(let ([mk-cm make-compilation-manager-load/use-compiled-handler])
  (current-namespace (make-namespace))
  (use-compiled-file-paths (list "compiled"))
  (current-load/use-compiled (mk-cm)))

(dynamic-require '(lib "xform-mod.ss" "xform") (void))

(with-output-to-file "xform-collects/version.ss"
  (lambda () (write (version))))

(printf "Done making xform-collects.\n")
