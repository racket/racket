
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
	  (collection-path "racket"))
  (let ([p (build-path (current-load-relative-directory)
		       'up
		       'up
		       'up
                       "lib"
		       "collects")])
    (printf "Setting collection path: ~s\n" p)
    (current-library-collection-paths 
     (list p))))

(require syntax/moddep
	 compiler/cm)

(define (go mod-path rel-to target)
  (let* ([path (if target
                   mod-path
                   (if (module-path-index? mod-path)
                       (resolve-module-path-index mod-path rel-to)
                       (resolve-module-path mod-path rel-to)))])
    (unless (symbol? path)
      ;; Copy file to here. The filename is from the resolved module
      ;; path, so it is ".rkt" even if the source is ".ss".
      (let* ([path (if (pair? path)
                       (cadr path) ; extra from submodule
                       path)]
             [path (if (file-exists? path)
                       path
                       (if (regexp-match? #rx#"[.]rkt$" (if (path? path)
                                                            (path->bytes path)
                                                            path))
                           (let ([p2 (path-replace-suffix path #".ss")])
                             (if (file-exists? p2)
                                 p2
                                 path))
                           path))]
             [target 
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
          (printf "Copying ~a to ~a\n" path target)
          (copy-file path target)
          (let ([code (get-module-code path "no-such-dir")])
            (map (lambda (x)
                   (go x path #f))
                 (apply append (map cdr (module-compiled-imports code))))))))))

(unless (directory-exists? "xform-collects")
  (make-directory "xform-collects"))
(unless (directory-exists? "xform-collects/xform")
  (make-directory "xform-collects/xform"))

(go (build-path (current-load-relative-directory) "xform-mod.rkt")
    #f
    "xform-collects/xform/xform-mod.rkt")
;; Readers:
(map (lambda (r) (go r #f #f))
     '(s-exp/lang/reader
       racket/base/lang/reader
       racket/runtime-config))

(current-library-collection-paths 
 (list (build-path (current-directory) "xform-collects")))

(printf "Compiling xform support...\n")

(let ([mk-cm make-compilation-manager-load/use-compiled-handler]
      [old-namespace (current-namespace)])
  (parameterize ([current-namespace (make-empty-namespace)])
    (namespace-attach-module old-namespace ''#%builtin)
    (parameterize ([use-compiled-file-paths (list "compiled")])
      (parameterize ([current-load/use-compiled (mk-cm)])
        (namespace-require 'racket/base)
        
        (dynamic-require 'xform/xform-mod (void))))))

(with-output-to-file "xform-collects/version.rkt"
  (lambda () (write (version))))

(printf "Done making xform-collects.\n")
