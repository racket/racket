(module file scheme/base
  (require scheme/file
           (prefix-in mz: (only-in mzscheme 
                                   open-input-file
                                   open-output-file)))
  (provide find-relative-path
	   explode-path
	   normalize-path
	   build-absolute-path
	   build-relative-path
	   filename-extension
	   file-name-from-path
	   path-only
	   delete-directory/files
	   copy-directory/files
	   make-directory*
	   make-temporary-file
	   find-library

	   get-preference
	   put-preferences

	   (rename-out [-call-with-input-file* call-with-input-file*]
                       [-call-with-output-file* call-with-output-file*])

	   fold-files
	   find-files
	   pathlist-closure)


  (define (find-library name . cp)
    (let ([dir (with-handlers ([exn:fail:filesystem? (lambda (exn) #f)])
                 (if (null? cp)
                     (collection-path "mzlib")
                     (apply collection-path cp)))])
      (and dir
           (let ([file (build-path dir name)])
             (and (file-exists? file) file)))))

  (define (-call-with-input-file* file thunk . flags)
    (let ([p (apply mz:open-input-file file flags)])
      (dynamic-wind
        void
        (lambda () (thunk p))
        (lambda () (close-input-port p)))))

  (define (-call-with-output-file* file thunk . flags)
    (let ([p (apply mz:open-output-file file flags)])
      (dynamic-wind
        void
        (lambda () (thunk p))
        (lambda () (close-output-port p))))))
