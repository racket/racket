(module file racket/base
  (require racket/file
           racket/path
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

  (define (build-relative-path p . args)
    (if (relative-path? p)
      (apply build-path p args)
      (error 'build-relative-path "base path ~s is absolute" p)))

  (define (build-absolute-path p . args)
    (if (relative-path? p)
      (error 'build-absolute-path "base path ~s is relative" p)
      (apply build-path p args)))

  (define (find-library name . cp)
    (apply collection-file-path name cp))

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
