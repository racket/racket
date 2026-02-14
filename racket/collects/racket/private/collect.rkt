(module collect '#%kernel
  (#%declare #:require=define)

  (#%require "core-macros.rkt"
             "path.rkt"
             "kw.rkt"
	     (prefix k: '#%kernel))
  
  (#%provide (rename collection-path new:collection-path)
	     [rename collection-file-path new:collection-file-path])
  
  (new-define collection-path
	      (new-lambda (collection 
			   #:fail [fail (lambda (s)
					  (raise
					   (exn:fail:filesystem
					    (string-append "collection-path: " s)
					    (current-continuation-marks))))]
			   . collections)
			  (k:collection-path fail collection collections)))

  (new-define collection-file-path
	      (new-lambda (file-name 
			   collection
			   #:check-compiled? [check-compiled?
					      (and (path-string? file-name)
						   (regexp-match? #rx".[.]rkt$" file-name))]
			   #:fail [fail (lambda (s)
					  (raise
					   (exn:fail:filesystem
					    (string-append "collection-file-path: " s)
					    (current-continuation-marks))))]
			   . collections)
			  (k:collection-file-path fail check-compiled? file-name collection collections))))
