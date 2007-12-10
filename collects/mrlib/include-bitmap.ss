(module include-bitmap mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
	   (lib "file.ss")
           (lib "main-collects.ss" "setup"))
  (require-for-syntax (lib "path-spec.ss" "syntax")
		      (lib "cm-accomplice.ss")
                      (lib "main-collects.ss" "setup"))

  (provide include-bitmap
           include-bitmap/relative-to)
  
  (define-syntax (-include-bitmap stx)
    (syntax-case stx ()
      [(_ orig-stx source path-spec type)
       (let* ([c-file (resolve-path-spec #'path-spec #'source #'orig-stx)]
	      [content
	       (with-handlers ([exn:fail?
				(lambda (exn)
				  (error 'include-bitmap
					 "could not load ~e: ~a"
					 c-file
					 (if (exn? exn)
					     (exn-message exn)
					     (format "~e" exn))))])
		 (with-input-from-file c-file
		   (lambda ()
		     (read-bytes (file-size c-file)))))])
	 (register-external-file c-file)
	 (with-syntax ([content content]
		       [c-file (path->main-collects-relative c-file)])
	   (syntax/loc stx
	     (get-or-load-bitmap content 'c-file type))))]))

  (define-syntax (include-bitmap/relative-to stx)
    (syntax-case stx ()
      [(_ source path-spec) #`(-include-bitmap #,stx source path-spec 'unknown/mask)]
      [(_ source path-spec type) #`(-include-bitmap #,stx source path-spec type)]))

  (define-syntax (include-bitmap stx)
    (syntax-case stx ()
      [(_ path-spec) #`(-include-bitmap #,stx #,stx path-spec 'unknown/mask)]
      [(_ path-spec type) #`(-include-bitmap #,stx #,stx path-spec type)]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Run-time support

  (define cached (make-hash-table 'equal))

  (define (get-or-load-bitmap content orig type)
    (hash-table-get cached (cons content type)
		    (lambda ()
		      (let ([bm (let ([fn (make-temporary-file)])
				  (dynamic-wind
				      void
				      (lambda ()
					(with-output-to-file fn
					  (lambda () (display content))
					  'truncate)
					(make-object bitmap% fn type))
				      (lambda ()
					(delete-file fn))))])
			(unless (send bm ok?)
			  (error 'include-bitmap
				 "unable to parse image, originated from: ~a"
				 (path->string (main-collects-relative->path orig))))
			(hash-table-put! cached (cons content type) bm)
			bm)))))
