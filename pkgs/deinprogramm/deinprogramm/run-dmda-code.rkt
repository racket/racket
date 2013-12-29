(module run-dmda-code mzscheme
  (require mred/mred mzlib/class mzlib/match syntax/modread)

  (define (run-dmda-file filename)
    (let ((p (open-input-graphical-file/fixed filename))
	  (expected-module-name
	   (let-values (((base name dir) (split-path filename)))
	     (string->symbol (path->string (path-replace-suffix name #""))))))
      (dynamic-wind
	  values
	  (lambda ()
	    (with-module-reading-parameterization
	     (lambda ()
	       (let* ((code (read-syntax filename p))
		      (pimped-code
		       (syntax-case code ()
			 ((?module ?name ?language ?body ...)
			  (syntax
			   (?module ?name ?language 
				    (require (lib "testing.rkt" "htdp"))
				    ?body ...)))))
		      (module-ized-code
		       (check-module-form pimped-code expected-module-name filename)))
		 (eval module-ized-code)
		 (dynamic-require expected-module-name #f)))))
	  (lambda ()
	    (close-input-port p)))))

;; The following definitions work around a bug in PLT 371.

;; build-input-port : string -> (values input any)
;; constructs an input port for the load handler. Also
;; returns a value representing the source of code read from the file.
  (define (build-input-port filename)
    (let ([p (open-input-file filename)])
      (port-count-lines! p)
      (let ([p (cond
		[(regexp-match-peek #rx#"^(?:#reader[(]lib\"read[.]ss\"\"wxme\"[)])?WXME01[0-9][0-9] ##[ \r\n]" p)
		 (let ([t (make-object text%)])
		   (send t insert-port p 'standard)
		   (close-input-port p)
		   (open-input-text-editor t 0 'end values filename))]
		[else p])])
	(port-count-lines! p)           ; in case it's new
	(values p filename))))

  (define (open-input-graphical-file/fixed filename)
    (let-values ([(p name) (build-input-port filename)])
      p))

  (run-dmda-file
   (vector-ref (current-command-line-arguments) 0))

  (module test racket/base))
