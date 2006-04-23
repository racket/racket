
(module macfw mzscheme
  (require (lib "process.ss"))

  (provide update-framework-path
	   get-current-framework-path
	   update-framework-path/cmdline)

  (define (update-framework-path/cmdline)
    (let ([v (current-command-line-arguments)])
      (update-framework-path (vector-ref v 0)
			     (vector-ref v 1)
			     (equal? (vector-ref v 2) "mred"))))

  (define (update-framework-path fw-path dest mred?)
    (let ([dest (if (path? dest)
		    (path->string dest)
		    dest)])
      (for-each (lambda (p)
		  (let* ([orig (get-current-framework-path dest p)]
			 [3m (if (and orig (regexp-match #rx"_3m" orig))
				 "_3m"
				 "")])
		    (system* "/usr/bin/install_name_tool"
			     "-change"
			     (or orig
				 (format "~a.framework/Versions/~a~a/~a" p (version) 3m p))
			     (format "~a~a.framework/Versions/~a~a/~a" 
				     fw-path
				     p (version) 3m p)
			     dest)))
		(if mred?
		    '("PLT_MrEd")
		    '("PLT_MzScheme")))))

  (define (get-current-framework-path dest p)
    (let-values ([(r w) (make-pipe)])
      (parameterize ([current-output-port w])
	(system* "/usr/bin/otool"
		 "-L"
		 (if (path? dest)
		     (path->string dest)
		     dest)))
      (close-output-port w)
      (let ([m (regexp-match (bytes-append #"[\r\n][ \t]*([^ \t][^\r\n]*" 
					   (string->bytes/utf-8 p)
					   #"[^\r\n]*)"
					   #" [(]compatibility version [0-9.]*,"
					   #" current version [0-9.]*[)][\r\n]")
			     r)])
	(if m
	    (bytes->string/utf-8 (cadr m))
	    (begin
	      (fprintf (current-error-port)
		       "warning: cannot find existing link for ~a in ~a\n"
		       p dest)
	      #f))))))

	       


  