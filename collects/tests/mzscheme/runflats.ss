
(for-each (lambda (f)
	    (when (regexp-match "^flat-[0-9]+[.]ss$" (path->string f))
	      (parameterize ([current-namespace (make-namespace)]
                             [exit-handler void])
		(eval
		 `(begin
		    (require-for-syntax mzscheme)
		    (define quiet-load ,(path->string f))
		    (load-relative "quiet.ss"))))))
	  (directory-list))

