
(module collects-path mzscheme
  (provide set-collects-path)

  (define label "coLLECTs dIRECTORy:")

  (define set-collects-path
    (case-lambda
     [()
      (let ([v (current-command-line-arguments)])
	(set-collects-path (vector-ref v 0) (vector-ref v 1)))]
     [(dest path)
      (let-values ([(i o) (open-input-output-file dest 'update)])
	(let ([m (regexp-match-positions label i)]
	      [path (if (string? path)
			(string->path path)
			path)])
	  (unless m
	    (error 'set-collects-path
		   "cannot find collection-path label in executable file"))
	  (file-position o (cdar m))
	  (write-bytes (path->bytes path) o)
	  (write-byte 0 o)
	  (close-input-port i)
	  (close-output-port o)))])))
    