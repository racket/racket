
(module sigmatch mzscheme

  (require "../unit.ss")

  (define (hash-sig src-sig table)
    (and (pair? src-sig)
	 (vector? (car src-sig))
	 (andmap
	  (lambda (s)
	    (cond
	     [(symbol? s)
	      (if (hash-table-get table s (lambda () #f))
		  #f
		  (begin
		    (hash-table-put! table s s)
		    #t))]
	     [(and (pair? s) (symbol? (car s)))
	      (let ([name (car s)])
		(if (hash-table-get table name (lambda () #f))
		  #f
		  (let ([t (make-hash-table)])
		    (hash-table-put! table name t)
		    (hash-sig (cdr s) t))))]
	     [else #f]))
	  (vector->list (car src-sig)))))

  (define (sig-path-name name path)
    (let loop ([s (symbol->string name)]
	       [path path])
      (if (null? path)
	  s
	  (loop (format "~a:~a" s (car path))
		(cdr path)))))

  (define (check-sig-match table sig path exact? who src-context dest-context)
    (and (vector? (car sig))
	 (andmap
	  (lambda (s)
	    (cond
	     [(symbol? s)
	      (let ([v (hash-table-get table s
				       (lambda ()
					 (raise
					  (make-exn:fail:unit
					   (string->immutable-string
					    (format
					     "~a: ~a is missing a value name `~a', required by ~a"
					     who
					     src-context
					     (sig-path-name s path)
					     dest-context))
					   (current-continuation-marks)))))])
		(and v
		     (begin
		       (unless (symbol? v)
			 (let ([p (sig-path-name s path)])
			   (raise
			    (make-exn:fail:unit
			     (string->immutable-string
			      (format
			       "~a: ~a contains `~a' as a sub-unit name, but ~a contains `~a' as a value name"
			       who
			       src-context
			       p
			       dest-context
			       p))
			     (current-continuation-marks)))))
		       (hash-table-put! table s #f)
		       #t)))]
	     [(and (pair? s) (symbol? (car s)))
	      (let ([v (hash-table-get table (car s)
				       (lambda ()
					 (raise
					  (make-exn:fail:unit
					   (string->immutable-string
					    (format
					     "~a: ~a is missing a sub-unit name `~a', required by ~a"
					     who
					     src-context
					     (sig-path-name (car s) path)
					     dest-context))
					   (current-continuation-marks)))))])
		(and v
		     (begin
		       (unless (hash-table? v)
			 (let ([p (sig-path-name (car s) path)])
			   (raise
			    (make-exn:fail:unit
			     (string->immutable-string
			      (format
			       "~a: ~a contains `~a' as a value name, but ~a contains `~a' as a sub-unit name"
			       who
			       src-context
			       p
			       dest-context
			       p))
			     (current-continuation-marks)))))
		       (hash-table-put! table (car s) #f)
		       (check-sig-match v (cdr s) (cons (car s) path)
					exact? who src-context dest-context))))]
	     [else #f]))
	  (vector->list (car sig)))
	 (or (not exact?)
	     (hash-table-for-each
	      table
	      (lambda (k v)
		(when v
		  (let ([p (sig-path-name k path)])
		    (raise
		     (make-exn:fail:unit
		      (string->immutable-string
		       (format
			"~a: ~a contains an extra ~a name `~a' that is not required by ~a"
			who
			src-context
			(if (symbol? v) 'value 'sub-unit)
			p
			dest-context))
		      (current-continuation-marks)))))))
	     #t)))

  (define (verify-signature-match who exact? dest-context dest-sig src-context src-sig)
    (unless (symbol? who)
      (raise-type-error 'verify-signature-match "symbol" who))
    (unless (string? dest-context)
      (raise-type-error 'verify-signature-match "string" dest-context))
    (unless (string? src-context)
      (raise-type-error 'verify-signature-match "string" src-context))

    (let ([src-table (make-hash-table)])
      (unless (hash-sig src-sig src-table)
	(raise-type-error 'verify-signature-match "signature" src-sig))

      (unless (check-sig-match src-table dest-sig null
			       exact? who src-context dest-context)
	(raise-type-error 'verify-signature-match "signature" dest-sig))))

  (provide verify-signature-match))
