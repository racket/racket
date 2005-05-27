
(module trace mzscheme
  (require "pretty.ss")

  (provide trace untrace)

  (define max-dash-space-depth 10)
  (define number-nesting-depth 6)

  (define as-spaces
    (lambda (s)
      (let ((n (string-length s)))
	(apply string-append
	       (let loop ((k n))
		 (if (zero? k) '("")
		     (cons " " (loop (sub1 k)))))))))

  (define-struct prefix-entry (for-first for-rest))

  (define prefixes (make-vector 20 #f))
  (define prefix-vector-length 20)

  (define lookup-prefix
    (lambda (n)
      (and (< n prefix-vector-length)
	   (vector-ref prefixes n))))

  (define insert-prefix
    (lambda (n first rest)
      (if (>= n prefix-vector-length)
	  (let ((v (make-vector (* 2 prefix-vector-length) #f)))
	    (let loop ((k 0))
	      (when (< k prefix-vector-length)
		(vector-set! v k (vector-ref prefixes k))
		(loop (add1 k))))
	    (set! prefixes v)
	    (set! prefix-vector-length (* 2 prefix-vector-length))
	    (insert-prefix n first rest))
	  (vector-set! prefixes n (make-prefix-entry first rest)))))

  (define construct-prefixes
    (lambda (level)
      (let loop ((n level)
		 (first '("|"))
		 (rest '(" ")))
	(if (>= n max-dash-space-depth)
	    (let-values (((pre-first pre-rest)
			  (build-prefixes number-nesting-depth)))
	      (let ((s (number->string level)))
		(values
		 (apply string-append
			(cons pre-first (cons "[" (cons s (cons "]" '())))))
		 (apply string-append
			(cons pre-rest (cons " " (cons (as-spaces s)
						       (cons " " '()))))))))
	    (cond
	     ((= n 0) (values (apply string-append (reverse first))
			      (apply string-append (reverse rest))))
	     ((= n 1) (loop (- n 1)
			    (cons '" " first)
			    (cons '" " rest)))
	     (else (loop (- n 2)
			 (cons " |" first)
			 (cons "  " rest))))))))

  (define build-prefixes
    (lambda (level)
      (let ((p (lookup-prefix level)))
	(if p
	    (values (prefix-entry-for-first p)
		    (prefix-entry-for-rest p))
	    (let-values (((first rest)
			  (construct-prefixes level)))
	      (insert-prefix level first rest)
	      (values first rest))))))

  (define -:trace-print-args
    (lambda (name args level)
      (let-values (((first rest)
		    (build-prefixes level)))
	(parameterize ((pretty-print-print-line
			(lambda (n port offset width)
			  (display
			   (if n
			       (if (zero? n) first
				   (format "~n~a" rest))
			       (format "~n"))
			   port)
			  (if n
			      (if (zero? n)
				  (string-length first)
				  (string-length rest))
			      0))))
	  (pretty-print (cons name args))))))

  (define -:trace-print-results
    (lambda (name results level)
      (let-values (((first rest)
		    (build-prefixes level)))
	(parameterize ((pretty-print-print-line
			(lambda (n port offset width)
			  (display
			   (if n
			       (if (zero? n) first
				   (format "~n~a" rest))
			       (format "~n"))
			   port)
			  (if n
			      (if (zero? n)
				  (string-length first)
				  (string-length rest))
			      0))))
	  (cond
	   ((null? results)
	    (pretty-display "*** no values ***"))
	   ((null? (cdr results))
	    (pretty-print (car results)))
	   (else
	    (pretty-print (car results))
	    (parameterize ((pretty-print-print-line
			    (lambda (n port offset width)
			      (display
			       (if n
				   (if (zero? n) rest
				       (format "~n~a" rest))
				   (format "~n"))
			       port)
			      (if n
				  (string-length rest)
				  0))))
	      (for-each pretty-print (cdr results)))))))))

  ;; A traced-proc struct instance acts like a procedure,
  ;;  but preserves the original, too.
  (define-values (struct:traced-proc make-traced-proc traced-proc? traced-proc-ref traced-proc-set!)
    (make-struct-type 'traced-proc #f 2 0 #f null (current-inspector) 0))

  ;; Install traced versions of a given set of procedures.  The traced
  ;;  versions are also given, so that they can be constructed to have
  ;;  a nice name.
  (define (do-trace ids procs setters traced-procs)
    (for-each (lambda (id proc)
		(unless (procedure? proc)
		  (error 'trace
			 "the value of ~s is not a procedure: ~e" id proc)))
	      ids procs)
    (for-each (lambda (proc setter traced-proc)
		(unless (traced-proc? proc)
		  (setter (make-traced-proc traced-proc proc))))
	      procs setters traced-procs)
    ids)

  ;; Key used for a continuation mark to indicate
  ;;  the nesting depth:
  (define -:trace-level-key (gensym))

  ;; Apply a traced procedure to arguments, printing arguments
  ;; and results. We set and inspect the -:trace-level-key continuation
  ;; mark a few times to detect tail calls.
  (define (do-traced id args real-value)
    (let* ([levels (continuation-mark-set->list
		    (current-continuation-marks)
		    -:trace-level-key)]
	   [level (if (null? levels) 0 (car levels))])
      ;; Tentatively push the new depth level:
      (with-continuation-mark
	  -:trace-level-key
	  (add1 level)
	;; Check for tail-call => car of levels replaced,
	;;  which means that the first two new marks are
	;;  not consecutive:
	(let ([new-levels (continuation-mark-set->list
			   (current-continuation-marks)
			   -:trace-level-key)])
	  (if (and (pair? (cdr new-levels))
		   (> (car new-levels) (add1 (cadr new-levels))))
	      ;; Tail call: reset level and just call real-value.
	      ;;  (This is in tail position to the call to `do-traced'.)
	      ;;  We don't print the results, because the original
	      ;;  call will.
	      (begin
		(-:trace-print-args id args (sub1 level))
		(with-continuation-mark
		    -:trace-level-key
		    (car levels)
		  (apply real-value args)))
	      ;; Not a tail call; push the old level, again, to ensure
	      ;;  that when we push the new level, we have consecutive
	      ;;  levels associated with the mark (i.e., set up for 
	      ;;  tail-call detection the next time around):
	      (begin
		(-:trace-print-args id args level)
		(with-continuation-mark
		    -:trace-level-key
		    level
		  (call-with-values (lambda () 
				      (with-continuation-mark
					  -:trace-level-key
					  (add1 level)
					(apply real-value args)))
		    (lambda results
		      (flush-output)
		      ;; Print the results:
		      (-:trace-print-results id results level)
		      ;; Return the results:
		      (apply values results))))))))))

  (define-syntax trace
    (lambda (stx)
      (syntax-case stx ()
	[(_ id ...)
	 (let ([ids (syntax->list (syntax (id ...)))])
	   (for-each (lambda (id)
		       (unless (identifier? id)
			 (raise-syntax-error
			  #f
			  "not an identifier"
			  stx
			  id)))
		     ids)
	   (with-syntax ([(traced-name ...)
			  (map (lambda (id)
				 (datum->syntax-object
				  id
				  (string->symbol
				   (string-append "traced-"
						  (symbol->string (syntax-e id))))
				  #f))
			       ids)])
	     #'(do-trace
		'(id ...)
		(list id ...)
		(list (lambda (v) (set! id v)) ...)
		(list
		 (let ([real-value id])
		   (let ([traced-name
			  (lambda args
			    (do-traced 'id args real-value))])
		     traced-name))
		 ...))))])))

  (define-syntax untrace
    (lambda (stx)
      (syntax-case stx ()
	[(_ id ...)
	 (let ([ids (syntax->list (syntax (id ...)))])
	   (for-each (lambda (id)
		       (unless (identifier? id)
			 (raise-syntax-error
			  #f
			  "not an identifier"
			  stx
			  id)))
		     ids)
	   (syntax
	    (append
	     (if (traced-proc? id)
		 (begin
		   (set! id (traced-proc-ref id 1))
		   '(id))
		 null)
	     ...)))])))
  )

