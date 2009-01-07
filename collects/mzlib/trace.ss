
(module trace scheme/base
  (require scheme/pretty
           (for-syntax scheme/base))

  (provide trace untrace
           current-trace-print-args trace-apply
           current-trace-notify)

  (define max-dash-space-depth 10)
  (define number-nesting-depth 6)

  (define (as-spaces s)
    (build-string (string-length s)
                  (lambda (i) #\space)))
  
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

  (define current-trace-notify
    (make-parameter (lambda (s)
                      (display s)
                      (newline))
                    (lambda (p)
                      (unless (and (procedure? p)
                                   (procedure-arity-includes? p 1))
                        (raise-type-error 'current-trace-notify
                                          "procedure (arity 1)"
                                          p))
                      p)))

  (define (as-trace-notify thunk)
    (let ([p (open-output-bytes)])
      (parameterize ([current-output-port p])
        (thunk))
      (let ([b (get-output-bytes p #t 0 
                                 ;; drop newline:
                                 (sub1 (file-position p)))])
        ((current-trace-notify) (bytes->string/utf-8 b)))))

  (define -:trace-print-args
    (lambda (name args kws kw-vals level)
      (as-trace-notify
       (lambda ()
         ((current-trace-print-args) name args kws kw-vals level)))))

  (define current-trace-print-args
    (make-parameter
     (lambda (name args kws kw-vals level)
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
           (pretty-print (append (cons name args)
                                 (apply append (map list kws kw-vals)))))))))
  
  (define -:trace-print-results
    (lambda (name results level)
      (as-trace-notify
       (lambda ()
         (trace-print-results name results level)))))

  (define trace-print-results
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
		  (setter (make-traced-proc 
                           (let-values ([(a) (procedure-arity proc)]
                                        [(req allowed) (procedure-keywords proc)])
                             (procedure-reduce-keyword-arity traced-proc
                                                             a
                                                             req
                                                             allowed))
                           proc))))
	      procs setters traced-procs))

  ;; Key used for a continuation mark to indicate
  ;;  the nesting depth:
  (define -:trace-level-key (gensym))

  (define (trace-apply id f kws kw-vals . args) (do-traced id args kws kw-vals f))
  
  ;; Apply a traced procedure to arguments, printing arguments
  ;; and results. We set and inspect the -:trace-level-key continuation
  ;; mark a few times to detect tail calls.
  (define (do-traced id args kws kw-vals real-value)
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
		(-:trace-print-args id args kws kw-vals (sub1 level))
		(with-continuation-mark
		    -:trace-level-key
		    (car levels)
                  (if (null? kws)
                      (apply real-value args)
                      (keyword-apply real-value kws kw-vals args))))
	      ;; Not a tail call; push the old level, again, to ensure
	      ;;  that when we push the new level, we have consecutive
	      ;;  levels associated with the mark (i.e., set up for 
	      ;;  tail-call detection the next time around):
	      (begin
		(-:trace-print-args id args kws kw-vals level)
		(with-continuation-mark
		    -:trace-level-key
		    level
		  (call-with-values (lambda () 
				      (with-continuation-mark
					  -:trace-level-key
					  (add1 level)
                                        (if (null? kws)
                                            (apply real-value args)
                                            (keyword-apply real-value kws kw-vals args))))
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
				 (datum->syntax
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
                          (make-keyword-procedure
                           (lambda (kws vals . args)
                             (do-traced 'id args kws vals real-value))
                           (lambda args
                             (do-traced 'id args null null real-value)))])
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
	    (begin
	     (when (traced-proc? id)
               (set! id (traced-proc-ref id 1)))
	     ...)))])))
  )

