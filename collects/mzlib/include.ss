
(module include mzscheme
  (require-for-syntax (lib "stx.ss" "syntax")
		      (lib "path-spec.ss" "syntax")
		      "private/increader.ss"
		      "cm-accomplice.ss")
  (require (lib "etc.ss"))

  (define-syntax-set (do-include ; private
		      include-at/relative-to
		      include
		      include-at/relative-to/reader
		      include/reader)

    (define (do-include/proc stx)
      (syntax-case stx ()
	[(_ orig-stx ctx loc fn reader)
	 ;; Parse the file name
	 (let ([c-file (resolve-path-spec (syntax fn) (syntax loc) (syntax orig-stx) #'build-path)]
	       [ctx (syntax ctx)]
	       [loc (syntax loc)]
	       [reader (syntax reader)]
	       [orig-stx (syntax orig-stx)])
	   
	   (register-external-file c-file)

	   (let ([read-syntax (if (syntax-e reader)
                                  (reader-val
                                   (let loop ([e (syntax-object->datum
                                                  (local-expand reader 'expression null))])
                                     (cond
                                       [(reader? e) e]
                                       [(pair? e) (or (loop (car e))
                                                      (loop (cdr e)))]
                                       [else #f])))
				  read-syntax)])
	     (unless (and (procedure? read-syntax)
			  (procedure-arity-includes? read-syntax 2))
	       (raise-syntax-error
		#f
		"reader is not a procedure of two arguments"
		orig-stx))

	     ;; Open the included file
	     (let ([p (with-handlers ([exn:fail?
				       (lambda (exn)
					 (raise-syntax-error
					  #f
					  (format
					   "can't open include file (~a)"
					   (if (exn? exn)
					       (exn-message exn)
					       exn))
					  orig-stx
					  c-file))])
			(open-input-file c-file))])
	       (port-count-lines! p)
	       ;; Read expressions from file
	       (let ([content
		      (let loop ()
			(let ([r (with-handlers ([exn:fail?
						  (lambda (exn)
						    (close-input-port p)
						    (raise-syntax-error
						     #f
						     (format
						      "read error (~a)"
						      (if (exn? exn)
							  (exn-message exn)
							  exn))
						     orig-stx))])
				   (read-syntax c-file p))])
			  (if (eof-object? r)
			      null
			      (cons r (loop)))))])
		 (close-input-port p)
		 ;; Preserve src info for content, but set its
		 ;; lexical context to be that of the include expression
		 (let ([lexed-content
			(let loop ([content content])
			  (cond
			   [(pair? content)
			    (cons (loop (car content))
				  (loop (cdr content)))]
			   [(null? content) null]
			   [else
			    (let ([v (syntax-e content)])
			      (datum->syntax-object
			       ctx
			       (cond
				[(pair? v) 
				 (loop v)]
				[(vector? v)
				 (list->vector (loop (vector->list v)))]
				[(box? v)
				 (box (loop (unbox v)))]
				[else
				 v])
			       content))]))])
		   (datum->syntax-object
		    (quote-syntax here)
		    `(begin ,@lexed-content)
		    orig-stx))))))]))
    
    (define (include/proc stx)
      (syntax-case stx ()
	[(_ fn)
	 (with-syntax ([_stx stx])
	   (syntax/loc stx (do-include _stx _stx _stx fn #f)))]))

    (define (include-at/relative-to/proc stx)
      (syntax-case stx ()
	[(_ ctx loc fn)
	 (with-syntax ([_stx stx])
	   (syntax/loc stx (do-include _stx ctx loc fn #f)))]))
      
    (define (include/reader/proc stx)
      (syntax-case stx ()
	[(_ fn reader)
	 ;; Expand to do-include:
	 (with-syntax ([_stx stx])
	   (syntax/loc stx 
	     (do-include _stx _stx _stx fn 
			 (letrec-syntax ([the-reader (lambda (stx)
						       (datum->syntax-object
							#'here
							(make-reader reader)))])
			   the-reader))))]))
    
    (define (include-at/relative-to/reader/proc stx)
      (syntax-case stx ()
	[(_ ctx loc fn reader)
	 (with-syntax ([_stx stx])
	   (syntax/loc stx 
	     (do-include _stx ctx loc fn 
			 (letrec-syntax ([the-reader (lambda (stx)
						       (datum->syntax-object
							#'here
							(make-reader reader)))])
			   the-reader))))])))
  
  (provide include
	   include-at/relative-to
	   include/reader
	   include-at/relative-to/reader))

		 
			   
	      
		     
