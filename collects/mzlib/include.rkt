
(module include mzscheme
  (require-for-syntax syntax/stx
		      "private/increader.rkt"
		      "cm-accomplice.rkt")
  (require mzlib/etc)

  (define-for-syntax (resolve-path-spec fn loc stx build-path-stx)
    (let ([file
	   (syntax-case* fn (lib) module-or-top-identifier=?
	     [_
	      (string? (syntax-e fn))
	      (let ([s (syntax-e fn)])
		(unless (or (relative-path? s)
			    (absolute-path? s))
		  (raise-syntax-error
		   #f
		   "bad pathname string"
		   stx
		   fn))
		(string->path s))]
	     [(-build-path elem ...)
              (begin
                (collect-garbage)
                (module-identifier=? #'-build-path build-path-stx)
                (module-or-top-identifier=? #'-build-path build-path-stx))
	      (let ([l (syntax-object->datum (syntax (elem ...)))])
		(when (null? l)
		  (raise-syntax-error
		   #f
		   "`build-path' keyword is not followed by at least one string"
		   stx
		   fn))
		(apply build-path l))]
	     [(lib filename ...)
	      (let ([l (syntax-object->datum (syntax (filename ...)))])
		(unless (or (andmap string? l)
			    (pair? l))
		  (raise-syntax-error
		   #f
		   "`lib' keyword is not followed by a sequence of string datums"
		   stx
		   fn))
                (apply collection-file-path
                       (car l)
                       (if (null? (cdr l))
                           (list "mzlib")
                           (cdr l))))]
	     [else
	      (raise-syntax-error
	       #f
	       "not a pathname string, `build-path' form, or `lib' form for file"
	       stx
	       fn)])])
      (if (complete-path? file)
	  file
	  (path->complete-path
	   file
	   (cond
	    ;; Src of include expression is a path?
	    [(and (path? (syntax-source loc))
		  (complete-path? (syntax-source loc)))
	     (let-values ([(base name dir?) 
			   (split-path (syntax-source loc))])
	       (if dir?
		   (syntax-source loc)
		   base))]
	    ;; Load relative?
	    [(current-load-relative-directory)]
	    ;; Current directory
	    [(current-directory)])))))

  (define-syntax-set (do-include ; private
		      include-at/relative-to
		      include
		      include-at/relative-to/reader
		      include/reader)

    (define (do-include/proc stx)
      (syntax-case stx ()
	[(_ orig-stx ctx loc fn reader)
	 ;; Parse the file name
	 (let ([orig-c-file (resolve-path-spec (syntax fn) (syntax loc) (syntax orig-stx) #'build-path)]
	       [ctx (syntax ctx)]
	       [loc (syntax loc)]
	       [reader (syntax reader)]
	       [orig-stx (syntax orig-stx)]
               [rkt->ss (lambda (p)
                          (let ([b (path->bytes p)])
                            (if (regexp-match? #rx#"[.]rkt$" b)
                                (path-replace-suffix p #".ss")
                                p)))])
	   
           (let ([c-file (if (file-exists? orig-c-file)
                         orig-c-file
                         (let ([p2 (rkt->ss orig-c-file)])
                           (if (file-exists? p2)
                               p2
                               orig-c-file)))])
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
                      orig-stx)))))))]))
    
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
