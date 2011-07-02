(module code "slideshow.rkt"
  (require texpict/code
           mzlib/unit)
  (require-for-syntax syntax/to-string
                      mzlib/list)

  (define current-code-line-sep (make-parameter line-sep))

  (define-values/invoke-unit/infer code@)

  (define-code code typeset-code)

  (provide code
           current-code-line-sep
           define-code)
  (provide-signature-elements code^)
    
  (provide define-exec-code/scale
	   define-exec-code)
  (define-syntax (define-exec-code/scale stx)
    (define (drop-to-run l)
      (map (lambda (x)
             (cond
	      [(and (pair? (syntax-e x))
		    (eq? 'local (syntax-e (car (syntax-e x)))))
	       (let ([l (syntax->list x)])
		 (list* 'local
			(drop-to-run (syntax->list (cadr l)))
			(cddr l)))]
	      [(and (pair? (syntax-e x))
		    (eq? 'define (syntax-e (car (syntax-e x)))))
	       (let ([l (syntax->list x)])
		 (list* 'define
			(cadr l)
			(drop-to-run (cddr l))))]
	      [else x]))
           (filter (lambda (x)
                     (cond
		      [(eq? '_ (syntax-e x))
		       #f]
		      [(eq? '... (syntax-e x))
		       #f]
		      [(eq? 'code:blank (syntax-e x))
		       #f]
		      [(and (pair? (syntax-e x))
			    (eq? 'code:comment (syntax-e (car (syntax-e x)))))
		       #f]
		      [(and (pair? (syntax-e x))
			    (eq? 'code:contract (syntax-e (car (syntax-e x)))))
		       #f]
		      [(and (pair? (syntax-e x))
			    (eq? 'unsyntax (syntax-e (car (syntax-e x)))))
		       #f]
		      [else #t]))
                   l)))
    (define (drop-to-show l)
      (foldr (lambda (x r)
               (cond
		[(and (identifier? x) (eq? '_ (syntax-e x)))
		 (cdr r)]
		[(and (pair? (syntax-e x))
		      (eq? 'local (syntax-e (car (syntax-e x)))))
		 (cons
		  (let ([l (syntax->list x)])
		    (datum->syntax-object 
		     x
		     (list* (car l)
			    (datum->syntax-object
			     (cadr l)
			     (drop-to-show (syntax->list (cadr l)))
			     (cadr l))
			    (cddr l))
		     x))
		  r)]
		[(and (pair? (syntax-e x))
		      (eq? 'cond (syntax-e (car (syntax-e x)))))
		 (cons
		  (let ([l (syntax->list x)])
		    (datum->syntax-object 
		     x
		     (list* (car l)
			    (drop-to-show (cdr l)))
		     x))
		  r)]
		[(and (pair? (syntax-e x))
		      (eq? 'define (syntax-e (car (syntax-e x)))))
		 (cons (let ([l (syntax->list x)])
			 (datum->syntax-object 
			  x
			  (list* (car l)
				 (cadr l)
				 (drop-to-show (cddr l)))
			  x))
		       r)]
		[else (cons x r)]))
             empty
             l))

    (syntax-case stx ()
      [(_ s (showable-name runnable-name string-name) . c)
       #`(begin
           (define runnable-name
             (quote-syntax
              (begin
                #,@(drop-to-run (syntax->list #'c)))))
           (define showable-name
	     (scale/improve-new-text
	      (code
	       #,@(drop-to-show (syntax->list #'c)))
	      s))
	   (define string-name
	     #,(syntax->string #'c)))]))

  (define-syntax define-exec-code
    (syntax-rules ()
      [(_ (a b c) . r)
       (define-exec-code/scale 1 (a b c) . r)])))




