
(module code "slideshow.ss"
  (require (lib "code.ss" "texpict")
	   (lib "unitsig.ss"))
  (require-for-syntax (lib "list.ss"))

  (define-values/invoke-unit/sig code^
    code@
    #f
    code-params^)

  (define-code code typeset-code)

  (provide code)
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

    (define (to-string c)
      (let* ([s (open-output-string)]
	     [l (syntax->list c)]
	     [init-col (or (syntax-column (first l)) 0)]
	     [col init-col]
	     [line (or (syntax-line (first l)) 0)])
	(define (advance c init-line!)
	  (let ([c (syntax-column c)]
		[l (syntax-line c)])
	    (when (and l (l . > . line))
	      (newline)
	      (set! line l)
	      (init-line!))
	    (when c
	      (display (make-string (max 0 (- c col)) #\space))
	      (set! col c))))
	(parameterize ([current-output-port s]
		       [read-case-sensitive #t])
	  (define (loop init-line!)
	    (lambda (c)
	      (cond
	       [(eq? 'code:blank (syntax-e c))
		(advance c init-line!)]
	       [(eq? '_ (syntax-e c)) (void)]
	       [(eq? '... (syntax-e c))
		(void)]
	       [(and (pair? (syntax-e c))
		     (eq? (syntax-e (car (syntax-e c))) 'code:comment))
		(advance c init-line!)
		(printf "; ")
		(display (syntax-e (cadr (syntax->list c))))]
	       [(and (pair? (syntax-e c))
		     (eq? (syntax-e (car (syntax-e c))) 'code:contract))
		(advance c init-line!)
		(printf "; ")
		(let* ([l (cdr (syntax->list c))]
		       [s-col (or (syntax-column (first l)) col)])
		  (set! col s-col)
		  (for-each (loop (lambda ()
				    (set! col s-col)
				    (printf "; ")))
			    l))]
	       [(and (pair? (syntax-e c))
		     (eq? (syntax-e (car (syntax-e c))) 'quote))
		(advance c init-line!)
		(printf "'")
		(let ([i (cadr (syntax->list c))])
		  (set! col (or (syntax-column i) col))
		  ((loop init-line!) i))]
	       [(pair? (syntax-e c))
		(advance c init-line!)
		(printf "(")
		(set! col (+ col 1))
		(map (loop init-line!) (syntax->list c))
		(printf ")")
		(set! col (+ col 1))]
	       [else
		(advance c init-line!)
		(let ([s (format "~s" (syntax-e c))])
		  (set! col (+ col (string-length s)))
		  (display s))])))
	  (for-each (loop (lambda () (set! col init-col))) l))
	(get-output-string s)))
    
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
	     #,(to-string #'c)))]))

  (define-syntax define-exec-code
    (syntax-rules ()
      [(_ (a b c) . r)
       (define-exec-code/scale 1 (a b c) . r)])))




