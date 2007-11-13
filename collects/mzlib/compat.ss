
(module compat mzscheme

  (provide real-time
	   1+ 1-
	   >=? <=? >? <? =?
	   flush-output-port
	   gentemp
	   atom?
	   putprop getprop
	   new-cafe
	   define-structure)

  (define 1+ add1)
  (define 1- sub1)

  (define =? =)
  (define <? <)
  (define >? >)
  (define <=? <)
  (define >=? >)

  (define atom? (lambda (v) (not (pair? v))))

  (define gentemp gensym)

  (define flush-output-port flush-output)

  (define real-time current-milliseconds)

  (define table (make-hash-table))
  (define getprop
    (case-lambda
     [(k prop) (getprop k prop #f)]
     [(k prop def)
      (let ([al (hash-table-get table k (lambda () #f))])
	(if al
	    (let ([v (assq prop al)])
	      (if v
		  (unbox (cdr v))
		  def))
	    def))]))
  (define putprop
    (lambda (k prop nv)
      (let ([al (hash-table-get table k (lambda () '()))])
	(let ([v (assq prop al)])
	  (if v
	      (set-box! (cdr v) nv)
	      (hash-table-put! table k (cons (cons prop (box nv)) al)))))))

  ;; Chez's new-cafe
  (define new-cafe 
    (letrec ([nc
	      (case-lambda
	       [() (nc (current-eval))]
	       [(eval)
		(let/ec escape
		  (let ([orig-exit (exit-handler)]
			[orig-eval (current-eval)])
		    (dynamic-wind
			(lambda ()
			  (current-eval eval)
			  (exit-handler
			   (lambda (v) (escape v))))
			read-eval-print-loop
			(lambda ()
			  (current-eval orig-eval)
			  (exit-handler orig-exit)))))])])
      nc))

  (define-syntax define-structure
    (lambda (stx)
      (syntax-case stx ()
	[(_ (sname field ...))
	 (syntax (define-structure (sname field ...) ()))]
	[(_ (sname field ...) ([init-field init] ...))
	 (andmap identifier? (syntax->list 
			      (syntax (sname field ... init-field ...))))
	 (let ([name (symbol->string (syntax-e (syntax sname)))]
	       [fields (map symbol->string 
			    (map syntax-e 
				 (syntax->list (syntax (field ...)))))]
	       [init-fields (map symbol->string 
				 (map syntax-e 
				      (syntax->list (syntax (init-field ...)))))]
	       [+ (lambda args
		    (datum->syntax-object
		     (syntax sname)
		     (string->symbol (apply string-append args)) 
		     (syntax sname)))])
	   (with-syntax ([struct: (+ "struct:" name)]
			 [make- (+ "make-" name)]
			 [? (+ name "?")]
			 [(gs ...)
			  (apply
			   append
			   (map (lambda (f) (list (+ name "-" f)
						  (+ "set-" name "-" f "!")))
				(append fields init-fields)))])
	     (syntax
	      (define-values (struct: make- ? gs ...)
		(let ()
		  (define-struct sname (field ... init-field ...))
		  (values struct: 
			  (let ([make- (lambda (field ...)
					 (make- field ...
						init ...))])
			    make-)
			  ? gs ...))))))]))))
