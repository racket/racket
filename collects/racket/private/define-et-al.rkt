
;;----------------------------------------------------------------------
;; -define, when, unless, let/ec, define-struct

(module define-et-al '#%kernel
  (#%require (for-syntax '#%kernel "stx.rkt" "qq-and-or.rkt" 
                         "member.rkt" "cond.rkt"))
  
  ;; No error checking here, because these macros merely help
  ;;  us write macros before the real define and define-syntax
  (define-syntaxes (-define -define-syntax)
    (let ([here (quote-syntax here)])
      (let ([mk-define
	     (lambda (base)
	       (lambda (code)
		 (let ([body (stx-cdr code)])
		   (let ([first (stx-car body)])
		     (cond
		      [(identifier? first)
		       (datum->syntax
			here
			`(,base (,first) ,@(stx->list (stx-cdr body)))
			code)]
		      [else
		       (let ([pbody (stx-cdr body)])
			 (datum->syntax
			  (quote-syntax here)
			  `(,base (,(stx-car first)) 
				  (lambda ,(stx-cdr first) ,@(stx->list pbody)))
			  code))])))))])
	(values (mk-define (quote-syntax define-values))
		(mk-define (quote-syntax define-syntaxes))))))

  (-define-syntax when
    (lambda (x)
      (let ([l (syntax->list x)])
	(if (and l
		 (> (length l) 2))
	    (datum->syntax
	     (quote-syntax here)
	     (list (quote-syntax if)
		   (stx-car (stx-cdr x))
		   (list*
		    (quote-syntax let-values)
                    (quote-syntax ())
		    (stx-cdr (stx-cdr x)))
                   (quote-syntax (void)))
	     x)
	    (raise-syntax-error
	     #f
	     "bad syntax"
	     x)))))

  (-define-syntax unless
    (lambda (x)
      (let ([l (syntax->list x)])
	(if (and l
		 (> (length l) 2))
	    (datum->syntax
	     (quote-syntax here)
	     (list (quote-syntax if)
		   (cadr l)
		   (quote-syntax (void))
		   (list*
		    (quote-syntax let-values)
                    (quote-syntax ())
		    (cddr l)))
	     x)
	    (raise-syntax-error
	     #f
	     "bad syntax"
	     x)))))

  (-define-syntax let/ec 
    (lambda (code)
      (let ([l (syntax->list code)])
	(if (and l
		 (> (length l) 2)
		 (identifier? (cadr l)))
	    (let ([var (cadr l)]
		  [exprs (stx-cdr (stx-cdr code))])
	      (datum->syntax
	       (quote-syntax here)
	       `(call/ec (lambda (,var) ,@(stx->list exprs)))
	       code))
	    (raise-syntax-error
	     #f
	     "bad syntax"
	     code)))))

  (define-syntaxes (-define-struct)
    (let ([make-core
	   ;; generates the call to `make-struct-type'
	   (lambda (name inspector super-id/struct: field-names)
	     `(let-values ([(type maker pred access mutate)
			    (make-struct-type ',name
					      ,super-id/struct:
					      ,(length field-names)
					      0 #f null
					      ,inspector)])
		(values type maker pred
			,@(let loop ([field-names field-names][n 0])
			    (if (null? field-names)
				null
				(list* `(make-struct-field-accessor access ,n ',(car field-names))
				       `(make-struct-field-mutator mutate ,n ',(car field-names))
				       (loop (cdr field-names) (add1 n))))))))])
      ;; define-struct
      (lambda (stx)
	(if (identifier? stx)
	    (raise-syntax-error #f "bad syntax" stx)
            (void))
	(let ([body (stx->list (stx-cdr stx))])
	  (let ([syntax-error
		 (lambda (s . detail)
		   (apply
		    raise-syntax-error
		    #f
		    s
		    stx
		    detail))]
		[build-struct-names
		 (lambda (name fields)
		   (let ([name (symbol->string (syntax-e name))]
			 [fields (map symbol->string (map syntax-e fields))]
			 [+ string-append])
		     (map string->symbol
			  (append
			   (list 
			    (+ "struct:" name)
			    (+ "make-" name)
			    (+ name "?"))
			   (apply
			    append
			    (map
			     (lambda (f) 
			       (list 
				(+ name "-" f)
				(+ "set-" name "-" f "!")))
			     fields))))))])
	    (or (pair? body)
		(syntax-error "empty declaration"))
	    (or (stx-list? body)
		(syntax-error "illegal use of `.'"))
	    (or (<= 2 (length body) 3)
		(syntax-error "wrong number of parts"))
	    (or (identifier? (car body))
		(and (stx-pair? (car body))
		     (identifier? (stx-car (car body)))
		     (stx-pair? (stx-cdr (car body)))
		     (identifier? (stx-car (stx-cdr (car body))))
		     (stx-null? (stx-cdr (stx-cdr (car body)))))
		(syntax-error "first part must be an identifier or pair of identifiers"))
	    (or (stx-list? (cadr body))
		(if (stx-pair? (cadr body))
		    (syntax-error "illegal use of `.' in field name sequence")
		    (syntax-error "field names must be a sequence")))
	    (for-each (lambda (x) 
			(or (identifier? x)
			    (syntax-error "field name not a identifier" x)))
		      (stx->list (cadr body)))
	    (if (memq (syntax-local-context) '(expression))
		(syntax-error "allowed only in definition contexts")
                (void))
	    (let ([name (if (identifier? (car body))
			    (car body)
			    (stx-car (car body)))]
		  [field-names (stx->list (cadr body))]
		  [inspector (if (null? (cddr body))
				 (quote-syntax (current-inspector))
				 (caddr body))]
		  [super-id (if (identifier? (car body))
				#f
				(stx-car (stx-cdr (car body))))])
	      (let ([defined-names (map 
				    (lambda (n) (datum->syntax name n name)) 
				    (build-struct-names name field-names))])
		(let-values ([(super-id/struct: stx-info) (values #f #f)])
		  (let ([result
			 (datum->syntax
			  (quote-syntax here)
			  `(begin
			     (define-values
			       ,defined-names
			       ,(let ([core (make-core name (and inspector 'inspector) super-id/struct: field-names)])
				  (if inspector
				      `(let-values ([(inspector) ,inspector])
					 (if (if inspector (not (inspector? inspector)) #f)
					     (raise-argument-error 'define-struct "(or/c inspector? #f)" inspector)
                                             (void))
					 ,core)
				      core)))
			     (define-syntaxes (,name) ,stx-info))
			  stx)])
		    (if super-id
			(syntax-property result 
					 'disappeared-use 
					 (syntax-local-introduce super-id))
			result))))))))))

  (#%provide -define -define-syntax when unless let/ec -define-struct))
