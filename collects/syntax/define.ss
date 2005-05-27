
(module define mzscheme
  (require (lib "stx.ss" "syntax"))

  (provide normalize-definition)

  ;; This code was shamefully copied from MzScheme's startup.ss!

  (define normalize-definition
    (case-lambda 
     [(stx lambda-stx check-context?)
      (when (and check-context?
		 (memq (syntax-local-context) '(expression)))
	(raise-syntax-error 
	 #f
	 "not allowed in an expression context"
	 stx))
      (syntax-case stx ()
	[(_ id expr)
	 (identifier? #'id)
	 (values #'id #'expr)]
	[(_ id . rest)
	 (identifier? #'id)
	 (raise-syntax-error
	  #f
	  (syntax-case stx ()
	    [(_ id expr ...)
	     "bad syntax (multiple expressions after identifier)"]
	    [(_ id)
	     "bad syntax (zero expressions after identifier)"]
	    [(_ id . rest)
	     "bad syntax (illegal use of `.')"])
	  stx)]
	[(_ something . rest)
	 (not (stx-pair? #'something))
	 (raise-syntax-error
	  #f
	  "bad syntax"
	  stx
	  #'something)]
	[(_ proto . body)
	 (let-values ([(id mk-rhs)
		       (letrec ([simple-proto
				 ;; check the args and set up a proc-maker; we return
				 ;;  a proc maker instead of a final proc to enable
				 ;;  left-to-right checking of the function protos
				 (lambda (proto)
				   (let-values ([(args mk-rhs)
						 (syntax-case proto ()
						   [(id arg ...)
						    (values (syntax->list #'(arg ...))
							    (lambda (body)
							      (quasisyntax/loc stx (#,lambda-stx (arg ...)
												 . #,body))))]
						   [(id arg ... . rest)
						    (values (syntax->list #'(arg ... rest))
							    (lambda (body)
							      (quasisyntax/loc stx 
								(#,lambda-stx (arg ... . rest)
									      . #,body))))])])
				     (for-each (lambda (a)
						 (unless (identifier? a)
						   (raise-syntax-error
						    #f
						    "not an identifier for procedure argument"
						    stx
						    a)))
					       args)
				     (let ([dup (check-duplicate-identifier args)])
				       (when dup
					 (raise-syntax-error
					  #f
					  "duplicate argument identifier"
					  stx
					  dup)))
				     mk-rhs))]
				[general-proto
				 ;; proto is guaranteed to be a stx-pair
				 (lambda (proto)
				   (syntax-case proto ()
				     [(id . rest)
				      (identifier? #'id)
				      (values #'id
					      (simple-proto proto))]
				     [((something . more) . rest)
				      (let-values ([(id mk-rhs) (general-proto #'(something . more))])
					(let ([mk-inner (simple-proto proto)])
					  (values id
						  (lambda (body)
						    (mk-rhs (list (mk-inner body)))))))]
				     [(other . rest)
				      (raise-syntax-error
				       #f
				       "bad syntax (not an identifier for procedure name, and not a nested procedure form)"
				       stx
				       #'other)]))])
			 (general-proto #'proto))])
	   (unless (stx-list? #'body)
	     (raise-syntax-error
	      #f
	      "bad syntax (illegal use of `.' for procedure body)"
	      stx))
	   (when (stx-null? #'body)
	     (raise-syntax-error
	      #f
	      "bad syntax (no expressions for procedure body)"
	      stx))
	   (values id (mk-rhs #'body)))])]
     [(stx lambda-stx) (normalize-definition stx lambda-stx #t)])))
