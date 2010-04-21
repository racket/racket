
(module norm-define '#%kernel
  (#%require "small-scheme.rkt" "stxcase-scheme.rkt" "stx.rkt" "qqstx.rkt")

  (#%provide normalize-definition)

  (define-values (normalize-definition)
    (case-lambda 
     [(stx lambda-stx check-context? allow-key+opt?)
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
	    [(_ id expr0 expr ...)
	     "bad syntax (multiple expressions after identifier)"]
	    [(_ id)
	     "bad syntax (missing expression after identifier)"]
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
				   (let-values ([(args rests mk-rhs)
						 (syntax-case proto ()
						   [(id arg ...)
						    (values (syntax->list #'(arg ...))
                                                            null
							    (lambda (body)
							      (quasisyntax/loc stx (#,lambda-stx (arg ...)
												 . #,body))))]
						   [(id arg ... . rest)
						    (values (syntax->list #'(arg ...))
                                                            (list #'rest)
							    (lambda (body)
							      (quasisyntax/loc stx 
								(#,lambda-stx (arg ... . rest)
									      . #,body))))])])
                                     (let* ([args (if allow-key+opt?
                                                      (let* ([kw-ht (make-hasheq)]
                                                             [check-kw
                                                              (lambda (kw)
                                                                (when (hash-ref kw-ht (syntax-e kw) #f)
                                                                  (raise-syntax-error
                                                                   #f
                                                                   "duplicate keyword for argument"
                                                                   stx
                                                                   kw))
                                                                (hash-set! kw-ht (syntax-e kw) #t))])
                                                        (let loop ([args args][need-def? #f])
                                                          (syntax-case args ()
                                                            [() null]
                                                            [(id . more)
                                                             (identifier? #'id)
                                                             (if need-def?
                                                                 (raise-syntax-error
                                                                  #f
                                                                  "default-value expression missing"
                                                                  stx
                                                                  #'id)
                                                                 (cons #'id (loop #'more #f)))]
                                                            [([id def-expr] . more)
                                                             (identifier? #'id)
                                                             (cons #'id (loop #'more #t))]
                                                            [(kw id . more)
                                                             (and (keyword? (syntax-e #'kw))
                                                                  (identifier? #'id))
                                                             (begin
                                                               (check-kw #'kw)
                                                               (cons #'id (loop #'more need-def?)))]
                                                            [(kw [id def-expr] . more)
                                                             (and (keyword? (syntax-e #'kw))
                                                                  (identifier? #'id))
                                                             (begin
                                                               (check-kw #'kw)
                                                               (cons #'id (loop #'more need-def?)))]
                                                            [(kw . more)
                                                             (keyword? (syntax-e #'kw))
                                                             (raise-syntax-error #f
                                                                                 "missing argument identifier after keyword"
                                                                                 stx
                                                                                 #'kw)]
                                                            [(x . more)
                                                             (raise-syntax-error
                                                              #f
                                                              "not an identifier, identifier with default, or keyword for procedure argument"
                                                              stx
                                                              #'x)])))
                                                      args)]
                                            [all-args (if (null? rests)
                                                          args
                                                          (append args rests))])
                                       (for-each (lambda (a)
                                                   (unless (identifier? a)
                                                     (raise-syntax-error
                                                      #f
                                                      "not an identifier for procedure argument"
                                                      stx
                                                      a)))
                                                 all-args)
                                       (let ([dup (check-duplicate-identifier all-args)])
                                         (when dup
                                           (raise-syntax-error
                                            #f
                                            "duplicate argument identifier"
                                            stx
                                            dup)))
                                       mk-rhs)))]
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
     [(stx lambda-stx check-context?) (normalize-definition stx lambda-stx check-context? #f)]
     [(stx lambda-stx) (normalize-definition stx lambda-stx #t #f)])))
