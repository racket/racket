
(module defmacro mzscheme
  (require-for-syntax (lib "stx.ss" "syntax")
		      "private/dmhelp.ss")

  (provide define-macro
	   defmacro)

  (define-syntax define-macro
    (lambda (stx)
      (syntax-case stx ()
	[(_ (name . args) proc0 proc ...)
	 (begin
	   (unless (identifier? (syntax name))
	     (raise-syntax-error
	      #f
	      "expected an identifier for the macro name"
	      stx
	      (syntax name)))
	   (let loop ([args (syntax args)])
	     (cond
	      [(stx-null? args) 'ok]
	      [(identifier? args) 'ok]
	      [(stx-pair? args)
	       (unless (identifier? (stx-car args))
		 (raise-syntax-error
		  #f
		  "expected an identifier for a macro argument"
		  stx
		  (stx-car args)))
	       (loop (stx-cdr args))]
	      [else (raise-syntax-error
		     #f
		     "not a valid argument sequence after the macro name"
		     stx)]))
	   (syntax
	    (define-macro name (lambda args proc0 proc ...))))]
	[(_ name proc)
	 (begin
	   (unless (identifier? (syntax name))
	     (raise-syntax-error
	      #f
	      "expected an identifier for the macro name"
	      stx
	      (syntax name)))
	   (syntax
	    (define-syntax name
	      (let ([p proc])
		(unless (procedure? p)
		  (raise-type-error 
		   'define-macro 
		   "procedure (arity 1)"
		   p))
		(lambda (stx)
		  (let ([l (syntax->list stx)])
		    (unless (and l (procedure-arity-includes? p (sub1 (length l))))
		      (raise-syntax-error
		       #f
		       "bad form"
		       stx))
		    (let ([ht (make-hash-table)])
		      (datum->syntax-object 
		       stx
		       (dm-subst
			ht
			(apply proc (cdr (dm-syntax->datum stx ht))))
		       stx))))))))])))

  (define-syntax defmacro
    (syntax-rules ()
	[(_ name formals body1 body ...)
	 (define-macro (name . formals) body1 body ...)])))
