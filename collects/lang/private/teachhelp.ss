
(module teachhelp mzscheme
  (require "firstorder.ss")
  
  (provide make-undefined-check 
	   make-first-order-function)
  
  (define (make-undefined-check check-proc tmp-id)
    (let ([set!-stx (datum->syntax-object check-proc 'set!)])
      (make-set!-transformer
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (with-syntax ([tmp-id tmp-id])
	      (syntax/loc stx (set! tmp-id expr)))]
	   [(id . args)
	    (datum->syntax-object
	     check-proc
	     (cons (syntax-property
		    (datum->syntax-object
		     check-proc
		     (list check-proc 
			   (list 'quote (syntax id))
			   tmp-id))
		    'stepper-skipto
		    (list syntax-e cdr syntax-e cdr cdr car))
		   (syntax args))
	     stx)]
	   [id
	    (syntax-property
	     (datum->syntax-object
	      check-proc
	      (list check-proc 
		    (list 'quote (syntax id))
		    tmp-id)
	      stx)
             'stepper-skipto
             (list syntax-e cdr syntax-e cdr cdr car))]))))) ; this may make other stepper-skipto annotations obsolete.

  (define (appropriate-use what)
    (case what
     [(constructor)
      "called with values for the structure fields"]
     [(selector) 
      "applied to a structure to get the field value"]
     [(predicate procedure)
      "applied to arguments"]))

  (define (make-first-order-function what arity orig-id app)
    (make-set!-transformer
     (make-first-order
      (lambda (stx)
	(syntax-case stx (set!)
	  [(set! . _) (raise-syntax-error 
		       #f stx #f 
		       "internal error: assignment to first-order function")]
	  [id
	   (identifier? #'id)
	   (raise-syntax-error
	    #f
	    (format "this is a ~a, so it must be ~a (which requires using a parenthesis before the name)"
		    what
		    (appropriate-use what))
	    stx
	    #f)]
	  [(id . rest)
	   (let ([l (length (syntax->list #'rest))])
	     (unless (= l arity)
	       (raise-syntax-error
		#f
		(format "this ~a expects ~a argument~a, here it is provided ~a argument~a"
			what 
			arity (if (= 1 arity) "" "s")
			l (if (= 1 l) "" "s"))
		stx
		#f))
	     (datum->syntax-object
	      app
	      (list* app (datum->syntax-object orig-id (syntax-e orig-id) #'id #'id) #'rest)
	      stx stx))]))
      (syntax-local-introduce orig-id)))))
