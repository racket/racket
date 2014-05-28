(module teachhelp mzscheme
  (require "firstorder.rkt"
           "rewrite-error-message.rkt"
           stepper/private/syntax-property
           (for-template (prefix r: racket/base)))

  (provide make-first-order-function
           redirect-identifier-to)

  (define (redirect-identifier-to set!-stx tmp-id)
      (make-set!-transformer
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (with-syntax ([tmp-id tmp-id])
	      (syntax/loc stx (set! tmp-id expr)))]
	   [(id . args)
            (datum->syntax-object
             stx
             (cons '#%app stx)
             stx
             stx)]
	   [id
            tmp-id]))))
#;    
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
	    (format "expected a function call, but there is no open parenthesis before this function")
	    stx
	    #f)]
	  [(id . rest)
	   (let ([found (length (syntax->list #'rest))])
	     (unless (= found arity)
	       (raise-syntax-error
		#f
                (argcount-error-message #f arity found)
		stx
		#f))
	     (datum->syntax-object
	      app
	      (list* app (datum->syntax-object orig-id (syntax-e orig-id) #'id #'id) #'rest)
	      stx stx))]))
      (syntax-local-introduce orig-id)))))
