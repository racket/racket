
;; Provides `define-primitive' and `define-higher-order-primitive'
;; for use in teachpacks for Beginner, especially those that
;; define a primitive operator that consumes a procedure.
;; See doc.txt for more information.

(module prim mzscheme
  (require (lib "error.ss" "lang")
	   (rename (lib "htdp-beginner.ss" "lang") beginner-app #%app))
  (require-for-syntax "private/firstorder.ss")
  
  (provide define-primitive
	   define-higher-order-primitive
	   provide-primitive
	   provide-higher-order-primitive
	   provide-primitives)

  (define-syntax (define-primitive stx)
    (syntax-case stx ()
      [(_ name implementation)
       (with-syntax ([impl #'(let ([name (lambda argv
                                           (apply implementation argv))])
                               name)])
	 #'(define-syntax (name stx)
             (with-syntax ([tagged-impl (syntax-property
                                         (syntax-property (quote-syntax impl) 'stepper-skip-completely #t)
                                         'stepper-prim-name
                                         (quote-syntax name))])
	     (syntax-case stx ()
	       [(__ . ___)
		;; HACK: we disable all checks if #%app is not beginner-app
		(not (module-identifier=? #'beginner-app (datum->syntax-object stx '#%app)))
                (syntax/loc stx (tagged-impl . ___))]
	       [__
		;; HACK: see above
		(not (module-identifier=? #'beginner-app (datum->syntax-object stx '#%app)))
                (syntax/loc stx tagged-impl)]
	       [(id . args)
                (syntax/loc stx (#%app tagged-impl . args))]
	       [_else
		(raise-syntax-error
		 #f
		 (string-append
		  "this primitive operator must be applied to arguments; "
		  "expected an open parenthesis before the operator name")
		 stx)]))))]))

  (define-syntax (define-higher-order-primitive stx)
    (define (is-proc-arg? arg)
      (not (eq? '_ (syntax-e arg))))
    (syntax-case stx ()
      [(_ name implementation (arg ...))
       (let ([args (syntax->list (syntax (arg ...)))])
         (for-each (lambda (id)
                     (unless (identifier? id)
                       (raise-syntax-error #f "not an identifier" stx id)))
                   (cons (syntax name)
                         args))
	 (let ([new-args (generate-temporaries args)])
	   (with-syntax ([(new-arg ...) new-args]
			 [(checks ...)
			  (map (lambda (arg new-arg)
				 (if (not (is-proc-arg? arg))
				     #'(void)
				     #`(unless (identifier? (#,#'syntax #,new-arg))
					 (raise-syntax-error
					  #f
					  (format
					   "primitive operator ~a expects a defined procedure name (usually `~a') in this position"
					   'name
					   '#,arg)
					  s
					  (#,#'syntax #,new-arg)))))
			       args new-args)]
			 [(wrapped-arg ...)
			  (map (lambda (arg new-arg)
				 (cond
				  [(not (is-proc-arg? arg)) new-arg]
				  [else #`(fo->ho #,new-arg)]))
			       args new-args)]
			 [num-arguments (length args)])
	     (with-syntax ([impl #'(let ([name (lambda (new-arg ...)
                                                 (implementation new-arg ...))])
                                     name)])
	       (syntax/loc stx
		   (define-syntax (name s)
		     (with-syntax ([tagged-impl (syntax-property
                                                 (syntax-property (quote-syntax impl) 'stepper-skip-completely #t)
                                                 'stepper-prim-name
                                                 (quote-syntax name))])
                         (syntax-case s ()
		       [(__ . ___)
			;; HACK: see above
			(not (module-identifier=? #'beginner-app (datum->syntax-object s '#%app)))
                        (syntax/loc s (tagged-impl . ___))]
		       [__
			;; HACK: see above
			(not (module-identifier=? #'beginner-app (datum->syntax-object s '#%app)))
                        (syntax/loc s tagged-impl)]
		       [(__ new-arg ...)
                        (begin
			  checks ...
			  ;; s is a well-formed use of the primitive;
			  ;; generate the primitive implementation
                          (syntax/loc s (tagged-impl wrapped-arg ...))
                          )]
		       [(__ . rest)
			(raise-syntax-error
			 #f
			 (format
			  "primitive operator requires ~a arguments"
			  num-arguments)
			 s)]
		       [_else
			(raise-syntax-error
			 #f
			 (string-append
			  "this primitive operator must be applied to arguments; "
			  "expected an open parenthesis before the operator name")
			 s)]))))))))]))

  (define-syntax (fo->ho stx)
    (syntax-case stx ()
      [(_ id) (first-order->higher-order #'id)]))

  (define-syntax (provide-primitive stx)
    (syntax-case stx ()
      [(_ name)
       (with-syntax ([ex-name ((make-syntax-introducer) #'name)])
	 #'(begin
	     (define-primitive ex-name name)
	     (provide ex-name)))]))

  (define-syntax (provide-primitives stx)
    (syntax-case stx ()
      [(_ name ...)
       #'(begin
	   (provide-primitive name)
	   ...)]))

  (define-syntax (provide-higher-order-primitive stx)
    (syntax-case stx ()
      [(_ name (arg ...))
       (with-syntax ([ex-name ((make-syntax-introducer) #'name)])
	 #'(begin
	     (define-higher-order-primitive ex-name name (arg ...))
	     (provide ex-name)))])))


