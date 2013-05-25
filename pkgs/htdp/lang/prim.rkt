;; Provides `define-primitive' and `define-higher-order-primitive'
;; for use in teachpacks for Beginner, especially those that
;; define a primitive operator that consumes a procedure.
;; See manual for more information.

(module prim mzscheme
  (require lang/error
           (rename lang/htdp-beginner beginner-app #%app))
  
  (require-for-syntax (prefix fo: "private/firstorder.rkt")
                      stepper/private/syntax-property)
  
  (provide define-primitive
	   define-higher-order-primitive
	   provide-primitive
	   provide-higher-order-primitive
	   provide-primitives
           first-order->higher-order)

  (define-syntax (define-primitive stx)
    (define (go name implementation struct-info)
      (with-syntax ([name name][implementation implementation])
        (with-syntax ([impl #'(let ([name (lambda argv
                                            (apply implementation argv))])
                                name)])
          #`(begin
              ;; Make sure that `implementation' is bound:
              (define-values () (begin (lambda () implementation) (values)))
              ;; Bind `name':
              (define-syntax name 
                (#,(if struct-info
                       #'fo:make-first-order+struct
                       #'fo:make-first-order)
                 (lambda (stx)
                   (with-syntax ([tagged-impl (stepper-syntax-property
                                               (stepper-syntax-property (quote-syntax impl) 'stepper-skip-completely #t)
                                               'stepper-prim-name
                                               (quote-syntax name))])
                     (syntax-case stx ()
                       [(_ . body)
                        ;; HACK: we disable all checks if #%app is not beginner-app
                        (not (module-identifier=? #'beginner-app (datum->syntax-object stx '#%app)))
                        (syntax/loc stx (tagged-impl . body))]
                       [_
                        ;; HACK: see above
                        (not (module-identifier=? #'beginner-app (datum->syntax-object stx '#%app)))
                        (syntax/loc stx tagged-impl)]
                       [(id . args)
                        (syntax/loc stx (#%plain-app tagged-impl . args))]
                       [_
                        (raise-syntax-error
                         #f
                         "expected a function call, but there is no open parenthesis before this function"
                         stx)])))
                 ((syntax-local-certifier #t)
                  #'impl)
                 #,@(if struct-info
                        (list struct-info)
                        '())))))))
    (syntax-case stx ()
      [(_ name implementation) (go #'name #'implementation #f)]
      [(_ name implementation inf)
       (go #'name #'implementation #'inf)]))


  (define-syntax (define-higher-order-primitive stx)
    (define (is-proc-arg? arg)
      (not (eq? '_ (syntax-e arg))))
    (syntax-case stx ()
      [(_ name implementation (arg ...))
       (let ([args (syntax->list (syntax (arg ...)))])
         (for-each (lambda (id)
                     (unless (identifier? id)
                       (raise-syntax-error #f "expected a variable" stx id)))
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
					   "expects a function in this position")
					  s
					  (#,#'syntax #,new-arg)))))
			       args new-args)]
			 [(wrapped-arg ...)
			  (map (lambda (arg new-arg)
				 (cond
				  [(not (is-proc-arg? arg)) new-arg]
				  [else #`(first-order->higher-order #,new-arg)]))
			       args new-args)]
			 [num-arguments (length args)])
	     (with-syntax ([impl #'(let ([name (lambda (new-arg ...)
                                                 (implementation new-arg ...))])
                                     name)])
	       (syntax/loc stx
		   (define-syntax name
                     (fo:make-first-order
                      (lambda (s)
                        (with-syntax ([tagged-impl (stepper-syntax-property
                                                    (stepper-syntax-property (quote-syntax impl) 'stepper-skip-completely #t)
                                                    'stepper-prim-name
                                                    (quote-syntax name))])
                          (syntax-case s ()
                            [(_ . body)
                             ;; HACK: see above
                             (not (module-identifier=? #'beginner-app (datum->syntax-object s '#%app)))
                             (syntax/loc s (tagged-impl . body))]
                            [_
                             ;; HACK: see above
                             (not (module-identifier=? #'beginner-app (datum->syntax-object s '#%app)))
                             (syntax/loc s tagged-impl)]
                            [(_ new-arg ...)
                             (begin
                               checks ...
                               ;; s is a well-formed use of the primitive;
                               ;; generate the primitive implementation
                               (syntax/loc s (tagged-impl wrapped-arg ...))
                               )]
                            [(_ . rest)
                             (let ([num-actuals (length (syntax->list #'rest))])
                               (raise-syntax-error
                                #f
                                (format
                                 "this function expects ~a argument~a, here it is provided ~a argument~a"
                                 num-arguments
                                 (if (= num-arguments 1) "" "s")
                                 num-actuals
                                 (if (= num-actuals 1) "" "s"))
                                s))]
                            [_
                             (raise-syntax-error
                              #f
                              "expected a function call, but there is no open parenthesis before this function"
                              s)])))
                      ((syntax-local-certifier #t)
                       #'impl))))))))]))

  (define-syntax (first-order->higher-order stx)
    (syntax-case stx ()
      [(_ id) (identifier? #'id) (fo:first-order->higher-order #'id)]
      [(_ expr) #'expr]))

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


