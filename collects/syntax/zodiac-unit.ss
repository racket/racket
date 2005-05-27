;; Zodiac compatibility layer,
;;  for programs that used to manipulate the
;;  output of zodiac elaboration.

(module zodiac-unit mzscheme
  (require (lib "unitsig.ss")
	   (lib "list.ss"))
  (require "kerncase.ss")

  (require "zodiac-sig.ss"
	   "stx.ss")

  (provide zodiac@)

  (define zodiac@
    (unit/sig zodiac^
      (import)
      
      (define (stx-bound-assq ssym l)
	(ormap (lambda (p)
		 (and (bound-identifier=? ssym (car p))
		      p))
	       l))

      ;; Back boxes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define-struct secure-box (value))

      (define init-value-list '())

      (define register-initial-value
	(lambda (index value-thunk)
	  (set! init-value-list
		(append init-value-list
			(list value-thunk)))))

      (define make-initial-value-vector
	(lambda ()
	  (let ((v (make-vector current-vector-size uninitialized-flag)))
	    (let loop ((index 0) (inits init-value-list))
	      (unless (null? inits)
		(vector-set! v index ((car inits)))
		(loop (add1 index) (cdr inits))))
	    v)))

      (define make-empty-back-box
	(lambda ()
	  (make-secure-box (make-initial-value-vector))))

      (define current-vector-size 2)
      
      (define next-client-count
	(let ((count -1))
	  (lambda ()
	    (set! count (add1 count))
	    (when (>= count current-vector-size)
	      (set! current-vector-size (* 2 current-vector-size)))
	    count)))

      (define-struct uninitialized-back ())
      (define uninitialized-flag (make-uninitialized-back))
      
      (define getters-setters
	(lambda (index)
	  (values
	   (lambda (back)		; getter
	     (let ((v (secure-box-value back)))
	       (with-handlers
		   ((exn:fail:contract?
		     (lambda (exception)
		       (vector-ref (extend-back-vector back) index))))
		 (let ((value (vector-ref v index)))
		   (if (uninitialized-back? value)
		       (let ((correct-value
			      ((list-ref init-value-list index))))
			 (vector-set! v index correct-value)
			 correct-value)
		       value)))))
	   (lambda (back value)		; setter
	     (let ((v (secure-box-value back)))
	       (with-handlers
		   ((exn:fail:contract?
		     (lambda (exception)
		       (vector-set! (extend-back-vector back) index value))))
		 (vector-set! v index value)))))))
  
      (define register-client
	(lambda (client-name default-initial-value-thunk)
	  (let ((index (next-client-count)))
	    (register-initial-value index default-initial-value-thunk)
	    (getters-setters index))))
      
      (define extend-back-vector
	(lambda (back-box)
	  (let ((v (secure-box-value back-box)))
	    (let ((new-v (make-initial-value-vector)))
	      (let loop ((n (sub1 (vector-length v))))
		(when (>= n 0)
		  (vector-set! new-v n (vector-ref v n))
		  (loop (sub1 n))))
	      (set-secure-box-value! back-box new-v)
	      new-v))))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (mk-back) (make-empty-back-box))

      (define (get-slot stx table)
	(let ([l (hash-table-get table (syntax-e stx) (lambda () null))])
	  (let ([s (ormap (lambda (b)
			    (and (module-identifier=? stx (car b))
				 (cdr b)))
			  l)])
	    (if s
		s
		(let ([s (box #f)])
		  (hash-table-put! table (syntax-e stx) (cons (cons stx s) l))
		  s)))))

      (define (let-s->z mk-let rec? stx env loop)
	(syntax-case stx ()
	  [(_ ([vars rhs] ...) . body)
	   (let* ([varses (syntax->list (syntax (vars ...)))]
		  [rhses (syntax->list (syntax (rhs ...)))]
		  [z:varses (map (lambda (vars)
				   (map (lambda (var)
					  (make-binding
					   stx
					   (mk-back)
					   (gensym (syntax-e var))
					   (syntax-e var)))
					(syntax->list vars)))
				 varses)]
		  [body-env (append
			     (apply
			      append
			      (map (lambda (z:vars vars)
				     (map (lambda (z:var var)
					    (cons
					     var
					     z:var))
					  z:vars
					  (syntax->list vars)))
				   z:varses
				   varses))
			     env)])
	     (mk-let
	      stx
	      (mk-back)
	      z:varses
	      (map (lambda (rhs)
		     (loop rhs (if rec? body-env env)))
		   rhses)
	      (loop (syntax (begin . body)) body-env)))]))
      
      (define (args-s->z env args)
	(let-values ([(maker ids)
		      (syntax-case args ()
			[id
			 (identifier? (syntax id))
			 (values make-sym-arglist
				 (list (syntax id)))]
			[(id ...)
			 (values make-list-arglist (syntax->list args))]
			[_else (values make-ilist-arglist
				       (let loop ([args args])
					 (syntax-case args ()
					   [id (identifier? args) (list args)]
					   [(id . rest)
					    (cons (syntax id) (loop (syntax rest)))])))])])
	  (let ([bindings
		 (map (lambda (id)
			(make-binding
			 id
			 (mk-back)
			 (gensym (syntax-e id))
			 (syntax-e id)))
		      ids)])
	    (values
	     (append (map cons ids bindings) env)
	     (maker bindings)))))

      (define (syntax->zodiac stx)
	(define slot-table (make-hash-table))
	(define trans-slot-table (make-hash-table))
	(define syntax-slot-table (make-hash-table))

	(if (eof-object? stx)
	    stx
	    (let loop ([stx stx][env null][trans? #f])
	      (kernel-syntax-case stx trans?
		[id
		 (identifier? stx)
		 (let ([a (stx-bound-assq stx env)])
		   (if a
		       ;; Lexical reference:
		       (make-bound-varref
			stx
			(mk-back)
			(binding-var (cdr a))
			(cdr a))
		       ;; Top-level (or module) reference:
		       (let ([b (let ([b ((if trans?
					      identifier-transformer-binding
					      identifier-binding)
					  stx)])
				  ;; If b, is it imported?
				  (and (pair? b)
				       (let ([modname (and (pair? b) (car b))])
					 (and (or (symbol? modname)
						  (and (module-path-index? modname)
						       (let-values ([(name base) (module-path-index-split modname)])
							 (or name base))))
					      b))))])
			 (make-top-level-varref
			  stx
			  (mk-back)
			  (if b
			      (cadr b)
			      (syntax-e stx))
			  (let ([modname (and b (car b))])
			    modname)
			  (get-slot stx (if trans? trans-slot-table slot-table))
			  trans?
			  (and b (list-ref b 4))
			  (and b
			       ((if trans?
				    identifier-transformer-binding-export-position
				    identifier-binding-export-position)
				stx))))))]

		[(#%top . id)
		 ;; Top-level reference:
		 (make-top-level-varref
		  stx
		  (mk-back)
		  (syntax-e (syntax id))
		  #f
		  (get-slot (syntax id) (if trans? trans-slot-table slot-table))
		  trans?
		  #f
		  #f)]

		[(#%datum . val)
		 (let ([val (syntax val)])
		   (make-quote-form
		    stx
		    (mk-back)
		    (make-zread
		     val)))]

		[(define-values names rhs)
		 (make-define-values-form
		  stx
		  (mk-back)
		  (map (lambda (stx)
			 (let ([b (identifier-binding stx)])
			   (make-top-level-varref
			    stx
			    (mk-back)
			    (if (pair? b)
				(cadr b)
				(syntax-e stx))
			    (and (pair? b) (car b))
			    (get-slot stx slot-table)
			    #f
			    #f
			    #f)))
		       (syntax->list (syntax names)))
		  (loop (syntax rhs) null #f))]
		
		[(-define names rhs)
		 (or (module-identifier=? #'-define #'define-syntaxes)
		     (module-identifier=? #'-define #'define-values-for-syntax))
		 (let ([for-stx? (module-identifier=? #'-define #'define-values-for-syntax)])
		   ((if for-stx?
			make-define-for-syntax-form
			make-define-syntaxes-form)
		    stx
		    (mk-back)
		    (map (lambda (stx)
			   (let ([b (identifier-binding stx)])
			     (make-top-level-varref
			      stx
			      (mk-back)
			      (if (pair? b)
				  (cadr b)
				  (syntax-e stx))
			      (and (pair? b) (car b))
			      (get-slot stx syntax-slot-table)
			      #f
			      for-stx?
			      #f)))
			 (syntax->list (syntax names)))
		    (loop (syntax rhs) null #t)))]
		
		[(module name init-require (#%plain-module-begin . body))
		 (let* ([body (map (lambda (x)
				     (loop x env trans?))
				   (syntax->list (syntax body)))]
			[get-required-modules
			 (lambda (req)
			   (let loop ([body body])
			     (cond
			      [(null? body) null]
			      [(and (require/provide-form? (car body))
				    (module-identifier=? req (stx-car (zodiac-stx (car body)))))
			       (append
				(map (lambda (r)
				       (syntax-case* r (prefix all-except rename)
					   (lambda (a b) (eq? (syntax-e a)
							      (syntax-e b)))
					 [mod
					  (identifier? r)
					  r]
					 [(prefix id mod)
					  (syntax mod)]
					 [(rename mod . _)
					  (syntax mod)]
					 [(all-except mod . _)
					  (syntax mod)]
					 [_else r]))
				     (stx->list (stx-cdr (zodiac-stx (car body)))))
				(loop (cdr body)))]
			      [else (loop (cdr body))])))]
			[rt-required
			 (cons (syntax init-require)
			       (get-required-modules (quote-syntax require)))]
			[et-required
			 (cons (syntax init-require)
			       (get-required-modules (quote-syntax require-for-syntax)))]
			[tt-required
			 (cons (syntax init-require)
			       (get-required-modules (quote-syntax require-for-template)))]
			[et-body
			 (filter (lambda (e)
				   (or (define-syntaxes-form? e)
				       (define-for-syntax-form? e)))
				 body)]
			[rt-body
			 (filter (lambda (e) (and (not (define-syntaxes-form? e))
						  (not (define-for-syntax-form? e))
						  (not (require/provide-form? e))))
				 body)])
		   (make-module-form
		    stx
		    (mk-back)
		    (syntax name)
		    rt-required
		    et-required
		    tt-required
		    (make-begin-form
		     stx
		     (mk-back)
		     rt-body)
		    (make-begin-form
		     stx
		     (mk-back)
		     et-body)
		    (syntax-property stx 'module-variable-provides)
		    (syntax-property stx 'module-syntax-provides)
		    (syntax-property stx 'module-indirect-provides)
		    (syntax-property stx 'module-kernel-reprovide-hint)
		    (syntax-property stx 'module-self-path-index)))]
		[(require i ...)
		 (make-require/provide-form
		  stx
		  (mk-back))]
		[(require-for-syntax i ...)
		 (make-require/provide-form
		  stx
		  (mk-back))]
		[(require-for-template i ...)
		 (make-require/provide-form
		  stx
		  (mk-back))]
		[(provide i ...)
		 (make-require/provide-form
		  stx
		  (mk-back))]

		[(quote expr)
		 (make-quote-form
		  stx
		  (mk-back)
		  (make-zread (syntax expr)))]

		[(quote-syntax expr)
		 (make-quote-syntax-form
		  stx
		  (mk-back)
		  (syntax expr))]
		
		[(lambda args . body)
		 (let-values ([(env args) (args-s->z env (syntax args))])
		   (make-case-lambda-form
		    stx
		    (mk-back)
		    (list args)
		    (list (loop (syntax (begin . body)) env trans?))))]
		[(case-lambda [args . body] ...)
		 (let-values ([(envs argses)
			       (let ([es+as
				      (map
				       (lambda (args)
					 (let-values ([(env args) (args-s->z env args)])
					   (cons env args)))
				       (syntax->list (syntax (args ...))))])
				 (values
				  (map car es+as)
				  (map cdr es+as)))])
		   (make-case-lambda-form
		    stx
		    (mk-back)
		    argses
		    (map (lambda (env body)
			   (with-syntax ([body body])
			     (loop (syntax (begin . body)) env trans?)))
			 envs
			 (syntax->list (syntax (body ...))))))]

		[(let-values . _)
		 (let-s->z make-let-values-form #f stx env
			   (lambda (b env) (loop b env trans?)))]
		[(letrec-values . _)
		 (let-s->z make-letrec-values-form #t stx env
			   (lambda (b env) (loop b env trans?)))]
		
		[(set! var rhs)
		 (make-set!-form
		  stx
		  (mk-back)
		  (loop (syntax var) env trans?)
		  (loop (syntax rhs) env trans?))]
		
		[(begin . exprs)
		 (make-begin-form
		  stx
		  (mk-back)
		  (map (lambda (x)
			 (loop x env trans?))
		       (syntax->list (syntax exprs))))]
		
		[(begin0 . exprs)
		 (make-begin0-form
		  stx
		  (mk-back)
		  (map (lambda (x)
			 (loop x env trans?))
		       (syntax->list (syntax exprs))))]

		[(if test then)
		 (make-if-form
		  stx
		  (mk-back)
		  (loop (syntax test) env trans?)
		  (loop (syntax then) env trans?)
		  (loop (syntax (#%app void)) env trans?))]

		[(if test then else)
		 (make-if-form
		  stx
		  (mk-back)
		  (loop (syntax test) env trans?)
		  (loop (syntax then) env trans?)
		  (loop (syntax else) env trans?))]

		[(with-continuation-mark k v body)
		 (make-with-continuation-mark-form
		  stx
		  (mk-back)
		  (loop (syntax k) env trans?)
		  (loop (syntax v) env trans?)
		  (loop (syntax body) env trans?))]

		[(#%app)
		 (make-quote-form
		  (syntax/loc stx ())
		  (mk-back)
		  (make-zread (quote-syntax ())))]
		[(#%app func arg ...)
		 (make-app
		  stx
		  (mk-back)
		  (loop (syntax func) env trans?)
		  (map
		   (lambda (arg)
		     (loop arg env trans?))
		   (syntax->list (syntax (arg ...)))))]
		
		[_else
		 (error 'syntax->zodiac
			"unrecognized expression form: ~e"
			(syntax-object->datum stx))]))))

      
      (define (zodiac->syntax x)
	(let loop ([x x])
	  (cond
	   [(zread? x)
	    (zodiac-stx x)]

	   [(top-level-varref? x)
	    (zodiac-stx x)]
	   [(bound-varref? x)
	    ;; An stx object is getting gensymmed here!
	    (datum->syntax-object #f (binding-var (bound-varref-binding x)) #f)]
	   
	   [(app? x)
	    (with-syntax ([fun (loop (app-fun x))]
			  [args (map loop (app-args x))])
	      (syntax (#%app fun . args)))]

	   [(if-form? x)
	    (with-syntax ([test (loop (if-form-test x))]
			  [then (loop (if-form-then x))]
			  [else (loop (if-form-else x))])
	      (syntax (if test then else)))]

	   [(quote-form? x)
	    (with-syntax ([v (zodiac-stx (quote-form-expr x))])
	      (syntax (quote v)))]
	   [(quote-syntax-form? x)
	    (with-syntax ([v (quote-syntax-form-expr x)])
	      (syntax (quote-syntax v)))]

	   [(begin-form? x)
	    (with-syntax ([body (map loop (begin-form-bodies))])
	      (syntax (begin . body)))]
	   [(begin0-form? x)
	    (with-syntax ([body (map loop (begin-form-bodies))])
	      (syntax (begin0 . body)))]

	   [(let-values-form? x)
	    (with-syntax ([(vars ...)
			   (map (lambda (vars)
				  (map binding-var vars))
				(let-values-form-vars x))]
			  [(val ...)
			   (map loop (let-values-form-vals x))]
			  [body (loop (let-values-form-body x))])
	      (syntax (let-values ([vars val] ...) body)))]
	   [(letrec-values-form? x)
	    (with-syntax ([(vars ...)
			   (map (lambda (vars)
				  (map binding-var vars))
				(letrec-values-form-vars x))]
			  [(val ...)
			   (map loop (letrec-values-form-vals x))]
			  [body (loop (letrec-values-form-body x))])
	      (syntax (letrec-values ([vars val] ...) body)))]
	   
	   [(define-values-form? x)
	    (with-syntax ([vars (map zodiac-stx (define-values-form-vars x))]
			  [val (loop (define-values-form-val x))])
	      (syntax (define-values vars val)))]
	   
	   [(set!-form? x)
	    (with-syntax ([var (loop (set!-form-var x))]
			  [val (loop (set!-form-val x))])
	      (syntax (set! var val)))]
	   
	   [(case-lambda-form? x)
	    (with-syntax ([(args ...)
			   (map (lambda (args)
				  (cond
				   [(sym-arglist? args)
				    (datum->syntax-object #f (binding-var (car (arglist-vars args))) #f)]
				   [(list-arglist? args)
				    (map (lambda (var)
					   (datum->syntax-object #f (binding-var var) #f))
					 (arglist-vars args))]
				   [(ilist-arglist? args)
				    (let loop ([vars (arglist-vars args)])
				      (let ([id (datum->syntax-object #f (binding-var (car vars)) #f)])
					(if (null? (cdr vars))
					    id
					    (cons id (loop (cdr vars))))))]))
				(case-lambda-form-args x))]
			  [(body ...)
			   (map loop (case-lambda-form-bodies x))])
	      (syntax (case-lambda [args body] ...)))]

	   [(with-continuation-mark-form? x)
	    (with-syntax ([key (loop (with-continuation-mark-form-key x))]
			  [val (loop (with-continuation-mark-form-val x))]
			  [body (loop (with-continuation-mark-form-body x))])
	      (syntax (with-continuation-mark key val body)))]

	   [else (error 'zodiac->syntax
			"unknown zodiac record type: ~e"
			x)])))

      (define (zodiac-origin z) z)

      (define (origin-who z)
	(if (syntax-original? (zodiac-stx z))
	    'source
	    'macro))

      (define (origin-how z)
	(syntax-property (zodiac-stx z) 'origin))
      
      (define (zodiac-start z) z)
      (define (zodiac-finish z) z)

      (define (location-line z)
	(and (zodiac-stx z) (syntax-line (zodiac-stx z))))
      
      (define (location-column z)
	(and (zodiac-stx z) (syntax-column (zodiac-stx z))))

      (define (location-file z)
	(and (zodiac-stx z) (syntax-source (zodiac-stx z))))

      (define (zread-object z)
	(syntax-e (zodiac-stx z)))

      (define (structurize-syntax sexp)
	(make-zread (datum->syntax-object #f sexp #f)))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define eof? eof-object?)

      (define-struct zodiac (stx))
      (define-struct (zread zodiac) ())

      (define-struct (parsed zodiac) (back))

      (define-struct (varref parsed) (var))

      (define-struct (top-level-varref varref) (module slot exptime? expdef? position))
      (define (create-top-level-varref z var module slot exptime? expdef? position)
	(make-top-level-varref (zodiac-stx z) (mk-back) var module slot exptime? expdef? position))

      (define-struct (bound-varref varref) (binding))
      (define (create-bound-varref z var binding)
	(make-bound-varref (zodiac-stx z) (mk-back) var binding))

      (define lexical-varref? bound-varref?)
      (define make-lexical-varref make-bound-varref)
      (define create-lexical-varref create-bound-varref)

      (define-struct (binding parsed) (var orig-name))
      (define (create-binding z var orig-name)
	(make-binding (zodiac-stx z) (mk-back) var orig-name))

      (define lexical-binding? binding?)
      (define make-lexical-binding make-binding)
      (define create-lexical-binding create-binding)


      (define-struct (app parsed) (fun args))
      (define (create-app z fun args)
	(make-app (zodiac-stx z) (mk-back) fun args))

      (define-struct (if-form parsed) (test then else))
      (define (create-if-form z test then else)
	(make-if-form (zodiac-stx z) (mk-back) test then else))

      (define-struct (quote-form parsed) (expr))
      (define (create-quote-form z expr)
	(make-quote-form (zodiac-stx z) (mk-back) expr))

      (define-struct (begin-form parsed) (bodies))
      (define (create-begin-form z bodies)
	(make-begin-form (zodiac-stx z) (mk-back) bodies))

      (define-struct (begin0-form parsed) (bodies))
      (define (create-begin0-form z bodies)
	(make-begin0-form (zodiac-stx z) (mk-back) bodies))

      (define-struct (let-values-form parsed) (vars vals body))
      (define (create-let-values-form z vars vals body)
	(make-let-values-form (zodiac-stx z) (mk-back) vars vals body))

      (define-struct (letrec-values-form parsed) (vars vals body))
      (define (create-letrec-values-form z vars vals body)
	(make-letrec-values-form (zodiac-stx z) (mk-back) vars vals body))

      (define-struct (define-values-form parsed) (vars val))
      (define (create-define-values-form z vars val)
	(make-define-values-form (zodiac-stx z) (mk-back) vars val))

      (define-struct (set!-form parsed) (var val))
      (define (create-set!-form z var val)
	(make-set!-form (zodiac-stx z) (mk-back) var val))

      (define-struct (case-lambda-form parsed) (args bodies))
      (define (create-case-lambda-form z args bodies)
	(make-case-lambda-form (zodiac-stx z) (mk-back) args bodies))

      (define-struct (with-continuation-mark-form parsed) (key val body))
      (define (create-with-continuation-mark-form z key val body)
	(make-with-continuation-mark-form (zodiac-stx z) (mk-back) key val body))

      (define-struct (quote-syntax-form parsed) (expr))
      (define (create-quote-syntax-form z expr)
	(make-quote-syntax-form (zodiac-stx z) (mk-back) expr))

      (define-struct (define-syntaxes-form parsed) (names expr))
      (define (create-define-syntaxes-form z names expr)
	(make-define-syntaxes-form (zodiac-stx z) (mk-back) names expr))

      (define-struct (define-for-syntax-form parsed) (names expr))
      (define (create-define-for-syntax-form z names expr)
	(make-define-for-syntax-form (zodiac-stx z) (mk-back) names expr))

      (define-struct (module-form parsed) (name requires for-syntax-requires for-template-requires 
						body syntax-body 
						provides syntax-provides indirect-provides
						kernel-reprovide-hint
						self-path-index))
      (define (create-module-form z name rt-requires et-requires tt-requires
				  rt-body et-body
				  var-provides syntax-provides indirect-provides
				  kernel-hint self)
	(make-module-form (zodiac-stx z) (mk-back) 
			  name rt-requires et-requires tt-requires 
			  rt-body et-body
			  var-provides syntax-provides indirect-provides
			  kernel-hint self))

      (define-struct (require/provide-form parsed) ())
      (define (create-require/provide-form z)
	(make-require/provide-form (zodiac-stx z) (mk-back)))

      (define-struct arglist (vars))
      (define-struct (sym-arglist arglist) ())
      (define-struct (list-arglist arglist) ())
      (define-struct (ilist-arglist arglist) ()))))
