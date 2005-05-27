#cs(module compile mzscheme
     (require "parse.ss"
              (lib "match.ss")
              (lib "list.ss"))
     
     (provide compile-simplified)

     ;; The compiler generates references to prim.ss and
     ;; runtime.ss exports, as well as MzScheme forms
     ;; and functions. The `ctx' argument provides
     ;; an appropriate context for those bindings (in
     ;; the form of a syntax object to use with d->s-o).
     (define (compile-simplified stmt ctx)
       (datum->syntax-object 
	ctx
	(parameterize ([current-compile-context ctx])
	  (compile-a60 stmt 'void (empty-context) #t))))

     (define current-compile-context (make-parameter #f))
     
     (define (compile-a60 stmt next-label context add-to-top-level?)
       (match stmt
         [($ a60:block decls statements)
          (compile-block decls statements next-label context add-to-top-level?)]
         [else
          (compile-statement stmt next-label context)]))
     
     (define (compile-block decls statements next-label context add-to-top-level?)
       (let* ([labels-with-numbers (map car statements)]
              [labels (map (lambda (l)
                             (if (stx-number? l)
				 (datum->syntax-object
				  l
				  (string->symbol (format "~a" (syntax-e l)))
				  l
				  l)
                                 l))
                           labels-with-numbers)]
              ;; Build environment by adding labels, then decls:
              [context (foldl (lambda (decl context)
                                (match decl
                                  [($ a60:proc-decl result-type var arg-vars by-value-vars arg-specs body)
                                   (add-procedure context var result-type arg-vars by-value-vars arg-specs)]
                                  [($ a60:type-decl type ids)
                                   (add-atoms context ids type)]
                                  [($ a60:array-decl type arrays)
                                   (add-arrays context 
                                               (map car arrays) ; names
                                               (map cdr arrays) ; dimensions
                                               type)]
                                  [($ a60:switch-decl name exprs)
                                   (add-switch context name)]))
                              (add-labels
                               context
                               labels)
                              decls)])
         ;; Generate bindings and initialization for all decls,
         ;; plus all statements (thunked):
	 (let ([bindings
		(append
		 (apply
		  append
                  ;; Decls:
		  (map (lambda (decl)
			 (match decl
                           [($ a60:proc-decl result-type var arg-vars by-value-vars arg-specs body)
                            (let ([code
                                   `(lambda (kont . ,arg-vars)
                                      ;; Extract by-value variables
                                      (let ,(map (lambda (var)
                                                   `[,var (get-value ,var)])
                                                 by-value-vars)
                                        ;; Set up the result variable and done continuation:
                                        ,(let ([result-var (gensym 'prec-result)]
                                               [done (gensym 'done)])
                                           `(let* ([,result-var undefined]
                                                   [,done (lambda () (kont ,result-var))])
                                              ;; Include the compiled body:
                                              ,(compile-a60 body done
                                                            (add-settable-procedure
                                                             (add-bindings
                                                              context
                                                              arg-vars
                                                              by-value-vars
                                                              arg-specs)
                                                             var
                                                             result-type
                                                             result-var)
                                                            #f)))))])
                              (if add-to-top-level?
                                  `([,var
                                     (let ([tmp ,code])
                                       (namespace-set-variable-value! ',var tmp)
                                       tmp)])
                                  `([,var
                                     ,code])))]
			   [($ a60:type-decl type ids)
			    (map (lambda (id) `[,id undefined]) ids)]
			   [($ a60:array-decl type arrays)
			    (map (lambda (array) 
				   `[,(car array) (make-array
						   ,@(apply
						      append
						      (map
						       (lambda (bp)
							 (list
							  (compile-expression (car bp) context 'num)
							  (compile-expression (cdr bp) context 'num)))
						       (cdr array))))])
				 arrays)]
			   [($ a60:switch-decl name exprs)
			    `([,name (make-switch ,@(map (lambda (e) `(lambda () ,(compile-expression e context 'des)))
							 exprs))])]
			   [else (error "can't compile decl")]))
		       decls))
                 ;; Statements: most of the work is in `compile-statement', but
                 ;;  we provide the continuation label:
		 (cdr
		  (foldr (lambda (stmt label next-label+compiled)
			   (cons label
				 (cons
				  `[,label
				    (lambda ()
				      ,(compile-statement (cdr stmt) 
							  (car next-label+compiled)
							  context))]
				  (cdr next-label+compiled))))
			 (cons next-label null)
			 statements
			 labels)))])
           ;; Check for duplicate bindings:
	   (let ([dup (check-duplicate-identifier (filter identifier? (map car bindings)))])
	     (when dup
	       (raise-syntax-error
		#f
		"name defined twice"
		dup)))
           ;; Generate code; body of leterec jumps to the first statement label.
	   `(letrec ,bindings 
              (,(caar statements))))))
     
     (define (compile-statement statement next-label context)
       (match statement
         [($ a60:block decls statements)
          (compile-block decls statements next-label context #f)]
         [($ a60:branch test ($ a60:goto then) ($ a60:goto else))
          `(if (check-boolean ,(compile-expression test context 'bool))
               (goto ,(check-label then context))
               (goto ,(check-label else context)))]
         [($ a60:goto label)
          (at (expression-location label)
              `(goto ,(compile-expression label context 'des)))]
         [($ a60:dummy)
          `(,next-label)]
         [($ a60:call proc args)
          (at (expression-location proc)
              `(,(compile-expression proc context 'func)
                (lambda (val) 
                  (,next-label))
                ,@(map (lambda (arg) (compile-argument arg context))
                       args)))]
         [($ a60:assign vars val)
          ;; >>>>>>>>>>>>>>> Start clean-up here <<<<<<<<<<<<<<<<<
          ;; Lift out the spec-finding part, and use it to generate
          ;; an expected type that is passed to `compile-expression':
          `(begin
             (let ([val ,(compile-expression val context 'numbool)])
               ,@(map (lambda (avar)
                        (let ([var (a60:variable-name avar)])
                          (at var
                              (cond
                                [(null? (a60:variable-indices avar))
                                 (cond
                                   [(call-by-name-variable? var context)
                                    => (lambda (spec)
                                         `(set-target! ,var ',var (coerce ',(spec-coerce-target spec) val)))]
                                   [(procedure-result-variable? var context)
                                    `(set! ,(procedure-result-variable-name var context) 
                                           (coerce ',(spec-coerce-target (procedure-result-spec var context)) val))]
                                   [(or (settable-variable? var context)
                                        (array-element? var context))
                                    => (lambda (spec)
                                         `(,(if (own-variable? var context) 'set-box! 'set!)
                                           ,var
                                           (coerce ',(spec-coerce-target spec) val)))]
                                   [else (raise-syntax-error #f "confused by assignment" (expression-location var))])]
                                [else
                                 (let ([spec (or (array-element? var context)
                                                 (call-by-name-variable? var context))])
                                   `(array-set! ,(compile-expression (make-a60:variable var null) context 'numbool)
                                                (coerce ',(spec-coerce-target spec) val)
                                                ,@(map (lambda (e) (compile-expression e context 'num)) 
                                                       (a60:variable-indices avar))))]))))
                      vars))
             (,next-label))]
         [else (error "can't compile statement")]))
     
     (define (compile-expression expr context type)
       (match expr
         [(? (lambda (x) (and (syntax? x) (number? (syntax-e x)))) n) 
	  (if (eq? type 'des)
	      ;; Need a label:
	      (check-label (datum->syntax-object expr
						 (string->symbol (number->string (syntax-e expr)))
						 expr
						 expr)
			   context)
	      ;; Normal use of a number:
	      (begin
		(check-type 'num type expr) 
		(as-builtin n)))]
         [(? (lambda (x) (and (syntax? x) (boolean? (syntax-e x)))) n) (check-type 'bool type expr) (as-builtin n)]
         [(? (lambda (x) (and (syntax? x) (string? (syntax-e x)))) n)  (check-type 'string type expr) (as-builtin n)]
         [(? identifier? i) (compile-expression (make-a60:variable i null) context type)]
         [(? symbol? i) ; either a generated label or 'val:
	  (unless (eq? expr 'val)
	    (check-type 'des type expr))
	  (datum->syntax-object #f i)]
         [($ a60:subscript array index)
          ;; Maybe a switch index, or maybe an array reference
	  (at array
	      (cond
	       [(array-element? array context)
		`(array-ref ,array ,(compile-expression index context 'num))]
	       [(switch-variable? array context)
		`(switch-ref ,array ,(compile-expression index context 'num))]
	       [else (raise-syntax-error
		      #f
		      "confused by variable"
		      array)]))]
         [($ a60:binary t argt op e1 e2)
	  (check-type t type expr)
          (at op
              `(,(as-builtin op) ,(compile-expression e1 context argt) ,(compile-expression e2 context argt)))]
         [($ a60:unary t argt op e1)
	  (check-type t type expr)
          (at op
              `(,(as-builtin op) ,(compile-expression e1 context argt)))]
         [($ a60:variable var subscripts)
          (let ([sub (lambda (wrap v)
                       (wrap
                        (if (null? subscripts)
                            v
                            `(array-ref ,v ,@(map (lambda (e) (compile-expression e context 'num)) subscripts)))))])
            (cond
              [(call-by-name-variable? var context)
               => (lambda (spec)
                    (check-spec-type spec type var)
                    (sub (lambda (val) `(coerce ',(spec-coerce-target spec) ,val)) `(get-value ,var)))]
	      [(primitive-variable? var context)
	       => (lambda (name)
		    (sub values
                         (datum->syntax-object
			  (current-compile-context)
			  name
			  var
			  var)))]
              [(and (procedure-result-variable? var context)
                    (not (eq? type 'func)))
               (unless (null? subscripts)
                 (raise-syntax-error "confused by subscripts" var))
               (let ([spec (procedure-result-spec var context)])
                 (check-spec-type spec type var)
                 (at var
                     `(coerce
                       ',(spec-coerce-target spec)
                       ,(procedure-result-variable-name var context))))]
              [(or (procedure-result-variable? var context)
                   (procedure-variable? var context)
                   (label-variable? var context)
                   (settable-variable? var context)
                   (array-element? var context))
               => (lambda (spec)
                    (let ([spec (if (or (procedure-result-variable? var context)
                                        (procedure-variable? var context)
                                        (and (array-element? var context)
                                             (null? subscripts)))
                                    #f ;; need just the proc or array...
                                    spec)])
                      (check-spec-type spec type var)
                      (let ([target (spec-coerce-target spec)])
                        (sub (if target
                                 (lambda (v) `(coerce ',target ,v))
                                 values)
                             (if (own-variable? var context)
                                 `(unbox ,var)
                                 var)))))]
              [else (raise-syntax-error
                     #f
                     "confused by expression"
                     (expression-location var))]))]
                   
         [($ a60:app func args)
          (at (expression-location func)
              `(,(compile-expression func context 'func)
                values
                ,@(map (lambda (e) (compile-argument e context))
                       args)))]
         [($ a60:if test then else)
          `(if (check-boolean ,(compile-expression test context 'bool))
	       ,(compile-expression then context type)
	       ,(compile-expression else context type))]
         [else (error 'compile-expression "can't compile expression ~a" expr)]))
     
     (define (expression-location expr)
       (if (syntax? expr)
           expr
           (match expr
             [($ a60:subscript array index) (expression-location array)]
             [($ a60:binary type argtype op e1 e2) op]
             [($ a60:unary type argtype op e1) op]
             [($ a60:variable var subscripts) (expression-location var)]
             [($ a60:app func args)
              (expression-location func)]
             [else #f])))
     
     (define (compile-argument arg context)
       (cond
         [(and (a60:variable? arg) 
               (not (let ([v  (a60:variable-name arg)])
		      (or (procedure-variable? v context)
			  (label-variable? v context)
			  (primitive-variable? v context)))))
          `(case-lambda
             [() ,(compile-expression arg context 'any)]
             [(val)  ,(compile-statement (make-a60:assign (list arg) 'val) 'void context)])]
         [(identifier? arg)
          (compile-argument (make-a60:variable arg null) context)]
         [else `(lambda () ,(compile-expression arg context 'any))]))

     (define (check-type got expected expr)
       (or (eq? expected 'any)
	   (case got
	     [(num) (memq expected '(num numbool))]
	     [(bool) (memq expected '(bool numbool))]
	     [(des) (memq expected '(des))]
	     [(func) (memq expected '(func))]
	     [else #f])
	   (raise-syntax-error #f
			       (format "type mismatch (~a != ~a)" got expected)
			       expr)))
     
     (define (check-spec-type spec type expr)
       (let ([target (spec-coerce-target spec)])
         (when target
           (case (syntax-e target)
             [(integer real) (check-type 'num type expr)]
             [(boolean) (check-type 'bool type expr)]
             [(procedure) (check-type 'func type expr)]))))
       

     (define (check-label l context)
       (if (or (symbol? l)
	       (label-variable? l context))
	   l
	   (raise-syntax-error
	    #f
	    "undefined label"
	    l)))
     
     (define (at stx expr)
       (if (syntax? stx)
           (datum->syntax-object (current-compile-context) expr stx)
           expr))

     (define (as-builtin stx)
       ;; Preserve source loc, but change to reference to
       ;; a builtin operation by changing the context:
       (datum->syntax-object
	(current-compile-context)
	(syntax-e stx)
	stx
	stx))

     ;; --------------------
     
     (define (empty-context)
       `(((sign prim sign)
	  (entier prim entier)

	  (sin prim a60:sin)
	  (cos prim a60:cos)
	  (acrtan prim a60:arctan)
	  (sqrt prim a60:sqrt)
	  (abs prim a60:abs)
	  (ln prim a60:ln)
	  (exp prim a60:exp)

	  (prints prim prints)
	  (printn prim printn)
	  (printsln prim printsln)
	  (printnln prim printnln))))
     
     (define (add-labels context l)
       (cons (map (lambda (lbl) (cons (if (symbol? lbl)
                                          (datum->syntax-object #f lbl)
                                          lbl)
                                      'label)) l)
             context))
     
     (define (add-procedure context var result-type arg-vars by-value-vars arg-specs)
       (cons (list (cons var 'procedure))
             context))
     
     (define (add-settable-procedure context var result-type result-var)
       (cons (list (cons var `(settable-procedure ,result-var ,result-type)))
             context))
     
     (define (add-atoms context ids type)
       (cons (map (lambda (id) (cons id type)) ids)
             context))

     (define (add-arrays context names dimensionses type)
       (cons (map (lambda (name dimensions)
                    (cons name `(array ,type ,(length dimensions))))
                  names dimensionses)
             context))

     (define (add-switch context name)
       (cons (list (cons name 'switch))
             context))
     
     (define (add-bindings context arg-vars by-value-vars arg-specs)
       (cons (map (lambda (var)
                    (let ([spec (or (ormap (lambda (spec)
                                             (and (ormap (lambda (x) (bound-identifier=? var x))
                                                         (cdr spec))
                                                  (car spec)))
                                           arg-specs)
                                    #'unknown)])
                    (cons var
                          (if (ormap (lambda (x) (bound-identifier=? var x)) by-value-vars)
                              spec
                              (list 'by-name spec)))))
                  arg-vars)
             context))
     
     ;; var-binding : syntax context -> symbol
     ;; returns an identifier indicating where the var is
     ;; bound, or 'free if it isn't. The compiler inserts
     ;; top-level procedure definitions into the namespace; if 
     ;; the variable is bound there, it is a procedure.
     (define (var-binding var context)
       (cond
         [(null? context)
          (let/ec k
            (namespace-variable-value (syntax-e var)
                                      #t 
                                      (lambda () (k 'free)))
            'procedure)]
         [else
          (let ([m (var-in-rib var (car context))])
            (or m (var-binding var (cdr context))))]))
     
     (define (var-in-rib var rib)
       (ormap (lambda (b)
                (if (symbol? (car b))
                    ;; primitives:
                    (and (eq? (syntax-e var) (car b))
                         (cdr b))
                    ;; everything else:
                    (and (bound-identifier=? var (car b))
                         (cdr b))))
              rib))
     
     (define (primitive-variable? var context)
       (let ([v (var-binding var context)])
	 (and (pair? v)
	      (eq? (car v) 'prim)
	      (cadr v))))

     (define (call-by-name-variable? var context)
       (let ([v (var-binding var context)])
         (and (pair? v)
              (eq? (car v) 'by-name)
              (cadr v))))
     
     (define (procedure-variable? var context)
       (let ([v (var-binding var context)])
         (eq? v 'procedure)))

     (define (procedure-result-variable? var context)
       (let ([v (var-binding var context)])
         (and (pair? v)
              (eq? (car v) 'settable-procedure)
              (cdr v))))
     
     (define (procedure-result-variable-name var context)
       (let ([v (procedure-result-variable? var context)])
         (car v)))

     (define (procedure-result-spec var context)
       (let ([v (procedure-result-variable? var context)])
         (cadr v)))

     (define (label-variable? var context)
       (let ([v (var-binding var context)])
         (eq? v 'label)))
         
     (define (switch-variable? var context)
       (let ([v (var-binding var context)])
         (eq? v 'switch)))
         
     (define (settable-variable? var context)
       (let ([v (var-binding var context)])
         (or (box? v)
             (and (syntax? v)
                  (memq (syntax-e v) '(integer real boolean))
                  v))))
     
     (define (own-variable? var context)
       (let ([v (var-binding var context)])
         (box? v)))

     (define (array-element? var context)
       (let ([v (var-binding var context)])
         (and (pair? v)
              (eq? (car v) 'array)
              (or (cadr v)
                  #'unknown))))
     
     (define (spec-coerce-target spec)
       (cond
         [(and (syntax? spec) (memq (syntax-e spec) '(string label switch real integer boolean unknown))) spec]
         [(and (syntax? spec) (memq (syntax-e spec) '(unknown))) #f]
         [(or (not spec) (not (pair? spec))) #f]
         [(eq? (car spec) 'array) (cadr spec)]
         [(eq? (car spec) 'procedure) #'procedure]
         [else #f]))
     
     (define (stx-number? a) (and (syntax? a) (number? (syntax-e a)))))
