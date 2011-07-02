#lang scheme/base

  (require (for-syntax
             syntax/stx
             scheme/base
             syntax/kerncase
             syntax/define
             syntax/context
             syntax/name
             syntax/parse
             scheme/pretty
             "ops.rkt"
             "util.rkt"
             "contexts.rkt"
             ))

  (provide (all-defined-out))

  (begin-for-syntax
   ;; these definitions are used as stop-lists in local-expand
   (define kernel-forms (kernel-form-identifier-list))
   (define prop-expand-stop-forms (list* #'honu-typed
                                         #'honu-unparsed-block
                                         kernel-forms))
   (define block-expand-stop-forms prop-expand-stop-forms)
   (define prototype-expand-stop-forms (list
                                        #'honu-prototype))
   (define type-name-expand-stop-forms (list
                                        #'honu-type-name))


   ;; --------------------------------------------------------
   ;; Transformer procedure property and basic struct

   (define-values (prop:honu-transformer honu-transformer? honu-transformer-ref)
     (make-struct-type-property 'honu-transformer))


   (define-values (struct:honu-trans make-honu-trans honu-trans? honu-trans-ref honu-trans-set!)
     (make-struct-type 'honu-trans #f 1 0 #f 
		       (list (list prop:honu-transformer #t))
		       (current-inspector) 0))

   (define (make-honu-transformer proc)
     (unless (and (procedure? proc)
		  (procedure-arity-includes? proc 2))
       (raise-type-error
	'define-honu-syntax
	"procedure (arity 2)"
	proc))
     (make-honu-trans proc))

   ;; --------------------------------------------------------
   ;; Type

   (define-values (struct:honu-type make-h-type honu-type? honu-type-ref honu-type-set!)
     (make-struct-type 'honu-type #f 4 0 #f null (current-inspector) 0))

   (define (honu-type-stx v) (honu-type-ref v 0))
   (define (honu-type-name-stx v) (honu-type-ref v 1))
   (define (honu-type-pred-stx v) (honu-type-ref v 2))
   (define (honu-type-protect-stx v) (honu-type-ref v 3))

   ;; convert a honu type into a list with nice formatting
   ;; todo: need example
   (define (format-type t)
    (if (identifier? t)
        (syntax-e t)
        (syntax-case t (-> forall)
          [(-> (res . _) (arg . __) ...)
           (append (map format-type (syntax->list #'(arg ...)))
                   (list '-> (format-type #'res)))]
          [(forall (id ...) rhs bindings)
           (append (map syntax-e (syntax->list #'(id ...)))
                   (list '>-> (format-type #'rhs)))]
          [_else `(??? ,(syntax->datum t))])))

   ;; --------------------------------------------------------
   ;; Parsing blocks

   ;; #t if the syntax object contains an operator
   (define operator? 
     (let ([sym-chars (string->list "+-_=?:<>.!%^&*/~|")])
       (lambda (stx)
	 (and (identifier? stx)
	      (let ([str (symbol->string (syntax-e stx))])
		(and (positive? (string-length str))
		     (memq (string-ref str 0) sym-chars)))))))

   ;; #t if the identifier is not an operator nor a delimiter
   (define (honu-identifier? stx)
     (and (identifier? stx)
	  (not (ormap (lambda (i) (delim-identifier=? stx i)) (list #'\; #'\,)))
	  (not (operator? stx))))

   (define (get-transformer stx)
     ;; if its an identifier and bound to a transformer return it
     (define (bound-transformer stx)
       (and (stx-pair? stx)
            (identifier? (stx-car stx))
            (let ([v (syntax-local-value (stx-car stx) (lambda () #f))])
              (and (honu-transformer? v) v))))
     (define (special-transformer stx) 
       (and (stx-pair? stx)
            (let ([first (stx-car stx)])
              (cond
                [(and (stx-pair? first)
                      (identifier? (stx-car first))
                      (delim-identifier=? #'#%parens (stx-car first)))
                 ;; If the stx-car is a list with just one operator symbol,
                 ;;  try using the operator as a transformer
                 (let ([l (cdr (stx->list first))])
                   (let loop ([l l])
                     (cond
                       [(null? l) #f]
                       [(operator? (car l))
                        (if (ormap operator? (cdr l))
                          #f
                          (let ([v (syntax-local-value (car l) (lambda () #f))])
                            (and (honu-transformer? v)
                                 v)))]
                       [else (loop (cdr l))])))]
                [(and (stx-pair? first)
                      (identifier? (stx-car first))
                      (free-identifier=? #'#%angles (stx-car first)))
                 (let ([v (syntax-local-value (stx-car first) (lambda () #f))])
                   (and (honu-transformer? v) v))]
                [else #f]))))
     ;; (printf "~a bound transformer? ~a\n" stx (bound-transformer stx))
     (or (bound-transformer stx)
         (special-transformer stx)))
   
   ;; --------------------------------------------------------
   ;; Parsing blocks

   (define parse-an-expr #f)
   (define parse-a-tail-expr #f)

   (define (parse-block-one ctx body k done-k)
     (cond
      [(stx-null? body) (done-k)]
      [(get-transformer body)
       => (lambda (transformer)
	    (let-values ([(code rest) (transformer body ctx)])
	      (k code rest)))]
      [else (let-values ([(expr-stxs after-expr terminator)
                          (extract-until body (list #'\;))])
	      (unless expr-stxs
		(raise-syntax-error
		 #f
		 "expected a semicolon to terminate form"
		 (stx-car body)))
	      (when (null? expr-stxs)
		(raise-syntax-error
		 #f
		 "missing expression before terminator"
		 terminator))
	      (let ([code ((if (block-context-return? ctx)
			       parse-a-tail-expr
			       parse-an-expr)
			   expr-stxs)])
		(k #`(#%expression #,((if (top-block-context? ctx) 
                                          (lambda (x) 
                                            `(show-top-result ,x))
                                          values)
                                      code))
		   (stx-cdr after-expr))))]))
   
   (define (parse-block stx ctx)
     (let loop ([stx stx])
       (parse-block-one ctx
			stx 
			(lambda (code rest)
			  (cons code (loop rest)))
			(lambda ()
			  null))))

   (define (expression-result ctx expr rest)
     (if (top-block-context? ctx)
         (values #`(#%expression (show-top-result #,expr)) rest)
         (values #`(#%expression #,expr) rest)))


   (define (finish-parsing-expression what where expr rest ctx)
     (if (or (expression-context? ctx)
             (type-or-expression-context? ctx))
         (values expr rest)
         ;; Since we're parsing an expression in a
         ;; declaration context, we're responsible for
         ;; getting the whole expression:
         (let ([placeholder (datum->syntax #f (gensym))])
           (let-values ([(expr-stxs after-expr terminator) (extract-until (cons placeholder rest) (list #'\;))])
             (unless expr-stxs
               (raise-syntax-error
                #f
                (format "expected a semicolon to terminate form after ~a" what)
                where))
             (let* ([total-expr (let loop ([in-expr (parse-an-expr expr-stxs)])
                                  (cond
                                   [(eq? in-expr placeholder) expr]
                                   [(syntax? in-expr)
                                    (datum->syntax in-expr
                                                   (loop (syntax-e in-expr))
                                                   in-expr
                                                   in-expr
                                                   in-expr)]
                                   [(pair? in-expr) (cons (loop (car in-expr))
                                                          (loop (cdr in-expr)))]
                                   [else in-expr]))])
               (expression-result ctx
                                  total-expr
                                  (stx-cdr after-expr)))))))

   ;; --------------------------------------------------------
   ;; Parsing expressions

   (define parse-expr
     ;; The given syntax sequence must not be empty
     (let ()
       (define (parse-expr-seq stx)
         (define (start-expr stx) 
           (let ([trans (get-transformer stx)])
             (if trans
               (let-values ([(expr rest) (trans stx the-expression-context)])
                 (if (stx-null? rest)
                   (list expr)
                   (cons expr (start-operator rest))))
               (syntax-case* stx (#%parens #%braces #%angles) delim-identifier=?
                             [(v)
                              (or (number? (syntax-e #'v))
                                  (identifier? #'v)
                                  (string? (syntax-e #'v)))
                              (if (operator? #'v)
                                (raise-syntax-error
                                  #f
                                  "operator alone is not an expression and cannot start an expression"
                                  #'v)
                                (list #'v))]
                             [((#%parens . pexpr))
                              ;; parens as an expression
                              (if (stx-null? #'pexpr)
                                (raise-syntax-error
                                  #f
                                  "missing expression inside parentheses as expression"
                                  (stx-car stx))
                                (list (parse-expr #'pexpr)))]
                             [((#%parens . pexpr) expr . more)
                              (get-transformer #'pexpr)
                              ;; Expand pexpr in an expression-or-type context, and make a cast 
                              ;; if it's a type.
                              (let ([trans (get-transformer #'pexpr)])
                                (let-values ([(expr-or-type rest) (trans #'pexpr the-type-or-expression-context)])
                                  (if (honu-type? expr-or-type)
                                    ;; parens as a unary prefix operator
                                    (cons (make-cast-prefix (stx-car (stx-car stx)) expr-or-type)
                                          (start-expr #'(expr . more)))
                                    ;; must have been an expression
                                    (cons expr-or-type
                                          (start-operator #'(expr . more))))))]
                             [((#%braces . pexpr))
                              (if (stx-null? #'pexpr)
                                (raise-syntax-error
                                  #f
                                  "missing expression inside braces as expression"
                                  (stx-car stx))
                                (list #'(honu-unparsed-block #f obj 'obj #f #f . pexpr)))]
                             [(op . more)
                              (and (identifier? #'op)
                                   (memq (syntax-e #'op) unary-prefix-ops))
                              (cons (make-prefix (stx-car stx)) (start-expr #'more))]
                             [(expr then . more)
                              (append (start-expr (list #'expr))
                                      (start-operator #'(then . more)))]
                             [(bad . rest)
                              (raise-syntax-error
                                'expression
                                "unknown expression form"
                                #'bad)]))))
         (define (start-operator stx)
           (unless (or (and (stx-pair? (stx-car stx))
                            (let ([id (stx-car (stx-car stx))])
                              (or (delim-identifier=? #'#%brackets id)
                                  (delim-identifier=? #'#%parens id)
                                  (delim-identifier=? #'#%angles id))))
                       (and (identifier? (stx-car stx))
                            (hash-ref op-table
                                      (syntax-e (stx-car stx))
                                      (lambda () #f))))
             (raise-syntax-error
               'expression
               "expected an operator, but found something else"
               (stx-car stx)))
           ;; Check for postfix operator, first (or parens as a
           ;;  an "infix" operator)
           (cond
             [(stx-pair? (stx-car stx))
              ;; Convert vector index or application to a binary operator:
              (let ([opl (let ([id (stx-car (stx-car stx))])
                           ;; Note that we don't check for whether #%brackets, etc. is
                           ;;  bound as a transformer, which means that you can't
                           ;;  change the parsing of [], (), or <> as an "infix" operator.
                           (cond
                             [(delim-identifier=? #'#%brackets id)
                              (let ([index-expr (parse-expr (stx-cdr (stx-car stx)))])
                                (list (make-infix id)
                                      index-expr))]
                             [(delim-identifier=? #'#%parens id)
                              (let ([arg-exprs (parse-arg-list (stx-cdr (stx-car stx)))])
                                (list (make-infix id)
                                      arg-exprs))]
                             [(delim-identifier=? #'#%angles id)
                              (list (make-infix id)
                                    ;; These are normally type expressions, so
                                    ;;  leave parsing to honu-type-ap:
                                    (stx-cdr (stx-car stx)))]
                             [else (error "internal error parsing expr")]))])
                (if (stx-null? (stx-cdr stx))
                  opl
                  (append opl (start-operator (stx-cdr stx)))))]
             [(memq (syntax-e (stx-car stx)) unary-postfix-ops)
              (if (stx-null? (stx-cdr stx))
                (list (make-postfix (stx-car stx)))
                (cons (make-postfix (stx-car stx))
                      (start-operator (stx-cdr stx))))]
             [else
               ;; Otherwise, must be infix
               (cons (make-infix (stx-car stx))
                     (start-expr (stx-cdr stx)))]))
         (start-expr stx))

       (define (parse-expr stx)
         (let group ([seq (parse-expr-seq stx)])
           ;; seq is a list that mixes exprs with ops.
           ;; Find leftmost oper with maximal precedence
           (if (null? (cdr seq))
             (car seq)
             (let loop ([seq seq][before null][op #f][since null])
               (cond
                 [(null? seq)
                  (cond
                    [(cast-prefix? op)
                     (let ([after (reverse since)])
                       (group (append (reverse before)
                                      (list (quasisyntax/loc (op-id op)
                                                             (op-cast #,(op-id op) 
                                                                      #,(let ([t (cast-prefix-type op)])
                                                                          (list (honu-type-stx t)
                                                                                (honu-type-name-stx t)
                                                                                (honu-type-pred-stx t)
                                                                                (honu-type-protect-stx t)))
                                                                      #,(car after))))
                                      (cdr after))))]
                    [(prefix? op)
                     (let ([after (reverse since)])
                       (group (append (reverse before)
                                      (list (quasisyntax/loc (op-id op)
                                                             (op-app #,(op-id op) #%prefix #,(car after))))
                                      (cdr after))))]
                    [(postfix? op)
                     (let ([after (reverse since)]
                           [before (reverse before)])
                       (group (append (cdr before)
                                      (list (quasisyntax/loc (op-id op)
                                                             (op-app #,(op-id op) #%postfix #,(car before))))
                                      after)))]
                    [(infix? op)
                     (let ([after (reverse since)])
                       (group (append (reverse (cdr before))
                                      (list (quasisyntax/loc (op-id op)
                                                             (op-app #,(op-id op) #,(car before) #,(car after))))
                                      (cdr after))))]
                    [else (error 'parse-expr "not an op!: ~s ~s ~s" op before since)])]
                 [(not (op? (stx-car seq)))
                  (loop (cdr seq) before op (cons (car seq) since))]
                 [((if (prefix? op) >= >)
                   (hash-ref precedence-table (prec-key (car seq)) (lambda () 0))
                   (hash-ref precedence-table (prec-key op) (lambda () 0)))
                  (loop (cdr seq) 
                        (if op
                          (append since (list op) before)
                          since)
                        (car seq) null)]
                 [else
                   (loop (cdr seq) before op (cons (car seq) since))])))))

       (define (parse-arg-list stxs)
         (if (stx-null? stxs)
           stxs
           (let-values ([(val-stxs after-expr terminator) (extract-until stxs (list #'\,))])
             (when (and val-stxs
                        (stx-null? (stx-cdr after-expr)))
               (raise-syntax-error
                 'procedure\ call
                 "missing expression after comma"
                 (stx-car after-expr)))
             (when (null? val-stxs)
               (raise-syntax-error
                 'procedure\ call
                 "missing expression before token"
                 (stx-car after-expr)))
             (if val-stxs
               (cons (parse-expr val-stxs)
                     (parse-arg-list (stx-cdr after-expr)))
               (list (parse-expr stxs))))))

       parse-expr))

   (define (parse-tail-expr expr-stxs)
     (syntax-case expr-stxs (honu-return)
       [(honu-return expr ...)
	(let ([exprs #'(expr ...)])
	  (when (stx-null? exprs)
	    (raise-syntax-error 
	     #f
	     "missing expression"
	     (stx-car expr-stxs)))
	  (parse-expr exprs))]
       [_else
	(parse-expr expr-stxs)]))

   (set! parse-an-expr parse-expr)
   (set! parse-a-tail-expr parse-tail-expr)

   ;; --------------------------------------------------------
   ;; Parsing declarations (which always start with a type)

   (define (parse-one-argument proc-id type id k)
     (cons (list id 
		 (honu-type-stx type)
		 (honu-type-name-stx type)
		 (honu-type-pred-stx type)
		 (honu-type-protect-stx type))
	   (k)))

   (define (parse-arguments orig-args-stx proc-id)
     (if (stx-null? orig-args-stx)
	 null
	 (let loop ([args-stx orig-args-stx]
		    [where "at start of argument sequence"]
		    [where-stx orig-args-stx])
	   (let-values ([(type rest-stx) (if (syntax-case* args-stx (\,) delim-identifier=?
					       [(id \, . rest)
						(honu-identifier? #'id)
						#t]
					       [(id)
						(honu-identifier? #'id)
						#t]
					       [_else #f])
					     (values (make-h-type #'obj #''obj #f #f)
						     args-stx)
					     (let ([trans (get-transformer args-stx)])
					       (if trans
						   (trans args-stx the-type-context)
						   (values #f #f))))])
	     (unless (honu-type? type)
	       (raise-syntax-error
		'|procedure declaration|
		(format "expected an identifier or type ~a, found something else" where)
		where-stx))
	     (syntax-case rest-stx ()
	       [(id)
		(honu-identifier? #'id)
		(parse-one-argument proc-id type #'id
				    (lambda () null))]
	       [(id comma . rest)
		(and (honu-identifier? #'id)
		     (identifier? #'comma)
		     (delim-identifier=? #'comma #'\,))
		(parse-one-argument proc-id type #'id
				    (lambda ()
				      (loop #'rest
					    "after comma"
					    #'comma)))]
	       [(id something . rest)
		(honu-identifier? #'id)
		(raise-syntax-error
		 'procedure\ declaration
		 "expected a comma after argument identifier, found something else"
		 #'something)]
	       [_else
		(raise-syntax-error
		 'procedure\ declaration
		 "expected an argument identifier, found something else"
		 (car rest-stx))])))))

   (define (parse-types main-stx orig-args-stx)
     (if (stx-null? orig-args-stx)
	 null
	 (let loop ([args-stx orig-args-stx]
		    [where "at start of type sequence"]
		    [where-stx orig-args-stx])
	   (let-values ([(type rest-stx) (let ([trans (get-transformer args-stx)])
                                           (if trans
                                               (trans args-stx the-type-context)
                                               (values #f #f)))])
	     (unless (honu-type? type)
	       (raise-syntax-error
		'|type application|
		(format "expected a type ~a, found something else" where)
                main-stx
		where-stx))
	     (syntax-case rest-stx ()
	       [()
                (list type)]
	       [(comma . rest)
                (cons type (loop #'rest
                                 "after comma"
                                 #'comma))]
	       [(something . rest)
		(raise-syntax-error
		 'procedure\ declaration
		 "expected a comma after type, found something else"
                 main-stx
		 #'something)])))))

   (define (make-honu-type pred-id get-type-name type-name-expr mk-pred-def)
     (make-honu-trans
      (lambda (orig-stx ctx)
	(let-values ([(type-name type-name-expr pred-id protect-id) 
                      (if mk-pred-def
                          (mk-pred-def orig-stx)
                          (let ([name (get-type-name orig-stx)])
                            (values name 
                                    (or type-name-expr #`'#,name)
                                    pred-id
                                    #f)))])
	  (cond
	   [(let ([is-expr?
                   (lambda ()
                     (syntax-case orig-stx (function)
                       [(function . __) #t]
                       [(_ function . __) #t]
                       [_else #f]))])
              (and (or (block-context? ctx)
                       (definition-context? ctx)
                       (prototype-context? ctx)
                       (and (or (expression-context? ctx)
                                (type-or-expression-context? ctx))
                            (is-expr?)))
                   (or (not (expression-block-context? ctx))
                       (is-expr?))))
	    (with-syntax ([pred-id pred-id]
                          [protect-id protect-id]
			  [type-name type-name]
			  [type-name-expr type-name-expr])
	      (let loop ([stx (if (and (not (definition-context? ctx))
                                       (not (prototype-context? ctx))
                                       (syntax-case orig-stx (function)
                                         [(function . __) #t]
                                         [_else #f]))
                                  orig-stx
                                  (stx-cdr orig-stx))]
			 [after (stx-car orig-stx)]
			 [after-what "type name"])
		(syntax-case stx ()
		  [(id . rest)
		   (begin
		     (unless (honu-identifier? #'id)
		       (raise-syntax-error 'declaration
					   (format "expected a identifier after ~a" after-what)
					   (stx-car orig-stx)
					   #'id))
		     (if (and (or (value-definition-context? ctx)
                                  (not (free-identifier=? #'id #'function)))
                              (not (function-definition-context? ctx))
                              (not (prototype-context? ctx))
                              (identifier? (stx-car #'rest))
			      (free-identifier=? #'set! (stx-car #'rest)))
			 ;; -- Non-procedure declaration
			 (if (function-definition-context? ctx)
			     (raise-syntax-error 
			      'declaration
			      "expected parentheses after name for function definition"
			      (stx-car #'rest))
			     (let-values ([(val-stxs after-expr terminator) (extract-until (stx-cdr #'rest)
											   (list #'\; #'\,))])
			       (unless val-stxs
				 (raise-syntax-error 
				  'declaration
				  "missing semicolon or comma after initializing assignment"
				  (stx-car #'rest)))
			       (when (null? val-stxs)
				 (raise-syntax-error 
				  'declaration
				  "missing expression initializing assignment"
				  (stx-car #'rest)))
			       (let ([def #`(define-typed id 
					      #,(constant-definition-context? ctx)
					      #f type-name type-name-expr pred-id protect-id
					      (check-expr-type #f 'id type-name type-name-expr pred-id
                                                               (let ([id (honu-unparsed-expr #,@val-stxs)])
                                                                 id)))])
				 (if (delim-identifier=? #'\; (stx-car after-expr))
				     (values #`(begin #,def) (stx-cdr after-expr))
				     (let-values ([(defs remainder kind) (loop (stx-cdr after-expr) (stx-car after-expr) "comma" #f)])
				       (values #`(begin #,def #,defs) remainder))))))
			 ;; -- Procedure declaration
			 (if (value-definition-context? ctx)
			     (raise-syntax-error 
			      'declaration
			      (format "expected = after name in ~a context" (context->name ctx))
			      (stx-car #'rest))
			     (syntax-case* #'rest (#%parens \;) delim-identifier=?
			       [((#%parens . prest) (#%braces . body) . rest)
				(let ([args (parse-arguments #'prest #'id)])
				  (with-syntax ([((arg arg-type arg-type-name arg-pred-id arg-protect-id) ...) args]
						[(temp-id ...) (generate-temporaries (map car args))]
                                                [def-id (if (and (not (definition-context? ctx))
                                                                 (free-identifier=? #'id #'function))
                                                            (or (syntax-local-infer-name #'id)
                                                                (car (generate-temporaries '(function))))
                                                            #'id)])
                                    (if (prototype-context? ctx)
                                        ;; Just generate the prototype (as needed for polymorphic functions,
                                        ;;  for example)
                                        #`(honu-prototype (type-name protect-id)
                                                          (arg-type arg-pred-id arg-type-name) ...)
                                        ;; Generate a function declaration
                                        (let ([decl
                                               #`(begin
                                                   (define-typed-procedure def-id
                                                     (type-name type-name-expr protect-id)
                                                     ((arg arg-type arg-type-name arg-pred-id) ...)
                                                     (lambda (temp-id ...)
                                                       (define-typed arg #f id arg-type arg-type-name arg-pred-id arg-protect-id temp-id) ...
                                                       (honu-unparsed-block def-id type-name type-name-expr pred-id #t . body))))])
                                          (if (and (not (definition-context? ctx))
                                                   (free-identifier=? #'id #'function))
                                              ;; Anonymous function:
                                              ;;  We may have to continue parsing...
                                              (finish-parsing-expression "anonymous function"
                                                                         #'id
                                                                         #`(let () #,decl def-id) #'rest ctx)
                                              ;; Function definition:
                                              (values decl #'rest))))))]
			       ;; --- Error handling ---
			       [((#%parens . prest) . bad-rest)
				(begin
				  (parse-arguments #'prest #'id)
				  (raise-syntax-error 
				   '|procedure declaration|
				   "braces for function body after parenthesized arguments"
				   (stx-car #'rest)
				   #'id))]
			       [(id . _)
				(raise-syntax-error 
				 '|declaration|
				 (cond
				  [(constant-definition-context? ctx) "expected = (for constant initialization)"]
				  [(variable-definition-context? ctx) "expected = (for variable initialization)"]
				  [(function-definition-context? ctx) "expected parens (for function arguments)"]
				  [(or (expression-context? ctx)
                                       (type-or-expression-context? ctx)
                                       (expression-block-context? ctx))
                                   "expected parens (for function arguments)"]
				  [else
				   "expected either = (for variable intialization) or parens (for function arguments)"])
				 #'id)]))))]
		  [_else
		   (raise-syntax-error #f 
				       (format "expected a identifier after ~a" after-what)
				       after
				       #'id)])))]
	   [(or (type-context? ctx) 
                (type-or-expression-context? ctx))
	    (values (make-h-type type-name type-name-expr pred-id protect-id) (stx-cdr orig-stx))]
	   [else
	    (raise-syntax-error #f 
				(format "illegal in ~a context" (context->name ctx))
				(stx-car orig-stx))])))))

   (define (make-proc-predicate form)
     ;; `Form' starts with a operator-transformer sequence
     (let-values ([(args-stx ->-stx result-stx) 
		   (let loop ([stx (stx-cdr (stx-car form))][args null])
		     (if (and (identifier? (stx-car stx))
			      (free-identifier=? #'-> (stx-car stx)))
			 (values (reverse args) (stx-car stx) (stx-cdr stx))
			 (loop (stx-cdr stx) (cons (stx-car stx) args))))])
       (when (stx-null? result-stx)
	 (raise-syntax-error
	  #f
	  "missing type for result"
	  ->-stx))
       (let ([arg-types
	      (let loop ([args-stx args-stx])
		(if (stx-null? args-stx)
		    null
		    (let ([trans (get-transformer args-stx)])
		      (unless trans
			(raise-syntax-error #f
                                            "non-type within a procedure-type construction"
                                            ->-stx
                                            (stx-car args-stx)))
		      (let-values ([(type rest-stx) (trans args-stx the-type-context)])
			(cons type (loop rest-stx))))))]
	     [result-type 
	      (let ([trans (get-transformer result-stx)])
		(unless trans
		  (raise-syntax-error #f
                                      "non-type in result position for procedure-type construction"
                                      ->-stx
                                      (stx-car result-stx)))
		(let-values ([(type rest-stx) (trans result-stx the-type-context)])
		  (unless (stx-null? rest-stx)
		    (raise-syntax-error #f
                                        "extra tokens following result for procedure-type construction"
                                        ->-stx
                                        (stx-car rest-stx)))
		  type))])
	 (with-syntax ([(arg ...) (generate-temporaries arg-types)]
		       [(arg-type ...) (map honu-type-stx arg-types)]
		       [(arg-type-name ...) (map honu-type-name-stx arg-types)]
		       [(arg-pred-id ...) (map honu-type-pred-stx arg-types)]
		       [(arg-protect-id ...) (map honu-type-protect-stx arg-types)]
		       [result-type (honu-type-stx result-type)]
		       [result-type-name (honu-type-name-stx result-type)]
		       [result-pred-id (honu-type-pred-stx result-type)]
		       [result-protect-id (honu-type-protect-stx result-type)]
		       [n (length arg-types)])
           (values
            #'(-> (result-type result-protect-id)
                  (arg-type arg-type-name arg-pred-id) ...)
            #'`(,arg-type-name ... -> ,result-type-name)
            #`(lambda (v)
                (if (and (procedure? v)
                         (procedure-arity-includes? v n))
                    (values #t (lambda (arg ...)
                                 (check-expr-type
                                  #f #t result-type result-type-name result-pred-id
                                  (v (honu-typed arg #f arg-type arg-protect-id) ...))))
                    (values #f #f)))
            #`(lambda (v)
                (lambda (arg ...)
                  (honu-typed (v (check-expr-type #f #f arg-type arg-type-name arg-pred-id arg)
                                 ...)
                              #f
                              result-type
                              result-protect-id))))))))
  
   (define (make-poly-predicate form)
     ;; `Form' starts with an operator-transformer sequence
     (let-values ([(args-stx >->-stx result-stx) 
		   (let loop ([stx (stx-cdr (stx-car form))][args null])
		     (if (and (identifier? (stx-car stx))
			      (free-identifier=? #'>-> (stx-car stx)))
			 (values (reverse args) (stx-car stx) (stx-cdr stx))
			 (loop (stx-cdr stx) (cons (stx-car stx) args))))])
       (when (stx-null? result-stx)
	 (raise-syntax-error
	  #f
	  "missing right-hand-side type template"
	  >->-stx))
       (for-each (lambda (arg)
                   (unless (identifier? arg)
                     (raise-syntax-error
                      #f
                      "expected an identifier for a generic-type formal argument"
                      >->-stx
                      arg)))
                 args-stx)
       (with-syntax ([(arg-id ...) args-stx]
                     [(arg-pred-id ...) (generate-temporaries args-stx)]
                     [(arg-name-id ...) (generate-temporaries args-stx)]
                     [n (add1 (* 2 (length args-stx)))])
         ;; To get the right type name, we have to parse result-stx:
         (let-values ([(type-name result-type-name)
                       (let ([ex (local-expand #`(let ()
                                                   (define-syntax arg-id (make-honu-type #'values stx-car #f #f)) ...
                                                   (honu-type-name arg-id ...)
                                                   (honu-unparsed-type-name #'>->-stx . #,result-stx))
                                               'expression
                                               type-name-expand-stop-forms)])
                         (syntax-case ex (honu-type-name)
                           [(let () (l-s+v b1 b2 
                                           (honu-type-name bound-arg-id ...)
                                           (honu-type-name result-type result-type-name)))
                            (values #`(forall (bound-arg-id ...) result-type (arg-pred-id ... arg-name-id ...))
                                    #'result-type-name)]))])
           (values type-name
                   #``(arg-id ... >-> ,#,result-type-name)
                   #`(lambda (v)
                       (if (and (generic? v)
                                (procedure-arity-includes? (generic-val v) n))
                           ;; So far, so good. Check the rest lazily.
                           (values #t (make-generic
                                       (lambda (safe? arg-pred-id ... arg-name-id ...)
                                         (define-syntax arg-id (make-honu-type #'arg-pred-id stx-car #'arg-name-id #f)) ...
                                         (honu-unparsed-type-predicate #,>->-stx next-pred res-type-name . #,result-stx)
                                         (let ([v ((generic-val v) safe? arg-pred-id ... arg-name-id ...)])
                                           (check* #f #f res-type-name next-pred v)))))
                           ;; Not a generic
                           (values #f #f)))
                   ;; generics always protect themselves, for now:
                   #'(lambda (x) x))))))

   (define (poly-subs t orig-ids binding-ids new-types new-preds new-protects new-type-names)
     (syntax-case t (-> poly)
       [id
        (identifier? t)
        (or (and (identifier? t)
                 (ormap (lambda (orig new)
                          (and (free-identifier=? t orig)
                               new))
                        orig-ids new-types))
            t)]
       [(-> (res res-protect) (arg arg-pred arg-type-name) ...)
        (let ([cvt (lambda (p) (poly-subs p orig-ids binding-ids new-types new-preds new-protects new-type-names))]
              [wrap (lambda (expr) (and binding-ids
                                        (and (syntax-e expr)
                                             #`((lambda #,binding-ids #,expr)
                                                #,@new-preds
                                                #,@new-type-names))))])
          (with-syntax ([res (cvt #'res)]
                        [res-protect (wrap #'res-protect)]
                        [(arg ...) (map cvt (syntax->list #'(arg ...)))]
                        [(arg-pred ...) (map wrap (syntax->list #'(arg-pred ...)))]
                        [(arg-type-name ...) (map wrap (syntax->list #'(arg-type-name ...)))])
            #'(-> (res res-protect) (arg arg-pred arg-type-name) ...)))]
       [else t]))

   (define (apparent-type val-expr)
     (syntax-case val-expr (#%datum)
       [(#%datum . val-expr) (apparent-type #'val-expr)]
       [_else
        (cond
         [(and (integer? (syntax-e val-expr))
               (exact? (syntax-e val-expr))) #'int]
         [(real? (syntax-e val-expr)) #'real]
         [(number? (syntax-e val-expr)) #'num]
         [(string? (syntax-e val-expr)) #'string]
         [(boolean? (syntax-e val-expr)) #'bool]
         [(identifier? val-expr)
          (cond
           [(free-identifier=? #'false val-expr) #'bool]
           [(free-identifier=? #'true val-expr) #'bool]
           [else #'obj])]
         [else #'obj])]))
       
   (define (check-compatible-type val-expr orig-val-expr val-type target-type fail-k)
     ;; Check whether target-type subsumes val-type, and returns #t if so.
     ;; If val-type subsumes target-type, the result is #f.
     ;; If the two types are incompatible, `fail-k' is called.
     (syntax-case target-type (-> forall)
       [ttid
        (identifier? target-type)
        (or (free-identifier=? #'obj target-type)
            (and (identifier? val-type)
                 (free-identifier=? val-type target-type))
            (let ([val-type
                   (if (not val-type)
                       (apparent-type val-expr)
                       val-type)])
              (or (and (identifier? val-type)
                       (or (free-identifier=? val-type target-type)
                           (and (free-identifier=? #'num target-type)
                                (or (free-identifier=? val-type #'int)
                                    (free-identifier=? val-type #'real)))
                           (and (free-identifier=? #'real target-type)
                                (or (free-identifier=? val-type #'int)))))
                  (if (and (identifier? val-type)
                           (free-identifier=? val-type #'obj))
                      #f
                      (fail-k orig-val-expr val-type target-type)))))]
       [(-> (t-result-type t-result-protect-id) (t-arg-type t-arg-type-name t-arg-pred) ...)
        (let* ([val-type (or val-type (apparent-type val-expr))]
               [do-fail (lambda ()
                          (fail-k orig-val-expr val-type target-type))])
          (syntax-case val-type (->)
            [(-> (v-result-type v-result-protect-id) (v-arg-type v-arg-type-name v-arg-pred) ...)
             (let ([t-args (syntax->list #'(t-arg-type ...))]
                   [v-args (syntax->list #'(v-arg-type ...))])
               (and (or (= (length t-args) (length v-args))
                        (do-fail))
                    (check-compatible-type val-expr orig-val-expr
                                           #'v-result-type #'t-result-type
                                           (lambda (a b c) (do-fail)))
                    (andmap (lambda (t-arg v-arg)
                              (check-compatible-type val-expr orig-val-expr
                                                     t-arg v-arg
                                                     (lambda (a b c)
                                                       (do-fail))))
                            t-args v-args)))]
            [_else
             (if (free-identifier=? val-type #'obj)
                 #f
                 (do-fail))]))]
       [(forall (poly-id ...) poly-t bindings)
        (let ([val-type (or val-type (apparent-type val-expr))]
              [do-fail (lambda ()
                         (fail-k orig-val-expr val-type target-type))])
          (syntax-case val-type (forall)
            [(forall (v-poly-id ...) v-poly-t v-bindings)
             (let ([poly-ids (syntax->list #'(poly-id ...))]
                   [v-poly-ids (syntax->list #'(v-poly-id ...))])
               (if (= (length poly-ids) (length v-poly-ids))
                   (let ([new-ids (generate-temporaries poly-ids)])
                     (check-compatible-type
                      val-expr orig-val-expr
                      (poly-subs #'poly-t poly-ids #f new-ids new-ids new-ids #f)
                      (poly-subs #'v-poly-t v-poly-ids #f new-ids new-ids new-ids #f)
                      (lambda (a b c) (do-fail))))
                   (do-fail)))]
            [else
             (if (and (identifier? val-type)
                      (free-identifier=? val-type #'obj))
                 #f
                 (do-fail))]))]
       [_else
        (syntax-case val-type (-> forall)
          [(-> . rest)
           (fail-k orig-val-expr val-type target-type)]
          [(forall . rest)
           (fail-k orig-val-expr val-type target-type)]
          [else #f])]))

   (define (type-mismatch val-expr val-type target-type)
     (raise-syntax-error
      '|static type mismatch|
      (format "type `~s' does not match type `~s'"
	      (format-type val-type)
	      (format-type target-type))
      val-expr))

   (define parse-comma-separated
     (lambda (body terminated? empty-case parse-one combine)
       (syntax-case* body (\;) delim-identifier=?
         [(\;) terminated? (empty-case)]
         [_else
          (let loop ([body body][accum null][prev-comma #f])
            (syntax-case* body (\, \;) delim-identifier=?
              [()
               (not terminated?)
               (values (reverse accum) body)]
              [(\, . rest)
               (let-values ([(one) (parse-one (reverse accum) prev-comma (stx-car body))]
                            [(other rest) (loop #'rest null (stx-car body))])
                 (values (combine one other) rest))]
              [(\; . rest)
               terminated?
               (values (parse-one (reverse accum) prev-comma (stx-car body)) #'rest)]
              [(x . rest)
               (loop #'rest (cons #'x accum) #f)]))]))))

  ;; ----------------------------------------
  ;; end begin-for-syntax

  (define (check proc who type-name pred val)
    (let-values ([(tst new-val) (pred val)])
      (unless tst
        (raise
         (make-exn:fail:contract
          (format "~a: expected `~a' value for ~a, got something else: ~e"
                  (or proc (if (eq? who #t) #f who) "procedure")
                  type-name
                  (cond [(eq? who #t) "result"]
                        [else (if proc
                                  (format "`~a' argument" who)
                                  (if who "initialization" "argument"))])
                  val)
          (current-continuation-marks))))
      new-val))

  (define-syntax as-protected
    (syntax-rules ()
      [(_ expr) 
       ;; No need for `expr' to protect itself:
       (check-expr-type #f #f #t #t #t expr)]))

  (define-syntax as-test
    (syntax-rules ()
      [(_ expr) (as-protected expr)]))

  (define-for-syntax (extract-type v)
    ;; Lifts type checks up so that we can see them immediately:
    (syntax-case v (#%expression if begin quote-syntax honu-type-info)
      [(begin (quote-syntax (honu-type-info orig-expr val-type protect-id)) val)
       (list #'orig-expr #'val-type #'protect-id)]
      [(if t orig-then-expr orig-else-expr)
       (with-syntax ([(orig-then-expr then-type then-protect-id) (extract-type #'orig-then-expr)]
                     [(orig-else-expr else-type else-protect-id) (extract-type #'orig-else-expr)])
         (cond
          [(check-compatible-type #f v #'else-type #'then-type type-mismatch)
           (list v #'then-type #'then-protect-id)]
          [(check-compatible-type #f v #'then-type #'else-type type-mismatch)
           (list v #'else-type #'else-protect-id)]
          [else
           (raise-syntax-error #f
                               "need a least-upper bound?!"
                               v)]))]
      [(lv ([(lhs ...) expr] ...) ... body)
       (ormap (lambda (id)
                (free-identifier=? #'lv id))
              (list #'let-values #'letrec-values #'letrec-syntaxes+values))
       (extract-type #'body)]
      [(begin e ... last-expr)
       (extract-type #'last-expr)]
      [(%expression expr)
       (extract-type #'expr)]
      [_else
       (list v (apparent-type v) #f)]))

  (define-for-syntax (expand-for-type stx)
    (let-values ([(v pack-v) (syntax-local-expand-expression 
                              #`(as-protected #,stx))])
      (list* pack-v v (extract-type v))))

  ;; (define-for-syntax certify (syntax-local-certifier))

  (define-syntax (check-expr-type stx)
    ;; Pushes type checks down to be treated by later expansion:
    (syntax-case stx ()
      [(_ proc who type-name type-name-expr pred val)
       ;; Avoid the check if the static types are consistent
       (let ([v (local-expand
                  #'val
                  'expression
                  prop-expand-stop-forms)])
         ;; FIXME: this is where we run afoul of certificates, because we're
         ;; pulling apart something produced by `local-expand'.
	 (syntax-case v (honu-typed if 
                                    let-values letrec-values letrec-syntaxes+values 
                                    begin #%expression
                                    honu-unparsed-block)
	   [(honu-typed val orig-expr val-type protect-id)
            (if (eq? #t (syntax-e #'type-name))
                ;; Context guarantees correct use, as long as we report our type:
                #'(honu-report-type val orig-expr val-type protect-id)
                ;; Context guarantees use at a particular type...
                (if (check-compatible-type #'val #'orig-expr #'val-type #'type-name type-mismatch)
                    ;; Declared type subsumes actual type:
                    (if (and (syntax-e #'protect-id)
                             (not (check-compatible-type #f #f #'type-name #'val-type (lambda (a b c) #f))))
                        ;; Type subsumes, but still need to protect:
                        v
                        (if (syntax-e #'protect-id)
                            ;; Don't need protect:
                            #'(honu-typed val orig-expr val-type #f)
                            ;; Didn't declare protect anyway:
                            v))
                    ;; Need a run-time check:
                    (with-syntax ([val v])
                      #'(check* proc who type-name-expr pred val))))]
	   [(if test-expr then-expr else-expr)
            (if (eq? #t (syntax-e #'type-name))
                ;; Context guarantees correct use, but we have to manage any
                ;; merge for subsumption.
                (with-syntax ([(pack-t-expr t-expr orig-t-expr t-type t-protect-id)
                               (expand-for-type #'then-expr)]
                              [(pack-e-expr e-expr orig-e-expr e-type e-protect-id)
                               (expand-for-type #'else-expr)])
                  (if (check-compatible-type #'e-expr #'else-expr #'e-type #'t-type type-mismatch)
                      (if (check-compatible-type #'t-expr #'then-expr #'t-type #'e-type type-mismatch)
                          ;; branch types are equivalent
                          #'(honu-typed (if test-expr pack-t-expr pack-e-expr)
                                        val t-type t-protect-id)
                          ;; then subsumes else
                          #'(honu-typed (if test-expr
                                            pack-t-expr
                                            (e-protect-id pack-e-expr))
                                        val t-type t-protect-id))
                      (if (check-compatible-type #'t-expr #'then-expr #'t-type #'e-type type-mismatch)
                          ;; else subsumes then
                          #'(honu-typed (if test-expr
                                            (t-protect-id pack-t-expr)
                                            pack-e-expr)
                                        val e-type e-protect-id)
                          ;; neither subsumes the other, but they are compatible
                          ;; --- we're losing information about the LUB
                          #'(if test-expr
                                (t-protect-id pack-t-expr)
                                (e-protect-id pack-e-expr)))))
                ;; Context guarantees use at a particular type.
                ;; Simply propagate check to braches:
                #'(if test-expr
                      (check-expr-type proc who type-name type-name-expr pred then-expr)
                      (check-expr-type proc who type-name type-name-expr pred else-expr)))]
           [(let-values ([(id) rhs]) body-id)
            ;; recognized when `let' is being used to name an expression
            (and (identifier? #'body-id)
                 (bound-identifier=? #'id #'body-id))
            #'(let-values ([(id) (check-expr-type proc who type-name type-name-expr pred rhs)]) body-id)]
	   [(let-values bindings body0 ... body)
	    #'(let-values bindings 
                  body0 ...
		  (check-expr-type proc who type-name type-name-expr pred body))]
	   [(letrec-values bindings body0 ... body)
	    #'(letrec-values bindings 
                  body0 ...
		  (check-expr-type proc who type-name type-name-expr pred body))]
	   [(letrec-syntaxes+values bindings1 bindings2 body0 ... body)
	    #'(letrec-syntaxes+values bindings1 bindings2
                body0...
                (check-expr-type proc who type-name type-name-expr pred body))]
           [(begin e0 ... e)
            #'(begin e0 ... (check-expr-type proc who type-name type-name-expr pred e))]
	   [(#%expression e)
            #'(#%expression (check-expr-type proc who type-name type-name-expr pred e))]
	   [(honu-unparsed-block #f _ __ #f return-context? . body)
	    #'(honu-unparsed-block who type-name type-name-expr pred return-context? . body)]
	   [_else
            (if (eq? #t (syntax-e #'type-name))
                v
                ;; Even without a type for v, we might see a literal,
                ;;  or maybe the declaration is simply val
                (if (or (check-compatible-type v v #f #'type-name type-mismatch)
                        (not (syntax-e #'pred)))
                    ;; No run-time check:
                    v
                    ;; Run-time check:
                    (with-syntax ([val v])
                      #'(check* proc who type-name-expr pred val))))]))]))

  (define-syntax check*
    (syntax-rules ()
      [(_ proc who type-name #f val) val]
      [(_ proc who type-name pred val)
       (check proc who type-name pred val)]))

  (define-syntax (honu-app stx)
    (syntax-case stx ()
      [(_ a b ...)
       (with-syntax ([(pack-a-expr a-expr orig-a-expr a-type a-protect-id)
                      (expand-for-type #'a)]
                     [orig-expr stx])
         (syntax-case #'a-type (-> obj)
           [(-> (result-type result-protect-id) (arg-type arg-type-name arg-pred) ...)
            (if (= (length (syntax->list #'(arg-type ...)))
                   (length (syntax->list #'(b ...))))
                ;; Some run-time checks maybe needed on some arguments:
                (with-syntax ([app
                               (syntax/loc stx
                                 (pack-a-expr (check-expr-type #f #f arg-type arg-type-name arg-pred b) ...))])
                  (syntax/loc stx
                    (honu-typed app
                                orig-expr
                                result-type result-protect-id)))
                (raise-syntax-error #f
                                    (format (string-append 
                                             "static type mismatch: "
                                             "function called with the wrong number of arguments; "
                                             "expected ~a, given ~a")
                                            (length (syntax->list #'(arg-type ...)))
                                            (length (syntax->list #'(b ...))))
                                    #'orig-expr))]
           [obj
            ;; There will be a run-time check to make sure that a is the
            ;;  right kind of function, etc., and it will take care of the
            ;;  argument checks itself.
            (syntax/loc stx
              (#%app (honu-typed pack-a-expr orig-a-expr a-type a-protect-id) b ...))]
           [_else
            (type-mismatch #'orig-a-expr #'a-type #'(-> (.... #f) (.... #f #f)))]))]))

  (define-syntax (op-app stx)
    (syntax-case stx (#%parens #%angles)
      [(_ #%parens a (b ...))
       #'(honu-app a b ...)]
      [(_ #%angles a (b ...))
       #'(honu-type-app a b ...)]
      [(_ a b ...) 
       (datum->syntax #'a
                      (cons #'a #'(b ...))
                      #'a)]))

  (define-syntax (op-cast stx)
    (syntax-case stx (#%parens)
      [(_ #%parens (type-name type-name-expr pred-id protect-id) b)
       #'(honu-typed (check-expr-type #f #f type-name type-name-expr pred-id b)
                     b
                     type-name
                     #f)]))

  (define-syntax (honu-#%app stx)
    (syntax-case stx ()
      [(_ a b ...) #'(#%expression (honu-app a b ...))]))

  (define-syntax (honu-type-app stx)
    (syntax-case stx ()
      [(_ a b ...)
       (with-syntax ([(pack-a-expr a-expr orig-a-expr a-type a-protect-id)
                      (expand-for-type #'a)])
         (let ([types (parse-types stx #'(b ...))])
           (with-syntax ([ids (generate-temporaries types)])
             (check-compatible-type #'a-expr #'a #'a-type #'(forall ids obj #f)
                                    type-mismatch))
           (with-syntax ([(pred ...) (map honu-type-pred-stx types)]
                         [(name ...) (map honu-type-name-stx types)]
                         [cnt (add1 (* 2 (length types)))])
             (syntax-case #'a-type (forall)
               [(forall (formal-id ...) t bindings)
                (with-syntax ([new-type (poly-subs #'t 
                                                   (syntax->list #'(formal-id ...))
                                                   (syntax->list #'bindings)
                                                   (map honu-type-stx types)
                                                   (map honu-type-pred-stx types)
                                                   (map honu-type-protect-stx types)
                                                   (map honu-type-name-stx types))])
                  #`(honu-typed ((generic-val pack-a-expr) #t pred ... name ...) #,stx new-type #f))]
               [_else #'((extract-polymorphic pack-a-expr cnt) #f pred ... name ...)]))))]))
                    
  (define-syntax (define-typed stx)
    (syntax-case stx ()
      [(_ id const? proc-name type-name type-name-expr pred-id protect-id val)
       (with-syntax ([gen-id (car (generate-temporaries (list #'id)))])
	 #'(begin
	     (define gen-id val)
	     (define-syntax id
	       (make-set!-transformer
             (lambda (stx)
               (syntax-case stx (set!)
                 [(set! id rhs)
                  (if const?
                    (raise-syntax-error #f "cannot assign to constant" #'id)
                    (syntax/loc stx
                                (set! gen-id (check-expr-type 'set! id type-name type-name-expr pred-id rhs))))]
                 [(id arg (... ...))
                  (syntax/loc stx
                              (honu-app (honu-typed gen-id id type-name protect-id) arg (... ...)))]
                 [id
                   (syntax/loc stx
                               (honu-typed gen-id id type-name protect-id))]))))))]))

  (define-for-syntax (make-typed-procedure gen-id result-spec arg-spec protect-id)
    (with-syntax ([((arg arg-type arg-type-name arg-pred-id) ...) arg-spec]
                  ;; FIXME! protect-id is quote-syntax'd and expanding it here
                  ;; runs into trouble due to lexical marks
                  ;; [(result-type result-type-name result-protect-id) result-spec]
                  [(result-type result-type-name result-protect-id) (list #'#f #'#f #'#f)]
                  [gen-id gen-id])
      (with-syntax ([type-name #'(-> (result-type result-protect-id)
                                     (arg-type arg-type-name arg-pred-id) ...)])
        (make-set!-transformer
         (lambda (stx)
           (syntax-case stx (set! honu-safe-use-hack)
             [(set! id rhs)
              (raise-syntax-error #f
                                  "cannot assign to procedure name"
                                  stx
                                  #'id)]
             [(id honu-safe-use-hack)
              #'gen-id]
             [(id actual-arg ...)
              (let ([actual-args (syntax->list #'(actual-arg ...))]
                    [formal-args (syntax->list #'(arg ...))])
                (unless (= (length actual-args)
                           (length formal-args))
                  (raise-syntax-error
                   'id
                   (format "expects ~a arguments, provided ~a"
                           (length formal-args)
                           (length actual-args))
                   stx))
                ;; FIXME!
                #'(#%app gen-id actual-arg ...)
                #;
                #'(honu-typed (#%app gen-id actual-arg ...) id result-type result-protect-id)
                #;
                #'(honu-typed (#%app gen-id
                                     (check-expr-type 'id 'arg arg-type arg-type-name arg-pred-id actual-arg) 
                                     ...)
                              id
                              result-type
                              result-protect-id))]
             [id
              #`(honu-typed gen-id id type-name #,protect-id)]))))))

  ;; FIXME: some of these must be exported due to a bad `local-expand':
  (provide honu-typed check-expr-type honu-app op-app 
           define-typed-procedure define-typed
           honu-unparsed-block
           extract-polymorphic generic-val)

  (define-syntax (define-typed-procedure stx)
    (syntax-case stx ()
      [(_ id result-spec arg-spec val)
       (with-syntax ([gen-id (car (generate-temporaries (list #'id)))]
                     [((arg arg-type arg-type-name arg-pred-id) ...) #'arg-spec]
                     [(result-type result-type-name result-protect-id) #'result-spec])
	 #'(begin
	     (define gen-id val)
             (define-syntax id
               (make-typed-procedure (quote-syntax gen-id) 
                                     (quote-syntax result-spec) 
                                     (quote-syntax arg-spec)
                                     (quote-syntax
                                      (lambda (orig)
                                        (let ([id (lambda (arg ...)
                                                    (honu-typed (orig (check-expr-type 'id 'arg arg-type arg-type-name arg-pred-id arg)
                                                                      ...)
                                                                #f
                                                                result-type
                                                                result-protect-id))])
                                          id)))))))]))

  (define-syntax honu-typed
    (syntax-rules ()
      [(_ expr orig-expr type #f) 
       (honu-report-type expr orig-expr type #f)]
      [(_ expr orig-expr type protect-id) 
       ;; The `protect-id' must be an expression that can be
       ;;  lifted out of any enclosing `let' binding.
       ;; Since nothing stripped the `honu-typed' from `expr',
       ;;  we need to protect the value by applying `protect-id':
       (honu-report-type (protect-id expr) orig-expr type #f)]))

  (define-syntax honu-report-type
    (syntax-rules ()
      [(_ expr orig-expr type protect-id) 
       ;; Preserve information in a particular pattern that survives full
       ;; expansion, but that doesn't create any run-time overhead:
       (#%expression (begin (quote-syntax (honu-type-info orig-expr type protect-id)) 
                            expr))]))
      
  (define-syntax (honu-type-info stx) (raise-syntax-error #f "shouldn't appear unquoted!" stx))

  ;; (require-for-syntax syntax/context)
  (define-syntax (honu-block stx)
    ;; A block can have mixed exprs and defns. Wrap expressions with
    ;; `(define-values () ... (values))' as needed, and add a (void)
    ;; at the end if needed. Also, wrap the final expression with
    ;; a type check as needed.
    (let ([proc-id (stx-car (stx-cdr stx))]
	  [result-type-name (stx-car (stx-cdr (stx-cdr stx)))]
	  [result-type-name-expr (stx-car (stx-cdr (stx-cdr (stx-cdr stx))))]
	  [result-pred-id (stx-car (stx-cdr (stx-cdr (stx-cdr (stx-cdr stx)))))]
	  [exprs 
           (let ([def-ctx (syntax-local-make-definition-context)]
                 [ctx (generate-expand-context)])
             (begin0
              (let loop ([exprs (cddddr (cdr (syntax->list stx)))])
                (apply 
                 append
                 (map (lambda (expr)
                        (let ([expr (local-expand
                                     expr
                                     ctx
                                     block-expand-stop-forms
                                     def-ctx)])
                          (syntax-case expr (begin define-values define-syntaxes)
                            [(begin . rest)
                             (loop (syntax->list #'rest))]
                            [(define-syntaxes (id ...) rhs)
                             (andmap identifier? (syntax->list #'(id ...)))
                             (with-syntax ([rhs (local-transformer-expand
                                                 #'rhs
                                                 'expression
                                                 null)])
                               (syntax-local-bind-syntaxes
                                (syntax->list #'(id ...))
                                #'rhs def-ctx)
                               (list #'(define-syntaxes (id ...) rhs)))]
                            [(define-values (id ...) rhs)
                             (andmap identifier? (syntax->list #'(id ...)))
                             (let ([ids (syntax->list #'(id ...))])
                               (syntax-local-bind-syntaxes ids #f def-ctx)
                               (list expr))]
                            [else
                             (list expr)])))
                      exprs)))
              (internal-definition-context-seal def-ctx)))])
      #`(let ()
	  #,@(let loop ([exprs exprs][prev-defns null][prev-exprs null])
	       (cond
		[(null? exprs) (append 
				(reverse prev-defns)
				(if (pair? prev-exprs) 
				    (reverse (cons
					      #`(check-expr-type '#,proc-id #t
								 #,result-type-name 
								 #,result-type-name-expr 
								 #,result-pred-id 
								 #,(car prev-exprs))
					      (cdr prev-exprs)))
				    (begin
				      (unless (or (not proc-id)
						  (not (syntax-e proc-id))
						  (free-identifier=? #'type-name #'obj))
					(error "no expression for type check; should have been "
					       "caught earlier"))
				      (reverse prev-exprs)))
				(if (null? prev-exprs)
				    (list #'(void))
				    null))]
		[(and (stx-pair? (car exprs))
                      (identifier? (stx-car (car exprs)))
		      (or (free-identifier=? #'define-values (stx-car (car exprs)))
			  (free-identifier=? #'define-syntaxes (stx-car (car exprs)))))
		 (loop (cdr exprs)
		       (cons (car exprs)
			     (append
			      (map (lambda (expr)
				     #`(define-values () (begin #,expr (values))))
				   prev-exprs)
			      prev-defns))
		       null)]
		[else
		 (loop (cdr exprs) prev-defns (cons (car exprs) prev-exprs))])))))

  (define-syntax (honu-unparsed-block stx)
    (syntax-case stx (void)
      [(_ proc-id result-type-name result-type-name-expr result-pred-id return-context? . body) 
       #`(honu-block proc-id result-type-name result-type-name-expr result-pred-id 
                     #,@(parse-block 
                         #'body
                         (if (syntax-e #'return-context?)
                             the-return-block-context
                             the-block-context)))]))

  (define-syntax (honu-unparsed-expr stx)
    (syntax-case stx ()
      [(_ v ...) #`(#%expression #,(parse-expr (syntax->list #'(v ...))))]))

  (define-syntax (h-return stx)
    (syntax-case stx ()
      [(_ expr) #'(#%expression expr)]))

  (define-syntax (#%parens stx)
    (raise-syntax-error #f "misplaced parentheses" stx))

  (define-syntax (#%brackets stx)
    (raise-syntax-error #f "misplaced brackets" stx))

  (define-syntax (#%braces stx)
    (raise-syntax-error #f "misplaced braces" stx))

  (define-syntax (&& stx)
    (syntax-case stx ()
      [(_ a b) #'(and a b)]))

  (define-syntax (!= stx)
    (syntax-case stx ()
      [(_ a b) #'(not (equal? a b))]))

  (define-syntax (\|\| stx)
    (syntax-case stx ()
      [(_ a b) #'(or a b)]))

  ;; --------------------------------------------------------
  ;; Defining a new transformer or new type

  ;; (require-for-syntax syntax/define)
  (define-syntax (define-honu-syntax stx)
    (let-values ([(id rhs) (normalize-definition stx #'lambda #f)])
      (with-syntax ([id id]
		    [rhs rhs])
	#'(define-syntax id (make-honu-transformer rhs)))))

  (define-syntax (define-type stx)
    (syntax-case stx ()
      [(_ id pred-expr)
       (identifier? #'id)
       (with-syntax ([pred-id (car (generate-temporaries '(pred)))])
	 #'(begin
	     (define pred-id (let ([pred pred-expr])
				(lambda (v)
				  (values (pred v) v))))
	     (define-syntax id (make-honu-type ((syntax-local-certifier #t) #'pred-id) stx-car #f #f))))]))

  (define-syntax (define-type-constructor stx)
    (syntax-case stx ()
      [(_ id generator-expr)
       (identifier? #'id)
       #'(define-syntax id (make-honu-type #f #f #f generator-expr))]))

  ;; ----------------------------------------
  ;;  Definition forms

  (define-for-syntax (make-definition-form what this-context this-context?)
    (make-honu-transformer
     (lambda (orig-stx ctx)
       (when (this-context? ctx)
	 (raise-syntax-error #f 
			     (format "redundant in ~a context" (context->name ctx))
			     (stx-car orig-stx)))
       (unless (block-context? ctx)
	 (raise-syntax-error #f 
			     (format "illegal in ~a context" (context->name ctx))
			     (stx-car orig-stx)))
       (let ([body (stx-cdr orig-stx)])
	 (cond
	  [(stx-null? body)
	   (raise-syntax-error #f 
			       (format "expected a ~a definition after keyword" what)
			       (stx-car orig-stx))]
	  [(get-transformer body)
	   => (lambda (transformer)
		(transformer body this-context))]
	  [else
	   (let ([id (stx-car body)])
	     (unless (honu-identifier? id)
	       (raise-syntax-error #f
				   (format "expected an identifier or type for a ~a definition" what)
				   (stx-car orig-stx)
				   id))
	     ((make-honu-type #f (lambda (stx) #'obj) #f #f) orig-stx this-context))])))))

  (define-syntax var (make-definition-form 'variable the-variable-definition-context variable-definition-context?))
  (define-syntax const (make-definition-form 'variable the-constant-definition-context constant-definition-context?))

  (define-syntax function
    (make-honu-type #f (lambda (stx) #'obj) #f #f))

  (define-type-constructor -> make-proc-predicate)
  (define-type-constructor >-> make-poly-predicate)

  (define-for-syntax (honu-expand-type orig-stx type-stx)
    (let-values ([(type rest-stx) 
                  (let ([trans (get-transformer type-stx)])
                    (if trans
                        (trans type-stx the-type-context)
                        (values #f #f)))])
      (unless (honu-type? type)
        (raise-syntax-error
         #f
         "expected a type after arrow, found something else"
         orig-stx
         (stx-car type-stx)))
      (unless (stx-null? rest-stx)
        (raise-syntax-error
         #f
         "expected to end with result type, but found more"
         orig-stx
         (stx-car rest-stx)))
      type))

  
  (define-syntax (honu-unparsed-type-predicate stx)
    (syntax-case stx ()
      [(_ orig-stx next-pred res-type-name . type-stx)
       (let ([type (honu-expand-type #'orig-stx #'type-stx)])
         #`(begin
             (define (next-pred v) (#,(honu-type-pred-stx type) v))
             (define res-type-name #,(honu-type-name-stx type))))]))

  (define-syntax (honu-unparsed-type-name stx)
    (syntax-case stx ()
      [(_ orig-stx . type-stx)
       (let ([type (honu-expand-type #'orig-stx #'type-stx)])
         #`(honu-type-name #,(honu-type-stx type) #,(honu-type-name-stx type)))]))
  
  ;; ----------------------------------------

  (define-syntax (honu-unparsed-function-definition stx)
    (let-values ([(parsed rest)
                  ((make-definition-form '|generic function| the-function-definition-context (lambda (x) #f))
                   stx
                   the-block-context)])
      (unless (stx-null? rest)
        (raise-syntax-error #f "error: function definition didn't consume body" rest))
      parsed))

  (define-syntax (honu-unparsed-prototype stx)
    ((make-definition-form '|generic function| the-prototype-context (lambda (x) #f))
     stx
     the-block-context))

  ;; By defining #%angles as a Honu transfomer, we override any potential
  ;;  treatment as a prefix operator.
  (define-honu-syntax #%angles
    (lambda (stx ctx)
      (unless (or (type-or-expression-context? ctx)
                  (expression-context? ctx)
                  (block-context? ctx))
        (raise-syntax-error #f 
                            (format
                             "generic allowed only in a block or expression context, not in ~a context" 
                             (context->name ctx))
                            stx))
      (syntax-case (stx-car stx) (#%angles)
        [(#%angles . rest)
         (let-values ([(ids empty-rest)
                       ;; Parse inside angle brackets:
                       (parse-comma-separated
                        #'rest
                        #f
                        (lambda ()
                          (raise-syntax-error #f
                                              "expected at least one identifier"
                                              (stx-car stx)))
                        (lambda (stxes prev-comma-stx term-stx)
                          (unless (and (= (length stxes) 1)
                                       (honu-identifier? (car stxes)))
                            ;; Either prev-comma-stx is not #f or stxes is not null
                            ;;  (otherwise we'd hit the empty case, covered above)
                            (cond
                             [prev-comma-stx
                              (raise-syntax-error #f
                                                  "expected a single identifier after comma"
                                                  (stx-car stx)
                                                  prev-comma-stx)]
                             [(raise-syntax-error #f
                                                  "expected a single identifier before comma or closing bracket"
                                                  (stx-car stx)
                                                  (car stxes))]))
                          (car stxes))
                        (lambda (id ids)
                          (cons id ids)))]
                      [(new-id) (car (generate-temporaries '(poly)))])
           ;; Everything up to and including curly braces defines the poly function:
           (let-values ([(id defn rest)
                         (let loop ([rest (stx-cdr stx)][accum null])
                           (syntax-case* rest (#%braces \; \,) delim-identifier=?
                             [((#%braces . body) . new-rest)
                              (begin
                                ;; Maybe found the body. Preceeded by params and identifier?
                                (unless (and (pair? accum)
                                             (stx-pair? (car accum))
                                             (identifier? (stx-car (car accum)))
                                             (delim-identifier=? #'#%parens (stx-car (car accum))))
                                  (raise-syntax-error #f
                                                      "expected a function-argument list before generic function body braces"
                                                      (stx-car stx)
                                                      (if (null? accum)
                                                          (stx-car rest)
                                                          (car accum))))
                                (unless (and (pair? (cdr accum))
                                             (honu-identifier? (cadr accum)))
                                  (raise-syntax-error #f
                                                      "expected an identifier for a function name before generic function argument list"
                                                      (stx-car stx)
                                                      (if (null? (cdr accum))
                                                          (car accum)
                                                          (cadr accum))))
                                (values (cadr accum) (reverse (list* (stx-car rest) 
                                                                     (car accum)
                                                                     new-id
                                                                     (cddr accum))) #'new-rest))]
                             [()
                              (raise-syntax-error #f
                                                  "expected a function body in braces eventually after generic specification"
                                                  (stx-car stx))]
                             [(\; . _)
                              (raise-syntax-error #f
                                                  "expected a function body in braces (eventually), found a semi-colon"
                                                  (stx-car stx)
                                                  (stx-car rest))]
                             [(\, . _)
                              (raise-syntax-error #f
                                                  "expected a function body in braces (eventually), found a comma"
                                                  (stx-car stx)
                                                  (stx-car rest))]
                             [(something . rest)
                              ;; Assume anything else is ok:
                              (loop #'rest (cons #'something accum))]))])
             (unless (block-context? ctx)
               (unless (free-identifier=? id #'function)
                 (raise-syntax-error #f 
                                     (format
                                      "named generic allowed only in a block context, not in ~a context" 
                                      (context->name ctx))
                                     (stx-car stx)
                                     id)))
             (with-syntax ([(poly-id ...) ids]
                           [(poly-pred-id ...) (generate-temporaries ids)]
                           [(poly-name-id ...) (generate-temporaries ids)]
                           [def-id (if (free-identifier=? id #'function)
                                       (or (syntax-local-infer-name id)
                                           (car (generate-temporaries '(function))))
                                       id)]
                           [new-id new-id]
                           [defn defn])
               (with-syntax ([((bound-poly-id ...) (return-type return-protect) (arg-type arg-pred arg-type-name) ...)
                              (let ([ex (local-expand #`(let ([poly-pred-id #f] ... [poly-name-id #f] ...)
                                                          (define-syntax poly-id (make-honu-type #'poly-pred-id stx-car #'poly-name-id #f)) ...
                                                          (honu-prototype poly-id ...)
                                                          (honu-unparsed-prototype . defn))
                                                      'expression
                                                      prototype-expand-stop-forms)])
                                (syntax-case ex (honu-prototype)
                                  [(let b0 (l-s+v b1 b2 
                                                  (honu-prototype bound-poly-id ...)
                                                  (honu-prototype (return-type return-protect) (arg-type arg-pred arg-type-name) ...)))
                                   #'((bound-poly-id ...) (return-type return-protect) (arg-type arg-pred arg-type-name) ...)]
                                  [else (raise-syntax-error
                                         #f
                                         "expansion problem: didn't get expected prototype information"
                                         ex)]))])
                 (with-syntax ([(safe-arg ...) (generate-temporaries #'(arg-type ...))])
                   (let ([decl
                           #'(begin
                               (define-syntax def-id
                                 (make-set!-transformer
                                  (lambda (stx)
                                    (syntax-case stx (set!)
                                      [(set! def-id rhs)
                                       (raise-syntax-error #f
                                                           "cannot assign to generic procedure name"
                                                           stx
                                                           #'def-id)]
                                      [(def-id arg (... ...))
                                       (raise-syntax-error #f
                                                           "cannot apply generic procedure without first applying it to types"
                                                           stx)]
                                      [def-id
                                       #'(honu-typed gen-id def-id
                                                     (forall (bound-poly-id ...)
                                                             (-> (return-type return-protect) (arg-type arg-pred arg-type-name) ...)
                                                             (poly-pred-id ... poly-name-id ...))
                                                     #f)]))))
                               (define gen-id
                                 (make-generic
                                  (lambda (safe? poly-pred-id ... poly-name-id ...)
                                    (define-syntax poly-id (make-honu-type #'poly-pred-id stx-car #'poly-name-id #f)) ...
                                    (honu-unparsed-function-definition . defn)
                                    (if safe?
                                        (new-id honu-safe-use-hack)
                                        new-id)))))])

                    (if (free-identifier=? id #'function)
                        ;; Anonymous function:
                        ;;  We may have to continue parsing...
                        (finish-parsing-expression "anonymous generic function"
                                                   id
                                                   #`(let () #,decl def-id) rest ctx)
                        (values decl rest))))))))])))

  (define-syntax (honu-safe-use-hack stx) (raise-syntax-error #f "shouldn't see this" stx))
  (define-syntax (honu-prototype stx) (raise-syntax-error #f "shouldn't see this" stx))
  (define-syntax (honu-type-name stx) (raise-syntax-error #f "shouldn't see this" stx))

  (define-struct generic (val))
  (define (extract-polymorphic v n)
    (unless (generic? v)
      (raise-type-error '|type application|
                        "generic value"
                        v))
    (let ([p (generic-val v)])
      (unless (procedure-arity-includes? p n)
        (raise-type-error '|type application|
                          (format "generic value (type arity ~a)" n)
                          v))
      p))

  (define-syntax (#%prefix stx) (raise-syntax-error #f "should have been matched by an operator binding" stx))
  (define-syntax (#%postfix stx) (raise-syntax-error #f "should have been matched by an operator binding" stx))

  ;; ----------------------------------------
  ;;  Pre-defined types

  (define-type int exact-integer?)
  (define-type bool boolean?)
  (define-type real real?)
  (define-type num number?)
  (define-type obj (lambda (x) #t))
  (define-type string string?)

  ;; ----------------------------------------
  ;;  Pre-defined forms

  (define-honu-syntax honu-provide
    (lambda (body ctx)
      (unless (top-block-context? ctx)
	(raise-syntax-error #f "not allowed outside the top level" (stx-car body)))
      (parse-comma-separated
       (stx-cdr body)
       #t
       (lambda () #'(begin))
       (lambda (stxes prev-comma-stx term-stx)
	 (syntax-case stxes ()
	   [(id)
	    (honu-identifier? #'id)
	    #`(provide id)]
	   [else
	    (raise-syntax-error
	     #f
	     "unknown provide form"
	     (stx-car body)
	     (car stxes))]))
       (lambda (p decls)
	 #`(begin #,p #,decls)))))

  (define-honu-syntax honu-require
    (lambda (body ctx)
      (define (check-empty rest after-what)
	(unless (stx-null? rest)
	  (raise-syntax-error
	   #f
	   (format "expect a comma or semicolon after ~a" after-what)
	   (stx-car body)
	   (stx-car rest))))
      (unless (top-block-context? ctx)
	(raise-syntax-error #f "not allowed outside the top level" (stx-car body)))
      (parse-comma-separated
       (stx-cdr body)
       #t
       (lambda () #'(begin))
       (lambda (stxes prev-comma-stx term-stx)
	 #`(require 
	    #,(let ()
		(define (parse-module-name stxes)
		  (syntax-case* stxes (lib file #%parens) delim-identifier=?
		    [(fn . rest)
		     (string? (syntax-e #'fn))
		     (begin
		       (check-empty #'rest "path string")
		       #'fn)]
		    [(lib (#%parens names ...) . rest)
		     (let ([names (let loop ([names #'(names ...)])
				    (syntax-case* names (\,) delim-identifier=?
				      [() null]
				      [(name . rest)
				       (begin
					 (unless (string? (syntax-e #'name))
					   (raise-syntax-error
					    #f
					    "expected a string for a library path"
					    (car stxes)
					    #'name))
					 (syntax-case* #'rest (\,) delim-identifier=?
					   [() (list #'name)]
					   [(\, . rest)
					    (cons #'name (loop #'rest))]
					   [else
					    (raise-syntax-error
					     #f
					     "expected a comma"
					     (stx-car stxes)
					     (stx-car #'rest))]))]
				      [(\,)
				       (raise-syntax-error
					#f
					"expected a string before comma"
					(car stxes)
					(stx-car names))]
				      [_else
				       (raise-syntax-error
					#f
					"expected a string for a library path"
					(car stxes)
					(stx-car names))]))])
		       (when (null? names)
			 (raise-syntax-error
			  #f
			  "expected at least one string for the library path"
			  (cadr stxes)))
		       (check-empty #'rest "library path")
		       (syntax-local-introduce #`(lib #,@names)))]
		    [(lib . rest)
		     (raise-syntax-error 
		      #f
		      "expected a parenthesized sequence of strings after `lib' keyword"
		      (car stxes)
		      (stx-car body))]
		    [(file (#%parens name) . rest)
		     (string? (syntax-e #'name))
		     (begin
		       (check-empty #'rest "file name")
		       (syntax-local-introduce #`(file name)))]
		    [(file . rest)
		     (raise-syntax-error 
		      #f
		      "expected a parenthesized string after `file' keyword"
		      (car stxes)
		      (stx-car body))]
		    [(fn)
		     (honu-identifier? #'fn)
		     #'fn]
		    [else
		     (raise-syntax-error
		      #f
		      "unknown require form"
		      (stx-car body)
		      (car stxes))]))
		(define (parse-module-spec stxes)
		  (syntax-case* stxes (rename #%parens \,) delim-identifier=?
		    [(rename (#%parens spec0 spec ... \, local-id \, remote-id) . rest)
		     (begin
		       (unless (honu-identifier? #'local-id)
			 (raise-syntax-error
			  #f
			  "expected an identifier"
			  (stx-car stxes)
			  #'local-id))
		       (unless (honu-identifier? #'remote-id)
			 (raise-syntax-error
			  #f
			  "expected an identifier"
			  (stx-car stxes)
			  #'remote-id))
		       (begin0
			#`(rename #,(parse-module-name 
				     (syntax->list #'(spec0 spec ...))) 
				  local-id 
				  remote-id)
			(check-empty #'rest "rename")))]
		    [(rename . rest)
		     (raise-syntax-error 
		      #f
		      "expected a parenthesized id, id, and require spec `rename' keyword"
		      (car stxes)
		      (stx-car body))]
		    [_else (parse-module-name stxes)]))
		(parse-module-spec stxes))))
       (lambda (p decls)
	 #`(begin #,p #,decls)))))

  (define-honu-syntax honu-return
    (lambda (stx ctx)
      (unless (block-context-return? ctx)
	(raise-syntax-error #f "allowed only in a tail position" (stx-car stx)))
      (let-values ([(val-stxs after-expr terminator) (extract-until (stx-cdr stx)
								    (list #'\;))])
	(unless val-stxs
	  (raise-syntax-error 
	   #f
	   "missing semicolon"
	   (stx-car stx)))
	(when (null? val-stxs)
	  (raise-syntax-error 
	   #f
	   "missing expression"
	   (stx-car stx)))
	(with-syntax ([expr (parse-expr val-stxs)])
	  (unless (or (expression-block-context? ctx)
                      (stx-null? (stx-cdr after-expr)))
	    (raise-syntax-error 
	     #f
	     "not at a block end"
	     (stx-car stx)))
	  (values
	   (syntax/loc (stx-car stx)
	     (h-return expr))
           (stx-cdr after-expr))))))

  (define-honu-syntax honu-if
    (lambda (stx ctx)
      (define (get-block-or-statement kw rest)
	(syntax-case rest (#%braces)
	  [((#%braces then ...) . rrest)
           (values
            #`(honu-unparsed-block #f obj 'obj #f #,(and (block-context-return? ctx)
                                                    (stx-null? rest))
                                   . #,(stx-cdr (stx-car rest)))
            #'rrest)]
	  [else
           (parse-block-one (if (block-context-return? ctx)
                                the-expression-return-block-context
                                the-expression-block-context)
                            rest
                            (lambda (expr rest)
                              (values expr rest))
                            (lambda ()
                              (raise-syntax-error
                               #f
                               "expected a braced block or a statement"
                               kw)))]))

      (unless (block-context? ctx)
        (raise-syntax-error
         #f
         "allowed only in a block context"
         (stx-car stx)))

      (syntax-case* stx (#%parens) delim-identifier=?
	[(_ (#%parens test ...) . rest)
	 (let* ([tests #'(test ...)])
	   (when (stx-null? tests)
	     (raise-syntax-error
	      #f
	      "missing test expression"
	      (stx-car stx)
	      (stx-car (stx-cdr stx))))
	   (let ([test-expr (parse-expr (syntax->list tests))])
	     (let-values ([(then-exprs rest) (get-block-or-statement (stx-car stx) #'rest)])
	       (syntax-case rest (else)
		 [(else . rest2)
		  (let-values ([(else-exprs rest) (get-block-or-statement (stx-car rest) #'rest2)])
		    (expression-result ctx
                                       #`(if (as-test #,test-expr) #,then-exprs #,else-exprs)
                                       rest))]
		 [_else
		  (expression-result ctx #`(if (as-test #,test-expr) #,then-exprs (void)) rest)]))))]
	[_else
	 (raise-syntax-error
	  #f
	  "expected a parenthesized test after `if' keyword"
	  (stx-car stx))])))
  
  (define-honu-syntax honu-time
    (lambda (stx ctx)
      (unless (block-context? ctx)
        (raise-syntax-error
         #f
         "allowed only in a block context"
         (stx-car stx)))
      (let-values ([(val-stxs rest terminator) (extract-until (stx-cdr stx) (list #'\;) #f)])
        (unless val-stxs
          (raise-syntax-error
           #f
           "expected a terminating semicolon"
           (stx-car stx)))
        (when (null? val-stxs)
          (raise-syntax-error
           #f
           "expected an expression before semicolon"
           (stx-car stx)
           (stx-car rest)))
        (let ([time-expr (parse-expr val-stxs)])
          (expression-result ctx
                             #`(time (#%expression #,time-expr))
                             (stx-cdr rest))))))
      
  ;; ----------------------------------------
  ;;  Class form

  (define-honu-syntax honu-class
    (lambda (stx ctx)
      (syntax-case stx (#%braces)
	[(form id . rest)
	 (not (honu-identifier? #'id))
	 (raise-syntax-error
	  #f
	  "expected an identifier for the class"
	  #'form
	  #'id)]
	[(form id (#%braces content ...) . rest)
	 (let ([id #'id])
	   

	   10)]
	[(form)
	 (raise-syntax-error
	  #f
	  "missing name for the class"
	  #'form)]
	[(form id next . _)
	 (raise-syntax-error
	  #f
	  "expected braces after class name, found something else"
	  #'form
	  #'next)]
	[(form id)
	 (raise-syntax-error
	  #f
	  "missing braces after class name"
	  #'form
	  #'id)])))

  ;; ----------------------------------------
  ;; Main compiler loop

   (define (show-top-result v)
     (unless (void? v)
       (printf "~s\n" v)))

   (define-syntax (honu-unparsed-begin stx)
    (syntax-case stx ()
      [(_) #'(begin)]
      [(_ . body) (let-values ([(code rest) (parse-block-one the-top-block-context
							     #'body 
							     values
							     (lambda ()
							       (values #'(void) null)))])
		    #`(begin
			#,code
			(honu-unparsed-begin #,@rest)))]))

  (define-syntax (#%dynamic-honu-module-begin stx)
    ;; (printf "honu raw sexp ~a\n" (syntax->datum stx))
    (let ([result #`(#%plain-module-begin
                     (honu-unparsed-begin #,@(stx-cdr stx)))])
      ;; (pretty-print (syntax->datum (expand result)))
      result))
  
  (define-syntax (\; stx) (raise-syntax-error '\; "out of context" stx))
  
  (define true #t)
  (define false #f)

  (define-syntax define-integer-closed-op
    (syntax-rules ()
      [(_ id scheme-id)
       (define-syntax (id stx)
         (syntax-case stx (#%prefix)
           [(_ #%prefix a) (syntax/loc stx (honu-app (honu-typed scheme-id #f (-> (int #f) (int 'int #f)) #f) a))]
           [(_ a b) (syntax/loc stx (honu-app (honu-typed scheme-id #f (-> (int #f) (int 'int #f) (int 'int #f)) #f) a b))]
           [_ (syntax/loc stx (honu-typed scheme-id #f (-> (int #f) (int 'int #f) (int 'int #f)) #f))]))]))

  (define-integer-closed-op honu- -)
  (define-integer-closed-op honu+ +)
  (define-integer-closed-op honu* *)

  (define-syntax (? stx)
    (syntax-case stx (op-app :)
      [(_ t (opp-app : b1 b2))
       (syntax/loc stx (if t b1 b2))]
      [(? . _)
       (raise-syntax-error #f "misuse of operator (not matched with :)" #'?)]))

  (define-syntax (: stx)
    (raise-syntax-error #f "misuse of operator (not preceded with ?)" stx))

  (define-syntax (honu-end stx)
    (raise-syntax-error #f "ignore this" stx))

  (define-syntax (honu-top stx)
    (raise-syntax-error #f "interactive use is not yet supported"))
