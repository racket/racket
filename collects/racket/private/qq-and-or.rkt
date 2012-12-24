
;;----------------------------------------------------------------------
;; quasiquote, and, or

(module qq-and-or '#%kernel
  (#%require (for-syntax "stx.rkt" '#%kernel))
  
  (define-syntaxes (let let* letrec)
    (let-values ([(lambda-stx) (quote-syntax lambda-stx)]
                 [(letrec-values-stx) (quote-syntax letrec-values)])
      (let-values ([(go)
                    (lambda (stx named? star? target)
                      (define-values (stx-cadr) (lambda (x) (stx-car (stx-cdr x))))
                      (define-values (id-in-list?)
                        (lambda (id l)
                          (if (null? l)
                              #f
                              (if (bound-identifier=? id (car l)) 
                                  #t
                                  (id-in-list? id (cdr l))))))
                      (define-values (stx-2list?)
                        (lambda (x)
                          (if (stx-pair? x)
                              (if (stx-pair? (stx-cdr x))
                                  (stx-null? (stx-cdr (stx-cdr x)))
                                  #f)
                              #f)))
                      (let-values ([(maybe-msg)
                                    (if (not (stx-list? stx))
                                        ""
                                        (let-values ([(tail1) (stx-cdr stx)])
                                          (if (stx-null? tail1)
                                              (if named?
                                                  "(missing name or binding pairs)"
                                                  "(missing binding pairs)")
                                              (if (stx-null? (stx-cdr tail1))
                                                  (if named?
                                                      "(missing binding pairs or body)"
                                                      "(missing body)")
                                                  (if named?
                                                      (if (symbol? (syntax-e (stx-car tail1)))
                                                          (if (stx-null? (stx-cdr (stx-cdr tail1)))
                                                              "(missing body)"
                                                              #f)
                                                          #f)
                                                      #f)))))])
                        (if maybe-msg
                            (raise-syntax-error #f (string-append "bad syntax " maybe-msg) stx)
                            (void)))
                      (let-values ([(name) (if named?
                                               (let-values ([(n) (stx-cadr stx)])
                                                 (if (symbol? (syntax-e n))
                                                     n
                                                     #f))
                                               #f)])
                        (let-values ([(bindings) (stx->list (stx-cadr (if name
                                                                          (stx-cdr stx)
                                                                          stx)))]
                                     [(body) (stx-cdr (stx-cdr (if name
                                                                   (stx-cdr stx)
                                                                   stx)))])
                          (if (not bindings)
                              (raise-syntax-error 
                               #f 
                               "bad syntax (not a sequence of identifier--expression bindings)" 
                               stx
                               (stx-cadr stx))
                              (let-values ([(new-bindings)
                                            (letrec-values ([(loop)
                                                             (lambda (l)
                                                               (if (null? l)
                                                                   null
                                                                   (let-values ([(binding) (car l)])
                                                                     (cons
                                                                      (if (stx-2list? binding)
                                                                          (if (symbol? (syntax-e (stx-car binding)))
                                                                              (if name
                                                                                  (cons (stx-car binding)
                                                                                        (stx-cadr binding))
                                                                                  (datum->syntax
                                                                                   lambda-stx
                                                                                   (cons (cons (stx-car binding)
                                                                                               null)
                                                                                         (stx-cdr binding))
                                                                                   binding))
                                                                              (raise-syntax-error 
                                                                               #f 
                                                                               "bad syntax (not an identifier)" 
                                                                               stx
                                                                               (stx-car binding)))
                                                                          (raise-syntax-error 
                                                                           #f 
                                                                           "bad syntax (not an identifier and expression for a binding)" 
                                                                           stx
                                                                           binding))
                                                                      (loop (cdr l))))))])
                                              (loop bindings))])
                                (if star?
                                    (void)
                                    (if ((length new-bindings) . > . 5)
                                        (let-values ([(ht) (make-hasheq)])
                                          (letrec-values ([(check) (lambda (l)
                                                                     (if (null? l)
                                                                         (void)
                                                                         (let*-values ([(id) (if name
                                                                                                 (caar l)
                                                                                                 (stx-car (stx-car (car l))))]
                                                                                       [(idl) (hash-ref ht (syntax-e id) null)])
                                                                           (if (id-in-list? id idl)
                                                                               (raise-syntax-error
                                                                                #f
                                                                                "duplicate identifier"
                                                                                stx
                                                                                id)
                                                                               (begin
                                                                                 (hash-set! ht (syntax-e id) (cons id idl))
                                                                                 (check (cdr l)))))))])
                                            (check new-bindings)))
                                        (letrec-values ([(check) (lambda (l accum)
                                                                   (if (null? l)
                                                                       (void)
                                                                       (let-values ([(id) (if name
                                                                                              (caar l)
                                                                                              (stx-car (stx-car (car l))))])
                                                                         (if (id-in-list? id accum)
                                                                             (raise-syntax-error
                                                                              #f
                                                                              "duplicate identifier"
                                                                              stx
                                                                              id)
                                                                             (check (cdr l) (cons id accum))))))])
                                          (check new-bindings null))))
                                (datum->syntax
                                 lambda-stx
                                 (if name
                                     (apply list
                                            (list 
                                             (quote-syntax letrec-values)
                                             (list
                                              (list
                                               (list name)
                                               (list* (quote-syntax lambda)
                                                      (apply list (map car new-bindings))
                                                      body)))
                                             name)
                                            (map cdr new-bindings))
                                     (list* target
                                            new-bindings
                                            body))
                                 stx))))))])
        (values
         (lambda (stx) (go stx #t #f (quote-syntax let-values)))
         (lambda (stx) (go stx #f #t (quote-syntax let*-values)))
         (lambda (stx) (go stx #f #f (quote-syntax letrec-values)))))))

  (define-values (qq-append)
    (lambda (a b)
      (if (list? a)
	  (append a b)
	  (raise-argument-error 'unquote-splicing "list?" a))))

  (define-syntaxes (quasiquote)
    (let-values ([(here) (quote-syntax here)] ; id with module bindings, but not lexical
                 [(unquote-stx) (quote-syntax unquote)]
                 [(unquote-splicing-stx) (quote-syntax unquote-splicing)])
      (lambda (in-form)
	(if (identifier? in-form)
	    (raise-syntax-error #f "bad syntax" in-form)
            (void))
	(let-values
	    (((form) (if (stx-pair? (stx-cdr in-form))
			 (if (stx-null? (stx-cdr (stx-cdr in-form)))
			     (stx-car (stx-cdr in-form))
			     (raise-syntax-error #f "bad syntax" in-form))
			 (raise-syntax-error #f "bad syntax" in-form)))
	     ((normal)
	      (lambda (x old)
		(if (eq? x old)
		    (if (stx-null? x) 
			(quote-syntax ())
			(list (quote-syntax quote) x))
		    x)))
	     ((apply-cons)
	      (lambda (a d)
		(if (stx-null? d)
		    (list (quote-syntax list) a)
		    (if (if (pair? d)
			    (if (free-identifier=? (quote-syntax list) (car d))
				#t
				(free-identifier=? (quote-syntax list*) (car d)))
			    #f)
			(list* (car d) a (cdr d))
			(list (quote-syntax list*) a d))))))
	  (datum->syntax
	   here
	   (normal
	    (letrec-values
		(((qq)
		  (lambda (x level)
		    (let-values
			(((qq-list)
			  (lambda (x level)
			    (let-values
				(((old-first) (stx-car x)))
			      (let-values
				  (((old-second) (stx-cdr x)))
				(let-values
				    (((first) (qq old-first level)))
				  (let-values
				      (((second) (qq old-second level)))
				    (let-values
					()
				      (if (if (eq? first old-first)
					      (eq? second old-second)
					      #f)
					  x
					  (apply-cons
					   (normal first old-first)
					   (normal second old-second)))))))))))
		      (if (stx-pair? x)
			  (let-values
			      (((first) (stx-car x)))
			    (if (if (if (identifier? first)
					(free-identifier=? first unquote-stx)
					#f)
				    (stx-list? x)
				    #f)
				(let-values
				    (((rest) (stx-cdr x)))
				  (if (let-values
					  (((g35) (not (stx-pair? rest))))
					(if g35 g35 (not (stx-null? (stx-cdr rest)))))
				      (raise-syntax-error
				       'unquote
				       "expects exactly one expression"
				       in-form
				       x)
                                      (void))
				  (if (zero? level)
				      (stx-car rest)
				      (qq-list x (sub1 level))))
				(if (if (if (identifier? first)
					    (free-identifier=? first (quote-syntax quasiquote))
					    #f)
					(stx-list? x)
					#f)
				    (qq-list x (add1 level))
				    (if (if (if (identifier? first)
						(free-identifier=? first unquote-splicing-stx)
						#f)
					    (stx-list? x)
					    #f)
					(raise-syntax-error
					 'unquote-splicing
					 "invalid context within quasiquote"
					 in-form
					 x)
					(if (if (stx-pair? first)
						(if (identifier? (stx-car first))
						    (if (free-identifier=? (stx-car first)
                                                                           unquote-splicing-stx)
							(stx-list? first)
							#F)
						    #f)
						#f)
					    (let-values
						(((rest) (stx-cdr first)))
					      (if (let-values
						      (((g34) (not (stx-pair? rest))))
						    (if g34
							g34
							(not (stx-null? (stx-cdr rest)))))
						  (raise-syntax-error
						   'unquote
						   "expects exactly one expression"
						   in-form
						   x)
                                                  (void))
					      (let-values
						  (((uqsd) (stx-car rest))
						   ((old-l) (stx-cdr x))
						   ((l) (qq (stx-cdr x) level)))
						(if (zero? level)
						    (let-values
							(((l) (normal l old-l)))
                                                      (if (stx-null? l)
                                                          uqsd
                                                          (list (quote-syntax qq-append)
                                                                uqsd l)))
						    (let-values
							(((restx) (qq-list rest (sub1 level))))
						      (let-values
							  ()
							(if (if (eq? l old-l)
								(eq? restx rest)
								#f)
							    x
							    (apply-cons
							     (apply-cons
							      (quote-syntax (quote unquote-splicing))
							      (normal restx rest))
							     (normal l old-l))))))))
					    (qq-list x level))))))
			  (if (if (syntax? x) 
				  (vector? (syntax-e x))
				  #f)
			      (let-values
				  (((l) (vector->list (syntax-e x))))
                                ;; special case: disallow #(unquote <e>)
                                (if (stx-pair? l)
                                    (let-values ([(first) (stx-car l)])
                                      (if (identifier? first)
                                          (if (free-identifier=? first unquote-stx)
                                              (raise-syntax-error
                                               'unquote
                                               "invalid context within quasiquote"
                                               in-form
                                               first)
                                              (void))
                                          (void)))
                                    (void))
				(let-values
				    (((l2) (qq l level)))
                                  (if (eq? l l2)
                                      x
                                      (list (quote-syntax list->vector) l2))))
			      (if (if (syntax? x) (box? (syntax-e x)) #f)
				  (let-values
				      (((v) (unbox (syntax-e x))))
				    (let-values
					(((qv) (qq v level)))
				      (if (eq? v qv)
                                          x
                                          (list (quote-syntax box) qv))))
                                  (if (if (syntax? x) 
                                          (if (struct? (syntax-e x)) 
                                              (prefab-struct-key (syntax-e x))
                                              #f)
                                          #f)
                                      ;; pre-fab struct
                                      (let-values
                                          (((l) (cdr (vector->list (struct->vector (syntax-e x))))))
                                        (let-values
                                            (((l2) (qq l level)))
                                          (if (eq? l l2)
                                              x
                                              (list (quote-syntax apply)
                                                    (quote-syntax make-prefab-struct)
                                                    (list (quote-syntax quote)
                                                          (prefab-struct-key (syntax-e x)))
                                                    l2))))
                                      ;; hash[eq[v]]
                                      (if (if (syntax? x)
                                              (hash? (syntax-e x))
                                              #f)
                                          (letrec-values
                                              (((qq-hash-assocs)
						(lambda (x level)
						  (if (null? x)
						      x
						      (let-values
						          (((pair) (car x)))
                                                        (let-values ([(val)
                                                                      (qq (datum->syntax here (cdr pair)) level)]
                                                                     [(rest)
                                                                      (qq-hash-assocs (cdr x) level)])
                                                          (if (if (eq? val (cdr pair))
                                                                  (eq? rest (cdr x))
                                                                  #f)
                                                              x
                                                              (apply-cons
                                                               (list (quote-syntax list*)
                                                                     (list (quote-syntax quote)
                                                                           (datum->syntax here (car pair)))
                                                                     (if (eq? val (cdr pair))
                                                                         (list (quote-syntax quote)
                                                                               val)
                                                                         val))
                                                               (if (eq? rest (cdr x))
                                                                   (list (quote-syntax quote)
                                                                         rest)
                                                                   rest)))))))))
                                            (let-values (((l0) (hash-map (syntax-e x) cons)))
                                              (let-values
                                                  (((l) (qq-hash-assocs l0 level)))
                                                (if (eq? l0 l)
                                                    x
                                                    (list (if (hash-eq? (syntax-e x))
                                                              (quote-syntax make-immutable-hasheq)
                                                              (if (hash-eqv? (syntax-e x))
                                                                  (quote-syntax make-immutable-hasheqv)
                                                                  (quote-syntax make-immutable-hash)))
                                                          l)))))
                                          x)))))))))
	      (qq form 0))
	    form)
	   in-form)))))

  (define-syntaxes (and)
    (let-values ([(here) (quote-syntax here)])
      (lambda (x)
	(if (not (stx-list? x))
	    (raise-syntax-error #f "bad syntax" x)
            (void))
	(let-values ([(e) (stx-cdr x)])
	  (if (stx-null? e)
	      (quote-syntax #t)
	      (if (if (stx-pair? e)
		      (stx-null? (stx-cdr e))
		      #t)
                  (datum->syntax
                   here
                   (list (quote-syntax #%expression)
                         (stx-car e))
                   x)
		  (datum->syntax
		   here
		   (list (quote-syntax if)
			 (stx-car e)
			 (cons (quote-syntax and)
			       (stx-cdr e))
			 (quote-syntax #f))
		   x)))))))

  (define-syntaxes (or)
    (let-values ([(here) (quote-syntax here)])
      (lambda (x)
	(if (identifier? x)
	    (raise-syntax-error #f "bad syntax" x)
            (void))
	(let-values ([(e) (stx-cdr x)])
	  (if (stx-null? e) 
	      (quote-syntax #f)
	      (if (if (stx-pair? e)
		      (stx-null? (stx-cdr e))
		      #f)
                  (datum->syntax
                   here
                   (list (quote-syntax #%expression)
                         (stx-car e))
                   x)
		  (if (stx-list? e)
		      (let-values ([(tmp) 'or-part])
			(datum->syntax
			 here
			 (list (quote-syntax let) (list
						   (list
						    tmp
						    (stx-car e)))
			       (list (quote-syntax if)
				     tmp
				     tmp
				     (cons (quote-syntax or)
					   (stx-cdr e))))
			 x))
		      (raise-syntax-error 
		       #f
		       "bad syntax"
		       x))))))))

  (#%provide let let* letrec
             quasiquote and or))
