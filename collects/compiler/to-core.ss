(module to-core mzscheme
  (require (lib "kerncase.ss" "syntax")
           (lib "stx.ss" "syntax")
           (lib "list.ss")
           (lib "boundmap.ss" "syntax"))

  (provide top-level-to-core)

  ;; `module', `require', and `require-for-syntax' declarations must
  ;;  not be embedded in a `begin' sequence. For `require' and
  ;;  `require-for-syntax', it's a timing issue. For `module', it's
  ;;  because the transformation can only handle a single `module'
  ;;  declaration.
  (define (top-level-to-core stx lookup-stx set-stx safe-vector-ref-stx extract-stx 
			     simple-constant? stop-properties)
    (syntax-case stx (module begin)
      [(module m lang (plain-module-begin decl ...))
       (let-values ([(expr new-decls magic-sym) 
                     (lift-sequence (flatten-decls (syntax->list #'(decl ...)))
                                    lookup-stx set-stx safe-vector-ref-stx extract-stx
                                    #t
				    simple-constant? stop-properties)])
         (values (expand-syntax expr)
                 #`(module m lang (#%plain-module-begin #,@new-decls))
		 magic-sym))]
      [(begin decl ...)
       (let-values ([(expr new-decls magic-sym) 
                     (lift-sequence (flatten-decls (syntax->list #'(decl ...)))
                                    lookup-stx set-stx safe-vector-ref-stx extract-stx
                                    #f
				    simple-constant? stop-properties)])
         (values (expand-syntax expr)
                 #`(begin #,@new-decls)
		 magic-sym))]
      [else
       (top-level-to-core #`(begin #,stx) lookup-stx set-stx safe-vector-ref-stx extract-stx
			  simple-constant? stop-properties)]))
  
  (define (flatten-decls l)
    (apply append
           (map (lambda (stx)
                  (syntax-case stx (begin)
                    [(begin . e)
                     (flatten-decls (syntax->list #'e))]
                    [else (list stx)]))
                l)))
  
  (define-struct lifted-info (counter id-map slot-map))
  
  (define (make-vars)
    (make-lifted-info 
     0
     (make-module-identifier-mapping)
     (make-hash-table 'equal)))
  
  (define (is-id-ref? v)
    (or (identifier? v)
        (and (stx-pair? v)
             (identifier? (stx-car v))
             (module-identifier=? #'#%top (stx-car v)))))
  
  (define (vars-sequence li)
    (let loop ([i 0])
      (if (= i (lifted-info-counter li))
          null
          (cons (let ([v (hash-table-get (lifted-info-slot-map li) i)])
                  (if (is-id-ref? v)
                      #`(#%variable-reference #,v)
                      v))
                (loop (add1 i))))))
  
  (define (extract-vars li vec-id extract-stx)
    (let loop ([i 0])
      (if (= i (lifted-info-counter li))
          null
          (let ([v (hash-table-get (lifted-info-slot-map li) i)])
            (if (is-id-ref? v)
                (cons #`(#,extract-stx #,vec-id #,i)
                      (loop (add1 i)))
                (loop (add1 i)))))))
  
  (define (is-run-time? stx)
    (not (and (stx-pair? stx)
              (or (module-identifier=? #'define-syntaxes (stx-car stx))
                  (module-identifier=? #'define-values-for-syntax (stx-car stx))))))

  (define (has-symbol? decl magic-sym table)
    (cond
     [(hash-table-get table decl (lambda () #f))
      ;; cycle/graph
      #f]
     [else
      (hash-table-put! table decl #t)
      (cond
       [(eq? magic-sym decl)
	#t]
       [(pair? decl)
	(or (has-symbol? (car decl) magic-sym table)
	    (has-symbol? (cdr decl) magic-sym table))]
       [(vector? decl)
	(has-symbol? (vector->list decl) magic-sym table)]
       [(box? decl)
	(has-symbol? (unbox decl) magic-sym table)]
       [else
	#f])]))

  (define (generate-magic decls)
    (let ([magic-sym (string->symbol (format "magic~a~a" 
					     (current-seconds)
					     (current-milliseconds)))])
      (if (has-symbol? (map syntax-object->datum decls) magic-sym (make-hash-table))
	  (generate-magic decls)
	  magic-sym)))

  (define (need-thunk? rhs)
    (not (and (stx-pair? rhs)
	      (or (module-identifier=? #'lambda (stx-car rhs))
		  (module-identifier=? #'case-lambda (stx-car rhs))))))

  (define (lift-sequence decls lookup-stx set-stx safe-vector-ref-stx extract-stx 
			 in-module? simple-constant? stop-properties)
    (let ([ct-vars (make-vars)]
          [rt-vars (make-vars)]
          [compile-time (datum->syntax-object #f (gensym 'compile-time))]
          [run-time (datum->syntax-object #f (gensym 'run-time))]
	  [magic-sym (generate-magic decls)]
	  [magic-indirect (gensym)])
      (let ([ct-converted 
             (map (lambda (stx)
                    #`(lambda ()
                        #,(syntax-case stx ()
                            [(def ids rhs)
                             (let ([cvted (convert #'rhs #t 
                                                   lookup-stx set-stx safe-vector-ref-stx
                                                   compile-time ct-vars
                                                   in-module?
						   simple-constant? stop-properties)])
                               (if (and (not in-module?)
                                        (module-identifier=? #'def #'define-syntaxes))
                                   ;; Don't try to name macro procedures, because it
                                   ;;  inteferes with the 0-values hack at the top level
                                   cvted
                                   #`(let-values ([ids #,cvted])
                                       (values . ids))))])))
                  (filter (lambda (x) (not (is-run-time? x))) decls))]
            [rt-converted
             (map (lambda (stx)
		    (syntax-case stx (define-values provide require require-for-syntax require-for-template)
		      [(provide . _)
		       #'void]
		      [(require . _)
		       #'void]
		      [(require-for-syntax . _)
		       #'void]
		      [(require-for-template . _)
		       #'void]
		      [(define-values ids rhs)
		       (let ([converted (convert #'rhs #f
						 lookup-stx set-stx safe-vector-ref-stx
						 run-time rt-vars
						 in-module?
						 simple-constant? stop-properties)])
			 (if (need-thunk? #'rhs)
			     #`(lambda () #,converted)
			     #`(let-values ([ids #,converted])
				 (values . ids))))]
		      [else
		       #`(lambda ()
			   #,(convert stx #f 
				      lookup-stx set-stx safe-vector-ref-stx
				      run-time rt-vars
				      in-module?
				      simple-constant? stop-properties))]))
		  (filter is-run-time? decls))]
            [ct-rhs #`((let ([magic (car (cons '#,magic-sym 2))])
                         (if (symbol? magic) 
                             (lambda (x) (vector
					  #,@(map (lambda (stx)
						    (syntax-case stx ()
						      [(def (id) . _)
						       #'void]
						      [(def (id ...) . _)
						       (with-syntax ([(v ...) (map (lambda (x) #f)
										   (syntax->list #'(id ...)))])
							 
							 #`(lambda () (values v ...)))]))
						  (filter (lambda (x) (not (is-run-time? x))) decls))))
                             (car magic)))
                       (vector #,@(vars-sequence ct-vars)))]
            [rt-rhs #`((cdr '#,magic-sym) (vector #,@(vars-sequence rt-vars)))]
            [just-one-ct? (>= 1 (apply +
                                       (map (lambda (decl)
                                              (syntax-case decl (define-syntaxes define-values-for-syntax)
                                                [(define-values-for-syntax . _) 1]
                                                [(define-syntaxes . _) 1]
                                                [_else 0]))
                                            decls)))]
            [just-one-rt? (>= 1 (apply +
                                       (map (lambda (decl)
                                              (syntax-case decl (define-values provide require 
                                                                  require-for-syntax require-for-template
                                                                  define-syntaxes define-values-for-syntax)
                                                [(provide . _) 0]
                                                [(require . _) 0]
                                                [(require-for-syntax . _) 0]
                                                [(define-values-for-syntax . _) 0]
                                                [(define-syntaxes . _) 0]
                                                [_else 1]))
                                            decls)))])
        (values
         #`(cons (lambda (#,compile-time)
                   #,@(extract-vars ct-vars compile-time extract-stx)
                   (vector #,@ct-converted))
                 (lambda (#,run-time)
                   #,@(extract-vars rt-vars run-time extract-stx)
                   (vector #,@rt-converted)))
         #`(;; Lift require and require-for-syntaxes to the front, so they're ready for
	    ;;  variable references
	    #,@(filter (lambda (decl)
			 (syntax-case decl (require require-for-syntax)
			   [(require . _) #t]
			   [(require-for-syntax . _) #t]
			   [_else #f]))
		       decls)
	    ;; Lift define-for-values binding to front, so they can be referenced
            ;;  in compile-time definition
            #,@(let ([ids (apply
                           append
                           (map (lambda (stx)
                                  (syntax-case stx (define-values-for-syntax)
                                    [(define-values-for-syntax ids . _)
                                     (syntax->list #'ids)]
                                    [_else null]))
                                decls))])
                 (if (null? ids)
                     null
                     #`((define-values-for-syntax #,ids
                          (values #,@(map (lambda (x) #'#f) ids))))))
            #,@(if just-one-ct?
                   null
                   #`((define-values-for-syntax (#,compile-time) #,ct-rhs)))
            #,@(if just-one-rt?
                   null
                   #`((define-values (#,run-time) #,rt-rhs)))
            #,@(let loop ([decls decls][ct-pos 0][rt-pos 0])
                 (cond
                   [(null? decls) null]
                   [(is-run-time? (car decls))
                    (cons (syntax-case (car decls) (define-values provide require require-for-syntax require-for-template)
                            [(provide . _)
                             (car decls)]
                            [(require . _)
                             #'(void)]
                            [(require-for-syntax . _)
                             #'(void)]
                            [(require-for-template . _)
                             (car decls)]
                            [(define-values (id ...) rhs)
			     #`(define-values (id ...)
				 #,(let ([lookup #`(vector-ref #,(if just-one-rt? rt-rhs run-time) #,rt-pos)])
				     (if (need-thunk? #'rhs)
					 #`(#,lookup)
					 lookup)))]
                            [else
                             #`((vector-ref #,(if just-one-rt? rt-rhs run-time) #,rt-pos))])
                          (loop (cdr decls) ct-pos (add1 rt-pos)))]
                   [else
                    (cons (syntax-case (car decls) (define-syntaxes define-values-for-syntax)
                            [(define-syntaxes (id ...) . rhs)
                             #`(define-syntaxes (id ...)
                                 ((vector-ref #,(if just-one-ct? ct-rhs compile-time) #,ct-pos)))]
                            [(define-values-for-syntax (id ...) . rhs)
                             #`(define-values-for-syntax ()
                                 (begin
                                   (set!-values (id ...) ((vector-ref #,(if just-one-ct? ct-rhs compile-time) #,ct-pos)))
                                   (values)))])
                          (loop (cdr decls) (add1 ct-pos) rt-pos))])))
	 magic-sym))))

  (define (local-identifier? stx trans?)
    (eq? 'lexical ((if trans?
		      identifier-transformer-binding
		      identifier-binding)
		   stx)))

  (define (simple-identifier stx trans?)
    (let ([b ((if trans?
                  identifier-transformer-binding
                  identifier-binding)
              stx)])
      (cond
       [(eq? b 'lexical) stx]
       [(and (pair? b)
	     (eq? '#%kernel (car b)))
	;; Generate a syntax object that has the right run-time binding:
	(datum->syntax-object #'here (cadr b) stx stx)]
       [else #f])))

  (define (add-literal/pos stx li)
    (let ([pos (lifted-info-counter li)])
      (hash-table-put! (lifted-info-slot-map li) pos stx)
      (set-lifted-info-counter! li (add1 pos))
      pos))
  
  (define (add-literal stx li safe-vector-ref-stx id)
    #`(#,safe-vector-ref-stx #,id #,(add-literal/pos stx li)))
  
  (define (add-identifier/pos stx li trans?)
    (if (identifier? stx)
        ;; id :
        (or (module-identifier-mapping-get (lifted-info-id-map li)
                                           stx 
                                           (lambda () #f))
            (let ([pos (add-literal/pos (if (not ((if trans?
                                                      identifier-transformer-binding
                                                      identifier-binding)
                                                  stx))
                                            #`(#%top . #,stx)
                                            stx)
                                        li)])
              (module-identifier-mapping-put! (lifted-info-id-map li) stx pos)
              pos))
        ;; (#%top . id) :
        (add-literal/pos stx li)))

  (define (add-identifier stx li trans? lookup-stx id)
    #`(#,lookup-stx #,id #,(add-identifier/pos stx li trans?)))

  (define-syntax quasisyntax/loc+props
    (syntax-rules ()
      [(_ stx e) (let ([old-s stx]
		       [new-s (quasisyntax e)])
		   (syntax-recertify
		    (datum->syntax-object new-s
					  (syntax-e new-s)
					  old-s
					  old-s)
		    new-s
		    code-insp
		    #f))]))
  (define code-insp (current-code-inspector))

  (define (convert stx trans? lookup-stx set-stx safe-vector-ref-stx id li in-module? 
		   simple-constant? stop-properties)
    (define ((loop certs) stx)
      (let ([loop (loop (apply-certs stx certs))])
	(if (ormap (lambda (prop)
		     (syntax-property stx prop))
		   stop-properties)
	    stx
	    (kernel-syntax-case stx trans?
	      [_
	       (identifier? stx)
	       (or (simple-identifier stx trans?)
		   (add-identifier (apply-certs certs stx) li trans? lookup-stx id))]
	      [(provide . _)
	       stx]
	      [(lambda formals e ...)
	       (quasisyntax/loc+props 
		stx
		(lambda formals #,@(map loop (syntax->list #'(e ...)))))]
	      [(case-lambda [formals e ...] ...)
	       (with-syntax ([((e ...) ...)
			      (map (lambda (l)
				     (map loop (syntax->list l)))
				   (syntax->list #'((e ...) ...)))])
		 (quasisyntax/loc+props 
		  stx
		  (case-lambda [formals e ...] ...)))]
	      [(let-values ([(id ...) rhs] ...) e ...)
	       (with-syntax ([(rhs ...)
			      (map loop (syntax->list #'(rhs ...)))])
		 (quasisyntax/loc+props 
		  stx
		  (let-values ([(id ...) rhs] ...) #,@(map loop (syntax->list #'(e ...))))))]
	      [(letrec-values ([(id ...) rhs] ...) e ...)
	       (with-syntax ([(rhs ...)
			      (map loop (syntax->list #'(rhs ...)))])
		 (quasisyntax/loc+props 
		  stx
		  (letrec-values ([(id ...) rhs] ...) #,@(map loop (syntax->list #'(e ...))))))]
	      [(quote e)
	       (if (simple-constant? #'e)
		   #'(quote e)
		   (add-literal stx li safe-vector-ref-stx id))]
	      [(quote-syntax e)
	       (add-literal stx li safe-vector-ref-stx id)]
	      [(#%top . tid)
	       (let ([target (let ([b ((if trans?
					   identifier-transformer-binding
					   identifier-binding)
				       #'tid)])
			       (if (or (eq? b 'lexical) 
				       (and (not in-module?)
					    b))
				   #`(#%top . tid)
				   #'tid))])
		 (add-identifier (apply-certs certs target) li trans? lookup-stx id))]
	      [(#%datum . e)
	       (if (simple-constant? #'e)
		   #'(#%datum . e)
		   (add-literal stx li safe-vector-ref-stx id))]
	      [(set! x e)
	       (if (local-identifier? #'x trans?)
		   (quasisyntax/loc+props stx (set! x #,(loop #'e)))
		   (quasisyntax/loc+props 
		    stx
		    (#,set-stx #,id #,(add-identifier/pos (apply-certs certs #'x) li trans?) #,(loop #'e))))]
	      [(#%variable-reference e)
	       (add-literal stx li)]
	      [(if e ...)
	       (quasisyntax/loc+props 
		stx
		(if #,@(map loop (syntax->list #'(e ...)))))]
	      [(begin e ...)
	       (quasisyntax/loc+props 
		stx
		(begin #,@(map loop (syntax->list #'(e ...)))))]
	      [(begin0 e ...)
	       (quasisyntax/loc+props 
		stx
		(begin0 #,@(map loop (syntax->list #'(e ...)))))]
	      [(with-continuation-mark e ...)
	       (quasisyntax/loc+props 
		stx
		(with-continuation-mark #,@(map loop (syntax->list #'(e ...)))))]
	      [(#%app e ...)
	       (quasisyntax/loc+props 
		stx
		(#%app #,@(map loop (syntax->list #'(e ...)))))]))))
    ((loop #'certs) stx))
  
  (define (apply-certs from to)
    (syntax-recertify to from (current-code-inspector) #f)))
