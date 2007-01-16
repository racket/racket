
(module struct mzscheme
  (require (lib "etc.ss")
           (lib "contract.ss")
	   "stx.ss")
  (require-for-template mzscheme)
  
  (provide parse-define-struct

	   build-struct-generation
	   build-struct-expand-info
	   struct-declaration-info?
	   generate-struct-declaration)


  ;; parse-define-struct stx stx -> (values id id-or-#f list-of-id stx)
  (define (parse-define-struct stx orig-stx)

    (define (parse-at-id)
      (syntax-case stx ()
	[(_ id . rest)
	 (identifier? #'id)
	 (parse-at-fields #'id #f #'rest)]
	[(_ (id super-id) . rest)
	 (and (identifier? #'id)
	      (identifier? #'super-id))
	 (parse-at-fields #'id #'super-id #'rest)]
	[(_ bad . rest)
	   (raise-syntax-error
	    #f
	    "expected an identifer or parenthesized sequence of two identifiers"
	    orig-stx
	    #'bad)]))

    (define (parse-at-fields id sup-id rest)
	(syntax-case rest ()
	  [((field ...) . rest)
	   (let ([fields (syntax->list #'(field ...))])
	     (for-each (lambda (id)
			 (unless (identifier? id)
			   (raise-syntax-error
			    #f
			    "expected a field identifier"
			    orig-stx
			    id)))
		       fields)
	     (parse-at-inspector id sup-id fields #'rest))]
	  [(bad . rest)
	   (raise-syntax-error
	    #f
	    "expected a parenthesized sequence of field identifiers"
	    orig-stx
	    #'bad)]
	  [_
	   (raise-syntax-error
	    #f
	    (string-append "expected fields after " (if sup-id
							"struct name and parent name"
							"struct name"))
	    orig-stx)]))

      (define (parse-at-inspector id sup-id fields rest)
	(syntax-case rest ()
	  [() 
	   (values id sup-id fields #'(current-inspector))]
	  [(inspector more . rest)
	   (not (stx-null? #'rest))
	   (raise-syntax-error
	    #f
	    "unexpected form after inspector"
	    orig-stx
	    #'more)]
	  [(inspector)
	   (values id sup-id fields #'inspector)]
	  [(inspector . rest)
	   (raise-syntax-error
	    #f
	    "unexpected form after inspector"
	    orig-stx
	    #'rest)]
	  [_
	   (raise-syntax-error
	    #f
	    "bad syntax"
	    orig-stx)]))
	  

      (parse-at-id))

  ;; build-struct-names : id (list-of id) bool bool -> (list-of id)
  (define build-struct-names
    (opt-lambda (name-stx fields omit-sel? omit-set? [srcloc-stx #f])
      (let ([name (symbol->string (syntax-e name-stx))]
	    [fields (map symbol->string (map syntax-e fields))]
	    [+ string-append])
	(map (lambda (s)
	       (datum->syntax-object name-stx (string->symbol s) srcloc-stx))
	     (append
	      (list 
	       (+ "struct:" name)
	       (+ "make-" name)
	       (+ name "?"))
	      (let loop ([l fields])
		(if (null? l)
		    null
		    (append
		     (if omit-sel?
			 null
			 (list (+ name "-" (car l))))
		     (if omit-set?
			 null
			 (list (+ "set-" name "-" (car l) "!")))
		     (loop (cdr l))))))))))

  (define build-struct-generation
    (opt-lambda (name-stx fields omit-sel? omit-set? [super-type #f] [prop-value-list null]
                          [immutable-positions null] [mk-rec-prop-list (lambda (struct: make- ? acc mut) null)])
      (let ([names (build-struct-names name-stx fields omit-sel? omit-set?)])
	(build-struct-generation* names name-stx fields omit-sel? omit-set? super-type prop-value-list
				  immutable-positions mk-rec-prop-list))))

  (define build-struct-generation*
    (opt-lambda (names name fields omit-sel? omit-set? [super-type #f] [prop-value-list null]
		       [immutable-positions null] [mk-rec-prop-list (lambda (struct: make- ? acc mut) null)])
      (let ([num-fields (length fields)]
	    [acc/mut-makers (let loop ([l fields][n 0])
			      (if (null? l)
				  null
				  (let ([mk-one
					 (lambda (acc?)
					   (list
					    `(,(if acc?
						   'make-struct-field-accessor
						   'make-struct-field-mutator)
					      ,(if acc? 'acc 'mut)
					      ,n ',(car l))))])
				    (append
				     (if omit-sel?
					 null
					 (mk-one #t))
				     (if omit-set?
					 null
					 (mk-one #f))
				     (loop (cdr l) (add1 n))))))]
	    [extra-props (mk-rec-prop-list 'struct: 'make- '? 'acc 'mut)])
	`(let-values ([(struct: make- ? acc mut)
		       (make-struct-type ',name ,super-type ,num-fields 0 #f 
					 ,prop-value-list (current-inspector)
					 #f ,immutable-positions)])
	   (values struct:
		   make-
		   ?
		   ,@acc/mut-makers)))))

  (define build-struct-expand-info
    (lambda (name-stx fields omit-sel? omit-set? base-name base-getters base-setters)
      (let* ([names (build-struct-names name-stx fields omit-sel? omit-set?)])
	(build-struct-expand-info* names name-stx fields omit-sel? omit-set? base-name base-getters base-setters))))

  (define build-struct-expand-info*
    (lambda (names name-stx fields omit-sel? omit-set? base-name base-getters base-setters)
      (let* ([flds (cdddr names)]
	     [every-other (lambda (l)
			    (let loop ([l l])
			      (cond
			       [(null? l) null]
			       [(null? (cdr l)) (list (car l))]
			       [else (cons (car l) (loop (cddr l)))])))]
	     [add-#f (lambda (omit? base)
		       (if omit?
			   (if (let loop ([l base])
				 (cond
				  [(null? l) #t]
				  [(not (car l)) #f]
				  [else (loop (cdr l))]))
			       (append base '(#f))
			       base)
			   base))]
             [cert-f (gensym)]
	     [qs (lambda (x) (if (eq? x #t)
				 x
				 (and x `(,cert-f (quote-syntax ,x)))))])
        `(let ([,cert-f (syntax-local-certifier #t)])
           (list-immutable
            ,(qs (car names))
            ,(qs (cadr names))
            ,(qs (caddr names))
            (list-immutable 
             ,@(reverse (if omit-sel?
                            null
                            (map qs (if omit-set? flds (every-other flds)))))
             ,@(map qs (add-#f omit-sel? base-getters)))
            (list-immutable
             ,@(reverse (if omit-set?
                            null
                            (map qs (if omit-sel?
                                        flds
                                        (every-other (if (null? flds)
                                                         null
                                                         (cdr flds)))))))
             ,@(map qs (add-#f omit-set? base-setters)))
            ,(qs base-name))))))


  (define (struct-declaration-info? x)
    (define (identifier/#f? x)
      (or (not x) 
	  (identifier? x)))
    (define (id/#f-list? id? x)
      (or (null? x)
	  (and (pair? x)
	       (if (null? (cdr x))
		   (identifier/#f? (car x))
		   (and (id? (car x))
			(id/#f-list? id? (cdr x)))))))

    (and (list? x)
	 (= (length x) 6)
	 (identifier/#f? (car x))
	 (identifier/#f? (cadr x))
	 (identifier/#f? (caddr x))
	 (id/#f-list? identifier? (list-ref x 3))
	 (id/#f-list? identifier/#f? (list-ref x 4))
	 (or (eq? #t (list-ref x 5)) (identifier/#f? (list-ref x 5)))))


  ;; ----------------------------------------

  (define struct-info-type-id car)
  (define struct-info-constructor-id cadr)
  (define struct-info-predicate-id caddr)
  (define struct-info-accessor-ids cadddr)
  (define struct-info-mutator-ids (lambda (x) (list-ref x 4)))

  (define (get-stx-info orig-stx super-id defined-names)
    ;; Looks up super info, if needed, and builds compile-time info for the
    ;; new struct; called by all three forms, but does only half the work
    ;; if `defined-names' is #f.
    ;; If `expr?' is #t, then generate an expression to build the info,
    ;; otherwise build the info directly.
    (let* ([cert-f (gensym)]
           [qs (lambda (x) #`(#,cert-f (quote-syntax #,x)))]
           [every-other (lambda (l)
                          (let loop ([l l][r null])
                            (cond
                             [(null? l) r]
                             [(null? (cdr l)) (cons (car l) r)]
                             [else (loop (cddr l) (cons (car l) r))])))]
           [super-info (and super-id 
                            (syntax-local-value super-id (lambda () #f)))])
      (when super-id 
	;; Did we get valid super-info ?
	(when (or (not (struct-declaration-info? super-info))
		  (not (struct-info-type-id super-info)))
	  (raise-syntax-error
	   #f
	   (if (struct-declaration-info? super-info)
	       "parent struct information does not include a type for subtyping"
	       (format "parent struct type not defined~a"
		       (if super-info
			   (format " (~a does not name struct type information)"
				   (syntax-e super-id))
			   "")))
	   orig-stx
	   super-id)))
      ;; Generate the results:
      (values
       super-info
       (if defined-names
	   (let-values ([(initial-gets initial-sets)
			 (if super-info
			     (values (map qs (struct-info-accessor-ids super-info))
				     (map qs (struct-info-mutator-ids super-info)))
			     (values null null))]
			[(fields) (cdddr defined-names)]
			[(wrap) (lambda (x) #`(list-immutable #,@x))])
             #`(let ([#,cert-f (syntax-local-certifier)])
                 #,(wrap
                    (list-immutable (qs (car defined-names))
                                    (qs (cadr defined-names))
                                    (qs (caddr defined-names))
                                    (wrap
                                     (apply
                                      list-immutable
                                      (append (map qs (every-other fields)) 
                                              initial-gets)))
                                    (wrap
                                     (apply
                                      list-immutable
                                      (append (map qs (if (null? fields) 
                                                          null 
                                                          (every-other (cdr fields)))) 
                                              initial-sets)))
                                    (if super-id
                                        (qs super-id)
                                        #t)))))
	   #f))))

  (define (make-core make-make-struct-type orig-stx defined-names super-info name field-names)
    #`(let-values ([(type maker pred access mutate)
		    #,(make-make-struct-type orig-stx name defined-names super-info)])
	(values type maker pred
		#,@(let loop ([field-names field-names][n 0])
		     (if (null? field-names)
			 null
			 (list* #`(make-struct-field-accessor access #,n '#,(car field-names))
				#`(make-struct-field-mutator mutate #,n '#,(car field-names))
				(loop (cdr field-names) (add1 n))))))))

  (define generate-struct-declaration 
    (opt-lambda (orig-stx
                 name super-id field-names 
                 context 
                 make-make-struct-type
                 [no-sel? #f] [no-set? #f])
      (let ([defined-names (build-struct-names name field-names no-sel? no-set? name)])
        (let-values ([(super-info stx-info) (get-stx-info orig-stx super-id defined-names)])
          (let ([result
                 #`(begin
                     (define-values
                       #,defined-names
                       #,(make-core make-make-struct-type orig-stx defined-names super-info name field-names))
                     (define-syntaxes (#,name)
                       #,stx-info))])
            (if super-id
                (syntax-property result 
                                 'disappeared-use 
                                 (syntax-local-introduce super-id))
                result))))))
  
  (provide/contract
   [build-struct-names
    (opt-> (identifier? (listof identifier?) boolean? boolean?)
           ((union false/c syntax?))
           (listof identifier?))]))
