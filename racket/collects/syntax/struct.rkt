(module struct racket/base
  (require (for-syntax racket/base)
           racket/contract/base
           "stx.rkt"
           racket/struct-info)
  (require (for-template racket/base))
  
  (provide parse-define-struct

	   build-struct-generation
	   build-struct-generation*
	   build-struct-expand-info
	   struct-declaration-info?
	   extract-struct-info
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
	    "expected an identifier or parenthesized sequence of two identifiers"
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
    (lambda (name-stx fields omit-sel? omit-set? [srcloc-stx #f]
                      #:constructor-name [ctr-name #f])
      (let ([name (symbol->string (syntax-e name-stx))]
	    [fields (map symbol->string (map syntax-e fields))]
	    [+ string-append])
	(map (lambda (s)
               (if (string? s)
                   (datum->syntax name-stx (string->symbol s) srcloc-stx)
                   s))
	     (append
	      (list 
	       (+ "struct:" name)
	       (if ctr-name 
                   ctr-name
                   (+ "make-" name))
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
    (lambda (name-stx fields omit-sel? omit-set? [super-type #f] [prop-value-list '(list)]
                      [immutable-positions '(list)]
                      #:constructor-name [ctr-name #f])
      (let ([names (build-struct-names name-stx fields omit-sel? omit-set?
                                       #:constructor-name ctr-name)])
	(build-struct-generation* names name-stx fields omit-sel? omit-set? super-type prop-value-list
				  immutable-positions))))

  (define build-struct-generation*
    (lambda (names name fields omit-sel? omit-set? [super-type #f] [prop-value-list '(list)]
                   [immutable-positions '(list)])
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
				     (loop (cdr l) (add1 n))))))])
	`(let-values ([(struct: make- ? acc mut)
		       (make-struct-type ',name ,super-type ,num-fields 0 #f 
					 ,prop-value-list (current-inspector)
					 #f ,immutable-positions)])
	   (values struct:
		   make-
		   ?
		   ,@acc/mut-makers)))))

  (define build-struct-expand-info
    (lambda (name-stx fields omit-sel? omit-set? base-name base-getters base-setters 
                      #:omit-constructor? [no-ctr? #f]
                      #:constructor-name [ctr-name #f]
                      #:omit-struct-type? [no-type? #f])
      (let* ([names (build-struct-names name-stx fields omit-sel? omit-set?
                                        #:constructor-name ctr-name)]
             [names (if no-ctr?
                        (list* (car names)
                               #f
                               (cddr names))
                        names)]
             [names (if no-type?
                        (cons #f (cdr names))
                        names)])
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
             [qs (lambda (x) (if (eq? x #t)
                                 x
                                 (and x `(quote-syntax ,x))))]
             [self-sels (reverse (if omit-sel?
                                     null
                                     (map qs (if omit-set? flds (every-other flds)))))]
             [self-sets (reverse (if omit-sel?
                                     null
                                     (if omit-set?
                                         (map (lambda (sel) #f) self-sels)
                                         (map qs (every-other (if (null? flds)
                                                                  null
                                                                  (cdr flds)))))))]
             [all-sels
              `(list 
                ,@self-sels
                ,@(map qs (add-#f (or omit-sel? omit-set?) base-getters)))]
             [all-sets
              `(list
                ,@self-sets
                ,@(map qs (add-#f (or omit-sel? omit-set?) base-setters)))]
             [ans
              `(let ()
                 (list
                  ,(qs (car names))
                  ,(qs (cadr names))
                  ,(qs (caddr names))
                  ,all-sels
                  ,all-sets
                  ,(qs base-name)))])
        ans)))


  (define (struct-declaration-info? x)
    (struct-info? x))

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
    (let* ([qs (lambda (x) #`(quote-syntax #,x))]
           [every-other (lambda (l)
                          (let loop ([l l][r null])
                            (cond
                             [(null? l) r]
                             [(null? (cdr l)) (cons (car l) r)]
                             [else (loop (cddr l) (cons (car l) r))])))]
           [super-si (and super-id 
                          (syntax-local-value super-id (lambda () #f)))]
           [super-info (and super-si
                            (struct-declaration-info? super-si)
                            (extract-struct-info super-si))])
      (when super-id 
	;; Did we get valid super-info ?
	(when (or (not (struct-declaration-info? super-si))
		  (not (struct-info-type-id super-info)))
	  (raise-syntax-error
	   #f
	   (if (struct-declaration-info? super-si)
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
                        [(wrap) (lambda (x) #`(list #,@x))])
             #`(let ()
                 (make-struct-info
                  (lambda ()
                    #,(wrap
                       (list (qs (car defined-names))
                                       (qs (cadr defined-names))
                                       (qs (caddr defined-names))
                                       (wrap
                                        (apply
                                         list
                                         (append (map qs (every-other fields)) 
                                                 initial-gets)))
                                       (wrap
                                        (apply
                                         list
                                         (append (map qs (if (null? fields) 
                                                             null 
                                                             (every-other (cdr fields)))) 
                                                 initial-sets)))
                                       (if super-id
                                           (qs super-id)
                                           #t)))))))
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
    (lambda (orig-stx
             name super-id field-names 
             context 
             make-make-struct-type
             [no-sel? #f] [no-set? #f]
             #:constructor-name [ctr-name #f])
      (let ([defined-names (build-struct-names name field-names no-sel? no-set? name
                                               #:constructor-name ctr-name)])
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
    (->* (identifier? (listof identifier?) boolean? boolean?)
         ((or/c #f syntax?) 
          #:constructor-name (or/c #f identifier?))
         (listof identifier?))]))
