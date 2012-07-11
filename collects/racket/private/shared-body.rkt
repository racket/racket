
;; Used by ../shared.rkt, and also collects/lang/private/teach.rkt
;; Besides the usual things, this code expects `undefined' and
;; `the-cons', to be bound, it expects `struct-declaration-info?'
;; from the "struct.rkt" library of the "syntax" collection, and it
;; expects `code-insp' for-syntax.

(syntax-case stx ()
  [(_ ([name expr] ...) body1 body ...)
   (let ([names (syntax->list (syntax (name ...)))]
	 [exprs (syntax->list (syntax (expr ...)))])
     (for-each (lambda (name)
		 (unless (identifier? name)
		   (raise-syntax-error
		    'shared
		    "not an identifier"
		    stx
		    name)))
	       names)
     (let ([dup (check-duplicate-identifier names)])
       (when dup
	 (raise-syntax-error
	  'shared
	  "duplicate identifier"
	  stx
	  dup)))
     (let ([exprs (map (lambda (expr)
			 (let ([e (local-expand
				   expr
				   'expression
				   (append
				    (kernel-form-identifier-list)
				    names))])
			   ;; Remove #%app if present...
			   (syntax-case (syntax-disarm e code-insp) (#%plain-app)
			     [(#%plain-app a ...)
			      (syntax/loc e (a ...))]
			     [_else e])))
		       exprs)]
           [temp-ids (generate-temporaries names)]
           [placeholder-ids (generate-temporaries names)]
           [ph-used?s (map (lambda (x) (box #f)) names)]
	   [struct-decl-for (lambda (id)
			      (and (identifier? id)
                                   (let ([get-struct
                                          (lambda (id)
                                            (let ([v (syntax-local-value id (lambda () #f))])
                                              (and v
                                                   (struct-declaration-info? v)
                                                   (let ([decl (extract-struct-info v)])
                                                     (and (cadr decl)
                                                          (andmap values (list-ref decl 4))
                                                          (append decl
                                                                  (list
                                                                   (if (struct-auto-info? v)
                                                                       (struct-auto-info-lists v)
                                                                       (list null null)))))))))])
                                     (or (get-struct id)
                                         (let ([s (syntax-property id 'constructor-for)])
                                           (and s
                                                (identifier? s)
                                                (get-struct s)))
                                         (let* ([s (symbol->string (syntax-e id))]
                                                [m (regexp-match-positions "make-" s)])
                                           (and m
                                                (let ([name (datum->syntax
                                                             id
                                                             (string->symbol (string-append (substring s 0 (caar m))
                                                                                            (substring s (cdar m) (string-length s))))
                                                             id)])
                                                  (get-struct name))))))))]
           [append-ids null]
	   [same-special-id? (lambda (a b)
			       ;; Almost module-or-top-identifier=?,
			       ;; but handle the-cons specially
			       (or (free-identifier=? a b)
				   (free-identifier=? 
				    a 
				    (datum->syntax
				     #f
				     (if (eq? 'the-cons (syntax-e b))
					 'cons
					 (syntax-e b))))))]
           [remove-all (lambda (lst rmv-lst)
                         (define (remove e l)
                           (cond
                            [(free-identifier=? e (car l)) (cdr l)]
                            [else (cons (car l) (remove e (cdr l)))]))
                         (let loop ([lst lst] [rmv-lst rmv-lst])
                           (if (null? rmv-lst)
                               lst
                               (loop (remove (car rmv-lst) lst)
                                     (cdr rmv-lst)))))]
           [disarm (lambda (stx) (syntax-disarm stx code-insp))])
       (with-syntax ([(graph-expr ...)
		      (map (lambda (expr)
                             (let loop ([expr expr])
                               (define (bad n)
                                 (raise-syntax-error
                                  'shared
                                  (format "illegal use of ~a" n)
                                  stx
                                  expr))
                               (define (cons-elem expr)
                                 (or (and (identifier? expr)
                                          (ormap (lambda (i ph ph-used?) 
                                                   (and (free-identifier=? i expr)
                                                        (set-box! ph-used? #t)
                                                        ph))
                                                 names placeholder-ids ph-used?s))
                                     (loop expr)))
                               (syntax-case* (disarm expr) (the-cons mcons append box box-immutable vector vector-immutable) same-special-id?
                                 [(the-cons a d)
                                  (with-syntax ([a (cons-elem #'a)]
                                                [d (cons-elem #'d)])
                                    (syntax/loc expr (cons a d)))]
                                 [(the-cons . _)
                                  (bad "cons")]
                                 [(mcons a d)
                                  (syntax (mcons undefined undefined))]
                                 [(mcons . _)
                                  (bad "mcons")]
                                 [(lst e ...)
                                  (ormap (lambda (x) (same-special-id? #'lst x))
                                         (syntax->list #'(list list*)))
                                  (with-syntax ([(e ...)
                                                 (map (lambda (x) (cons-elem x))
                                                      (syntax->list (syntax (e ...))))])
                                    (syntax/loc expr (lst e ...)))]
                                 [(lst . _)
                                  (ormap (lambda (x) (same-special-id? #'lst x))
                                         (syntax->list #'(list list*)))
                                  (bad (syntax-e #'lst))]
                                 [(append e0 ... e)
                                  (let ([len-id (car (generate-temporaries '(len)))])
                                    (set! append-ids (cons len-id append-ids))
                                    (with-syntax ([e (cons-elem #'e)]
                                                  [len-id len-id])
                                      (syntax/loc expr (let ([ph (make-placeholder e)]
                                                             [others (append e0 ... null)])
                                                         (set! len-id (length others))
                                                         (append others ph)))))]
                                 [(append . _)
                                  (bad "append")]
                                 [(box v)
                                  (syntax (box undefined))]
                                 [(box . _)
                                  (bad "box")]
                                 [(box-immutable v)
                                  (with-syntax ([v (cons-elem #'v)])
                                    (syntax/loc expr (box-immutable v)))]
                                 [(vector e ...)
                                  (with-syntax ([(e ...)
                                                 (map (lambda (x) (syntax undefined))
                                                      (syntax->list (syntax (e ...))))])
                                    (syntax (vector e ...)))]
                                 [(vector . _)
                                  (bad "vector")]
                                 [(vector-immutable e ...)
                                  (with-syntax ([(e ...)
                                                 (map (lambda (x) (cons-elem x))
                                                      (syntax->list (syntax (e ...))))])
                                    (syntax/loc expr (vector-immutable e ...)))]
                                 [(vector-immutable . _)
                                  (bad "vector-immutable")]
                                 [(make-x . args)
                                  (struct-decl-for (syntax make-x))
                                  (let ([decl (struct-decl-for (syntax make-x))]
                                        [args (syntax->list (syntax args))])
                                    (unless args
                                      (bad "structure constructor"))
                                    (let ([expected (- (length (list-ref decl 4))
                                                       (length (car (list-ref decl 6))))])
                                      (unless (= expected (length args))
                                        (raise-syntax-error
                                         'shared
                                         (format "wrong argument count for structure constructor; expected ~a, found ~a"
                                                 expected (length args))
                                         stx
                                         expr)))
                                    (with-syntax ([undefineds (map (lambda (x) (syntax undefined)) args)])
                                      (syntax (make-x . undefineds))))]
                                 [_else expr])))
                           exprs)]
                     [(init-expr ...)
		      (map (lambda (expr temp-id used?)
                             (let ([init-id
                                    (syntax-case* expr (the-cons mcons list list* append box box-immutable vector vector-immutable) same-special-id?
                                      [(the-cons . _) temp-id]
                                      [(mcons . _) temp-id]
                                      [(list . _) temp-id]
                                      [(list* . _) temp-id]
                                      [(append . _) temp-id]
                                      [(box . _) temp-id]
                                      [(box-immutable . _) temp-id]
                                      [(vector . _) temp-id]
                                      [(vector-immutable . _) temp-id]
                                      [(make-x . _)
                                       (syntax-case (syntax-disarm expr code-insp) ()
                                         [(make-x . _)
                                          (struct-decl-for (syntax make-x))])
                                       temp-id]
                                      [else #f])])
                               (cond
                                [init-id
                                 (set-box! used? #t)
                                 init-id]
                                [(unbox used?)
                                 temp-id]
                                [else
                                 expr])))
			   exprs temp-ids ph-used?s)]
		     [(finish-expr ...)
		      (let ([gen-n (lambda (l)
				     (let loop ([l l][n 0])
				       (if (null? l)
					   null
					   (cons (datum->syntax (quote-syntax here) n #f)
						 (loop (cdr l) (add1 n))))))]
                            [append-ids (reverse append-ids)])
			(map (lambda (name expr)
                               (let loop ([name name] [expr expr])
                                 (with-syntax ([name name])
                                   (syntax-case* (disarm expr) (the-cons mcons list list* append box box-immutable vector vector-immutable) 
                                                 same-special-id?
                                     [(the-cons a d)
                                      #`(begin #,(loop #`(car name) #'a)
                                               #,(loop #`(cdr name) #'d))]
                                     [(mcons a d)
                                      (syntax (begin 
                                                (set-mcar! name a)
                                                (set-mcdr! name d)))]
                                     [(list e ...)
                                      (let ([es (syntax->list #'(e ...))])
                                        #`(begin
                                            #,@(map (lambda (n e)
                                                      (loop #`(list-ref name #,n) e))
                                                    (gen-n es)
                                                    es)))]
                                     [(list* e ...)
                                      (let* ([es (syntax->list #'(e ...))]
                                             [last-n (sub1 (length es))])
                                        #`(begin
                                            #,@(map (lambda (n e)
                                                      (loop #`(#,(if (= (syntax-e n) last-n)
                                                                     #'list-tail
                                                                     #'list-ref)
                                                               name 
                                                               #,n) 
                                                            e))
                                                    (gen-n es)
                                                    es)))]
                                     [(append e0 ... e)
                                      (with-syntax ([len-id (car append-ids)])
                                        (set! append-ids (cdr append-ids))
                                        (loop #`(list-tail name len-id) #'e))]
                                     [(box v)
                                      (syntax (set-box! name v))]
                                     [(box-immutable v)
                                      (loop #'(unbox name) #'v)]
                                     [(vector e ...)
                                      (with-syntax ([(n ...) (gen-n (syntax->list (syntax (e ...))))])
                                        (syntax (let ([vec name])
                                                  (vector-set! vec n e)
                                                  ...)))]
                                     [(vector-immutable e ...)
                                      (let ([es (syntax->list #'(e ...))])
                                        #`(begin
                                            #,@(map (lambda (n e)
                                                      (loop #`(vector-ref name #,n) e))
                                                    (gen-n es)
                                                    es)))]
                                     [(make-x e ...)
                                      (struct-decl-for (syntax make-x))
                                      (let ([decl (struct-decl-for (syntax make-x))])
                                        (syntax-case (remove-all (reverse (list-ref decl 4)) (cadr (list-ref decl 6))) ()
                                          [() 
                                           (syntax (void))]
                                          [(setter ...) 
                                           (syntax (begin (setter name e) ...))]))]
                                     [_else (syntax (void))]))))
                             names exprs))]
		     [(check-expr ...)
		      (if make-check-cdr
			  (map (lambda (name expr)
				 (syntax-case* expr (the-cons) same-special-id?
				   [(the-cons a d)
				    (make-check-cdr name)]
				   [_else (syntax #t)]))
			       names exprs)
			  null)]
                     [(temp-id ...) temp-ids]
                     [(placeholder-id ...) placeholder-ids]
                     [(ph-used? ...)  (map unbox ph-used?s)]
                     [(used-ph-id ...) (filter values
                                               (map (lambda (ph ph-used?)
                                                      (and (unbox ph-used?)
                                                           ph))
                                                    placeholder-ids ph-used?s))]
                     [(maybe-ph-id ...) (map (lambda (ph ph-used?)
                                               (and (unbox ph-used?)
                                                    ph))
                                             placeholder-ids ph-used?s)])
         (with-syntax ([(ph-init ...) (filter values
                                              (map (lambda (ph ph-used? graph-expr)
                                                     (and (unbox ph-used?)
                                                          #`(placeholder-set! #,ph #,graph-expr)))
                                                   placeholder-ids ph-used?s
                                                   (syntax->list #'(graph-expr ...))))]
                       [(append-id ...) append-ids])
           (syntax/loc stx
             (letrec-values ([(used-ph-id) (make-placeholder #f)] ...
                             [(append-id) #f] ...
                             [(temp-id ...)
                              (begin
                                ph-init ...
                                (apply values (make-reader-graph
                                               (list maybe-ph-id ...))))]
                             [(name) init-expr] ...)
               finish-expr
               ...
               check-expr
               ...
               body1
               body
               ...))))))])
