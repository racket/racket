
;; Unit system

(module unit200 mzscheme
  (require racket/undefined)
  (require-for-syntax syntax/kerncase
		      syntax/stx
		      syntax/name
		      syntax/context
                      racket/syntax
		      (only racket/base filter)
		      "private/unitidmap.rkt")

  ;; ----------------------------------------------------------------------
  ;; Structures and helpers

  (define insp (current-inspector)) ; for named structures

  (define-struct unit (num-imports exports go)) ; unit value
  (define-struct (exn:fail:unit exn:fail) ()) ; run-time exception

  ;; For units with inferred names, generate a struct that prints using the name:
  (define (make-naming-constructor type name)
    (let-values ([(struct: make- ? -accessor -mutator)
		  (make-struct-type name type 0 0 #f null insp)])
      make-))

  ;; Make a unt value (call by the macro expansion of `unit')
  (define (make-a-unit name num-imports exports go)
    ((if name 
	 (make-naming-constructor 
	  struct:unit
	  (string->symbol (format "unit:~a" name)))
	 make-unit)
     num-imports exports go))

  ;; ----------------------------------------------------------------------
  ;; The `unit' syntactic form

  (define-syntaxes (:unit unit/no-expand)
    (let ([do-unit 
	   (lambda (stx expand?)
	     (syntax-case stx (import export)
	       [(_ (import ivar ...)
		   (export evar ...)
		   defn&expr ...)
		(let ([check-id (lambda (v)
				  (unless (identifier? v)
				    (raise-syntax-error
				     #f
				     "import is not an identifier"
				     stx
				     v)))]
		      [check-renamed-id 
		       (lambda (v)
			 (syntax-case v ()
			   [id (identifier? (syntax id)) (list v)]
			   [(lid eid) (and (identifier? (syntax lid))
					   (identifier? (syntax eid))) 
			    (list #'lid #'eid)]
			   [else (raise-syntax-error
				  #f
				  "export is not an identifier or renamed identifier"
				  stx
				  v)]))]
		      [expand-context (generate-expand-context)]
		      [def-ctx (and expand?
				    (syntax-local-make-definition-context))]
		      [localify (lambda (ids def-ctx)
				  (if (andmap identifier? ids)
				      ;; In expand mode, add internal defn context
				      (if expand?
					  (begin
					    ;; Treat imports as internal-defn names:
					    (syntax-local-bind-syntaxes ids #f def-ctx)
                                            (syntax->list
                                             (internal-definition-context-apply def-ctx ids)))
					  ids)
				      ;; Let later checking report an error:
				      ids))])
		  (let ([ivars (localify (syntax->list (syntax (ivar ...))) def-ctx)]
			[evars (syntax->list (syntax (evar ...)))])
		    (for-each check-id ivars)
		    (for-each check-renamed-id evars)
		    
		    ;; Get import/export declared names:
		    (let* ([exported-names
			    (localify
			     (map (lambda (v)
				    (syntax-case v ()
				      [(lid eid) (syntax lid)]
				      [id (syntax id)]))
				  evars)
			     def-ctx)]
			   [extnames (map (lambda (v)
					    (syntax-case v ()
					      [(lid eid) (syntax eid)]
					      [id (syntax id)]))
					  evars)]
			   [imported-names ivars]
			   [declared-names (append imported-names exported-names)])
		      ;; Check that all exports are distinct (as symbols)
		      (let ([ht (make-hash-table)])
			(for-each (lambda (name)
				    (when (hash-table-get ht (syntax-e name) (lambda () #f))
				      (raise-syntax-error
				       #f
				       "duplicate export"
				       stx
				       name))
				    (hash-table-put! ht (syntax-e name) #t))
				  extnames))

		      ;; Expand all body expressions
		      ;; so that all definitions are exposed.
		      (letrec ([expand-all
				(if expand?
				    (lambda (defns&exprs)
				      (apply
				       append
				       (map
					(lambda (defn-or-expr)
					  (let ([defn-or-expr
						  (local-expand
						   defn-or-expr
						   expand-context
						   (append
						    (kernel-form-identifier-list)
						    declared-names)
						   def-ctx)])
					    (syntax-case defn-or-expr (begin define-values define-syntaxes)
					      [(begin . l)
					       (let ([l (syntax->list (syntax l))])
						 (unless l
						   (raise-syntax-error
						    #f
						    "bad syntax (illegal use of `.')"
						    defn-or-expr))
						 (expand-all (map (lambda (s)
								    (syntax-track-origin s defn-or-expr #'begin))
								  l)))]
					      [(define-syntaxes (id ...) rhs)
					       (andmap identifier? (syntax->list #'(id ...)))
					       (with-syntax ([rhs (local-transformer-expand
								   #'rhs
								   'expression
								   null)])
						 (syntax-local-bind-syntaxes (syntax->list #'(id ...)) #'rhs def-ctx)
						 (list #'(define-syntaxes (id ...) rhs)))]
					      [(define-values (id ...) rhs)
					       (andmap identifier? (syntax->list #'(id ...)))
					       (begin
						 (syntax-local-bind-syntaxes (syntax->list #'(id ...)) #f def-ctx)
						 (list defn-or-expr))]
					      [else (list defn-or-expr)])))
					defns&exprs)))
				    values)])

			(let ([all-expanded (expand-all (syntax->list (syntax (defn&expr ...))))])
                          (when def-ctx
                            (internal-definition-context-seal def-ctx))
			  ;; Get all the defined names, sorting out variable definitions
			  ;; from syntax definitions.
			  (let* ([definition?
				   (lambda (id)
				     (and (identifier? id)
					  (or (module-identifier=? id (quote-syntax define-values))
					      (module-identifier=? id (quote-syntax define-syntaxes)))))]
				 [all-defined-names/kinds
				  (apply
				   append
				   (map
				    (lambda (defn-or-expr)
				      (syntax-case defn-or-expr (define-values define-syntaxes)
					[(dv (id ...) expr)
					 (definition? (syntax dv))
					 (let ([l (syntax->list (syntax (id ...)))])
					   (for-each (lambda (i)
						       (unless (identifier? i)
							 (raise-syntax-error
							  #f
							  "not an identifier in definition"
							  defn-or-expr
							  i)))
						     l)
					   (let ([key (if (module-identifier=? (syntax dv) (quote-syntax define-syntaxes))
							  'stx
							  'val)])
					     (map (lambda (id) (cons key id)) l)))]
					[(define-values . l)
					 (raise-syntax-error
					  #f
					  "bad definition form"
					  defn-or-expr)]
					[(define-syntaxes . l)
					 (raise-syntax-error
					  #f
					  "bad syntax definition form"
					  defn-or-expr)]
					[else null]))
				    all-expanded))]
				 [all-defined-names (map cdr all-defined-names/kinds)]
				 [all-defined-val-names (map cdr 
							     (filter (lambda (i) (eq? (car i) 'val))
								     all-defined-names/kinds))])
			    ;; Check that all defined names (var + stx) are distinct:
			    (let ([name (check-duplicate-identifier
					 (append imported-names all-defined-names))])
			      (when name
				(raise-syntax-error 
				 #f
				 "variable imported and/or defined twice"
				 stx
				 name)))
			    ;; Check that all exported names are defined (as var):
			    (let ([ht (make-hash-table)]
				  [stx-ht (make-hash-table)])
			      (for-each
			       (lambda (kind+name)
				 (let ([name (cdr kind+name)])
				   (let ([l (hash-table-get ht (syntax-e name) (lambda () null))])
				     (hash-table-put! (if (eq? (car kind+name) 'val) ht stx-ht)
						      (syntax-e name) 
						      (cons name l)))))
			       all-defined-names/kinds)
			      (for-each 
			       (lambda (n)
				 (let ([v (hash-table-get ht (syntax-e n) (lambda () null))])
				   (unless (ormap (lambda (i) (bound-identifier=? i n)) v)
				     ;; Either not defined, or defined as syntax:
				     (let ([stx-v (hash-table-get stx-ht (syntax-e n) (lambda () null))])
				       (if (ormap (lambda (i) (bound-identifier=? i n)) stx-v)
					   (raise-syntax-error
					    #f
					    "cannot export syntax from a unit"
					    stx
					    n)
					   (raise-syntax-error
					    #f
					    "exported variable is not defined"
					    stx
					    n))))))
			       exported-names))

			    ;; Compute defined but not exported:
			    (let ([ht (make-hash-table)])
			      (for-each
			       (lambda (name)
				 (let ([l (hash-table-get ht (syntax-e name) (lambda () null))])
				   (hash-table-put! ht (syntax-e name) (cons name l))))
			       exported-names)
			      (let ([internal-names
				     (let loop ([l all-defined-val-names])
				       (cond
					[(null? l) null]
					[(let ([v (hash-table-get ht (syntax-e (car l)) (lambda () null))])
					   (ormap (lambda (i) (bound-identifier=? i (car l))) v))
					 (loop (cdr l))]
					[else (cons (car l) (loop (cdr l)))]))])
				;; Generate names for import/export boxes, etc:
				(with-syntax ([(ivar ...) ivars]
					      [(iloc ...) (generate-temporaries ivars)]
					      [(eloc ...) (generate-temporaries evars)]
					      [(extname ...) extnames]
					      [(expname ...) exported-names]
					      [(intname ...) internal-names])
				  ;; Change all definitions to set!s. Convert evars to set-box!,
				  ;; because set! on exported variables is not allowed.
				  (with-syntax ([(defn&expr ...) 
						 (let ([elocs (syntax->list (syntax (eloc ...)))])
						   (filter
						    values
						    (map (lambda (defn-or-expr)
							   (syntax-case defn-or-expr (define-values define-syntaxes)
							     [(define-values ids expr)
							      (let* ([ids (syntax->list (syntax ids))])
								(if (null? ids)
								    (syntax/loc defn-or-expr (set!-values ids expr))
								    (let ([do-one
									   (lambda (id tmp name)
									     (let loop ([evars exported-names]
											[elocs elocs])
									       (cond
										[(null? evars)
										 ;; not an exported id
										 (with-syntax ([id id][tmp tmp])
										   (syntax/loc
										       defn-or-expr
										     (set! id tmp)))]
										[(bound-identifier=? (car evars) id)
										 ;; set! exported id:
										 (with-syntax 
										     ([loc (car elocs)]
										      [tmp 
										       (if name
											   (with-syntax 
											       ([tmp tmp]
												[name name])
											     (syntax 
											      (let ([name tmp])
												name)))
											   tmp)])
										   (syntax/loc defn-or-expr
										     (set-box! loc tmp)))]
										[else (loop (cdr evars) 
											    (cdr elocs))])))])
								      (if (null? (cdr ids))
									  (do-one (car ids) (syntax expr) (car ids))
									  (let ([tmps (generate-temporaries ids)])
									    (with-syntax ([(tmp ...) tmps]
											  [(set ...)
											   (map (lambda (id tmp)
												  (do-one id tmp #f))
												ids tmps)])
									      (syntax/loc defn-or-expr
										(let-values ([(tmp ...) expr])
										  set ...))))))))]
							     [(define-syntaxes . l) #f]
							     [else defn-or-expr]))
							 all-expanded)))]
						[(stx-defn ...) 
						 (filter
						  values
						  (map (lambda (defn-or-expr)
							 (syntax-case defn-or-expr (define-syntaxes)
							   [(define-syntaxes . l) #'l]
							   [else #f]))
						       all-expanded))])
				    ;; Build up set! redirection chain:
				    (with-syntax ([redirections
						   (let ([varlocs 
							  (syntax->list 
							   (syntax ((ivar iloc) ... (expname eloc) ...)))])
						     (with-syntax ([vars (map stx-car varlocs)]
								   [rhss
								    (map
								     (lambda (varloc)
								       (with-syntax ([(var loc) varloc])
									 (syntax
									  (make-id-mapper (quote-syntax (unbox loc))
											  (quote-syntax var)))))
								     varlocs)])
						       (syntax
							([vars (values . rhss)]))))]
						  [num-imports (datum->syntax-object
								(quote-syntax here)
								(length (syntax->list (syntax (iloc ...))))
								#f)]
						  [name (syntax-local-infer-name stx)])
				      (syntax/loc stx
					(make-a-unit
					 'name
					 num-imports
					 (list (quote extname) ...)
					 (lambda ()
					   (let ([eloc (box undefined)] ...)
					     (list (vector eloc ...)
						   (lambda (iloc ...)
						     (letrec-syntaxes+values 
						      (stx-defn ... . redirections)
						      ([(intname) undefined] ...)
						      (void) ; in case the body would be empty
						      defn&expr ...))))))))))))))))))]))])
      (values (lambda (stx) (do-unit stx #t))
	      (lambda (stx) (do-unit stx #f)))))

  ;; ----------------------------------------------------------------------
  ;; check-expected-interface: used by the expansion of `compound-unit'
  
  (define (check-expected-interface tag unit num-imports exports)
    (unless (unit? unit)
      (raise
       (make-exn:fail:unit
	(format "compound-unit: result of expression for tag ~s not a unit: ~e" tag unit)
	(current-continuation-marks))))
    (unless (= num-imports (unit-num-imports unit))
      (raise
       (make-exn:fail:unit
	(format "compound-unit: unit for tag ~s expects ~a imports, given ~a" 
                tag
                (unit-num-imports unit)
                num-imports)
	(current-continuation-marks))))
    (list->vector
     (map (lambda (ex)
	    (let loop ([l (unit-exports unit)][i 0])
	      (cond
	       [(null? l)
		(raise
		 (make-exn:fail:unit
		  (format "compound-unit: unit for tag ~s has no ~s export" 
                          tag ex)
		  (current-continuation-marks)))]
	       [(eq? (car l) ex)
		i]
	       [else (loop (cdr l) (add1 i))])))
	  exports)))

  ;; ----------------------------------------------------------------------
  ;; The `compound-unit' syntactic form

  (define-syntax compound-unit
    (lambda (stx)
      (syntax-case stx (import export link)
	[(_ (import ivar ...)
	    (link [tag (unit-expr linkage ...)] ...)
	    (export exportage ...))
	 (let ([check-id (lambda (v)
			   (unless (identifier? v)
			     (raise-syntax-error
			      #f
			      "import is not an identifier"
			      stx
			      v)))]
	       [check-tag (lambda (v)
			   (unless (identifier? v)
			     (raise-syntax-error
			      #f
			      "tag is not an identifier"
			      stx
			      v)))]
	       [check-linkage (lambda (v)
				(syntax-case v ()
				  [id (identifier? (syntax id)) #t]
				  [(tag id ...)
				   (for-each (lambda (v)
					       (unless (identifier? v)
						 (raise-syntax-error
						  #f
						  "non-identifier in linkage"
						  stx
						  v)))
					     (syntax->list v))]
				  [else
				   (raise-syntax-error
				    #f
				    "ill-formed linkage"
				    stx
				    v)]))]
	       [check-exportage (lambda (v)
				  (syntax-case v ()
				    [(tag ex ...)
				     (begin
				       (unless (identifier? (syntax tag))
					 (raise-syntax-error
					  #f
					  "export tag is not an identifier"
					  stx
					  (syntax tag)))
				       (for-each 
					(lambda (e)
					  (syntax-case e ()
					    [id (identifier? (syntax id)) #t]
					    [(iid eid)
					     (begin
					       (unless (identifier? (syntax iid))
						 (raise-syntax-error
						  #f
						  "export internal name is not an identifier"
						  stx
						  (syntax iid)))
					       (unless (identifier? (syntax eid))
						 (raise-syntax-error
						  #f
						  "export internal name is not an identifier"
						  stx
						  (syntax eid))))]
					    [else
					     (raise-syntax-error
					      #f
					      (format "ill-formed export with tag ~a" 
						      (syntax-e (syntax tag)))
					      stx
					      e)]))
					(syntax->list (syntax (ex ...)))))]
				    [else
				     (raise-syntax-error
				      #f
				      "ill-formed export"
				      stx
				      v)]))]
	       [imports (syntax->list (syntax (ivar ...)))]
	       [tags (syntax->list (syntax (tag ...)))]
	       [linkages (map syntax->list (syntax->list (syntax ((linkage ...) ...))))]
	       [exports (syntax->list (syntax (exportage ...)))])
	   ;; Syntax checks:
	   (for-each check-id imports)
	   (for-each check-tag tags)
	   (for-each (lambda (l) (for-each check-linkage l)) linkages)
	   (for-each check-exportage exports)
	   ;; Check for duplicate imports
	   (let ([dup (check-duplicate-identifier imports)])
	     (when dup
	       (raise-syntax-error
		#f
		"duplicate import"
		stx
		dup)))
	   ;; Check for duplicate tags
	   (let ([dup (check-duplicate-identifier tags)])
	     (when dup
	       (raise-syntax-error
		#f
		"duplicate tag"
		stx
		dup)))
	   ;; Check referenced imports and tags
	   (let ([check-linkage-refs (lambda (v)
				       (syntax-case v ()
					 [(tag . exs)
					  (unless (ormap (lambda (t)
							   (bound-identifier=? t (syntax tag)))
							 tags)
					    (raise-syntax-error
					     #f
					     "linkage tag is not bound"
					     stx
					     (syntax tag)))]
					 [id (unless (ormap (lambda (i)
							      (bound-identifier=? i (syntax id)))
							    imports)
					       (raise-syntax-error
						#f
						"no imported identified for linkage"
						stx
						(syntax id)))]))]
		 [check-export-refs (lambda (v)
				      (syntax-case v ()
					[(tag . r)
					 (unless (ormap (lambda (t)
							  (bound-identifier=? t (syntax tag)))
							tags)
					   (raise-syntax-error
					    #f
					    "export tag is not bound"
					    stx
					    (syntax tag)))]))])
	     (for-each (lambda (l) (for-each check-linkage-refs l))
		       linkages)
	     (for-each check-export-refs exports)
	     ;; Get all export names, and check for duplicates
	     (let ([export-names
		    (apply
		     append
		     (map
		      (lambda (v)
			(syntax-case v ()
			  [(tag . exs)
			   (map
			    (lambda (e)
			      (syntax-case e ()
				[(iid eid) (syntax eid)]
				[id e]))
			    (syntax->list (syntax exs)))]))
		      exports))])
	       (let ([dup (check-duplicate-identifier export-names)])
		 (when dup
		   (raise-syntax-error
		    #f
		    "duplicate export"
		    stx
		    dup)))

	       (let ([constituents (generate-temporaries tags)]
		     [unit-export-positionss (generate-temporaries tags)]
		     [unit-setups (generate-temporaries tags)]
		     [unit-extracts (generate-temporaries tags)]
		     [unit-export-lists
		      ;; For each tag, get all expected exports
		      (let* ([hts (map (lambda (x) (make-hash-table)) tags)]
			     [get-add-name 
			      (lambda (tag)
				(ormap (lambda (t ht)
					 (and (bound-identifier=? t tag)
					      (lambda (name)
						(hash-table-put! ht (syntax-e name) name))))
				       tags hts))])
			;; Walk though linkages
			(for-each
			 (lambda (linkage-list)
			   (for-each 
			    (lambda (linkage)
			      (syntax-case linkage ()
				[(tag . ids)
				 (let ([add-name (get-add-name (syntax tag))])
				   (for-each add-name (syntax->list (syntax ids))))]
				[else (void)]))
			    linkage-list))
			 linkages)
			;; Walk through exports
			(for-each
			 (lambda (v)
			   (syntax-case v ()
			     [(tag . exs)
			      (let ([add-name (get-add-name (syntax tag))])
				(for-each 
				 (lambda (e)
				   (syntax-case e ()
				     [(iid eid) (add-name (syntax iid))]
				     [id (add-name (syntax id))]))
				 (syntax->list (syntax exs))))]))
			 exports)
			;; Extract names from hash tables
			(map (lambda (ht)
			       (hash-table-map ht (lambda (k v) v)))
			     hts))])
		 ;; Map exports to imports and indices based on expected unit exports
		 (let ([map-tag (lambda (t l)
				  (let loop ([tags tags][l l])
				    (if (bound-identifier=? (car tags) t)
					(car l)
					(loop (cdr tags) (cdr l)))))]
		       [unit-export-hts (map (lambda (export-list)
					       (let ([ht (make-hash-table)])
						 (let loop ([l export-list][p 0])
						   (unless (null? l)
						     (hash-table-put! ht (syntax-e (car l)) p)
						     (loop (cdr l) (add1 p))))
						 ht))
					     unit-export-lists)]
		       [interned-integer-lists null]
		       [interned-id-lists null])
		   (let ([make-mapping
			  (lambda (v)
			    (syntax-case v ()
			      [(tag . exs)
			       (let ([extract (map-tag (syntax tag)
						       unit-extracts)]
				     [ht (map-tag (syntax tag)
						  unit-export-hts)])
				 (with-syntax ([extract extract]
					       [pos-name
						(let ([il
						       (map
							(lambda (e)
							  (hash-table-get
							   ht
							   (syntax-e
							    (syntax-case e ()
							      [(iid eid) (syntax iid)]
							      [id e]))))
							(syntax->list (syntax exs)))])
						  (or (ormap (lambda (i)
							       (and (equal? il (cadadr i))
								    (car i)))
							     interned-integer-lists)
						      (let ([name (car (generate-temporaries 
									(list (syntax tag))))])
							(set! interned-integer-lists
							      (cons `(,name ',il)
								    interned-integer-lists))
							name)))])
				   (syntax (map extract pos-name))))]
			      [import v]))]
			 [collapse (lambda (l)
				     (let loop ([l l])
				       (cond
					[(null? l) null]
					[(identifier? (car l))
					 (let-values ([(ids rest)
						       (let loop ([l l][ids null])
							 (if (or (null? l)
								 (not (identifier? (car l))))
							     (values (reverse ids) l)
							     (loop (cdr l) (cons (car l) ids))))])
					   (let ([name
						  (let ([id-syms (map syntax-e ids)])
						    (or (ormap (lambda (i)
								 (and (equal? id-syms (cadr i))
								      (car i)))
							       interned-id-lists)
							(let ([name 
							       (car (generate-temporaries (list 'ids)))])
							  (set! interned-id-lists
								(cons (list* name id-syms ids)
								      interned-id-lists))
							  name)))])
					     (cons name
						   (loop rest))))]
					[else (cons (car l) (loop (cdr l)))])))])
		     (let ([export-mapping (collapse (map make-mapping exports))]
			   [import-mappings (map (lambda (linkage-list)
						   (collapse
						    (map make-mapping linkage-list)))
						 linkages)])
		       (with-syntax ([(constituent ...) constituents]
				     [(unit-export-positions ...) unit-export-positionss]
				     [(unit-setup ...) unit-setups]
				     [(unit-extract ...) unit-extracts]
				     [interned-integer-lists interned-integer-lists]
				     [interned-id-lists (map (lambda (i)
							       (with-syntax ([name (car i)]
									     [ids (cddr i)])
								 (syntax [name (list . ids)])))
							     interned-id-lists)]
				     [(unit-export-list ...) unit-export-lists]
				     [(import-mapping ...) import-mappings]
				     [(unit-import-count ...) 
				      (map (lambda (l) 
					     (datum->syntax-object
					      (quote-syntax here)
					      (apply
					       +
					       (map (lambda (v)
						      (if (identifier? v)
							  1
							  (length (cdr (syntax->list v)))))
						    l))
					      #f))
					   linkages)]
				     [num-imports (datum->syntax-object
						   (quote-syntax here)
						   (length imports)
						   #f)]
				     [export-names export-names]
				     [export-mapping export-mapping]
				     [name (syntax-local-infer-name stx)])
			 (syntax/loc
			  stx
			  (let ([constituent unit-expr]
				...)
			    (let ([unit-export-positions
				   (check-expected-interface 
                                    'tag
				    constituent
				    unit-import-count
				    'unit-export-list)]
				  ...)
			      (make-a-unit
			       'name
			       num-imports
			       (quote export-names)
			       (lambda ()
				 (let ([unit-setup ((unit-go constituent))] ...)
				   (let ([unit-extract
					  (lambda (pos)
					    (vector-ref (car unit-setup)
							(vector-ref unit-export-positions pos)))]
					 ...
					 .
					 interned-integer-lists)
				     (list (list->vector (append . export-mapping))
					   (lambda (ivar ...)
					     (let interned-id-lists
						 (void) ;; in case there are no units
					       (apply (list-ref unit-setup 1) 
						      (append . import-mapping))
					       ...))))))))))))))))))])))

  ;; ----------------------------------------------------------------------
  ;; check-unit: used by the expansion of `invoke-unit'
  
  (define (check-unit u n)
    (unless (unit? u)
      (raise
       (make-exn:fail:unit
	(format "invoke-unit: result of unit expression was not a unit: ~e" u)
	(current-continuation-marks))))
    (unless (= (unit-num-imports u) n)
      (raise
       (make-exn:fail:unit
	(format "invoke-unit: expected a unit with ~a imports, given one with ~a imports"
                n (unit-num-imports u))
	(current-continuation-marks)))))

  ;; ----------------------------------------------------------------------
  ;; The `invoke-unit' syntactic form
  
  (define-syntax invoke-unit
    (lambda (stx)
      (syntax-case stx (import export)
	[(_ unit-expr expr ...)
	 (let ([exprs (syntax (expr ...))])
	   (with-syntax ([(bx ...) (generate-temporaries (syntax (expr ...)))]
			 [num (datum->syntax-object
			       (quote-syntax here)
			       (length (syntax->list exprs)) 
			       #f)])
	     (syntax/loc
	      stx
	      (let ([u unit-expr])
		(check-unit u num)
		(let ([bx (box expr)] ...)
		  ((list-ref ((unit-go u)) 1)
		   bx ...))))))])))

  (define-syntaxes (define-values/invoke-unit
		     namespace-variable-bind/invoke-unit)
    (let ([mk
	   (lambda (global?)
	     (lambda (stx)
	       (syntax-case stx ()
		 [(_ exports unite . prefix+imports)
		  (let* ([badsyntax (lambda (s why)
				      (raise-syntax-error
				       #f
				       (format "bad syntax (~a)" why)
				       stx
				       s))]
			 [symcheck (lambda (s)
				     (or (identifier? s)
					 (badsyntax s "not an identifier")))])
		    (unless (stx-list? (syntax exports))
		      (badsyntax (syntax exports) "not a sequence of identifiers"))
		    (for-each symcheck (syntax->list (syntax exports)))
		    (let ([prefix (if (stx-null? (syntax prefix+imports))
				      #f
				      (stx-car (syntax prefix+imports)))])
		      (unless (or (not prefix)
				  (not (syntax-e prefix))
				  (identifier? prefix))
			(badsyntax prefix "prefix is not an identifier"))
		      (for-each symcheck (let ([v (syntax prefix+imports)])
					   (cond
					    [(stx-null? v) null]
					    [(stx-list? v) (cdr (syntax->list v))]
					    [else
					     (badsyntax (syntax prefix+imports) "illegal use of `.'")])))
		      (with-syntax ([(tagged-export ...) 
				     (if (and prefix (syntax-e prefix))
					 (let ([prefix (string-append
							(symbol->string 
							 (syntax-e prefix))
						      ":")])
					   (map (lambda (s)
						  (datum->syntax-object
						   s
						   (string->symbol
						    (string-append
						     prefix
						     (symbol->string (syntax-e s))))
						   s))
						(syntax->list (syntax exports))))
					 (syntax exports))]
				    [extract-unit (syntax (:unit
							    (import . exports)
							    (export)
							    (values . exports)))])
			(with-syntax ([invoke-unit (with-syntax ([(x . imports)
								  (if prefix
								      (syntax prefix+imports)
								      `(#f))])
						     (syntax (invoke-unit
							      (compound-unit
							       (import . imports)
							       (link [unit-to-invoke (unite . imports)]
								     [export-extractor 
								      (extract-unit (unit-to-invoke . exports))])
							       (export))
							      . imports)))])
			  (if global?
			      (syntax (let-values ([(tagged-export ...) invoke-unit])
					(namespace-set-variable-value! 'tagged-export tagged-export)
					...
					(void)))
			      (syntax (define-values (tagged-export ...) invoke-unit)))))))])))])
      (values (mk #f) (mk #t))))
  
  (provide (rename :unit unit) unit/no-expand
	   compound-unit invoke-unit unit?
	   (struct exn:fail:unit ())

	   define-values/invoke-unit
	   namespace-variable-bind/invoke-unit))
