
(module sigutil mzscheme
  ;; Used by unitsig.rkt
  ;; (needs an overhaul, too)

  (require syntax/stx
	   syntax/struct
	   syntax/context)

  (require "sigmatch.rkt")
  (require "../unit200.rkt")
  (require (only racket/base filter sort))

  (define-struct signature (name      ; sym
			    src       ; sym
			    elems     ; list of syms and signatures
			    ctxs      ; list of stx
			    structs)) ; list of struct-infos
  (define-struct parsed-unit (imports renames vars import-vars body stx-checks))

  (define-struct struct-def (name super-name names))

  (define-struct sigdef (content interned))
  (define (make-sig x) (make-sigdef x #f))
  (provide make-sig)

  (define inline-sig-name '<unnamed>)

  (define-syntax literal?
    (lambda (expr)
      (syntax-case expr ()
	[(_ a)
	 (syntax (eq? (syntax-e (syntax a)) 'a))])))

  (define (stx->sym s)
    (if (syntax? s) (syntax-e s) s))

  (define syntax-error
    (case-lambda 
     [(who expr msg sub)
      (raise-syntax-error who msg expr sub)]
     [(who expr msg)
      (raise-syntax-error who msg expr)]))

  (define undef-sig-error
    (lambda (who expr what)
      (syntax-error #f expr 
		    "signature not defined"
		    what)))

  (define not-a-sig-error
    (lambda (who expr what)
      (syntax-error #f expr 
		    "not a signature"
		    what)))

  (define rename-signature
    (lambda (sig name main-ctx)
      (make-signature name 
		      (signature-src sig) 
		      (signature-elems sig)
		      (if main-ctx
			  (map (lambda (ctx) (or ctx main-ctx)) (signature-ctxs sig))
			  (signature-ctxs sig))
		      (signature-structs sig))))

  (define intern-signature
    (lambda (name desc)
      (let ([elems (vector->list (car desc))])
	(make-signature
	 name
	 name
	 (map
	  (lambda (elem)
	    (cond
	     [(symbol? elem) elem]
	     [(and (pair? elem) (symbol? (car elem)))
	      (intern-signature (car elem) (cdr elem))]
	     [else (error "intern failed")]))
	  elems)
	 (map (lambda (elem) #f) elems)
	 (map
	  (lambda (elem)
	    (make-struct-def (vector-ref elem 0)
			     (vector-ref elem 1)
			     (cddr (vector->list elem))))
	  (vector->list (cdr desc)))))))

  (define get-sig
    (lambda (who expr name sigid main-ctx)
      (if (not (identifier? sigid))
	  (parse-signature who expr 
			   (if name
			       name
			       inline-sig-name)
			   sigid
			   main-ctx)
	  (let ([v (syntax-local-value sigid (lambda () #f))])
	    (unless v
	      (undef-sig-error who expr sigid))
	    (unless (sigdef? v)
	      (not-a-sig-error who expr sigid))
	    (unless (sigdef-interned v)
	      (set-sigdef-interned! v (intern-signature (syntax-e sigid) (sigdef-content v))))
	    (let ([s (sigdef-interned v)])
	      (if name
		  (rename-signature s (stx->sym name) (and main-ctx sigid))
		  s))))))

  (define check-unique
    (lambda (names error-k)
      (let ([dup (check-duplicate-identifier 
		  (map (lambda (n)
			 (if (syntax? n)
			     n
			     (datum->syntax-object #f n #f)))
		       names))])
	(when dup
	  (error-k dup)))))

  (define parse-signature
    (lambda (who expr name body main-ctx)
      (let-values ([(elems ctxs struct-defs)
		    (let loop ([body body][accum null][ctx-accum null][struct-accum null])
		      (syntax-case body ()
			[() (values (reverse accum) (reverse ctx-accum) (reverse struct-accum))]
			[(something . rest)
			 (syntax-case (syntax something) ()
			   [:
			    (literal? :)
			    (syntax-error #f expr
					  "misplaced `:'"
					  (syntax something))]
			   [id
			    (identifier? (syntax id))
			    (loop
			     (syntax rest)
			     (cons (syntax id) accum)
			     (cons (syntax id) ctx-accum)
			     struct-accum)]
			   [(struct name (field ...) omission ...)
			    (literal? struct)
			    (let ([name (syntax name)]
				  [fields (syntax->list (syntax (field ...)))]
				  [omissions (syntax->list (syntax (omission ...)))])
			      (unless (or (identifier? name)
					  ;; >>> disabled the `(name super)' case for now <<<
					  (and #f (syntax-case name ()
						    [(name super)
						     (and (identifier? (syntax name))
							  (identifier? (syntax super)))]
						    [_else #f])))
				(syntax-error #f expr
					      "struct name is not an identifier" ;; " or a parenthesized name--super sequence of identifiers"
					      name))
			      (for-each
			       (lambda (fld)
				 (unless (identifier? fld)
				   (syntax-error #f expr
						 "field name is not an identifier"
						 fld)))
			       fields)
			      (let-values ([(omit-names
					     omit-setters?
					     omit-selectors?)
					    (let loop ([omissions omissions]
						       [names null]
						       [no-set? #f]
						       [no-sel? #f])
					      (if (null? omissions)
						  (values names no-set? no-sel?)
						  (let ([rest (cdr omissions)])
						    (syntax-case (car omissions) ()
						      [-selectors
						       (literal? -selectors)
						       (loop rest names no-sel? #t)]
						      [-setters
						       (literal? -setters)
						       (loop rest names #t no-set?)]
						      [(- name)
						       (and (literal? -) (identifier? (syntax name)))
						       (loop rest (cons (syntax name) names)
							     no-set? no-sel?)]
						      [else
						       (syntax-error #f expr
								     "bad struct omission"
								     (car omissions))]))))]
					   [(name super-name) (if (identifier? name)
								  (values name #f)
								  (values (stx-car name)
									  (stx-car (stx-cdr name))))])
				(letrec ([names (build-struct-names 
						 name fields 
						 omit-selectors? omit-setters?)]
					 [filter
					  (lambda (names)
					    (cond
					     [(null? names) null]
					     [(ormap (lambda (x) (eq? (syntax-e (car names))
								      (syntax-e x)))
						     omit-names)
					      (filter (cdr names))]
					     [else (cons (car names) (filter (cdr names)))]))])
				  (let ([elems (if (null? omit-names)
						   names 
						   (filter names))])
				    (loop (syntax rest)
					  (append
					   elems
					   accum)
					  (append
					   (map (lambda (elem) name) elems)
					   ctx-accum)
					  (cons (make-struct-def (syntax-e name) 
								 (and super-name (syntax-e super-name))
								 names)
						struct-accum))))))]
			   [(struct . _)
			    (literal? struct)
			    (syntax-error #f expr
					  "bad `struct' clause form"
					  (syntax something))]
			   [(unit name : sig) 
			    (and (literal? unit)
				 (identifier? (syntax name)))
			    (let ([s (get-sig who expr (syntax name) (syntax sig) (and main-ctx (syntax sig)))])
			      (loop (syntax rest)
				    (cons s accum)
				    (cons (syntax name) ctx-accum)
				    struct-accum))]
			   [(unit . _)
			    (literal? unit)
			    (syntax-error #f expr 
					  "bad `unit' clause form"
					  (syntax something))]
			   [(open sig)
			    (literal? open)
			    (let ([s (get-sig who expr #f (syntax sig) (and main-ctx (syntax sig)))])
			      (loop (syntax rest)
				    (append (signature-elems s) accum)
				    (append
				     (map (lambda (e ctx)
					    (or ctx (syntax sig)))
					  (signature-elems s)
					  (signature-ctxs s))
				     ctx-accum)
				    (append (signature-structs s) struct-accum)))]
			   [(open . _)
			    (literal? open)
			    (syntax-error #f expr 
					  "bad `open' clause form"
					  (syntax something))]
			   [else
			    (syntax-error #f expr "improper signature clause type"
					  (syntax something))])]
			[_else (syntax-error #f expr "ill-formed signature"
					     body)]))])
	(check-unique (map
		       (lambda (elem)
			 (cond
			  [(symbol? elem) elem]
			  [(identifier? elem) (syntax-e elem)]
			  [else (signature-name elem)]))
		       elems)
		      (lambda (name)
			(syntax-error #f expr
				      "duplicate name in signature"
				      name)))
	(let ([sorted (sort-signature-elems (map cons 
						 (map (lambda (id)
							(if (identifier? id)
							    (syntax-e id)
							    id))
						      elems)
						 (if main-ctx
						     (map (lambda (ctx) (or ctx main-ctx)) ctxs)
						     (map (lambda (id) #f) ctxs))))])
	  (make-signature (stx->sym name)
			  (stx->sym name)
			  (map car sorted)
			  (map cdr sorted)
			  struct-defs)))))

  (define (intern-vector intern-box v)
    (if (and intern-box
	     (equal? #() (cdr v))
	     (andmap symbol? (vector->list (car v))))
	(or (ormap (lambda (i)
		     (and (equal? (car v) (caadr i))
			  (list 'unquote (car i))))
		   (unbox intern-box))
	    (let ([name (car (generate-temporaries '(idvec)))])
	      (set-box! intern-box
			(cons (list name v)
			      (unbox intern-box)))
	      (list 'unquote name)))
	v))

  (define explode-sig
    (lambda (sig intern-box)
      (intern-vector
       intern-box
       (cons
	(list->vector
	 (map
	  (lambda (v)
	    (if (symbol? v)
		v
		(cons
		 (signature-name v)
		 (explode-sig v intern-box))))
	  (signature-elems sig)))
	(list->vector
	 (map
	  (lambda (v)
	    (list->vector (list* (struct-def-name v)
				 (struct-def-super-name v)
				 (struct-def-names v))))
	  (signature-structs sig)))))))

  (define explode-named-sig
    (lambda (s intern-box)
      (cons
       (cond
	[(signature-name s)]
	[(signature-src s)]
	[else inline-sig-name])
       (explode-sig s intern-box))))

  (define explode-named-sigs
    (lambda (sigs intern-box)
      (map (lambda (sig) (explode-named-sig sig intern-box)) sigs)))

  (define sort-signature-elems
    (lambda (elems)
      (map car
	   (sort (map (lambda (ip)
                        (let ([i (car ip)])
                          (cons ip (symbol->string
                                    (if (symbol? i) i (signature-name i))))))
                      elems)
                 ;; Less-than; put subs at front
                 (lambda (a b)
                   (if (symbol? (caar a))
                     (if (symbol? (caar b))
                       (string<? (cdr a) (cdr b))
                       #f)
                     (if (symbol? (caar b))
                       #t
                       (string<? (cdr a) (cdr b)))))))))
  
  (define flatten-signature
    (lambda (id sig main-ctx)
      (apply
       append
       (map
	(lambda (elem ctx)
	  (if (symbol? elem)
	      (let ([sym
		     (if id
			 (string->symbol (string-append id ":" (symbol->string elem)))
			 elem)])
		(list
		 (if main-ctx
		     (datum->syntax-object (or ctx main-ctx) sym)
		     sym)))
	      (flatten-signature (let* ([n (signature-name elem)]
					[s (if n
					       (symbol->string n)
					       #f)])
				   (if (and id s)
				       (string-append id ":" s)
				       (or id s)))
				 elem
				 (or ctx main-ctx))))
	(signature-elems sig)
	(signature-ctxs sig)))))

  (define flatten-signatures
    (lambda (sigs main-ctx)
      (apply append (map (lambda (s) 
			   (let* ([name (signature-name s)]
				  [id (if name
					  (symbol->string name)
					  #f)])
			     (flatten-signature id s main-ctx)))
			 sigs))))

  (define signature-parts
    (lambda (q?)
      (lambda (sig)
	(let loop ([elems (signature-elems sig)])
	  (cond
	   [(null? elems) null]
	   [(q? (car elems)) (cons (car elems) (loop (cdr elems)))]
	   [else (loop (cdr elems))])))))
  (define signature-vars (signature-parts symbol?))
  (define signature-subsigs (signature-parts signature?))

  (define do-rename
    (lambda (export-name renames)
      (let loop ([renames renames])
	(cond
	 [(null? renames) export-name]
	 [(eq? (cadar renames) export-name)
	  (caar renames)]
	 [else (loop (cdr renames))]))))

  (define (make-struct-stx-decls sig prefix init-prefix? src-stx check?)
    ;; If check? is #f, generates a syntax definition for <name> for
    ;; each <name> struct form in `sig'. Used for imports.
    ;; If check? is #t, generates an empty syntax "definition" that has
    ;; the side-effect of checking <name> against its expected shape.
    ;; CURRENTLY, check? is always #f.
    (let ([signame (and init-prefix?
			(signature-name sig))])
      (append
       (apply
	append
	(map (lambda (s)
	       (make-struct-stx-decls s
				      (if signame
					  (format "~a~a:"
						  (or prefix "")
						  signame)
					  prefix)
				      #t
				      src-stx
				      check?))
	     (filter signature? (signature-elems sig))))
       (map (lambda (si)
	      (let ([names (struct-def-names si)]
		    [pfx (lambda (s)
			   `(quote-syntax
			     ,(let ([id (string->symbol 
					 (format "~a~a~a" 
						 (or prefix "")
						 (if signame
						     (format "~a:" signame)
						     "")
						 (if (syntax? s)
						     (syntax-e s)
						     s)))])
				(datum->syntax-object src-stx id))))])
		(let* ([name (pfx (struct-def-name si))]
		       [check (if check?
				  (lambda (l)
				    `(verify-struct-shape ,name ,l))
				  values)])
		  `(define-syntaxes (,@(if check? null (list (cadr name))))
		     ,(check
		       `(list ,(pfx (car names))
                              ,(pfx (cadr names))
                              ,(pfx (caddr names))
                              ;; trailing #fs below mean that we don't know whether we have all the fields:
                              (list
                               ,@(map pfx (every-other (cdddr names)))
                               #f)
                              (list
                               ,@(map pfx (every-other (if (null? (cdddr names)) null (cddddr names))))
                               #f)
                              #f))))))
	    (signature-structs sig)))))

  ;; Could be called at expansion time from the result of a `unit/sig' expansion.
  ;; NOT CURRENTLY USED.
  (define (verify-struct-shape name shape)
    (let ([v (syntax-local-value name (lambda () #f))]
	  [n (length (list-ref shape 3))])
      (unless (and (list? v)
		   (= (length v) 5)
		   (identifier? (car v))
		   (module-identifier=? (car v) (car shape))
		   (identifier? (cadr v))
		   (module-identifier=? (cadr v) (cadr shape))
		   (identifier? (caddr v))
		   (module-identifier=? (caddr v) (caddr shape))
		   (list? (list-ref v 3))
		   (= (length (list-ref v 3)) n)
		   (andmap identifier? (list-ref v 3))
		   (andmap module-identifier=? (list-ref v 3) (list-ref shape 3))
		   (list? (list-ref v 4))
		   (= (length (list-ref v 4)) n)
		   (andmap identifier? (list-ref v 4))
		   (andmap module-identifier=? (list-ref v 4) (list-ref shape 4)))
	(raise-syntax-error
	 'unit/sig
	 (format "struct definition for `~a' within the unit does not match the export signature (~a)"
		 (let ([s (symbol->string (syntax-e name))])
		   (substring s 0 (- (string-length s) 2)))
		 ;; Say why:
		 (cond
		  [(not v) "definition is missing or does not use `define-struct'"]
		  [(not (and (list? v)
			     (= (length v) 5)
			     (identifier? (car v))
			     (identifier? (cadr v))
			     (identifier? (caddr v))
			     (list? (list-ref v 3))
			     (andmap identifier? (list-ref v 3))
			     (list? (list-ref v 4))
			     (andmap identifier? (list-ref v 4))
			     (= (length (list-ref v 4)) (length (list-ref v 3)))))
		   "compile-time struct information is stragely malformed"]
		  [(not (< (length (list-ref v 3)) n))
		   "extra fields in definition"]
		  [(not (> (length (list-ref v 3)) n))
		   "missing fields in definition"]
		  [else
		   "different field names or order"]))))
      ;; Return 0 values to context
      (values)))
	 

  (define (every-other l)
    (let loop ([l l][r null])
      (cond
       [(null? l) r]
       [(null? (cdr l)) (cons (car l) r)]
       [else (loop (cddr l) (cons (car l) r))])))
  
  ;; ------------------------------------------------------------
      
  (define check-signature-unit-body
    (lambda (sig a-unit renames who expr)
      (let ([vars (map syntax-e (parsed-unit-vars a-unit))])
	(for-each
	 (lambda (var)
	   (let ([renamed (let ([s (do-rename var renames)])
			    (if (syntax? s)
				(syntax-e s)
				s))])
	     (unless (memq renamed vars)
		     (syntax-error #f expr
				   (format 
				    "signature \"~s\" requires variable \"~s\"~a"
				    (signature-src sig)
				    var
				    (if (eq? var renamed)
					""
					(format " renamed \"~s\"" renamed)))))))
	 (signature-vars sig))
	(unless (null? (signature-subsigs sig))
		(syntax-error #f expr
			      (format 
			       "signature \"~s\" requires sub-units"
			       (signature-src sig)))))))

  (define parse-imports
    (lambda (who untagged-legal? really-import? expr clause keep-ctx?)
      (let ([bad
	     (lambda (why . rest)
	       (apply
		syntax-error #f expr 
		(format (if really-import?
			    "bad `import' clause~a" 
			    "bad linkage specification~a")
			why)
		rest))])
	(let ([clause (stx->list clause)])
	  (unless (stx-list? clause)
	    (bad ""))
	  (map
	   (lambda (item)
	     (syntax-case item ()
	       [id 
		(and (identifier? (syntax id))
		     untagged-legal?)
		(rename-signature (get-sig who expr #f item (and keep-ctx? (syntax id))) #f (syntax id))]
	       [(id : sig)
		(and (identifier? (syntax id))
		     (literal? :))
		(get-sig who expr (syntax id) (syntax sig) (and keep-ctx? (syntax sig)))]
	       [any
		untagged-legal?
		(rename-signature (get-sig who expr #f item (and keep-ctx? (syntax any))) #f (syntax any))]
	       [_else
		(bad "" item)]))
	   clause)))))

  (define parse-unit
    (lambda (expr body sig user-stx-forms dv-stx ds-stx begin-stx)
      (let ([body (stx->list body)])
	(unless body
	  (syntax-error #f expr "illegal use of `.'"))
	(unless (and (pair? body)
		     (stx-pair? (car body))
		     (eq? 'import (syntax-e (stx-car (car body)))))
	  (syntax-error #f expr 
			"expected `import' clause"))
	(let* ([imports (parse-imports 'unit/sig #t #t expr (stx-cdr (car body)) #t)]
	       [imported-names (flatten-signatures imports #f)]
	       [exported-names (flatten-signature #f sig #f)]
	       [def-ctx (syntax-local-make-definition-context)]
	       [body (cdr body)])
	  (let-values ([(renames body)
			(if (and (stx-pair? body)
				 (stx-pair? (car body))
				 (eq? 'rename (syntax-e (stx-car (car body)))))
			    (values (map (lambda (p)
					   (list (stx-car p)
						 (syntax-e (stx-car (stx-cdr p)))))
					 (cdr (stx->list 
					       (let ([rn (car body)])
                                                 ;; Use internal-definition-context-apply ??
						 (local-expand rn
							       'expression
							       (list (stx-car rn))
							       def-ctx)))))
				    (cdr body))
			    (values null body))])
	    (unless renames
	      (syntax-error #f expr "illegal use of `.'" (car body)))
	    ;; Check renames:
	    (let ([bad
		   (lambda (why sub)
		     (syntax-error #f expr 
				   (format "bad `rename' clause~a" why)
				   sub))])
	      (for-each
	       (lambda (id)
		 (syntax-case id ()
		   [(iid eid)
		    (begin
		      (unless (identifier? (syntax iid))
			(bad ": original name is not an identifier" (syntax iid)))
		      (unless (identifier? (syntax eid))
			(bad ": new name is not an identifier" (syntax eid))))]
		   [else
		    (bad "" id)]))
	       renames))
	    (check-unique (map car renames)
			  (lambda (name)
			    (syntax-error #f expr
					  "id renamed twice"
					  name)))
	    (let* ([renamed-internals (map car renames)]
		   [swapped-renames (map (lambda (s) (cons (cadr s) (car s))) renames)]
		   [filtered-exported-names 
		    (if (null? renames) ;; an optimization
			exported-names
			(let loop ([e exported-names])
			  (if (null? e)
			      e
			      (if (ormap (lambda (rn) (eq? (car rn) (car e))) 
					 swapped-renames)
				  (loop (cdr e))
				  (cons (car e) (loop (cdr e)))))))]
		   [local-vars (map (lambda (s)
                                      (datum->syntax-object expr s))
                                    (append renamed-internals filtered-exported-names imported-names))]
		   [expand-context (generate-expand-context)]
		   [import-stxes (apply append (map (lambda (i) 
						      (map
						       (lambda (d)
							 (datum->syntax-object expr d))
						       (make-struct-stx-decls i #f #t expr #f)))
						    imports))]
		   [import-vars
		    (let ([vars (map (lambda (sym) (datum->syntax-object expr sym expr))
				     (flatten-signatures imports 'must-have-ctx))])
		      ;; Treat imported names like internal definitions:
		      (syntax-local-bind-syntaxes vars #f def-ctx)
		      (cdr (syntax->list (local-expand #`(stop #,@vars)
						       'expression
						       (list #'stop)
						       def-ctx))))])
	      (let loop ([pre-lines null][lines (append import-stxes body)][port #f][port-name #f][body null][vars null])
		(cond
		 [(and (null? pre-lines) (not port) (null? lines))
                  (internal-definition-context-seal def-ctx)
		  (make-parsed-unit imports 
				    renames 
				    vars 
				    import-vars
				    body
				    (lambda (src-stx) 
				      ;; Disabled until we have a mechanism for declaring precise information in signatures:
				      ;; (make-struct-stx-decls sig #f #f src-stx #t)
				      null))]
		 [(and (null? pre-lines) (not port) (not (pair? lines)))
		  (syntax-error #f expr "improper body list form")]
		 [else
		  (let-values ([(line) (let ([s (cond
						 [(pair? pre-lines) (car pre-lines)]
						 [port (read-syntax port-name port)]
						 [else (car lines)])])
					 (if (eof-object? s)
					     s
					     (local-expand s    
							   expand-context
							   (append
							    user-stx-forms
							    local-vars)
							   def-ctx)))]
			       [(rest-pre-lines) 
				(if (null? pre-lines)
				    null
				    (cdr pre-lines))]
			       [(rest-lines)
				(if (and (null? pre-lines) (not port))
				    (cdr lines)
				    lines)])
		    (cond
		     [(and (null? pre-lines) 
			   port
			   (eof-object? line))
		      (values lines body vars)]
		     [(and (stx-pair? line)
			   (identifier? (stx-car line))
			   (module-identifier=? (stx-car line) dv-stx))
		      (syntax-case line ()
			[(_ (id ...) rhs)
			 (let ([ids (syntax->list #'(id ...))])
			   (for-each (lambda (id)
				       (unless (identifier? #'id)
					 (syntax-error #f id "not an identifier" line)))
				     ids)
			   (syntax-local-bind-syntaxes ids #f def-ctx)
			   (loop rest-pre-lines
				 rest-lines
				 port
				 port-name
				 (cons line body)
				 (append ids vars)))]
			[else
			 (syntax-error #f expr 
				       "improper `define-values' clause form"
				       line)])]
		     [(and (stx-pair? line)
			   (identifier? (stx-car line))
			   (module-identifier=? (stx-car line) ds-stx))
		      (syntax-case line ()
			[(_ (id ...) rhs)
			 (let ([ids (syntax->list #'(id ...))])
			   (for-each (lambda (id)
				       (unless (identifier? #'id)
					 (syntax-error #f id "not an identifier" line)))
				     ids)
			   (with-syntax ([rhs (local-transformer-expand
					       #'rhs
					       'expression
					       null)])
			     (syntax-local-bind-syntaxes ids #'rhs def-ctx)
			     (loop rest-pre-lines
				   rest-lines
				   port
				   port-name
				   (cons line body)
				   vars)))]
			[else
			 (syntax-error #f expr 
				       "improper `define-syntaxes' clause form"
				       line)])]
		     [(and (stx-pair? line)
			   (identifier? (stx-car line))
			   (module-identifier=? (stx-car line) begin-stx))
		      (let ([line-list (stx->list line)])
			(unless line-list
			  (syntax-error #f expr 
					"improper `begin' clause form"
					line))
			(loop (append (cdr line-list) rest-pre-lines)
			      rest-lines
			      port
			      port-name
			      body
			      vars))]
		     [else
		      (loop rest-pre-lines
			    rest-lines
			    port
			    port-name
			    (cons line body)
			    vars)]))]))))))))
  
  (define-struct link (name sig expr links))
  (define-struct sig-explode-pair (sigpart exploded))

  (define parse-compound-unit
    (lambda (expr body)
      (syntax-case body ()
	[((import . imports)
	  (link . links)
	  (export . exports))
	 (and (literal? import) (literal? link) (literal? export))
	 (let* ([imports (parse-imports 'compound-unit/sig #f #t expr (syntax imports) #f)])
	   (let ([link-list (syntax->list (syntax links))])
	     (unless link-list
	       (syntax-error #f expr 
			     "improper `link' clause form"
			     (syntax links)))
	     (let* ([bad
		     (lambda (why sub)
		       (syntax-error #f expr 
				     (format "bad `link' element~a" why)
				     sub))]
		    [links
		     (map
		      (lambda (line)
			(syntax-case line ()
			  [(tag : sig (expr linkage ...))
			   (literal? :)
			   (begin
			     (unless (identifier? (syntax tag))
			       (bad ": link tag is not an identifier" line))
			     (make-link (syntax-e (syntax tag))
					(get-sig 'compound-unit/sig (syntax expr) #f (syntax sig) #f)
					(syntax expr)
					(syntax->list (syntax (linkage ...)))))]
			  [(tag . x)
			   (not (identifier? (syntax tag)))
			   (bad ": tag is not an identifier" (syntax tag))]
			  [(tag : sig (expr linkage ...) . rest)
			   (literal? :)
			   (bad ": extra expressions in sub-clause" line)]
			  [(tag : sig (expr . rest))
			   (literal? :)
			   (bad ": illegal use of `.' in linkages" line)]
			  [(tag : sig)
			   (literal? :)
			   (bad ": expected a unit expression and its linkages" line)]
			  [(tag : sig . e)
			   (literal? :)
			   (bad ": unit expression and its linkages not parenthesized" line)]
			  [(tag :)
			   (literal? :)
			   (bad ": expected a signature" line)]
			  [(tag)
			   (bad ": expected `:'" line)]
			  [_else
			   (bad "" line)]))
		      link-list)]
		    [in-sigs imports]
		    [find-link
		     (lambda (name links)
		       (let loop ([links links])
			 (cond
			  [(null? links) #f]
			  [(eq? name (link-name (car links)))
			   (car links)]
			  [else (loop (cdr links))])))]
		    [find-sig
		     (lambda (name sigs)
		       (let loop ([sigs sigs])
			 (cond
			  [(null? sigs) #f]
			  [(and (signature? (car sigs))
				(eq? name (signature-name (car sigs))))
			   (car sigs)]
			  [else (loop (cdr sigs))])))]
		    [flatten-path
		     (lambda (clause path var-k unit-k)
		       (letrec ([check-sig
				 (lambda (sig use-sig)
				   (when use-sig
				     (with-handlers ([exn:fail:unit?
						      (lambda (exn)
							(syntax-error 
							 #f expr
							 (exn-message exn)))])
				       (alt-verify-signature-match
					'compound-unit/sig #f
					(format "signature ~s" (signature-src use-sig))
					(explode-sig use-sig #f)
					(format "signature ~s" (signature-src sig))
					(explode-sig sig #f)))))]
				[flatten-subpath
				 (lambda (base last use-sig name sig p)
				   (cond
				    [(stx-null? p) 
				     (check-sig sig use-sig)
				     (unit-k base last name (if use-sig
								use-sig
								sig))]
				    [(or (not (stx-pair? p))
					 (not (identifier? (stx-car p))))
				     (syntax-error #f expr
						   path)]
				    [(memq (syntax-e (stx-car p)) (signature-vars sig))
				     (if (and (stx-null? (stx-cdr p)) (not use-sig))
					 (let* ([id-nopath (syntax-e (stx-car p))]
						[id (if name
							(string->symbol 
							 (string-append name
									":"
									(symbol->string id-nopath)))
							id-nopath)])
					   (var-k base id id-nopath))
					 (syntax-error #f expr
						       (format 
							"bad `~a' path: \"~a\" is a variable" 
							clause
							(syntax-e (stx-car p)))
						       path))]
				    [(find-sig (syntax-e (stx-car p)) (signature-elems sig))
				     =>
				     (lambda (s)
				       (flatten-subpath base
							(syntax-e (stx-car p))
							use-sig
							(let ([n (symbol->string 
								  (signature-name s))])
							  (if name
							      (string-append name ":" n)
							      n))
							s
							(stx-cdr p)))]
				    [else
				     (syntax-error #f expr
						   (format 
						    "bad `~a' path: \"~a\" not found"
						    clause
						    (syntax-e (stx-car p)))
						   path)]))])
			 (let-values ([(p use-sig)
				       (syntax-case path ()
					[_
					 (identifier? path)
					 (values (list path) #f)]
					[(name : sig)
					 (and (identifier? (syntax name))
					      (literal? :))
					 (values (list (syntax name))
						 (get-sig 'compound-unit/sig expr
							  #f
							  (syntax sig)
							  #f))]
					[((elem ...) : sig)
					 (and (andmap (lambda (s)
							(and (identifier? s)
							     (not (eq? (syntax-e s) ':))))
						      (syntax->list (syntax (elem ...))))
					      (literal? :))
					 (values (syntax (elem ...))
						 (get-sig 'compound-unit/sig expr
							  #f
							  (syntax sig)
							  #f))]
					[(elem1 elem ...)
					 (andmap (lambda (s)
						   (and (identifier? s)
							(not (eq? (syntax-e s) ':))))
						 (syntax->list (syntax (elem1 elem ...))))
					 (values path #f)]
					[else
					 (syntax-error #f expr
						       (format 
							"bad `~a' path"
							clause)
						       path)])])
			   (cond
			    [(find-link (syntax-e (stx-car p)) links)
			     => (lambda (link) 
				  (flatten-subpath (link-name link)
						   (syntax-e (stx-car p))
						   use-sig
						   #f
						   (link-sig link)
						   (stx-cdr p)))]
			    [(find-sig (syntax-e (stx-car p)) in-sigs)
			     => (lambda (sig) 
				  (let ([s (symbol->string (signature-name sig))])
				    (flatten-subpath #f
						     (syntax-e (stx-car p))
						     use-sig
						     s
						     sig
						     (stx-cdr p))))]
			    [else
			     (syntax-error #f expr
					   (format 
					    "bad `~a' path: \"~a\" not found"
					    clause
					    (syntax-e (stx-car p)))
					   path)]))))])
	       (check-unique (map link-name links)
			     (lambda (name)
			       (syntax-error #f expr
					     (format "duplicate sub-unit tag \"~s\"" name))))
	       (check-unique (map signature-name imports)
			     (lambda (name)
			       (syntax-error #f expr
					     (format "duplicate import identifier \"~s\"" name))))
	       (check-unique (append (map signature-name imports)
				     (map link-name links))
			     (lambda (name)
			       (syntax-error #f expr
					     (format 
					      "name \"~s\" is both import and sub-unit identifier" 
					      name))))
	       ;; Expand `link' clause using signatures
	       (for-each
		(lambda (link)
		  (set-link-links! 
		   link
		   (map
		    (lambda (link)
		      (flatten-path 'link link
				    (lambda (base var var-nopath)
				      (make-sig-explode-pair
				       var
				       (list
					(if base
					    (list base var)
					    var))))
				    (lambda (base last id sig)
				      (make-sig-explode-pair
				       (rename-signature sig last #f)
				       (if base
					   (list (cons base (flatten-signature id sig #f)))
					   (flatten-signature id sig #f))))))
		    (link-links link))))
		links)
	       (let ([export-list (syntax->list (syntax exports))])
		 (unless export-list
		   (syntax-error #f expr 
				 "improper `export' clause form"
				 (syntax exports)))
		 (let* ([upath? (lambda (p)
				  (or (identifier? p)
				      (and (stx-list? p)
					   (andmap identifier? (stx->list p)))))]
			[spath? (lambda (p)
				  (syntax-case p ()
				    [(name : sig)
				     (and (literal? :)
					  (upath? (syntax name))
					  (or (identifier? (syntax sig))
					      (parse-signature 'compound-unit/sig expr #f (syntax sig) #f)))
				     #t]
				    [_else
				     (upath? p)]))]
			[exports 
			 (map
			  (lambda (export)
			    (syntax-case export ()
			      [(open spath)
			       (literal? open)
			       (begin
				 (unless (spath? (syntax spath))
				   (syntax-error #f expr 
						 "bad `open' sub-clause of `export'"
						 export))
				 (flatten-path 'export
					       (syntax spath)
					       (lambda (base var var-nopath)
						 (syntax-error 
						  #f expr 
						  "`open' sub-clause path is a variable"
						  (car export)))
					       (lambda (base last name sig)
						 (if base
						     (make-sig-explode-pair
						      (signature-elems sig)
						      (cons base 
							    (map
							     list
							     (flatten-signature name sig #f)
							     (flatten-signature #f sig #f))))
						     (syntax-error 
						      #f expr 
						      "cannot export imported variables"
						      export)))))]
			      [(var (upath vname) . exname)
			       (literal? var)
			       (let ([upath (syntax upath)]
				     [vname (syntax vname)]
				     [exname (syntax exname)])
				 (unless (and (upath? upath)
					      (identifier? vname)
					      (or (stx-null? exname)
						  (and (stx-pair? exname)
						       (identifier? (stx-car exname))
						       (stx-null? (stx-cdr exname)))))
				   (syntax-error #f expr 
						 "bad `var' sub-clause of `export'"
						 export))
				 (flatten-path 'export
					       (if (identifier? upath)
						   (list upath vname)
						   (append (stx->list upath) (list vname)))
					       (lambda (base var var-nopath)
						 (if base
						     (make-sig-explode-pair
						      (list (if (stx-null? exname)
								var-nopath
								(syntax-e (stx-car exname))))
						      (list base
							    (if (stx-null? exname)
								(list var var-nopath)
								(list var (syntax-e (stx-car exname))))))
						     (syntax-error 
						      #f expr 
						      "cannot export imported variables"
						      export)))
					       (lambda (base last name var)
						 (syntax-error 
						  #f expr 
						  "`var' sub-clause path specifies a unit"
						  export))))]
			      [(unit spath . exname)
			       (literal? unit)
			       (let ([spath (syntax spath)]
				     [exname (syntax exname)])
				 (unless (and (spath? spath)
					      (or (stx-null? exname)
						  (and (stx-pair? exname)
						       (identifier? (stx-car exname))
						       (stx-null? (stx-cdr exname)))))
				   (syntax-error #f expr 
						 "bad `unit' sub-clause of `export'"
						 export))
				 (flatten-path 'export
					       spath
					       (lambda (base var var-nopath)
						 (syntax-error 
						  #f expr 
						  "`unit' sub-clause path is a variable"
						  export))
					       (lambda (base last name sig)
						 (if base
						     (make-sig-explode-pair
						      (list (rename-signature
							     sig
							     (if (stx-null? exname)
								 last
								 (syntax-e (stx-car exname)))
							     #f))
						      (let ([flat (flatten-signature name sig #f)])
							(cons base 
							      (map
							       list
							       flat
							       (flatten-signature 
								(symbol->string
								 (if (stx-null? exname)
								     last
								     (syntax-e (stx-car exname))))
								sig
								#f)))))
						     (syntax-error 
						      #f expr 
						      "cannot export imported variables"
						      export)))))]
			      [_else
			       (syntax-error #f expr 
					     (format 
					      "bad `export' sub-clause")
					     export)]))
			  export-list)]
			[interned-vectors (box null)])
		   (check-unique (map
				  (lambda (s)
				    (if (signature? s)
					(signature-name s)
					s))
				  (apply
				   append
				   (map sig-explode-pair-sigpart exports)))
				 (lambda (name)
				   (syntax-error #f expr
						 "name is exported twice"
						 name)))
		   (values (map link-name links)
			   (map link-expr links)
			   (map (lambda (link) (explode-sig (link-sig link) interned-vectors)) links)
			   (map
			    (lambda (link)
			      (map (lambda (sep)
				     (explode-named-sig (sig-explode-pair-sigpart sep) interned-vectors))
				   (link-links link)))
			    links)
			   (flatten-signatures imports #f)
			   (map (lambda (link)
				  (apply
				   append
				   (map 
				    sig-explode-pair-exploded 
				    (link-links link))))
				links)
			   (map sig-explode-pair-exploded exports)
			   (explode-named-sigs imports interned-vectors)
			   (explode-sig
			    (let ([elems (apply
					  append
					  (map sig-explode-pair-sigpart exports))])
			      (make-signature
			       'dummy
			       'dummy
			       elems
			       (map (lambda (x) #f) elems)
			       null))
			    interned-vectors)
			   interned-vectors))))))]
	[_else (raise-syntax-error 
		#f
		"bad syntax"
		expr)])))
  
  (define parse-invoke-vars
    (lambda (who rest expr)
      (parse-imports who #t #f expr rest #f)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (provide parse-unit
	   parse-compound-unit
	   parse-invoke-vars

	   parsed-unit-renames
	   parsed-unit-imports
	   parsed-unit-import-vars
	   parsed-unit-body
	   parsed-unit-stx-checks
	   parsed-unit-vars

	   make-struct-stx-decls
	   verify-struct-shape

	   signature-vars
	   signature-structs
	   do-rename
	   get-sig
	   explode-sig
	   explode-named-sigs
	   check-signature-unit-body
	   flatten-signature
	   flatten-signatures
	   struct-def-name))
