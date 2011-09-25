
;; This implementation of `unit/sig' was ported from the old v100
;; implementation, and then hacked a bit to produce more compact
;; output, and finally mangled to handle the v200 `struct' (with
;; compile-time information). It's in dire need of an overhaul.

(module unitsig200 mzscheme
  (require "unit200.rkt")
  (require "private/sigmatch.rkt")

  (require-for-syntax "private/sigutil.rkt")
  (require-for-syntax syntax/kerncase)

  (define-struct signed-unit (unit imports exports))

  (define-syntax define-signature
    (lambda (expr)
      (syntax-case expr ()
	[(_ name sig)
	 (identifier? (syntax name))
	 (let ([sig (get-sig 'define-signature expr (syntax-e (syntax name))
			     (syntax sig) #f)])
	   (with-syntax ([content (explode-sig sig #f)])
	     (syntax (define-syntax name
		       (make-sig (quote content))))))])))

  (define-syntax let-signature
    (lambda (expr)
      (syntax-case expr ()
	[(_ name sig . body)
	 (identifier? (syntax name))
	 (let ([sig (get-sig 'let-signature expr (syntax-e (syntax name))
			     (syntax sig) #f)])
	   (with-syntax ([content (explode-sig sig #f)])
	     (syntax (letrec-syntax ([name (make-sig (quote content))])
		       . body))))])))
  
  (define-syntax unit/sig
    (lambda (expr)
      (syntax-case expr ()
	[(_ sig . rest)
	 (let ([sig (get-sig 'unit/sig expr #f (syntax sig) #f)])
	  (let ([a-unit (parse-unit expr (syntax rest) sig
				    (kernel-form-identifier-list)
				    (quote-syntax define-values)
				    (quote-syntax define-syntaxes)
				    (quote-syntax begin))])
	    (check-signature-unit-body sig a-unit (parsed-unit-renames a-unit) 'unit/sig expr)
	    (with-syntax ([imports (parsed-unit-import-vars a-unit)]
			  [exports (datum->syntax-object
				    expr
				    (let ([vars (make-hash-table)])
				      (for-each (lambda (var)
						  (hash-table-put! vars (syntax-e var) var))
						(parsed-unit-vars a-unit))
				      (map
				       (lambda (name)
					 (list (let ([name (do-rename name (parsed-unit-renames a-unit))])
						 (hash-table-get vars name name))
					       name))
				       (signature-vars sig)))
				    expr)]
			  [body (append
				 (reverse (parsed-unit-body a-unit))
				 ((parsed-unit-stx-checks a-unit) expr))]
			  [import-sigs (explode-named-sigs (parsed-unit-imports a-unit) #f)]
			  [export-sig (explode-sig sig #f)])
	      (syntax/loc expr
		(make-signed-unit
		 (unit/no-expand
		   (import . imports)
		   (export . exports)
		   . body)
		 (quote import-sigs)
		 (quote export-sig))))))])))

  (define-syntax compound-unit/sig
    (lambda (expr)
      (syntax-case expr ()
	[(_ . body)
	 (let-values ([(tags
			exprs
			exploded-link-imports
			exploded-link-exports
			flat-imports
			link-imports
			flat-exports
			exploded-imports
			exploded-exports
			boxed-interned-symbol-vectors)
		       (parse-compound-unit expr (syntax body))]
		      [(t) (lambda (l) (datum->syntax-object expr l expr))])
	   (with-syntax ([(tag ...) (t tags)]
			 [(uexpr ...) (t exprs)]
			 [(tagx ...) (t (map (lambda (t) (string->symbol (format "u:~a" t))) tags))]
			 [exploded-link-imports (t exploded-link-imports)]
			 [exploded-link-exports (t exploded-link-exports)]
			 [flat-imports (t flat-imports)]
			 [(link-import ...) (t link-imports)]
			 [flat-exports (t flat-exports)]
			 [exploded-imports (t exploded-imports)]
			 [exploded-exports (t exploded-exports)]
			 [interned-vectors (t (map (lambda (x) `(,(car x) (quote ,(cadr x))))
						   (unbox boxed-interned-symbol-vectors)))])
	     (syntax/loc
	      expr
	      (let ([tagx uexpr] ... . interned-vectors)
		(alt-verify-linkage-signature-match
		 'compound-unit/sig
		 '(tag ...)
		 (list tagx ...)
		 `exploded-link-imports
		 `exploded-link-exports)
		;; All checks done. Make the unit:
		(make-signed-unit
		 (compound-unit
		  (import . flat-imports)
		  (link [tag ((signed-unit-unit tagx)
			      . link-import)]
			...)
		  (export . flat-exports))
		 `exploded-imports
		 `exploded-exports)))))])))

  (define-syntax invoke-unit/sig
    (lambda (expr)
      (syntax-case expr ()
	[(_ u sig ...)
	 (let ([sigs (parse-invoke-vars 'invoke-unit/sig (syntax (sig ...)) expr)])
	   (with-syntax ([exploded-sigs (datum->syntax-object
					 expr
					 (explode-named-sigs sigs #f)
					 expr)]
			 [flat-sigs (datum->syntax-object
				     expr
				     (flatten-signatures sigs #f) 
				     expr)])
	     (syntax/loc
	      expr
	      (let ([unt u])
		(alt-verify-linkage-signature-match
		 (quote invoke-unit/sig)
		 (quote (invoke))
		 (list unt)
		 (quote ((#() . #())))
		 (quote (exploded-sigs)))
		(invoke-unit (signed-unit-unit unt)
			     . flat-sigs)))))])))
  
  (define-syntax unit->unit/sig
    (lambda (expr)
      (syntax-case expr ()
	[(_ e (im-sig ...) ex-sig)
	 (let ([im-sigs (map (lambda (sig)
			       (get-sig 'unit->unit/sig expr #f sig #f))
			     (syntax->list (syntax (im-sig ...))))]
	       [ex-sig (get-sig 'unit->unit/sig expr #f (syntax ex-sig) #f)])
	   (with-syntax ([exploded-imports (datum->syntax-object
					    expr
					    (explode-named-sigs im-sigs #f)
					    expr)]
			 [exploded-exports (datum->syntax-object
					    expr
					    (explode-sig ex-sig #f)
					    expr)])
	     (syntax
	      (make-signed-unit
	       e
	       (quote exploded-imports)
	      (quote exploded-exports)))))])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define -verify-linkage-signature-match
    (let ([make-exn make-exn:fail:unit]
	  [p-suffix (lambda (pos) (case pos [(1) 'st][(2) 'nd][(3) 'rd][else 'th]))])
      (lambda (who tags units esigs isigs wrapped? unwrap)
	(for-each
	 (lambda (u tag)
	   (unless (signed-unit? u)
	     (raise
	      (make-exn
	       (format "~s: expression for \"~s\" is not a signed unit: ~e"
                       who tag u)
	       (current-continuation-marks)))))
	 units tags)
	(for-each
	 (lambda (u tag esig)
	   (-verify-signature-match
	    who #f
	    (format "specified export signature for ~a" tag)
	    esig
	    (format "export signature for actual ~a sub-unit" tag)
	    (signed-unit-exports u)
	    wrapped? unwrap))
	 units tags esigs)
	(for-each
	 (lambda (u tag isig)
	   (let ([n (length (signed-unit-imports u))]
		 [c (length isig)])
	     (unless (= c n)
	       (raise
		(make-exn
		 (format
                  "~s: ~a unit imports ~a units, but ~a units were provided"
                  who tag n c)
		 (current-continuation-marks))))))
	 units tags isigs)
	(for-each
	 (lambda (u tag isig)
	   (let loop ([isig isig][expecteds (signed-unit-imports u)][pos 1])
	     (unless (null? isig)
	       (let ([expected (car expecteds)]
		     [provided (car isig)])
		 (-verify-signature-match
		  who #t
		  (format "~a unit's ~s~s import (which is ~a)" tag
			  pos (p-suffix pos)
			  (car expected))
		  (cdr expected)
		  (format "~a's ~s~s linkage (which is ~a)"
			  tag
			  pos (p-suffix pos)
			  (car provided))
		  (cdr provided)
		  wrapped? unwrap)
		 (loop (cdr isig) (cdr expecteds) (add1 pos))))))
	 units tags isigs))))

  (define verify-linkage-signature-match
    (lambda (who tags units esigs isigs)
      (-verify-linkage-signature-match who tags units esigs isigs values values)))

  (define alt-verify-linkage-signature-match
    (lambda (who tags units esigs isigs)
      (-verify-linkage-signature-match who tags units esigs isigs pair? car)))

  (define-syntax signature->symbols
    (lambda (stx)
      (syntax-case stx ()
	[(_ name)
	 (identifier? (syntax name))
	 (let ([sig (get-sig 'signature->symbols stx #f (syntax name) #f)])
	   (with-syntax ([e (let cleanup ([p (explode-sig sig #f)])
			      ;; Strip struct info:
			      (list->vector 
			       (map (lambda (i)
				      (if (symbol? i)
					  i
					  (cons (car i) (cleanup (cdr i)))))
				    (vector->list (car p)))))])
	     (syntax 'e)))])))

  ;; Internal:
  (define-syntax do-define-values/invoke-unit/sig
    (lambda (stx)
      (syntax-case stx ()
	[(_ global? signame unite prefix imports orig)
	 (let* ([formname (if (syntax-e (syntax global?))
			      'namespace-variable-bind/invoke-unit/sig
			      'define-values/invoke-unit/sig)]
		[badsyntax (lambda (s why)
			     (raise-syntax-error
			      #f
			      (format "bad syntax (~a)" why)
			      (syntax orig)
			      s))])
	   (unless (or (not (syntax-e (syntax prefix)))
		       (identifier? (syntax prefix)))
	     (badsyntax (syntax prefix) "prefix is not an identifier"))
	   (let ([ex-sig (get-sig formname (syntax orig) #f (syntax signame) (syntax signame))])
	     (let ([ex-exploded (explode-sig ex-sig #f)]
		   [ex-flattened (flatten-signature #f ex-sig #'signame)])
	       (let ([im-sigs
		      (parse-invoke-vars formname (syntax imports) (syntax orig))])
		 (let ([im-explodeds (explode-named-sigs im-sigs #f)]
		       [im-flattened (flatten-signatures im-sigs #f)]
		       [d->s (lambda (x) (datum->syntax-object (syntax orig) x (syntax orig)))])
		   (with-syntax ([dv/iu (if (syntax-e (syntax global?))
					    (quote-syntax namespace-variable-bind/invoke-unit)
					    (quote-syntax define-values/invoke-unit))]
				 [ex-flattened ex-flattened]
				 [ex-exploded (d->s ex-exploded)]
				 [im-explodeds (d->s im-explodeds)]
				 [im-flattened (d->s im-flattened)]
				 [formname formname]
				 [stx-decls (if (syntax-e (syntax global?))
						null
						(make-struct-stx-decls ex-sig #f #f (syntax signame) #f))])
		     (syntax/loc stx
		       (begin
			 (dv/iu
			  ex-flattened
			  (let ([unit-var unite])
			    (alt-verify-linkage-signature-match
			     'formname
			     '(invoke)
			     (list unit-var)
			     '(ex-exploded)
			     '(im-explodeds))
			    (signed-unit-unit unit-var))
			  prefix
			  . im-flattened)
			 . stx-decls))))))))])))
  
  (define-syntax define-values/invoke-unit/sig
    (lambda (stx)
      (with-syntax ([orig stx])
	(syntax-case stx ()
	  [(_ signame unit prefix . imports)
	   (syntax (do-define-values/invoke-unit/sig #f signame unit prefix imports orig))]
	  [(_ signame unit)
	   (syntax (do-define-values/invoke-unit/sig #f signame unit #f () orig))]))))

  (define-syntax namespace-variable-bind/invoke-unit/sig
    (lambda (stx)
      (with-syntax ([orig stx])
	(syntax-case stx ()
	  [(_ signame unit prefix . imports)
	   (syntax (do-define-values/invoke-unit/sig #t signame unit prefix imports orig))]
	  [(_ signame unit)
	   (syntax (do-define-values/invoke-unit/sig #t signame unit #f () orig))]))))

  (define-syntax provide-signature-elements
    (lambda (stx)
      (with-syntax ([orig stx])
	(syntax-case stx ()
	  [(_ signame)
	   (let ([sig (get-sig 'provide-signature-elements stx #f (syntax signame) (syntax signame))])
	     (let ([flattened (flatten-signature #f sig (syntax signame))]
		   [structs (map struct-def-name (signature-structs sig))])
	       (with-syntax ([flattened (map (lambda (x) (datum->syntax-object (syntax signame) x #f))
					     (append flattened structs))])
		 (syntax/loc stx
		   (provide . flattened)))))]))))
  
  (define (unit/sig? x) (signed-unit? x))
  (define (unit/sig->unit x) (signed-unit-unit x))

  (provide define-signature
	   let-signature
	   unit/sig
	   compound-unit/sig
	   invoke-unit/sig
	   unit->unit/sig
	   signature->symbols
	   verify-signature-match
	   verify-linkage-signature-match

	   (struct signed-unit (unit imports exports))
	   unit/sig? unit/sig->unit
	   
	   define-values/invoke-unit/sig
	   namespace-variable-bind/invoke-unit/sig
	   provide-signature-elements))
