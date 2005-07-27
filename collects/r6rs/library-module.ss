
(module library-module mzscheme
  (require-for-syntax "private/helpers.ss"
		      (lib "kerncase.ss" "syntax")
		      (lib "context.ss" "syntax")
		      (lib "boundmap.ss" "syntax")
		      (lib "stxparam.ss")
		      (lib "list.ss"))
  (require (lib "stxparam.ss"))

  (provide (rename library-module-begin #%module-begin)
	   import)

  (define-syntax define-impdef-placeholder
    (syntax-rules ()
      [(_ id) (begin
		(define-syntax (id stx)
		  (raise-syntax-error
		   #f
		   "only allowed at the beginning of a `library' form"
		   stx))
		(provide id))]))

  (define-impdef-placeholder export)
  (define-impdef-placeholder indirect-export)

  (define-syntax (import stx)
    (unless (eq? (syntax-local-context) 'top-level)
      (raise-syntax-error
       #f
       "only allowed at the beginning of a `library' form or outside a library at the top level"
       stx))
    (syntax-case stx ()
      [(_ i ...)
       #`(begin #,@(map translate-import (syntax->list #'(i ...))))]))

  (define-for-syntax (split-bodies bodies)
    (let loop ([bodies bodies]
	       [imports null]
	       [exports null])
      (if (null? bodies)
	  (values (reverse imports)
		  (reverse exports)
		  null)
	  (syntax-case (car bodies) (import export)
	    [(import in ...)
	     (loop (cdr bodies)
		   (append (syntax->list #'(in ...)) imports)
		   exports)]
	    [(import . rest)
	     (raise-syntax-error #f "bad syntax" (car bodies))]
	    [(export out ...)
	     (loop (cdr bodies)
		   imports
		   (append (syntax->list #'(out ...)) exports))]
	    [(export . rest)
	     (raise-syntax-error #f "bad syntax" (car bodies))]
	    [else (values (reverse imports)
			  (reverse exports)
			  bodies)]))))

  (define-for-syntax (make-unboxer id in-src-module-id)
    (with-syntax ([id id])
      (make-set!-transformer
       (lambda (stx)
	 (syntax-case stx (set!)
	   [(set! _ v) #'(set-box! id v)]
	   [(_ arg ...) #'((unbox id) arg ...)]
	   [_ #'(unbox id)])))))

  (define-for-syntax (box-rhs stx)
    (syntax-case stx ()
      [(_ rhs) #'(box rhs)]))

  (define-for-syntax (make-protected-unboxer id  in-src-module-id)
    (with-syntax ([id id])
      (make-set!-transformer
       (lambda (stx)
	 (unless (syntax-parameter-value in-src-module-id)
	   (raise-syntax-error
	    #f
	    "reference to non-exported identifier allowed only within its source library"
	    stx))
	 (syntax-case stx (set!)
	   [(set! _ v) #'(set! id v)]
	   [(_ arg ...) #'(id arg ...)]
	   [_ #'id])))))

  (define-for-syntax (no-box-rhs stx)
    (syntax-case stx ()
      [(_ rhs) #'rhs]))

  (define-for-syntax (check-exported-macro f ok?)
    (let ([wrap (lambda (f)
		  (lambda (stx)
		    (unless (ok?)
		      (raise-syntax-error
		       #f
		       "reference to non-exported identifier allowed only within its source library"
		       stx))
		    (f stx)))])
      (cond
       [(and (procedure? f) (procedure-arity-includes? f 1))
	(wrap f)]
       [(set!-transformer? f)
	(make-set!-transformer (wrap (set!-transformer-procedure f)))]
       [else f])))

  (define-syntax (library-module-begin stx)
    (syntax-case stx ()
      [(_ (__ name lang body ...))
       (let ([stx (syntax-case stx () [(_ o) #'o])])
	 (unless (and (string? (syntax-e #'name))
		      (uri? (syntax-e #'name)))
	   (raise-syntax-error 
	    #f
	    "library name must be a URI"
	    stx
	    #'name))
	 (unless (and (string? (syntax-e #'lang))
		      (string=? "scheme://r6rs" (syntax-e #'lang)))
	   (raise-syntax-error 
	    #f
	    "language position must be \"scheme://r6rs\""
	    stx
	    #'lang))
	 (let ([bodies (syntax->list #'(body ...))])
	   (let-values ([(imports exports bodies)
			 (split-bodies bodies)])
	     (let ([provides (map translate-export exports)])
	       #`(#%plain-module-begin
		  (require #,(datum->syntax-object stx '(all-except (lib "r6rs.ss" "r6rs")
								    #%module-begin)))
		  (require-for-syntax #,(datum->syntax-object stx '(lib "r6rs.ss" "r6rs")))
		  (require #,(datum->syntax-object stx '(lib "library-module.ss" "r6rs")))
		  #,@(map translate-import imports)
		  #,@provides
		  (define-syntax-parameter in-src-module #f)
		  (begin-library-body
		   in-src-module
		   #,(apply append (map (lambda (prov)
					  (map (lambda (p)
						 (syntax-case p ()
						   [(_ loc ext) #'loc]
						   [_else p]))
					       (cdr (syntax->list prov))))
					provides))
		   ()
		   #,bodies
		   ()
		   ()))))))]
      [(_ x)
       (raise-syntax-error
	#f
	"bad syntax"
	#'x)]))

  (define-for-syntax stops (list*
			    #'import
			    #'export
			    #'indirect-export
			    (kernel-form-identifier-list #'here)))

  (define-syntax (begin-library-body stx)
    (syntax-case stx ()
      [(_ in-src-module export-info ((macro-id ind-id ...) ...) 
	  () ; no body forms left
	  ((def-macro-id check-id) ...)
	  ((id gen-id boxdef-id) ...))
       ;; We've processed the whole body, and now we need to
       ;;  create unboxers for the defined names:
       (let ([macro-ids (syntax->list #'(macro-id ...))]
	     [ind-idss (map syntax->list (syntax->list #'((ind-id ...) ...)))])
	 ;; Check that each inidirect-export id was defined
	 (let ([t (make-bound-identifier-mapping)])
	   (for-each (lambda (id)
		       (bound-identifier-mapping-put! t id #t))
		     (syntax->list #'(def-macro-id ...)))
	   (for-each (lambda (macro-id)
		       (unless (bound-identifier-mapping-get t macro-id (lambda () #f))
			 (raise-syntax-error
			  #f
			  "id to trigger indirect exports not defined as syntax in the library"
			  macro-id)))
		     macro-ids)
	   (for-each (lambda (id)
		       (bound-identifier-mapping-put! t id #t))
		     (syntax->list #'(id ...)))
	   (for-each (lambda (id)
		       (unless (bound-identifier-mapping-get t id (lambda () #f))
			 (raise-syntax-error
			  #f
			  "indirect export not defined in the library"
			  id)))
		     (apply append ind-idss)))
	 ;; Add each explicitly exported id to a table
	 (let ([t (make-bound-identifier-mapping)])
	   (for-each (lambda (id)
		       (bound-identifier-mapping-put! t id #t))
		     (syntax->list #'export-info))
	   ;; Find fixpoint, adding indirect ids when the macro id is
	   ;;  exported:
	   (let loop ([macro-ids macro-ids]
		      [ind-idss ind-idss]
		      [next-macro-ids null]
		      [next-ind-idss null]
		      [added? #f])
	     (cond
	      [(null? macro-ids)
	       (when added?
		 (loop next-macro-ids next-ind-idss null null #f))]
	      [(bound-identifier-mapping-get t (car macro-ids) (lambda () #f))
	       (for-each (lambda (ind-id)
			   (bound-identifier-mapping-put! t ind-id #t))
			 (car ind-idss))
	       (loop (cdr macro-ids) (cdr ind-idss) next-macro-ids next-ind-idss #t)]
	      [else
	       (loop (cdr macro-ids) (cdr ind-idss) 
		     (cons (car macro-ids) next-macro-ids)
		     (cons (car ind-idss) next-ind-idss)
		     added?)]))
	   ;; For each defined id, select an unboxer:
	   (with-syntax ([((make-an-unboxer . box-a-def) ...)
			  (map (lambda (id)
				 (if (bound-identifier-mapping-get t id (lambda () #f))
				     #'(make-unboxer . box-rhs)
				     #'(make-protected-unboxer . no-box-rhs)))
			       (syntax->list #'(id ...)))])
	     ;; For each unexported macro id, add compile-time set!:
	     (with-syntax ([(check-id ...)
			    (map cdr (filter (lambda (p)
					       (not (bound-identifier-mapping-get t (car p) (lambda () #f))))
					     (map cons 
						  (syntax->list #'(def-macro-id ...))
						  (syntax->list #'(check-id ...)))))])
	       #'(begin
		   (begin-for-syntax (set! check-id #f) ...)
		   (define-syntaxes (boxdef-id) box-a-def) ...
		   (define-syntaxes (id ...)
		     (values (make-an-unboxer (quote-syntax gen-id) (quote-syntax in-src-module)) ...)))))))]
      [(_ in-src-module export-info indirects (body0 body ...) define-macro-ids defined-ids)
       ;; Process one body form, body0
       (let ([comdef (local-expand #'body0
				   'module
				   stops)])
	 (syntax-case comdef (begin define-syntaxes define-values indirect-export)
	   [(begin comdef ...)
	    #`(begin-library-body in-src-module
				  export-info 
				  indirects
				  (comdef ... body ...)
				  define-macro-ids
				  defined-ids)]
	   [(define-syntaxes (id ...) rhs)
	    (with-syntax ([(check-id ...) (generate-temporaries #'(id ...))])
	      #`(begin (define-for-syntax check-id #t) ...
		       (define-syntaxes (id ...)
			 (let-values ([(id ...) rhs])
			   (values (check-exported-macro id (lambda () check-id)) ...)))
		       (begin-library-body in-src-module
					   export-info 
					   indirects
					   (body ...)
					   ((id check-id) ... . define-macro-ids)
					   defined-ids)))]
	   [(define-values (id ...) rhs)
	    (with-syntax ([(gen-id ...) (generate-temporaries #'(id ...))]
			  [(boxdef-id ...) (generate-temporaries #'(id ...))])
	      #`(begin 
		  (define-values (gen-id ...) 
		    (syntax-parameterize ([in-src-module #t])
		      (let-values ([(id ...) rhs])
			(values (boxdef-id id) ...))))
		  (begin-library-body in-src-module
				      export-info 
				      indirects
				      (body ...)
				      define-macro-ids
				      ((id gen-id boxdef-id) ... . defined-ids))))]
	   [(indirect-export (macro-id id ...) ...)
	    (begin
	      (for-each (lambda (x)
			  (unless (identifier? x)
			    (raise-syntax-error
			     #f
			     "expected an identifier"
			     comdef
			     x)))
			(syntax->list #'(macro-id ... id ... ...)))
	      #`(begin-library-body in-src-module
				    export-info 
				    ((macro-id id ...) ... . indirects)
				    (body ...)
				    define-macro-ids
				    defined-ids))]
	   [(indirect-export . _)
	    (raise-syntax-error
	     #f
	     "bad syntax"
	     comdef)]
	   [expr
	    ;; syntax-parameterize forces an expression (not defn):
	    #`(begin
		(syntax-parameterize ([in-src-module #t]) 
		  expr)
		(begin-library-body in-src-module
				    export-info 
				    indirects
				    (body ...)
				    define-macro-ids
				    defined-ids))]))])))
