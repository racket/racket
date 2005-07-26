
(module library-module mzscheme
  (require-for-syntax "private/helpers.ss"
		      (lib "kerncase.ss" "syntax")
		      (lib "context.ss" "syntax"))

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
	       [exports null]
	       [indirect-exports null])
      (if (null? bodies)
	  (values (reverse imports)
		  (reverse exports)
		  (reverse indirect-exports)
		  null)
	  (syntax-case (car bodies) (import export indirect-export)
	    [(import in ...)
	     (loop (cdr bodies)
		   (append (syntax->list #'(in ...)) imports)
		   exports
		   indirect-exports)]
	    [(import . rest)
	     (raise-syntax-error #f "bad syntax" (car bodies))]
	    [(export out ...)
	     (loop (cdr bodies)
		   imports
		   (append (syntax->list #'(out ...)) exports)
		   indirect-exports)]
	    [(export . rest)
	     (raise-syntax-error #f "bad syntax" (car bodies))]
	    [(indirect-export indirect ...)
	     (loop (cdr bodies)
		   imports
		   exports
		   (append (syntax->list #'(indirect ...)) indirect-exports))]
	    [(indirect-export . rest)
	     (raise-syntax-error #f "bad syntax" (car bodies))]
	    [else (values (reverse imports)
			  (reverse exports)
			  (reverse indirect-exports)
			  bodies)]))))

  (define-for-syntax (make-unboxer id)
    (with-syntax ([id id])
      (make-set!-transformer
       (lambda (stx)
	 (syntax-case stx (set!)
	   [(set! _ v) #'(set-box! id v)]
	   [(_ arg ...) #'((unbox id) arg ...)]
	   [_ #'(unbox id)])))))

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
	   (let-values ([(imports exports indirect-exports bodies)
			 (split-bodies bodies)])
	     #`(#%plain-module-begin
		(require #,(datum->syntax-object stx '(all-except (lib "r6rs.ss" "r6rs")
								  #%module-begin)))
		(require-for-syntax #,(datum->syntax-object stx '(lib "r6rs.ss" "r6rs")))
		(require #,(datum->syntax-object stx '(lib "library-module.ss" "r6rs")))
		#,@(map translate-import imports)
		#,@(map translate-export exports)
		(begin-library-body
		 #,indirect-exports
		 #,bodies)))))]
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
      [(_ indirects ())
       #'(begin)]
      [(_ indirects (body0 body ...))
       (let ([comdef (local-expand #'body0
				   'module
				   stops)])
	 (syntax-case comdef (begin define-syntaxes define-values)
	   [(begin comdef ...)
	    #`(begin-library-body indirects (comdef ... body ...))]
	   [(define-syntaxes (id ...) rhs)
	    #`(begin (define-syntaxes (id ...) rhs)
		     (begin-library-body indirects (body ...)))]
	   [(define-values (id ...) rhs)
	    (with-syntax ([(gen-id ...) (generate-temporaries #'(id ...))])
	      #`(begin 
		  (define-values (gen-id ...) 
		    (let-values ([(id ...) rhs])
		      (values (box id) ...)))
		  (define-syntaxes (id ...)
		    (values (make-unboxer (quote-syntax gen-id)) ...))
		  (begin-library-body indirects (body ...))))]
	   [expr
	    ;; begin0 forces an expression (not defn):
	    #`(begin
		(begin0 expr)
		(begin-library-body indirects (body ...)))]))])))
