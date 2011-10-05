
;; by Jacob Matthews (and others)

(module struct mzscheme
  (provide copy-struct 
	   define-struct/properties
	   make-->vector)
  (require-for-syntax syntax/struct
		      (only racket/base filter)
                      syntax/stx)

  ;; ------------------------------------------------------------
  ;; copy-struct

  ;; copy-struct expands to `do-copy-struct' to delay the expansion
  ;;  in an internal-definition context. (The `begin0' wrapper
  ;;  effectively declares the form to be an expression.)
  (define-syntax (copy-struct stx)
    (syntax-case stx ()
      [frm #'(begin0 (do-copy-struct frm))]))
  
  (define-syntax (do-copy-struct dstx)
    (syntax-case dstx ()
      [(_ stx)
       (let ([stx #'stx])
	 (syntax-case stx ()
	   [(_ info structure (accessor-name new-val) ...)
	    (let ([ans (syntax->list  #'((accessor-name new-val) ...))])
	      (unless (identifier? #'info)
		(raise-syntax-error #f "not an identifier for structure type" stx #'info))
	      (for-each (lambda (an)
			  (unless (identifier? (stx-car an))
			    (raise-syntax-error #f "not an identifier for accessor name" stx (stx-car an))))
			ans)
	      
	      ;; new-binding-for : syntax[field-name] -> (union syntax[expression] #f)
	      (let ((new-binding-for 
		     (lambda (f)
		       (ormap (lambda (x) 
				(if (module-or-top-identifier=? (stx-car x) f)
				    (cadr (syntax-e x)) 
				    #f))
			      ans))))
		
		(let-values ([(construct pred accessors)
			      (let ([v (syntax-local-value #'info (lambda () #f))])
				(unless (struct-declaration-info? v)
				  (raise-syntax-error #f "identifier is not bound to a structure type" stx #'info))
                                (let ([v (extract-struct-info v)])
                                  (values (cadr v)
                                          (caddr v)
                                          (cadddr v))))]
			     [(as) (map (lambda (an) (stx-car an)) ans)])
		  (let ([dests
			 (map
			  (lambda (field)
			    (or (ormap (lambda (f2) (and f2 (module-or-top-identifier=? field f2) f2)) accessors)
				(raise-syntax-error #f "accessor name not associated with the given structure type" stx field)))
			  as)])
		    ;; Check for duplicates using dests, not as, because mod=? as might not be id=?
		    (let ((dupe (check-duplicate-identifier dests)))
		      (when dupe 
			(raise-syntax-error #f 
					    "duplicate field assignment" 
					    stx 
					    ;; Map back to an original field:
					    (ormap (lambda (a)
						     (and a
							  (module-or-top-identifier=? dupe a)
							  a))
						   (reverse as))))))
		  
		  ;; the actual result
		  #`(let ((the-struct structure))
		      (if (#,pred the-struct)
			  (#,construct
			   #,@(map 
			       (lambda (field) (or (new-binding-for field) #`(#,field the-struct)))
			       (reverse accessors)))
			  (raise-type-error '_  #,(format "struct:~a" (syntax-object->datum #'info)) the-struct))))))]))]))

  ;; --------------------------------------------------
  ;; define-struct/properties
  
  ;; Used at run time:
  (define (check-prop v)
    (unless (struct-type-property? v)
      (raise-type-error
       'define-struct/properties
       "struct-type property"
       v))
    v)
    
  ;; This compile-time proc fills in the crucial part of the expansion.
  ;; Because it's called through a syntax trampoline, the arguments have to
  ;; to be packaged as a syntax object.
  (define-for-syntax (make-make-make-struct-type props+insp-stx)
    (with-syntax ([(([prop expr] ...) inspector) props+insp-stx])
      (lambda (orig-stx name-stx defined-name-stxes super-info)
	#`(make-struct-type '#,name-stx 
			    #,(and super-info (list-ref super-info 0))
			    #,(/ (- (length defined-name-stxes) 3) 2)
			    0 #f
			    (list
			     (cons (check-prop prop)
				   expr)
			     ...)
			    inspector))))
  
  ;; The main macro:
  (define-syntax (define-struct/properties stx)    

    ;; Start paring. Exploit `parse-define-struct' as much as possible.
    (define (parse-at-main)
      (syntax-case stx ()
	[(_ id/sup fields . rest)
	 ;; Check initial part:
	 (let-values ([(id sup-id fields _) 
		       (parse-define-struct #`(_ id/sup fields) stx)])
	   (parse-at-props id sup-id fields #'rest))]
	[_
	 ;; Not even right up to define-struct, so let the
	 ;;  simple parser report the problem:
	 (parse-define-struct stx stx)]))
    
    ;; So far, so good. Parse props.
    (define (parse-at-props id sup-id fields rest)
      (syntax-case rest ()
	[(([prop expr] ...) . rrest)
	 (parse-at-inspector id sup-id fields (stx-car rest) (stx-cdr rest))]
	[((bad ...) . rest)
	 (for-each (lambda (bad)
		     (syntax-case bad ()
		       [(a b) 'ok]
		       [_ (raise-syntax-error
			   #f
			   "expected a parenthesized property--value pairing"
			   stx
			   bad)]))
		   (syntax->list #'(bad ...)))]
	[(bad . rest)
	 (raise-syntax-error
	  #f
	  "expected a parenthesized sequence of property--value pairings"
	  stx
	  #'bad)]
	[_
	 (raise-syntax-error
	  #f
	  "expected a parenthesized sequence of property--value pairings after fields"
	  stx)]))

    ;; Finally, parse optional inspector expr, again exploiting
    ;;  `parse-define-struct'.
    (define (parse-at-inspector id sup-id fields props rest)
      (let-values ([(_ __ ___ inspector-stx)
		    (parse-define-struct #`(ds id () #,@rest) stx)])
	(build-result id sup-id fields props inspector-stx)))

    ;; Build the result using `generate-struct-declaration', which 
    ;;  sometimes needs the `continue-ds/p' trampoline to eventually get
    ;;  to make-make-make-struct-type.
    (define (build-result id sup-id fields props inspector)
      (let ([props+insp #`(#,props #,inspector)])
	(generate-struct-declaration stx
				     id sup-id fields
				     (syntax-local-context)
				     (make-make-make-struct-type props+insp))))
    
    (parse-at-main))

  ;; ------------------------------------------------------------
  ;; make->vector
  
  (define-syntax (make-->vector stx)
    (syntax-case stx ()
      [(_ name) ; a struct type name
       (identifier? (syntax name))
       (let ([info (syntax-local-value (syntax name))])
         (if (struct-declaration-info? info)
             (with-syntax ([(accessor ...)
                            (reverse
                             (filter identifier? (list-ref (extract-struct-info info) 3)))])
               (syntax
                (λ (s)
                  (vector (accessor s) ...))))
             (raise-syntax-error
              #f
              "not a declared structure type name"
              stx
              (syntax name))))])))
