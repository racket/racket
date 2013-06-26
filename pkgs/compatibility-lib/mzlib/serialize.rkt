(module serialize mzscheme
  (require-for-syntax syntax/struct)
  (require ;; core [de]serializer:
           racket/private/serialize)

  (provide define-serializable-struct
	   define-serializable-struct/versions

           ;; core [de]serializer:
           (all-from racket/private/serialize))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; define-serializable-struct
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-values (prop:internal-deserialize internal-deserialize? internal-deserialize-info)
    (make-struct-type-property 'internal-deserialize #f))

  ;; generate-struct-declaration wants a function to generate the actual
  ;;  call to `make-struct-type'. This is where we insert the serializable property.
  (define-for-syntax (make-make-make-struct-type inspector+deserializer-stx)
    (let-values ([(inspector-stx deserialize-id)
		  (apply values (syntax->list inspector+deserializer-stx))])
      (lambda (orig-stx name-stx defined-name-stxes super-info)
	(when super-info
	  (unless (andmap values (list-ref super-info 3))
	    (raise-syntax-error
	     #f
	     "not all fields are known for parent struct type"
	     orig-stx
	     (syntax-case orig-stx ()
	       [(_ (__ super-id) . rest) #'super-id]))))
	(let ([num-fields (/ (- (length defined-name-stxes) 3) 2)])
	  #`(letrec-values ([(type maker pred access mutate)
			     (make-struct-type '#,name-stx 
					       #,(and super-info (list-ref super-info 0))
					       #,num-fields
					       0 #f
					       (list
						;; --- The prop:serializable property means this is serializable
						(cons
						 prop:serializable
						 (make-serialize-info
						  ;; The struct-to-vector function: --------------------
						  (lambda (v)
						    (vector
						     #,@(if super-info
							    (reverse
							     (map (lambda (sel)
								    #`(#,sel v))
								  (list-ref super-info 3)))
							    null)
						     #,@(let loop ([n num-fields][r null])
							  (if (zero? n)
							      r
							      (loop (sub1 n)
								    (cons
								     #`(access v #,(sub1 n))
								     r))))))
						  ;; The serializer id: --------------------
						  (quote-syntax #,deserialize-id)
						  ;; Alternate --- does the same thing, due to the way
						  ;;  this macro is set up:
						  #;
						  (let ([b (identifier-binding (quote-syntax #,deserialize-id))])
						    (if (list? b)
							(cons '#,deserialize-id (caddr b))
							'#,deserialize-id))
						  ;; Can handle cycles? --------------------
						  ;;  Yes, as long as we have mutators for the
						  ;;  superclass.
						  #,(or (not super-info)
							(andmap values (list-ref super-info 4)))
						  ;; Directory for last-ditch resolution --------------------
						  (or (current-load-relative-directory) 
						      (current-directory))))
						;; --- The prop:internal-deserialize property just communicates
						;;     information to the deserialize binding (because it's more
						;;     convenient to generate the deserialize info here)
						(cons
						 prop:internal-deserialize
						 (list
						  ;; The maker-getter: --------------------
						  (lambda () maker)
						  ;; The shell function: --------------------
						  ;;  Returns an shell object plus
						  ;;  a function to update the shell (used for
						  ;;  building cycles): 
						  (let ([super-sets
							 (list #,@(if super-info
								      (list-ref super-info 4)
								      null))])
						    (lambda ()
						      (let ([s0
							     (#,(list-ref defined-name-stxes 1)
							      #,@(append
								  (if super-info
								      (map (lambda (x) #f)
									   (list-ref super-info 3))
								      null)
								  (vector->list
								   (make-vector num-fields #f))))])
							(values
							 s0
							 (lambda (s)
							   #,@(if super-info
								  (map (lambda (set get)
									 #`(#,set s0 (#,get s)))
								       (list-ref super-info 4)
								       (list-ref super-info 3))
								  null)
							   #,@(let loop ([n num-fields])
								(if (zero? n)
								    null
								    (let ([n (sub1 n)])
								      (cons #`(mutate s0 #,n (access s #,n))
									    (loop n)))))
							   (void)))))))))
					       #,inspector-stx)])
	      (values type maker pred access mutate))))))
    
  (define-syntaxes (define-serializable-struct define-serializable-struct/versions)
    (let ()
      (define expected-ids
	"expected an identifier or parenthesized sequence of struct identifier and parent identifier")
      (define expected-fields
	"expected parenthesized sequence of field identifiers")
      (define expected-version
	"expected a version (literal, exact, non-negative integer)")
      (define (after-name id/sup-stx)
	(if (identifier? id/sup-stx)
	    " after struct identifier"
	    " after sequence of struct and parent identifiers"))
      (define version-conflicts
	"version number for other-version deserializer conflicts with")
      
      (define (context-check stx)
	(unless (memq (syntax-local-context) '(top-level module))
	  (raise-syntax-error
	   #f
	   "allowed only at the top level or within a module top level"
	   stx)))

      (define (ok-version? v)
	(and (number? v)
	     (integer? v)
	     (v . >= . 0)
	     (exact? v)))

      ;; ------------------------------
      ;;  Main parsing
      
      ;; Check high-level syntax, then dispatch to parsing parts
      (define main
	(lambda (stx)
	  (syntax-case stx ()
	    [(_ id/sup fields)
	     (parse stx #'id/sup #'0 #'fields #'() #'(current-inspector))]
	    [(_ id/sup fields inspector-expr)
	     (parse stx #'id/sup #'0 #'fields #'() #'inspector-expr)]
	    [(_ id/sup)
	     (parse stx #'id/sup #'0 #f #f #f)]
	    [(_)
	     (raise-syntax-error
	      #f
	      expected-ids
	      stx)])))

      ;; Check high-level syntax with versions, then dispatch to parsing parts
      (define main/versions
	(lambda (stx)
	  (syntax-case stx ()
	    [(_ id/sup version-num fields other-versions)
	     (parse stx #'id/sup #'version-num #'fields #'other-versions #'(current-inspector))]
	    [(_ id/sup version-num fields other-versions inspector-expr)
	     (parse stx #'id/sup #'version-num #'fields #'other-versions #'inspector-expr)]
	    [(_ id/sup)
	     (parse stx #'id/sup #f #f #f #f)]
	    [(_ id/sup version-num)
	     (parse stx #'id/sup #'version-num #f #f #f)]
	    [(_ id/sup version-num fields)
	     (parse stx #'id/sup #'version-num #'fields #f #f)]
	    [(_)
	     (raise-syntax-error
	      #f
	      expected-ids
	      stx)])))
      
      ;; ------------------------------
      ;;  Part parsing

      ;; Parse parts, then dispatch to result generation
      (define (parse stx id/sup-stx version-num-stx fields-stx other-versions-stx inspector-stx)
	;; First, check id or id+super:
	(let-values ([(id super-id) (parse-id+super stx id/sup-stx)])
	  (let* ([version-num (parse-version stx id/sup-stx version-num-stx)]
		 [field-ids (parse-fields stx id/sup-stx version-num-stx fields-stx)]
		 [other-versions (parse-other-versions stx version-num other-versions-stx)])
	    ;; Input syntax is ok! Generate the results
	    #`(begin
		#,(generate-main-result stx id super-id field-ids inspector-stx version-num)
		#,@(map (lambda (other-version)
			  (generate-other-result stx id other-version))
			other-versions)))))

      ;; id+super
      (define (parse-id+super stx id/sup-stx)
	(syntax-case id/sup-stx ()
	  [id
	   (identifier? #'id)
	   (values #'id #f)]
	  [(id sup-id)
	   (and (identifier? #'id)
		(identifier? #'sup-id))
	   (values #'id #'sup-id)]
	  [(id other)
	   (identifier? #'id)
	   (raise-syntax-error
	    #f
	    "expected identifier for parent struct type"
	    stx
	    #'other)]
	  [else
	   (raise-syntax-error
	    #f
	    expected-ids
	    stx
	    id/sup-stx)]))

      ;; version
      (define (parse-version stx id/sup-stx version-num-stx)
	;; Check version; #f means no version in original expression
	(unless version-num-stx
	  (raise-syntax-error
	   #f
	   (string-append expected-version
			  (after-name id/sup-stx))
	   stx))
	(let ([v (syntax-e version-num-stx)])
	  (unless (ok-version? v)
	    (raise-syntax-error
	     #f
	     (string-append expected-version
			    (after-name id/sup-stx))
	     stx
	     version-num-stx))
	  v))

      ;; fields
      (define (parse-fields stx id/sup-stx version-num-stx fields-stx)
	;; Now check fields; #f means no fields in oirignal expression
	(unless fields-stx
	  (raise-syntax-error
	   #f
	   (string-append expected-fields
			  (cond
			   [version-num-stx
			    " after version number"]
			   [else (after-name id/sup-stx)]))
	   stx))
	(let ([field-ids (syntax-case fields-stx ()
			   [(field ...)
			    (let ([field-ids (syntax->list #'(field ...))])
			      (for-each (lambda (id)
					  (unless (identifier? id)
					      (raise-syntax-error
					       #f
					       "expected a field identifier"
					       stx
					       id)))
					field-ids)
			      field-ids)]
			   [else
			    (raise-syntax-error
			     #f
			     expected-fields
			     stx
			     fields-stx)])])
	  ;; Fields are all identifiers, so check for distinct fields
	  (let ([dup (check-duplicate-identifier field-ids)])
	    (when dup
	      (raise-syntax-error
	       #f
	       "duplicate field identifier"
	       stx
	       dup)))
	  field-ids))

      ;; Other-version deserializers
      (define (parse-other-versions stx main-version-num other-versions-stx)
	(when (or (not other-versions-stx)
		  (not (syntax->list other-versions-stx)))
	  (raise-syntax-error
	   #f
	   "expected a parenthesized sequence of other-version deserializers after field sequence"
	   stx
	   other-versions-stx))
	(let* ([ht (make-hash-table 'equal)]
	       [other-versions
		(map (lambda (other-stx)
		       (syntax-case other-stx ()
			 [(version-num maker-expr cycle-maker-expr)
			  (let ([v (syntax-e #'version-num)])
			    (unless (ok-version? v)
			      (raise-syntax-error
			       #f
			       (string-append expected-version
					      " for other-version deserializer")
			       stx
			       #'version-num))
			    (when (= v main-version-num)
			      (raise-syntax-error
			       #f
			       (string-append version-conflicts " the main version")
			       stx
			       other-stx))
			    (when (hash-table-get ht v (lambda () #f))
			      (raise-syntax-error
			       #f
			       (string-append version-conflicts " another deserializer")
			       stx
			       other-stx))
			    (hash-table-put! ht v #t)
			    (list v #'maker-expr #'cycle-maker-expr))]
			 [else
			  (raise-syntax-error
			   #f
			   "expected a deserializer for another version: version number, constructor, and cycle constructor"
			   stx
			   other-stx)]))
		     (syntax->list other-versions-stx))])
	  other-versions))

      ;; ------------------------------
      ;;  Generate result

      ;; Generate the result expression. This is complicated
      ;; by the fact that super-id information may or may not be
      ;; immediately available, so we also need continue-define-...
      (define (generate-main-result stx id super-id field-ids inspector-stx version-num)
	(with-syntax ([deserializer-id (make-deserialize-name id version-num)]
		      [struct-type-id (datum->syntax-object
				       id
				       (string->symbol
					(format "struct:~a" (syntax-e id)))
				       id)]
		      [inspector-expr inspector-stx])
	  #`(begin
	      #,(generate-struct-declaration stx
					     id super-id field-ids 
					     (syntax-local-context)
					     (make-make-make-struct-type #'(inspector-expr deserializer-id)))
	      (define deserializer-id (let ([l (internal-deserialize-info struct-type-id)])
					(make-deserialize-info
					 ((car l))
					 (cadr l))))
	      #,@(make-deserialize-provide stx #'deserializer-id))))

      (define (generate-other-result stx id other-version)
	(with-syntax ([deserializer-id (make-deserialize-name id (car other-version))])
	  #`(begin
	      (define deserializer-id (make-deserialize-info #,(cadr other-version)
							     #,(caddr other-version)))
	      #,@(make-deserialize-provide stx #'deserializer-id))))

      (define (make-deserialize-name id version-num)
	(datum->syntax-object
	 id
	 (string->symbol
	  (format "deserialize-info:~a-v~a" 
		  (syntax-e id) 
		  version-num))
	 id))

      (define (make-deserialize-provide stx deserializer-id-stx)
	(if (eq? 'top-level (syntax-local-context))
	    ;; Top level; in case deserializer-id-stx is macro-introduced,
	    ;;  explicitly use namespace-set-variable-value!
	    (list (quasisyntax/loc stx
		    (namespace-set-variable-value! '#,deserializer-id-stx 
						   #,deserializer-id-stx )))
	    ;; In a module; provide:
	    (list (quasisyntax/loc stx
		    (provide #,deserializer-id-stx)))))

      ;; ------------------------------
      ;;  The transformers

      (values
       (lambda (stx)
	 (context-check stx)
	 (main stx))
       (lambda (stx)
	 (context-check stx)
	 (main/versions stx))))))
