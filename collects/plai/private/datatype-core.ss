
;; Shared infrastructure for define-type and define-datatype (eventually)

;; NOTE: datatypes are currently transparent. This works with EoPL's
;; use of `equal?', and also makes constructor-style printing show
;; all fields.

(module datatype-core mzscheme
  (require (lib "pconvert-prop.ss")
	   (lib "contract.ss" "mzlib" "private"))
  (require-for-syntax "core-utils.ss")

  (provide define-datatype-core
	   cases-core
	   provide-datatype-core)

  (define-for-syntax (generate-dt-temporaries l)
    (generate-temporaries l))

  (define (projection-contract name proc)
    (let ([name `(,(car name) ,@(map (lambda (c)
				       (if (contract? c)
					   (contract-name c)
					   (or (object-name c)
					       c)))
				     (cdr name)))])
      (make-contract name
		     (lambda (pos neg src-info orig-str)
		       (let ([proc (proc pos neg src-info orig-str)])
			 (lambda (v)
			   (let ([v2 (proc v)])
			     (unless v2
			       (raise-contract-error
				src-info
				pos
				neg
				orig-str
				"expected <~a>, given: ~e"
				name
				v))
			     v2)))))))

  (define (dt-contract-proc c)
    (contract-proc
     (if (contract? c)
	 c
	 (flat-contract c))))

  ;; Syntax:
  ;;   (define-datatype-core orig-form (option ...) d-v name (alpha ...) pred-name variant ...)
  ;; where the syntax is like `define-datatype' starting with `pred-name'.
  ;;
  ;; The `orig-stx' part is used for syntax-error reporting.
  ;; The `d-v' is used in place of `define-values' to bind procedures. Beware
  ;;   that variant-constructor procedures are bound as syntax and an different
  ;;   name is bound to the actual procedure; if this "actual" binding itself
  ;;   turns out to be a macro, then uses of the constructor name are expanded
  ;;   by directly calling the macro from the "actual" binding
  ;; Each `alpha' is a parameter to the contract expressions of each variant field;
  ;;   using `x-of' for variant `x' allows the parameter contracts to be supplied, 
  ;;   while using `x' directly instantiates each parameter as `any/c'.
  ;; The syntax for each `variant' is checked here; perform other syntax
  ;;  checks before using `define-datatype-core'.
  ;; The valid options are:
  ;;   define-predicates : include `x?' for each variant `x'
  ;;   define-selectors : include `x-f' for each field `f' of each variant `x'
  ;;   define-polymorphic : include a definition of `x-of' for each variant `x'
  ;;   define-contracts : include a definition of `x-of/c' for each variant `x'
  ;;                      requires define-selectors
  ;;   define-compatibility: include `make-x' for each variant `x'
  ;;   (kind "str") : uses "str" to name the result, either "type" or "datatype"
  ;;
  ;; Internals:
  ;;  The `name' is bound as syntax to a dt record, which supplies an id
  ;;   for the datatype's predicate, and also lists the ;;  datatype's variants
  ;;   through vt records.
  ;;  Each variant constructor name is bound as syntax to a dtvt record,
  ;;   which gives the variant's vt record as well as its datatype's dt
  ;;   record.
  ;;  (See "core-utils.ss" for the dt, vt, and dtvt records.)
  ;;
  (define-syntax define-datatype-core
    (lambda (stx)
      (syntax-case stx ()
	[(_ orig-stx (option ...) define-proc-values name (alpha ...) pred-name 
	    (variant-name (field-name field-pred) ...) 
	    ...)
	 (let ([stx #'orig-stx]
	       [options (syntax-object->datum #'(option ...))]
	       [variant-names (syntax->list (syntax (variant-name ...)))]
	       [field-nameses (map syntax->list
				   (syntax->list (syntax ((field-name ...) ...))))])
	   ;; More syntax checks...
	   (unless (identifier? (syntax name))
	     (raise-syntax-error #f
				 "expected an identifier for the datatype name"
				 stx (syntax name)))
	   (unless (identifier? (syntax pred-name))
	     (raise-syntax-error #f
				 "expected an identifier for the predicate name"
				 stx (syntax pred-name)))
	   (for-each (lambda (vt fields)
		       (unless (identifier? vt)
			 (raise-syntax-error 
			  #f
			  "expected an identifier for the variant name"
			  stx vt))
		       (for-each (lambda (field)
				   (unless (identifier? field)
				     (raise-syntax-error 
				      #f
				      "expected an identifier for the field name"
				      stx field)))
				 fields))
		     variant-names
		     field-nameses)
	   ;; Count the fields for each variant:
	   (with-syntax ([(variant-field-count ...) 
			  (map (lambda (n) 
				 (datum->syntax-object (quote-syntax here) n #f))
			       (map length field-nameses))]
			 [(orig-variant-name ...)
			  (generate-dt-temporaries variant-names)]
                         [(variant-name/no-contract ...)
			  (generate-dt-temporaries variant-names)]
                         [(variant-of ...)
			  (map (lambda (variant-name) 
				 (datum->syntax-object variant-name
						       (string->symbol
							(format "~a-of" (syntax-e variant-name)))))
			       variant-names)]
			 [(variant-of/c ...)
			  (map (lambda (variant-name) 
				 (datum->syntax-object variant-name
						       (string->symbol
							(format "~a-of/c" (syntax-e variant-name)))))
			       variant-names)]
                         [type-of/c (datum->syntax-object #'name
							  (string->symbol
							   (format "~a-of/c" (syntax-e #'name))))]
                         [(variant? ...)
                          (map (lambda (vn)
                                 (datum->syntax-object
                                  vn
                                  ((if (memq 'define-predicates options) string->symbol string->uninterned-symbol)
                                   (format "~a?" (syntax-e vn)))))
                               variant-names)]
                         [(variant-accessor ...)
                          (map (lambda (vn)
                                 (datum->syntax-object
                                  vn
                                  (string->uninterned-symbol
                                   (format "~a-accessor" (syntax-e vn)))))
                               variant-names)]
			 [(variant-mutator ...)
                          (generate-dt-temporaries variant-names)]
                         [(make-variant ...)
                          (generate-dt-temporaries variant-names)]
                         [(struct:variant ...)
                          (generate-dt-temporaries variant-names)]
			 [((selector-name ...) ...)
			  (map (lambda (variant-name field-names)
				 (if (memq 'define-selectors options)
				     (map (lambda (field-name)
					    (datum->syntax-object
					     variant-name
					     (string->symbol
					      (format "~a-~a" 
						      (syntax-e variant-name)
						      (syntax-e field-name)))))
					  field-names)
				     null))
			       variant-names
			       field-nameses)]
			 [((sub-contract-proc ...) ...)
			  (map (lambda (field-names)
				 (generate-dt-temporaries field-names))
			       field-nameses)]
			 [((field-pos ...) ...)
			  (map (lambda (field-names)
				 (let loop ([l field-names][i 0])
				   (if (null? l)
				       null
				       (cons i (loop (cdr l) (add1 i))))))
			       field-nameses)]
                         [(make-variant-name ...)
                          (map (lambda (vn)
                                 (datum->syntax-object
                                  vn
                                  (string->symbol
                                   (format "make-~a" (syntax-e vn)))))
                               variant-names)]
			 [datatype-str (or (ormap (lambda (option)
						    (and (pair? option)
							 (eq? 'kind (car option))
							 (cadr option)))
						  options)
					   "datatype")])
	     (quasisyntax
	      (begin
		(define-syntaxes (name variant-name ...)
		  ;; Note: we're back to the transformer environment, here.
		  ;; Also, this isn't a transformer function, so any direct
		  ;;  use of the name will trigger a syntax error. The name
		  ;;  can be found by `syntax-local-value', though.
		  (let ([cert (syntax-local-certifier)])
		    (let-values ([(variant-name ...)
				  (values
				   (make-vt (cert (quote-syntax variant-name))
					    (cert (quote-syntax variant?))
					    (cert (quote-syntax variant-accessor))
					    (list (quote-syntax selector-name) ...)
					    variant-field-count)
				   ...)])
		      (let ([dt (make-dt (cert (syntax pred-name))
					 (list variant-name ...)
					 datatype-str)])
			(values
			 (make-set!-transformer dt)
			 (make-set!-transformer
			  (make-dtvt dt variant-name (quote-syntax orig-variant-name)))
			 ...)))))
		;; Bind the predicate and selector functions:
		(define-proc-values (pred-name
				     variant-name/no-contract ...
				     variant? ...
				     variant-accessor ...
				     selector-name ... ...
				     orig-variant-name ...)
		  ;; Create a new structure for the datatype (using the
		  ;; datatype name in `struct', so it prints nicely).
		  (let-values ([(struct:x make-x x? acc mut)
				(make-struct-type 'name #f 0 0 #f null (make-inspector))])
                    (let-values ([(struct:variant make-variant variant? 
                                                  variant-accessor variant-mutator)
                                  (make-struct-type 'variant-name struct:x variant-field-count 0
                                                    #f 
						    `((,prop:print-convert-constructor-name . variant-name))
						    (make-inspector))]
                                 ...)
		      (let-values #,(if (memq 'define-selectors options)
					#`([(selector-name ...)
					    (let ([accessor variant-accessor])
					      (values (make-struct-field-accessor accessor field-pos 'field-name)
						      ...))]
					   ...)
					())
			;; User-available functions:
			(values
			 x? ;; The datatype predicate
			 ;; Rename the constructor:
			 make-variant ...
			 variant? ...
			 variant-accessor ...
			 selector-name ... ...
			 ;; Constructors:
			 (let ([f (delay (contract (let ([alpha any/c] ...) (-> field-pred ... x?))
						   make-variant
						   'definition 'use (quote-syntax variant-name)))])
			   (let ([variant-name (lambda (field-name ...) ((force f) field-name ...))])
			     variant-name))
			 ...)))))
		#,@(if (memq 'define-contracts options)
		       #`((define (type-of/c alpha ...)
			    (projection-contract
			     `(type-of/c ,alpha ...)
			     (lambda (pos neg src-info orig-str)
			       (let ([sub-contract-proc (delay
							  ((dt-contract-proc field-pred) pos neg src-info orig-str))]
				     ... ...)
				 (lambda (x)
				   (and (pred-name x)
					(or (and (variant? x)
						 (variant-name/no-contract
						  ((force sub-contract-proc) (selector-name x)) ...))
					    ...)))))))
			  (define (variant-of/c alpha ...)
			    (projection-contract
			     `(variant-of/c ,alpha ...)
			     (lambda (pos neg src-info orig-str)
			       (let ([sub-contract-proc (delay
							  ((dt-contract-proc field-pred) pos neg src-info orig-str))]
				     ...)
				 (lambda (x)
				   (and (variant? x)
					(variant-name/no-contract
					 ((force sub-contract-proc) (selector-name x)) ...)))))))
			  ...)
		       null)
		#,@(if (memq 'define-polymorphic options)
		       #`((define (variant-of alpha ...)
			    (let ([f (contract (-> field-pred ... pred-name)
					       variant-name/no-contract
					       'definition 'use (quote-syntax variant-name))])
			      (let ([variant-name (lambda (field-name ...)
						    (f field-name ...))])
				variant-name)))
			  ...)
		       null)
		;; Compatibility bindings
		#,@(if (memq 'define-compatibility options)
		      #`((define-proc-values (make-variant-name ...) (values variant-name ...)))
		      null)))))]
        [(_ orig-stx (option ...) define-proc-values name (alpha ...) pred-name variant ...)
         ;; Must be a bad variant...
         (for-each (lambda (variant)
                     (syntax-case variant ()
                       [(variant-name field ...)
                        (let ([name (syntax variant-name)])
                          (unless (identifier? name)
                            (raise-syntax-error
                             #f
                             "expected an identifier for the variant name"
                             #'orig-stx
                             name))
                          ;; Must be a bad field:
                          (for-each (lambda (field)
                                      (syntax-case field ()
                                        [(field-name field-pred)
                                         (let ([name (syntax field-name)])
                                           (unless (identifier? name)
                                             (raise-syntax-error
                                              #f
                                              "expected an identifier for the field name"
                                              #'orig-stx
                                              name)))]
                                        [_else
                                         (raise-syntax-error
                                          #f
                                          "expected a field name followed by a predicate expression, all in parentheses"
                                          #'orig-stx
                                          field)]))
                                    (syntax->list (syntax (field ...)))))]
                       [_else
                        (raise-syntax-error
                         #f
                         "expected a variant name followed by a sequence of field declarations, all in parentheses"
                         #'orig-stx
                         variant)]))
                   (syntax->list (syntax (variant ...))))]
	[(_ orig_stx . __)
	 ;; trigger "bad syntax" error:
	 (syntax-case #'orig-stx ())])))

  (define-for-syntax (lookup-datatype datatype)
    (let ([v (and (identifier? datatype)
		  (syntax-local-value datatype (lambda () #f)))])
      (and v
	   (set!-transformer? v)
	   (set!-transformer-procedure v))))

  (define-syntax cases-core
    (lambda (stx)
      (syntax-case stx ()
	[(_ orig-stx datatype-str case-begin cases-else
	    datatype expr 
	    clause
	    ...)
	 ;; Get datatype information:
	 (let ([stx #'orig-stx]
	       [dt (lookup-datatype #'datatype)])
	   (unless (dt? dt)
	     (raise-syntax-error 
	      #f
	      (format "not a ~a name" (syntax-e #'datatype-str))
	      stx
	      (syntax datatype)))
	   
	   ;; Parse clauses:
	   (let-values ([(vts field-idss bodys else-body)
			 (let loop ([clauses (syntax->list (syntax (clause ...)))][saw-cases null])
			   (cond
			    [(null? clauses)
			     (values null null null #f)]
			    [else
			     (let ([clause (car clauses)])
			       (syntax-case* clause (else) (lambda (a b)
							     (module-identifier=? a #'cases-else))
				 [(variant (field-id ...) body0 body1 ...)
				  (let* ([variant (syntax variant)]
					 [vt
					  (ormap (lambda (dtv) 
						   (let ([vt-name (vt-name-stx dtv)])
						     (and (module-identifier=? variant vt-name)
							  dtv)))
						 (dt-variants dt))]
                                         [orig-variant (and vt (vt-name-stx vt))])
				    (unless orig-variant
				      (raise-syntax-error 
				       #f
				       (format "not a variant of `~a'"
					       (syntax-object->datum (syntax datatype)))
				       stx
				       variant))

				    (let ([field-ids (syntax->list (syntax (field-id ...)))])
				      (for-each (lambda (fid)
						  (unless (identifier? fid)
						    (raise-syntax-error
						     #f
						     "expected an identifier for a field"
						     stx
						     fid)))
						field-ids)
				      (let ([dtv (variant-assq variant (dt-variants dt))])
					(unless (= (length field-ids)
						   (vt-field-count dtv))
					  (raise-syntax-error
					   #f
					   (format
					    "variant case `~a' for `~a' has wrong field count (expected ~a, found ~a)"
					    (syntax-object->datum variant)
					    (syntax-object->datum (syntax datatype))
					    (vt-field-count dtv)
					    (length field-ids))
					   stx
					   clause)))

				      ;; Check for duplicate local field ids:
				      (let ([dup (check-duplicate-identifier field-ids)])
					(when dup
					  (raise-syntax-error
					   #f
					   "duplicate field identifier"
					   stx
					   dup)))

				      ;; Check for redundant case:
				      (when (memq orig-variant saw-cases)
					(raise-syntax-error
					 #f
					 "duplicate case"
					 stx
					 clause))
				    
				      ;; This clause is ok:
				      (let-values ([(vts idss bodys else)
						    (loop (cdr clauses) (cons orig-variant saw-cases))])
					(values (cons vt vts)
						(cons field-ids idss)
						(cons (with-syntax ([clause clause])
							(syntax (case-begin orig-stx clause body0 body1 ...)))
						      bodys)
						else))))]
				 [(else body0 body1 ...)
				  (begin
				    (unless (null? (cdr clauses))
				      (raise-syntax-error
				       #f
				       "else clause must be last"
				       stx
				       clause))
				    (values null null null (syntax (begin body0 body1 ...))))]
				 [_else (raise-syntax-error
					 #f
					 "bad clause"
					 stx
					 clause)]))]))])
             
             ;; Missing any variants?
             (unless (or else-body
                         (= (length vts) (length (dt-variants dt))))
               (let* ([here (map vt-name-stx vts)]
                      [missing (let loop ([l (dt-variants dt)])
                                (cond
                                  [(null? l) ""]
                                  [(ormap (lambda (i) (module-identifier=? (vt-name-stx (car l)) i)) here)
                                   (loop (cdr l))]
                                  [else
                                   (format " ~a~a" 
                                           (syntax-e (vt-name-stx (car l)))
                                           (loop (cdr l)))]))])
                 (raise-syntax-error
                  #f
                  (format "missing cases for the following variants:~a" missing)
                  stx)))

	     (with-syntax ([form-name (syntax-case stx () [(name . _) #'name])])

	       ;; Create the result:
	       (with-syntax ([pred (dt-pred-stx dt)]
			     [(variant? ...) (map vt-predicate-stx vts)]
			     [((field-extraction ...) ...) 
			      (map (lambda (vt)
				     (with-syntax ([accessor (vt-accessor-stx vt)])
				       (let loop ([n 0])
					 (if (= n (vt-field-count vt))
					     null
					     (cons (with-syntax ([n n])
						     (syntax (accessor v n)))
						   (loop (add1 n)))))))
				   vts)]
			     [((field-id ...) ...) field-idss]
			     [(body ...) bodys]
			     [else-body (or else-body
					    (syntax 
					     (error 'form-name "no variant case matched")))])
		 (syntax/loc stx
		   (let ([v expr])
		     (if (not (pred v))
			 (error 'form-name "not a ~a: ~e" 
				(quote datatype) v)
			 (cond
			  [(variant? v)
			   (let ([field-id field-extraction] ...)
			     body)]
			  ...
			  [else else-body]))))))))]
	[(_ orig-stx datatype-str cases-else datatype)
	 (begin
	   (unless (dt? (lookup-datatype #'datatype))
	     (raise-syntax-error 
	      #f
	      (format "not a ~a name"  (syntax-e #'datatype-str))
	      #'orig-stx
	      (syntax datatype)))
	   (raise-syntax-error
	    #f
	    (format "expected an expression after the ~a name" (syntax-e #'datatype-str))
	    #'orig-stx))]
	[(_ orig-stx datatype-str cases-else)
	 (raise-syntax-error
	  #f
	  (format "expected a ~a name" (syntax-e #'datatype-str))
	  #'orig-stx)]
	[(_ orig_stx . __)
	 ;; trigger "bad syntax" error:
	 (syntax-case #'orig-stx ())])))
  
  (define-syntax provide-datatype-core
    (lambda (stx)
      (syntax-case stx ()
	[(_ orig-stx datatype)
	 (let ([stx #'orig-stx]
	       [dt (syntax-local-value (syntax datatype) (lambda () #f))])
	   (unless (dt? dt)
	     (raise-syntax-error
	      #f
	      "not a datatype name" 
	      stx
	      (syntax datatype)))
	   (with-syntax ([pred (dt-pred-stx dt)]
			 [(orig-variant ...) 
			  (map vt-name-stx (dt-variants dt))]
			 [((selector ...) ...)
			  (map vt-selector-stxes (dt-variants dt))]
			 [(variant? ...)
			  (map vt-predicate-stx (dt-variants dt))])
	     (syntax
	      (provide datatype
		       pred
		       orig-variant ...
		       variant? ...
		       selector ... ...))))]
	[(_ orig_stx . __)
	 ;; trigger "bad syntax" error:
	 (syntax-case #'orig-stx ())]))))
