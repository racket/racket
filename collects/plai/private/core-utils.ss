
(module core-utils mzscheme
  (require-for-template mzscheme)
  
  (define-values (struct:dt make-dt dt? dt-selector dt-accessor)
    (make-struct-type 'dt #f 3 0 #f null (current-inspector)	       
		      (lambda (dt stx)
			(raise-syntax-error
			 #f
			 (format "illegal use of ~a name" (dt-kind dt))
			 stx))))
  (define dt-pred-stx (make-struct-field-accessor dt-selector 0 'pred-stx))
  (define dt-variants (make-struct-field-accessor dt-selector 1 'variants))
  (define dt-kind (make-struct-field-accessor dt-selector 2 'kind))
		  
  (define-struct vt (name-stx predicate-stx accessor-stx selector-stxes field-count))

  (define-values (struct:dtvt make-dtvt dtvt? dtvt-selector dtvt-accessor)
    (make-struct-type 'dtvt #f 3 0 #f null (current-inspector)       
		      (lambda (dtvt stx)
			(syntax-case stx (set!)
			  [(set! id v)
			   (raise-syntax-error 
			    #f
			    "cannot assign to a variant name"
			    stx
			    #'id)]
			  [(id . args)
			   (let ([v (syntax-local-value (dtvt-orig-id dtvt)
							(lambda () #f))])
			     (if (and (procedure? v)
				      (procedure-arity-includes? v 1))
				 ;; Apply macro binding for orig id to this id:
				 (v stx)
				 ;; Orig id is not bound to a macro:
				 (datum->syntax-object
				  stx
				  (cons (dtvt-orig-id dtvt)
					(syntax args))
				  stx)))]
			  [else
			   (let ([v (syntax-local-value (dtvt-orig-id dtvt)
							(lambda () #f))])
			     (if (and (procedure? v)
				      (procedure-arity-includes? v 1))
				 ;; Apply macro binding for orig id to this id:
				 (v stx)
				 ;; Orig id is not bound to a macro:
				 (dtvt-orig-id dtvt)))]))))
  
  (define dtvt-dt (make-struct-field-accessor dtvt-selector 0 'dt))
  (define dtvt-vt (make-struct-field-accessor dtvt-selector 1 'vt))
  (define dtvt-orig-id (make-struct-field-accessor dtvt-selector 2 'orig-id))
  
  ;; Helper function:
  (define (variant-assq name-stx variants)
    (let loop ([l variants])
      (if (module-identifier=? name-stx 
			       (vt-name-stx (car l)))
	  (car l)
	  (loop (cdr l)))))
  
  (provide make-dt dt? dt-pred-stx dt-variants
	   (struct vt (name-stx predicate-stx accessor-stx selector-stxes field-count))
	   make-dtvt dtvt? dtvt-dt dtvt-vt
	   variant-assq))
