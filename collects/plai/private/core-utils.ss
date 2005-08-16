
(module core-utils mzscheme

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
  
  ;; Helper function:
  (define (variant-assq name-stx variants)
    (let loop ([l variants])
      (if (module-identifier=? name-stx 
			       (vt-name-stx (car l)))
	  (car l)
	  (loop (cdr l)))))
  
  (provide make-dt dt? dt-pred-stx dt-variants
	   (struct vt (name-stx predicate-stx accessor-stx selector-stxes field-count))
	   variant-assq))
