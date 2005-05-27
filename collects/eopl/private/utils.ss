
(module utils mzscheme

  ;; Generative structure definitions:
  (define-struct dt (pred-stx variants))
  (define-struct vt (name-stx predicate-stx accessor-stx field-count))
  
  ;; Helper function:
  (define (variant-assq name-stx variants)
    (let loop ([l variants])
      (if (module-identifier=? name-stx 
			       (vt-name-stx (car l)))
	  (car l)
	  (loop (cdr l)))))
  
  (provide (struct dt (pred-stx variants))
	   (struct vt (name-stx predicate-stx accessor-stx field-count))
	   variant-assq))
