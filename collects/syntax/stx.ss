(module stx mzscheme

  ;; These utilities facilitate operations on syntax objects.
  ;; A syntax object that represents a parenthesized sequence
  ;; can contain a mixture of cons cells and syntax objects,
  ;; hence the need for `stx-null?', `stx-car', etc.

  ;; a syntax null?
  (define (stx-null? p)
    (or (null? p)
	(and (syntax? p) 
	     (null? (syntax-e p)))))

  ;; a syntax pair?
  (define (stx-pair? p)
    (or (pair? p)
	(and (syntax? p)
	     (pair? (syntax-e p)))))

  ;; a syntax list?
  (define (stx-list? p)
    (or (list? p)
	(if (syntax? p) 
	    (or (list? (syntax-e p))
		(let loop ([l (syntax-e p)])
		  (if (pair? l)
		      (loop (cdr l))
		      (stx-list? l))))
	    (and (pair? p)
		 (stx-list? (cdr p))))))

  ;; car of a syntax pair
  (define (stx-car p)
    (if (pair? p)
	(car p)
	(car (syntax-e p))))

  ;; cdr of a syntax pair
  (define (stx-cdr p)
    (if (pair? p)
	(cdr p)
	(cdr (syntax-e p))))

  ;; Flattens a syntax list into a list
  (define (stx->list e)
    (if (syntax? e)
	(syntax->list e)
	(let ([flat-end
	       (let loop ([l e])
		 (if (null? l) 
		     #f
		     (if (pair? l)
			 (loop (cdr l))
			 (if (syntax? l) 
			     (syntax->list l)
			     #f))))])
	  (if flat-end
	      ;; flatten
	      (let loop ([l e])
		(if (null? l) 
		    null
		    (if (pair? l) 
			(cons (car l) (loop (cdr l)))
			(if (syntax? l) 
			    flat-end))))
	      e))))

  (define (module-or-top-identifier=? a b)
    (or (module-identifier=? a b)
	(and (eq? (syntax-e a) (syntax-e b))
	     (module-identifier=? a
				  (datum->syntax-object
				   #f
				   (syntax-e b))))))

  (provide stx-null? stx-pair? stx-list?
	   stx-car stx-cdr stx->list
	   module-or-top-identifier=?))
