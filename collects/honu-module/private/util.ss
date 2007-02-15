
(module util mzscheme
  (provide delim-identifier=?
           extract-until)

  (require (lib "stx.ss" "syntax"))

   (define (delim-identifier=? a b)
     (eq? (syntax-e a) (syntax-e b)))

  (define extract-until
    (case-lambda
     [(r ids keep?)
      (let loop ([r r][val-stxs null])
	(cond
	 [(stx-null? r)
	  (values #f #f #f)]
	 [(and (identifier? (stx-car r))
	       (ormap (lambda (id)
			(delim-identifier=? id (stx-car r)))
		      ids))
	  (values (reverse (if keep?
			       (cons (stx-car r) val-stxs) 
			       val-stxs))
		  r
		  (stx-car r))]
	 [else
	  (loop (stx-cdr r) (cons (stx-car r) val-stxs))]))]
     [(r ids) (extract-until r ids #f)])))

     
