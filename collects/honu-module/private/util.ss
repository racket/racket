
(module util mzscheme
  (provide extract-until)

  (require (lib "stx.ss" "syntax"))

  (define extract-until
    (case-lambda
     [(r ids keep?)
      (let loop ([r r][val-stxs null])
	(cond
	 [(stx-null? r)
	  (values #f #f #f)]
	 [(and (identifier? (stx-car r))
	       (ormap (lambda (id)
			(module-identifier=? id (stx-car r)))
		      ids))
	  (values (reverse (if keep?
			       (cons (stx-car r) val-stxs) 
			       val-stxs))
		  r
		  (stx-car r))]
	 [else
	  (loop (stx-cdr r) (cons (stx-car r) val-stxs))]))]
     [(r ids) (extract-until r ids #f)])))

     
