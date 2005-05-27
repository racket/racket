
(module util mzscheme
  (provide extract-until)

  (require (lib "stx.ss" "syntax"))

  (define (extract-until r ids)
     (let loop ([r r][val-stxs null])
       (cond
	[(stx-null? r)
	 (values #f #f)]
	[(and (identifier? (stx-car r))
	      (ormap (lambda (id)
		       (module-identifier=? id (stx-car r)))
		     ids))
	 (values (reverse val-stxs) r)]
	[else
	 (loop (stx-cdr r) (cons (stx-car r) val-stxs))]))))
     
