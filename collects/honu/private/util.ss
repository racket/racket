
(module util mzscheme
  (provide delim-identifier=?
           extract-until)

  (require syntax/stx)

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
     [(r ids) (extract-until r ids #f)]))

  (define (test)
    (let* ([original #'(a b c d e)]
           [delimiter #'c]
           [expected-before #'(a b)]
           [expected-rest #'(c d e)]
           [expected-delimiter #'c]
           )
    (let-values ([(before rest hit) (extract-until original (list delimiter))])
      ;; is there a better way to test equality between two syntaxes?
      (when (not (and (equal? (syntax-object->datum expected-before)
                              (map syntax-object->datum before))
                      (equal? (syntax-object->datum expected-rest)
                              (map syntax-object->datum rest))
                      (equal? (syntax-object->datum expected-delimiter)
                              (syntax-object->datum hit))))
        (printf "failure: original ~a until ~a\n" (syntax-object->datum original) (map syntax-object->datum (list delimiter)))
        (printf " before expected ~a actual ~a\n" (syntax-object->datum expected-before) (map syntax-object->datum before))
        (printf " rest expected ~a actual ~a\n" (syntax-object->datum expected-rest) (map syntax-object->datum rest))
        (printf " delimiter expected ~a actual ~a\n" (syntax-object->datum expected-delimiter) (syntax-object->datum hit))
        ))))

  (test)

  )
