
(module ops mzscheme
  (provide unary-prefix-ops
	   (struct op (id))
	   (struct prefix ())
	   (struct infix ())
	   (struct postfix ())
	   prec-key
	   precedence-table
	   op-table)

  (define unary-prefix-ops (list #'++
				 #'--
				 #'+
				 #'-
				 #'!
				 #'~))
  
  (define-struct op (id))
  (define-struct (prefix op) ())
  (define-struct (infix op) ())
  (define-struct (postfix op) ())

  (define (prec-key op)
    (and op
	 (cons (cond
		[(prefix? op) 'pre]
		[(infix? op) 'in]
		[(postfix? op) 'post])
	       (syntax-e (op-id op)))))
  
  (define precedence-table (make-immutable-hash-table
			    '(((in . |.|) . 100)
			      ((in . #%parens) . 100)
			      ((in . #%brackets) . 100)
			      ((post . ++) . 100)
			      ((post . --) . 100)
			      ((pre . ++) . 95)
			      ((pre . --) . 95)
			      ((pre . +) . 95)
			      ((pre . -) . 95)
			      ((pre . ~) . 95)
			      ((pre . !) . 95)
			      ((in . *) . 90)
			      ((in . %) . 90)
			      ((in . /) . 90)
			      ((in . +) . 85)
			      ((in . -) . 85)
			      ((in . >>) . 80)
			      ((in . <<) . 80)
			      ((in . >>>) . 80)
			      ((in . <) . 70)
			      ((in . >) . 70)
			      ((in . <=) . 70)
			      ((in . >=) . 70)
			      ((in . ==) . 60)
			      ((in . !=) . 60)
			      ((in . &) . 55)
			      ((in . ^) . 50)
			      ((in . \|) . 45)
			      ((in . &&) . 40)
			      ((in . \|\|) . 35)
			      ((in . =) . 10)
			      ((in . +=) . 10)
			      ((in . -=) . 10)
			      ((in . *=) . 10)
			      ((in . /=) . 10)
			      ((in . %=) . 10)
			      ((in . &=) . 10)
			      ((in . ^=) . 10)
			      ((in . \|=) . 10)
			      ((in . <<=) . 10)
			      ((in . >>=) . 10)
			      ((in . >>>=) . 10))
			    'equal))

(define op-table (make-hash-table))
(hash-table-for-each precedence-table (lambda (k v)
					(hash-table-put! op-table (cdr k) #t))))

