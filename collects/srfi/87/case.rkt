(module case mzscheme
  
  (provide (rename my-case srfi:case))
  
  (define-syntax case-test
    (lambda (x)
      (syntax-case x ()
	[(_ x (k))
	 (if (symbol? (syntax-e #'k))
	     (syntax (eq? x 'k))
	     (syntax (eqv? x 'k)))]
	[(_ x (k ...))
	 (syntax (memv x '(k ...)))])))

  (define-syntax my-case
    (lambda (x)
      (syntax-case x (else =>)
	((_ v)
	 (syntax (begin v (cond))))
	((_ v (else => e))
	 (syntax/loc x (e v)))
	((_ v (else e1 e2 ...))
	 (syntax/loc x (begin v e1 e2 ...)))
	((_ v ((k ...) => e) c ...)
	 (syntax/loc x (let ((x v))
			 (if (case-test x (k ...))
			     (e x)
			     (my-case x c ...)))))
	((_ v ((k ...) e1 e2 ...))
	 (syntax/loc x (if (case-test v (k ...)) (begin e1 e2 ...))))
	((_ v ((k ...) e1 e2 ...) c1 c2 ...)
	 (syntax/loc x (let ((x v))
			 (if (case-test x (k ...))
			     (begin e1 e2 ...)
			     (my-case x c1 c2 ...)))))
	((_ v (bad e1 e2 ...) . rest)
	 (raise-syntax-error 
	  #f
	  "bad syntax (not a datum sequence)"
	  x
	  (syntax bad)))
	((_ v clause . rest)
	 (raise-syntax-error 
	  #f
	  "bad syntax (missing expression after datum sequence)"
	  x
	  (syntax clause)))
	((_ . v)
	 (not (null? (syntax-e (syntax v))))
	 (raise-syntax-error 
	  #f
	  "bad syntax (illegal use of `.')"
	  x)))))
  )
