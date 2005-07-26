
(module library mzscheme
  (provide (rename lib-read read)
	   (rename lib-read-syntax read-syntax))

  (require (prefix r6rs: "reader.ss"))

  (define lib-read
    (case-lambda
     [() (lib-read (current-input-port))]
     [(input) (lib-read-syntax (object-name (current-input-port)) (current-input-port))]))
		  
  (define lib-read-syntax
    (case-lambda
     [() (lib-read-syntax (object-name (current-input-port)) (current-input-port))]
     [(src-v) (lib-read-syntax src-v (current-input-port))]
     [(src-v input) (let ([r1 (r6rs:read-syntax src-v input)]
			  [r2 (r6rs:read-syntax src-v input)])
		      (let ([name-stx (and (syntax? r1)
					   (eof-object? r2)
					   (pair? (syntax-e r1))
					   (eq? 'library (syntax-e (car (syntax-e r1))))
					   (or (and
						(pair? (cdr (syntax-e r1)))
						(cadr (syntax-e r1)))
					       (and
						(syntax? (cdr (syntax-e r1)))
						(pair? (syntax-e (cdr (syntax-e r1))))
						(car (syntax-e (cdr (syntax-e r1)))))))])
			(unless (and name-stx (string? (syntax-e name-stx)))
			  (error 'r6rs-load-handler
				 "expected a single `library' form with a string name, found something else"))
			(datum->syntax-object
			 #f
			 `(module ,(string->symbol (syntax-e name-stx)) (lib "library-module.ss" "r6rs")
			    (#%module-begin ,r1)))))])))

			    
			

