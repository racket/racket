
(module make mzscheme
  (require (lib "unitsig.ss"))

  (require "make-sig.ss"
	  "make-unit.ss")
  
  (define-values/invoke-unit/sig make^ make@)

  (provide-signature-elements make^)

  (define-syntax make
    (lambda (stx)
      (syntax-case stx ()
	[(_ spec)
	 (syntax (make spec #()))]
	[(_ spec argv)
	 (let ([form-error (lambda (s . p) 
			     (apply raise-syntax-error 'make s stx p))])
	   (let ([sl (syntax->list (syntax spec))])
	     (unless (list? sl)
	       (form-error "illegal specification (not a sequence)"))
	     (unless (pair? sl)
	       (form-error "empty specification"))
	     (andmap
	      (lambda (line)
		(let ([ll (syntax->list line)])
		  (unless (and (list? ll) (>= (length ll) 2))
		    (form-error "clause does not have at least 2 parts" line))
		  (let ([name (car ll)])
		    (unless (syntax->list (cadr ll))
		      (form-error "second part of clause is not a sequence" (cadr ll))))))
	      sl)
	     (with-syntax ([(line ...)
			    (map (lambda (line)
				   (syntax-case line () 
				     [(target deps) (syntax (list target (list . deps)))]
				     [(target deps . c) (syntax (list target (list . deps)
								      (lambda () . c)))]))
				 sl)])
	       (syntax (make/proc 
			(list line ...)
			argv)))))])))

  (provide make))
