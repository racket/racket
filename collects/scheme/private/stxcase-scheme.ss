
;;----------------------------------------------------------------------
;; #%stxcase-scheme: adds let-syntax, syntax-rules, and
;;  check-duplicate-identifier, and assembles everything we have so far

(module stxcase-scheme '#%kernel
  (#%require "small-scheme.ss" "stx.ss" "stxcase.ss" "with-stx.ss" "stxloc.ss"
             (for-syntax '#%kernel "small-scheme.ss" "stx.ss" "stxcase.ss" "with-stx.ss" 
                         "stxloc.ss"))

  (-define (check-duplicate-identifier names)
    (unless (and (list? names) (andmap identifier? names))
      (raise-type-error 'check-duplicate-identifier "list of identifiers" names))
    (let/ec escape
      (let ([ht (make-hasheq)])
	(for-each
	 (lambda (defined-name)
	   (unless (identifier? defined-name)
	     (raise-type-error 'check-duplicate-identifier
			       "list of identifiers" names))
	   (let ([l (hash-ref ht (syntax-e defined-name) null)])
	     (when (ormap (lambda (i) (bound-identifier=? i defined-name)) l)
	       (escape defined-name))
	     (hash-set! ht (syntax-e defined-name) (cons defined-name l))))
	 names)
	#f)))
  
  ;; From Dybvig, mostly:
  (-define-syntax syntax-rules
    (lambda (stx)
      (syntax-case** syntax-rules #t stx () free-identifier=?
	((sr (k ...) ((keyword . pattern) template) ...)
	 (andmap identifier? (syntax->list (syntax (k ...))))
	 (begin
           (for-each (lambda (id)
                       (unless (identifier? id)
                         (raise-syntax-error
                          #f
                          "pattern must start with an identifier, found something else"
                          stx
                          id)))
                     (syntax->list (syntax (keyword ...))))
	   (syntax/loc stx
	     (lambda (x)
	       (syntax-case** sr #t x (k ...) free-identifier=?
		 ((_ . pattern) (syntax/loc x template))
		 ...))))))))

  (-define-syntax syntax-id-rules
    (lambda (x)
      (syntax-case** syntax-id-rules #t x () free-identifier=?
	((sidr (k ...) (pattern template) ...)
	 (andmap identifier? (syntax->list (syntax (k ...))))
	 (syntax/loc x
	   (make-set!-transformer
	    (lambda (x)
	      (syntax-case** sidr #t x (k ...) free-identifier=?
		(pattern (syntax/loc x template))
		...))))))))

  (#%provide syntax (all-from "with-stx.ss") (all-from "stxloc.ss") 
             check-duplicate-identifier
             syntax-rules syntax-id-rules
             (for-syntax syntax-pattern-variable?)))
