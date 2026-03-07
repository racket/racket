
;;----------------------------------------------------------------------
;; #%stxcase-scheme: adds let-syntax, syntax-rules, and
;;  check-duplicate-identifier, and assembles everything we have so far

(module stxcase-scheme '#%kernel
  (#%require "define-et-al.rkt" "qq-and-or.rkt" "stx.rkt" "stxcase.rkt" "with-stx.rkt" "stxloc.rkt"
             (for-syntax '#%kernel "define-et-al.rkt" "stx.rkt" "stxcase.rkt"
                         "stxloc.rkt"))

  (-define (check-duplicate-identifier names)
    (unless (and (list? names) (andmap identifier? names))
      (raise-argument-error 'check-duplicate-identifier "(listof identifier?)" names))
    (let-values ([(dup origs) (stx-find-duplicate-identifiers names)])
      dup))

  (begin-for-syntax
   (define-values (check-sr-rules)
     (lambda (stx kws)
       (for-each (lambda (id)
                   (unless (identifier? id)
                     (raise-syntax-error
                      #f
                      "pattern must start with an identifier, found something else"
                      stx
                      id)))
                 (syntax->list kws)))))
  
  ;; From Dybvig, mostly:
  (-define-syntax syntax-rules
    (lambda (stx)
      (syntax-case** syntax-rules #t stx () free-identifier=? #f
	((sr (k ...) ((keyword . pattern) template) ...)
	 (andmap identifier? (syntax->list (syntax (k ...))))
	 (begin
           (check-sr-rules stx (syntax (keyword ...)))
	   (syntax/loc stx
	     (lambda (x)
	       (syntax-case** sr #t x (k ...) free-identifier=? #f
		 ((_ . pattern) (syntax/loc x template))
		 ...))))))))

  (-define-syntax syntax-id-rules
    (lambda (x)
      (syntax-case** syntax-id-rules #t x () free-identifier=? #f
	((sidr (k ...) (pattern template) ...)
	 (andmap identifier? (syntax->list (syntax (k ...))))
	 (syntax/loc x
	   (make-set!-transformer
	    (lambda (x)
	      (syntax-case** sidr #t x (k ...) free-identifier=? #f
		(pattern (syntax/loc x template))
		...))))))))

  (-define (syntax-protect stx)
    (if (syntax? stx)
        stx
        (raise-argument-error 'syntax-protect "syntax?" stx)))

  (#%provide syntax datum (all-from "with-stx.rkt") (all-from "stxloc.rkt") 
             check-duplicate-identifier syntax-protect
             syntax-rules syntax-id-rules
             (for-syntax syntax-pattern-variable?)))
