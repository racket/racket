#lang scheme/base

(provide :
	 contract contract/arbitrary
	 define-contract
	 define/contract define-values/contract
	 -> mixed one-of predicate combined property list-of)

(require deinprogramm/contract/contract
	 scheme/promise
	 (for-syntax scheme/base)
	 (for-syntax syntax/stx)
	 (for-syntax stepper/private/shared)
	 (only-in lang/private/teachprims beginner-equal?))

(define-for-syntax (phase-lift stx)
  (with-syntax ((?stx stx))
    (with-syntax ((?stx1 (syntax/loc stx #'?stx))) ; attach the occurrence position to the syntax object
      #'?stx1)))

(define-for-syntax (parse-contract name stx)
  (syntax-case* stx
		(mixed one-of predicate list-of -> combined property reference at contract)
		module-or-top-identifier=?
    ((mixed ?contract ...)
     (with-syntax ((?stx (phase-lift stx))
		   (?name name)
		   ((?contract-expr ...) (map (lambda (ctr)
						(parse-contract #f ctr))
					      (syntax->list #'(?contract ...)))))
       #'(make-mixed-contract '?name
			      (list ?contract-expr ...)
			      ?stx)))
    ((one-of ?exp ...)
     (with-syntax ((((?temp ?exp) ...) 
		    (map list
			 (generate-temporaries #'(?exp ...)) (syntax->list #'(?exp ...))))
		   (?stx (phase-lift stx))
		   (?name name))
       (with-syntax (((?check ...) 
		      (map (lambda (lis)
			     (with-syntax (((?temp ?exp) lis))
			       (with-syntax ((?raise
					      (syntax/loc 
					       #'?exp
					       (error 'contracts "no contract permissible here, value required"))))
				 #'(when (contract? ?temp)
				     ?raise))))
			   (syntax->list #'((?temp ?exp) ...)))))
       #'(let ((?temp ?exp) ...)
	   ?check ...
	   (make-case-contract '?name (list ?temp ...) beginner-equal? ?stx)))))
    ((predicate ?exp)
     (with-syntax ((?stx (phase-lift stx))
		   (?name name))
       #'(make-predicate-contract '?name (delay ?exp) ?stx)))
    ((list-of ?contract)
     (with-syntax ((?stx (phase-lift stx))
		   (?name name)
		   (?contract-expr (parse-contract #f #'?contract)))
       #'(make-list-contract '?name ?contract-expr ?stx)))
    ((list-of ?contract1 ?rest ...)
     (raise-syntax-error #f
			 "list-of contract accepts only a single operand"
			 (syntax ?contract1)))
    ((?arg-contract ... -> ?return-contract)
     (with-syntax ((?stx (phase-lift stx))
		   (?name name)
		   ((?arg-contract-exprs ...) (map (lambda (ctr)
						     (parse-contract #f ctr))
						   (syntax->list #'(?arg-contract ...))))
		   (?return-contract-expr (parse-contract #f #'?return-contract)))
       #'(make-procedure-contract '?name (list ?arg-contract-exprs ...) ?return-contract-expr ?stx)))
    ((?arg-contract ... -> ?return-contract1 ?return-contract2 . ?_)
     (raise-syntax-error #f
			 "only one contract is allowed after ->"
			 (syntax ?return-contract2)))
    ((at ?loc ?ctr)
     (with-syntax ((?ctr-expr (parse-contract #f #'?ctr)))
       #'(contract-update-syntax ?ctr-expr #'?loc)))
    (contract
     (with-syntax ((?stx (phase-lift stx)))
       #'(contract-update-syntax contract/contract #'?loc)))
    (?id
     (identifier? #'?id)
     (with-syntax ((?stx (phase-lift stx))
		   (?name name))
       (let ((name (symbol->string (syntax->datum #'?id))))
	 (if (char=? #\% (string-ref name 0))
	     #'(make-type-variable-contract '?id ?stx)
	     (with-syntax
		 ((?raise
		   (syntax/loc #'?stx
			       (error 'contracts "expected a contract, found ~e" ?id))))
	       (with-syntax
		   ((?ctr
		     #'(make-delayed-contract '?name
					      (delay
						(begin
						  (when (not (contract? ?id))
						    ?raise)
						  ?id)))))
		 ;; for local variables (parameters, most probably),
		 ;; we want the value to determine the blame location
		 (if (eq? (identifier-binding #'?id) 'lexical)
		     #'?ctr
		     #'(contract-update-syntax ?ctr #'?stx))))))))
    ((combined ?contract ...)
     (with-syntax ((?stx (phase-lift stx))
		   (?name name)
		   ((?contract-expr ...) (map (lambda (ctr)
						(parse-contract #f ctr))
					      (syntax->list #'(?contract ...)))))
       #'(make-combined-contract '?name
				 (list ?contract-expr ...)
				 ?stx)))
    ((property ?access ?contract)
     (with-syntax ((?stx (phase-lift stx))
		   (?name name)
		   (?contract-expr (parse-contract #f #'?contract)))
       #'(make-property-contract '?name
				 ?access
				 ?contract-expr
				 ?stx)))
    ((?contract-abstr ?contract ...)
     (identifier? #'?contract-abstr)
     (with-syntax ((?stx (phase-lift stx))
		   (?name name)
		   ((?contract-expr ...) (map (lambda (ctr)
						(parse-contract #f ctr))
					      (syntax->list #'(?contract ...)))))
       (with-syntax
	   ((?call (syntax/loc stx (?contract-abstr ?contract-expr ...))))
	 #'(make-call-contract '?name
			       (delay ?call)
			       (delay ?contract-abstr) (delay (list ?contract-expr ...))
			       ?stx))))
    (else
     (raise-syntax-error 'contract
			 "invalid contract" stx))))

; regrettable
(define contract/contract
  (make-predicate-contract 'contract
			   (delay contract?)
			   #f))
			     

(define-syntax contract
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?contr)
       #'(contract #f ?contr))
      ((_ ?name ?contr)
       (stepper-syntax-property
	(parse-contract (syntax->datum #'?name) #'?contr)
	'stepper-skip-completely #t)))))

(define-syntax contract/arbitrary
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?arb ?contr . ?rest)
       #'(let ((contr (contract ?contr . ?rest)))
	   (set-contract-arbitrary! contr ?arb)
	   contr)))))

(define-syntax define-contract
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?name ?ctr)
       (identifier? #'?name)
       (stepper-syntax-property #'(define ?name (contract ?name ?ctr))
				'stepper-skip-completely
				#t))
      ((_ (?name ?param ...) ?ctr)
       (and (identifier? #'?name)
	    (andmap identifier? (syntax->list #'(?param ...))))
       (stepper-syntax-property #'(define (?name ?param ...) (contract ?name ?ctr))
				'stepper-skip-completely
				#t)))))

(define-syntax define/contract
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?name ?cnt ?expr)
       (with-syntax ((?enforced
		       (stepper-syntax-property #'(attach-name '?name (apply-contract/blame ?cnt ?expr))
						'stepper-skipto/discard
						;; apply-contract/blame takes care of itself
						;; remember there's an implicit #%app
						'(syntax-e cdr syntax-e cdr cdr car))))
							   
	 #'(define ?name ?enforced))))))

(define-syntax define-values/contract
  (lambda (stx)
    (syntax-case stx ()
      ((_ (?id ...) ?expr)
       (andmap identifier? (syntax->list #'(?id ...)))
       (syntax-track-origin
	#'(define-values (?id ...) ?expr)
	stx
	(car (syntax-e stx))))
      ((_ ((?id ?cnt)) ?expr)
       (identifier? #'?id)
       #'(define/contract ?id ?cnt ?expr)) ; works with stepper
      ((_ (?bind ...) ?expr)
       (let ((ids+enforced
	      (map (lambda (bind)
		     (syntax-case bind ()
		       (?id
			(identifier? #'?id)
			(cons #'?id #'?id))
		       ((?id ?cnt)
			(identifier? #'?id)
			(cons #'?id
			      #'(attach-name '?id (apply-contract/blame ?cnt ?id))))))
		   (syntax->list #'(?bind ...)))))
	 (with-syntax (((?id ...) (map car ids+enforced))
		       ((?enforced ...) (map cdr ids+enforced)))
	   (stepper-syntax-property
	    #'(define-values (?id ...)
		(call-with-values
		    (lambda () ?expr)
		  (lambda (?id ...)
		    (values ?enforced ...))))
	    'stepper-skip-completely #t)))))))

;; Matthew has promised a better way of doing this in the future.
(define (attach-name name thing)
  (if (procedure? thing)
      (procedure-rename thing name)
      thing))

(define-syntax :
  (syntax-rules ()
    ((: ?id ?ctr) (begin)))) ; probably never used, we're only interested in the binding for :

(define-for-syntax (within-contract-syntax-error stx name)
  (raise-syntax-error #f
		      "may only occur within contracts"
		      name))

;; Expression -> Expression
;; Transforms unfinished code (... and the like) to code
;; raising an appropriate error.
(define-for-syntax within-contract-syntax-transformer
  (make-set!-transformer
   (lambda (stx)
     (syntax-case stx (set!)
       [(set! form expr) (within-contract-syntax-error stx (syntax form))]
       [(form . rest) (within-contract-syntax-error stx (syntax form))]
       [form (within-contract-syntax-error stx stx)]))))

(define-syntax -> within-contract-syntax-transformer)
(define-syntax mixed within-contract-syntax-transformer)
(define-syntax one-of within-contract-syntax-transformer)
(define-syntax predicate within-contract-syntax-transformer)
(define-syntax combined within-contract-syntax-transformer)
(define-syntax property within-contract-syntax-transformer)
(define-syntax list-of within-contract-syntax-transformer)
