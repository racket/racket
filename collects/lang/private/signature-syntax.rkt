#lang scheme/base

(provide :
	 signature signature/arbitrary
	 define/signature define-values/signature
	 -> mixed one-of predicate combined property list-of vector-of)

(require deinprogramm/signature/signature
	 deinprogramm/signature/signature-english
	 scheme/promise
	 (for-syntax scheme/base)
	 (for-syntax syntax/stx)
         (for-syntax stepper/private/syntax-property)
	 (for-syntax "firstorder.rkt"))

(define-for-syntax (phase-lift stx)
  (with-syntax ((?stx stx))
    (with-syntax ((?stx1 (syntax/loc stx #'?stx))) ; attach the occurrence position to the syntax object
      #'?stx1)))

(define-for-syntax (parse-signature name stx)
  (syntax-case* stx
		(mixed one-of predicate list-of vector-of -> combined property reference at signature)
		module-or-top-identifier=?
    ((mixed ?signature ...)
     (with-syntax ((?stx (phase-lift stx))
		   (?name name)
		   ((?signature-expr ...) (map (lambda (sig)
						(parse-signature #f sig))
					      (syntax->list #'(?signature ...)))))
       #'(make-mixed-signature '?name
			      (list ?signature-expr ...)
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
					       (error 'signatures "no signature permissible here, value required"))))
				 #'(when (signature? ?temp)
				     ?raise))))
			   (syntax->list #'((?temp ?exp) ...)))))
         #'(let ((?temp ?exp) ...)
             ?check ...
             (make-case-signature '?name (list ?temp ...) equal? ?stx)))))
    ((predicate ?exp)
     (with-syntax ((?stx (phase-lift stx))
		   (?name name))
       #'(make-predicate-signature '?name (delay ?exp) ?stx)))
    ((list-of ?signature)
     (with-syntax ((?stx (phase-lift stx))
		   (?name name)
		   (?signature-expr (parse-signature #f #'?signature)))
       #'(make-list-signature '?name ?signature-expr ?stx)))
    ((list-of ?signature1 ?rest ...)
     (raise-syntax-error #f
			 "list-of signature accepts only a single operand"
			 (syntax ?signature1)))
    ((vector-of ?signature)
     (with-syntax ((?stx (phase-lift stx))
		   (?name name)
		   (?signature-expr (parse-signature #f #'?signature)))
       #'(make-vector-signature '?name ?signature-expr ?stx)))
    ((vector-of ?signature1 ?rest ...)
     (raise-syntax-error #f
			 "vector-of signature accepts only a single operand"
			 (syntax ?signature1)))
    ((?arg-signature ... -> ?return-signature)
     (with-syntax ((?stx (phase-lift stx))
		   (?name name)
		   ((?arg-signature-exprs ...) (map (lambda (sig)
						     (parse-signature #f sig))
						   (syntax->list #'(?arg-signature ...))))
		   (?return-signature-expr (parse-signature #f #'?return-signature)))
       #'(make-procedure-signature '?name (list ?arg-signature-exprs ...) ?return-signature-expr ?stx)))
    ((?arg-signature ... -> ?return-signature1 ?return-signature2 . ?_)
     (raise-syntax-error #f
			 "only one signature is allowed after ->"
			 (syntax ?return-signature2)))
    ((at ?loc ?sig)
     (with-syntax ((?sig-expr (parse-signature #f #'?sig)))
       #'(signature-update-syntax ?sig-expr #'?loc)))
    (signature
     (with-syntax ((?stx (phase-lift stx)))
       #'(signature-update-syntax signature/signature #'?loc)))
    (?id
     (identifier? #'?id)
     (with-syntax ((?stx (phase-lift stx))
		   (?name (or name (syntax->datum #'?id))))
       (let ((name (symbol->string (syntax->datum #'?id))))
	 (if (char=? #\% (string-ref name 0))
	     #'(make-type-variable-signature '?name ?stx)
	     (with-syntax
		 ((?raise
                   #'(error 'signatures "expected a signature, found ~e" ?id)))
	       (with-syntax
		   ((?sig
		     #'(make-delayed-signature '?name
					      (delay
						(begin
						  (when (not (signature? ?id))
						    ?raise)
						  ?id)))))
		 ;; for local variables (parameters, most probably),
		 ;; we want the value to determine the blame location
		 (if (eq? (identifier-binding #'?id) 'lexical)
		     #'?sig
		     #'(signature-update-syntax ?sig #'?stx))))))))
    ((combined ?signature ...)
     (with-syntax ((?stx (phase-lift stx))
		   (?name name)
		   ((?signature-expr ...) (map (lambda (sig)
						(parse-signature #f sig))
					      (syntax->list #'(?signature ...)))))
       #'(make-combined-signature '?name
				 (list ?signature-expr ...)
				 ?stx)))
    ((property ?access ?signature)
     (with-syntax ((?stx (phase-lift stx))
		   (?name name)
		   (?signature-expr (parse-signature #f #'?signature)))
       #'(make-property-signature '?name
				 ?access
				 ?signature-expr
				 ?stx)))
    ((signature ?stuff ...)
     (raise-syntax-error #f
			 "`signature' makes no sense as an operator"
			 stx))
    ((?signature-abstr ?signature ...)
     (identifier? #'?signature-abstr)
     (with-syntax ((?stx (phase-lift stx))
		   (?name name)
		   ((?signature-expr ...) (map (lambda (sig)
						(parse-signature #f sig))
					      (syntax->list #'(?signature ...)))))
       (with-syntax
	   ((?call (syntax/loc stx (?signature-abstr ?signature-expr ...)))
	    (?signature-abstr-ho (first-order->higher-order #'?signature-abstr)))
	 #'(make-call-signature '?name
			       (delay ?call)
			       (delay ?signature-abstr-ho)
			       (delay (list ?signature-expr ...))
			       ?stx))))
    (else
     (raise-syntax-error #f
			 "invalid signature" stx))))

; regrettable
(define signature/signature
  (make-predicate-signature 'signature
			   (delay signature?)
			   #f))
			     

(define-syntax signature
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?contr)
       #'(signature #f ?contr))
      ((_ ?name ?contr)
       (stepper-syntax-property
	(parse-signature (syntax->datum #'?name) #'?contr)
	'stepper-skip-completely #t)))))

(define-syntax signature/arbitrary
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?arb ?contr . ?rest)
       #'(let ((contr (signature ?contr . ?rest)))
	   (set-signature-arbitrary! contr ?arb)
	   contr)))))

(define-syntax define/signature
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?name ?cnt ?expr)
       (with-syntax ((?enforced
		       (stepper-syntax-property #'(attach-name '?name (apply-signature/blame ?cnt ?expr))
						'stepper-skipto/discard
						;; apply-signature/blame takes care of itself
						;; remember there's an implicit #%app
						'(syntax-e cdr syntax-e cdr cdr car))))
							   
	 #'(define ?name ?enforced))))))

(define-syntax define-values/signature
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
       #'(define/signature ?id ?cnt ?expr)) ; works with stepper
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
			      #'(attach-name '?id (apply-signature/blame ?cnt ?id))))))
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
    ((: ?id ?sig) (begin)))) ; probably never used, we're only interested in the binding for :

(define-for-syntax (within-signature-syntax-error stx name)
  (raise-syntax-error #f
		      "may only occur within signatures"
		      name))

;; Expression -> Expression
;; Transforms unfinished code (... and the like) to code
;; raising an appropriate error.
(define-for-syntax within-signature-syntax-transformer
  (make-set!-transformer
   (lambda (stx)
     (syntax-case stx (set!)
       [(set! form expr) (within-signature-syntax-error stx (syntax form))]
       [(form . rest) (within-signature-syntax-error stx (syntax form))]
       [form (within-signature-syntax-error stx stx)]))))

(define-syntax -> within-signature-syntax-transformer)
(define-syntax mixed within-signature-syntax-transformer)
(define-syntax one-of within-signature-syntax-transformer)
(define-syntax predicate within-signature-syntax-transformer)
(define-syntax combined within-signature-syntax-transformer)
(define-syntax property within-signature-syntax-transformer)
(define-syntax list-of within-signature-syntax-transformer)
(define-syntax vector-of within-signature-syntax-transformer)
