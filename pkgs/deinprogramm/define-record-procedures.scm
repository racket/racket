(define-syntax define-record-procedures*
  
  (let ()
    (define (filter-map proc l)
      (if (null? l)
	  '()
	  (let ((result (proc (car l))))
	    (if result
		(cons result (filter-map proc (cdr l)))
		(filter-map proc (cdr l))))))
    
    
    (define (syntax-member? thing stuff)
      (cond
       ((null? stuff) #f)
       ((free-identifier=? thing (car stuff)) #t)
       (else (syntax-member? thing (cdr stuff)))))

    (define (map-with-index proc list)
      (let loop ((i 0) (list list) (rev-result '()))
	(if (null? list)
	    (reverse rev-result)
	    (loop (+ 1 i)
		  (cdr list)
		  (cons (proc i (car list)) rev-result)))))

    (lambda (x)
      (syntax-case x ()
	((_ ?stx
	    ?type-name
	    ?mutable?
	    ?signature-constructor-name
	    ?constructor
	    ?predicate
	    (?field-spec ...))

	 (with-syntax
	     ((number-of-fields (length (syntax->list (syntax (?field-spec ...)))))
	      ((accessor ...)
	       (map (lambda (field-spec)
		      (syntax-case field-spec ()
			((accessor mutator) (syntax accessor))
			(accessor (syntax accessor))))
		    (syntax->list (syntax (?field-spec ...)))))
	      ((mutator ...)
	       (map (lambda (field-spec dummy-mutator)
		      (syntax-case field-spec ()
			((accessor mutator) (syntax mutator))
			(accessor dummy-mutator)))
		    (syntax->list (syntax (?field-spec ...)))
		    (generate-temporaries (syntax (?field-spec ...))))))
	   (with-syntax
	       (((accessor-proc ...)
		 (map-with-index
		  (lambda (i accessor)
		    (with-syntax ((i i)
				  (tag accessor))
		      (syntax-property (syntax/loc
					accessor
					(lambda (s)
					  (when (not (raw-predicate s))
					    (raise
					     (make-exn:fail:contract
					      (string->immutable-string
					       (format "~a: Argument kein ~a: ~e" 
						       'tag '?type-name s))
					      (current-continuation-marks))))
					  (raw-generic-access s i)))
				       'inferred-name
				       (syntax-e accessor))))
		  (syntax->list #'(accessor ...))))
		((our-accessor ...) (generate-temporaries #'(accessor ...)))
		((mutator-proc ...)
		 (map-with-index
		  (lambda (i mutator)
		    (with-syntax ((i i)
				  (tag mutator))
		      (syntax-property (syntax/loc
					mutator
					(lambda (s v)
					  (when (not (raw-predicate s))
					    (raise
					     (make-exn:fail:contract
					      (string->immutable-string
					       (format "~a: Argument kein ~a: ~e" 
						       'tag '?type-name s))
					      (current-continuation-marks))))
					  (raw-generic-mutate s i v)))
				       'inferred-name
				       (syntax-e mutator))))
		  (syntax->list #'(mutator ...))))
		(constructor-proc
		 (syntax-property #'(lambda (accessor ...)
				      (raw-constructor accessor ... #f))
				  'inferred-name
				  (syntax-e #'?constructor)))
		(predicate-proc
		 (syntax-property #'(lambda (thing)
				     (raw-predicate thing))
				  'inferred-name
				  (syntax-e #'?predicate)))
		((raw-accessor-proc ...)
		 (map-with-index (lambda (i _)
				   #`(lambda (r)
				       (raw-generic-access r #,i)))
				 (syntax->list #'(?field-spec ...))))
		((raw-mutator-proc ...)
		 (map-with-index (lambda (i _)
				   #`(lambda (r val)
				       (raw-generic-mutate r #,i val)))
				 (syntax->list #'(?field-spec ...))))

		(record-equal? #`(lambda (r1 r2 equal?)
				   (and #,@(map-with-index (lambda (i field-spec)
							     #`(equal? (raw-generic-access r1 #,i)
								       (raw-generic-access r2 #,i)))
							   (syntax->list #'(?field-spec ...)))))))

				 
	     (with-syntax
		 ((struct-type-defs
		   #'(define-values (type-descriptor
				     raw-constructor
				     raw-predicate
				     raw-generic-access
				     raw-generic-mutate)
		       (make-struct-type
			'?type-name #f (+ 1 number-of-fields) 0
			#f
			(list
			 (cons prop:print-convert-constructor-name
			       '?constructor)
			 (cons prop:custom-write
			       (lambda (r port write?)
				 (custom-write-record '?type-name 
						      (access-record-fields r raw-generic-access number-of-fields)
						      port write?)))
			 (cons prop:print-converter
			       (lambda (r recur)
				 (list '?constructor
				       (recur (raw-accessor-proc r)) ...)))
			 (cons prop:equal+hash
			       (list record-equal?
				     (make-equal-hash (lambda (r i) (raw-generic-access r i)) number-of-fields) 
				     (make-equal2-hash (lambda (r i) (raw-generic-access r i)) number-of-fields)))
			 (cons prop:lazy-wrap
			       (make-lazy-wrap-info constructor-proc
						    (list raw-accessor-proc ...)
						    (list raw-mutator-proc ...)
						    (lambda (r)
						      (raw-generic-access r number-of-fields))
						    (lambda (r val)
						      (raw-generic-mutate r number-of-fields val)))))
			(make-inspector))))
		  (constructor-def #'(define ?constructor constructor-proc))
		  (predicate-def #'(define-values (?predicate real-predicate)
				     (values predicate-proc predicate-proc)))
		  (accessor-defs #'(define-values (accessor ... our-accessor ...)
				     (values accessor-proc ... accessor-proc ...)))
		  (mutator-defs #'(define-values (mutator ...) (values mutator-proc ...)))
		  (signature-def
		   (with-syntax (((?param ...) (generate-temporaries #'(?field-spec ...))))
		     (with-syntax (((component-signature ...)
				    (map (lambda (accessor param)
					   (with-syntax ((?accessor accessor)
							 (?param param))
					     #'(at ?param (property ?accessor ?param))))
					 (syntax->list #'(our-accessor ...))
					 (syntax->list #'(?param ...)))))
		       (with-syntax ((base-signature
				      (stepper-syntax-property
				       #`(define ?type-name
					   (let ((sig (signature ?type-name (predicate real-predicate))))
					     #,(if (null? (syntax->list #'(?field-spec ...)))
						   #'(set-signature-arbitrary-promise!
						      sig
						      (delay (arbitrary-one-of equal? (?constructor))))
						   #'(begin))
					     sig))
				       'stepper-skip-completely 
				       #t))
				     (constructor-signature
				      (stepper-syntax-property
				       (if (syntax->datum #'?mutable?)
					   ;; no lazy signatures
					   #'(define (?signature-constructor-name ?param ...)
					       (signature
						(combined (at ?type-name (predicate real-predicate))
							  component-signature ...)))
					   ;; lazy signatures
					   #'(define (?signature-constructor-name ?param ...)
					       (let* ((sigs (list ?param ...))
						      (sig
						       (make-lazy-wrap-signature '?type-name #t
										 type-descriptor raw-predicate
										 sigs
										 #'?type-name)))
						 (set-signature-arbitrary-promise! 
						  sig
						  (delay
						    (let ((arbs (map signature-arbitrary sigs)))
						      (when (andmap values arbs)
							(apply arbitrary-record
							       ?constructor
							       (list raw-accessor-proc ...)
							       arbs)))))
						 sig)))
				       'stepper-skip-completely
				       #t)))
			 #'(begin
			     ;; we use real-predicate to avoid infinite recursion if a signature
			     ;; for ?type-name using ?predicate is inadvertently defined
			     base-signature
			     constructor-signature))))))
	       ;; again, with properties
	       (with-syntax ((struct-type-defs
			      (stepper-syntax-property
			       (syntax/loc x struct-type-defs) 'stepper-black-box-expr #'?stx))
			     (constructor-def
			      (stepper-syntax-property #'constructor-def 'stepper-skip-completely #t))
			     (predicate-def
			      (stepper-syntax-property #'predicate-def 'stepper-skip-completely #t))
			     (accessor-defs
			      (stepper-syntax-property #'accessor-defs 'stepper-skip-completely #t))
			     (mutator-defs
			      (stepper-syntax-property #'mutator-defs 'stepper-skip-completely #t)))
		 #'(begin
		     signature-def
		     ;; the signature might be used in the definitions, hence this ordering
		     struct-type-defs predicate-def constructor-def accessor-defs mutator-defs))))))
      ((_ ?type-name
	  ?signature-constructor-name
	  ?constructor
	  ?predicate
	  rest)
       (raise-syntax-error 
	#f 
	"Der vierte Operand ist illegal" (syntax rest)))
      ((_ ?type-name
	  ?signature-constructor-name
	  ?constructor
	  ?predicate
	  rest1 rest2 ... (?field-spec ...))
       (raise-syntax-error 
	#f 
	"Vor den Selektoren/Mutatoren steht eine Form zuviel" #'rest1))
      ((_ ?type-name
	  ?signature-constructor-name
	  ?constructor
	  ?predicate
	  rest1 rest2 ...)
       (raise-syntax-error 
	#f 
	"Zu viele Operanden für define-record-procedures*" x))
      ((_ arg1 ...)
       (raise-syntax-error 
	#f 
	"Zu wenige Operanden für define-record-procedures*" x))))))

(define (access-record-fields rec acc count)
  (let recur ((i 0))
    (if (= i count)
	'()
	(cons (acc rec i)
	      (recur (+ i 1))))))

(define (make-equal-hash generic-access field-count)
  (lambda (r recur)
    (let loop ((i 0)
	       (factor 1)
	       (hash 0))
      (if (= i field-count)
	  hash
	  (loop (+ 1 i)
		(* factor 33)
		(+ hash (* factor (recur (generic-access r i)))))))))

(define (make-equal2-hash generic-access field-count)
  (lambda (r recur)
    (let loop ((i 0)
	       (factor 1)
	       (hash 0))
      (if (= i field-count)
	  hash
	  (loop (+ 1 i)
		(* factor 33)
		(+ hash (* factor 
			   (recur (generic-access r (- field-count i 1))))))))))

#|
(define-record-procedures :pare kons pare? (kar kdr))
(kons 1 (kons 2 (kons 3 (kons 5 (kons 6 (kons 7 (kons 8 "asdjkfdshfdsjkf")))))))

prints as:

#<record:pare 1
              #<record:pare 2
                            #<record:pare 3
                                          #<record:pare 5
                                                        #<record:pare 6
                                                                      #<record:pare 7 #<record:pare 8 "asdjkfdshfdsjkf">>>>>>>

|#

(define (custom-write-record name field-values port write?)
  (let ((pp? (and (pretty-printing)
		  (number? (pretty-print-columns)))))

    (write-string "#<" port)
    (write-string "record" port)
    (let ((name (symbol->string name)))
      (when (not (and (positive? (string-length name))
		    (char=? #\: (string-ref name 0))))
	  (write-char #\: port))
      (write-string name port))

    (let-values (((ref-line ref-column ref-pos)
		  (if pp?
		      (port-next-location port)
		      (values 0 -1 0)))) ; to compensate for space
      (for-each
       (if pp?
	   (lambda (field-value)
	     (let* ((max-column (- (pretty-print-columns) 1)) ; > terminator
		    (tentative
		     (make-tentative-pretty-print-output-port
		      port
		      max-column
		      void)))
	       (display " " tentative)
	       ((if write? write display) field-value tentative)
	       (let-values (((line column pos) (port-next-location tentative)))
		 (if (< column max-column)
		     (tentative-pretty-print-port-transfer tentative port)
		     (begin
		       (tentative-pretty-print-port-cancel tentative)
		       (let ((count (pretty-print-newline port max-column)))
			 (write-string (make-string (max 0 (- (+ ref-column 1) count)) #\space) 
				       port)
			 ((if write? write display) field-value port)))))))
	   (lambda (field-value)
	       (display " " port)
	       ((if write? write display) field-value port)))
       field-values)
      
      (write-string ">" port))))

;; (define-record-procedures :pare kons pare? (kar kdr))

(define-syntax define-record-procedures
  (lambda (x)
    (syntax-case x ()
      ((_ ?type-name
          ?constructor
          ?predicate
          (accessor  ...))

       (begin
         (check-for-id!
          (syntax ?type-name)
          "Typ-Name ist kein Bezeichner")
         
         (check-for-id!
          (syntax ?constructor)
          "Konstruktor ist kein Bezeichner")
         
         (check-for-id!
          (syntax ?predicate)
          "Prädikat ist kein Bezeichner")
         
         (check-for-id-list! 
          (syntax->list (syntax (accessor ...)))
          "Selektor ist kein Bezeichner")

         (with-syntax ((?stx x)
		       ((dummy-mutator ...)
                        (generate-temporaries (syntax (accessor ...)))))
           (syntax
            (define-record-procedures* ?stx ?type-name #f
	      dummy-signature-constructor-name
              ?constructor
              ?predicate
              ((accessor dummy-mutator) ...))))))

       ((_ ?type-name
           ?constructor
           ?predicate
           rest)
        (raise-syntax-error 
         #f 
         "Der vierte Operand ist keine Liste von Selektoren" (syntax rest)))
       ((_ ?type-name
           ?constructor
           ?predicate
           rest1 rest2 ... (accessor ...))
        (raise-syntax-error 
         #f 
         "Vor den Selektoren steht eine Form zuviel" #'rest1))
       ((_ ?type-name
           ?constructor
           ?predicate
           rest1 rest2 ...)
        (raise-syntax-error 
         #f 
         "Zu viele Operanden für define-record-procedures" x))
       ((_ arg1 ...)
        (raise-syntax-error 
         #f 
         "Zu wenige Operanden für define-record-procedures" x))
      )))

(define-syntax define-record-procedures-parametric
  (lambda (x)
    (syntax-case x ()
      ((_ ?type-name
	  ?signature-constructor-name
          ?constructor
          ?predicate
          (accessor  ...))


       (begin
	 (check-for-id!
	  (syntax ?type-name)
	  "Record-Name ist kein Bezeichner")

	 (check-for-id!
	  (syntax ?signature-constructor-name)
	  "Signaturkonstruktor-Name ist kein Bezeichner")

         (check-for-id!
          (syntax ?constructor)
          "Konstruktor ist kein Bezeichner")

	 (check-for-id!
	  (syntax ?predicate)
	  "Prädikat ist kein Bezeichner")
         
	 (check-for-id-list! 
	  (syntax->list (syntax (accessor ...)))
	  "Selektor ist kein Bezeichner")

	 (with-syntax ((?stx x)
		       ((dummy-mutator ...)
			(generate-temporaries (syntax (accessor ...)))))
	   (syntax
	    (define-record-procedures* ?stx ?type-name #f ?signature-constructor-name
	      ?constructor
	      ?predicate
	      ((accessor dummy-mutator) ...))))))

      ((_ ?type-name
	  ?signature-constructor-name
	  ?constructor
	  ?predicate
	  rest)
       (raise-syntax-error 
	#f 
	"Der vierte Operand ist keine Liste von Selektoren" (syntax rest)))
      ((_ ?type-name
	  ?signature-constructor-name
	  ?constructor
	  ?predicate
	  rest1 rest2 ...)
       (raise-syntax-error 
	#f 
	"Zu viele Operanden für define-record-procedures-parametric" x))
      ((_ arg1 ...)
       (raise-syntax-error 
	#f 
	"Zu wenige Operanden für define-record-procedures-parametric" x))
      )))

;; (define-record-procedures-2 :pare kons pare? ((kar set-kar!) kdr))

(define-syntax define-record-procedures-2
  (lambda (x)
    (syntax-case x ()
      ((_ ?type-name
	  ?constructor
	  ?predicate
	  (?field-spec ...))

       (begin
	 (check-for-id!
	  (syntax ?type-name)
	  "Record-Name ist kein Bezeichner")
         
	 (check-for-id!
	  (syntax ?constructor)
	  "Konstruktor ist kein Bezeichner")
         
	 (check-for-id!
	  (syntax ?predicate)
	  "Prädikat ist kein Bezeichner")
	 
	 (for-each (lambda (field-spec)
		     (syntax-case field-spec ()
		       ((accessor mutator)
			(check-for-id! (syntax accessor)
				       "Selektor ist kein Bezeichner")
			(check-for-id! (syntax mutator)
				       "Mutator ist kein Bezeichner"))
		       (accessor
			(check-for-id! (syntax accessor)
				       "Selektor ist kein Bezeichner"))))
		   (syntax->list (syntax (?field-spec ...))))

	 (with-syntax ((?stx x))
	   #'(define-record-procedures* ?stx ?type-name #t
	       dummy-signature-constructor-name
	       ?constructor
	       ?predicate
	       (?field-spec ...)))))
      ((_ ?type-name
	  ?constructor
	  ?predicate
	  rest)
       (raise-syntax-error 
	#f 
	"Der vierte Operand ist illegal" (syntax rest)))
      ((_ ?type-name
	  ?constructor
	  ?predicate
	  rest1 rest2 ...)
       (raise-syntax-error 
	#f 
	"Zu viele Operanden für define-record-procedures-2" x))
      ((_ arg1 ...)
       (raise-syntax-error 
	#f 
	"Zu wenige Operanden für define-record-procedures-2" x)))))

(define-syntax define-record-procedures-parametric-2
  (lambda (x)
    (syntax-case x ()
      ((_ ?type-name
	  ?signature-constructor-name
	  ?constructor
	  ?predicate
	  (?field-spec ...))

       (begin
	 (check-for-id!
	  (syntax ?type-name)
	  "Record-Name ist kein Bezeichner")

	 (check-for-id!
	  (syntax ?signature-constructor-name)
	  "Signaturkonstruktor-Name ist kein Bezeichner")

	 (check-for-id!
	  (syntax ?constructor)
	  "Konstruktor ist kein Bezeichner")
         
	 (check-for-id!
	  (syntax ?predicate)
	  "Prädikat ist kein Bezeichner")
	 
	 (for-each (lambda (field-spec)
		     (syntax-case field-spec ()
		       ((accessor mutator)
			(check-for-id! (syntax accessor)
				       "Selektor ist kein Bezeichner")
			(check-for-id! (syntax mutator)
				       "Mutator ist kein Bezeichner"))
		       (accessor
			(check-for-id! (syntax accessor)
				       "Selektor ist kein Bezeichner"))))
		   (syntax->list (syntax (?field-spec ...))))

	 (with-syntax ((?stx x))
	   #'(define-record-procedures* ?stx ?type-name #t ?signature-constructor-name
	       ?constructor
	       ?predicate
	       (?field-spec ...)))))
      ((_ ?type-name
	  ?signature-constructor-name
	  ?constructor
	  ?predicate
	  rest)
       (raise-syntax-error 
	#f 
	"Der vierte Operand ist illegal" (syntax rest)))
      ((_ ?type-name
	  ?signature-constructor-name
	  ?constructor
	  ?predicate
	  rest1 rest2 ...)
       (raise-syntax-error 
	#f 
	"Zu viele Operanden für define-record-procedures-parametric-2" x))
      ((_ arg1 ...)
       (raise-syntax-error 
	#f 
	"Zu wenige Operanden für define-record-procedures-parametric-2" x)))))


