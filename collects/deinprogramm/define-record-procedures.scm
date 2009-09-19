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
	((_ ?type-name
	    ?contract-constructor-name
	    ?constructor
	    ?predicate
	    (?field-spec ...))

	 (with-syntax
	     (((accessor ...)
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
	       ((number-of-fields (length (syntax->list
					   (syntax (accessor ...)))))
		(generic-access (syntax generic-access))
		(generic-mutate (syntax generic-mutate)))
	     (with-syntax
		 (((accessor-proc ...)
		   (map-with-index
		    (lambda (i accessor)
		      (with-syntax ((i i)
				    (tag accessor))
			(syntax-property (syntax/loc
					  accessor
					  (lambda (s)
					    (when (not (?predicate s))
					      (raise
					       (make-exn:fail:contract
						(string->immutable-string
						 (format "~a: Argument kein ~a: ~e" 
							 'tag '?type-name s))
						(current-continuation-marks))))
					    (generic-access s i)))
					 'inferred-name
					 (syntax-e accessor))))
		    (syntax->list (syntax (accessor ...)))))
		  ((our-accessor ...) (generate-temporaries #'(accessor ...)))
		  ((mutator-proc ...)
		   (map-with-index
		    (lambda (i mutator)
		      (with-syntax ((i i)
				    (tag mutator))
			(syntax-property (syntax/loc
					  mutator
					  (lambda (s v)
					    (when (not (?predicate s))
					      (raise
					       (make-exn:fail:contract
						(string->immutable-string
						 (format "~a: Argument kein ~a: ~e" 
							 'tag '?type-name s))
						(current-continuation-marks))))
					    (generic-mutate s i v)))
					 'inferred-name
					 (syntax-e mutator))))
		    (syntax->list (syntax (mutator ...)))))
		  (constructor-proc
		   (syntax-property (syntax
				     (lambda (accessor ...)
				       (?constructor accessor ...)))
				    'inferred-name
				    (syntax-e (syntax ?constructor))))
		  (predicate-proc
		   (syntax-property (syntax
				     (lambda (thing)
				       (?predicate thing)))
				    'inferred-name
				    (syntax-e (syntax ?predicate))))
		  (constructor-name (syntax ?constructor)))
	       (with-syntax
		   ((defs
		      #'(define-values (?constructor
					?predicate real-predicate
					accessor ...
					our-accessor ...
					mutator ...)
			  (letrec-values (((type-descriptor
					    ?constructor
					    ?predicate
					    generic-access
					    generic-mutate)
					   (make-struct-type
					    '?type-name #f number-of-fields 0
					    #f
					    (list
					     (cons prop:print-convert-constructor-name
						   'constructor-name)
					     (cons prop:deinprogramm-struct
						   #t)
					     (cons prop:custom-write
						   (lambda (r port write?)
						     (custom-write-record '?type-name 
									  (access-record-fields r generic-access number-of-fields)
									  port write?))))
					    (make-inspector))))
			    (values constructor-proc
				    predicate-proc predicate-proc
				    accessor-proc ...
				    accessor-proc ...
				    mutator-proc ...))))
		    (contract
		     (with-syntax (((?param ...) (generate-temporaries #'(?field-spec ...))))
		       (with-syntax (((component-contract ...)
				      (map (lambda (accessor param)
					     (with-syntax ((?accessor accessor)
							   (?param param))
					       #'(at ?param (property ?accessor ?param))))
					   (syntax->list #'(our-accessor ...))
					   (syntax->list #'(?param ...)))))
			 (with-syntax ((base-contract
					(stepper-syntax-property
					 #'(define ?type-name (contract (predicate real-predicate)))
					 'stepper-skip-completely
					 #t))
				       (constructor-contract
					(stepper-syntax-property
					 #'(define (?contract-constructor-name ?param ...)
					     (contract
					      (combined (at ?type-name (predicate real-predicate))
							component-contract ...)))
					 'stepper-skip-completely
					 #t)))
			   #'(begin
			       ;; we use real-predicate to avoid infinite recursion if a contract
			       ;; for ?type-name using ?predicate is inadvertently defined
			       base-contract
			       constructor-contract))))))
		 (with-syntax ((defs
				 (stepper-syntax-property
				  (syntax/loc x defs) 'stepper-skip-completely #t)))
				 
		   #'(begin
		       contract
		       ;; the contract might be used in the definitions, hence this ordering
		       defs)))))))

      ((_ ?type-name
	  ?contract-constructor-name
	  ?constructor
	  ?predicate
	  rest)
       (raise-syntax-error 
	#f 
	"Der vierte Operand ist illegal" (syntax rest)))
      ((_ ?type-name
	  ?contract-constructor-name
	  ?constructor
	  ?predicate
	  rest1 rest2 ... (?field-spec ...))
       (raise-syntax-error 
	#f 
	"Vor den Selektoren/Mutatoren steht eine Form zuviel" #'rest1))
      ((_ ?type-name
	  ?contract-constructor-name
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

         (with-syntax (((dummy-mutator ...)
                        (generate-temporaries (syntax (accessor ...)))))
           (syntax
            (define-record-procedures* ?type-name
	      dummy-contract-constructor-name
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
	  ?contract-constructor-name
          ?constructor
          ?predicate
          (accessor  ...))


       (begin
	 (check-for-id!
	  (syntax ?type-name)
	  "Record-Name ist kein Bezeichner")

	 (check-for-id!
	  (syntax ?contract-constructor-name)
	  "Vertrags-Konstruktor-Name ist kein Bezeichner")

         (check-for-id!
          (syntax ?constructor)
          "Konstruktor ist kein Bezeichner")

	 (check-for-id!
	  (syntax ?predicate)
	  "Prädikat ist kein Bezeichner")
         
	 (check-for-id-list! 
	  (syntax->list (syntax (accessor ...)))
	  "Selektor ist kein Bezeichner")

	 (with-syntax (((dummy-mutator ...)
			(generate-temporaries (syntax (accessor ...)))))
	   (syntax
	    (define-record-procedures* ?type-name ?contract-constructor-name
	      ?constructor
	      ?predicate
	      ((accessor dummy-mutator) ...))))))

      ((_ ?type-name
	  ?contract-constructor-name
	  ?constructor
	  ?predicate
	  rest)
       (raise-syntax-error 
	#f 
	"Der vierte Operand ist keine Liste von Selektoren" (syntax rest)))
      ((_ ?type-name
	  ?contract-constructor-name
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

	 #'(define-record-procedures* ?type-name
	     dummy-contract-constructor-name
	     ?constructor
	     ?predicate
	     (?field-spec ...))))
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
	  ?contract-constructor-name
	  ?constructor
	  ?predicate
	  (?field-spec ...))

       (begin
	 (check-for-id!
	  (syntax ?type-name)
	  "Record-Name ist kein Bezeichner")

	 (check-for-id!
	  (syntax ?contract-constructor-name)
	  "Vertrags-Konstruktor-Name ist kein Bezeichner")

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

	 #'(define-record-procedures* ?type-name ?contract-constructor-name
	     ?constructor
	     ?predicate
	     (?field-spec ...))))
      ((_ ?type-name
	  ?contract-constructor-name
	  ?constructor
	  ?predicate
	  rest)
       (raise-syntax-error 
	#f 
	"Der vierte Operand ist illegal" (syntax rest)))
      ((_ ?type-name
	  ?contract-constructor-name
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


