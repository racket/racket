#lang scheme/base

(provide contract?
	 contract-name contract-syntax
	 contract-arbitrary set-contract-arbitrary!
	 contract-info-promise
	 contract-violation
	 contract-violation-proc call-with-contract-violation-proc
	 make-delayed-contract
	 make-call-contract
	 make-property-contract
	 make-predicate-contract
	 make-type-variable-contract
	 make-list-contract
	 make-mixed-contract
	 make-combined-contract
	 make-case-contract
	 make-procedure-contract
	 contract-update-syntax contract-update-info-promise
	 apply-contract apply-contract/blame
	 procedure-contract-info? 
	 procedure-contract-info-arg-contracts procedure-contract-info-return-contract
	 make-lazy-wrap-info lazy-wrap-info-constructor lazy-wrap-info-raw-accessors
	 prop:lazy-wrap lazy-wrap? lazy-wrap-ref
	 make-struct-wrap-contract
	 check-struct-wraps!
	 contract=? contract<=?)

(require scheme/promise
	 mzlib/struct
	 (for-syntax scheme/base)
	 (for-syntax stepper/private/shared))

(require deinprogramm/quickcheck/quickcheck)

(define (contract=? c1 c2)
  (or (eq? c1 c2)
      (eq? (contract-enforcer c1) (contract-enforcer c2))
      (and (contract-=?-proc c1)
	   ((contract-=?-proc c1)
	    (force (contract-info-promise c1)) 
	    (force (contract-info-promise c2))))))

; name may be #f
; enforcer: contract val -> val
; 
; syntax: syntax data from where the contract was defined

(define-struct contract (name enforcer syntax-promise (arbitrary-promise #:mutable) info-promise <=?-proc =?-proc)
  #:constructor-name really-make-contract
  #:transparent ; #### for debugging, remove
  #:property prop:equal+hash
  (list (lambda (c1 c2 equal?) (contract=? c1 c2)) ; #### use equal?
	void void)) ; hash procs

(define (make-contract name enforcer syntax-promise
		       #:arbitrary-promise (arbitrary-promise #f)
		       #:info-promise (info-promise (delay #f))
		       #:<=?-proc (<=?-proc
				   (lambda (this-info other-info)
				     #f))
		       #:=?-proc (=?-proc
				 (lambda (this-info other-info)
				   #f)))
  (really-make-contract name enforcer syntax-promise arbitrary-promise info-promise <=?-proc =?-proc))

(define (contract-syntax ctr)
  (force (contract-syntax-promise ctr)))

(define (contract-arbitrary ctr)
  (force (contract-arbitrary-promise ctr)))

(define (set-contract-arbitrary! ctr arb)
  (set-contract-arbitrary-promise! ctr (delay arb)))

(define (contract-update-syntax ctr stx)
  (struct-copy contract ctr (syntax-promise (delay stx))))

;; it's a promise because of ordering constraints in the structs
(define (contract-update-info-promise ctr inf)
  (struct-copy contract ctr (info-promise inf)))

; message may be #f
(define contract-violation-proc (make-parameter (lambda (obj contract message blame)
						  (raise (make-exn:fail:contract (or message
										     (format "got ~e" obj))
										 (current-continuation-marks))))))

(define (contract-violation obj contract msg blame)
  ((contract-violation-proc) obj contract msg blame))

(define (call-with-contract-violation-proc proc thunk)
  (parameterize ((contract-violation-proc proc))
    (thunk)))

(define (make-delayed-contract name promise)
  (make-contract name
		 (lambda (self obj)
		   ((contract-enforcer (force promise)) self obj))
		 (delay (contract-syntax (force promise)))
		 #:arbitrary-promise
		 (delay
		   (force (contract-arbitrary-promise (force promise))))
		 #:info-promise
		 (delay
		   (force (contract-info-promise (force promise))))
		 #:<=?-proc
		 (lambda (this-info other-info)
		   ((contract-<=?-proc (force promise)) this-info other-info))
		 #:=?-proc
		 (lambda (this-info other-info)
		   ((contract-=?-proc (force promise)) this-info other-info))))

; specialized version of the above, supports comparison
; the promise must produce the result of (proc . args), but its passed separately
; to give us the right location on backtrace
(define (make-call-contract name promise proc-promise args-promise syntax)
  (make-contract name
		 (lambda (self obj)
		   ((contract-enforcer (force promise)) self obj))
		 (delay syntax)
		 #:arbitrary-promise
		 (delay
		   (force (contract-arbitrary-promise (force promise))))
		 #:info-promise
		 (delay
		   (make-call-info (force proc-promise) (force args-promise)))
		 #:=?-proc
		 (lambda (this-info other-info)
		   (and (call-info? other-info)
			(eqv? (force proc-promise) (call-info-proc other-info))
			(equal? (force args-promise) (call-info-args other-info))))))

(define-struct call-info (proc args) #:transparent)

(define (make-property-contract name access contract syntax)
  (let ((enforce (contract-enforcer contract)))
    (make-contract name
		   (lambda (self obj)
		     (enforce self (access obj)) ; #### problematic: enforcement doesn't stick
		     obj)
		   syntax)))

(define (make-predicate-contract name predicate-promise syntax)
  (make-contract
   name
   (lambda (self obj) ; dynamic binding because of syntax remapping via `contract-update-syntax'
     (if ((force predicate-promise) obj)
	 obj
	 (begin
	   (contract-violation obj self #f #f)
	   obj)))
   (delay syntax)
   #:info-promise
   (delay (make-predicate-info (force predicate-promise)))
   #:=?-proc
   (lambda (this-info other-info)
     (and (predicate-info? other-info)
	  (eq? (force predicate-promise)
	       (predicate-info-predicate other-info))))))

(define-struct predicate-info (predicate) #:transparent)

(define (make-type-variable-contract name syntax)
  (make-contract
   name
   (lambda (self obj) obj)
   (delay syntax)
   #:info-promise
   (delay (make-type-variable-info))
   #:=?-proc
   (lambda (this-info other-info)
     (type-variable-info? other-info))))

(define-struct type-variable-info ())

; maps lists to pairs of contract, enforced value
(define lists-table (make-weak-hasheq))

(define (make-list-contract name arg-contract syntax)
  (make-contract
   name
   (lambda (self obj)
     ;;(write (list 'list obj) (current-error-port)) (newline (current-error-port))
     (let recur ((l obj))

       (define (go-on)
	 (let ((enforced (cons (apply-contract arg-contract (car l))
			       (recur (cdr l)))))
	   (hash-set! lists-table l (cons self enforced))
	   (hash-set! lists-table enforced (cons self enforced))
	   enforced))
       
       (cond
	((null? l)
	 l)
	((not (pair? l))
	 (contract-violation obj self #f #f)
	 obj)
	((hash-ref lists-table l #f)
	 => (lambda (seen)
	      ;;(write (list 'seen seen (eq? self (car seen))) (current-error-port)) (newline (current-error-port))
	      (if (eq? self (car seen))
		  (cdr seen)
		  (go-on))))
	(else
	 (go-on)))))
   syntax
   #:arbitrary-promise
   (delay
     (lift->arbitrary arbitrary-list arg-contract))
   #:info-promise
   (delay (make-list-info arg-contract))
   #:=?-proc
   (lambda (this-info other-info)
     (and (list-info? other-info)
	  (contract=? arg-contract (list-info-arg-contract other-info))))))

(define-struct list-info (arg-contract) #:transparent)

(define (lift->arbitrary proc . contracts)
  (let ((arbitraries (map force (map contract-arbitrary-promise contracts))))
    (if (andmap values arbitraries)
	(apply proc arbitraries)
	#f)))

(define (make-mixed-contract name alternative-contracts syntax)
  (make-contract
   name
   (lambda (self obj)
     (let loop ((alternative-contracts alternative-contracts))
       (cond
	((null? alternative-contracts)
	 (contract-violation obj self #f #f)
	 obj)
	((eq? (car alternative-contracts) self)
	 (raise
	  (make-exn:fail:contract
	   (string->immutable-string
	    (if name
		(format "rekursiver Vertrag: ~a" name)
		"rekursiver Vertrag"))
	   (current-continuation-marks))))
	(else
	 (check-contract (car alternative-contracts)
			 obj
			 values
			 (lambda () (loop (cdr alternative-contracts))))))))
   (delay syntax)
   #:arbitrary-promise
   (delay
     (let ((arbitraries (map force (map contract-arbitrary-promise alternative-contracts))))
       (if (andmap values arbitraries)
	   (arbitrary-mixed 
	    (map (lambda (ctr arb)
		   (cons (contract->predicate ctr)
			 arb))
		 alternative-contracts arbitraries))
	   #f)))))

(define (check-contract ctr val success fail)
  ((let/ec exit
     (let ((enforced
	    (call-with-contract-violation-proc
	     (lambda (contract syntax msg blame)
	       (exit fail))
	     (lambda ()
	       (apply-contract ctr val)))))
       (lambda () (success enforced))))))

(define (contract->predicate ctr)
  (lambda (val)
    (check-contract ctr val (lambda (_) #t) (lambda () #f))))

(define (make-combined-contract name contracts syntax)
  (make-contract
   name
   (lambda (self obj)
     (let ((old-violation-proc (contract-violation-proc)))
       ((let/ec exit
	  (call-with-contract-violation-proc
	   (lambda (contract syntax msg blame)
	     (exit
	      (lambda ()
		(old-violation-proc contract syntax msg blame)
		obj)))
	   (lambda ()
	     (let loop ((contracts contracts)
			(obj obj))
	       (if (null? contracts)
		   (lambda () obj)
		   (loop (cdr contracts)
			 (apply-contract (car contracts) obj))))))))))
   (delay syntax)))

(define (make-case-contract name cases =? syntax)
  (make-contract
   name
   (lambda (self obj)
     (let loop ((cases cases))
       (cond
	((null? cases)
	 (contract-violation obj self #f #f)
	 obj)
	((=? (car cases) obj)
	 obj)
	(else
	 (loop (cdr cases))))))
   (delay syntax)
   #:arbitrary-promise
   (delay (apply arbitrary-one-of =? cases))))

(define-struct procedure-to-blame (proc syntax))

(define contract-key (gensym 'contract-key))

(define-struct procedure-contract-info (arg-contracts return-contract) #:transparent)

(define (make-procedure-contract name arg-contracts return-contract syntax)
  (let ((arg-count (length arg-contracts)))
    (make-contract
     name
     (lambda (self thing)
       (let-values (((proc blame-syntax)
		     (if (procedure-to-blame? thing)
			 (values (procedure-to-blame-proc thing)
				 (procedure-to-blame-syntax thing))
			 (values thing #f))))
	 (cond
	  ((not (procedure? proc))
	   (contract-violation proc self #f #f))
	  ((not (procedure-arity-includes? proc arg-count)) ; #### variable arity
	   (contract-violation proc self "falsche Anzahl von Parametern" #f))
	  (else
	   (attach-name
	    (object-name proc)
	    (procedure-reduce-arity
	     (lambda args
	       (call-with-immediate-continuation-mark
		contract-key
		(lambda (maybe)
		  (if (not (= (length args) arg-count))
		      (begin
			(contract-violation proc self "falsche Anzahl von Argumenten" #f)
			(apply-contract return-contract (apply proc args)))
		      (let* ((old-violation-proc (contract-violation-proc))
			     (arg-violation? #f)
			     (args
			      (call-with-contract-violation-proc
			       (lambda (obj contract message blame)
				 (set! arg-violation? #t)
				 (old-violation-proc obj contract message blame))
			       (lambda ()
				 (map apply-contract arg-contracts args)))))
			(if (eq? maybe return-contract)
			    (apply proc args)
			    (let ((retval
				   (with-continuation-mark 
				    contract-key return-contract
				    (apply proc args))))
			      (if arg-violation?
				  retval
				  (call-with-contract-violation-proc
				   (lambda (obj contract message _)
				     ;; blame the procedure
				     (old-violation-proc obj contract message blame-syntax))
				   (lambda ()
				     (apply-contract return-contract retval)))))))))))
	     (procedure-arity proc)))))))
     (delay syntax)
     #:arbitrary-promise
     (delay
       (apply lift->arbitrary arbitrary-procedure return-contract arg-contracts))
     #:info-promise
     (delay
       (make-procedure-contract-info arg-contracts return-contract)))))

(define (attach-name name thing)
  (if (and (procedure? thing)
	   (symbol? name))
      (procedure-rename thing name)
      thing))

; Lazy contract checking for structs

;; This is attached prop:lazy-wrap property of struct types subject to
;; lazy checking.
(define-struct lazy-wrap-info
  (constructor 
   raw-accessors raw-mutators
   ;; procedures for referencing or setting an additional field within the struct
   ;; that field contains a list of lists of unchecked field contracts
   ref-proc set!-proc))

; value should be a lazy-wrap-info
(define-values (prop:lazy-wrap lazy-wrap? lazy-wrap-ref)
  (make-struct-type-property 'lazy-wrap))

(define (make-struct-wrap-contract name type-descriptor field-contracts syntax)
  (let ((lazy-wrap-info (lazy-wrap-ref type-descriptor))
	(struct-wrap-info (make-struct-wrap-info type-descriptor field-contracts))
	(predicate (lambda (thing)
		     (and (struct? thing)
			  (let-values (((thing-descriptor _) (struct-info thing)))
			    (eq? thing-descriptor type-descriptor))))))
    (let ((constructor (lazy-wrap-info-constructor lazy-wrap-info))
	  (raw-accessors (lazy-wrap-info-raw-accessors lazy-wrap-info))
	  (wrap-ref (lazy-wrap-info-ref-proc lazy-wrap-info))
	  (wrap-set! (lazy-wrap-info-set!-proc lazy-wrap-info)))
    (make-contract
     name
     (lambda (self thing)

       (cond
	((not (predicate thing))
	 (contract-violation thing self #f #f)
	 thing)
	((ormap (lambda (wrap-field-contracts)
		  (andmap contract<=?
			  wrap-field-contracts
			  field-contracts))
		(wrap-ref thing))
	 thing)
	(else
	 (wrap-set! thing
		    (cons field-contracts (wrap-ref thing)))
	 thing)))
     (delay syntax)
     #:info-promise
     (delay struct-wrap-info)
     #:=?-proc
     (lambda (this-info other-info)
       (and (struct-wrap-info? other-info)
	    (struct-wrap-info-field-contracts other-info)
	    (eq? type-descriptor (struct-wrap-info-descriptor other-info))
	    (andmap contract=?
		    field-contracts
		    (struct-wrap-info-field-contracts other-info))))
     #:<=?-proc
     (lambda (this-info other-info)
       (and (struct-wrap-info? other-info)
	    (struct-wrap-info-field-contracts other-info) 
	    (eq? type-descriptor (struct-wrap-info-descriptor other-info))
	    (andmap contract<=?
		    field-contracts
		    (struct-wrap-info-field-contracts other-info))))))))

(define-struct struct-wrap-info (descriptor field-contracts))

(define (check-struct-wraps! thing)
  (let-values (((descriptor skipped?) (struct-info thing)))
    (let ((lazy-wrap-info (lazy-wrap-ref descriptor)))
      
      (let ((constructor (lazy-wrap-info-constructor lazy-wrap-info))
	    (raw-accessors (lazy-wrap-info-raw-accessors lazy-wrap-info))
	    (raw-mutators (lazy-wrap-info-raw-mutators lazy-wrap-info))
	    (wrap-ref (lazy-wrap-info-ref-proc lazy-wrap-info))
	    (wrap-set! (lazy-wrap-info-set!-proc lazy-wrap-info)))

	(when (pair? (wrap-ref thing)) ; fast path
	  (let loop ((field-vals (map (lambda (raw-accessor)
					(raw-accessor thing))
				      raw-accessors))
		     (field-contracts-list (wrap-ref thing)))
	    (if (null? field-contracts-list)
		(begin
		  (for-each (lambda (raw-mutator field-val)
			      (raw-mutator thing field-val))
			    raw-mutators field-vals)
		  (wrap-set! thing '()))
		(loop (map apply-contract (car field-contracts-list) field-vals)
		      (cdr field-contracts-list)))))))))

; like apply-contract, but can track more precise blame into the contract itself
(define-syntax apply-contract/blame
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?cnt-exp ?val-exp)
       (syntax-case (local-expand #'?val-exp 'expression #f) (lambda #%plain-lambda)
	 ((lambda ?params ?body0 ?body1 ...)
	  (stepper-syntax-property
	   ;; remember there's an implicit #%app
	   #'(apply-contract ?cnt-exp
			     (make-procedure-to-blame ?val-exp
						      #'?val-exp))
	   'stepper-skipto/discard
	   '(syntax-e cdr syntax-e cdr cdr car
		      syntax-e cdr syntax-e cdr car)))
	 ((#%plain-lambda ?params ?body0 ?body1 ...)
	  (stepper-syntax-property
	   #'(apply-contract ?cnt-exp
			     (make-procedure-to-blame ?val-exp
						      #'?val-exp))
	   'stepper-skipto/discard
	   '(syntax-e cdr syntax-e cdr cdr car
		      syntax-e cdr syntax-e cdr car)))
	 (_
	  (stepper-syntax-property
	   #'(apply-contract ?cnt-exp ?val-exp)
	   'stepper-skipto/discard
	   '(syntax-e cdr syntax-e cdr cdr car))))))))

(define (apply-contract contract val)
  ((contract-enforcer contract) contract val))

; "do the values that fulfill c1 also fulfill c2?"
(define (contract<=? c1 c2)
  (or (contract=? c1 c2)
      (let ((i1 (force (contract-info-promise c1)))
	    (i2 (force (contract-info-promise c2))))
	(or (type-variable-info? i2) ; kludge, maybe dispatch should be on second arg
	    (and i1 i2
		 ((contract-<=?-proc c1) i1 i2))))))
	     

