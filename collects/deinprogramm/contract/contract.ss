#lang scheme/base

(provide contract?
	 contract-name contract-syntax
	 contract-violation-proc
	 call-with-contract-violation-proc
	 make-delayed-contract
	 make-property-contract
	 make-predicate-contract
	 make-type-variable-contract
	 make-list-contract
	 make-mixed-contract
	 make-combined-contract
	 make-case-contract
	 make-procedure-contract
	 contract-update-syntax
	 apply-contract apply-contract/blame)

(require scheme/promise
	 (for-syntax scheme/base)
	 (for-syntax stepper/private/shared))

; name may be #f
; enforcer: contract val -> val
; 
; syntax: syntax data from where the contract was defined

(define-struct contract (name enforcer syntax))

(define (contract-update-syntax ctr stx)
  (struct-copy contract ctr (syntax stx)))

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

(define (make-delayed-contract name promise syntax)
  (make-contract name
		 (lambda (self obj)
		   ((contract-enforcer (force promise)) self obj))
		 syntax))

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
   syntax))

(define (make-type-variable-contract name syntax)
  (make-predicate-contract name (lambda (obj) #t) syntax))

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
   syntax))

(define (make-mixed-contract name alternative-contracts syntax)
  (make-contract
   name
   (lambda (self obj)
     (let loop ((alternative-contracts alternative-contracts))
       (if (null? alternative-contracts)
	   (begin
	     (contract-violation obj self #f #f)
	     obj)
	   ((let/ec exit
	      (let ((enforced
		     (call-with-contract-violation-proc
		      (lambda (contract syntax msg blame)
			(exit (lambda () (loop (cdr alternative-contracts)))))
		      (lambda ()
			(let ((ctr (car alternative-contracts)))
			  (if (eq? ctr self)
			      (raise
			       (make-exn:fail:contract
				(string->immutable-string
				 (if name
				     (format "rekursiver Vertrag: ~a" name)
				     "rekursiver Vertrag"))
				(current-continuation-marks)))
			      (apply-contract ctr obj)))))))
		(lambda () enforced)))))))
   syntax))

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
   syntax))

(define (make-case-contract name cases syntax)
  (make-contract
   name
   (lambda (self obj)
     (let loop ((cases cases))
       (cond
	((null? cases)
	 (contract-violation obj self #f #f)
	 obj)
	((equal? (car cases) obj)
	 obj)
	(else
	 (loop (cdr cases))))))
   syntax))

(define-struct procedure-to-blame (proc syntax))

(define contract-key (gensym 'contract-key))

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
	   (contract-violation proc self "wrong number of parameters" #f)))
	 (attach-name
	  (object-name proc)
	  (lambda args
	    (call-with-immediate-continuation-mark
	     contract-key
	     (lambda (maybe)
	       (if (not (= (length args) arg-count))
		   (begin
		     (contract-violation proc self "wrong number of arguments" #f)
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
				  (apply-contract return-contract retval))))))))))))))
     syntax)))

;; Matthew has promised a better way of doing this in the future.
(define (attach-name name thing)
  (if (and (procedure? thing)
	   (symbol? name))
      (procedure-rename thing name)
      thing))

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
