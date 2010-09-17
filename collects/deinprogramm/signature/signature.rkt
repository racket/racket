#lang scheme/base

(provide signature?
	 signature-name signature-syntax
	 signature-arbitrary set-signature-arbitrary!
	 signature-info-promise
	 signature-violation
	 signature-violation-proc call-with-signature-violation-proc
	 make-delayed-signature
	 make-call-signature
	 make-property-signature
	 make-predicate-signature
	 make-type-variable-signature
	 make-list-signature
	 make-mixed-signature
	 make-combined-signature
	 make-case-signature
	 make-procedure-signature
	 signature-update-syntax signature-update-info-promise
	 apply-signature apply-signature/blame
	 procedure-signature-info? 
	 procedure-signature-info-arg-signatures procedure-signature-info-return-signature
	 make-lazy-wrap-info lazy-wrap-info-constructor lazy-wrap-info-raw-accessors
	 prop:lazy-wrap lazy-wrap? lazy-wrap-ref
	 make-struct-wrap-signature
	 check-struct-wraps!
	 signature=? signature<=?)

(require scheme/promise
	 mzlib/struct
	 (for-syntax scheme/base)
	 (for-syntax stepper/private/shared))

(require deinprogramm/quickcheck/quickcheck)

(define (signature=? c1 c2)
  (or (eq? c1 c2)
      (eq? (signature-enforcer c1) (signature-enforcer c2))
      (and (signature-=?-proc c1)
	   ((signature-=?-proc c1)
	    (force (signature-info-promise c1)) 
	    (force (signature-info-promise c2))))))

; name may be #f
; enforcer: signature val -> val
; 
; syntax: syntax data from where the signature was defined

(define-struct signature (name enforcer syntax-promise (arbitrary-promise #:mutable) info-promise <=?-proc =?-proc)
  #:constructor-name really-make-signature
  #:property prop:equal+hash
  (list (lambda (c1 c2 equal?) (signature=? c1 c2)) ; #### use equal?
	(lambda (r recur)
	  (+ (recur (signature-name r))
	     (* 33 (recur (signature-enforcer r)))))
	(lambda (r recur)
	  (+ (* 33 (recur (signature-name r)))
	     (recur (signature-enforcer r)))))
  #:property prop:custom-write
  (lambda (r port write?)
    (cond
     ((signature-name r)
      => (lambda (name)
	   (display "#<signature " port)
	   (display name port)
	   (display ">" port)))
     (else
      (display "#<signature>" port)))))

(define (make-signature name enforcer syntax-promise
		       #:arbitrary-promise (arbitrary-promise #f)
		       #:info-promise (info-promise (delay #f))
		       #:<=?-proc (<=?-proc
				   (lambda (this-info other-info)
				     #f))
		       #:=?-proc (=?-proc
				 (lambda (this-info other-info)
				   #f)))
  (really-make-signature name enforcer syntax-promise arbitrary-promise info-promise <=?-proc =?-proc))

(define (signature-syntax sig)
  (force (signature-syntax-promise sig)))

(define (signature-arbitrary sig)
  (force (signature-arbitrary-promise sig)))

(define (set-signature-arbitrary! sig arb)
  (set-signature-arbitrary-promise! sig (delay arb)))

(define (signature-update-syntax sig stx)
  (struct-copy signature sig (syntax-promise (delay stx))))

;; it's a promise because of ordering constraints in the structs
(define (signature-update-info-promise sig inf)
  (struct-copy signature sig (info-promise inf)))

; message may be #f
(define signature-violation-proc (make-parameter (lambda (obj signature message blame)
						  (raise (make-exn:fail:contract (or message
										     (format "got ~e" obj))
										 (current-continuation-marks))))))

(define (signature-violation obj signature msg blame)
  ((signature-violation-proc) obj signature msg blame))

(define (call-with-signature-violation-proc proc thunk)
  (parameterize ((signature-violation-proc proc))
    (thunk)))

(define (make-delayed-signature name promise)
  (make-signature name
		 (lambda (self obj)
		   ((signature-enforcer (force promise)) self obj))
		 (delay (signature-syntax (force promise)))
		 #:arbitrary-promise
		 (delay
		   (force (signature-arbitrary-promise (force promise))))
		 #:info-promise
		 (delay
		   (force (signature-info-promise (force promise))))
		 #:<=?-proc
		 (lambda (this-info other-info)
		   ((signature-<=?-proc (force promise)) this-info other-info))
		 #:=?-proc
		 (lambda (this-info other-info)
		   ((signature-=?-proc (force promise)) this-info other-info))))

; specialized version of the above, supports comparison
; the promise must produce the result of (proc . args), but its passed separately
; to give us the right location on backtrace
(define (make-call-signature name promise proc-promise args-promise syntax)
  (make-signature name
		 (lambda (self obj)
		   ((signature-enforcer (force promise)) self obj))
		 (delay syntax)
		 #:arbitrary-promise
		 (delay
		   (force (signature-arbitrary-promise (force promise))))
		 #:info-promise
		 (delay
		   (make-call-info promise (force proc-promise) (force args-promise)))
		 #:=?-proc
		 (lambda (this-info other-info)
		   (and (call-info? other-info)
			(eqv? (force proc-promise) (call-info-proc other-info))
			(equal? (force args-promise) (call-info-args other-info))))))

(define-struct call-info (promise proc args) #:transparent)

; klude to support mixed
(define (real-signature-info sig)
  (let ((raw-info (force (signature-info-promise sig))))
    (if (call-info? raw-info)
	(real-signature-info (force (call-info-promise raw-info)))
	(force raw-info))))

(define (make-property-signature name access signature syntax)
  (let ((enforce (signature-enforcer signature)))
    (make-signature name
		   (lambda (self obj)
		     (enforce self (access obj)) ; #### problematic: enforcement doesn't stick
		     obj)
		   syntax)))

(define (make-predicate-signature name predicate-promise syntax)
  (make-signature
   name
   (lambda (self obj) ; dynamic binding because of syntax remapping via `signature-update-syntax'
     (if ((force predicate-promise) obj)
	 obj
	 (begin
	   (signature-violation obj self #f #f)
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

(define (make-type-variable-signature name syntax)
  (make-signature
   name
   (lambda (self obj) obj)
   (delay syntax)
   #:info-promise
   (delay (make-type-variable-info))
   #:=?-proc
   (lambda (this-info other-info)
     (type-variable-info? other-info))))

(define-struct type-variable-info ())

; maps lists to pairs of signature, enforced value
(define lists-table (make-weak-hasheq))

(define (make-list-signature name arg-signature syntax)
  (make-signature
   name
   (lambda (self obj)
     ;;(write (list 'list obj) (current-error-port)) (newline (current-error-port))
     (let recur ((l obj))

       (define (go-on)
	 (let ((enforced (cons (apply-signature arg-signature (car l))
			       (recur (cdr l)))))
	   (hash-set! lists-table l (cons self enforced))
	   (hash-set! lists-table enforced (cons self enforced))
	   enforced))
       
       (cond
	((null? l)
	 l)
	((not (pair? l))
	 (signature-violation obj self #f #f)
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
     (lift->arbitrary arbitrary-list arg-signature))
   #:info-promise
   (delay (make-list-info arg-signature))
   #:=?-proc
   (lambda (this-info other-info)
     (and (list-info? other-info)
	  (signature=? arg-signature (list-info-arg-signature other-info))))))

(define-struct list-info (arg-signature) #:transparent)

(define (lift->arbitrary proc . signatures)
  (let ((arbitraries (map force (map signature-arbitrary-promise signatures))))
    (if (andmap values arbitraries)
	(apply proc arbitraries)
	#f)))

(define (make-mixed-signature name alternative-signatures syntax)
  (letrec ((alternative-signatures-promise 
	    (delay
	      (normalize-mixed-signatures mixed-signature alternative-signatures)))
	   (mixed-signature
	    (make-signature
	     name
	     (lambda (self obj)
	       (let loop ((alternative-signatures (force alternative-signatures-promise)))
		 (cond
		  ((null? alternative-signatures)
		   (signature-violation obj self #f #f)
		   obj)
		  ((eq? (car alternative-signatures) self)
		   (raise
		    (make-exn:fail:contract
		     (string->immutable-string
		      (if name
			  (format "rekursive Signatur: ~a" name)
			  "rekursive Signatur"))
		     (current-continuation-marks))))
		  (else
		   (check-signature (car alternative-signatures)
				    obj
				    values
				    (lambda () (loop (cdr alternative-signatures))))))))
	     (delay syntax)
	     #:info-promise
	     (delay (make-mixed-info (force alternative-signatures-promise)))
	     #:arbitrary-promise
	     (delay
	       (let ((arbitraries (map force (map signature-arbitrary-promise (force alternative-signatures-promise)))))
		 (if (andmap values arbitraries)
		     (arbitrary-mixed 
		      (map (lambda (sig arb)
			     (cons (signature->predicate sig)
				   arb))
			   (force alternative-signatures-promise) arbitraries))
		     #f)))
	     #:=?-proc
	     (lambda (this-info other-info)
	       (and (mixed-info? other-info)
		    (andmap signature=? 
			    (mixed-info-signatures this-info) 
			    (mixed-info-signatures other-info))))
	     #:<=?-proc
	     (lambda (this-info other-info)
	       (and (mixed-info? other-info)
		    (mixed-signature<=? (mixed-info-signatures this-info) 
					(mixed-info-signatures other-info)))))))
    mixed-signature))

(define (mixed-signature<=? sigs1 sigs2)
  (andmap (lambda (sig1)
	    (ormap (lambda (sig2)
		     (signature<=? sig1 sig2))
		   sigs2))
	  sigs1))

; Flatten out mixed signatures, and fold in in the struct-wrap
; signatures
	
(define (normalize-mixed-signatures mixed-signature sigs)
  (fold-struct-wrap-signatures mixed-signature (flatten-mixed-signatures sigs)))

(define (flatten-mixed-signatures sigs)
  (apply append
	 (map (lambda (sig)
		(let ((info (force (signature-info-promise sig))))
		  (if (mixed-info? info)
		      (mixed-info-signatures info)
		      (list sig))))
	      sigs)))

(define-struct mixed-info (signatures) #:transparent)

(define (check-signature sig val success fail)
  ((let/ec exit
     (let ((enforced
	    (call-with-signature-violation-proc
	     (lambda (signature syntax msg blame)
	       (exit fail))
	     (lambda ()
	       (apply-signature sig val)))))
       (lambda () (success enforced))))))

(define (signature->predicate sig)
  (lambda (val)
    (check-signature sig val (lambda (_) #t) (lambda () #f))))

(define (make-combined-signature name signatures syntax)
  (make-signature
   name
   (lambda (self obj)
     (let ((old-violation-proc (signature-violation-proc)))
       ((let/ec exit
	  (call-with-signature-violation-proc
	   (lambda (signature syntax msg blame)
	     (exit
	      (lambda ()
		(old-violation-proc signature syntax msg blame)
		obj)))
	   (lambda ()
	     (let loop ((signatures signatures)
			(obj obj))
	       (if (null? signatures)
		   (lambda () obj)
		   (loop (cdr signatures)
			 (apply-signature (car signatures) obj))))))))))
   (delay syntax)))

(define (make-case-signature name cases =? syntax)
  (make-signature
   name
   (lambda (self obj)
     (let loop ((cases cases))
       (cond
	((null? cases)
	 (signature-violation obj self #f #f)
	 obj)
	((=? (car cases) obj)
	 obj)
	(else
	 (loop (cdr cases))))))
   (delay syntax)
   #:arbitrary-promise
   (delay (apply arbitrary-one-of =? cases))))

(define-struct procedure-to-blame (proc syntax)
  #:property prop:procedure 0)

(define signature-key (gensym 'signature-key))

(define-struct procedure-signature-info (arg-signatures return-signature) #:transparent)

(define (make-procedure-signature name arg-signatures return-signature syntax)
  (let ((arg-count (length arg-signatures)))
    (make-signature
     name
     (lambda (self thing)
       (let-values (((proc blame-syntax)
		     (if (procedure-to-blame? thing)
			 (values (procedure-to-blame-proc thing)
				 (procedure-to-blame-syntax thing))
			 (values thing #f))))
	 (cond
	  ((not (procedure? proc))
	   (signature-violation proc self #f #f)
	   thing)
	  ((not (procedure-arity-includes? proc arg-count)) ; #### variable arity
	   (signature-violation proc self "falsche Anzahl von Parametern" #f)
	   thing)
	  (else
	   (attach-name
	    (object-name proc)
	    (procedure-reduce-arity
	     (lambda args
	       (call-with-immediate-continuation-mark
		signature-key
		(lambda (maybe)
		  (if (not (= (length args) arg-count))
		      (begin
			(signature-violation proc self "falsche Anzahl von Argumenten" #f)
			(apply-signature return-signature (apply proc args)))
		      (let* ((old-violation-proc (signature-violation-proc))
			     (arg-violation? #f)
			     (args
			      (call-with-signature-violation-proc
			       (lambda (obj signature message blame)
				 (set! arg-violation? #t)
				 (old-violation-proc obj signature message blame))
			       (lambda ()
				 (map apply-signature arg-signatures args)))))
			(if (eq? maybe return-signature)
			    (apply proc args)
			    (let ((retval
				   (with-continuation-mark 
				    signature-key return-signature
				    (apply proc args))))
			      (if arg-violation?
				  retval
				  (call-with-signature-violation-proc
				   (lambda (obj signature message _)
				     ;; blame the procedure
				     (old-violation-proc obj signature message blame-syntax))
				   (lambda ()
				     (apply-signature return-signature retval)))))))))))
	     (procedure-arity proc)))))))
     (delay syntax)
     #:arbitrary-promise
     (delay
       (apply lift->arbitrary arbitrary-procedure return-signature arg-signatures))
     #:info-promise
     (delay
       (make-procedure-signature-info arg-signatures return-signature)))))

(define (attach-name name thing)
  (if (and (procedure? thing)
	   (symbol? name))
      (procedure-rename thing name)
      thing))

; Lazy signature checking for structs

;; This is attached prop:lazy-wrap property of struct types subject to
;; lazy checking.
(define-struct lazy-wrap-info
  (constructor 
   raw-accessors raw-mutators
   ;; procedures for referencing or setting an additional field within the struct
   ;; that field contains a list of lists of unchecked field signatures
   ref-proc set!-proc))

; value should be a lazy-wrap-info
(define-values (prop:lazy-wrap lazy-wrap? lazy-wrap-ref)
  (make-struct-type-property 'lazy-wrap))

; The field accessed by ref-proc and set!-proc contains one of these:

(define-struct lazy-wrap-log
  ;; list of lazy-log-not-checked
  (not-checked 
  ;; list of lists of field signatures
   checked)
  #:transparent)

; This situation makes trouble:
; (make-mixed-signature (make-struct-wrap-signature ...)  (make-struct-wrap-signature ...) ...)

; We need to push the `mixed' signature inside the struct-wrap
; signature, which is why the struct-map signature has an implicit
; `mixed'.
; To this end, a `lazy-log-not-checked' object tracks a list of
; `mixed' alternatives.  The `mixed-signature' field tracks from which
; `mixed' contract the mixture has originally come from.  
; (It may be #f, in which case the `field-signatures-list' is a one-element list.)
(define-struct lazy-log-not-checked
  (mixed-signature field-signatures-list)
  #:transparent)

(define (make-struct-wrap-signature name type-descriptor field-signatures syntax)
  (really-make-struct-wrap-signature name type-descriptor #f (list field-signatures) syntax))

; The lists of signatures in `field-signatures-list' form an implicit mixed signature.
(define (really-make-struct-wrap-signature name type-descriptor 
					   mixed-signature field-signatures-list
					   syntax)
  (let ((lazy-wrap-info (lazy-wrap-ref type-descriptor))
	(not-checked (make-lazy-log-not-checked mixed-signature field-signatures-list))
	(struct-wrap-info (make-struct-wrap-info type-descriptor field-signatures-list))
	(predicate (lambda (thing)
		     (and (struct? thing)
			  (let-values (((thing-descriptor _) (struct-info thing)))
			    (eq? thing-descriptor type-descriptor))))))
    (let ((constructor (lazy-wrap-info-constructor lazy-wrap-info))
	  (raw-accessors (lazy-wrap-info-raw-accessors lazy-wrap-info))
	  (wrap-ref (lazy-wrap-info-ref-proc lazy-wrap-info))
	  (wrap-set! (lazy-wrap-info-set!-proc lazy-wrap-info)))
    (make-signature
     name
      (lambda (self thing)

       (if (not (predicate thing))
	   (signature-violation thing self #f #f)
	   (let ((log (wrap-ref thing)))
	     (cond
	      ((not log)
	       (wrap-set! thing
			  (make-lazy-wrap-log (list not-checked) '())))
	      ((not (let ()
		      (define (<=? sigs1 sigs2)
			(andmap signature<=? sigs1 sigs2))
		      (define (check wrap-field-signatures)
			(ormap (lambda (field-signatures)
				 (<=? wrap-field-signatures field-signatures))
			       field-signatures-list))
		      (or (ormap (lambda (wrap-not-checked)
				   (andmap check 
					   (lazy-log-not-checked-field-signatures-list wrap-not-checked)))
				 (lazy-wrap-log-not-checked log))
			  (ormap check (lazy-wrap-log-checked log)))))
	       (wrap-set! thing
			  (make-lazy-wrap-log (cons not-checked (lazy-wrap-log-not-checked log))
					      (lazy-wrap-log-checked log)))))))
       
       thing)
     (delay syntax)
     #:info-promise
     (delay struct-wrap-info)
     #:=?-proc
     (lambda (this-info other-info)
       (and (struct-wrap-info? other-info)
	    (struct-wrap-info-field-signatures-list other-info)
	    (eq? type-descriptor (struct-wrap-info-descriptor other-info))
	    (andmap (lambda (this-field-signatures)
		      (andmap (lambda (other-field-signatures)
				(andmap signature=? this-field-signatures other-field-signatures))
			      (struct-wrap-info-field-signatures-list other-info)))
		    (struct-wrap-info-field-signatures-list this-info))))
     #:<=?-proc
     (lambda (this-info other-info)
       (and (struct-wrap-info? other-info)
	    (struct-wrap-info-field-signatures-list other-info) 
	    (eq? type-descriptor (struct-wrap-info-descriptor other-info))
	    (andmap (lambda (this-field-signatures)
		      (ormap (lambda (other-field-signatures)
			       (andmap signature<=? this-field-signatures other-field-signatures))
			      (struct-wrap-info-field-signatures-list other-info)))
		    (struct-wrap-info-field-signatures-list this-info))))))))

(define-struct struct-wrap-info (descriptor field-signatures-list) #:transparent)

(define (check-struct-wraps! thing)
  (let-values (((descriptor skipped?) (struct-info thing)))
    (let ((lazy-wrap-info (lazy-wrap-ref descriptor)))
      
      (let ((constructor (lazy-wrap-info-constructor lazy-wrap-info))
	    (raw-accessors (lazy-wrap-info-raw-accessors lazy-wrap-info))
	    (raw-mutators (lazy-wrap-info-raw-mutators lazy-wrap-info))
	    (wrap-ref (lazy-wrap-info-ref-proc lazy-wrap-info))
	    (wrap-set! (lazy-wrap-info-set!-proc lazy-wrap-info)))

	(let ((log (wrap-ref thing)))
	  (when (and log (pair? (lazy-wrap-log-not-checked log)))
	    (let loop ((field-vals (map (lambda (raw-accessor)
					  (raw-accessor thing))
					raw-accessors))
		       (now-checked '())
		       (not-checkeds (lazy-wrap-log-not-checked log)))
	      (if (null? not-checkeds)
		  (begin
		    (for-each (lambda (raw-mutator field-val)
				(raw-mutator thing field-val))
			      raw-mutators field-vals)
		    (wrap-set! thing
			       (make-lazy-wrap-log '()
						   (append now-checked
							   (lazy-wrap-log-checked log)))))
		  (let ((not-checked (car not-checkeds)))
		    (let ((field-signatures-list (lazy-log-not-checked-field-signatures-list not-checked))
			  (mixed-signature (lazy-log-not-checked-mixed-signature not-checked)))
		    (if (not mixed-signature) ; one-element list
			(loop (map apply-signature (car field-signatures-list) field-vals)
			      (cons (car field-signatures-list) now-checked)
			      (cdr not-checkeds))
			(let inner ((field-signatures-list field-signatures-list)) ; implicit mixed
			  (if (null? field-signatures-list)
			      (signature-violation thing mixed-signature #f #f)
			      (let map ((sigs (car field-signatures-list))
					(field-vals field-vals)
					(new-field-vals '()))
				(if (null? sigs)
				    (loop (reverse new-field-vals)
					  (cons (car field-signatures-list) now-checked)
					  (cdr not-checkeds))
				    (check-signature (car sigs)
						     (car field-vals)
						     (lambda (new-val)
						       (map (cdr sigs)
							    (cdr field-vals)
							    (cons new-val new-field-vals)))
						     (lambda ()
						       (inner (cdr field-signatures-list)))))))))))))))))))

; pushes down mixed contracts
(define (fold-struct-wrap-signatures mixed-signature sigs)
  (let ((struct-wrap-sigs (make-hasheq))) ; maps a type descriptor to signatures

    (define (push-down-struct-wrap-sigs)
      (hash-map struct-wrap-sigs
		(lambda (type-desc signatures)
		  (really-make-struct-wrap-signature 
		   (signature-name (car signatures)) type-desc 
		   mixed-signature
		   (apply append
			  (map (lambda (sig)
				 (struct-wrap-info-field-signatures-list (real-signature-info sig)))
			       signatures))
		   (signature-syntax (car signatures))))))
  
    (let loop ((sigs sigs)
	       (vanilla-sigs '()))
      (if (null? sigs)
	  (append (push-down-struct-wrap-sigs)
		  (reverse vanilla-sigs))
	  (let* ((sig (car sigs))
		 (info (real-signature-info sig)))
	    (if (struct-wrap-info? info)
		(let ((type-desc (struct-wrap-info-descriptor info)))
		  (hash-update! struct-wrap-sigs
				type-desc
				(lambda (old)
				  (cons sig old))
				(lambda ()
				  (list sig)))
		  (loop (cdr sigs) vanilla-sigs))
		(loop (cdr sigs) (cons sig vanilla-sigs))))))))

; like apply-signature, but can track more precise blame into the signature itself
(define-syntax apply-signature/blame
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?cnt-exp ?val-exp)
       (syntax-case (local-expand #'?val-exp 'expression #f) (lambda #%plain-lambda)
	 ((lambda ?params ?body0 ?body1 ...)
	  (stepper-syntax-property
	   ;; remember there's an implicit #%app
	   #'(apply-signature ?cnt-exp
			     (make-procedure-to-blame ?val-exp
						      #'?val-exp))
	   'stepper-skipto/discard
	   '(syntax-e cdr syntax-e cdr cdr car
		      syntax-e cdr syntax-e cdr car)))
	 ((#%plain-lambda ?params ?body0 ?body1 ...)
	  (stepper-syntax-property
	   #'(apply-signature ?cnt-exp
			     (make-procedure-to-blame ?val-exp
						      #'?val-exp))
	   'stepper-skipto/discard
	   '(syntax-e cdr syntax-e cdr cdr car
		      syntax-e cdr syntax-e cdr car)))
	 (_
	  (stepper-syntax-property
	   #'(apply-signature ?cnt-exp ?val-exp)
	   'stepper-skipto/discard
	   '(syntax-e cdr syntax-e cdr cdr car))))))))

(define (apply-signature signature val)
  ((signature-enforcer signature) signature val))

; "do the values that fulfill c1 also fulfill c2?"
(define (signature<=? c1 c2)
  (or (signature=? c1 c2)
      (let ((i1 (force (signature-info-promise c1)))
	    (i2 (force (signature-info-promise c2))))
	(or (type-variable-info? i2) ; kludge, maybe dispatch should be on second arg
	    (and i1 i2
		 ((signature-<=?-proc c1) i1 i2))))))
	     

