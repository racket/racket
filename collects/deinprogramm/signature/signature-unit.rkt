#lang scheme/base
(provide signature-messages^ signatures^ signatures@)

(require racket/unit)

(require scheme/promise
	 mzlib/struct
         (only-in mzlib/list first rest)
	 (for-syntax scheme/base)
	 (for-syntax stepper/private/syntax-property))

(require deinprogramm/signature/signature)

(require deinprogramm/quickcheck/quickcheck)

(define-signature signature-messages^
  (recursive-signature-message
   parameter-count-mismatch-message
   argument-count-mismatch-message))

(define-signature signatures^
  (make-delayed-signature
   make-call-signature
   make-property-signature
   make-predicate-signature
   make-type-variable-signature
   make-list-signature
   make-vector-signature
   make-mixed-signature
   make-combined-signature
   make-case-signature
   make-procedure-signature
   procedure-signature-info? 
   procedure-signature-info-arg-signatures procedure-signature-info-return-signature
   make-lazy-wrap-info lazy-wrap-info-constructor lazy-wrap-info-raw-accessors
   prop:lazy-wrap lazy-wrap? lazy-wrap-ref
   make-lazy-wrap-signature
   check-lazy-wraps!
   make-pair-signature checked-car checked-cdr checked-first checked-rest))

(define-unit signatures@
  (import signature-messages^)
  (export signatures^)

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
		    (delay syntax))))

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

; maps lists to pairs of signature, enforced value
(define lists-table (make-weak-hasheq))

(define (make-list-signature name arg-signature syntax)
  (make-signature
   name
   (lambda (self obj)
     ;;(eprintf "~s\n" (list 'list obj))
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
	      ;;(eprintf "~s\n" (list 'seen seen (eq? self (car seen))))
	      (if (eq? self (car seen))
		  (cdr seen)
		  (go-on))))
	(else
	 (go-on)))))
   (delay syntax)
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

(define vectors-table (make-weak-hasheq)) ; #### ought to do ephemerons, too

(define (make-vector-signature name arg-signature syntax)
  (make-signature
   name
   (lambda (self obj)

     (define (check old-sigs)
       (let ((old-sigs (cons arg-signature old-sigs)))
	 (hash-set! vectors-table obj old-sigs)
	 (let* ((orig (vector->list obj))
		(els (map (lambda (x)
			    (apply-signature arg-signature x))
			  orig)))
	   (if (andmap eq? orig els)
	       obj
	       (let ((new (list->vector els)))
		 (hash-set! vectors-table obj old-sigs)
		 new)))))

     (cond
      ((not (vector? obj))
       (signature-violation obj self #f #f)
       obj)
      ((hash-ref vectors-table obj #f)
       => (lambda (old-sigs)
	    (if (ormap (lambda (old-sig)
			 (signature<=? old-sig arg-signature))
		       old-sigs)
		obj
		(check old-sigs))))
      (else
       (check '()))))
   (delay syntax)
   #:arbitrary-promise
   (delay
     (lift->arbitrary arbitrary-vector arg-signature))
   #:info-promise
   (delay (make-vector-info arg-signature))
   #:=?-proc
   (lambda (this-info other-info)
     (and (vector-info? other-info)
	  (signature=? arg-signature (vector-info-arg-signature other-info))))))

(define-struct vector-info (arg-signature) #:transparent)

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
			  (format "~a: ~a" recursive-signature-message)
			  recursive-signature-message))
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
	       (let* ((arbitrary-promises (map signature-arbitrary-promise alternative-signatures))
		      (raising-promises
		       (map (lambda (prm)
			      (delay
				(or (force prm)
				    (error "Signatur hat keinen Generator")))) ; #### src location
			    arbitrary-promises)))
		 (arbitrary-mixed
		  (map (lambda (sig arbp)
			 (cons (signature->predicate sig)
			       arbp))
		       alternative-signatures raising-promises))))
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

; Flatten out mixed signatures, and fold in in the lazy-wrap
; signatures
	
(define (normalize-mixed-signatures mixed-signature sigs)
  (fold-lazy-wrap-signatures mixed-signature (flatten-mixed-signatures sigs)))

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
	   (signature-violation proc self parameter-count-mismatch-message #f)
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
			(signature-violation proc self argument-count-mismatch-message #f)
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
; (make-mixed-signature (make-lazy-wrap-signature ...)  (make-lazy-wrap-signature ...) ...)

; We need to push the `mixed' signature inside the lazy-wrap
; signature, which is why the struct-map signature has an implicit
; `mixed'.
; To this end, a `lazy-log-not-checked' object tracks a list of
; `mixed' alternatives.  The `mixed-signature' field tracks from which
; `mixed' contract the mixture has originally come from.  
; (It may be #f, in which case the `field-signatures-list' is a one-element list.)
(define-struct lazy-log-not-checked
  (mixed-signature field-signatures-list)
  #:transparent)

(define (make-lazy-wrap-signature name eager-checking? type-descriptor predicate field-signatures syntax)
  (really-make-lazy-wrap-signature name eager-checking? 
				   type-descriptor predicate #f (list field-signatures) syntax))

; The lists of signatures in `field-signatures-list' form an implicit mixed signature.
(define (really-make-lazy-wrap-signature name eager-checking? type-descriptor predicate
					 mixed-signature field-signatures-list
					 syntax)
  (let ((lazy-wrap-info (lazy-wrap-ref type-descriptor))
	(not-checked (make-lazy-log-not-checked mixed-signature field-signatures-list))
	(lazy-wrap-signature-info
	 (make-lazy-wrap-signature-info eager-checking? type-descriptor predicate field-signatures-list)))
    (let ((constructor (lazy-wrap-info-constructor lazy-wrap-info))
	  (raw-accessors (lazy-wrap-info-raw-accessors lazy-wrap-info))
	  (wrap-ref (lazy-wrap-info-ref-proc lazy-wrap-info))
	  (wrap-set! (lazy-wrap-info-set!-proc lazy-wrap-info)))
      (make-signature
       name
       (lambda (self thing)
	 (if (not (predicate thing))
	     (signature-violation thing self #f #f)
	     (begin
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
						  (lazy-wrap-log-checked log))))))

	       (when eager-checking?
		 (check-lazy-wraps! type-descriptor thing))))
       
	 thing)
       (delay syntax)
       #:info-promise
       (delay lazy-wrap-signature-info)
       #:=?-proc
       (lambda (this-info other-info)
	 (and (lazy-wrap-signature-info? other-info)
	      (lazy-wrap-signature-info-field-signatures-list other-info)
	      (eq? type-descriptor (lazy-wrap-signature-info-descriptor other-info))
	      (andmap (lambda (this-field-signatures)
			(andmap (lambda (other-field-signatures)
				  (andmap signature=? this-field-signatures other-field-signatures))
				(lazy-wrap-signature-info-field-signatures-list other-info)))
		      (lazy-wrap-signature-info-field-signatures-list this-info))))
       #:<=?-proc
       (lambda (this-info other-info)
	 (and (lazy-wrap-signature-info? other-info)
	      (lazy-wrap-signature-info-field-signatures-list other-info) 
	      (eq? type-descriptor (lazy-wrap-signature-info-descriptor other-info))
	      (andmap (lambda (this-field-signatures)
			(ormap (lambda (other-field-signatures)
				 (andmap signature<=? this-field-signatures other-field-signatures))
			       (lazy-wrap-signature-info-field-signatures-list other-info)))
		      (lazy-wrap-signature-info-field-signatures-list this-info))))))))

(define-struct lazy-wrap-signature-info (eager-checking? descriptor predicate field-signatures-list) #:transparent)

(define (check-lazy-wraps! descriptor thing)
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
						       (inner (cdr field-signatures-list))))))))))))))))))

; pushes down mixed contracts
(define (fold-lazy-wrap-signatures mixed-signature sigs)
  (let ((lazy-wrap-sigs (make-hasheq))) ; maps a type descriptor to signatures

    (define (push-down-lazy-wrap-sigs)
      (hash-map lazy-wrap-sigs
		(lambda (type-desc signatures)
		  (let* ((sig (car signatures))
			 (info (real-signature-info (car signatures))))
		    (really-make-lazy-wrap-signature 
		     (signature-name sig) 
		     (lazy-wrap-signature-info-eager-checking? info)
		     type-desc 
		     (lazy-wrap-signature-info-predicate info)
		     mixed-signature
		     (apply append
			    (map (lambda (sig)
				   (lazy-wrap-signature-info-field-signatures-list (real-signature-info sig)))
				 signatures))
		     (signature-syntax sig))))))
  
    (let loop ((sigs sigs)
	       (vanilla-sigs '()))
      (if (null? sigs)
	  (append (push-down-lazy-wrap-sigs)
		  (reverse vanilla-sigs))
	  (let* ((sig (car sigs))
		 (info (real-signature-info sig)))
	    (if (lazy-wrap-signature-info? info)
		(let ((type-desc (lazy-wrap-signature-info-descriptor info)))
		  (hash-update! lazy-wrap-sigs
				type-desc
				(lambda (old)
				  (cons sig old))
				(lambda ()
				  (list sig)))
		  (loop (cdr sigs) vanilla-sigs))
		(loop (cdr sigs) (cons sig vanilla-sigs))))))))

(define checked-pair-table (make-weak-hasheq))

(define-struct checked-pair
  (car cdr log)
  #:mutable)

(define (checked-pair-access checked-access raw-access)
  (lambda (p)
    (cond
     ((hash-ref checked-pair-table
		p
		(lambda () #f))
      => (lambda (eph)
	   (checked-access (ephemeron-value eph))))
     (else (raw-access p)))))

(define checked-raw-car   (checked-pair-access checked-pair-car car))
(define checked-raw-cdr   (checked-pair-access checked-pair-cdr cdr))
(define checked-raw-first (checked-pair-access checked-pair-car first))
(define checked-raw-rest  (checked-pair-access checked-pair-cdr rest))

(define (checked-raw-set! checked-set!)
  (lambda (p new)
    (cond
     ((hash-ref checked-pair-table
		p
		(lambda () #f))
      => (lambda (eph)
	   (checked-set! (ephemeron-value eph) new)))
     (else 
      (let ((cp (make-checked-pair (car p) (cdr p) #f)))
	(checked-set! cp new)
	(hash-set! checked-pair-table p (make-ephemeron p cp)))))))

(define checked-raw-set-car! (checked-raw-set! set-checked-pair-car!))
(define checked-raw-set-cdr! (checked-raw-set! set-checked-pair-cdr!))

(define (checked-pair-get-log p)
  (cond
   ((hash-ref checked-pair-table
	      p
	      (lambda () #f))
    => (lambda (eph)
	 (checked-pair-log (ephemeron-value eph))))
   (else #f)))

(define (checked-pair-set-log! p new)
  (cond
   ((hash-ref checked-pair-table
	      p
	      (lambda () #f))
    => (lambda (eph)
	 (set-checked-pair-log! (ephemeron-value eph) new)))
   (else
    (hash-set! checked-pair-table p 
	       (make-ephemeron p
			       (make-checked-pair (car p) (cdr p) new))))))

(define checked-pair-lazy-wrap-info
  (make-lazy-wrap-info cons
		       (list checked-raw-car checked-raw-cdr)
		       (list checked-raw-set-car! checked-raw-set-cdr!)
		       checked-pair-get-log
		       checked-pair-set-log!))

(define checked-pair-descriptor
  (call-with-values
      (lambda ()
	(make-struct-type 'dummy-checked-pair #f 0 0 #f
			  (list
			   (cons prop:lazy-wrap checked-pair-lazy-wrap-info))))
    (lambda (desc . _)
      desc)))

(define (make-pair-signature eager-checking? car-sig cdr-sig)
  (make-lazy-wrap-signature 'pair eager-checking? 
			    checked-pair-descriptor pair? (list car-sig cdr-sig) #'pair))

(define (checked-car p)
  (car p)
  (check-lazy-wraps! checked-pair-descriptor p)
  (checked-raw-car p))

(define (checked-cdr p)
  (cdr p)
  (check-lazy-wraps! checked-pair-descriptor p)
  (checked-raw-cdr p))

(define (checked-first p)
  (first p)
  (check-lazy-wraps! checked-pair-descriptor p)
  (checked-raw-first p))

(define (checked-rest p)
  (rest p)
  (check-lazy-wraps! checked-pair-descriptor p)
  (checked-raw-rest p))

)
