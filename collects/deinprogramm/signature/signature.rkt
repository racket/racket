#lang scheme/base

(provide make-signature signature?
	 signature-name signature-syntax signature-enforcer
	 signature-arbitrary-promise
	 signature-<=?-proc signature-=?-proc
	 signature-arbitrary set-signature-arbitrary! set-signature-arbitrary-promise!
	 signature-info-promise
	 signature-violation
	 signature-violation-proc call-with-signature-violation-proc
	 signature-update-syntax signature-update-info-promise
	 apply-signature apply-signature/blame
	 signature=? signature<=?
	 make-procedure-to-blame
	 procedure-to-blame?
	 procedure-to-blame-proc procedure-to-blame-syntax
	 make-type-variable-info type-variable-info?
	 signature-checking-enabled?)

(require scheme/promise
	 mzlib/struct
	 (for-syntax scheme/base)
	 (for-syntax stepper/private/syntax-property))

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
		       #:arbitrary-promise (arbitrary-promise (delay #f))
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

(define-struct procedure-to-blame (proc syntax)
  #:property prop:procedure 0)
	     
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

(define signature-checking-enabled? (make-parameter #t))

(define (apply-signature signature val)
  (if (signature-checking-enabled?)
      ((signature-enforcer signature) signature val)
      val))

(define-struct type-variable-info ())

; "do the values that fulfill c1 also fulfill c2?"
(define (signature<=? c1 c2)
  (or (signature=? c1 c2)
      (let ((i1 (force (signature-info-promise c1)))
	    (i2 (force (signature-info-promise c2))))
	(or (type-variable-info? i2) ; kludge, maybe dispatch should be on second arg
	    (and i1 i2
		 ((signature-<=?-proc c1) i1 i2))))))
