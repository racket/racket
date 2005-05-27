(module lock mzscheme
  (require (prefix wx: "kernel.ss"))
  (provide (protect as-entry
		    as-exit
		    entry-point
		    mk-param))

  ;; ;;;;;;;;;;;;; Thread Safety ;;;;;;;;;;;;;;;;;;;;
  
  ;; When the user creates an object or calls a method, or when the
  ;; system invokes a callback, many steps may be required to initialize
  ;; or reset fields to maintain invariants. To ensure that other
  ;; threads do not call methods during a time when invariants do not
  ;; hold, we force all of the following code to be executed in a single
  ;; threaded manner, and we temporarily disable breaks. This accompiled
  ;; with a single monitor: all entry points into the code use
  ;; `entry-point' or `as-entry', and all points with this code that
  ;; call back out to user code uses `as-exit'.

  ;; If an exception is raised within an `enter'ed area, control is
  ;; moved back outside by the exception handler, and then the exception
  ;; is re-raised. The user can't tell that the exception was caught an
  ;; re-raised. But without the catch-and-reraise, the user's exception
  ;; handler might try to use GUI elements from a different thread,
  ;; leading to deadlock.

  (define monitor-sema (make-semaphore 1))
  (define monitor-owner #f)

  ;; An exception may be constructed while we're entered:
  (define entered-err-string-handler
    (lambda (s n)
      (as-exit
       (lambda ()
	 ((error-value->string-handler) s n)))))

  (define old-paramz #f)
  (define old-break-paramz #f)

  (define (as-entry f)
    (cond
     [(eq? monitor-owner (current-thread))
      (f)]
     [else
      ((let/ec k
	 (dynamic-wind
	     (lambda () 
	       (wx:in-atomic-region monitor-sema)	     
	       (set! monitor-owner (current-thread)))
	     (lambda () 
	       (set! old-paramz (current-parameterization))
	       (set! old-break-paramz (current-break-parameterization))
	       (parameterize ([error-value->string-handler entered-err-string-handler]
			      [current-exception-handler
			       (lambda (exn)
				 ;; Get out of atomic region before letting
				 ;;  an exception handler work
				 (k (lambda () (raise exn))))])
		 (parameterize-break #f
		   (call-with-values 
		       f 
		     (lambda args (lambda () (apply values args)))))))
	     (lambda ()
	       (set! monitor-owner #f)
	       (semaphore-post monitor-sema)
	       (wx:in-atomic-region #f)))))]))

					; entry-point macros in macros.ss

  (define (as-exit f)
    ;; (unless (eq? monitor-owner (current-thread)) (error 'monitor-exit "not in monitored area"))
    (let ([paramz old-paramz]
	  [break-paramz old-break-paramz])
      (call-with-parameterization
       paramz
       (lambda ()
	 (call-with-break-parameterization
	  break-paramz
	  (lambda ()
	    (dynamic-wind
		(lambda ()		
		  (set! monitor-owner #f)	 
		  (semaphore-post monitor-sema)
		  (wx:in-atomic-region #f))
		f
		(lambda ()
		  (set! old-paramz paramz)
		  (set! old-break-paramz break-paramz)
		  (wx:in-atomic-region monitor-sema)
		  (set! monitor-owner (current-thread))))))))))

  (define-syntax entry-point 
    (lambda (stx)
      (syntax-case stx (lambda case-lambda)
	[(_ (lambda args body1 body ...))
	 (syntax (lambda args (as-entry (lambda () body1 body ...))))]
	[(_ (case-lambda [vars body1 body ...] ...))
	 (syntax (case-lambda 
		  [vars (as-entry (lambda () body1 body ...))]
		  ...))])))

  (define-syntax mk-param
    (lambda (stx)
      (syntax-case stx ()
	[(_ val filter check force-redraw)
	 (syntax
	  (case-lambda
	   [() val]
	   [(v) (check v)
	    (let ([v2 (filter v)])
	      (unless (eq? v2 val)
		(set! val v2)
		(force-redraw)))]))]))))

  
  
