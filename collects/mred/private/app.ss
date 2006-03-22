(module app mzscheme
  (require (lib "class.ss")
	   (lib "class100.ss")
	   "wxkernel.ss"
	   "lock.ss"
	   "helper.ss"
	   "wx.ss"
	   "wxtop.ss")

  (provide application-preferences-handler
	   application-about-handler
	   application-quit-handler
	   application-file-handler
	   current-eventspace-has-standard-menus?
	   current-eventspace-has-menu-root?
	   eventspace-handler-thread)

  ;; An app-handler record keeps a wrapped procedure with
  ;; its original wrappee.
  (define-values (struct:app-handler
		  make-app-handler
		  app-handler?
		  app-handler-ref
		  app-handler-set!)
    (make-struct-type 'app-handler
		      #f 2 0
		      #f null (current-inspector)
		      0))
  (define (app-handler-orig ah)
    (app-handler-ref ah 1))

  (let* ([running-quit? #f]
	 [f (entry-point
	     (lambda ()
	       (unless running-quit?
		 (let ([af (weak-box-value active-main-frame)])
		   (when af
		     (set! running-quit? #t)
		     (queue-window-callback
		      af
		      (entry-point
		       (lambda ()
			 (dynamic-wind
			     void
			     (lambda ()
			       (send af on-exit)
			       (unless (null? (wx:get-top-level-windows))
				 (wx:cancel-quit)))
			     (lambda () 
			       (set! running-quit? #f)))))))))))])
    (wx:application-quit-handler (make-app-handler f f)))

  (define (set-handler! who proc param arity result-filter)
    (when proc
      (unless (and (procedure? proc)
		   (procedure-arity-includes? proc arity))
	(raise-type-error who
			  (format "procedure (arity ~a) or #f" arity)
			  proc)))
    (let ([e (wx:current-eventspace)])
      (when (wx:main-eventspace? e)
	(param (make-app-handler
		(lambda args
		  (parameterize ([wx:current-eventspace e])
		    (wx:queue-callback
		     (lambda () (result-filter (apply proc args)))
		     wx:middle-queue-key)))
		proc)))))

  (define application-preferences-handler
    (case-lambda
     [() (and (wx:main-eventspace? (wx:current-eventspace))
	      (app-handler-orig (wx:application-pref-handler)))]
     [(proc)
      (set-handler! 'application-preferences-handler proc
		    wx:application-pref-handler
		    0
		    values)]))

  (define application-about-handler
    (case-lambda
     [() (or (and (wx:main-eventspace? (wx:current-eventspace))
		  (app-handler-orig (wx:application-about-handler)))
	     void)]
     [(proc)
      (set-handler! 'application-about-handler proc
		    wx:application-about-handler
		    0
		    values)]))

  (define application-quit-handler
    (case-lambda
     [() (or (and (wx:main-eventspace? (wx:current-eventspace))
		  (app-handler-orig (wx:application-quit-handler)))
	     void)]
     [(proc)
      (set-handler! 'application-quit-handler proc
		    wx:application-quit-handler
		    0
		    (lambda (v) (unless v (wx:cancel-quit)) v))]))

  (define default-application-file-handler
    (entry-point
     (lambda (f)
       (let ([af (weak-box-value active-main-frame)])
	 (when af
	   (queue-window-callback
	    af
	    (entry-point
	     (lambda () (when (send af accept-drag?)
			  (send af on-drop-file f))))))))))

  (define (install-defh)
    (wx:application-file-handler (make-app-handler
				  default-application-file-handler
				  default-application-file-handler)))
  (install-defh)

  (define application-file-handler
    (case-lambda
     [() (or (and (wx:main-eventspace? (wx:current-eventspace))
		  (app-handler-orig (wx:application-file-handler)))
	     void)]
     [(proc)
      ;; Special case for default-application-file-handler,
      ;; because it need not be constrained to the main eventspace:
      (if (eq? proc default-application-file-handler)
	  (install-defh)
	  (set-handler! 'application-file-handler proc
			wx:application-file-handler
			1
			values))]))


  (define (current-eventspace-has-standard-menus?)
    (and (eq? 'macosx (system-type))
	 (wx:main-eventspace? (wx:current-eventspace))))

  (define (current-eventspace-has-menu-root?)
    (and (memq (system-type) '(macos macosx))
	 (wx:main-eventspace? (wx:current-eventspace))))

  (define (eventspace-handler-thread e)
    (let ([t (wx:eventspace-handler-thread e)])
      (or t
	  ;; eventspace dead, or just no thread, yet?
	  (with-handlers ([exn:fail?
			   (lambda (x)
			     (if (wx:eventspace-shutdown? e)
				 (raise-mismatch-error
				  'eventspace-handler-thread
				  "eventspace is shutdown: "
				  e)
				 (raise x)))])
	    (let ([done (make-semaphore)]
		  [t #f])
	      (parameterize ([wx:current-eventspace e])
		(wx:queue-callback
		 (lambda () 
		   (set! t (current-thread))
		   (semaphore-post done))
		 #t)
		(if (sync/timeout 1.0 done)
		    t
		    ;; Weird - no response after 1 second.  Maybe
		    ;; someone killed the handler thread before it could
		    ;; do our work? Or shutdown the eventspace? Or the
		    ;; thread is busy? In any of those cases, we'll
		    ;; succeed on the next iteration.
		    (eventspace-handler-thread e)))))))))

