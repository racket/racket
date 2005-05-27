(define make-invokable-unit
  (lambda (application)
    (compound-unit/sig (import)
      (link [wx : wx^ (wx@)]
	    [core : mzlib:core^ (mzlib:core@)]
	    [mred : mred^ ((require-library "linkwx.ss" "mred") core wx)]
	    [application : () (application mred core wx)])
      (export (unit mred mred2)))))

(define (go flags)
  (define die? #f)
  (define my-app
    (unit/sig ()
     (import mred^ 
	     mzlib:core^
	     [wx : wx^])
     
     (define app-name "Tester")
     (define console (if (memq 'console flags)
			 (make-object console-frame%)
			 #f))
     (define eval-string pretty-print@:pretty-print)
     (when (memq 'thread flags)
	   (let ([s (make-semaphore 1)]
		 [s2 (make-semaphore 0)]
		 [done (make-semaphore 0)])
	     ; Use of semaphore-callback insures that thread is a child
	     ; of the eventspace
	     (semaphore-callback s
				 (lambda ()
				   (semaphore-post done)
				   (thread (lambda ()
					     (let loop ()
					       (sleep 1)
					       (loop))))
				   (when (begin0
					  die?
					  (set! die? (not die?)))
				       (kill-thread (current-thread))))) ; kills handler thread
	     ; Add another callback that we know will not get triggered
	     (semaphore-callback s2 void)
	     (wx:yield done)))
     (when (memq 'eventspace flags)
	   (let ([e (wx:make-eventspace)])
	     (parameterize ([wx:current-eventspace e])
		(send (make-object wx:frame% null "Testing" -1 -1 100 100)
		      show #t))))
     (unless (memq 'force flags)
	     (run-exit-callbacks))))
  
  (let loop ()
    (collect-garbage)
    (collect-garbage)
    (wx:yield) (sleep) (wx:yield) (sleep)
    (wx:yield) (sleep) (wx:yield) (sleep)
    (wx:yield) (sleep) (wx:yield) (sleep)
    (wx:yield) (sleep) (wx:yield) (sleep)
    (wx:yield) (sleep) (wx:yield) (sleep)
    (dump-memory-stats)
    (let ([custodian (make-custodian)])
      (parameterize ([current-custodian custodian]
		     [wx:current-eventspace 
		      (if (memq 'force flags)
			  (wx:make-eventspace)
			  (wx:current-eventspace))])
	  (invoke-unit/sig
	   (make-invokable-unit my-app)))
      (when (memq 'force flags)
	    (custodian-shutdown-all custodian)))
    (loop)))
