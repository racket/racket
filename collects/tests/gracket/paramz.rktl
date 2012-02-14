
(load-relative "loadtest.rktl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Yield Tests                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define s (make-semaphore))
(define v 4)
(queue-callback (lambda () (set! v 5)))
(yield)
(test v 'yield-run 5)
(queue-callback (lambda () (set! v 6)))

(semaphore-post s)
(yield s)
(test v 'yield-wait 5)
(yield)
(test v 'yield-run 6)

(queue-callback (lambda () (set! v 7) (semaphore-post s)))
(yield s)
(test v 'yield-run-post 7)

(queue-callback (lambda () 
		  (set! v 8)
		  (semaphore-post s)
		  (queue-callback
		   (lambda () (set! v 9)))))
(yield s)
(test v 'yield-wait-post 8)
(yield)
(test v 'yield-run 9)

(define d (make-object dialog% "hello"))
(thread (lambda () 
	  (sync (system-idle-evt))
	  (queue-callback (lambda () (set! v 11)))
	  (send d show #f)))
(queue-callback (lambda () (set! v 10)))
(send d show #t)
(test v 'dialog-wait 10)
(yield)
(test v 'dialog-run 11)

(define d (make-object dialog% "Hello"))
(let ([t (thread (lambda ()
		   (send d show #t)))])
  (let loop () (unless (send d is-shown?) (loop)))
  (st #t d is-shown?)
  (thread-suspend t)
  (stv d show #f)
  (st #f d is-shown?)
  (thread-resume t)
  (thread-wait t)
  (st #f d is-shown?)

  (let ([t (thread (lambda ()
                     (send d show #t)))])
    (let loop () (unless (send d is-shown?) (sleep) (loop)))
    (st #t d is-shown?)
    (thread-suspend t)
    (stv d show #f)
    (st #f d is-shown?)
    (let ([t2 (thread (lambda () (send d show #t)))])
      (yield (system-idle-evt))
      (st #t d is-shown?)
      (thread-resume t)
      (yield (system-idle-evt))
      (st #t d is-shown?)
      (test #t 'thread2 (thread-running? t2))
      (stv d show #f)
      (thread-wait t)
      (thread-wait t2)
      (st #f d is-shown?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Parameterization Tests                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Killing an eventspace
(define c (make-custodian))
(define e (parameterize ([current-custodian c]) (make-eventspace)))
(define tmr (parameterize ([current-eventspace e]) 
	      (new timer% [notify-callback void])))
(parameterize ([current-eventspace e]) (send (make-object frame% "x" #f 50 50) show #t))
(test #f 'shutdown? (eventspace-shutdown? e))
(custodian-shutdown-all c)
(test #t 'shutdown? (eventspace-shutdown? e))
(define (try-use-es t)
  (test
   'error
   'shutdown-eventspace
   (with-handlers ([(lambda (x)
		      (and (exn:fail? x)
			   (regexp-match "shutdown" (exn-message x))))
		    (lambda (x)
		      (printf "got expected error: ~a\n" (exn-message x))
		      'error)])
     (parameterize ([current-eventspace e]) 
       (t)))))
(try-use-es (lambda () (make-object frame% "x" #f 50 50)))
(try-use-es (lambda () (make-object dialog% "x" #f 50 50)))
(try-use-es (lambda () (make-object timer%)))
(try-use-es (lambda () (queue-callback void)))
(try-use-es (lambda () (send tmr start 100 #t)))

;; ----------------------------------------
;;  Check that breaking an eventspace thread doesn't kill it:

(let ()
  (define o (open-output-bytes))
  (define evtsp (parameterize ([current-error-port o])
                  (make-eventspace)))
  (define evtth (eventspace-handler-thread evtsp))
  
  (sleep 0.1)
  (break-thread evtth)

  (define done (make-semaphore))
  (parameterize ((current-eventspace evtsp))
    (queue-callback (lambda () (semaphore-post done))))

  (unless (sync/timeout 3 done)
    (error "broken thread is really broken")))

;; ----------------------------------------

(report-errs)
