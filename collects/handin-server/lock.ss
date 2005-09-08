
(module lock mzscheme
  (require (lib "list.ss"))

  (provide wait-for-lock)

  ;; wait-for-lock : string -> void
  ;;  Gets a lock on `user' for the calling thread; the lock
  ;;  lasts until the calling thread terminates.
  (define (wait-for-lock user)
    (let ([s (make-semaphore)])
      (channel-put req-ch (make-req 
			   (thread-dead-evt (current-thread))
			   user
			   s))
      (semaphore-wait s)))

  (define req-ch (make-channel))
  
  (define-struct req (thread-dead-evt
		      user
		      sema))

  (thread (lambda ()
	    (let loop ([locks null]
		       [reqs null])
	      (let-values ([(locks reqs)
			    ;; Try to satisfy lock requests:
			    (let loop ([reqs (reverse reqs)][locks locks][new-reqs null])
			      (cond
			       [(null? reqs) (values locks new-reqs)]
			       [(assoc (req-user (car reqs)) locks)
				;; Lock not available:
				(loop (cdr reqs) locks (cons (car reqs) new-reqs))]
			       [else
				;; Lock is available, so take it:
				(let ([req (car reqs)])
				  (semaphore-post (req-sema req))
				  (loop (cdr reqs) (cons (cons (req-user req) req) locks) new-reqs))]))])
		(sync
		 (handle-evt
		  req-ch
		  (lambda (req)
		    (loop locks (cons req reqs))))
		 ;; Release a lock whose thread is gone:
		 (apply choice-evt
			(map (lambda (name+req)
			       (handle-evt
				(req-thread-dead-evt (cdr name+req))
				(lambda (v)
				  (loop (remq name+req locks) reqs))))
			     locks))
		 ;; Throw away a request whose thread is gone:
		 (apply choice-evt
			(map (lambda (req)
			       (handle-evt
				(req-thread-dead-evt req)
				(lambda (v)
				  (loop locks (remq req reqs)))))
			     reqs))))))))
