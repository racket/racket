(module web-hammer mzscheme
  (require (lib "url.ss" "net"))
  (provide server-performance)
  (provide (all-from (lib "url.ss" "net")))

  (define BUFFER-SIZE 8192)

  ;; server-performance : Url Nat Nat Nat -> Num
  ;; num-clients client threads request the url from the server repeatedly
  ;; think-time-msecs appart for duration-seconds
  ;; the performance is completed requests per second
  (define (server-performance url num-clients think-time-msec duration-seconds)
    (let ([cust (make-custodian)]
	  [think-time-seconds (/ think-time-msec 1000)]
	  [responses (make-vector num-clients 0)]
	  [start (current-milliseconds)])
      (parameterize ([current-custodian cust])
	(let loop ([n num-clients])
	  (let ([n-1 (sub1 n)])
	    (unless (zero? n)
	      (thread
	       (lambda ()
		 (let request ()
		   (with-handlers ([exn:fail? void])
		     (parameterize ([current-custodian (make-custodian)])
		       (let ([port (get-pure-port url)])
			 (let discard-all ()
			   (let ([s (read-string BUFFER-SIZE port)])
			     (unless (eof-object? s)
			       (discard-all)))))
		       (custodian-shutdown-all (current-custodian)))
		     ; vector-set! is inside ignore errors so it doesn't happen for unfulfilled requests
		     (vector-set! responses n-1 (add1 (vector-ref responses n-1))))
		   (sleep think-time-seconds)
		   (request))))
	      (loop n-1)))))
      (sleep duration-seconds)
      (custodian-shutdown-all cust)
      (let ([stop (current-milliseconds)])
	(* 1000 (/ (apply + (vector->list responses)) (- stop start)))))))
