
(define id 40000)

(define max-send 100000)
(define print-mod 10000)

(define (client host)
  (lambda ()
    (let-values ([(r w) (tcp-connect host id)])
      (values r w void))))
  
(define server
  (lambda ()
    (let ([l (tcp-listen id)])
      (let-values ([(r w) (tcp-accept l)])
	(values r w (lambda () (tcp-close l)))))))

(define (tread connect)
  (let-values ([(r w close) (connect)])
    (printf "Hit return to start reading~n")
    (read-line)
    (let loop ([last -1])
      (let ([v (read r)])
	(if (eof-object? v)
	    (begin
	      (close-input-port r)
	      (close-output-port w)
	      (close)
	      last)
	    (begin
	      (unless (= v (add1 last))
		(printf "skipped! ~a ~a~n" last v))
	      (when (zero? (modulo v print-mod))
		(printf "got ~a~n" v))
	      (loop v)))))))

(define (twrite connect)
  (let-values ([(r w close) (connect)]
	       [(t) (thread (lambda ()
			      (let loop ()
				(sleep 1)
				(printf "tick~n")
				(loop))))])
    (let ([done (lambda ()
		  (close-output-port w)
		  (close-input-port r)
		  (close)
		  (kill-thread t))])
      (let loop ([n 0])
	(if (= n max-send)
	    (begin
	      (printf "stopped before ~a~n" n)
	      (done))
	    
	    (begin
	      (fprintf w "~s~n" n)
	      (when (zero? (modulo n print-mod))
		(printf "sent ~a~n" n))
	      (loop (add1 n))))))))
