
; run a thunk using a censor that removes dangerous chars from a 
; string for printing to a terminal
(lambda (thunk)
  (let ([censor (lambda (s)
		  (list->bytes
		   (let loop ([s (bytes->list s)])
		     (if (null? s)
			 null
			 (let ([c (car s)])
			   (cond
			    [(and (or (< c 32) (>= c #o200))
				  (not (= c 10)))
			     (append (cons (char->integer #\{)
					   (map
					    char->integer
					    (string->list 
					     (number->string c))))
				     (cons (char->integer #\})
					   (loop (cdr s))))]
			    [else
			     (cons c (loop (cdr s)))]))))))])
    (let* ([oldp (current-output-port)]
	   [cp (make-output-port
		'censor
		oldp
		(lambda (s start end nonblock? breakable?)
		  (display (censor (subbytes s start end)) oldp)
		  (- end start))
		void)])
      (dynamic-wind
       (lambda () (current-output-port cp))
       thunk
       (lambda ()
	 (current-output-port oldp))))))


