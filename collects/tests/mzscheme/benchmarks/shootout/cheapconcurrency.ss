
(module cheapconcurrency mzscheme 

  (define (generate receive-ch n)
    (if (zero? n)
	receive-ch
	(let ([ch (make-channel)])
	  (thread (lambda ()
		    (let loop ()
		      (channel-put ch (add1 (channel-get receive-ch)))
		      (loop))))
	  (generate ch (sub1 n)))))

  (let ([n (string->number 
	    (vector-ref (current-command-line-arguments) 0))])
    (let* ([start-ch (make-channel)]
	   [end-ch (generate start-ch 500)])
      (let loop ([n n][total 0])
	(if (zero? n)
	    (printf "~a\n" total)
	    (begin
	      (channel-put start-ch 0)
	      (loop (sub1 n)
		    (+ total (channel-get end-ch)))))))))
