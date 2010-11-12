(define (wait-for-frame name)
  (let ([timeout 10]
	[pause-time 1/2])
    (send-sexp-to-mred
     `(let loop ([n ,(/ timeout pause-time)])
	(if (zero? n)
	    (error 'wait-for-mred-frame
		   ,(format "after ~a seconds, frame labelled ~s didn't appear" timeout name))
	    (let ([win (get-top-level-focus-window)])
	      (printf "win: ~a label ~a\n" win (and win (string=? (send win get-label) ,name)))
	      (unless (and win (string=? (send win get-label) ,name))
		(sleep ,pause-time)
		(loop (- n 1)))))))))
