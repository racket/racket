(define fact
    (lambda (n)
      (let loop ([n n][res 1])
	(if (zero? n)
	    res
	    (loop (sub1 n) (* n res))))))
