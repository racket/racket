(define k 
  (call-with-current-continuation
   (lambda (exit)
     (let loop ((n 60000))
       (if (zero? n)
	   (let ((v (call-with-current-continuation (lambda (k) k))))
	    (if (number? v)
		v
		(exit v)))
	  (- (loop (- n 1)) 1))))))

