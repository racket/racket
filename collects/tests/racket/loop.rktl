

(define five +)

(define (one v)
  (if (equal? v 15)
      (apply five (list 1 2 3 4 5))
      15))

(define (dloop x d)
  (if (zero? d)
      0
      (if (equal? x 15)
	  (let ([v (one 10)])
	    (let ([c (one v)])
	      (add1 (dloop c (sub1 d)))))
	  (dloop 15 d))))

(define (loop)
  (let loop ([n 0])
    (let ([v (dloop 0 n)])
      (if (equal? n v)
	  (begin
	    (when (zero? (modulo n 100))
		  (printf "~a\n" n))
	    (loop (add1 n)))
	  (error 'loop "messed up: ~a != ~a\n" n v)))))
