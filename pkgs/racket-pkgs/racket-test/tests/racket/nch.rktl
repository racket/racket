
(define (fact n)
  (if (zero? n)
      1
      (* n (fact (- n 1)))))

(define f1000 (fact 1000))

(define (divall n d)
  (if (<= n 1)
      d
      (divall (/ n d) (+ 1 d))))

(define (nch n c)
  (/ (fact n) (fact (- n c)) (fact c)))

(define (snch n)
  (letrec ((loop
	    (lambda (i)
	      (if (> i n)
		  0
		  (+ (nch n i) (loop (+ i 1)))))))
    (loop 0)))

(define (fsum n)
  (if (zero? n)
      1
      (+ (fact n) (fsum (- n 1)))))


