(define-values (odd) (lambda (x) (if (zero? x) #f (even (- x 1)))))
(define-values (even) (lambda (x) (if (zero? x) #t (odd (- x 1)))))

(define-values (odd2)
  (letrec ([even (lambda (x) (if (zero? x) #t (odd (- x 1))))]
	   [odd (lambda (x) (if (zero? x) #f (even (- x 1))))])
    odd))

(define-values (odd3)
  (let ([test (lambda (base other)
		(lambda (x) (if (zero? x) base ((other) (- x 1)))))])
    (letrec ([odd (test #f (lambda () even))]
	     [even (test #t (lambda () odd))])
      odd)))

(define-values (fib)
  (lambda (n)
    (if (<= n 1)
	1
	(+ (fib (- n 1)) (fib (- n 2))))))

(define-values (mutate)
  (lambda (n)
    (let loop ()
      (unless (zero? n)
	      (set! n (sub1 n))
	      (loop)))))

(define-values (mutate-evil)
  (lambda (n)
    (let loop ([n n])
      (unless (zero? n)
	      (set! n (sub1 n))
	      (loop n)))))

(define-values (c-loop)
  (let-values ([(a b c d e f g) (values 1 2 3 4 5 6 7)])
    (lambda (n)
      (let loop ([n n])
	(if (zero? n)
	    (+ a b c d e f g)
	    (loop (sub1 n)))))))
