; This is a purely functional random generator, based on Lennart
; Augustsson's generator shipped with Hugs.

; Its comment says:

; This implementation uses the Portable Combined Generator of L'Ecuyer
; ["System.Random\#LEcuyer"] for 32-bit computers, transliterated by
; Lennart Augustsson.  It has a period of roughly 2.30584e18.

; This makes it not as good as Sebastian Egner's reference
; implementation of SRFI 27, but much faster for applications that
; need a pure random-number generator with a `split' operation.

(define-record-type :random-generator
  (really-make-random-generator s1 s2)
  random-generator?
  (s1 random-generator-s1)
  (s2 random-generator-s2))

(define-record-discloser :random-generator
  (lambda (r)
    (list 'random-generator
	  (random-generator-s1 r)
	  (random-generator-s2 r))))

(define min-bound (- (expt 2 31)))
(define max-bound (- (expt 2 31) 1))
(define int-range (- max-bound min-bound))

(define (make-random-generator s)
  (if (negative? s)
      (make-random-generator (- s))
      (let ((q (quotient s 2147483562))
	    (s1 (remainder s 2147483562)))
	(let ((s2 (remainder q 2147483398)))
	  (really-make-random-generator (+ 1 s1) (+ 1 s2))))))

(define (random-generator-next rg)
  (let ((s1 (random-generator-s1 rg))
	(s2 (random-generator-s2 rg)))

    (let ((k (quotient s1 53668))
	  (k* (quotient s2 52774)))
      (let ((s1*  (- (* 40014 (- s1 (* k 53668)))
		     (* k 12211)))
	    (s2* (- (* 40692 (- s2 (* k* 52774)))
		    (* k* 3791))))
	(let ((s1** (if (negative? s1*)
			(+ s1* 2147483563)
			s1*))
	      (s2** (if (negative? s2*)
			(+ s2* 2147483399)
			s2*)))
	  (let* ((z (- s1** s2**))
		 (z* (if (< z 1)
			 (+ z 2147483562)
			 z)))
	    (values z* (really-make-random-generator s1** s2**))))))))

(define (random-generator-split rg)
  (let ((s1 (random-generator-s1 rg))
	(s2 (random-generator-s2 rg)))
    (let ((new-s1 (if (= s1 2147483562)
		      1
		      (+ s1 1)))
	  (new-s2 (if (= s2 1)
		      2147483398
		      (- s2 1))))
      (call-with-values
	  (lambda ()
	    (random-generator-next rg))
	(lambda (_ nrg)
	  (values (really-make-random-generator new-s1
						(random-generator-s2 nrg))
		  (really-make-random-generator (random-generator-s1 nrg)
						new-s2)))))))


; The intervals are inclusive.

(define (random-integer rg low high)
  (let ((b 2147483561)
	(k (+ (- high low) 1)))
    (let loop ((n (ilogbase b k))
	       (acc low)
	       (rg rg))
      (if (zero? n)
	  (values (+ low (modulo acc k))
		  rg)
	  (call-with-values
	      (lambda () (random-generator-next rg))
	    (lambda (x rgn)
	      (loop (- n 1) (+ x (* acc b)) rgn)))))))

(define (random-real rg low high)
  (call-with-values
      (lambda ()
	(random-integer rg min-bound max-bound))
    (lambda (x nrg)
      (let ((scaled-x (+ (/ (+ low high) 2)
			 (* (/ (- high low) int-range)
			    x))))
	(values scaled-x nrg)))))

(define (ilogbase b i)
  (if (< i b)
      1
      (+ 1 (ilogbase b (quotient i b)))))

