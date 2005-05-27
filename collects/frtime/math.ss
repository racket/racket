;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; math.ss: some extra math routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module math (lib "frtime.ss" "frtime")
  (provide e 
	  pi 
	  sqr
	  sgn conjugate
	  sinh cosh)

  (define (sqr z) (* z z))
  
  ;; circular constants and aliases
  (define e (exp 1.0)) 
  (define pi (atan 0 -1))
  
  ;; sgn function 
  (define sgn
    (lambda (x)
      (if (exact? x)
	  (cond
	   ((< x 0) -1)
	   ((> x 0)  1)
	   (else 0))
	  (cond
	   ((< x 0.0) -1.0)
	   ((> x 0.0)  1.0)
	   (else 0.0)))))
  
  ;; complex conjugate
  (define conjugate
    (lambda (z)
      (make-rectangular 
       (real-part z)
       (- (imag-part z)))))
  
  ;; real hyperbolic functions
  (define sinh 
    (lambda (x)
      (/
       (- (exp x) (exp (- x)))
       2.0)))
  
  (define cosh
    (lambda (x)
      (/
       (+ (exp x) (exp (- x)))
       2.0))))
