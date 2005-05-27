;;; -*- scheme -*-
;;; Fortran-style implementation of an EM clustering algorithm.
;;;
;;; Written by Jeffrey Mark Siskind (qobi@cs.toronto.edu)
;;; R4RS-ified by by Lars Thomas Hansen (lth@cs.uoregon.edu)
;;; Random number generator by Ozan Yigit.
;;;
;;; To run: (run-benchmark)
;;; You must provide your own timer function.
;;;
;;; Some benchmark times:
;;;
;;; Chez Scheme 4.1 for SunOS running on Sparc 10/51 (1MB,96MB,50MHz), Solaris:
;;;   Optimize-level 2: 112s run (CPU), 2.8s gc, 326 MB allocated, 1181 GCs
;;;   Optimize-level 3:  79s run (CPU), 2.8s gc, 326 MB allocated, 1163 GCs

(define make-model vector)
(define (model-pi model) (vector-ref model 0))
(define (set-model-pi! model x) (vector-set! model 0 x))
(define (model-mu model) (vector-ref model 1))
(define (model-sigma model) (vector-ref model 2))
(define (model-log-pi model) (vector-ref model 3))
(define (set-model-log-pi! model x) (vector-set! model 3 x))
(define (model-sigma-inverse model) (vector-ref model 4))
(define (model-log-determinant-sigma model) (vector-ref model 5))
(define (set-model-log-sigma-determinant! model x) (vector-set! model 5 x))

;---------------------------------------------------------------------------
; Minimal Standard Random Number Generator
; Park & Miller, CACM 31(10), Oct 1988, 32 bit integer version.
; better constants, as proposed by Park.
; By Ozan Yigit

(define *seed* 1)

(define (srand seed)
  (set! *seed* seed)
  *seed*)

(define (rand)
  (let ((A 48271)
        (M 2147483647)
        (Q 44488)
        (R 3399))
    (let* ((hi (quotient *seed* Q))
           (lo (modulo *seed* Q))
           (test (- (* A lo) (* R hi))))
      (if (> test 0)
          (set! *seed* test)
          (set! *seed* (+ test M)))))
  *seed*)

;---------------------------------------------------------------------------

(define (panic s) (error 'panic s))

(define *rand-max* 2147483648)

(define log-math-precision 35.0)

(define minus-infinity (- *rand-max*))

(define first car)

(define second cadr)

(define rest cdr)

(define (reduce f l i)
 (cond ((null? l) i)
       ((null? (rest l)) (first l))
       (else (let loop ((l (rest l)) (c (first l)))
	      (if (null? l) c (loop (rest l) (f c (first l))))))))

(define (every-n p n)
 (let loop ((i 0)) (or (>= i n) (and (p i) (loop (+ i 1))))))

(define (sum f n)
 (let loop ((n (- n 1)) (c 0.0))
  (if (negative? n) c (loop (- n 1) (+ c (f n))))))

(define (add-exp e1 e2)
 (let* ((e-max (max e1 e2))
	(e-min (min e1 e2))
	(factor (floor e-min)))
  (if (= e-max minus-infinity)
      minus-infinity
      (if (> (- e-max factor) log-math-precision)
	  e-max
	  (+ (log (+ (exp (- e-max factor)) (exp (- e-min factor))))
	     factor)))))

(define (map-n f n)
 (let loop ((i 0) (c '()))
  (if (< i n) (loop (+ i 1) (cons (f i) c)) (reverse c))))

(define (map-n-vector f n)
 (let ((v (make-vector n)))
  (let loop ((i 0))
   (if (< i n)
       (begin (vector-set! v i (f i))
	      (loop (+ i 1)))))
  v))

(define (remove-if-not p l)
 (let loop ((l l) (c '()))
  (cond ((null? l) (reverse c))
	((p (first l)) (loop (rest l) (cons (first l) c)))
	(else (loop (rest l) c)))))

(define (positionv x l)
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((eqv? x (first l)) i)
	(else (loop (rest l) (+ i 1))))))

(define (make-matrix m n)
  (map-n-vector (lambda (i) (make-vector n)) m))

(define (make-matrix-initial m n initial)
  (map-n-vector (lambda (i) (make-vector n initial)) m))

(define (matrix-rows a) (vector-length a))

(define (matrix-columns a) (vector-length (vector-ref a 0)))

(define (matrix-ref a i j) (vector-ref (vector-ref a i) j))

(define (matrix-set! a i j x) (vector-set! (vector-ref a i) j x))

(define (matrix-row-ref a i) (vector-ref a i))

(define (matrix-row-set! a i v) (vector-set! a i v))

(define (determinant a)
 (if (not (= (matrix-rows a) (matrix-columns a)))
     (panic "Can only find determinant of a square matrix"))
 (call-with-current-continuation
  (lambda (return)
   (let* ((n (matrix-rows a))
	  (b (make-matrix n n))
	  (d 1.0))
    (do ((i 0 (+ i 1))) ((= i n))
     (do ((j 0 (+ j 1))) ((= j n)) (matrix-set! b i j (matrix-ref a i j))))
    (do ((i 0 (+ i 1))) ((= i n))
     ;; partial pivoting reduces rounding errors
     (let ((greatest (abs (matrix-ref b i i)))
	   (index i))
      (do ((j (+ i 1) (+ j 1))) ((= j n))
       (let ((x (abs (matrix-ref b j i))))
	(if (> x greatest) (begin (set! index j) (set! greatest x)))))
      (if (= greatest 0.0) (return 0.0))
      (if (not (= index i))
       (let ((v (matrix-row-ref b i)))
	(matrix-row-set! b i (matrix-row-ref b index))
	(matrix-row-set! b index v)
	(set! d (- d))))
      (let ((c (matrix-ref b i i)))
       (set! d (* d c))
       (do ((j i (+ j 1))) ((= j n))
	(matrix-set! b i j (/ (matrix-ref b i j) c)))
       (do ((j (+ i 1) (+ j 1))) ((= j n))
	(let ((e (matrix-ref b j i)))
	 (do ((k (+ i 1) (+ k 1))) ((= k n))
	  (matrix-set!
	   b j k (- (matrix-ref b j k) (* e (matrix-ref b i k))))))))))
    d))))

(define (invert-matrix! a b)
 (if (not (= (matrix-rows a) (matrix-columns a)))
  (panic "Can only invert a square matrix"))
 (let* ((n (matrix-rows a))
	(c (make-matrix n n)))
  (do ((i 0 (+ i 1))) ((= i n))
   (do ((j 0 (+ j 1))) ((= j n))
    (matrix-set! b i j 0.0)
    (matrix-set! c i j (matrix-ref a i j))))
  (do ((i 0 (+ i 1))) ((= i n)) (matrix-set! b i i 1.0))
  (do ((i 0 (+ i 1))) ((= i n))
   (if (zero? (matrix-ref c i i))
       (call-with-current-continuation
	(lambda (return)
	  (do ((j 0 (+ j 1))) ((= j n))
	    (if (and (> j i) (not (zero? (matrix-ref c j i))))
		(begin
		  (let ((e (vector-ref c i)))
		    (vector-set! c i (vector-ref c j))
		    (vector-set! c j e))
		  (let ((e (vector-ref b i)))
		    (vector-set! b i (vector-ref b j))
		    (vector-set! b j e))
		  (return #f))))
	  (panic "Matrix is singular"))))
   (let ((d (/ (matrix-ref c i i))))
    (do ((j 0 (+ j 1))) ((= j n))
     (matrix-set! c i j (* d (matrix-ref c i j)))
     (matrix-set! b i j (* d (matrix-ref b i j))))
    (do ((k 0 (+ k 1))) ((= k n))
     (let ((d (- (matrix-ref c k i))))
      (if (not (= k i))
       (do ((j 0 (+ j 1))) ((= j n))
	(matrix-set!
	 c k j (+ (matrix-ref c k j) (* d (matrix-ref c i j))))
	(matrix-set!
	 b k j (+ (matrix-ref b k j) (* d (matrix-ref b i j))))))))))))

(define (jacobi! a)
 (if (not (and (= (matrix-rows a) (matrix-columns a))
	      (every-n (lambda (i)
			(every-n (lambda (j)
				  (= (matrix-ref a i j) (matrix-ref a j i)))
				 (matrix-rows a)))
		       (matrix-rows a))))
  (panic "Can only compute eigenvalues/eigenvectors of a symmetric matrix"))
 (let* ((n (matrix-rows a))
	(d (make-vector n))
	(v (make-matrix-initial n n 0.0))
	(b (make-vector n))
	(z (make-vector n 0.0)))
  (do ((ip 0 (+ ip 1))) ((= ip n))
   (matrix-set! v ip ip 1.0)
   (vector-set! b ip (matrix-ref a ip ip))
   (vector-set! d ip (matrix-ref a ip ip)))
  (let loop ((i 0))
   (if (> i 50) (panic "Too many iterations in JACOBI!"))
   (let ((sm (sum (lambda (ip)
		   (sum (lambda (ir)
			 (let ((iq (+ ip ir 1)))
			  (abs (matrix-ref a ip iq))))
			(- n ip 1)))
		  (- n 1))))
    (if (not (zero? sm))
	(begin
	  (let ((tresh (if (< i 3) (/ (* 0.2 sm) (* n n)) 0.0)))
	    (do ((ip 0 (+ ip 1))) ((= ip (- n 1)))
	      (do ((ir 0 (+ ir 1))) ((= ir (- n ip 1)))
		(let* ((iq (+ ip ir 1))
		       (g (* 100.0 (abs (matrix-ref a ip iq)))))
		  (cond
		   ((and (> i 3)
			 (= (+ (abs (vector-ref d ip)) g)
			    (abs (vector-ref d ip)))
			 (= (+ (abs (vector-ref d iq)) g)
			    (abs (vector-ref d iq))))
		    (matrix-set! a ip iq 0.0))
		   ((> (abs (matrix-ref a ip iq)) tresh)
		    (let* ((h (- (vector-ref d iq) (vector-ref d ip)))
			   (t (if (= (+ (abs h) g) (abs h))
				  (/ (matrix-ref a ip iq) h)
				  (let ((theta (/ (* 0.5 h)
						  (matrix-ref a ip iq))))
				    (if (negative? theta)
					(- (/ (+ (abs theta)
						 (sqrt (+ (* theta theta) 1.0)))))
					(/ (+ (abs theta)
					      (sqrt (+ (* theta theta) 1.0))))))))
			   (c (/ (sqrt (+ (* t t) 1.0))))
			   (s (* t c))
			   (tau (/ s (+ c 1.0)))
			   (h (* t (matrix-ref a ip iq))))
		      (define (rotate a i j k l)
			(let ((g (matrix-ref a i j))
			      (h (matrix-ref a k l)))
			  (matrix-set! a i j (- g (* s (+ h (* g tau)))))
			  (matrix-set! a k l (+ h (* s (- g (* h tau)))))))
		      (vector-set! z ip (- (vector-ref z ip) h))
		      (vector-set! z iq (+ (vector-ref z iq) h))
		      (vector-set! d ip (- (vector-ref d ip) h))
		      (vector-set! d iq (+ (vector-ref d iq) h))
		      (matrix-set! a ip iq 0.0)
		      (do ((j 0 (+ j 1))) ((= j n))
			(cond ((< j ip) (rotate a j ip j iq))
			      ((< ip j iq) (rotate a ip j j iq))
			      ((< iq j) (rotate a ip j iq j)))
			(rotate v j ip j iq)))))))))
	  (do ((ip 0 (+ ip 1))) ((= ip n))
	    (vector-set! b ip (+ (vector-ref b ip) (vector-ref z ip)))
	    (vector-set! d ip (vector-ref b ip))
	    (vector-set! z ip 0.0))
	  (loop (+ i 1))))))
  (do ((i 0 (+ i 1))) ((= i (- n 1)))
   (let ((k i)
	 (p (vector-ref d i)))
    (do ((l 0 (+ l 1))) ((= l (- n i 1)))
     (let* ((j (+ i l 1)))
      (if (>= (vector-ref d j) p)
	  (begin (set! k j) (set! p (vector-ref d j))))))
    (if (not (= k i))
	(begin (vector-set! d k (vector-ref d i))
	       (vector-set! d i p)
	       (do ((j 0 (+ j 1))) ((= j n))
		 (let ((p (matrix-ref v j i)))
		   (matrix-set! v j i (matrix-ref v j k))
		   (matrix-set! v j k p)))))))
  (list d v)))

(define (clip-eigenvalues! a v)
 (let* ((j (jacobi! a))
	(l (first j))
	(e (second j)))
  (do ((k1 0 (+ k1 1))) ((= k1 (vector-length a)))
   (let ((a-k1 (vector-ref a k1))
	 (e-k1 (vector-ref e k1)))
    (do ((k2 0 (+ k2 1))) ((= k2 (vector-length a-k1)))
     (let ((e-k2 (vector-ref e k2))
	   (s 0.0))
      (do ((k 0 (+ k 1))) ((= k (vector-length a)))
       (set! s (+ s (* (max (vector-ref v k) (vector-ref l k))
		       (vector-ref e-k1 k)
		       (vector-ref e-k2 k)))))
      (vector-set! a-k1 k2 s)))))))

;;; EM

(define (e-step! x z models)
 (do ((i 0 (+ i 1))) ((= i (vector-length x)))
  (let ((xi (vector-ref x i))
	(zi (vector-ref z i)))
   (do ((j 0 (+ j 1))) ((= j (vector-length models)))
    ;; Compute for each model.
    (let* ((model (vector-ref models j))
	   (log-pi (model-log-pi model))
	   (mu (model-mu model))
	   (sigma-inverse (model-sigma-inverse model))
	   (log-determinant-sigma (model-log-determinant-sigma model))
	   (t 0.0))
     ;; Compute likelihoods (note: up to constant for all models).
     (set! t 0.0)
     (do ((k1 0 (+ k1 1))) ((= k1 (vector-length xi)))
      (let ((sigma-inverse-k1 (vector-ref sigma-inverse k1)))
       (do ((k2 0 (+ k2 1))) ((= k2 (vector-length xi)))
	(set! t (+ t (* (- (vector-ref xi k1) (vector-ref mu k1))
			(vector-ref sigma-inverse-k1 k2)
			(- (vector-ref xi k2) (vector-ref mu k2))))))))
     (vector-set! zi j (- log-pi (* 0.5 (+ log-determinant-sigma t))))))))
 (let ((l 0.0))
  (do ((i 0 (+ i 1))) ((= i (vector-length x)))
   (let ((s minus-infinity)
	 (zi (vector-ref z i)))
    ;; Normalize ownerships to sum to one.
    (do ((j 0 (+ j 1))) ((= j (vector-length models)))
     (set! s (add-exp s (vector-ref zi j))))
    (do ((j 0 (+ j 1))) ((= j (vector-length models)))
     (vector-set! zi j (exp (- (vector-ref zi j) s))))
    (set! l (+ l s))))
  ;; Return log likelihood.
  l))

(define (m-step! x models z clip)
 (let ((kk (vector-length (vector-ref x 0))))
  ;; For each model, optimize parameters.
  (do ((j 0 (+ j 1))) ((= j (vector-length models)))
   (let* ((model (vector-ref models j))
	  (mu (model-mu model))
	  (sigma (model-sigma model))
	  (s 0.0))
    ;; Optimize values.
    (do ((k 0 (+ k 1))) ((= k kk))
     (do ((i 0 (+ i 1))) ((= i (vector-length x)))
      (set! s (+ s (vector-ref (vector-ref z i) j)))))
    (do ((k 0 (+ k 1))) ((= k kk))
     (let ((m 0.0))
      (do ((i 0 (+ i 1))) ((= i (vector-length x)))
       (set! m (+ m (* (vector-ref (vector-ref z i) j)
		       (vector-ref (vector-ref x i) k)))))
      (vector-set! mu k (/ m s))))
    (do ((k1 0 (+ k1 1))) ((= k1 kk))
     (let ((sigma-k1 (vector-ref sigma k1))
	   (mu-k1 (vector-ref mu k1)))
      (do ((k2 0 (+ k2 1))) ((= k2 kk))
       (let ((mu-k2 (vector-ref mu k2))
	     (m 0.0))
	(do ((i 0 (+ i 1))) ((= i (vector-length x)))
	 (set! m (+ m (* (vector-ref (vector-ref z i) j)
			 (- (vector-ref (vector-ref x i) k1) mu-k1)
			 (- (vector-ref (vector-ref x i) k2) mu-k2)))))
	(vector-set! sigma-k1 k2 (/ m s))))))
    (clip-eigenvalues! sigma clip)
    (set-model-pi! model (/ s (vector-length x)))
    (set-model-log-pi! model (log (/ s (vector-length x))))
    (invert-matrix! sigma (model-sigma-inverse model))
    (set-model-log-sigma-determinant! model (log (determinant sigma)))))))

(define (em! x z models clip em-kick-off-tolerance em-convergence-tolerance)
 (let loop ((old-log-likelihood minus-infinity) (starting? #t))
  (let ((log-likelihood (e-step! x z models)))
   (cond
    ((or (and starting? (> log-likelihood old-log-likelihood))
	 (> log-likelihood (+ old-log-likelihood em-convergence-tolerance)))
     (m-step! x models z clip)
     (loop log-likelihood
	   (and starting?
		(not (= (vector-length models) 1))
		(or (= old-log-likelihood minus-infinity)
		    (< log-likelihood
		       (+ old-log-likelihood em-kick-off-tolerance))))))
    (else old-log-likelihood)))))

(define (noise epsilon) (- (* 2.0 epsilon (/ (rand) *rand-max*)) epsilon))

(define (initial-z ii jj)
 (map-n-vector
  (lambda (i)
   (let ((zi (map-n-vector (lambda (j) (+ (/ jj) (noise (/ jj)))) jj))
	 (s 0.0))
    (do ((j 0 (+ j 1))) ((= j jj)) (set! s (+ s (vector-ref zi j))))
    (do ((j 0 (+ j 1))) ((= j jj)) (vector-set! zi j (/ (vector-ref zi j) s)))
    zi))
  ii))

(define (ems x clip em-kick-off-tolerance em-convergence-tolerance
	     ems-convergence-tolerance)
 (let loop ((jj 1)
	    (old-z #f)
	    (old-models #f)
	    (old-log-likelihood minus-infinity))
  (let* ((kk (vector-length (vector-ref x 0)))
	 (z (initial-z (vector-length x) jj))
	 (models (map-n-vector
		  (lambda (j)
		   (make-model 0.0
			       (make-vector kk)
			       (make-matrix kk kk)
			       0.0
			       (make-matrix kk kk)
			       0.0))
		  jj)))
   (m-step! x models z clip)
   (let ((new-log-likelihood
	  (em!
	   x z models clip em-kick-off-tolerance em-convergence-tolerance)))
    (if (> (- (/ old-log-likelihood new-log-likelihood) 1.0)
	   ems-convergence-tolerance)
	(loop (+ jj 1) z models new-log-likelihood)
	(list old-z old-models))))))

(define (em-clusterer x clip em-kick-off-tolerance em-convergence-tolerance
		      ems-convergence-tolerance)
 (let* ((z-models (ems x clip em-kick-off-tolerance
		       em-convergence-tolerance
		       ems-convergence-tolerance))
	(z (first z-models))
	(models (second z-models)))
  (e-step! x z models)
  (let ((clusters
	 (map-n (lambda (i)
		 (let ((zi (vector->list (vector-ref z i))))
		  (list i (positionv (reduce max zi minus-infinity) zi))))
		(vector-length z))))
   (map-n (lambda (j)
	   (map (lambda (cluster) (vector-ref x (first cluster)))
		(remove-if-not (lambda (cluster) (= (second cluster) j))
			       clusters)))
	  (vector-length (vector-ref z 0))))))

(define (go)
  (em-clusterer
   '#(#(1.0) #(2.0) #(3.0) #(11.0) #(12.0) #(13.0)) '#(1.0) 10.0 1.0 0.01))

(define (run-benchmark)
  (srand 1)
  (do ((i 0 (+ i 1))) ((= i 100))
    (write (go))
    (newline)))

; eof

