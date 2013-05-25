(require racket/flonum)
(define-struct point (x y z))

(define-struct scene (c r scenes))

(define-struct hit (l n))

(define delta (flsqrt 1e-15))

(define ss 4)
(define orig (point 0.0 0.0 -4.0))

(define (s*v s b)
  (point (* s (point-x b)) (* s (point-y b)) (* s (point-z b))))

(define (v+v a b)
  (point (+ (point-x a) (point-x b))
         (+ (point-y a) (point-y b))
         (+ (point-z a) (point-z b))))

(define (v-v a b)
  (point (- (point-x a) (point-x b))
         (- (point-y a) (point-y b))
         (- (point-z a) (point-z b))))

(define (dot a b)
 (+ (* (point-x a) (point-x b))
    (* (point-y a) (point-y b))
    (* (point-z a) (point-z b))))

(define (magnitude r) (flsqrt (dot r r)))

(define (unitise r) (s*v (/ 1.0 (magnitude r)) r))

(define (ray-sphere orig dir center radius)
 (let* ((v (v-v center orig))
	(b (dot v dir))
	(disc (+ (- (* b b) (dot v v)) (* radius radius))))
  (if (negative? disc)
      +inf.0
      (let* ((disc (flsqrt disc)) (t2 (+ b disc)))
       (if (negative? t2)
	   +inf.0
	   (let ((t1 (- b disc))) (if (positive? t1) t1 t2)))))))

(define zero (point 0.0 0.0 0.0))

(define (intersect orig dir scene)
 (let aux ((scene scene) (hit (make-hit +inf.0 zero)))
  (let ((l (hit-l hit)))
   (if (null? (scene-scenes scene))
       (let ((l-prime (ray-sphere orig dir (scene-c scene) (scene-r scene))))
	(if (>= l-prime l)
	    hit
	    (make-hit
	     l-prime
	     (unitise (v+v orig (v-v (s*v l-prime dir) (scene-c scene)))))))
       (if (>= (ray-sphere orig dir (scene-c scene) (scene-r scene)) l)
	   hit
	   (foldr aux hit (scene-scenes scene)))))))

(define neg-light (unitise (point 1.0 3.0 -2.0)))

(define (ray-trace orig dir scene)
 (let* ([hit (intersect orig dir scene)] [lam (hit-l hit)] [n (hit-n hit)])
  (if (= lam +inf.0)
      0.0
      (let ([g (dot n neg-light)])
       (if (and (positive? g)
                (= (hit-l (intersect
                           (v+v orig (v+v (s*v lam dir) (s*v delta n)))
                           neg-light
                           scene))
                   +inf.0))
	   g
	   0.0)))))

(define (create level r c)
 (let ((obj (scene c r '())))
  (if (= level 1)
      obj
      (let* ((r-prime (* 3.0 (/ r (flsqrt 12.0))))
	     (aux (lambda (x-prime z-prime)
		   (create (- level 1)
			   (* 0.5 r)
			   (v+v c (point x-prime r-prime z-prime)))))
	     (objs (list obj
			 (aux (- r-prime) (- r-prime))
			 (aux r-prime (- r-prime))
			 (aux (- r-prime) r-prime)
			 (aux r-prime r-prime))))
       (scene c (* 3.0 r) objs)))))

(define level 9)
(define n 128.0)

(define the-scene (create level 1.0 (point 0.0 -1.0 0.0)))

(define (aux x d) (+ (- x (/ n 2.0)) (/ d (exact->inexact ss))))

(define (g x y)
  (for*/fold ([sum 0.0])
             ([dx (in-range ss)] [dy (in-range ss)])
    (+ sum
       (ray-trace orig
                  (unitise
                   (point (aux x (exact->inexact dx))
                          (aux (- (- n 1.0) y) (exact->inexact dy))
                          n))
                  the-scene))))

(define (pixel x y)
  (round (inexact->exact (* 255.0 (/ (g x y) (* ss ss))))))

(time (for* ([y (in-range n)])
        (for* ([x (in-range n)])
          (pixel (exact->inexact x) (exact->inexact y)))))
