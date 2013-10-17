(require racket/flonum)
(define-struct: point ([x : Float] [y : Float] [z : Float]))

(define-struct: scene ([c : point] [r : Float] [scenes : (Listof scene)]))

(define-struct: hit ([l : Float] [n : point]))

(define delta (flsqrt 1e-15))

(define ss 4)
(define orig (point 0.0 0.0 -4.0))

(: s*v : Float point -> point)
(define (s*v s b)
  (point (* s (point-x b)) (* s (point-y b)) (* s (point-z b))))

(: v+v : point point -> point)
(define (v+v a b)
  (point (+ (point-x a) (point-x b))
         (+ (point-y a) (point-y b))
         (+ (point-z a) (point-z b))))

(: v-v : point point -> point)
(define (v-v a b)
  (point (- (point-x a) (point-x b))
         (- (point-y a) (point-y b))
         (- (point-z a) (point-z b))))

(: dot : point point -> Float)
(define (dot a b)
 (+ (* (point-x a) (point-x b))
    (* (point-y a) (point-y b))
    (* (point-z a) (point-z b))))

(: magnitude : point -> Float)
(define (magnitude r) (flsqrt (dot r r)))

(: unitise : point -> point)
(define (unitise r) (s*v (/ 1.0 (magnitude r)) r))

(: ray-sphere : point point point Float -> Float)
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

(: intersect : point point scene -> hit)
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

(: ray-trace : point point scene -> Float)
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

(: create : Real Float point -> scene)
(define (create level r c)
 (let ((obj (scene c r '())))
  (if (= level 1)
      obj
      (let* ((r-prime (* 3.0 (/ r (flsqrt 12.0))))
	     (aux (lambda: ([x-prime : Float] [z-prime : Float])
		   (create (- level 1)
			   (* 0.5 r)
			   (v+v c (point x-prime r-prime z-prime)))))
	     (objs (list obj
			 (aux (- r-prime) (- r-prime))
			 (aux r-prime (- r-prime))
			 (aux (- r-prime) r-prime)
			 (aux r-prime r-prime))))
       (scene c (* 3.0 r) objs)))))

(: level Integer)
(define level 9)
(: n Float)
(define n 128.0)

(define the-scene (create level 1.0 (point 0.0 -1.0 0.0)))

(: aux : Float Float -> Float)
(define (aux x d) (+ (- x (/ n 2.0)) (/ d (exact->inexact ss))))

(: g : Float Float -> Float)
(define (g x y)
  (for*/fold: : Float ([sum : Float 0.0])
              ([dx : Natural (in-range ss)] [dy : Natural (in-range ss)])
    (+ sum
       (ray-trace orig
                  (unitise
                   (point (aux x (exact->inexact dx))
                          (aux (- (- n 1.0) y) (exact->inexact dy))
                          n))
                  the-scene))))

(: pixel : Float Float -> Integer)
(define (pixel x y)
  (round (inexact->exact (* 255.0 (/ (g x y) (* ss ss))))))

(time (for*: ([y : Natural (in-range n)])
        (for*: ([x : Natural (in-range n)])
          (pixel (exact->inexact x) (exact->inexact y)))))
