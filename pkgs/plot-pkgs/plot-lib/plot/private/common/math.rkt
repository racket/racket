#lang racket/base

(require racket/math racket/string racket/match racket/list racket/vector
         racket/contract racket/unsafe/ops
         unstable/flonum unstable/latent-contract/defthing)

(provide (all-defined-out))

;; ===================================================================================================
;; Integers

(defproc (factorial [n exact-nonnegative-integer?]) exact-nonnegative-integer?
  (if (zero? n) 1 (* n (factorial (sub1 n)))))

;; ===================================================================================================
;; Flonums

(defproc (flblend [x flonum?] [y flonum?] [α flonum?]) flonum?
  (cond [(not (flonum? x))  (raise-type-error 'flblend "flonum" 0 x y α)]
        [(not (flonum? y))  (raise-type-error 'flblend "flonum" 1 x y α)]
        [(not (flonum? α))  (raise-type-error 'flblend "flonum" 2 x y α)]
        [else  (unsafe-fl+ (unsafe-fl* α x) (unsafe-fl* (unsafe-fl- 1.0 α) y))]))

(defproc (flsum [f (any/c . -> . flonum?)] [xs (listof any/c)]) flonum?
  (define ys (map f xs))
  (cond [(not (andmap flonum? ys))  (raise-type-error 'sum "any -> flonum" f)]
        [else  (for/fold ([sum 0.0]) ([y  (in-list ys)])
                 (unsafe-fl+ sum y))]))

(define fldistance
  (case-lambda
    [()   0.0]
    [(x)  (if (flonum? x) (abs x) (raise-type-error 'fldistance "flonum" x))]
    [(x y)  (cond [(not (flonum? x))  (raise-type-error 'fldistance "flonum" 0 x y)]
                  [(not (flonum? y))  (raise-type-error 'fldistance "flonum" 1 x y)]
                  [else  (unsafe-flsqrt (unsafe-fl+ (unsafe-fl* x x) (unsafe-fl* y y)))])]
    [(x y z)  (cond [(not (flonum? x))  (raise-type-error 'fldistance "flonum" 0 x y z)]
                    [(not (flonum? y))  (raise-type-error 'fldistance "flonum" 1 x y z)]
                    [(not (flonum? z))  (raise-type-error 'fldistance "flonum" 2 x y z)]
                    [else  (unsafe-flsqrt (unsafe-fl+ (unsafe-fl+ (unsafe-fl* x x) (unsafe-fl* y y))
                                                      (unsafe-fl* z z)))])]
    [xs  (cond [(not (andmap flonum? xs))  (raise-type-error 'fldistance "flonums" xs)]
               [else  (unsafe-flsqrt (flsum (λ (x) (unsafe-fl* x x)) xs))])]))

;; ===================================================================================================
;; Reals

(defproc (maybe-inexact->exact [x (or/c rational? #f)]) (or/c rational? #f)
  (cond [x  (unless (rational? x)
              (raise-type-error 'maybe-inexact->exact "rational or #f" x))
            (inexact->exact x)]
        [else  #f]))

(define equal?*
  (case-lambda
    [()   #t]
    [(x)  #t]
    [xs   (and (equal? (car xs) (cadr xs))
               (apply equal?* (cdr xs)))]))

(define-syntax-rule (min2* x y)
  (cond [(x . < . y)  x]
        [(y . < . x)  y]
        [(exact? x)   x]
        [else  y]))

(define-syntax-rule (max2* x y)
  (cond [(x . > . y)  x]
        [(y . > . x)  y]
        [(exact? x)   x]
        [else  y]))

(define min*
  (case-lambda
    [()        +inf.0]
    [(x)       (if (real? x) x (raise-type-error 'min* "real number" x))]
    [(x y)     (cond [(not (real? x))  (raise-type-error 'min* "real number" 0 x y)]
                     [(not (real? y))  (raise-type-error 'min* "real number" 1 x y)]
                     [else  (min2* x y)])]
    [(x . xs)  (cond [(not (real? x))  (apply raise-type-error 'min* "real number" 0 x xs)]
                     [else  (for/fold ([m x]) ([y  (in-list xs)] [i  (in-naturals 1)])
                              (cond [(real? y)  (min2* m y)]
                                    [else  (apply raise-type-error 'min* "real number" i x xs)]))])]))

(define max*
  (case-lambda
    [()        -inf.0]
    [(x)       (if (real? x) x (raise-type-error 'max* "real number" x))]
    [(x y)     (cond [(not (real? x))  (raise-type-error 'max* "real number" 0 x y)]
                     [(not (real? y))  (raise-type-error 'max* "real number" 1 x y)]
                     [else  (max2* x y)])]
    [(x . xs)  (cond [(not (real? x))  (apply raise-type-error 'max* "real number" 0 x xs)]
                     [else  (for/fold ([m x]) ([y  (in-list xs)] [i  (in-naturals 1)])
                              (cond [(real? y)  (max2* m y)]
                                    [else  (apply raise-type-error 'max* "real number" i x xs)]))])]))

(defproc (blend [x real?] [y real?] [α real?]) real?
  (cond [(not (real? x))  (raise-type-error 'blend "real number" 0 x y α)]
        [(not (real? y))  (raise-type-error 'blend "real number" 1 x y α)]
        [(not (real? α))  (raise-type-error 'blend "real number" 2 x y α)]
        [else  (+ (* α x) (* (- 1 α) y))]))

(defproc (atan2 [y real?] [x real?]) real?
  (cond [(not (real? y))  (raise-type-error 'atan2 "real number" 0 y x)]
        [(not (real? x))  (raise-type-error 'atan2 "real number" 1 y x)]
        [(and (zero? y) (zero? x))  0]
        [else  (atan y x)]))

(defproc (sum [f (any/c . -> . real?)] [xs (listof any/c)]) real?
  (define ys (map f xs))
  (cond [(not (andmap real? ys))  (raise-type-error 'sum "any -> real" f)]
        [else  (apply + ys)]))

(defproc (real-modulo [x real?] [y real?]) real?
  (cond [(not (real? x))  (raise-type-error 'real-modulo "real number" 0 x y)]
        [(not (real? y))  (raise-type-error 'real-modulo "real number" 1 x y)]
        [else  (- x (* y (floor (/ x y))))]))

(define distance
  (case-lambda
    [()  0]
    [(x)  (if (real? x) (abs x) (raise-type-error 'distance "real number" x))]
    [(x y)  (cond [(not (real? x))  (raise-type-error 'distance "real number" 0 x y)]
                  [(not (real? y))  (raise-type-error 'distance "real number" 1 x y)]
                  [else  (sqrt (+ (* x x) (* y y)))])]
    [(x y z)  (cond [(not (real? x))  (raise-type-error 'distance "real number" 0 x y z)]
                    [(not (real? y))  (raise-type-error 'distance "real number" 1 x y z)]
                    [(not (real? z))  (raise-type-error 'distance "real number" 2 x y z)]
                    [else  (sqrt (+ (* x x) (* y y) (* z z)))])]
    [xs  (cond [(not (andmap real? xs)) (raise-type-error 'distance "real numbers" xs)]
               [else  (sqrt (sum sqr xs))])]))

(define (exact∘log x)
  (define y (log x))
  (cond [(infinite? y)  (- (inexact->exact (log (numerator x)))
                           (inexact->exact (log (denominator x))))]
        [else  (inexact->exact y)]))

(defproc (floor-log/base [b (and/c exact-integer? (>=/c 2))] [x (>/c 0)]) exact-integer?
  (cond [(not (and (exact-integer? b) (b . >= . 2)))
         (raise-type-error 'floor-log/base "exact integer >= 2" 0 b x)]
        [(not (and (real? x) (x . > . 0)))
         (raise-type-error 'floor-log/base "real > 0" 1 b x)]
        [else
         (define q (inexact->exact x))
         (define m (floor (/ (exact∘log q) (inexact->exact (log b)))))
         (let loop ([m m] [p  (expt b m)])
           (cond [(q . < . p)  (loop (sub1 m) (/ p b))]
                 [else  (define u (* p b))
                        (cond [(q . >= . u)  (loop (add1 m) u)]
                              [else  m])]))]))

(defproc (ceiling-log/base [b (and/c exact-integer? (>=/c 2))] [x (>/c 0)]) exact-integer?
  (- (floor-log/base b (/ (inexact->exact x)))))

(defproc (polar->cartesian [θ real?] [r real?]) (vector/c real? real?)
  (cond [(not (real? θ))  (raise-type-error 'polar->cartesian "real number" 0 θ r)]
        [(not (real? r))  (raise-type-error 'polar->cartesian "real number" 1 θ r)]
        [else  (let ([θ  (exact->inexact θ)]
                     [r  (exact->inexact r)])
                 (vector (unsafe-fl* r (unsafe-flcos θ))
                         (unsafe-fl* r (unsafe-flsin θ))))]))

(defproc (3d-polar->3d-cartesian [θ real?] [ρ real?] [r real?]) (vector/c real? real? real?)
  (cond [(not (real? θ))  (raise-type-error '3d-polar->3d-cartesian "real number" 0 θ ρ r)]
        [(not (real? ρ))  (raise-type-error '3d-polar->3d-cartesian "real number" 1 θ ρ r)]
        [(not (real? r))  (raise-type-error '3d-polar->3d-cartesian "real number" 2 θ ρ r)]
        [else  (let ([θ  (exact->inexact θ)]
                     [ρ  (exact->inexact ρ)]
                     [r  (exact->inexact r)])
                 (let ([cos-ρ  (unsafe-flcos ρ)])
                   (vector (unsafe-fl* r (unsafe-fl* (unsafe-flcos θ) cos-ρ))
                           (unsafe-fl* r (unsafe-fl* (unsafe-flsin θ) cos-ρ))
                           (unsafe-fl* r (unsafe-flsin ρ)))))]))

;; ===================================================================================================
;; Vectors

(define vector-andmap
  (case-lambda
    [(f v)  (let/ec break
              (for ([e  (in-vector v)])
                (unless (f e) (break #f)))
              #t)]
    [(f v . vs)  (define ns (cons (vector-length v) (map vector-length vs)))
                 (unless (apply equal?* ns)
                   (error 'vector-andmap "all vectors must have same size; arguments were ~e ~e ~e"
                          f v (string-join (map (λ (v) (format "~e" v)) vs))))
                 (let/ec break
                   (define ess (apply map list (map vector->list vs)))
                   (for ([e  (in-vector v)] [es  (in-list ess)])
                     (when (not (apply f e es)) (break #f)))
                   #t)]))

(define vector-ormap
  (case-lambda
    [(f v)  (let/ec break
              (for ([e  (in-vector v)])
                (when (f e) (break #t)))
              #f)]
    [(f v . vs)  (define ns (cons (vector-length v) (map vector-length vs)))
                 (unless (apply equal?* ns)
                   (error 'vector-andmap "all vectors must have same size; arguments were ~e ~e ~e"
                          f v (string-join (map (λ (v) (format "~e" v)) vs))))
                 (let/ec break
                   (define ess (apply map list (map vector->list vs)))
                   (for ([e  (in-vector v)] [es  (in-list ess)])
                     (when (apply f e es) (break #t)))
                   #f)]))

(defproc (vcross [v1 (vector/c real? real? real?)] [v2 (vector/c real? real? real?)]
                 ) (vector/c real? real? real?)
  (match v1
    [(vector (? real? x1) (? real? y1) (? real? z1))
     (match v2
       [(vector (? real? x2) (? real? y2) (? real? z2))
        (vector (- (* y1 z2) (* z1 y2))
                (- (* z1 x2) (* x1 z2))
                (- (* x1 y2) (* y1 x2)))]
       [_  (raise-type-error 'vcross "vector of 3 real numbers" 1 v1 v2)])]
    [_  (raise-type-error 'vcross "vector of 3 real numbers" 0 v1 v2)]))

(defproc (vcross2 [v1 (vector/c real? real?)] [v2 (vector/c real? real?)]) real?
  (match v1
    [(vector (? real? x1) (? real? y1))
     (match v2
       [(vector (? real? x2) (? real? y2))  (- (* x1 y2) (* y1 x2))]
       [_  (raise-type-error 'vcross "vector of 2 real numbers" 1 v1 v2)])]
    [_  (raise-type-error 'vcross "vector of 2 real numbers" 0 v1 v2)]))

(define-syntax-rule (vmap name f v)
  (let ()
    (unless (vector? v)
      (raise-type-error name "vector of real numbers" v))
    (define n (vector-length v))
    (for/vector #:length n ([x  (in-vector v)])
      (cond [(real? x)    (f x)]
            [else  (raise-type-error name "vector of real numbers" v)]))))

(define-syntax-rule (unrolled-vmap name f v)
  (let ()
    (match v
      [(vector (? real? x) (? real? y))              (vector (f x) (f y))]
      [(vector (? real? x) (? real? y) (? real? z))  (vector (f x) (f y) (f z))]
      [_  (vmap name f v)])))

(define-syntax-rule (vmap2 name f v1 v2)
  (let ()
    (unless (vector? v1)
      (raise-type-error name "vector of real numbers" 0 v1 v2))
    (unless (vector? v2)
      (raise-type-error name "vector of real numbers" 1 v1 v2))
    (define n (vector-length v1))
    (unless (= n (vector-length v2))
      (raise-type-error name (format "vector of ~a real numbers" n) 1 v1 v2))
    (for/vector #:length n ([x  (in-vector v1)] [y  (in-vector v2)])
      (if (real? x)
          (if (real? y)
              (f x y)
              (raise-type-error name "vector of real numbers" 1 v1 v2))
          (raise-type-error name "vector of real numbers" 0 v1 v2)))))

(define-syntax-rule (unrolled-vmap2 name f v1 v2)
  (match v1
    [(vector (? real? x1) (? real? y1))
     (match v2
       [(vector (? real? x2) (? real? y2))  (vector (f x1 x2) (f y1 y2))]
       [_  (raise-type-error name "vector of 2 real numbers" 1 v1 v2)])]
    [(vector (? real? x1) (? real? y1) (? real? z1))
     (match v2
       [(vector (? real? x2) (? real? y2) (? real? z2))  (vector (f x1 x2) (f y1 y2) (f z1 z2))]
       [_  (raise-type-error name "vector of 3 real numbers" 1 v1 v2)])]
    [_  (vmap2 name f v1 v2)]))

(defproc (v+ [v1 (vectorof real?)] [v2 (vectorof real?)]) (vectorof real?)
  (unrolled-vmap2 'v+ + v1 v2))

(defproc (v- [v1 (vectorof real?)] [v2 (vectorof real?)]) (vectorof real?)
  (unrolled-vmap2 'v- - v1 v2))

(defproc (vneg [v (vectorof real?)]) (vectorof real?)
  (unrolled-vmap 'vneg - v))

(defproc (v* [v (vectorof real?)] [c real?]) (vectorof real?)
  (cond [(real? c)  (define-syntax-rule (f x) (* x c))
                    (unrolled-vmap 'v* f v)]
        [else  (raise-type-error 'v* "real" 1 v c)]))

(defproc (v/ [v (vectorof real?)] [c real?]) (vectorof real?)
  (cond [(real? c)  (define-syntax-rule (f x) (/ x c))
                    (unrolled-vmap 'v/ f v)]
        [else  (raise-type-error 'v/ "real" 1 v c)]))

(defproc (vmag^2 [v (vectorof real?)]) real?
  (match v
    [(vector (? real? x) (? real? y))              (+ (* x x) (* y y))]
    [(vector (? real? x) (? real? y) (? real? z))  (+ (* x x) (* y y) (* z z))]
    [_  (unless (vector? v)
          (raise-type-error 'vmag^2 "vector of real numbers" v))
        (for/fold ([mag  0]) ([x  (in-vector v)])
          (+ mag (cond [(real? x)    (* x x)]
                       [else  (raise-type-error 'vmag^2 "vector of real numbers" v)])))]))

(defproc (vmag [v (vectorof real?)]) real?
  (sqrt (vmag^2 v)))

(defproc (vnormalize [v (vectorof real?)]) (vectorof real?)
  (match v
    [(vector (? real? x) (? real? y))              (define m (sqrt (+ (* x x) (* y y))))
                                                   (if (= m 0) v (vector (/ x m) (/ y m)))]
    [(vector (? real? x) (? real? y) (? real? z))  (define m (sqrt (+ (* x x) (* y y) (* z z))))
                                                   (if (= m 0) v (vector (/ x m) (/ y m) (/ z m)))]
    [_  (define m (vmag v))
        (if (= m 0) v (v/ v m))]))

(defproc (vdot [v1 (vectorof real?)] [v2 (vectorof real?)]) real?
  (match v1
    [(vector (? real? x1) (? real? y1))
     (match v2
       [(vector (? real? x2) (? real? y2))  (+ (* x1 x2) (* y1 y2))]
       [_  (raise-type-error 'vdot "vector of 2 real numbers" 1 v1 v2)])]
    [(vector (? real? x1) (? real? y1) (? real? z1))
     (match v2
       [(vector (? real? x2) (? real? y2) (? real? z2))  (+ (* x1 x2) (* y1 y2) (* z1 z2))]
       [_  (raise-type-error 'vdot "vector of 3 real numbers" 1 v1 v2)])]
    [_  (unless (= (vector-length v1) (vector-length v2))
          (raise-type-error 'vdot (format "vector of ~a real numbers" (vector-length v1)) 1 v1 v2))
        (for/fold ([dot  0]) ([x1  (in-vector v1)] [x2  (in-vector v2)])
          (if (real? x1)
              (if (real? x2)
                  (+ dot (* x1 x2))
                  (raise-type-error 'vdot "vector of real numbers" 1 v1 v2))
              (raise-type-error 'vdot "vector of real numbers" 0 v1 v2)))]))

(defproc (vcos-angle [v1 (vectorof real?)] [v2 (vectorof real?)]) real?
  (define d (vdot v1 v2))
  (cond [(= d 0)  0]
        [else     (/ d (vmag v1) (vmag v2))]))

(defproc (vrational? [v (vectorof real?)]) boolean?
  (match v
    [(vector (? rational? x) (? rational? y))  #t]
    [(vector (? rational? x) (? rational? y) (? rational? z))  #t]
    [(? vector?)  (vector-andmap rational? v)]
    [_  (raise-type-error 'vrational? "vector" v)]))

(defproc (v= [v1 (vectorof real?)] [v2 (vectorof real?)]) boolean?
  (match v1
    [(vector (? real? x1) (? real? y1))
     (match v2
       [(vector (? real? x2) (? real? y2))  (and (= x1 x2) (= y1 y2))]
       [_  (raise-type-error 'v= "vector of 2 real numbers" 1 v1 v2)])]
    [(vector (? real? x1) (? real? y1) (? real? z1))
     (match v2
       [(vector (? real? x2) (? real? y2) (? real? z2))  (and (= x1 x2) (= y1 y2) (= z1 z2))]
       [_  (raise-type-error 'v= "vector of 3 real numbers" 1 v1 v2)])]
    [_  (unless (= (vector-length v1) (vector-length v2))
          (raise-type-error 'v= (format "vector of ~a real numbers" (vector-length v1)) 1 v1 v2))
        (let/ec break
          (for ([x1  (in-vector v1)] [x2  (in-vector v2)])
            (if (real? x1)
                (if (real? x2)
                    (unless (= x1 x2) (break #f))
                    (raise-type-error 'v= "vector of real numbers" 1 v1 v2))
                (raise-type-error 'v= "vector of real numbers" 0 v1 v2)))
          #t)]))

(defproc (vcenter [vs (listof (vectorof real?))]) (vectorof real?)
  (match vs
    [(list (vector xs ys) ...)
     (define x-min (apply min* xs))
     (define x-max (apply max* xs))
     (define y-min (apply min* ys))
     (define y-max (apply max* ys))
     (vector (* 1/2 (+ x-min x-max)) (* 1/2 (+ y-min y-max)))]
    [(list (vector xs ys zs) ...)
     (define x-min (apply min* xs))
     (define x-max (apply max* xs))
     (define y-min (apply min* ys))
     (define y-max (apply max* ys))
     (define z-min (apply min* zs))
     (define z-max (apply max* zs))
     (vector (* 1/2 (+ x-min x-max)) (* 1/2 (+ y-min y-max)) (* 1/2 (+ z-min z-max)))]
    [_
     (define xss (apply vector-map list vs))
     (define mins (vector-map (λ (xs) (apply min xs)) xss))
     (define maxs (vector-map (λ (xs) (apply max xs)) xss))
     (unrolled-vmap2 'vcenter (λ (x1 x2) (* 1/2 (+ x1 x2))) mins maxs)]))

(define (vrational-sublists vs)
  (define res
    (let loop ([vs vs])
      (cond [(null? vs)  (list null)]
            [(vrational? (car vs))  (define rst (loop (cdr vs)))
                                    (cons (cons (car vs) (car rst)) (cdr rst))]
            [else  (cons null (loop (cdr vs)))])))
  (cond [(and (not (null? res)) (null? (car res)))  (cdr res)]
        [else  res]))

(define (remove-degenerate-edges vs)
  (cond
    [(empty? vs)  empty]
    [else
     (let*-values ([(last vs)
                    (for/fold ([last  (first vs)] [vs  (list (first vs))])
                      ([v  (in-list (rest vs))])
                      (cond [(v= last v)  (values v vs)]
                            [else         (values v (cons v vs))]))]
                   [(vs)  (reverse vs)])
       (cond [(v= last (first vs))  (rest vs)]
             [else  vs]))]))

(define default-normal (vector 0.0 -1.0 0.0))

(define (vnormal vs)
  (let ([vs  (remove-degenerate-edges vs)])
    (cond
      [((length vs) . < . 3)  default-normal]
      [else
       (let ([vs  (append vs (take vs 2))])
         (define norm
           (for/fold ([norm  (vector 0.0 0.0 0.0)]) ([v1  (in-list vs)]
                                                     [v2  (in-list (rest vs))]
                                                     [v3  (in-list (rest (rest vs)))])
             (v+ norm (vcross (v- v3 v2) (v- v1 v2)))))
         (define m (vmag norm))
         (if (m . > . 0) (v/ norm m) default-normal))])))

;; ===================================================================================================
;; Intervals

(define-syntax-rule (maybe-min x y)
  (if x (if y (min* x y) x)
      (if y y #f)))

(define-syntax-rule (maybe-max x y)
  (if x (if y (max* x y) x)
      (if y y #f)))

(define (maybe-real? x)
  (or (real? x) (not x)))

(struct ivl (min max) #:transparent
  #:guard (λ (a b _)
            (cond [(or (and a (nan? a)) (and b (nan? b)))  (values +nan.0 +nan.0)]
                  [(and a b)  (values (min* a b) (max* a b))]
                  [else  (values a b)])))

(defthing empty-ivl ivl? (ivl +nan.0 +nan.0))
(defthing unknown-ivl ivl? (ivl #f #f))

(defproc (ivl-empty? [i ivl?]) boolean?
  (define a (ivl-min i))
  (and a (nan? a)))

(defproc (ivl-known? [i ivl?]) boolean?
  (match-define (ivl a b) i)
  (and a b #t))

(defproc (ivl-rational? [i ivl?]) boolean?
  (match-define (ivl a b) i)
  (and (rational? a) (rational? b)))

(defproc (rational-ivl? [i any/c]) boolean?
  (and (ivl? i) (ivl-rational? i)))

(defproc (ivl-singular? [i ivl?]) boolean?
  (match-define (ivl a b) i)
  (and a b (= a b)))

(defproc (ivl-length [i ivl?]) (or/c real? #f)
  (match-define (ivl a b) i)
  (if (and a b) (- b a) #f))

(defproc (ivl-center [i ivl?]) (or/c real? #f)
  (match-define (ivl a b) i)
  (if (and a b) (* 1/2 (+ a b)) #f))

(defproc (ivl-zero-length? [i ivl?]) boolean?
  (or (ivl-empty? i) (ivl-singular? i)))

(defproc (ivl-inexact->exact [i ivl?]) ivl?
  (match-define (ivl a b) i)
  (ivl (and a (if (nan? a) a (inexact->exact a)))
       (and b (if (nan? b) b (inexact->exact b)))))

(defproc (ivl-contains? [i ivl?] [x real?]) boolean?
  (match-define (ivl a b) i)
  (and a b (x . >= . a) (x . <= . b)))

(define (ivl-meet2 i1 i2) ivl?
  (cond [(or (ivl-empty? i1) (ivl-empty? i2))  empty-ivl]
        [else
         (match-define (ivl a1 b1) i1)
         (match-define (ivl a2 b2) i2)
         (define a (maybe-max a1 a2))
         (define b (maybe-min b1 b2))
         (if (and a b (a . > . b)) empty-ivl (ivl a b))]))

(define (ivl-meet . is)
  (for/fold ([res  unknown-ivl]) ([i  (in-list is)])
    (ivl-meet2 res i)))

(define (ivl-join2 i1 i2)
  (cond [(ivl-empty? i1)  i2]
        [(ivl-empty? i2)  i1]
        [else
         (match-define (ivl a1 b1) i1)
         (match-define (ivl a2 b2) i2)
         (ivl (maybe-min a1 a2) (maybe-max b1 b2))]))

(define (ivl-join . is)
  (for/fold ([res  empty-ivl]) ([i  (in-list is)])
    (ivl-join2 res i)))

(defproc (ivl-translate [i ivl?] [d real?]) ivl?
  (match-define (ivl a b) i)
  (ivl (and a (+ a d)) (and b (+ b d))))

(defproc (bounds->intervals [xs (listof real?)]) (listof ivl?)
  (cond [((length xs) . < . 2)  (raise-type-error 'bounds->intervals "list with length >= 2" xs)]
        [else
         (for/list ([x1  (in-list xs)]
                    [x2  (in-list (rest xs))])
           (ivl x1 x2))]))

(defproc (clamp-real [x real?] [i ivl?]) real?
  (match-define (ivl a b) i)
  (max (min x b) a))

;; ===================================================================================================
;; Rectangles

(defproc (empty-rect [n exact-nonnegative-integer?]) (vectorof ivl?)
  (make-vector n empty-ivl))

(defproc (unknown-rect [n exact-nonnegative-integer?]) (vectorof ivl?)
  (make-vector n unknown-ivl))

(defproc (bounding-rect [vs (listof (vectorof ivl?))]) (vectorof ivl?)
  (define xss (apply vector-map list vs))
  (define vmin (vector-map (λ (xs) (apply min xs)) xss))
  (define vmax (vector-map (λ (xs) (apply max xs)) xss))
  (vector-map ivl vmin vmax))

(defproc (rect-empty? [r (vectorof ivl?)]) boolean?
  (vector-ormap ivl-empty? r))

(defproc (rect-known? [r (vectorof ivl?)]) boolean?
  (vector-andmap ivl-known? r))

(defproc (rect-rational? [r (vectorof ivl?)]) boolean?
  (vector-andmap ivl-rational? r))

(defproc (rational-rect? [r any/c]) boolean?
  (and (vector? r) (vector-andmap rational-ivl? r)))

(defproc (rect-area [r (vectorof ivl?)]) (or/c real? #f)
  (let/ec break
    (for/fold ([area 1]) ([i  (in-vector r)])
      (define len (ivl-length i))
      (when (or (not len) (zero? len)) (break len))
      (* area (ivl-length i)))))

(defproc (rect-center [r (vectorof ivl?)]) (vectorof real?)
  (vector-map ivl-center r))

(defproc (rect-zero-area? [r (vectorof ivl?)]) boolean?
  (vector-ormap ivl-zero-length? r))

(defproc (rect-singular? [r (vectorof ivl?)]) boolean?
  (vector-andmap ivl-singular? r))

(defproc (rect-inexact->exact [r (vectorof ivl?)]) (vectorof ivl?)
  (vector-map ivl-inexact->exact r))

(defproc (rect-contains? [r (vectorof ivl?)] [v (vectorof real?)]) boolean?
  (vector-andmap ivl-contains? r v))

(define (rect-meet . rs)
  (apply vector-map ivl-meet rs))

(define (rect-join . rs)
  (apply vector-map ivl-join rs))

(defproc (rect-translate [r (vectorof ivl?)] [v (vectorof real?)]) (vectorof ivl?)
  (vector-map ivl-translate r v))
