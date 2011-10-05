#lang racket/base

;; Extra math functions and constants.

(require racket/math racket/flonum racket/match racket/list racket/contract
         "contract.rkt" "contract-doc.rkt")

(provide (all-defined-out))

(define-struct ivl (min max) #:transparent
  #:guard (λ (a b _)
            (cond [(and (regular? a) (regular? b))  (values (min* a b) (max* a b))]
                  [else  (values a b)])))

(defproc (bounds->intervals [xs (listof real?)]) (listof ivl?)
  (cond [((length xs) . < . 2)  (raise-type-error 'bounds->intervals "list with length >= 2" xs)]
        [else
         (for/list ([x1  (in-list xs)]
                    [x2  (in-list (rest xs))])
           (ivl x1 x2))]))

(define -pi (- pi))
(define 2pi (* 2 pi))
(define -1/2pi (* -1/2 pi))
(define 1/2pi (* 1/2 pi))

(define 180/pi (fl/ 180.0 pi))
(define pi/180 (fl/ pi 180.0))

(defproc (degrees->radians [d real?]) real?
  (fl* (exact->inexact d) pi/180))

(defproc (radians->degrees [r real?]) real?
  (fl* (exact->inexact r) 180/pi))

(define (atan2 y x)
  (if (and (zero? y) (zero? x)) 0 (atan y x)))

(define (real-modulo x y) (- x (* y (floor (/ x y)))))

(define (sum f xs) (apply + (map f xs)))

(define (flmodulo x y) (fl- x (fl* y (flfloor (fl/ x y)))))
(define (fldist2 x y) (flsqrt (fl+ (fl* x x) (fl* y y))))
(define (fldist3 x y z) (flsqrt (fl+ (fl* x x) (fl+ (fl* y y) (fl* z z)))))

(define (flsum ps)
  (for/fold ([sum 0.0]) ([p  (in-list ps)])
    (fl+ sum p)))

(define (factorial n)
  (if (zero? n) 1 (* n (factorial (sub1 n)))))

(define (alpha-blend x1 x2 a)
  (+ (* a x1) (* (- 1 a) x2)))

(define (build-linear-seq start step num)
  (for/list ([n  (in-range num)])
    (+ start (* n step))))

(defproc (linear-seq [start real?] [end real?] [num (integer>=/c 0)]
                     [#:start? start? boolean? #t]
                     [#:end? end? boolean? #t]) (listof real?)
  (cond
    [(zero? num)  empty]
    ; ambiguous request: arbitrarily return start
    [(and start? end? (= 1 num))  (list start)]
    [else
     (define size (- end start))
     (define step (/ size (cond [(and start? end?)  (- num 1)]
                                [(or start? end?)   (- num 1/2)]
                                [else               num])))
     (define real-start
       (cond [start?  start]
             [else    (+ start (* 1/2 step))]))
     
     (build-linear-seq real-start step num)]))

(defproc (linear-seq* [points (listof real?)] [num (integer>=/c 0)]
                      [#:start? start? boolean? #t]
                      [#:end? end? boolean? #t]) (listof real?)
  (let/ec return
    (when (empty? points) (raise-type-error 'linear-seq* "nonempty (listof real?)" points))
    
    (define pts (list->vector points))
    (define len (vector-length pts))
    
    (define indexes (linear-seq 0 (sub1 len) num #:start? start? #:end? end?))
    (define int-parts (map floor indexes))
    (define frac-parts (map - indexes int-parts))
    (map (λ (i f)
           (if (= i (sub1 len))
               (vector-ref pts i)
               (alpha-blend (vector-ref pts (add1 i)) (vector-ref pts i) f)))
         int-parts frac-parts)))

(define (nan? x) (eqv? x +nan.0))
(define (infinite? x) (or (eqv? x +inf.0) (eqv? x -inf.0)))
(define (regular? x) (and (real? x) (not (nan? x)) (not (infinite? x))))

(define (min2 x y)
  (cond [(x . < . y)  x]
        [(y . < . x)  y]
        [(exact? x)  x]
        [else  y]))

(define (max2 x y)
  (cond [(x . > . y)  x]
        [(y . > . x)  y]
        [(exact? x)  x]
        [else  y]))

(define (clamp x mn mx) (min (max x mn) mx))
(define (clamp* x mn mx) (min2 (max2 x mn) mx))

(define (min* x . xs) (foldl min2 x xs))
(define (max* x . xs) (foldl max2 x xs))

(define (maybe-min x . xs)
  (for/fold ([x x]) ([y  (in-list xs)])
    (if x (if y (min* x y) x)
        (if y y #f))))

(define (maybe-max x . xs)
  (for/fold ([x x]) ([y  (in-list xs)])
    (if x (if y (max* x y) x)
        (if y y #f))))

(define (floor-log10 x)
  (inexact->exact (floor (/ (log (abs x)) (log 10)))))

(define (ceiling-log10 x)
  (inexact->exact (ceiling (/ (log (abs x)) (log 10)))))

(define (bin-samples bin-bounds xs)
  (let* ([bin-bounds  (filter (compose not nan?) (remove-duplicates bin-bounds))]
         [bin-bounds  (sort bin-bounds <)]
         [x-min  (first bin-bounds)]
         [x-max  (last bin-bounds)]
         [xs  (filter (λ (x) (<= x-min x x-max)) xs)]
         [xs  (sort xs <)])
    (define-values (res rest-xs)
      (for/fold ([res empty] [xs xs]) ([x1  (in-list bin-bounds)]
                                       [x2  (in-list (rest bin-bounds))])
        (define-values (lst rest-xs)
          (let loop ([lst empty] [xs xs])
            (if (and (not (empty? xs)) (<= x1 (first xs) x2))
                (loop (cons (first xs) lst) (rest xs))
                (values lst xs))))
        (values (cons (reverse lst) res)
                rest-xs)))
    (reverse res)))

(define (polar->cartesian θ r)
  (let ([θ  (exact->inexact θ)]
        [r  (exact->inexact r)])
    (vector (fl* r (flcos θ))
            (fl* r (flsin θ)))))

(define (3d-polar->3d-cartesian θ ρ r)
  (let* ([θ  (exact->inexact θ)]
         [ρ  (exact->inexact ρ)]
         [r  (exact->inexact r)]
         [cos-ρ  (flcos ρ)])
    (vector (fl* r (fl* (flcos θ) cos-ρ))
            (fl* r (fl* (flsin θ) cos-ρ))
            (fl* r (flsin ρ)))))
