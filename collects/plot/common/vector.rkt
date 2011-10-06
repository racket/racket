#lang racket/base

;; A small vector library.

(require racket/match racket/vector racket/math racket/list
         "math.rkt")

(provide (all-defined-out))

(define (vcross v1 v2)
  (match-define (vector x1 y1 z1) v1)
  (match-define (vector x2 y2 z2) v2)
  (vector (- (* y1 z2) (* z1 y2))
          (- (* z1 x2) (* x1 z2))
          (- (* x1 y2) (* y1 x2))))

(define (v+ v1 v2) (vector-map + v1 v2))
(define (v- v1 v2) (vector-map - v1 v2))
(define (vneg v) (vector-map - v))
(define (v* v c) (vector-map (λ (x) (* x c)) v))
(define (v/ v c) (vector-map (λ (x) (/ x c)) v))
(define (vround v) (vector-map round v))

(define (vmag^2 v)
  (for/fold ([mag  0]) ([x  (in-vector v)])
    (+ mag (sqr x))))

(define (vdot v1 v2)
  (for/fold ([dot  0]) ([x1  (in-vector v1)] [x2  (in-vector v2)])
    (+ dot (* x1 x2))))

(define (vmag v) (sqrt (vmag^2 v)))
(define (vnormalize v) (v/ v (vmag v)))

(define (vregular? v)
  (let/ec return
    (for ([x  (in-vector v)])
      (when (not (regular? x))
        (return #f)))
    #t))

(define (v= v1 v2)
  (let/ec return
    (for ([x1  (in-vector v1)] [x2  (in-vector v2)])
      (when (not (= x1 x2))
        (return #f)))
    #t))

(define (vregular-sublists vs)
  (cond [(null? vs)  (list null)]
        [(vregular? (car vs))  (define rst (vregular-sublists (cdr vs)))
                               (cons (cons (car vs) (car rst)) (cdr rst))]
        [else  (cons null (vregular-sublists (cdr vs)))]))

(define (bounding-box vs)
  (match-define (list (vector xs ys zs) ...) vs)
  (values (apply min xs) (apply max xs)
          (apply min ys) (apply max ys)
          (apply min zs) (apply max zs)))

;; Returns the center of the smallest axial bounding rectangle containing the points.
(define (center-coord vs)
  (define-values (x-min x-max y-min y-max z-min z-max) (bounding-box vs))
  (vector (* 1/2 (+ x-min x-max))
          (* 1/2 (+ y-min y-max))
          (* 1/2 (+ z-min z-max))))

(define default-normal (vector 0 -1 0))

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

(define (surface-normal vs)
  (let ([vs  (remove-degenerate-edges vs)])
    (cond
      [((length vs) . < . 3)  default-normal]
      [else
       (let* ([vs  (append vs (take vs 2))]
              [n   (for/fold ([n  (vector 0 0 0)])
                             ([v1  (in-list vs)]
                              [v2  (in-list (rest vs))]
                              [v3  (in-list (rest (rest vs)))])
                     (v+ n (vcross (v- v3 v2) (v- v1 v2))))]
              [m   (vmag^2 n)])
         (cond [(m . > . 0)  (v/ n (sqrt m))]
               [else  default-normal]))])))
