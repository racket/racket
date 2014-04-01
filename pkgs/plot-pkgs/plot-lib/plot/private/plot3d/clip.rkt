#lang racket/base

;; Small library for clipping points, lines and polygons against axial planes.

(require racket/match racket/list racket/unsafe/ops
         "../common/utils.rkt")

(provide point-in-bounds? clip-line clip-polygon clip-lines
         clip-polygon-x-min clip-polygon-x-max
         clip-polygon-y-min clip-polygon-y-max
         clip-polygon-z-min clip-polygon-z-max)

;; ===================================================================================================
;; Points

(define (point-in-bounds? v x-min x-max y-min y-max z-min z-max)
  (match-define (vector x y z) v)
  (and (<= x-min x x-max) (<= y-min y y-max) (<= z-min z z-max)))

;; ===================================================================================================
;; Line clipping

(define (clip-line-x start-in-bounds? x x1 y1 z1 x2 y2 z2)
  (let-map
   (x x1 y1 z1 x2 y2 z2) inexact->exact
   (define t (/ (- x x1) (- x2 x1)))
   (cond [start-in-bounds?  (values x1 y1 z1 x (+ y1 (* t (- y2 y1))) (+ z1 (* t (- z2 z1))))]
         [else              (values x (+ y1 (* t (- y2 y1))) (+ z1 (* t (- z2 z1))) x2 y2 z2)])))

(define (clip-line-y start-in-bounds? y x1 y1 z1 x2 y2 z2)
  (let-map
   (y x1 y1 z1 x2 y2 z2) inexact->exact
   (define t (/ (- y y1) (- y2 y1)))
   (cond [start-in-bounds?  (values x1 y1 z1 (+ x1 (* t (- x2 x1))) y (+ z1 (* t (- z2 z1))))]
         [else              (values (+ x1 (* t (- x2 x1))) y (+ z1 (* t (- z2 z1))) x2 y2 z2)])))

(define (clip-line-z start-in-bounds? z x1 y1 z1 x2 y2 z2)
  (let-map
   (z x1 y1 z1 x2 y2 z2) inexact->exact
   (define t (/ (- z z1) (- z2 z1)))
   (cond [start-in-bounds?  (values x1 y1 z1 (+ x1 (* t (- x2 x1))) (+ y1 (* t (- y2 y1))) z)]
         [else              (values (+ x1 (* t (- x2 x1))) (+ y1 (* t (- y2 y1))) z x2 y2 z2)])))

(define (clip-line-x-min x-min x1 y1 z1 x2 y2 z2)
  (cond [(and (x1 . >= . x-min) (x2 . < . x-min))  (clip-line-x #t x-min x1 y1 z1 x2 y2 z2)]
        [(and (x2 . >= . x-min) (x1 . < . x-min))  (clip-line-x #f x-min x1 y1 z1 x2 y2 z2)]
        [else                                      (values x1 y1 z1 x2 y2 z2)]))

(define (clip-line-x-max x-max x1 y1 z1 x2 y2 z2)
  (cond [(and (x1 . <= . x-max) (x2 . > . x-max))  (clip-line-x #t x-max x1 y1 z1 x2 y2 z2)]
        [(and (x2 . <= . x-max) (x1 . > . x-max))  (clip-line-x #f x-max x1 y1 z1 x2 y2 z2)]
        [else                                      (values x1 y1 z1 x2 y2 z2)]))

(define (clip-line-y-min y-min x1 y1 z1 x2 y2 z2)
  (cond [(and (y1 . >= . y-min) (y2 . < . y-min))  (clip-line-y #t y-min x1 y1 z1 x2 y2 z2)]
        [(and (y2 . >= . y-min) (y1 . < . y-min))  (clip-line-y #f y-min x1 y1 z1 x2 y2 z2)]
        [else                                      (values x1 y1 z1 x2 y2 z2)]))

(define (clip-line-y-max y-max x1 y1 z1 x2 y2 z2)
  (cond [(and (y1 . <= . y-max) (y2 . > . y-max))  (clip-line-y #t y-max x1 y1 z1 x2 y2 z2)]
        [(and (y2 . <= . y-max) (y1 . > . y-max))  (clip-line-y #f y-max x1 y1 z1 x2 y2 z2)]
        [else                                      (values x1 y1 z1 x2 y2 z2)]))

(define (clip-line-z-min z-min x1 y1 z1 x2 y2 z2)
  (cond [(and (z1 . >= . z-min) (z2 . < . z-min))  (clip-line-z #t z-min x1 y1 z1 x2 y2 z2)]
        [(and (z2 . >= . z-min) (z1 . < . z-min))  (clip-line-z #f z-min x1 y1 z1 x2 y2 z2)]
        [else                                      (values x1 y1 z1 x2 y2 z2)]))

(define (clip-line-z-max z-max x1 y1 z1 x2 y2 z2)
  (cond [(and (z1 . <= . z-max) (z2 . > . z-max))  (clip-line-z #t z-max x1 y1 z1 x2 y2 z2)]
        [(and (z2 . <= . z-max) (z1 . > . z-max))  (clip-line-z #f z-max x1 y1 z1 x2 y2 z2)]
        [else                                      (values x1 y1 z1 x2 y2 z2)]))

(define (clip-line v1 v2 x-min x-max y-min y-max z-min z-max)
  (let/ec return
    (match-define (vector x1 y1 z1) v1)
    (match-define (vector x2 y2 z2) v2)
    ;; early accept: both endpoints in bounds
    (when (and (<= x-min x1 x-max) (<= y-min y1 y-max) (<= z-min z1 z-max)
               (<= x-min x2 x-max) (<= y-min y2 y-max) (<= z-min z2 z-max))
      (return v1 v2))
    ;; early reject: both endpoints on the outside of the same plane
    (when (or (and (x1 . < . x-min) (x2 . < . x-min)) (and (x1 . > . x-max) (x2 . > . x-max))
              (and (y1 . < . y-min) (y2 . < . y-min)) (and (y1 . > . y-max) (y2 . > . y-max))
              (and (z1 . < . z-min) (z2 . < . z-min)) (and (z1 . > . z-max) (z2 . > . z-max)))
      (return #f #f))
    (let*-values ([(x1 y1 z1 x2 y2 z2)  (clip-line-x-min x-min x1 y1 z1 x2 y2 z2)]
                  [(x1 y1 z1 x2 y2 z2)  (clip-line-x-max x-max x1 y1 z1 x2 y2 z2)]
                  [(x1 y1 z1 x2 y2 z2)  (clip-line-y-min y-min x1 y1 z1 x2 y2 z2)]
                  [(x1 y1 z1 x2 y2 z2)  (clip-line-y-max y-max x1 y1 z1 x2 y2 z2)]
                  [(x1 y1 z1 x2 y2 z2)  (clip-line-z-min z-min x1 y1 z1 x2 y2 z2)]
                  [(x1 y1 z1 x2 y2 z2)  (clip-line-z-max z-max x1 y1 z1 x2 y2 z2)])
      (values (vector x1 y1 z1) (vector x2 y2 z2)))))

;; ===================================================================================================
;; Polygon clipping

(define-syntax-rule (make-clip-polygon idx test? clip-line)
  (λ (val vs ls)
    (define-values (new-vs new-ls)
      (for/fold ([vs empty] [ls empty]) ([v1  (in-list (cons (last vs) vs))]
                                         [v2  (in-list vs)]
                                         [l   (in-list ls)])
        (define v1-in-bounds? (test? (unsafe-vector-ref v1 idx) val))
        (define v2-in-bounds? (test? (unsafe-vector-ref v2 idx) val))
        (cond [(and v1-in-bounds? v2-in-bounds?)
               (values (cons v2 vs) (cons l ls))]
              [(and (not v1-in-bounds?) (not v2-in-bounds?))
               (values vs ls)]
              [else
               (match-define (vector x1 y1 z1) v1)
               (match-define (vector x2 y2 z2) v2)
               (let-values ([(x1 y1 z1 x2 y2 z2) (clip-line v1-in-bounds? val x1 y1 z1 x2 y2 z2)])
                 (cond [v2-in-bounds?
                        (values (list* (vector x2 y2 z2) (vector x1 y1 z1) vs)
                                (list* l #t ls))]
                       [else
                        (values (cons (vector x2 y2 z2) vs)
                                (cons l ls))]))])))
    (values (reverse new-vs) (reverse new-ls))))

(define clip-polygon-x-min (make-clip-polygon 0 >= clip-line-x))
(define clip-polygon-x-max (make-clip-polygon 0 <= clip-line-x))
(define clip-polygon-y-min (make-clip-polygon 1 >= clip-line-y))
(define clip-polygon-y-max (make-clip-polygon 1 <= clip-line-y))
(define clip-polygon-z-min (make-clip-polygon 2 >= clip-line-z))
(define clip-polygon-z-max (make-clip-polygon 2 <= clip-line-z))

(define (clip-polygon vs ls x-min x-max y-min y-max z-min z-max)
  (let/ec return
    ;; early reject: no polygon
    (when (empty? vs) (return empty empty))
    (match-define (list (vector xs ys zs) ...) vs)
    ;; early accept: all endpoints in bounds
    (when (and (andmap (λ (x) (<= x-min x x-max)) xs)
               (andmap (λ (y) (<= y-min y y-max)) ys)
               (andmap (λ (z) (<= z-min z z-max)) zs))
      (return vs ls))
    ;; early reject: all endpoints on the outside of the same plane
    (when (or (andmap (λ (x) (x . < . x-min)) xs) (andmap (λ (x) (x . > . x-max)) xs)
              (andmap (λ (y) (y . < . y-min)) ys) (andmap (λ (y) (y . > . y-max)) ys)
              (andmap (λ (z) (z . < . z-min)) zs) (andmap (λ (z) (z . > . z-max)) zs))
      (return empty empty))
    (let*-values ([(vs ls)  (clip-polygon-x-min x-min vs ls)]
                  [(_)      (when (empty? vs) (return empty empty))]
                  [(vs ls)  (clip-polygon-x-max x-max vs ls)]
                  [(_)      (when (empty? vs) (return empty empty))]
                  [(vs ls)  (clip-polygon-y-min y-min vs ls)]
                  [(_)      (when (empty? vs) (return empty empty))]
                  [(vs ls)  (clip-polygon-y-max y-max vs ls)]
                  [(_)      (when (empty? vs) (return empty empty))]
                  [(vs ls)  (clip-polygon-z-min z-min vs ls)]
                  [(_)      (when (empty? vs) (return empty empty))]
                  [(vs ls)  (clip-polygon-z-max z-max vs ls)])
      (values vs ls))))

;; ===================================================================================================
;; Connected lines clipping

(define-syntax-rule (make-clip-lines idx test? clip-line)
  (λ (val vs)
    (define v1 (first vs))
    (define v1? (test? (unsafe-vector-ref v1 idx) val))
    
    (define-values (vss _v1 _v1?)
      (for/fold ([vss (if v1? (list (list v1)) (list empty))]
                 [v1 v1]
                 [v1? v1?]
                 ) ([v2  (in-list (rest vs))])
        (define v2? (test? (unsafe-vector-ref v2 idx) val))
        (cond [(and v1? v2?)       (values (cons (cons v2 (first vss)) (rest vss)) v2 v2?)]
              [(not (or v1? v2?))  (values vss v2 v2?)]
              [else
               (match-define (vector x1 y1 z1) v1)
               (match-define (vector x2 y2 z2) v2)
               (let-values ([(x1 y1 z1 x2 y2 z2) (clip-line v1? val x1 y1 z1 x2 y2 z2)])
                 (cond [v2?
                        (values (cons (list (vector x2 y2 z2) (vector x1 y1 z1)) vss) v2 v2?)]
                       [else
                        (values (cons (cons (vector x2 y2 z2) (first vss)) (rest vss)) v2 v2?)]))])))
    
    (filter (compose not empty?) vss)))

(define clip-lines-x-min (make-clip-lines 0 >= clip-line-x))
(define clip-lines-x-max (make-clip-lines 0 <= clip-line-x))
(define clip-lines-y-min (make-clip-lines 1 >= clip-line-y))
(define clip-lines-y-max (make-clip-lines 1 <= clip-line-y))
(define clip-lines-z-min (make-clip-lines 2 >= clip-line-z))
(define clip-lines-z-max (make-clip-lines 2 <= clip-line-z))

(define (clip-lines vs x-min x-max y-min y-max z-min z-max)
  (let/ec return
    ;; early reject: no polygon
    (when (empty? vs) (return empty))
    (match-define (list (vector xs ys zs) ...) vs)
    ;; early accept: all endpoints in bounds
    (when (and (andmap (λ (x) (<= x-min x x-max)) xs)
               (andmap (λ (y) (<= y-min y y-max)) ys)
               (andmap (λ (z) (<= z-min z z-max)) zs))
      (return (list vs)))
    ;; early reject: all endpoints on the outside of the same plane
    (when (or (andmap (λ (x) (x . < . x-min)) xs) (andmap (λ (x) (x . > . x-max)) xs)
              (andmap (λ (y) (y . < . y-min)) ys) (andmap (λ (y) (y . > . y-max)) ys)
              (andmap (λ (z) (z . < . z-min)) zs) (andmap (λ (z) (z . > . z-max)) zs))
      (return empty))
    (let* ([vss  (clip-lines-x-min x-min vs)]
           [_    (when (empty? vss) (return empty))]
           [vss  (append* (map (λ (vs) (clip-lines-x-max x-max vs)) vss))]
           [_    (when (empty? vss) (return empty))]
           [vss  (append* (map (λ (vs) (clip-lines-y-min y-min vs)) vss))]
           [_    (when (empty? vss) (return empty))]
           [vss  (append* (map (λ (vs) (clip-lines-y-max y-max vs)) vss))]
           [_    (when (empty? vss) (return empty))]
           [vss  (append* (map (λ (vs) (clip-lines-z-min z-min vs)) vss))]
           [_    (when (empty? vss) (return empty))]
           [vss  (append* (map (λ (vs) (clip-lines-z-max z-max vs)) vss))])
      vss)))
