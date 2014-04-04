#lang racket/base

(require (for-syntax racket/base)
         racket/match
         racket/list)

(provide point-in-bounds?
         point-inside-plane?
         clip-line/bounds
         clip-line/plane
         clip-lines/bounds
         clip-lines/plane
         clip-polygon/bounds
         clip-polygon/plane)

;; ===================================================================================================
;; Basic plane intersection and distance tests

;; Need to use this instead of `*' to make axial plane tests as fast as hand-written because the
;; optimizer doesn't always figure out (* 0 e) = 0, etc.
(define-syntax (times stx)
  (syntax-case stx ()
    [(_ 0 e)  (syntax/loc stx 0)]
    [(_ 1 e)  (syntax/loc stx e)]
    [(_ 0.0 e)  (syntax/loc stx 0.0)]
    [(_ 1.0 e)  (syntax/loc stx (real->double-flonum e))]
    [(_ . es)  (syntax/loc stx (* . es))]))

(define-syntax-rule (plane-line-intersect a b c d x1-stx y1-stx z1-stx x2-stx y2-stx z2-stx)
  (let ([x1 x1-stx] [y1 y1-stx] [z1 z1-stx] [x2 x2-stx] [y2 y2-stx] [z2 z2-stx])
    (let ([dot1  (+ (times a x1) (times b y1) (times c z1))]
          [dot2  (+ (times a x2) (times b y2) (times c z2))])
      (let ([denom  (- dot1 dot2)])
        (if (zero? denom)
            (values x2 y2 z2)
            (let ([t  (/ (+ dot1 d) denom)])
              (values (+ x1 (* t (- x2 x1)))
                      (+ y1 (* t (- y2 y1)))
                      (+ z1 (* t (- z2 z1))))))))))

(define-syntax-rule (plane-point-dist a b c d x y z)
  (+ (times a x) (times b y) (times c z) d))

;; ===================================================================================================
;; Points

(define (point-in-bounds? v x-min x-max y-min y-max z-min z-max)
  (match-define (vector x y z) v)
  (and (<= x-min x x-max) (<= y-min y y-max) (<= z-min z z-max)))

(define (point-inside-plane? v plane)
  (match-define (vector x y z) v)
  (match-define (vector a b c d) plane)
  (>= (plane-point-dist a b c d x y z) 0))

;; ===================================================================================================
;; Line clipping

(define (clip-line/plane v1 v2 plane)
  (match-define (vector x1 y1 z1) v1)
  (match-define (vector x2 y2 z2) v2)
  (match-define (vector a b c d) plane)
  (define v1? (>= (plane-point-dist a b c d x1 y1 z1) 0))
  (define v2? (>= (plane-point-dist a b c d x2 y2 z2) 0))
  (cond [(and v1? v2?)       (values v1 v2)]
        [(not (or v1? v2?))  (values #f #f)]
        [else
         (define-values (x y z) (plane-line-intersect a b c d x1 y1 z1 x2 y2 z2))
         (if v1? (values v1 (vector x y z)) (values (vector x y z) v2))]))

;; ---------------------------------------------------------------------------------------------------
;; Clipping inside axial bounding box

(define-syntax-rule (make-clip-line/axis a b c gte?)
  (λ (val x1 y1 z1 x2 y2 z2)
    (define v1? (gte? (plane-point-dist a b c (- val) x1 y1 z1) 0))
    (define v2? (gte? (plane-point-dist a b c (- val) x2 y2 z2) 0))
    (cond [(or (and v1? v2?) (not (or v1? v2?))) (values x1 y1 z1 x2 y2 z2)]
          [else
           (define-values (x y z) (plane-line-intersect a b c (- val) x1 y1 z1 x2 y2 z2))
           (if v1? (values x1 y1 z1 x y z) (values x y z x2 y2 z2))])))

(define clip-line-x-min (make-clip-line/axis 1 0 0 >=))
(define clip-line-x-max (make-clip-line/axis 1 0 0 <=))
(define clip-line-y-min (make-clip-line/axis 0 1 0 >=))
(define clip-line-y-max (make-clip-line/axis 0 1 0 <=))
(define clip-line-z-min (make-clip-line/axis 0 0 1 >=))
(define clip-line-z-max (make-clip-line/axis 0 0 1 <=))

(define (clip-line/bounds v1 v2 x-min x-max y-min y-max z-min z-max)
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
;; Connected lines clipping

(define-syntax-rule (make-clip-lines/plane a b c d gte?)
  (λ (vs)
    (cond
      [(empty? vs)  empty]
      [else
       (define v1 (first vs))
       (match-define (vector x1 y1 z1) v1)
       (define v1? (gte? (plane-point-dist a b c d x1 y1 z1) 0))
       
       (define init-vss (if v1? (list (list v1)) (list empty)))
       
       (define-values (vss _x1 _y1 _z1 _v1?)
         (for/fold ([vss init-vss] [x1 x1] [y1 y1] [z1 z1] [v1? v1?]) ([v2  (in-list (rest vs))])
           (define x2 (vector-ref v2 0))
           (define y2 (vector-ref v2 1))
           (define z2 (vector-ref v2 2))
           (define v2? (gte? (plane-point-dist a b c d x2 y2 z2) 0))
           (cond [(and v1? v2?)       (values (cons (cons v2 (first vss)) (rest vss)) x2 y2 z2 v2?)]
                 [(not (or v1? v2?))  (values vss x2 y2 z2 v2?)]
                 [else
                  (define-values (x y z) (plane-line-intersect a b c d x1 y1 z1 x2 y2 z2))
                  (if v1?
                      (values (cons (cons (vector x y z) (first vss)) (rest vss)) x2 y2 z2 v2?)
                      (values (cons (list v2 (vector x y z)) vss) x2 y2 z2 v2?))])))
       
       (filter (compose not empty?) vss)])))

(define (clip-lines/plane vs plane)
  (match-define (vector a b c d) plane)
  ((make-clip-lines/plane a b c d >=) vs))

;; ---------------------------------------------------------------------------------------------------
;; Clipping inside axial bounding box

;; Early accept: all endpoints in bounds (or empty vs)
(define (early-accept? vs x-min x-max y-min y-max z-min z-max)
  (andmap (λ (v) (and (<= x-min (vector-ref v 0) x-max)
                      (<= y-min (vector-ref v 1) y-max)
                      (<= z-min (vector-ref v 2) z-max)))
          vs))

;; Early reject: all endpoints on the outside of the same plane
(define (early-reject? vs x-min x-max y-min y-max z-min z-max)
  (or (andmap (λ (v) ((vector-ref v 0) . < . x-min)) vs)
      (andmap (λ (v) ((vector-ref v 0) . > . x-max)) vs)
      (andmap (λ (v) ((vector-ref v 1) . < . y-min)) vs)
      (andmap (λ (v) ((vector-ref v 1) . > . y-max)) vs)
      (andmap (λ (v) ((vector-ref v 2) . < . z-min)) vs)
      (andmap (λ (v) ((vector-ref v 2) . > . z-max)) vs)))

(define-syntax-rule (make-clip-lines/axis a b c gte?)
  (λ (val vs)
    ((make-clip-lines/plane a b c (- val) gte?) vs)))

(define clip-lines-x-min (make-clip-lines/axis 1 0 0 >=))
(define clip-lines-x-max (make-clip-lines/axis 1 0 0 <=))
(define clip-lines-y-min (make-clip-lines/axis 0 1 0 >=))
(define clip-lines-y-max (make-clip-lines/axis 0 1 0 <=))
(define clip-lines-z-min (make-clip-lines/axis 0 0 1 >=))
(define clip-lines-z-max (make-clip-lines/axis 0 0 1 <=))

(define (clip-lines/bounds vs x-min x-max y-min y-max z-min z-max)
  (let/ec return
    (when (early-accept? vs x-min x-max y-min y-max z-min z-max) (return (list vs)))
    (when (early-reject? vs x-min x-max y-min y-max z-min z-max) (return empty))
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

;; ===================================================================================================
;; Polygon clipping

(define-syntax-rule (make-clip-polygon/plane a b c d gte?)
  (λ (vs ls)
    (define v1 (last vs))
    (define x1 (vector-ref v1 0))
    (define y1 (vector-ref v1 1))
    (define z1 (vector-ref v1 2))
    (define v1? (gte? (plane-point-dist a b c d x1 y1 z1) 0))
    
    (define-values (new-vs new-ls _x1 _y1 _z1 _v1?)
      (for/fold ([vs empty] [ls empty] [x1 x1] [y1 y1] [z1 z1] [v1? v1?]) ([v2  (in-list vs)]
                                                                           [l   (in-list ls)])
        (define x2 (vector-ref v2 0))
        (define y2 (vector-ref v2 1))
        (define z2 (vector-ref v2 2))
        (define v2? (gte? (plane-point-dist a b c d x2 y2 z2) 0))
        (cond [(and v1? v2?)       (values (cons v2 vs) (cons l ls) x2 y2 z2 v2?)]
              [(not (or v1? v2?))  (values vs ls x2 y2 z2 v2?)]
              [else
               (define-values (x y z) (plane-line-intersect a b c d x1 y1 z1 x2 y2 z2))
               (if v1?
                   (values (cons (vector x y z) vs) (cons l ls) x2 y2 z2 v2?)
                   (values (list* v2 (vector x y z) vs) (list* l #t ls) x2 y2 z2 v2?))])))
    
    (values (reverse new-vs) (reverse new-ls))))

(define (clip-polygon/plane vs ls plane)
  (match-define (vector a b c d) plane)
  ((make-clip-polygon/plane a b c d >=) vs ls))

;; ---------------------------------------------------------------------------------------------------
;; Clipping inside axial bounding box

(define-syntax-rule (make-clip-polygon/axis a b c gte?)
  (λ (vs ls val)
    ((make-clip-polygon/plane a b c (- val) gte?) vs ls)))

(define clip-polygon-x-min (make-clip-polygon/axis 1 0 0 >=))
(define clip-polygon-x-max (make-clip-polygon/axis 1 0 0 <=))
(define clip-polygon-y-min (make-clip-polygon/axis 0 1 0 >=))
(define clip-polygon-y-max (make-clip-polygon/axis 0 1 0 <=))
(define clip-polygon-z-min (make-clip-polygon/axis 0 0 1 >=))
(define clip-polygon-z-max (make-clip-polygon/axis 0 0 1 <=))

(define (clip-polygon/bounds vs ls x-min x-max y-min y-max z-min z-max)
  (let/ec return
    (when (early-accept? vs x-min x-max y-min y-max z-min z-max) (return vs ls))
    (when (early-reject? vs x-min x-max y-min y-max z-min z-max) (return empty empty))
    (let*-values ([(vs ls)  (clip-polygon-x-min vs ls x-min)]
                  [(_)      (when (empty? vs) (return empty empty))]
                  [(vs ls)  (clip-polygon-x-max vs ls x-max)]
                  [(_)      (when (empty? vs) (return empty empty))]
                  [(vs ls)  (clip-polygon-y-min vs ls y-min)]
                  [(_)      (when (empty? vs) (return empty empty))]
                  [(vs ls)  (clip-polygon-y-max vs ls y-max)]
                  [(_)      (when (empty? vs) (return empty empty))]
                  [(vs ls)  (clip-polygon-z-min vs ls z-min)]
                  [(_)      (when (empty? vs) (return empty empty))]
                  [(vs ls)  (clip-polygon-z-max vs ls z-max)])
      (values vs ls))))
