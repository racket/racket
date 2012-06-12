#lang typed/racket/base

(require racket/match racket/math racket/bool
         (only-in racket/unsafe/ops
                  unsafe-flvector-ref
                  unsafe-fx+ unsafe-fx-)
         "flonum.rkt"
         "flomap-struct.rkt")

(provide flomap-flip-horizontal flomap-flip-vertical flomap-transpose
         flomap-cw-rotate flomap-ccw-rotate flomap-rotate
         flomap-2d-mapping flomap-2d-mapping-fun flomap-2d-mapping-inv
         flomap-2d-mapping-bounded-by make-flomap-2d-mapping
         Flomap-Transform
         flomap-transform flomap-transform-bounds
         flomap-id-transform flomap-rotate-transform flomap-scale-transform flomap-whirl-transform
         flomap-transform-compose
         perspective-projection linear-projection orthographic-projection
         equal-area-projection stereographic-projection flomap-projection-transform
         flomap-fisheye-transform
         Projection (struct-out projection-mapping))

;; ===================================================================================================
;; Basic transformations

(: flomap-flip-horizontal (flomap -> flomap))
(define (flomap-flip-horizontal fm)
  (match-define (flomap vs c w h) fm)
  (define w-1 (fx- w 1))
  (inline-build-flomap c w h (λ (k x y _i)
                               (unsafe-flvector-ref vs (coords->index c w k (unsafe-fx- w-1 x) y)))))

(define (flomap-flip-vertical fm)
  (match-define (flomap vs c w h) fm)
  (define h-1 (fx- h 1))
  (inline-build-flomap c w h (λ (k x y _i)
                               (unsafe-flvector-ref vs (coords->index c w k x (unsafe-fx- h-1 y))))))

(define (flomap-transpose fm)
  (match-define (flomap vs c w h) fm)
  (inline-build-flomap c h w (λ (k x y _i)
                               (unsafe-flvector-ref vs (coords->index c w k y x)))))

(define (flomap-cw-rotate fm)
  (match-define (flomap vs c w h) fm)
  (define w-1 (fx- w 1))
  (inline-build-flomap c h w (λ (k x y _i)
                               (unsafe-flvector-ref vs (coords->index c w k (unsafe-fx- w-1 y) x)))))

(define (flomap-ccw-rotate fm)
  (match-define (flomap vs c w h) fm)
  (define h-1 (fx- h 1))
  (inline-build-flomap c h w (λ (k x y _i)
                               (unsafe-flvector-ref vs (coords->index c w k y (unsafe-fx- h-1 x))))))

(: flomap-rotate (flomap Real -> flomap))
(define (flomap-rotate fm θ)
  (flomap-transform fm (flomap-rotate-transform θ)))

;; ===================================================================================================
;; Data types

(struct: flomap-2d-mapping ([fun : (Float Float -> (values Float Float))]
                            [inv : (Float Float -> (values Float Float))]
                            [bounded-by : (U 'id 'corners 'edges 'all)])
  #:transparent)

(: 2d-mapping-real->double-flonum ((Float Float -> (values Real Real))
                                   -> (Float Float -> (values Float Float))))
(define ((2d-mapping-real->double-flonum f) x y)
  (let-values ([(x y)  (f x y)])
    (values (real->double-flonum x) (real->double-flonum y))))

(: make-flomap-2d-mapping (case-> ((Float Float -> (values Real Real))
                                   (Float Float -> (values Real Real))
                                   -> flomap-2d-mapping)
                                  ((Float Float -> (values Real Real))
                                   (Float Float -> (values Real Real))
                                   (U 'id 'corners 'edges 'all) -> flomap-2d-mapping)))
(define make-flomap-2d-mapping
  (case-lambda
    [(fun inv)  (make-flomap-2d-mapping fun inv 'edges)]
    [(fun inv bounded-by)  (flomap-2d-mapping (2d-mapping-real->double-flonum fun)
                                              (2d-mapping-real->double-flonum inv)
                                              bounded-by)]))

(define-type Flomap-Transform (Integer Integer -> flomap-2d-mapping))

;; ===================================================================================================
;; Transformations

(: flomap-transform-bounds (Flomap-Transform Integer Integer
                                               -> (values Integer Integer Integer Integer)))
(define (flomap-transform-bounds t w h)
  (match-define (flomap-2d-mapping fun _ bounded-by) (t w h))
  
  (: maybe-expand (Integer Integer Float Float Float Float -> (values Float Float Float Float)))
  (define (maybe-expand x y x-min y-min x-max y-max)
    ;; transform the coordinate, possibly update the mins and maxes
    (define-values (new-x new-y) (fun (->fl x) (->fl y)))
    (values (if (new-x . < . x-min) new-x x-min)
            (if (new-y . < . y-min) new-y y-min)
            (if (new-x . > . x-max) new-x x-max)
            (if (new-y . > . y-max) new-y y-max)))
  
  (define-values (x-min y-min x-max y-max)
    (case bounded-by
      [(id)  (values 0 0 w h)]
      [(corners)
       (for*/fold: ([x-min : Float  +inf.0]
                    [y-min : Float  +inf.0]
                    [x-max : Float  -inf.0]
                    [y-max : Float  -inf.0]
                    ) ([y : Integer  (list 0 h)]
                       [x : Integer  (list 0 w)])
         (maybe-expand x y x-min y-min x-max y-max))]
      [(edges)
       (define-values (x-min1 y-min1 x-max1 y-max1)
         (for*/fold: ([x-min : Float  +inf.0]
                      [y-min : Float  +inf.0]
                      [x-max : Float  -inf.0]
                      [y-max : Float  -inf.0]
                      ) ([y : Integer  (in-range (fx+ h 1))]
                         [x : Integer  (list 0 w)])
           (maybe-expand x y x-min y-min x-max y-max)))
       (define-values (x-min2 y-min2 x-max2 y-max2)
         (for*/fold: ([x-min : Float  +inf.0]
                      [y-min : Float  +inf.0]
                      [x-max : Float  -inf.0]
                      [y-max : Float  -inf.0]
                      ) ([y : Integer  (list 0 h)]
                         [x : Integer  (in-range (fx+ w 1))])
           (maybe-expand x y x-min y-min x-max y-max)))
       (values (min x-min1 x-min2) (min y-min1 y-min2)
               (max x-max1 x-max2) (max y-max1 y-max2))]
      [(all)
       ;; these will be mutated within the loop (instead of accumulating them, which is annoying)
       (define-values (x-min y-min x-max y-max) (values +inf.0 +inf.0 -inf.0 -inf.0))
       ;; for each point...
       (let: y-loop : Void ([y : Nonnegative-Fixnum  0])
         (when (y . fx<= . h)
           (let: x-loop : Void ([x : Nonnegative-Fixnum  0])
             (cond [(x . fx<= . w)
                    ;; transform the coordinate, possibly set the mins and maxes
                    (define-values (new-x new-y) (fun (->fl x) (->fl y)))
                    (when (new-x . < . x-min) (set! x-min new-x))
                    (when (new-x . > . x-max) (set! x-max new-x))
                    (when (new-y . < . y-min) (set! y-min new-y))
                    (when (new-y . > . y-max) (set! y-max new-y))
                    (x-loop (unsafe-fx+ x 1))]
                   [else
                    (y-loop (unsafe-fx+ y 1))]))))
       (values x-min y-min x-max y-max)]))
  ;; return integer bounds
  (cond [(and (rational? x-min) (rational? y-min) (rational? x-max) (rational? y-max))
         (values (round (inexact->exact x-min))
                 (round (inexact->exact y-min))
                 (round (inexact->exact x-max))
                 (round (inexact->exact y-max)))]
        [else  (values 0 0 0 0)]))

(: flomap-transform-compose (Flomap-Transform Flomap-Transform -> Flomap-Transform))
(define ((flomap-transform-compose t2 t1) w0 h0)
  (match-define (flomap-2d-mapping fun1 inv1 bounded-by1) (t1 w0 h0))
  (define-values (x-start y-start x-end y-end) (flomap-transform-bounds t1 w0 h0))
  (define w1 (- x-end x-start))
  (define h1 (- y-end y-start))
  (match-define (flomap-2d-mapping fun2 inv2 bounded-by2) (t2 w1 h1))
  (flomap-2d-mapping
   (λ (x y)
     (let-values ([(x y)  (fun1 x y)])
       (fun2 (- x x-start) (- y y-start))))
   (λ (x y)
     (let-values ([(x y)  (inv2 x y)])
       (inv1 (+ x x-start) (+ y y-start))))
   (cond [(or (symbol=? bounded-by1 'all) (symbol=? bounded-by2 'all))  'all]
         [(or (symbol=? bounded-by1 'edges) (symbol=? bounded-by2 'edges))  'edges]
         [(or (symbol=? bounded-by1 'corners) (symbol=? bounded-by2 'corners))  'corners]
         [(or (symbol=? bounded-by1 'id) (symbol=? bounded-by2 'id))  'id])))

(: flomap-transform (case-> (flomap Flomap-Transform -> flomap)
                            (flomap Flomap-Transform Integer Integer Integer Integer -> flomap)))
(define flomap-transform
  (case-lambda
    [(fm t)  (match-define (flomap _vs _c w h) fm)
             (define-values (x-start y-start x-end y-end)
               (flomap-transform-bounds t w h))
             (flomap-transform fm t x-start y-start x-end y-end)]
    [(fm t x-start y-start x-end y-end)
     (match-define (flomap _ c w h) fm)
     (match-define (flomap-2d-mapping _ inv _) (t w h))  ; only need the inverse mapping
     (define new-w (- x-end x-start))
     (define new-h (- y-end y-start))
     (define x-offset (+ 0.5 x-start))
     (define y-offset (+ 0.5 y-start))
     (inline-build-flomap*
      c new-w new-h
      (λ (x y _i)
        (define-values (old-x old-y) (inv (+ (fx->fl x) x-offset)
                                          (+ (fx->fl y) y-offset)))
        (flomap-bilinear-ref* fm old-x old-y)))]))

(: flomap-id-transform Flomap-Transform)
(define (flomap-id-transform w h)
  (flomap-2d-mapping (λ (x y) (values x y)) (λ (x y) (values x y)) 'id))

(: flomap-scale-transform (case-> (Real -> Flomap-Transform)
                                  (Real Real -> Flomap-Transform)))
(define flomap-scale-transform
  (case-lambda
    [(x-scale)  (flomap-scale-transform x-scale x-scale)]
    [(x-scale y-scale)
     (let ([x-scale  (real->double-flonum x-scale)]
           [y-scale  (real->double-flonum y-scale)])
       (λ (w h)
         (flomap-2d-mapping (λ (x y) (values (* x x-scale) (* y y-scale)))
                            (λ (x y) (values (/ x x-scale) (/ y y-scale)))
                            'corners)))]))

(: flomap-rotate-transform (Real -> Flomap-Transform))
(define ((flomap-rotate-transform θ) w h)
  (let ([θ  (- (real->double-flonum θ))])
    (define cos-θ (cos θ))
    (define sin-θ (sin θ))
    (define x-mid (* 0.5 (->fl w)))
    (define y-mid (* 0.5 (->fl h)))
    (flomap-2d-mapping
     (λ: ([x : Float] [y : Float])
       (let ([x  (- x x-mid)]
             [y  (- y y-mid)])
         (values (+ x-mid (- (* x cos-θ) (* y sin-θ)))
                 (+ y-mid (+ (* x sin-θ) (* y cos-θ))))))
     (λ: ([x : Float] [y : Float])
       (let ([x  (- x x-mid)]
             [y  (- y y-mid)])
         (values (+ x-mid (+ (* x cos-θ) (* y sin-θ)))
                 (+ y-mid (- (* y cos-θ) (* x sin-θ))))))
     'corners)))

(: whirl-function (Real Integer Integer -> (Float Float -> (values Float Float))))
(define (whirl-function θ w h)
  (let ([θ  (real->double-flonum θ)])
    (define x-mid (* 0.5 (->fl w)))
    (define y-mid (* 0.5 (->fl h)))
    (define-values (x-scale y-scale)
      (cond [(x-mid . < . y-mid)  (values (/ y-mid x-mid) 1.0)]
            [(x-mid . > . y-mid)  (values 1.0 (/ x-mid y-mid))]
            [else  (values 1.0 1.0)]))
    (define fm-radius (* 0.5 (->fl (max w h))))
    (define fm-radius^2 (sqr fm-radius))
    (define x-max (->fl w))
    (define y-max (->fl h))
    (λ: ([x : Float] [y : Float])
      (define dx (* (- x x-mid) x-scale))
      (define dy (* (- y y-mid) y-scale))
      (define r^2 (+ (sqr dx) (sqr dy)))
      (cond [(r^2 . < . fm-radius^2)
             (define r (flsqrt (/ r^2 fm-radius^2)))
             (define ang (* θ (sqr (- 1.0 r))))
             (define cos-a (cos ang))
             (define sin-a (sin ang))
             (define old-x (+ (/ (- (* dx cos-a) (* dy sin-a)) x-scale) x-mid))
             (define old-y (+ (/ (+ (* dx sin-a) (* dy cos-a)) y-scale) y-mid))
             (values (max 0.0 (min x-max old-x))
                     (max 0.0 (min y-max old-y)))]
            [else
             (values x y)]))))

(: flomap-whirl-transform (Real -> Flomap-Transform))
(define ((flomap-whirl-transform θ) w h)
  (flomap-2d-mapping (whirl-function (- θ) w h) (whirl-function θ w h) 'id))

;; ===================================================================================================
;; Projection transforms

(struct: projection-mapping ([fun : (Float -> Float)]
                             [inv : (Float -> Float)]))

(define-type Projection (Float -> projection-mapping))

(: perspective-projection (Real -> Projection))
(define ((perspective-projection α) d)
  (define f (/ d 2.0 (tan (* 0.5 (real->double-flonum α)))))
  (projection-mapping (λ (ρ) (* (tan ρ) f))
                      (λ (r) (atan (/ r f)))))

(: linear-projection (Real -> Projection))
(define ((linear-projection α) d)
  (define f (/ d (real->double-flonum α)))
  (projection-mapping (λ (ρ) (* ρ f))
                      (λ (r) (/ r f))))

(: orthographic-projection (Real -> Projection))
(define ((orthographic-projection α) d)
  (define f (/ d 2.0 (sin (* 0.5 (real->double-flonum α)))))
  (projection-mapping (λ (ρ) (* (sin ρ) f))
                      (λ (r) (asin (/ r f)))))

(: equal-area-projection (Real -> Projection))
(define ((equal-area-projection α) d)
  (define f (/ d 4.0 (sin (* 0.25 (real->double-flonum α)))))
  (projection-mapping (λ (ρ) (* 2.0 (sin (* 0.5 ρ)) f))
                      (λ (r) (* 2.0 (asin (/ r 2.0 f))))))

(: stereographic-projection (Real -> Projection))
(define ((stereographic-projection α) d)
  (define f (/ d 4.0 (tan (* 0.25 (real->double-flonum α)))))
  (projection-mapping (λ (ρ) (* 2.0 (tan (* 0.5 ρ)) f))
                      (λ (r) (* 2.0 (atan (/ r 2.0 f))))))

(: reproject (Projection Projection Boolean Integer Integer -> (Float Float -> (values Float Float))))
(define (reproject to-proj from-proj crop? w h)
  (define x-max (->fl w))
  (define y-max (->fl h))
  (define x-mid (* 0.5 x-max))
  (define y-mid (* 0.5 y-max))
  (define d (* 2.0 (flsqrt (+ (sqr x-mid) (sqr y-mid)))))
  (match-define (projection-mapping _ inv) (from-proj d))
  (match-define (projection-mapping fun _) (to-proj d))
  (λ: ([x : Float] [y : Float])
    (define dx (- x x-mid))
    (define dy (- y y-mid))
    (define θ (atan dy dx))
    (define r (flsqrt (+ (sqr dx) (sqr dy))))
    (define new-r (fun (inv r)))
    (define new-x (+ x-mid (* (cos θ) new-r)))
    (define new-y (+ y-mid (* (sin θ) new-r)))
    (cond [crop?  (values (if (or (new-x . < . 0.0) (new-x . > . x-max)) +nan.0 new-x)
                          (if (or (new-y . < . 0.0) (new-y . > . y-max)) +nan.0 new-y))]
          [else   (values new-x new-y)])))

(: flomap-projection-transform (case-> (Projection Projection -> Flomap-Transform)
                                       (Projection Projection Boolean -> Flomap-Transform)))
(define flomap-projection-transform
  (case-lambda
    [(to-proj from-proj)  (flomap-projection-transform to-proj from-proj #t)]
    [(to-proj from-proj crop?)
     (λ (w h) (flomap-2d-mapping (reproject to-proj from-proj crop? w h)
                                 (reproject from-proj to-proj crop? w h)
                                 'edges))]))

(: flomap-fisheye-transform (Real -> Flomap-Transform))
(define (flomap-fisheye-transform α)
  (flomap-projection-transform (equal-area-projection α)
                               (perspective-projection α)
                               #f))
