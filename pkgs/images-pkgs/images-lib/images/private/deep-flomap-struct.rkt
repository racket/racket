#lang typed/racket/base

(require racket/match racket/math
         "flonum.rkt"
         "flomap.rkt")

(provide deep-flomap deep-flomap? deep-flomap-argb deep-flomap-z
         deep-flomap-width deep-flomap-height deep-flomap-z-min deep-flomap-z-max
         deep-flomap-size deep-flomap-alpha deep-flomap-rgb
         flomap->deep-flomap
         ;; Sizing
         deep-flomap-inset deep-flomap-trim deep-flomap-scale deep-flomap-resize
         ;; Z-adjusting
         deep-flomap-scale-z deep-flomap-smooth-z deep-flomap-raise deep-flomap-tilt
         deep-flomap-emboss
         deep-flomap-bulge deep-flomap-bulge-round deep-flomap-bulge-round-rect
         deep-flomap-bulge-spheroid deep-flomap-bulge-horizontal deep-flomap-bulge-vertical
         deep-flomap-bulge-ripple
         ;; Compositing
         deep-flomap-pin deep-flomap-pin*
         deep-flomap-lt-superimpose deep-flomap-lc-superimpose deep-flomap-lb-superimpose
         deep-flomap-ct-superimpose deep-flomap-cc-superimpose deep-flomap-cb-superimpose
         deep-flomap-rt-superimpose deep-flomap-rc-superimpose deep-flomap-rb-superimpose
         deep-flomap-vl-append deep-flomap-vc-append deep-flomap-vr-append
         deep-flomap-ht-append deep-flomap-hc-append deep-flomap-hb-append)

(struct: deep-flomap ([argb : flomap] [z : flomap])
  #:transparent
  #:guard
  (λ (argb-fm z-fm name)
    (match-define (flomap _ 4 w h) argb-fm)
    (match-define (flomap _ 1 zw zh) z-fm)
    (unless (and (= w zw) (= h zh))
      (error 'deep-flomap "expected same-size flomaps; given sizes ~e×~e and ~e×~e" w h zw zh))
    (values argb-fm z-fm)))

(: flomap->deep-flomap (flomap -> deep-flomap))
(define (flomap->deep-flomap argb-fm)
  (match-define (flomap _ 4 w h) argb-fm)
  (deep-flomap argb-fm (make-flomap 1 w h)))

(: deep-flomap-width (deep-flomap -> Nonnegative-Fixnum))
(define (deep-flomap-width dfm)
  (define w (flomap-width (deep-flomap-argb dfm)))
  (with-asserts ([w  nonnegative-fixnum?])
    w))

(: deep-flomap-height (deep-flomap -> Nonnegative-Fixnum))
(define (deep-flomap-height dfm)
  (define h (flomap-height (deep-flomap-argb dfm)))
  (with-asserts ([h  nonnegative-fixnum?])
    h))

(: deep-flomap-z-min (deep-flomap -> Flonum))
(define (deep-flomap-z-min dfm)
  (flomap-min-value (deep-flomap-z dfm)))

(: deep-flomap-z-max (deep-flomap -> Flonum))
(define (deep-flomap-z-max dfm)
  (flomap-max-value (deep-flomap-z dfm)))

(: deep-flomap-size (deep-flomap -> (values Nonnegative-Fixnum Nonnegative-Fixnum)))
(define (deep-flomap-size dfm)
  (values (deep-flomap-width dfm) (deep-flomap-height dfm)))

(: deep-flomap-alpha (deep-flomap -> flomap))
(define (deep-flomap-alpha dfm)
  (flomap-ref-component (deep-flomap-argb dfm) 0))

(: deep-flomap-rgb (deep-flomap -> flomap))
(define (deep-flomap-rgb dfm)
  (flomap-drop-components (deep-flomap-argb dfm) 1))

;; ===================================================================================================
;; Z adjusters

(: deep-flomap-scale-z (deep-flomap (U Real flomap) -> deep-flomap))
(define (deep-flomap-scale-z dfm z)
  (match-define (deep-flomap argb-fm z-fm) dfm)
  (deep-flomap argb-fm (fm* z-fm z)))

(: deep-flomap-smooth-z (deep-flomap Real -> deep-flomap))
(define (deep-flomap-smooth-z dfm σ)
  (let ([σ  (real->double-flonum σ)])
    (match-define (deep-flomap argb-fm z-fm) dfm)
    (define new-z-fm (flomap-blur z-fm σ))
    (deep-flomap argb-fm new-z-fm)))

;; deep-flomap-raise and everything derived from it observe an invariant:
;; when z is added, added z must be 0.0 everywhere alpha is 0.0

(: deep-flomap-raise (deep-flomap (U Real flomap) -> deep-flomap))
(define (deep-flomap-raise dfm z)
  (match-define (deep-flomap argb-fm z-fm) dfm)
  (define alpha-fm (deep-flomap-alpha dfm))
  (deep-flomap argb-fm (fm+ z-fm (fm* alpha-fm z))))

(: deep-flomap-emboss (deep-flomap Real (U Real flomap) -> deep-flomap))
(define (deep-flomap-emboss dfm xy-amt z-amt)
  (let ([σ      (/ xy-amt 3.0)])
    (define z-fm (flomap-normalize (deep-flomap-alpha dfm)))
    (define new-z-fm (fm* (flomap-blur z-fm σ) z-amt))
    (deep-flomap-raise dfm new-z-fm)))

(define-syntax-rule (inline-deep-flomap-bulge dfm f)
  (let ()
    (define-values (w h) (deep-flomap-size dfm))
    (define half-x-size (- (* 0.5 (fx->fl w)) 0.5))
    (define half-y-size (- (* 0.5 (fx->fl h)) 0.5))
    (define z-fm
      (inline-build-flomap
       1 w h
       (λ (_ x y _i)
         (f (- (/ (fx->fl x) half-x-size) 1.0)
            (- (/ (fx->fl y) half-y-size) 1.0)))))
    (deep-flomap-raise dfm z-fm)))

(: deep-flomap-bulge (deep-flomap (Flonum Flonum -> Real) -> deep-flomap))
(define (deep-flomap-bulge dfm f)
  (inline-deep-flomap-bulge dfm (λ (cx cy) (real->double-flonum (f cx cy)))))

(: deep-flomap-tilt (deep-flomap Real Real Real Real -> deep-flomap))
(define (deep-flomap-tilt dfm left-z-amt top-z-amt right-z-amt bottom-z-amt)
  (let ([l  (real->double-flonum left-z-amt)]
        [t  (real->double-flonum top-z-amt)]
        [r  (real->double-flonum right-z-amt)]
        [b  (real->double-flonum bottom-z-amt)])
    (define: (f [x : Flonum] [y : Flonum]) : Flonum
      (define α (/ (+ x 1.0) 2.0))
      (define β (/ (+ y 1.0) 2.0))
      (+ (* (- 1.0 α) l) (* α r)
         (* (- 1.0 β) t) (* β b)))
    (inline-deep-flomap-bulge dfm f)))

(: deep-flomap-bulge-round (deep-flomap Real -> deep-flomap))
(define (deep-flomap-bulge-round dfm z-amt)
  (let ([z-amt  (real->double-flonum z-amt)])
    (define: (f [x : Flonum] [y : Flonum]) : Flonum
      (define d^2 (+ (* x x) (* y y)))
      (* z-amt (flsqrt (/ (- 2.0 d^2) 2.0))))
    (inline-deep-flomap-bulge dfm f)))

(: deep-flomap-bulge-round-rect (deep-flomap Real -> deep-flomap))
(define (deep-flomap-bulge-round-rect dfm z-amt)
  (let ([z-amt  (real->double-flonum z-amt)])
    (define: (f [x : Flonum] [y : Flonum]) : Flonum
      (* z-amt (flsqrt (* (- 1.0 (* x x))
                          (- 1.0 (* y y))))))
    (inline-deep-flomap-bulge dfm f)))

(: deep-flomap-bulge-spheroid (deep-flomap Real -> deep-flomap))
(define (deep-flomap-bulge-spheroid dfm z-amt)
  (let ([z-amt  (real->double-flonum z-amt)])
    (define: (f [x : Flonum] [y : Flonum]) : Flonum
      (define d^2 (+ (* x x) (* y y)))
      (if (d^2 . < . 1.0) (* z-amt (flsqrt (- 1.0 d^2))) 0.0))
    (inline-deep-flomap-bulge dfm f)))

(: deep-flomap-bulge-horizontal (deep-flomap Real -> deep-flomap))
(define (deep-flomap-bulge-horizontal dfm z-amt)
  (let ([z-amt  (real->double-flonum z-amt)])
    (define: (f [x : Flonum] [y : Flonum]) : Flonum
      (* z-amt (flsqrt (- 1.0 (* x x)))))
    (inline-deep-flomap-bulge dfm f)))

(: deep-flomap-bulge-vertical (deep-flomap Real -> deep-flomap))
(define (deep-flomap-bulge-vertical dfm z-amt)
  (let ([z-amt  (real->double-flonum z-amt)])
    (define: (f [x : Flonum] [y : Flonum]) : Flonum
      (* z-amt (flsqrt (- 1.0 (* y y)))))
    (inline-deep-flomap-bulge dfm f)))

(: deep-flomap-bulge-ripple (deep-flomap Real Real -> deep-flomap))
(define (deep-flomap-bulge-ripple dfm freq z-amt)
  (let ([freq   (real->double-flonum freq)]
        [z-amt  (real->double-flonum z-amt)])
    (define: (f [x : Flonum] [y : Flonum]) : Flonum
      (define d^2 (+ (* x x) (* y y)))
      (define d (* freq pi (flsqrt d^2)))
      (* z-amt 0.5 (- 1.0 (cos d))))
    (inline-deep-flomap-bulge dfm f)))

;; ===================================================================================================
;; Sizing

(: deep-flomap-inset (case-> (deep-flomap Integer -> deep-flomap)
                             (deep-flomap Integer Integer -> deep-flomap)
                             (deep-flomap Integer Integer Integer Integer -> deep-flomap)))
(define deep-flomap-inset
  (case-lambda
    [(dfm amt)          (deep-flomap-inset dfm amt amt amt amt)]
    [(dfm h-amt v-amt)  (deep-flomap-inset dfm h-amt v-amt h-amt v-amt)]
    [(dfm l-amt t-amt r-amt b-amt)
     (match-define (deep-flomap argb-fm z-fm) dfm)
     (deep-flomap (flomap-inset argb-fm l-amt t-amt r-amt b-amt)
                  (flomap-inset z-fm l-amt t-amt r-amt b-amt))]))

(: deep-flomap-trim (deep-flomap -> deep-flomap))
(define (deep-flomap-trim dfm)
  (define-values (w h) (deep-flomap-size dfm))
  (define-values (x-min y-min x-max y-max)
    (flomap-nonzero-rect (deep-flomap-alpha dfm)))
  (deep-flomap-inset dfm (- x-min) (- y-min) (- x-max w) (- y-max h)))

(: deep-flomap-scale (case-> (deep-flomap Real -> deep-flomap)
                             (deep-flomap Real Real Real -> deep-flomap)))
(define deep-flomap-scale
  (case-lambda
    [(dfm scale)
     (match-define (deep-flomap argb-fm z-fm) (deep-flomap-scale-z dfm scale))
     (deep-flomap (flomap-scale argb-fm scale)
                  (flomap-scale z-fm scale))]
    [(dfm x-scale y-scale z-scale)
     (match-define (deep-flomap argb-fm z-fm) (deep-flomap-scale-z dfm z-scale))
     (deep-flomap (flomap-scale argb-fm x-scale y-scale)
                  (flomap-scale z-fm x-scale y-scale))]))

(: deep-flomap-resize (deep-flomap (Option Integer) (Option Integer) (Option Real) (Option Real)
                                   -> deep-flomap))
(define (deep-flomap-resize dfm width height z-min z-max)
  (match-define (deep-flomap argb-fm z-fm) dfm)
  (define new-z-fm
    (cond [(or z-min z-max)
           (let ([z-min  (if z-min z-min (flomap-min-value z-fm))]
                 [z-max  (if z-max z-max (flomap-max-value z-fm))])
             (fm+ (fm* (flomap-normalize z-fm) (- z-max z-min)) z-min))]
          [else  z-fm]))
  (deep-flomap (flomap-resize argb-fm width height)
               (flomap-resize new-z-fm width height)))

;; ===================================================================================================
;; Combining

(define-type Z-Mode (U 'add 'blend 'place 'replace))

(: deep-flomap-pin (Z-Mode deep-flomap Real Real deep-flomap Real Real -> deep-flomap))
(define (deep-flomap-pin z-mode dfm1 x1 y1 dfm2 x2 y2)
  (cond
    [(not (and (zero? x2) (zero? y2)))
     (deep-flomap-pin z-mode dfm1 (- x1 x2) (- y1 y2) dfm2 0 0)]
    [else
     (define-values (w1 h1) (deep-flomap-size dfm1))
     (define-values (w2 h2) (deep-flomap-size dfm2))
     (let ([x1  (real->double-flonum x1)]
           [y1  (real->double-flonum y1)])
       ;; dfm1 and dfm2 offsets, in final image coordinates
       (define dx1 (fl->fx (round (max 0.0 (- x1)))))
       (define dy1 (fl->fx (round (max 0.0 (- y1)))))
       (define dx2 (fl->fx (round (max 0.0 x1))))
       (define dy2 (fl->fx (round (max 0.0 y1))))
       ;; final image size
       (define w (fxmax (fx+ dx1 w1) (fx+ dx2 w2)))
       (define h (fxmax (fx+ dy1 h1) (fx+ dy2 h2)))
       
       (case z-mode
         [(place)  (deep-flomap-superimpose/place w h dfm1 dx1 dy1 w1 h1 dfm2 dx2 dy2 w2 h2)]
         [(blend)  (deep-flomap-superimpose/blend w h dfm1 dx1 dy1 w1 h1 dfm2 dx2 dy2 w2 h2)]
         [else     (deep-flomap-superimpose/replace z-mode w h
                                                    dfm1 dx1 dy1 w1 h1
                                                    dfm2 dx2 dy2 w2 h2)]))]))

(: deep-flomap-superimpose/replace
   (Z-Mode Integer Integer
           deep-flomap Integer Integer Integer Integer
           deep-flomap Integer Integer Integer Integer -> deep-flomap))
(define (deep-flomap-superimpose/replace z-mode w h dfm1 dx1 dy1 w1 h1 dfm2 dx2 dy2 w2 h2)
  (match-define (deep-flomap argb1-fm z1-fm) dfm1)
  (match-define (deep-flomap argb2-fm z2-fm) dfm2)
  (define argb1-vs (flomap-values argb1-fm))
  (define argb2-vs (flomap-values argb2-fm))
  (define z1-vs (flomap-values z1-fm))
  (define z2-vs (flomap-values z2-fm))
  
  (: get-argbz-pixel (FlVector FlVector Integer Integer Integer Integer Integer Integer
                               -> (values Flonum Flonum Flonum Flonum Flonum)))
  (define (get-argbz-pixel argb-vs z-vs dx dy w h x y)
    (let ([x  (fx- x dx)] [y  (fx- y dy)])
      (cond [(and (x . fx>= . 0) (x . fx< . w) (y . fx>= . 0) (y . fx< . h))
             (define i (fx+ x (fx* y w)))
             (define j (fx* 4 i))
             (values (flvector-ref argb-vs j)
                     (flvector-ref argb-vs (fx+ j 1))
                     (flvector-ref argb-vs (fx+ j 2))
                     (flvector-ref argb-vs (fx+ j 3))
                     (flvector-ref z-vs i))]
            [else
             (values 0.0 0.0 0.0 0.0 0.0)])))
  
  (define argb-vs (make-flvector (* 4 w h)))
  (define z-vs (make-flvector (* w h)))
  (let: y-loop : Void ([y : Nonnegative-Fixnum  0])
    (when (y . fx< . h)
      (let: x-loop : Void ([x : Nonnegative-Fixnum  0])
        (cond [(x . fx< . w)
               (define-values (a1 r1 g1 b1 z1) (get-argbz-pixel argb1-vs z1-vs dx1 dy1 w1 h1 x y))
               (define-values (a2 r2 g2 b2 z2) (get-argbz-pixel argb2-vs z2-vs dx2 dy2 w2 h2 x y))
               
               (define i (fx+ x (fx* y w)))
               (define j (fx* 4 i))
               (flvector-set! argb-vs j (fl-alpha-blend a1 a2 a2))
               (flvector-set! argb-vs (fx+ j 1) (fl-alpha-blend r1 r2 a2))
               (flvector-set! argb-vs (fx+ j 2) (fl-alpha-blend g1 g2 a2))
               (flvector-set! argb-vs (fx+ j 3) (fl-alpha-blend b1 b2 a2))
               (flvector-set! z-vs i (case z-mode
                                       [(replace)  (fl-alpha-blend z1 z2 a2)]
                                       [else       (+ z1 z2)]))
               (x-loop (fx+ x 1))]
              [else
               (y-loop (fx+ y 1))]))))
  
  (deep-flomap (flomap argb-vs 4 w h)
               (flomap z-vs 1 w h)))

(: deep-flomap-superimpose/place (Integer Integer
                                          deep-flomap Integer Integer Integer Integer
                                          deep-flomap Integer Integer Integer Integer -> deep-flomap))
(define (deep-flomap-superimpose/place w h dfm1 dx1 dy1 w1 h1 dfm2 dx2 dy2 w2 h2)
  (match-define (deep-flomap argb1-fm z1-fm) dfm1)
  (match-define (deep-flomap argb2-fm z2-fm) dfm2)
  (match-define (flomap argb1-vs 4 argb1-w argb1-h) argb1-fm)
  (match-define (flomap argb2-vs 4 argb2-w argb2-h) argb2-fm)
  (match-define (flomap z1-vs 1 z1-w z1-h) z1-fm)
  (match-define (flomap z2-vs 1 z2-w z2-h) z2-fm)
  
  (: get-alpha-pixel (FlVector Integer Integer Integer Integer Integer Integer
                               -> Flonum))
  (define (get-alpha-pixel vs dx dy w h x y)
    (let ([x  (fx- x dx)] [y  (fx- y dy)])
      (cond [(and (x . fx>= . 0) (x . fx< . w) (y . fx>= . 0) (y . fx< . h))
             (flvector-ref vs (fx* 4 (fx+ x (fx* y w))))]
            [else  0.0])))
  
  (: get-z-pixel (FlVector Integer Integer Integer Integer Integer Integer
                           -> Flonum))
  (define (get-z-pixel vs dx dy w h x y)
    (let ([x  (fx- x dx)] [y  (fx- y dy)])
      (cond [(and (x . fx>= . 0) (x . fx< . w) (y . fx>= . 0) (y . fx< . h))
             (flvector-ref vs (fx+ x (fx* y w)))]
            [else  0.0])))
  
  (define z1-max -inf.0)
  (let: y-loop : Void ([y : Nonnegative-Fixnum  0])
    (when (y . fx< . h)
      (let: x-loop : Void ([x : Nonnegative-Fixnum  0])
        (cond [(x . fx< . w)
               (define a1 (get-alpha-pixel argb1-vs dx1 dy1 w1 h1 x y))
               (define a2 (get-alpha-pixel argb2-vs dx2 dy2 w2 h2 x y))
               (when (and (a1 . > . 0.0) (a2 . > . 0.0))
                 (define z1 (get-z-pixel z1-vs dx1 dy1 w1 h1 x y))
                 (set! z1-max (max z1-max z1)))
               (x-loop (fx+ x 1))]
              [else
               (y-loop (fx+ y 1))]))))
  
  (define new-dfm2 (deep-flomap argb2-fm (fm+ z2-fm z1-max)))
  (deep-flomap-superimpose/replace 'replace w h dfm1 dx1 dy1 w1 h1 new-dfm2 dx2 dy2 w2 h2))

(: deep-flomap-superimpose/blend (Integer Integer
                                          deep-flomap Integer Integer Integer Integer
                                          deep-flomap Integer Integer Integer Integer -> deep-flomap))
(define (deep-flomap-superimpose/blend w h dfm1 dx1 dy1 w1 h1 dfm2 dx2 dy2 w2 h2)
  (match-define (deep-flomap argb1-fm z1-fm) dfm1)
  (match-define (deep-flomap argb2-fm z2-fm) dfm2)
  (define argb1-vs (flomap-values argb1-fm))
  (define argb2-vs (flomap-values argb2-fm))
  (define z1-vs (flomap-values z1-fm))
  (define z2-vs (flomap-values z2-fm))
  
  (define-values (u1-fm v1-fm) (flomap-gradient z1-fm))
  (define-values (u2-fm v2-fm) (flomap-gradient z2-fm))
  (define u1-vs (flomap-values u1-fm))
  (define v1-vs (flomap-values v1-fm))
  (define u2-vs (flomap-values u2-fm))
  (define v2-vs (flomap-values v2-fm))
  
  (: get-argbzuv-pixel (FlVector FlVector FlVector FlVector
                                 Integer Integer Integer Integer Integer Integer
                                 -> (values Flonum Flonum Flonum Flonum Flonum Flonum Flonum)))
  (define (get-argbzuv-pixel argb-vs z-vs u-vs v-vs dx dy w h x y)
    (let ([x  (fx- x dx)] [y  (fx- y dy)])
      (cond [(and (x . fx>= . 0) (x . fx< . w) (y . fx>= . 0) (y . fx< . h))
             (define i (fx+ x (fx* y w)))
             (define j (fx* 4 i))
             (values (flvector-ref argb-vs j)
                     (flvector-ref argb-vs (fx+ j 1))
                     (flvector-ref argb-vs (fx+ j 2))
                     (flvector-ref argb-vs (fx+ j 3))
                     (flvector-ref z-vs i)
                     (flvector-ref u-vs i)
                     (flvector-ref v-vs i))]
            [else
             (values 0.0 0.0 0.0 0.0 0.0 0.0 0.0)])))
  
  (define argb-vs (make-flvector (* 4 w h)))
  (define z-vs (make-flvector (* w h)))
  (let: y-loop : Void ([y : Nonnegative-Fixnum  0])
    (when (y . fx< . h)
      (let: x-loop : Void ([x : Nonnegative-Fixnum  0])
        (cond [(x . fx< . w)
               (define-values (a1 r1 g1 b1 z1 u1 v1)
                 (get-argbzuv-pixel argb1-vs z1-vs u1-vs v1-vs dx1 dy1 w1 h1 x y))
               (define-values (a2 r2 g2 b2 z2 u2 v2)
                 (get-argbzuv-pixel argb2-vs z2-vs u2-vs v2-vs dx2 dy2 w2 h2 x y))
               
               ;; softmax blending
               (define α
                 (cond [(and (a1 . > . 0.0) (a2 . > . 0.0))
                        (define u (- (* a2 u2) (* a1 u1)))
                        (define v (- (* a2 v2) (* a1 v1)))
                        (define β (/ (- (* a2 z2) (* a1 z1))
                                     (flsqrt (+ (* u u) (* v v)))))
                        (flsigmoid (* 15.0 β))]
                       [(a1 . > . 0.0)  0.0]
                       [(a2 . > . 0.0)  1.0]
                       [else  0.5]))
               
               (define i (fx+ x (fx* y w)))
               (define j (fx* 4 i))
               (flvector-set! argb-vs j (fl-convex-combination a1 a2 α))
               (flvector-set! argb-vs (fx+ j 1) (fl-convex-combination r1 r2 α))
               (flvector-set! argb-vs (fx+ j 2) (fl-convex-combination g1 g2 α))
               (flvector-set! argb-vs (fx+ j 3) (fl-convex-combination b1 b2 α))
               (flvector-set! z-vs i (fl-convex-combination z1 z2 α))
               (x-loop (fx+ x 1))]
              [else
               (y-loop (fx+ y 1))]))))
  
  (deep-flomap (flomap argb-vs 4 w h)
               (flomap z-vs 1 w h)))

(: deep-flomap-pin* (Z-Mode Real Real Real Real deep-flomap deep-flomap * -> deep-flomap))
(define (deep-flomap-pin* z-mode x1-frac y1-frac x2-frac y2-frac dfm . dfms)
  (for/fold ([dfm1 dfm]) ([dfm2  (in-list dfms)])
    (define-values (w1 h1) (deep-flomap-size dfm1))
    (define-values (w2 h2) (deep-flomap-size dfm2))
    (deep-flomap-pin z-mode
                     dfm1 (* x1-frac w1) (* y1-frac h1)
                     dfm2 (* x2-frac w2) (* y2-frac h2))))

(: deep-flomap-lt-superimpose (Z-Mode deep-flomap deep-flomap * -> deep-flomap))
(: deep-flomap-lc-superimpose (Z-Mode deep-flomap deep-flomap * -> deep-flomap))
(: deep-flomap-lb-superimpose (Z-Mode deep-flomap deep-flomap * -> deep-flomap))
(: deep-flomap-ct-superimpose (Z-Mode deep-flomap deep-flomap * -> deep-flomap))
(: deep-flomap-cc-superimpose (Z-Mode deep-flomap deep-flomap * -> deep-flomap))
(: deep-flomap-cb-superimpose (Z-Mode deep-flomap deep-flomap * -> deep-flomap))
(: deep-flomap-rt-superimpose (Z-Mode deep-flomap deep-flomap * -> deep-flomap))
(: deep-flomap-rc-superimpose (Z-Mode deep-flomap deep-flomap * -> deep-flomap))
(: deep-flomap-rb-superimpose (Z-Mode deep-flomap deep-flomap * -> deep-flomap))

(define (deep-flomap-lt-superimpose z-mode dfm . dfms)
  (apply deep-flomap-pin* z-mode 0 0 0 0 dfm dfms))

(define (deep-flomap-lc-superimpose z-mode dfm . dfms)
  (apply deep-flomap-pin* z-mode 0 1/2 0 1/2 dfm dfms))

(define (deep-flomap-lb-superimpose z-mode dfm . dfms)
  (apply deep-flomap-pin* z-mode 0 1 0 1 dfm dfms))

(define (deep-flomap-ct-superimpose z-mode dfm . dfms)
  (apply deep-flomap-pin* z-mode 1/2 0 1/2 0 dfm dfms))

(define (deep-flomap-cc-superimpose z-mode dfm . dfms)
  (apply deep-flomap-pin* z-mode 1/2 1/2 1/2 1/2 dfm dfms))

(define (deep-flomap-cb-superimpose z-mode dfm . dfms)
  (apply deep-flomap-pin* z-mode 1/2 1 1/2 1 dfm dfms))

(define (deep-flomap-rt-superimpose z-mode dfm . dfms)
  (apply deep-flomap-pin* z-mode 1 0 1 0 dfm dfms))

(define (deep-flomap-rc-superimpose z-mode dfm . dfms)
  (apply deep-flomap-pin* z-mode 1 1/2 1 1/2 dfm dfms))

(define (deep-flomap-rb-superimpose z-mode dfm . dfms)
  (apply deep-flomap-pin* z-mode 1 1 1 1 dfm dfms))

(: deep-flomap-vl-append (deep-flomap deep-flomap * -> deep-flomap))
(: deep-flomap-vc-append (deep-flomap deep-flomap * -> deep-flomap))
(: deep-flomap-vr-append (deep-flomap deep-flomap * -> deep-flomap))
(: deep-flomap-ht-append (deep-flomap deep-flomap * -> deep-flomap))
(: deep-flomap-hc-append (deep-flomap deep-flomap * -> deep-flomap))
(: deep-flomap-hb-append (deep-flomap deep-flomap * -> deep-flomap))

(define (deep-flomap-vl-append dfm . dfms) (apply deep-flomap-pin* 'add 0 1 0 0 dfm dfms))
(define (deep-flomap-vc-append dfm . dfms) (apply deep-flomap-pin* 'add 1/2 1 1/2 0 dfm dfms))
(define (deep-flomap-vr-append dfm . dfms) (apply deep-flomap-pin* 'add 1 1 1 0 dfm dfms))
(define (deep-flomap-ht-append dfm . dfms) (apply deep-flomap-pin* 'add 1 0 0 0 dfm dfms))
(define (deep-flomap-hc-append dfm . dfms) (apply deep-flomap-pin* 'add 1 1/2 0 1/2 dfm dfms))
(define (deep-flomap-hb-append dfm . dfms) (apply deep-flomap-pin* 'add 1 1 0 1 dfm dfms))
