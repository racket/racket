#lang typed/racket/base

(require racket/match racket/math
         "flonum.rkt"
         "flomap.rkt"
         "deep-flomap-struct.rkt"
         "deep-flomap-parameters.rkt")

(provide deep-flomap-render)

;; Hacks
(define specular-blur 1/2)
(define diffuse-blur 1/2)
(define ambient-transmission-blur-fraction 1/32)

;; ===================================================================================================
;; Ray tracing ops

;; assumes direction to viewer is 0.0 0.0 1.0 (i.e. viewer above at infinity)
(: reflect-view-ray (Flonum Flonum Flonum -> (values Flonum Flonum Flonum)))
(define (reflect-view-ray nx ny nz)
  (values (* 2.0 (* nz nx))
          (* 2.0 (* nz ny))
          (- (* 2.0 (* nz nz)) 1.0)))

;; calculates intensity of transmitted rays using Fresnel's equation
(: transmission-intensity (Flonum Flonum Flonum -> Flonum))
(define (transmission-intensity cos-i η1 η2)
  (define n1/n2 (/ η1 η2))
  (define cos^2-i (* cos-i cos-i))
  (define sin^2-t (* (* n1/n2 n1/n2) (- 1.0 cos^2-i)))
  (define cos-t (flsqrt (- 1.0 sin^2-t)))
  (define n1-cos-i (* η1 cos-i))
  (define n2-cos-t (* η2 cos-t))
  (define n1-cos-t (* η1 cos-t))
  (define n2-cos-i (* η2 cos-i))
  (define perp (/ (- n1-cos-i n2-cos-t)
                  (+ n1-cos-i n2-cos-t)))
  (define parl (/ (- n2-cos-i n1-cos-t)
                  (+ n2-cos-i n1-cos-t)))
  (- 1.0 (* 0.5 (+ (* perp perp) (* parl parl)))))

(: transmitted-vector (Flonum Flonum Flonum Flonum Flonum Flonum Flonum Flonum
                              -> (values Flonum Flonum Flonum)))
(define (transmitted-vector nx ny nz ix iy iz η1 η2)
  (define η1/η2  (/ η1 η2))
  (define cos-i (- (fl3dot nx ny nz ix iy iz)))
  (define cos^2-i (* cos-i cos-i))
  (define sin^2-t (* (* η1/η2 η1/η2) (- 1.0 cos^2-i)))
  (define c (- (* η1/η2 cos-i) (flsqrt (- 1.0 sin^2-t))))
  (define-values (tx1 ty1 tz1) (fl3* ix iy iz η1/η2))
  (define-values (tx2 ty2 tz2) (fl3* nx ny nz c))
  (fl3+ tx1 ty1 tz1 tx2 ty2 tz2))

(: absorb-intensity (Flonum Flonum -> Flonum))
(define (absorb-intensity opacity dist)
  (let* ([o  (+ (* opacity 0.99) 0.005)])
    (cond [(o . = . 0.0)  0.0]
          [else  (exp (* (fllog o) dist))])))

(: beckmann-distribution (Flonum Flonum -> Flonum))
(define (beckmann-distribution cos-θ m)
  (define x (/ (tan (acos cos-θ)) m))
  (define m*cos^2-θ (* m cos-θ cos-θ))
  (/ (exp (- (* x x))) (* pi m*cos^2-θ m*cos^2-θ)))

;; ===================================================================================================
;; Pass 1: tracing from a directional light source

(: trace-directional-light (flomap flomap flomap flomap
                                   Integer Integer Integer Integer -> (values flomap flomap)))
(define (trace-directional-light alpha-fm rgb-fm z-fm normal-fm
                                 x-min x-max y-min y-max)
  (match-define (flomap alpha-vs 1 w h) alpha-fm)
  (match-define (list rgb-vs z-vs normal-vs)
    (map flomap-values (list rgb-fm z-fm normal-fm)))
  
  (define z-max (flomap-max-value z-fm))
  (define opacity-z (/ z-max (transmission-density)))
  ;; max coordinates of the shadow image
  (define sx-max (- w 1.0))
  (define sy-max (- h 1.0))
  ;; vector pointing toward light source, incident vector, and light color
  (define-values (lx ly lz) (match-let ([(list lx ly lz)  (light-direction)])
                              (fl3normalize lx ly lz)))
  (define-values (ix iy iz) (fl3- lx ly lz))
  (match-define (list lr lg lb) (light-intensity))
  ;; view and "half" directions
  (define-values (hx hy hz) (fl3-half-norm lx ly lz 0.0 0.0 1.0))
  ;; material properties
  (define η2 (real->double-flonum (refractive-index)))
  (define η1/η2 (/ 1.0 η2))
  ;; proportion of diffracted reflection
  (define 0.5*v-dot-h (* 0.5 hz))
  (define Ra (ambient-reflectance))
  (define Ta (ambient-transmission))
  (define Rd (diffuse-reflectance))
  (define Rs (specular-reflectance))
  (define Ti (ideal-transmission))
  (define roughness (specular-roughness))
  (define purity (specular-purity))
  
  (match-define (list ar ag ab) (ambient-intensity))
  (define-values (Tar Tag Tab) (fl3* ar ag ab Ta))
  (define-values (Rar Rag Rab) (fl3* ar ag ab Ra))
  
  (define intensity-fm (make-flomap 3 w h))
  (define intensity-vs (flomap-values intensity-fm))
  (define specular-fm (make-flomap 1 w h))
  (define specular-vs (flomap-values specular-fm))
  (define diffuse-fm (make-flomap 3 w h lz))
  (define diffuse-vs (flomap-values diffuse-fm))
  
  ;(define sx-vs (make-flvector (* w h) +nan.0))
  ;(define sy-vs (make-flvector (* w h) +nan.0))
  (define sx-fm (inline-build-flomap 1 w h (λ (k x y i) (+ (fx->fl x) 0.5))))
  (define sy-fm (inline-build-flomap 1 w h (λ (k x y i) (+ (fx->fl y) 0.5))))
  (define sx-vs (flomap-values sx-fm))
  (define sy-vs (flomap-values sy-fm))
  (define Irgb-vs (make-flvector (* 3 w h)))
  
  (for*: ([int-y : Integer  (in-range y-min y-max)]
          [int-x : Integer  (in-range x-min x-max)])
    (define i (fx+ int-x (fx* int-y w)))
    (define a (flvector-ref alpha-vs i))
    (when (a . > . 0.0)
      (define j (fx* 3 i))
      ;; altitude and surface normal
      (define z (flvector-ref z-vs i))
      (define nx (flvector-ref normal-vs j))
      (define ny (flvector-ref normal-vs (fx+ j 1)))
      (define nz (flvector-ref normal-vs (fx+ j 2)))
      ;; cosine of angle between light and surface normal
      (define n-dot-l (fl3dot nx ny nz lx ly lz))
      ;; intensity of incident light (Lambert's cosine law)
      (define-values (Ilr Ilg Ilb) (fl3* lr lg lb n-dot-l))
      (flvector-set! intensity-vs j Ilr)
      (flvector-set! intensity-vs (fx+ j 1) Ilg)
      (flvector-set! intensity-vs (fx+ j 2) Ilb)
      ;; diffraction intensity due to specular, diffuse and ambient reflection
      (cond
        [(n-dot-l . > . 0.0)  ; does the microfacet face the light?
         (define Is
           (cond
             ;; Cook-Torrance specular reflection intensity
             [(Rs . > . 0.0)
              (define n-dot-h (fl3dot nx ny nz hx hy hz))
              (define n-dot-v nz)
              ;; geometrical attenuation factor (has something to do with local reflections)
              (define G (min 1.0
                             (/ (* n-dot-h n-dot-v) 0.5*v-dot-h)
                             (/ (* n-dot-h n-dot-l) 0.5*v-dot-h)))
              ;; scatter distribution
              (define D (beckmann-distribution n-dot-h roughness))
              ;; Fresnel term
              (define F (- 1.0 (transmission-intensity n-dot-l 1.0 η2)))
              (* Rs F (/ D n-dot-l) (/ G n-dot-v))]
             [else  0.0]))
         (flvector-set! specular-vs i Is)
         
         (let*-values ([(Idr Idg Idb)  (fl3* Ilr Ilg Ilb Rd)]
                       [(Idr Idg Idb)  (fl3+ Idr Idg Idb Rar Rag Rab)])
           (flvector-set! diffuse-vs j Idr)
           (flvector-set! diffuse-vs (fx+ j 1) Idg)
           (flvector-set! diffuse-vs (fx+ j 2) Idb))]
        [else
         (flvector-set! diffuse-vs j Rar)
         (flvector-set! diffuse-vs (fx+ j 1) Rag)
         (flvector-set! diffuse-vs (fx+ j 2) Rab)])
      
      (when (and (Ti . > . 0.0) (n-dot-l . > . 0.0))
        ;; ideal transmission vector
        (define-values (tx ty tz) (transmitted-vector nx ny nz ix iy iz 1.0 η2))
        ;; sz = z + dist * tz, so dist = (sz - z) / tz
        (define dist (/ (- 0.0 z) tz))
        (when (and (dist . >= . 0.0) (dist . < . +inf.0))
          ;; transmitted ray intersects with shadow plane at sx sy 0.0
          (define sx (+ 0.5 (->fl int-x) (* dist tx)))
          (define sy (+ 0.5 (->fl int-y) (* dist ty)))
          ;; actual transmission proportion (Fresnel's law)
          (define T (* Ti (transmission-intensity n-dot-l 1.0 η2)))
          ;; intensity of incident light (Lambert's cosine law)
          (define-values (Ilr Ilg Ilb) (fl3* lr lg lb n-dot-l))
          ;; normalized distance to the surface
          (define norm-dist (/ dist opacity-z))
          ;; intensity of the light that strikes the surface
          (define r (flvector-ref rgb-vs j))
          (define g (flvector-ref rgb-vs (fx+ j 1)))
          (define b (flvector-ref rgb-vs (fx+ j 2)))
          (define-values (Ir Ig Ib)
            (values (* T Ilr (absorb-intensity r norm-dist))
                    (* T Ilg (absorb-intensity g norm-dist))
                    (* T Ilb (absorb-intensity b norm-dist))))
          (flvector-set! sx-vs i sx)
          (flvector-set! sy-vs i sy)
          (flvector-set! Irgb-vs j Ir)
          (flvector-set! Irgb-vs (fx+ j 1) Ig)
          (flvector-set! Irgb-vs (fx+ j 2) Ib)))))
  
  (define diffracted-fm (fm+ (fm* (flomap-blur diffuse-fm diffuse-blur)
                                  rgb-fm)
                             (fm* (flomap-blur specular-fm specular-blur)
                                  (fm+ (fm* (- 1.0 purity) rgb-fm)
                                       (fm* purity intensity-fm)))))
  
  ;; approximate ambient transmission by casting light downward with no refraction, then blurring
  (define ambient-shadow-fm (make-flomap 3 w h))
  (define ambient-shadow-vs (flomap-values ambient-shadow-fm))
  (when (Ta . > . 0.0)
    (for*: ([int-y : Integer  (in-range y-min y-max)]
            [int-x : Integer  (in-range x-min x-max)])
      (define i (fx+ int-x (fx* int-y w)))
      (define a (flvector-ref alpha-vs i))
      (when (a . > . 0.0)
        (define z (flvector-ref z-vs i))
        (define j (fx* 3 i))
        (define r (flvector-ref rgb-vs j))
        (define g (flvector-ref rgb-vs (fx+ j 1)))
        (define b (flvector-ref rgb-vs (fx+ j 2)))
        (define norm-dist (/ z opacity-z))
        (define-values (Ir Ig Ib)
          (values (* Tar (absorb-intensity r norm-dist))
                  (* Tag (absorb-intensity g norm-dist))
                  (* Tab (absorb-intensity b norm-dist))))
        (flvector-set! ambient-shadow-vs j Ir)
        (flvector-set! ambient-shadow-vs (fx+ j 1) Ig)
        (flvector-set! ambient-shadow-vs (fx+ j 2) Ib))))
  
  ;; cast approximate shadow volumes
  (define shadow-fm (flomap-blur ambient-shadow-fm (* ambient-transmission-blur-fraction (min w h))))
  (define shadow-vs (flomap-values shadow-fm))
  (when (Ti . > . 0.0)
    ;; Gaussian kernels - make as wide as possible to keep from having to reallocate
    (define kxs (make-flvector w))
    (define kys (make-flvector h))
    (for*: ([int-y : Integer  (in-range y-min (- y-max 1))]
            [int-x : Integer  (in-range x-min (- x-max 1))])
      (define i00 (fx+ int-x (fx* int-y w)))
      (define i01 (fx+ i00 1))
      (define i10 (fx+ i00 w))
      (define i11 (fx+ i10 1))
      (define sx00 (flvector-ref sx-vs i00))
      (define sx01 (flvector-ref sx-vs i01))
      (define sx10 (flvector-ref sx-vs i10))
      (define sx11 (flvector-ref sx-vs i11))
      (when (and (flrational? sx00) (flrational? sx01)
                 (flrational? sx10) (flrational? sx11))
        (define sy00 (flvector-ref sy-vs i00))
        (define sy01 (flvector-ref sy-vs i01))
        (define sy10 (flvector-ref sy-vs i10))
        (define sy11 (flvector-ref sy-vs i11))
        (define sx-min (min sx00 sx01 sx10 sx11))
        (define sy-min (min sy00 sy01 sy10 sy11))
        (define sx-max (max sx00 sx01 sx10 sx11))
        (define sy-max (max sy00 sy01 sy10 sy11))
        ;; find the mean and standard deviation
        (define sx-mid (* 0.25 (+ sx00 sx01 sx10 sx11)))
        (define sy-mid (* 0.25 (+ sy00 sy01 sy10 sy11)))
        (define sx-mid^2 (* 0.25 (+ (* sx00 sx00) (* sx01 sx01) (* sx10 sx10) (* sx11 sx11))))
        (define sy-mid^2 (* 0.25 (+ (* sy00 sy00) (* sy01 sy01) (* sy10 sy10) (* sy11 sy11))))
        (define sx-stddev (flsqrt (- sx-mid^2 (* sx-mid sx-mid))))
        (define sy-stddev (flsqrt (- sy-mid^2 (* sy-mid sy-mid))))
        (define x-min (fxmax 0 (fl->fx (floor sx-min))))
        (define x-max (fxmin w (fx+ 1 (fl->fx (floor sx-max)))))
        (define y-min (fxmax 0 (fl->fx (floor sy-min))))
        (define y-max (fxmin h (fx+ 1 (fl->fx (floor sy-max)))))
        (define x-size (fx- x-max x-min))
        (define y-size (fx- y-max y-min))
        (when (and (x-size . fx> . 0) (y-size . fx> . 0))
          ;; average the color
          (define j00 (fx* 3 i00))
          (define j01 (fx* 3 i01))
          (define j10 (fx* 3 i10))
          (define j11 (fx* 3 i11))
          (define r (* 0.25 (+ (flvector-ref Irgb-vs j00)
                               (flvector-ref Irgb-vs j01)
                               (flvector-ref Irgb-vs j10)
                               (flvector-ref Irgb-vs j11))))
          (define g (* 0.25 (+ (flvector-ref Irgb-vs (fx+ j00 1))
                               (flvector-ref Irgb-vs (fx+ j01 1))
                               (flvector-ref Irgb-vs (fx+ j10 1))
                               (flvector-ref Irgb-vs (fx+ j11 1)))))
          (define b (* 0.25 (+ (flvector-ref Irgb-vs (fx+ j00 2))
                               (flvector-ref Irgb-vs (fx+ j01 2))
                               (flvector-ref Irgb-vs (fx+ j10 2))
                               (flvector-ref Irgb-vs (fx+ j11 2)))))
          ;; precalculate the Gaussian kernel for the x direction
          (for ([dx  (in-range x-size)])
            (define x (fx+ dx x-min))
            (define d (/ (- (+ 0.5 (fx->fl x)) sx-mid) sx-stddev))
            (define kx (exp (* -0.5 (* d d))))
            (flvector-set! kxs dx kx))
          ;; precalculate the Gaussian kernel for the y direction
          ;; this shouldn't help because it's used only once per y iteration, but it reduces allocs
          ;; within the loop (unsafe-flexp has no bytecode op yet, so its args and return are boxed)
          (for ([dy  (in-range y-size)])
            (define y (fx+ dy y-min))
            (define d (/ (- (+ 0.5 (fx->fl y)) sy-mid) sy-stddev))
            (define ky (exp (* -0.5 (* d d))))
            (flvector-set! kys dy ky))
          ;; normalization constant for a 2D Gaussian kernel
          (define c (* 2.0 pi sx-stddev sy-stddev))
          ;; cast the approximate shadow volume
          (let y-loop ([dy 0])
            (when (dy . fx< . y-size)
              (define ky (flvector-ref kys dy))
              (cond [(ky . > . 0.1)
                     (define a (/ ky c))
                     (define Ir (* r a))
                     (define Ig (* g a))
                     (define Ib (* b a))
                     (define i (fx* 3 (fx+ x-min (fx* (fx+ dy y-min) w))))
                     (let x-loop ([dx 0] [i i])
                       (cond [(dx . fx< . x-size)
                              (define kx (flvector-ref kxs dx))
                              (when (kx . > . 0.1)
                                (flvector-set!
                                 shadow-vs i (+ (* Ir kx) (flvector-ref shadow-vs i)))
                                (define i1 (fx+ i 1))
                                (flvector-set!
                                 shadow-vs i1 (+ (* Ig kx) (flvector-ref shadow-vs i1)))
                                (define i2 (fx+ i 2))
                                (flvector-set!
                                 shadow-vs i2 (+ (* Ib kx) (flvector-ref shadow-vs i2))))
                              (x-loop (fx+ 1 dx) (fx+ 3 i))]
                             [else
                              (y-loop (fx+ 1 dy))]))]
                    [else
                     (y-loop (fx+ 1 dy))])))))))
  
  ;; blur the shadow a bit to make up for approximating it with Gaussians
  (values diffracted-fm (flomap-box-blur shadow-fm 1)))

;; ===================================================================================================
;; Pass 2: tracing from a directional viewer

(: trace-directional-view (flomap flomap flomap flomap flomap
                                  Integer Integer Integer Integer -> (values flomap flomap)))
(define (trace-directional-view alpha-fm rgb-fm z-fm normal-fm shadow-fm
                                x-min x-max y-min y-max)
  (define-values (w h) (flomap-size alpha-fm))
  (match-define (list alpha-vs rgb-vs z-vs normal-vs shadow-vs)
    (map flomap-values (list alpha-fm rgb-fm z-fm normal-fm shadow-fm)))
  
  (define w-1 (fx- w 1))
  (define h-1 (fx- h 1))
  (define x-size (fx->fl w))
  (define y-size (fx->fl h))
  (define z-size (flomap-max-value z-fm))
  (define x-mid (* 0.5 x-size))
  (define y-mid (* 0.5 y-size))
  (define opacity-z (/ z-size (transmission-density)))
  
  ;; reflected wall is tilted a bit toward the viewer
  (define wall-tilt-θ (* 1/8 pi))
  (define cos-wall-tilt-θ (cos wall-tilt-θ))
  (define sin-wall-tilt-θ (sin wall-tilt-θ))
  (match-define (list Irr Irg Irb) (reflected-intensity))
  
  ;; material properties
  (define η2 (refractive-index))
  (define η1/η2 (/ 1.0 η2))
  (define Ri (ideal-reflectance))
  (define Ti (ideal-transmission))
  
  (define reflected-fm (make-flomap 3 w h))
  (define reflected-vs (flomap-values reflected-fm))
  (define transmitted-fm (make-flomap 3 w h))
  (define transmitted-vs (flomap-values transmitted-fm))
  
  (when (or (Ri . > . 0.0) (Ti . > . 0.0))
    (for*: ([int-y : Integer  (in-range y-min y-max)]
            [int-x : Integer  (in-range x-min x-max)])
      (define i (fx+ int-x (fx* int-y w)))
      (define a (flvector-ref alpha-vs i))
      (when (a . > . 0.0)
        (define j (fx* 3 i))
        ;; surface normal
        (define nx (flvector-ref normal-vs j))
        (define ny (flvector-ref normal-vs (fx+ j 1)))
        (define nz (flvector-ref normal-vs (fx+ j 2)))
        ;; cosine of angle between viewer and surface normal
        ;; with gradient inferred from z flomap, this is always > 0.0
        (define cos-i nz)
        ;; transmitted intensity
        (define orig-T (transmission-intensity cos-i 1.0 η2))
        (define T (* Ti orig-T))
        (define R (* Ri (- 1.0 orig-T)))
        ;; surface coordinates
        (define x (+ 0.5 (->fl int-x)))
        (define y (+ 0.5 (->fl int-y)))
        (define z (flvector-ref z-vs i))
        ;; reflection
        (when (and (Ri . > . 0.0)
                   (int-x . fx> . 0) (int-x . fx< . w-1)
                   (int-y . fx> . 0) (int-y . fx< . h-1))
          (define-values (rx ry rz) (reflect-view-ray nx ny nz))
          ;; tilt the wall a little so flat surfaces reflect something
          (define ry* (- (* ry cos-wall-tilt-θ) (* rz sin-wall-tilt-θ)))
          ;(define rz* (+ (* ry sin-wall-tilt-θ) (* rz cos-wall-tilt-θ)))
          ;; distance to the wall
          (define rdist (/ (- (- z-size) y) ry*))
          (define sx (+ x (* rx rdist)))
          (define sy (+ y (* ry rdist)))
          (define sz (+ z (* rz rdist)))
          (when (rdist . >= . 0.0)
            (define cdist (fl3dist sx sy sz x-mid y-mid 0.0))
            (define v (flsigmoid (* 0.25 (- (* 4.5 z-size) cdist))))
            (let-values ([(r g b) (fl3* Irr Irg Irb (* R v))])
              (flvector-set! reflected-vs j r)
              (flvector-set! reflected-vs (fx+ j 1) g)
              (flvector-set! reflected-vs (fx+ j 2) b))))
        ;; transmission (refraction)
        (when (Ti . > . 0.0)
          (define snx (flvector-ref normal-vs j))
          (define sny (flvector-ref normal-vs (fx+ j 1)))
          (define snz (flvector-ref normal-vs (fx+ j 2)))
          (define-values (tx ty tz) (transmitted-vector snx sny snz 0.0 0.0 -1.0 1.0 η2))
          ;; sz = z + dist * tz, so dist = (sz - z) / tz
          (define dist (/ (- 0.0 z) tz))
          (when (and (dist . >= . 0.0) (dist . < . +inf.0))
            ;; Shadow intersection point
            (define sx (+ x (* dist tx)))
            (define sy (+ y (* dist ty)))
            ;; Shadow intersection color
            (define sr (flomap-bilinear-ref shadow-fm 0 sx sy))
            (define sg (flomap-bilinear-ref shadow-fm 1 sx sy))
            (define sb (flomap-bilinear-ref shadow-fm 2 sx sy))
            ;; normalized distance to the surface
            (define norm-dist (/ dist opacity-z))
            ;; intensities of each r g b by the time the light emerges from the surface
            (define-values (r g b)
              ;; colors represent absorption rates
              (let ([r  (flvector-ref rgb-vs j)]
                    [g  (flvector-ref rgb-vs (fx+ j 1))]
                    [b  (flvector-ref rgb-vs (fx+ j 2))])
                (values (* T sr (absorb-intensity r norm-dist))
                        (* T sg (absorb-intensity g norm-dist))
                        (* T sb (absorb-intensity b norm-dist)))))
            (flvector-set! transmitted-vs j r)
            (flvector-set! transmitted-vs (fx+ j 1) g)
            (flvector-set! transmitted-vs (fx+ j 2) b))))))
  
  (values reflected-fm transmitted-fm))

;; ===================================================================================================
;; Full rendering

(: prep-background (flomap Integer Integer -> (Option flomap)))
(define (prep-background fm w h)
  (let loop ([fm  (flomap-cc-crop fm w h)])
    (case (flomap-components fm)
      [(0)  #f]
      [(1)  (flomap-append-components fm fm fm)]
      [(2)  (define value-fm (flomap-ref-component fm 1))
            (loop (flomap-append-components fm value-fm value-fm))]
      [(3)  fm]
      [(4)  (flomap-drop-components (flomap-cc-superimpose (make-flomap 4 w h 1.0) fm) 1)]
      [else  (raise-type-error 'deep-flomap-render "flomap with 0, 1, 2, 3 or 4 components" fm)])))

(: deep-flomap-render (case-> (deep-flomap -> flomap)
                              (deep-flomap (Option flomap) -> flomap)))
(define deep-flomap-render
  (case-lambda
    [(dfm)  (deep-flomap-render dfm #f)]
    [(dfm background-fm)
     (let ([dfm  (deep-flomap-inset dfm 1)])
       (define-values (w h) (deep-flomap-size dfm))
       (define argb-fm (flomap-divide-alpha (deep-flomap-argb dfm)))
       (define alpha-fm (flomap-ref-component argb-fm 0))
       (define rgb-fm (flomap-drop-components argb-fm 1))
       (define z-fm (fmmax 0.0 (deep-flomap-z dfm)))
       (define normal-fm (flomap-gradient-normal z-fm))
       (define bg-fm (if background-fm (prep-background background-fm w h) #f))
       (define-values (x-min y-min x-max y-max)
         (let-values ([(x-min y-min x-max y-max) (flomap-nonzero-rect alpha-fm)])
           (values (max 0 (- x-min 1)) (max 0 (- y-min 1))
                   (min w (+ x-max 1)) (min h (+ y-max 1)))))
       
       ;; pass 1: trace from the light source
       (define-values (diffracted-fm raw-shadow-fm)
         (trace-directional-light alpha-fm rgb-fm z-fm normal-fm x-min x-max y-min y-max))
       
       ;; two Gaussian blurs by half of σ^2 is equivalent to one Gaussian blur by σ^2
       (define σ^2 (real->double-flonum (sqr (* (min w h) (shadow-blur)))))
       
       ;; blur the shadow to simulate internal scatter
       (define shadow-fm
         (cond [bg-fm
                (let* ([fm  (flomap-blur raw-shadow-fm (flsqrt (* 1/3 σ^2)))]
                       [fm  (fm* fm bg-fm)]
                       [fm  (flomap-blur fm (flsqrt (* 1/3 σ^2)))])
                  fm)]
               [else
                (flomap-blur raw-shadow-fm (flsqrt (* 2/3 σ^2)))]))
       
       ;; pass 2: trace from the viewer
       (define-values (reflected-fm raw-transmitted-fm)
         (trace-directional-view alpha-fm rgb-fm z-fm normal-fm shadow-fm x-min x-max y-min y-max))
       ;; simulate scatter some more
       (define transmitted-fm (flomap-blur raw-transmitted-fm (flsqrt (* 1/3 σ^2))))
       ;; add all the light together, convert to premultiplied-alpha flomap
       (let* ([fm  (fm+ (fm+ diffracted-fm transmitted-fm) reflected-fm)]
              [fm  (flomap-append-components alpha-fm fm)]
              [fm  (flomap-multiply-alpha fm)])
         (flomap-inset fm -1)))]))
