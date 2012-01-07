#lang racket/base

(require racket/match racket/math racket/provide unstable/parameter-group racket/flonum
         "unsafe.rkt"
         "flomap.rkt"
         "deep-flomap.rkt")

(provide
 ;; lighting parameters
 light-direction
 light-intensity
 ambient-intensity
 reflected-intensity
 deep-flomap-lighting
 (struct-out deep-flomap-lighting-value)
 ;; material parameters
 refractive-indexes
 ->refractive-index
 refractive-index
 ideal-reflectance
 ideal-transmission
 transmission-density
 specular-reflectance
 specular-roughness
 specular-purity
 diffuse-reflectance
 ambient-reflectance
 ambient-transmission
 shadow-blur
 deep-flomap-material
 (struct-out deep-flomap-material-value)
 (matching-identifiers-out #rx".*-material" (all-defined-out))
 ;; ray tracing functions
 deep-flomap-render
 )

;; ===================================================================================================
;; Rendering parameters

;; Hacks
(define specular-blur 1/2)
(define diffuse-blur 1/2)
(define ideal-transmission-blur 1)
(define ambient-transmission-blur-fraction 1/32)

(define refractive-indexes
  #hash((diamond . 2.42)
        (cubic-zirconia . 2.15)
        (ruby . 1.76)
        (enamel . 1.63)
        (glass . 1.54)
        (wax . 1.43)
        (water . 1.33)
        (vacuum . 1.0)))

(define (->refractive-index idx)
  (cond [(symbol? idx)
         (hash-ref refractive-indexes idx
                   (λ () (error 'refractive-index
                                "`refractive-indexes' does not have a refractive index for ~e"
                                idx)))]
        [(rational? idx)  (exact->inexact idx)]))

(define (list-exact->inexact vs)
  (map exact->inexact vs))

;; light parameters
(define light-direction (make-parameter '(0.0 -1.0 1.0) list-exact->inexact))
(define light-intensity (make-parameter '(1.0 1.0 1.0) list-exact->inexact))
(define ambient-intensity (make-parameter '(1.0 1.0 1.0) list-exact->inexact))
(define reflected-intensity (make-parameter '(1.0 1.0 1.0) list-exact->inexact))

(define-parameter-group deep-flomap-lighting
  (light-direction light-intensity ambient-intensity reflected-intensity))

;; material parameters
(define refractive-index (make-parameter (->refractive-index 'glass) ->refractive-index))
(define ideal-reflectance (make-parameter 1.0 exact->inexact))
(define ideal-transmission (make-parameter 1.0 exact->inexact))
(define transmission-density (make-parameter 0.65 exact->inexact))
(define specular-reflectance (make-parameter 0.15 exact->inexact))
(define specular-roughness (make-parameter 0.15 exact->inexact))
(define specular-purity (make-parameter 1.0 exact->inexact))
(define diffuse-reflectance (make-parameter 0.25 exact->inexact))
(define ambient-reflectance (make-parameter 0.1 exact->inexact))
(define ambient-transmission (make-parameter 0.7 exact->inexact))
(define shadow-blur (make-parameter 0.02 exact->inexact))

(define-parameter-group deep-flomap-material
  (refractive-index ideal-reflectance ideal-transmission transmission-density
                    specular-reflectance specular-roughness specular-purity
                    diffuse-reflectance ambient-reflectance ambient-transmission
                    shadow-blur))

(define matte-material
  (deep-flomap-material-value
   'vacuum 0.0 0.0 1.0
   0.0 1.0 1.0
   1.0 0.25 0.0
   0.0))

(define dull-plastic-material
  (deep-flomap-material-value
   'glass 0.0 0.0 1.0
   1.0 0.25 1.0
   1.0 0.25 0.0
   0.0))

(define wax-material
  (deep-flomap-material-value
   'wax 1.0 0.5 1.25
   0.5 0.5 0.5
   0.5 0.5 0.5
   0.04))

(define plastic-material
  (deep-flomap-material-value
   'glass 0.375 1.0 2.0
   0.25 0.15 1.0
   0.6 0.5 0.1
   0.03))

(define metal-material
  (deep-flomap-material-value
   3.0 0.3 0.0 1.0
   0.8 0.1 0.2
   0.2 0.8 0.0
   0.0))

(define porcelain-material
  (deep-flomap-material-value
   'enamel 0.9 0.5 1.5
   0.4 0.2 1.0
   0.5 0.5 0.5
   0.04))

(define frosted-glass-material
  (deep-flomap-material-value
   'glass 0.9 1.0 0.8
   0.4 0.2 1.0
   0.5 0.1 0.5
   0.04))

(define glass-material
  (deep-flomap-material-value
   'glass 1.0 1.0 0.65
   0.15 0.15 1.0
   0.25 0.1 0.7
   0.02))

(define diamond-material
  (deep-flomap-material-value
   'diamond 1.0 1.0 0.5
   0.15 0.15 1.0
   0.15 0.1 0.7
   0.02))

;; ===================================================================================================
;; Ray tracing ops

;; assumes direction to viewer is 0.0 0.0 1.0 (i.e. viewer above at infinity)
(define (unsafe-reflect-view-ray nx ny nz)
  (values (unsafe-fl* 2.0 (unsafe-fl* nz nx))
          (unsafe-fl* 2.0 (unsafe-fl* nz ny))
          (unsafe-fl- (unsafe-fl* 2.0 (unsafe-fl* nz nz)) 1.0)))

(define (unsafe-transmission-intensity cos-i η1 η2)
  ;; Fresnel's equation
  (define n1/n2 (unsafe-fl/ η1 η2))
  (define cos^2-i (unsafe-fl* cos-i cos-i))
  (define sin^2-t (unsafe-fl* (unsafe-fl* n1/n2 n1/n2) (unsafe-fl- 1.0 cos^2-i)))
  (define cos-t (unsafe-flsqrt (unsafe-fl- 1.0 sin^2-t)))
  (define n1-cos-i (unsafe-fl* η1 cos-i))
  (define n2-cos-t (unsafe-fl* η2 cos-t))
  (define n1-cos-t (unsafe-fl* η1 cos-t))
  (define n2-cos-i (unsafe-fl* η2 cos-i))
  (define perp (unsafe-fl/ (unsafe-fl- n1-cos-i n2-cos-t)
                           (unsafe-fl+ n1-cos-i n2-cos-t)))
  (define parl (unsafe-fl/ (unsafe-fl- n2-cos-i n1-cos-t)
                           (unsafe-fl+ n2-cos-i n1-cos-t)))
  (unsafe-fl- 1.0 (unsafe-fl* 0.5 (unsafe-fl+ (unsafe-fl* perp perp) (unsafe-fl* parl parl)))))

(define (unsafe-transmitted-vector nx ny nz ix iy iz η1 η2)
  (define η1/η2  (unsafe-fl/ η1 η2))
  (define cos-i (unsafe-flneg (unsafe-fl3dot nx ny nz ix iy iz)))
  (define cos^2-i (unsafe-fl* cos-i cos-i))
  (define sin^2-t (unsafe-fl* (unsafe-fl* η1/η2 η1/η2) (unsafe-fl- 1.0 cos^2-i)))
  (define c (unsafe-fl- (unsafe-fl* η1/η2 cos-i) (unsafe-flsqrt (unsafe-fl- 1.0 sin^2-t))))
  (define-values (tx1 ty1 tz1) (unsafe-fl3* ix iy iz η1/η2))
  (define-values (tx2 ty2 tz2) (unsafe-fl3* nx ny nz c))
  (unsafe-fl3+ tx1 ty1 tz1 tx2 ty2 tz2))

(define-syntax-rule (unsafe-transmit opacity dist)
  (let* ([o  (unsafe-fl+ (unsafe-fl* opacity 0.99) 0.005)])
    (cond [(unsafe-fl= 0.0 o)  0.0]
          [else  (unsafe-flexp (unsafe-flproduct (unsafe-fllog o) dist))])))

(define-syntax-rule (unsafe-beckmann-distribution n-dot-h surface-roughness)
  (let ([cos-θ  n-dot-h]
        [m      surface-roughness])
    (define x (unsafe-fl/ (unsafe-fltan (unsafe-flacos cos-θ)) m))
    (define m*cos^2-θ (unsafe-flproduct m cos-θ cos-θ))
    (unsafe-fl/ (unsafe-flexp (unsafe-flneg (unsafe-fl* x x)))
                (unsafe-flproduct pi m*cos^2-θ m*cos^2-θ))))

;; ===================================================================================================
;; Pass 1: tracing from a directional light source

(define (trace-directional-light alpha-fm rgb-fm z-fm normal-fm)
  (match-define (flomap alpha-vs 1 w h) alpha-fm)
  (match-define (list rgb-vs z-vs normal-vs) (map flomap-values (list rgb-fm z-fm normal-fm)))
  
  (define z-max (flomap-max-value z-fm))
  (define opacity-z (/ z-max (transmission-density)))
  ;; max coordinates of the shadow image
  (define sx-max (- w 1.0))
  (define sy-max (- h 1.0))
  ;; vector pointing toward light source, incident vector, and light color
  (define-values (lx ly lz) (match-let ([(list lx ly lz)  (light-direction)])
                              (unsafe-fl3normalize lx ly lz)))
  (define-values (ix iy iz) (unsafe-fl3neg lx ly lz))
  (match-define (list lr lg lb) (light-intensity))
  ;; view and "half" directions
  (define-values (hx hy hz) (unsafe-fl3-half-norm lx ly lz 0.0 0.0 1.0))
  ;; material properties
  (define η2 (exact->inexact (refractive-index)))
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
  (define-values (Tar Tag Tab) (unsafe-fl3* ar ag ab Ta))
  (define-values (Rar Rag Rab) (unsafe-fl3* ar ag ab Ra))
  
  (define intensity-fm (make-flomap 3 w h))
  (define intensity-vs (flomap-values intensity-fm))
  (define specular-fm (make-flomap 1 w h))
  (define specular-vs (flomap-values specular-fm))
  (define diffuse-fm (make-flomap 3 w h lz))
  (define diffuse-vs (flomap-values diffuse-fm))
  
  (define sx-vs (make-flvector (* w h) +nan.0))
  (define sy-vs (make-flvector (* w h) +nan.0))
  (define Irgb-vs (make-flvector (* 3 w h)))
  
  (for* ([int-y  (in-range h)] [int-x  (in-range w)])
    (define i (unsafe-fx+ int-x (unsafe-fx* int-y w)))
    (define a (unsafe-flvector-ref alpha-vs i))
    (when (a . unsafe-fl> . 0.0)
      (define j (unsafe-fx* 3 i))
      ;; altitude and surface normal
      (define z (unsafe-flvector-ref z-vs i))
      (define-values (nx ny nz) (unsafe-flvector-3ref normal-vs j))
      ;; cosine of angle between light and surface normal
      (define n-dot-l (unsafe-fl3dot nx ny nz lx ly lz))
      ;; intensity of incident light (Lambert's cosine law)
      (define-values (Ilr Ilg Ilb) (unsafe-fl3* lr lg lb n-dot-l))
      (unsafe-flvector-3set! intensity-vs j Ilr Ilg Ilb)
      ;; diffraction intensity due to specular, diffuse and ambient reflection
      (cond
        [(n-dot-l . unsafe-fl> . 0.0)  ; does the microfacet face the light?
         (define Is
           (cond
             #;; just Beckmann's distribution
             [(Rs . unsafe-fl> . 0.0)
              (define n-dot-h (unsafe-fl3dot nx ny nz hx hy hz))
              (unsafe-fl* Rs (unsafe-beckmann-distribution n-dot-h roughness))]
             ;; Cook-Torrance specular reflection intensity
             [(Rs . unsafe-fl> . 0.0)
              (define n-dot-h (unsafe-fl3dot nx ny nz hx hy hz))
              (define n-dot-v nz)
              ;; geometrical attenuation factor (has something to do with local reflections)
              (define G (unsafe-flmin
                         1.0 (unsafe-flmin (unsafe-fl/ (unsafe-fl* n-dot-h n-dot-v) 0.5*v-dot-h)
                                           (unsafe-fl/ (unsafe-fl* n-dot-h n-dot-l) 0.5*v-dot-h))))
              ;; scatter distribution
              (define D (unsafe-beckmann-distribution n-dot-h roughness))
              ;; Fresnel term
              (define F (unsafe-fl- 1.0 (unsafe-transmission-intensity n-dot-l 1.0 η2)))
              (unsafe-flproduct Rs F (unsafe-fl/ D n-dot-l) (unsafe-fl/ G n-dot-v))]
             [else  0.0]))
         (unsafe-flvector-set! specular-vs i Is)
         
         (let*-values ([(Idr Idg Idb)  (unsafe-fl3* Ilr Ilg Ilb Rd)]
                       [(Idr Idg Idb)  (unsafe-fl3+ Idr Idg Idb Rar Rag Rab)])
           (unsafe-flvector-3set! diffuse-vs j Idr Idg Idb))]
        [else
         (unsafe-flvector-3set! diffuse-vs j Rar Rag Rab)])
      
      (when (and (Ti . unsafe-fl> . 0.0) (n-dot-l . unsafe-fl> . 0.0))
        ;; ideal transmission vector
        (define-values (tx ty tz) (unsafe-transmitted-vector nx ny nz ix iy iz 1.0 η2))
        ;; sz = z + dist * tz, so dist = (sz - z) / tz
        (define dist (unsafe-fl/ (unsafe-fl- 0.0 z) tz))
        (when (and (dist . unsafe-fl>= . 0.0) (dist . unsafe-fl< . +inf.0))
          ;; transmitted ray intersects with shadow plane at sx sy 0.0
          (define sx (unsafe-flsum 0.5 (unsafe-fx->fl int-x) (unsafe-fl* dist tx)))
          (define sy (unsafe-flsum 0.5 (unsafe-fx->fl int-y) (unsafe-fl* dist ty)))
          ;; actual transmission proportion (Fresnel's law)
          (define T (unsafe-fl* Ti (unsafe-transmission-intensity n-dot-l 1.0 η2)))
          ;; intensity of incident light (Lambert's cosine law)
          (define-values (Ilr Ilg Ilb) (unsafe-fl3* lr lg lb n-dot-l))
          ;; normalized distance to the surface
          (define norm-dist (unsafe-fl/ dist opacity-z))
          ;; intensity of the light that strikes the surface
          (define-values (r g b) (unsafe-flvector-3ref rgb-vs j))
          (define-values (Ir Ig Ib)
            ;; unsafe-transmit calculates intensity using color as absorption rate
            (values (unsafe-flproduct T Ilr (unsafe-transmit r norm-dist))
                    (unsafe-flproduct T Ilg (unsafe-transmit g norm-dist))
                    (unsafe-flproduct T Ilb (unsafe-transmit b norm-dist))))
          (unsafe-flvector-set! sx-vs i sx)
          (unsafe-flvector-set! sy-vs i sy)
          (unsafe-flvector-3set! Irgb-vs j Ir Ig Ib)))))
  
  (define diffracted-fm (fm+ (fm* rgb-fm (flomap-blur diffuse-fm diffuse-blur))
                             (fm* (flomap-blur specular-fm specular-blur)
                                  (fm+ (fm* (- 1.0 purity) rgb-fm)
                                       (fm* purity intensity-fm)))))
  
  ;; approximate ambient transmission by casting light downward with no refraction, then blurring
  (define ambient-shadow-fm (make-flomap 3 w h))
  (define ambient-shadow-vs (flomap-values ambient-shadow-fm))
  (when (Ta . unsafe-fl> . 0.0)
    (for* ([int-y  (in-range h)] [int-x  (in-range w)])
      (define i (unsafe-fx+ int-x (unsafe-fx* int-y w)))
      (define a (unsafe-flvector-ref alpha-vs i))
      (when (a . unsafe-fl> . 0.0)
        (define z (unsafe-flvector-ref z-vs i))
        (define j (unsafe-fx* 3 i))
        (define-values (r g b) (unsafe-flvector-3ref rgb-vs j))
        (define norm-dist (unsafe-fl/ z opacity-z))
        (define-values (Ir Ig Ib)
          ;; note: unsafe-transmit converts colors to absorption rates
          (values (unsafe-fl* Tar (unsafe-transmit r norm-dist))
                  (unsafe-fl* Tag (unsafe-transmit g norm-dist))
                  (unsafe-fl* Tab (unsafe-transmit b norm-dist))))
        (unsafe-flvector-3set! ambient-shadow-vs j Ir Ig Ib))))
  
  ;; cast approximate shadow volumes
  (define shadow-fm (flomap-blur ambient-shadow-fm (* ambient-transmission-blur-fraction (min w h))))
  (define shadow-vs (flomap-values shadow-fm))
  (when (Ti . unsafe-fl> . 0.0)
    ;; Gaussian kernels - make as wide as possible to keep from having to reallocate
    (define kxs (make-flvector w))
    (define kys (make-flvector h))
    (for* ([int-y  (in-range (- h 1))] [int-x  (in-range (- w 1))])
      (define i00 (unsafe-fx+ int-x (unsafe-fx* int-y w)))
      (define i01 (unsafe-fx+ i00 1))
      (define i10 (unsafe-fx+ i00 w))
      (define i11 (unsafe-fx+ i10 1))
      (define sx00 (unsafe-flvector-ref sx-vs i00))
      (define sx01 (unsafe-flvector-ref sx-vs i01))
      (define sx10 (unsafe-flvector-ref sx-vs i10))
      (define sx11 (unsafe-flvector-ref sx-vs i11))
      (when (and (unsafe-flrational? sx00) (unsafe-flrational? sx01)
                 (unsafe-flrational? sx10) (unsafe-flrational? sx11))
        (define sy00 (unsafe-flvector-ref sy-vs i00))
        (define sy01 (unsafe-flvector-ref sy-vs i01))
        (define sy10 (unsafe-flvector-ref sy-vs i10))
        (define sy11 (unsafe-flvector-ref sy-vs i11))
        (define sx-min (unsafe-flmin* sx00 sx01 sx10 sx11))
        (define sy-min (unsafe-flmin* sy00 sy01 sy10 sy11))
        (define sx-max (unsafe-flmax* sx00 sx01 sx10 sx11))
        (define sy-max (unsafe-flmax* sy00 sy01 sy10 sy11))
        
        (define sx-mid (unsafe-fl* 0.25 (unsafe-flsum sx00 sx01 sx10 sx11)))
        (define sy-mid (unsafe-fl* 0.25 (unsafe-flsum sy00 sy01 sy10 sy11)))
        (define sx-mid^2 (unsafe-fl* 0.25 (unsafe-flsum (unsafe-flsqr sx00) (unsafe-flsqr sx01)
                                                        (unsafe-flsqr sx10) (unsafe-flsqr sx11))))
        (define sy-mid^2 (unsafe-fl* 0.25 (unsafe-flsum (unsafe-flsqr sy00) (unsafe-flsqr sy01)
                                                        (unsafe-flsqr sy10) (unsafe-flsqr sy11))))
        (define sx-stddev (unsafe-flsqrt (unsafe-fl- sx-mid^2 (unsafe-flsqr sx-mid))))
        (define sy-stddev (unsafe-flsqrt (unsafe-fl- sy-mid^2 (unsafe-flsqr sy-mid))))
        (define x-min (unsafe-fxmax 0 (unsafe-fl->fx (unsafe-flfloor sx-min))))
        (define x-max (unsafe-fxmin w (unsafe-fx+ 1 (unsafe-fl->fx (unsafe-flfloor sx-max)))))
        (define y-min (unsafe-fxmax 0 (unsafe-fl->fx (unsafe-flfloor sy-min))))
        (define y-max (unsafe-fxmin h (unsafe-fx+ 1 (unsafe-fl->fx (unsafe-flfloor sy-max)))))
        (define x-size (unsafe-fx- x-max x-min))
        (define y-size (unsafe-fx- y-max y-min))
        (when (and (x-size . unsafe-fx> . 0) (y-size . unsafe-fx> . 0))
          ;; average the color
          (define-values (r00 g00 b00) (unsafe-flvector-3ref Irgb-vs (unsafe-fx* 3 i00)))
          (define-values (r01 g01 b01) (unsafe-flvector-3ref Irgb-vs (unsafe-fx* 3 i01)))
          (define-values (r10 g10 b10) (unsafe-flvector-3ref Irgb-vs (unsafe-fx* 3 i10)))
          (define-values (r11 g11 b11) (unsafe-flvector-3ref Irgb-vs (unsafe-fx* 3 i11)))
          (define r (unsafe-fl* 0.25 (unsafe-flsum r00 r01 r10 r11)))
          (define g (unsafe-fl* 0.25 (unsafe-flsum g00 g01 g10 g11)))
          (define b (unsafe-fl* 0.25 (unsafe-flsum b00 b01 b10 b11)))
          ;; precalculate the Gaussian kernel for the x direction
          (for ([dx  (in-range x-size)])
            (define x (unsafe-fx+ dx x-min))
            (define d (unsafe-fl/ (unsafe-fl- (unsafe-fl+ 0.5 (unsafe-fx->fl x)) sx-mid) sx-stddev))
            (define kx (unsafe-flexp (unsafe-fl* -0.5 (unsafe-fl* d d))))
            (unsafe-flvector-set! kxs dx kx))
          ;; precalculate the Gaussian kernel for the y direction
          ;; this shouldn't help because it's used only once per y iteration, but it reduces allocs
          ;; within the loop (unsafe-flexp has no bytecode op yet, so its args and return are boxed)
          (for ([dy  (in-range y-size)])
            (define y (unsafe-fx+ dy y-min))
            (define d (unsafe-fl/ (unsafe-fl- (unsafe-fl+ 0.5 (unsafe-fx->fl y)) sy-mid) sy-stddev))
            (define ky (unsafe-flexp (unsafe-fl* -0.5 (unsafe-fl* d d))))
            (unsafe-flvector-set! kys dy ky))
          ;; normalization constant for a 2D Gaussian kernel
          (define c (unsafe-flproduct 2.0 pi sx-stddev sy-stddev))
          ;; cast the approximate shadow volume
          ;; this loop doesn't use the nice unsafe-fl3 macros or define-values, which (currently)
          ;; makes it about 2x faster
          (let y-loop ([dy 0])
            (when (dy . unsafe-fx< . y-size)
              (define ky (unsafe-flvector-ref kys dy))
              (cond [(ky . unsafe-fl> . 0.1)
                     (define a (unsafe-fl/ ky c))
                     (define Ir (unsafe-fl* r a))
                     (define Ig (unsafe-fl* g a))
                     (define Ib (unsafe-fl* b a))
                     (define i (unsafe-fx* 3 (unsafe-fx+ x-min (unsafe-fx* (unsafe-fx+ dy y-min) w))))
                     (let x-loop ([dx 0] [i i])
                       (cond [(dx . unsafe-fx< . x-size)
                              (define kx (unsafe-flvector-ref kxs dx))
                              (when (kx . unsafe-fl> . 0.1)
                                (unsafe-flvector-set!
                                 shadow-vs i (unsafe-fl+ (unsafe-fl* Ir kx)
                                                         (unsafe-flvector-ref shadow-vs i)))
                                (define i1 (unsafe-fx+ i 1))
                                (unsafe-flvector-set!
                                 shadow-vs i1 (unsafe-fl+ (unsafe-fl* Ig kx)
                                                          (unsafe-flvector-ref shadow-vs i1)))
                                (define i2 (unsafe-fx+ i 2))
                                (unsafe-flvector-set!
                                 shadow-vs i2 (unsafe-fl+ (unsafe-fl* Ib kx)
                                                          (unsafe-flvector-ref shadow-vs i2))))
                              (x-loop (unsafe-fx+ 1 dx) (unsafe-fx+ 3 i))]
                             [else
                              (y-loop (unsafe-fx+ 1 dy))]))]
                    [else
                     (y-loop (unsafe-fx+ 1 dy))]))))
        )))
  
  ;; blur the shadow a bit to make up for approximating it with Gaussians
  (values diffracted-fm (flomap-box-blur shadow-fm 1)))

;; ===================================================================================================
;; Pass 2: tracing from a directional viewer

(define (trace-directional-view alpha-fm rgb-fm z-fm normal-fm shadow-fm)
  (match-define (flomap alpha-vs 1 w h) alpha-fm)
  (match-define (list rgb-vs z-vs normal-vs shadow-vs)
    (map flomap-values (list rgb-fm z-fm normal-fm shadow-fm)))
  
  (define w-1 (unsafe-fx- w 1))
  (define h-1 (unsafe-fx- h 1))
  (define x-size (exact->inexact w))
  (define y-size (exact->inexact h))
  (define z-size (flomap-max-value z-fm))
  (define x-mid (* 0.5 x-size))
  (define y-mid (* 0.5 y-size))
  (define opacity-z (/ z-size (transmission-density)))
  
  ;; reflected wall is tilted a bit toward the viewer
  (define wall-tilt-θ (* 1/8 pi))
  (define cos-wall-tilt-θ (cos wall-tilt-θ))
  (define sin-wall-tilt-θ (sin wall-tilt-θ))
  (match-define (list Irr Irg Irb) (reflected-intensity))
  
  ;; max coords of the shadow image
  ;; subtract epsilon to ensure that sx < (w - 1) so that (flfloor sx) < (w - 1) (similarly for sy)
  (define sx-max (- w 1.00001))
  (define sy-max (- h 1.00001))
  ;; material properties
  (define η2 (exact->inexact (refractive-index)))
  (define η1/η2 (/ 1.0 η2))
  (define Ri (ideal-reflectance))
  (define Ti (ideal-transmission))
  
  (define reflected-fm (make-flomap 3 w h))
  (define reflected-vs (flomap-values reflected-fm))
  (define transmitted-fm (make-flomap 3 w h))
  (define transmitted-vs (flomap-values transmitted-fm))
  
  (when (or (Ri . unsafe-fl> . 0.0) (Ti . unsafe-fl> . 0.0))
    (for* ([int-y  (in-range h)] [int-x  (in-range w)])
      (define i (unsafe-fx+ int-x (unsafe-fx* int-y w)))
      (define a (unsafe-flvector-ref alpha-vs i))
      (when (a . unsafe-fl> . 0.0)
        (define j (unsafe-fx* 3 i))
        ;; surface normal
        (define-values (nx ny nz) (unsafe-flvector-3ref normal-vs j))
        ;; cosine of angle between viewer and surface normal
        ;; with gradient inferred from z flomap, this is always > 0.0
        (define cos-i nz)
        ;; transmitted intensity
        (define orig-T (unsafe-transmission-intensity cos-i 1.0 η2))
        (define T (unsafe-fl* Ti orig-T))
        (define R (unsafe-fl* Ri (unsafe-fl- 1.0 orig-T)))
        ;; surface coordinates
        (define x (unsafe-fl+ 0.5 (unsafe-fx->fl int-x)))
        (define y (unsafe-fl+ 0.5 (unsafe-fx->fl int-y)))
        (define z (unsafe-flvector-ref z-vs i))
        
        ;; reflection
        (when (and (Ri . unsafe-fl> . 0.0)
                   (int-x . unsafe-fx> . 0) (int-x . unsafe-fx< . w-1)
                   (int-y . unsafe-fx> . 0) (int-y . unsafe-fx< . h-1))
          (define-values (rx ry rz) (unsafe-reflect-view-ray nx ny nz))
          ;; tilt the wall a little so flat surfaces reflect something
          (define ry* (- (* ry cos-wall-tilt-θ) (* rz sin-wall-tilt-θ)))
          ;(define rz* (+ (* ry sin-wall-tilt-θ) (* rz cos-wall-tilt-θ)))
          ;; distance to the wall
          (define rdist (unsafe-fl/ (unsafe-fl- (- z-size) y) ry*))
          (define sx (unsafe-fl+ x (unsafe-fl* rx rdist)))
          (define sy (unsafe-fl+ y (unsafe-fl* ry rdist)))
          (define sz (unsafe-fl+ z (unsafe-fl* rz rdist)))
          (when (rdist . unsafe-fl>= . 0.0)
            (define cdist (unsafe-fl3dist sx sy sz x-mid y-mid 0.0))
            (define v (unsafe-flsigmoid (unsafe-fl* 0.25 (unsafe-fl- (* 4.5 z-size) cdist))))
            (let-values ([(r g b)  (unsafe-fl3* Irr Irg Irb (* R v))])
              (unsafe-flvector-3set! reflected-vs j r g b))))
        
        ;; transmission (refraction)
        (when (Ti . unsafe-fl> . 0.0)
          (define-values (tx ty tz) (unsafe-transmitted-vector nx ny nz 0.0 0.0 -1.0 1.0 η2))
          ;; sz = z + dist * tz, so dist = (sz - z) / tz
          (define dist (unsafe-fl/ (unsafe-fl- 0.0 z) tz))
          (when (and (dist . unsafe-fl>= . 0.0) (dist . unsafe-fl< . +inf.0))
            ;; Find the color of the point on the shadow that the ray struck
            (define sx (unsafe-flmax 0.0 (unsafe-flmin sx-max (unsafe-fl+ x (unsafe-fl* dist tx)))))
            (define sy (unsafe-flmax 0.0 (unsafe-flmin sy-max (unsafe-fl+ y (unsafe-fl* dist ty)))))
            (define floor-sx (unsafe-flfloor sx))
            (define floor-sy (unsafe-flfloor sy))
            (define bx (unsafe-fl->fx floor-sx))
            (define by (unsafe-fl->fx floor-sy))
            ;; Bilinearly interpolate the four colors nearest the point on the shadow
            (define 1-αx (unsafe-fl- sx floor-sx))
            (define 1-αy (unsafe-fl- sy floor-sy))
            (define αx (unsafe-fl- 1.0 1-αx))
            (define αy (unsafe-fl- 1.0 1-αy))
            ;; upper-left weighted values
            (define j1 (unsafe-fx* 3 (unsafe-fx+ bx (unsafe-fx* by w))))
            (define-values (r1 g1 b1) (unsafe-flvector-3ref shadow-vs j1))
            (define-values (sr1 sg1 sb1) (unsafe-fl3* r1 g1 b1 (unsafe-fl* αx αy)))
            ;; upper-right weighted values
            (define j2 (unsafe-fx+ j1 3))
            (define-values (r2 g2 b2) (unsafe-flvector-3ref shadow-vs j2))
            (define-values (sr2 sg2 sb2) (unsafe-fl3* r2 g2 b2 (unsafe-fl* 1-αx αy)))
            ;; lower-left weighted values
            (define j3 (unsafe-fx+ j1 (unsafe-fx* 3 w)))
            (define-values (r3 g3 b3) (unsafe-flvector-3ref shadow-vs j3))
            (define-values (sr3 sg3 sb3) (unsafe-fl3* r3 g3 b3 (unsafe-fl* αx 1-αy)))
            ;; lower-right weighted values
            (define j4 (unsafe-fx+ j3 3))
            (define-values (r4 g4 b4) (unsafe-flvector-3ref shadow-vs j4))
            (define-values (sr4 sg4 sb4) (unsafe-fl3* r4 g4 b4 (unsafe-fl* 1-αx 1-αy)))
            ;; final interpolated shadow color
            (define-values (sr sg sb)
              (values (unsafe-flsum sr1 sr2 sr3 sr4)
                      (unsafe-flsum sg1 sg2 sg3 sg4)
                      (unsafe-flsum sb1 sb2 sb3 sb4)))
            ;; normalized distance to the surface
            (define norm-dist (unsafe-fl/ dist opacity-z))
            ;; intensities of each r g b by the time the light emerges from the surface
            (define-values (r g b)
              ;; colors represent absorption rates
              (let-values ([(r g b)  (unsafe-flvector-3ref rgb-vs j)])
                (values (unsafe-flproduct T sr (unsafe-transmit r norm-dist))
                        (unsafe-flproduct T sg (unsafe-transmit g norm-dist))
                        (unsafe-flproduct T sb (unsafe-transmit b norm-dist)))))
            (unsafe-flvector-3set! transmitted-vs j r g b))))))
  
  ;; blur to cut down on sparklies (poor man's supersampling)
  (values reflected-fm
          (flomap-blur transmitted-fm ideal-transmission-blur)))

;; ===================================================================================================
;; Full rendering

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

(define (deep-flomap-render dfm [background-fm #f])
  (define-values (w h) (deep-flomap-size dfm))
  (define argb-fm (flomap-divide-alpha (deep-flomap-argb dfm)))
  (define alpha-fm (flomap-ref-component argb-fm 0))
  (define rgb-fm (flomap-drop-components argb-fm 1))
  (define z-fm (fmmax 0.0 (deep-flomap-z dfm)))
  (define normal-fm (flomap-gradient-normal z-fm))
  ;(printf "~v~n" (flomap->bitmap (fm* 0.5 (fm+ 1.0 normal-fm))))
  (define bg-fm (if background-fm (prep-background background-fm) #f))
  
  ;; pass 1: trace from the light source
  (define-values (diffracted-fm raw-shadow-fm)
    (trace-directional-light alpha-fm rgb-fm z-fm normal-fm))
  #;
  (printf "diffracted: ~v~nraw shadow: ~v~n"
          (flomap->bitmap diffracted-fm #;(flomap-normalize diffracted-fm))
          (flomap->bitmap raw-shadow-fm #;(flomap-normalize raw-shadow-fm)))
  
  ;; blur the shadow to simulate internal scatter
  (define σ (* (min w h) (shadow-blur)))
  (define shadow-fm
    (cond [bg-fm
           ;; two Gaussian blurs by half-σ is equivalent to one Gaussian blur by σ
           (define half-σ (* (/ 1 (sqrt 2)) σ))
           (let* ([fm  (flomap-blur raw-shadow-fm half-σ)]
                  [fm  (fm* fm bg-fm)]
                  [fm  (flomap-blur fm half-σ)])
             fm)]
          [else
           (flomap-blur raw-shadow-fm σ)]))
  ;(printf "~v~n" (flomap->bitmap (flomap-normalize scattered-shadow-fm)))
  
  ;; pass 2: trace from the viewer
  (define-values (reflected-fm transmitted-fm)
    (trace-directional-view alpha-fm rgb-fm z-fm normal-fm shadow-fm))
  #;
  (printf "reflected: ~v~ntransmitted: ~v~n"
          (flomap->bitmap (flomap-normalize reflected-fm))
          (flomap->bitmap (flomap-normalize transmitted-fm)))
  
  ;; add all the light together, convert to premultiplied-alpha flomap
  (let* ([fm  (fm+ (fm+ diffracted-fm transmitted-fm) reflected-fm)]
         [fm  (flomap-append-components alpha-fm fm)]
         [fm  (flomap-multiply-alpha fm)]
         )
    fm))
