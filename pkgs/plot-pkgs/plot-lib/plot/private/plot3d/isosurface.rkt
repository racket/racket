#lang racket/base

(require racket/class racket/match racket/list racket/flonum racket/contract racket/math
         unstable/latent-contract/defthing
         plot/utils
         "../common/utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Surfaces of constant value (isosurfaces)

(define ((isosurface3d-render-proc
          f d samples color style line-color line-width line-style alpha label)
         area)
  (match-define (vector x-ivl y-ivl z-ivl) (send area get-bounds-rect))
  (match-define (ivl x-min x-max) x-ivl)
  (match-define (ivl y-min y-max) y-ivl)
  (match-define (ivl z-min z-max) z-ivl)
  (define num (animated-samples samples))
  (define sample (f (vector x-ivl y-ivl z-ivl) (vector num num num)))
  (match-define (3d-sample xs ys zs dsss d-min d-max) sample)
  
  (send area put-alpha alpha)
  (send area put-brush color style)
  (send area put-pen line-color line-width line-style)
  (for-3d-sample
   (xa xb ya yb za zb d1 d2 d3 d4 d5 d6 d7 d8) sample
   (for ([vs  (in-list (heights->cube-polys xa xb ya yb za zb d d1 d2 d3 d4 d5 d6 d7 d8))])
     (send area put-polygon vs)))
  
  (cond [label  (rectangle-legend-entry
                 label color style line-color line-width line-style)]
        [else  empty]))

(defproc (isosurface3d [f (real? real? real? . -> . real?)] [d rational?]
                       [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
                       [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
                       [z-min (or/c rational? #f) #f] [z-max (or/c rational? #f) #f]
                       [#:samples samples (and/c exact-integer? (>=/c 2)) (plot3d-samples)]
                       [#:color color plot-color/c (surface-color)]
                       [#:style style plot-brush-style/c (surface-style)]
                       [#:line-color line-color plot-color/c (surface-line-color)]
                       [#:line-width line-width (>=/c 0) (surface-line-width)]
                       [#:line-style line-style plot-pen-style/c (surface-line-style)]
                       [#:alpha alpha (real-in 0 1) (surface-alpha)]
                       [#:label label (or/c string? #f) #f]
                       ) renderer3d?
  (define x-ivl (ivl x-min x-max))
  (define y-ivl (ivl y-min y-max))
  (define z-ivl (ivl z-min z-max))
  (define g (3d-function->sampler f (vector x-ivl y-ivl z-ivl)))
  (renderer3d (vector x-ivl y-ivl z-ivl) #f default-ticks-fun
              (isosurface3d-render-proc
               g d samples color style line-color line-width line-style alpha label)))

;; ===================================================================================================
;; Nested isosurfaces

(define ((isosurfaces3d-render-proc f rd-min rd-max levels samples colors styles
                                    line-colors line-widths line-styles alphas label)
         area)
  (match-define (vector x-ivl y-ivl z-ivl) (send area get-bounds-rect))
  (match-define (ivl x-min x-max) x-ivl)
  (match-define (ivl y-min y-max) y-ivl)
  (match-define (ivl z-min z-max) z-ivl)
  (define num (animated-samples samples))
  (define sample (f (vector x-ivl y-ivl z-ivl) (vector num num num)))
  (match-define (3d-sample xs ys zs dsss fd-min fd-max) sample)
  
  (define d-min (if rd-min rd-min fd-min))
  (define d-max (if rd-max rd-max fd-max))
  
  (match-define (list (tick ds _ labels) ...)
    (cond [(and d-min d-max)  (contour-ticks (plot-d-ticks) d-min d-max levels #f)]
          [else  empty]))
  
  (let* ([colors  (maybe-apply colors ds)]
         [styles  (maybe-apply styles ds)]
         [alphas  (maybe-apply alphas ds)]
         [line-colors  (maybe-apply line-colors ds)]
         [line-widths  (maybe-apply line-widths ds)]
         [line-styles  (maybe-apply line-styles ds)])
    (for ([d      (in-list ds)]
          [color  (in-cycle* colors)]
          [style  (in-cycle* styles)]
          [alpha  (in-cycle* alphas)]
          [line-color  (in-cycle* line-colors)]
          [line-width  (in-cycle* line-widths)]
          [line-style  (in-cycle* line-styles)])
      (send area put-alpha alpha)
      (send area put-brush color style)
      (send area put-pen line-color line-width line-style)
      (for-3d-sample
       (xa xb ya yb za zb d1 d2 d3 d4 d5 d6 d7 d8) sample
       (for ([vs  (in-list (heights->cube-polys xa xb ya yb za zb d d1 d2 d3 d4 d5 d6 d7 d8))])
         (send area put-polygon vs)))))
  
  (cond
    [(and label (not (empty? ds)))  (rectangle-legend-entries
                                     label ds colors styles line-colors line-widths line-styles)]
    [else  empty]))

(defproc (isosurfaces3d
          [f (real? real? real? . -> . real?)]
          [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
          [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
          [z-min (or/c rational? #f) #f] [z-max (or/c rational? #f) #f]
          [#:d-min d-min (or/c rational? #f) #f] [#:d-max d-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (plot3d-samples)]
          [#:levels levels (or/c 'auto exact-positive-integer? (listof real?)) (isosurface-levels)]
          [#:colors colors (plot-colors/c (listof real?)) (isosurface-colors)]
          [#:styles styles (plot-brush-styles/c (listof real?)) (isosurface-styles)]
          [#:line-colors line-colors (plot-colors/c (listof real?)) (isosurface-line-colors)]
          [#:line-widths line-widths (pen-widths/c (listof real?)) (isosurface-line-widths)]
          [#:line-styles line-styles (plot-pen-styles/c (listof real?)) (isosurface-line-styles)]
          [#:alphas alphas (alphas/c (listof real?)) (isosurface-alphas)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (define x-ivl (ivl x-min x-max))
  (define y-ivl (ivl y-min y-max))
  (define z-ivl (ivl z-min z-max))
  (define g (3d-function->sampler f (vector x-ivl y-ivl z-ivl)))
  (renderer3d (vector x-ivl y-ivl z-ivl) #f default-ticks-fun
              (isosurfaces3d-render-proc g d-min d-max levels samples colors styles
                                         line-colors line-widths line-styles alphas
                                         label)))

;; ===================================================================================================

(define ((polar3d-render-proc f g samples color style line-color line-width line-style alpha label)
         area)
  (match-define (vector x-ivl y-ivl z-ivl) (send area get-bounds-rect))
  (match-define (ivl x-min x-max) x-ivl)
  (match-define (ivl y-min y-max) y-ivl)
  (match-define (ivl z-min z-max) z-ivl)
  (define num (animated-samples samples))
  (define sample (g (vector x-ivl y-ivl z-ivl) (vector num num num)))
  (match-define (3d-sample xs ys zs dsss d-min d-max) sample)
  
  (define (draw-cube xa xb ya yb za zb d d1 d2 d3 d4 d5 d6 d7 d8)
    (for ([vs  (in-list (heights->cube-polys xa xb ya yb za zb d d1 d2 d3 d4 d5 d6 d7 d8))])
      (send area put-polygon vs)))
  
  (send area put-alpha alpha)
  (send area put-brush color style)
  (send area put-pen line-color line-width line-style)
  (define d 0)
  (for-3d-sample
   (xa xb ya yb za zb d1 d2 d3 d4 d5 d6 d7 d8) sample
   (cond [(and (xb . > . 0) (ya . < . 0) (yb . > . 0))
          (let* ([yb  -0.00001]
                 [d3  (f xb yb za)]
                 [d4  (f xa yb za)]
                 [d7  (f xb yb zb)]
                 [d8  (f xa yb zb)])
            (draw-cube xa xb ya yb za zb d d1 d2 d3 d4 d5 d6 d7 d8))
          (let* ([ya  0.00001]
                 [d1  (f xa ya za)]
                 [d2  (f xb ya za)]
                 [d5  (f xa ya zb)]
                 [d6  (f xb ya zb)])
            (draw-cube xa xb ya yb za zb d d1 d2 d3 d4 d5 d6 d7 d8))]
         [else
          (draw-cube xa xb ya yb za zb d d1 d2 d3 d4 d5 d6 d7 d8)]))
  
  (cond [label  (rectangle-legend-entry
                 label color style line-color line-width line-style)]
        [else  empty]))

(define 2pi (* 2 pi))

(define (flmodulo x y)
  (fl- x (fl* y (flfloor (fl/ x y)))))

(define ((2d-polar->3d-function f) x y z)
  (let ([x  (exact->inexact x)]
        [y  (exact->inexact y)]
        [z  (exact->inexact z)])
    (define-values (θ ρ)
      (cond [(and (fl= x 0.0) (fl= y 0.0))  (values 0.0 0.0)]
            [else  (values (flmodulo (atan y x) 2pi)
                           (flatan (fl/ z (fldistance x y))))]))
    (fl- (exact->inexact (f θ ρ)) (fldistance x y z))))

(defproc (polar3d
          [f (real? real? . -> . real?)]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:z-min z-min (or/c rational? #f) #f] [#:z-max z-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (plot3d-samples)]
          [#:color color plot-color/c (surface-color)]
          [#:style style plot-brush-style/c (surface-style)]
          [#:line-color line-color plot-color/c (surface-line-color)]
          [#:line-width line-width (>=/c 0) (surface-line-width)]
          [#:line-style line-style plot-pen-style/c (surface-line-style)]
          [#:alpha alpha (real-in 0 1) (surface-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (define vs (for*/list ([θ  (in-list (linear-seq 0.0 2pi (* 4 samples)))]
                         [ρ  (in-list (linear-seq (* -1/2 pi) (* 1/2 pi) (* 2 samples)))])
               (3d-polar->3d-cartesian θ ρ (f θ ρ))))
  (define rvs (filter vrational? vs))
  (cond [(empty? rvs)  (renderer3d #f #f #f #f)]
        [else
         (match-define (list (vector rxs rys rzs) ...) rvs)
         (let ([x-min  (if x-min x-min (apply min* rxs))]
               [x-max  (if x-max x-max (apply max* rxs))]
               [y-min  (if y-min y-min (apply min* rys))]
               [y-max  (if y-max y-max (apply max* rys))]
               [z-min  (if z-min z-min (apply min* rzs))]
               [z-max  (if z-max z-max (apply max* rzs))])
           (define x-ivl (ivl x-min x-max))
           (define y-ivl (ivl y-min y-max))
           (define z-ivl (ivl z-min z-max))
           (define new-f (2d-polar->3d-function f))
           (define g (3d-function->sampler new-f (vector x-ivl y-ivl z-ivl)))
           (renderer3d (vector x-ivl y-ivl z-ivl) #f
                       default-ticks-fun
                       (polar3d-render-proc new-f g samples color style
                                            line-color line-width line-style alpha label)))]))
