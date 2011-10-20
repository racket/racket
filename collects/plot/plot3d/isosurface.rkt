#lang racket/base

(require racket/class racket/match racket/list racket/flonum racket/contract
         "../common/marching-cubes.rkt"
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/contract.rkt"
         "../common/contract-doc.rkt"
         "../common/draw.rkt"
         "../common/legend.rkt"
         "../common/sample.rkt"
         "../common/parameters.rkt"
         "../common/renderer.rkt")

(provide isosurface3d isosurfaces3d polar3d)

;; ===================================================================================================
;; Surfaces of constant value (isosurfaces)

(define (scale-normalized-polys polys xa xb ya yb za zb)
  (map (λ (poly) (scale-normalized-poly poly xa xb ya yb za zb))
       polys))

(define ((isosurface3d-render-proc f d samples color line-color line-width line-style alpha label)
         area)
  (define-values (x-min x-max y-min y-max z-min z-max) (send area get-bounds))
  (match-define (list xs ys zs dsss)
    (f x-min x-max (animated-samples samples)
       y-min y-max (animated-samples samples)
       z-min z-max (animated-samples samples)))
  
  (send area put-alpha alpha)
  (send area put-brush color 'solid)
  (send area put-pen line-color line-width line-style)
  (for ([za  (in-list zs)]
        [zb  (in-list (rest zs))]
        [dss0  (in-vector dsss)]
        [dss1  (in-vector dsss 1)]
        #:when #t
        [ya  (in-list ys)]
        [yb  (in-list (rest ys))]
        [ds00  (in-vector dss0)]
        [ds01  (in-vector dss0 1)]
        [ds10  (in-vector dss1)]
        [ds11  (in-vector dss1 1)]
        #:when #t
        [xa  (in-list xs)]
        [xb  (in-list (rest xs))]
        [d1  (in-vector ds00)]
        [d2  (in-vector ds00 1)]
        [d3  (in-vector ds01 1)]
        [d4  (in-vector ds01)]
        [d5  (in-vector ds10)]
        [d6  (in-vector ds10 1)]
        [d7  (in-vector ds11 1)]
        [d8  (in-vector ds11)])
    (define polys
      (heights->cube-polys
       (exact->inexact d)
       (exact->inexact d1) (exact->inexact d2) (exact->inexact d3) (exact->inexact d4)
       (exact->inexact d5) (exact->inexact d6) (exact->inexact d7) (exact->inexact d8)))
    
    (when (not (empty? polys))
      (send area put-polygons
            (scale-normalized-polys polys xa xb ya yb za zb)
            (center-coord (list (vector xa ya za)
                                (vector xb yb zb))))))
  
  (cond [label  (rectangle-legend-entry
                 label color 'solid line-color line-width line-style)]
        [else  empty]))

(defproc (isosurface3d [f (real? real? real? . -> . real?)] [d real?]
                       [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
                       [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
                       [z-min (or/c real? #f) #f] [z-max (or/c real? #f) #f]
                       [#:samples samples (and/c exact-integer? (>=/c 2)) (plot3d-samples)]
                       [#:color color plot-color/c (surface-color)]
                       [#:line-color line-color plot-color/c (surface-line-color)]
                       [#:line-width line-width (>=/c 0) (surface-line-width)]
                       [#:line-style line-style plot-pen-style/c (surface-line-style)]
                       [#:alpha alpha (real-in 0 1) (surface-alpha)]
                       [#:label label (or/c string? #f) #f]
                       ) renderer3d?
  (define g (3d-function->sampler f))
  (renderer3d (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) #f default-ticks-fun
              (isosurface3d-render-proc g d samples color line-color line-width line-style alpha
                                        label)))

;; ===================================================================================================
;; Nested isosurfaces

(define ((isosurfaces3d-render-proc
          f rd-min rd-max levels samples colors line-colors line-widths line-styles alphas label)
         area)
  (define-values (x-min x-max y-min y-max z-min z-max) (send area get-bounds))
  (match-define (list xs ys zs dsss)
    (f x-min x-max (animated-samples samples)
       y-min y-max (animated-samples samples)
       z-min z-max (animated-samples samples)))
  
  (define-values (fd-min fd-max)
    (let ([regular-ds  (filter regular? (3d-sample->list dsss))])
      (values (if (empty? regular-ds) #f (apply min* regular-ds))
              (if (empty? regular-ds) #f (apply max* regular-ds)))))
  
  (define d-min (if rd-min rd-min fd-min))
  (define d-max (if rd-max rd-max fd-max))
  
  (cond
    [(not (and d-min d-max))  empty]
    [else
     (define ds (linear-seq d-min d-max levels #:start? (and rd-min #t) #:end? (and rd-max #t)))
     
     (for ([d           (in-list ds)]
           [color       (in-cycle (maybe-apply/list colors ds))]
           [line-color  (in-cycle (maybe-apply/list line-colors ds))]
           [line-width  (in-cycle (maybe-apply/list line-widths ds))]
           [line-style  (in-cycle (maybe-apply/list line-styles ds))]
           [alpha       (in-cycle (maybe-apply/list alphas ds))])
       (send area put-alpha alpha)
       (send area put-brush color 'solid)
       (send area put-pen line-color line-width line-style)
       (for ([za  (in-list zs)]
             [zb  (in-list (rest zs))]
             [dss0  (in-vector dsss)]
             [dss1  (in-vector dsss 1)]
             #:when #t
             [ya  (in-list ys)]
             [yb  (in-list (rest ys))]
             [ds00  (in-vector dss0)]
             [ds01  (in-vector dss0 1)]
             [ds10  (in-vector dss1)]
             [ds11  (in-vector dss1 1)]
             #:when #t
             [xa  (in-list xs)]
             [xb  (in-list (rest xs))]
             [d1  (in-vector ds00)]
             [d2  (in-vector ds00 1)]
             [d3  (in-vector ds01 1)]
             [d4  (in-vector ds01)]
             [d5  (in-vector ds10)]
             [d6  (in-vector ds10 1)]
             [d7  (in-vector ds11 1)]
             [d8  (in-vector ds11)])
         (define polys
           (heights->cube-polys
            (exact->inexact d)
            (exact->inexact d1) (exact->inexact d2) (exact->inexact d3) (exact->inexact d4)
            (exact->inexact d5) (exact->inexact d6) (exact->inexact d7) (exact->inexact d8)))
         
         (when (not (empty? polys))
           (send area put-polygons
                 (scale-normalized-polys polys xa xb ya yb za zb)
                 (center-coord (list (vector xa ya za) (vector xb yb zb)))))))
     
     (cond
       [label  (rectangle-legend-entries
                label ds colors '(solid) line-colors line-widths line-styles)]
       [else  empty])]))

(defproc (isosurfaces3d [f (real? real? real? . -> . real?)]
                        [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
                        [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
                        [z-min (or/c real? #f) #f] [z-max (or/c real? #f) #f]
                        [#:d-min d-min (or/c real? #f) #f] [#:d-max d-max (or/c real? #f) #f]
                        [#:levels levels exact-positive-integer? (isosurface-levels)]
                        [#:samples samples (and/c exact-integer? (>=/c 2)) (plot3d-samples)]
                        [#:colors colors plot-colors/c (isosurface-colors)]
                        [#:line-colors line-colors plot-colors/c (isosurface-line-colors)]
                        [#:line-widths line-widths pen-widths/c (isosurface-line-widths)]
                        [#:line-styles line-styles plot-pen-styles/c (isosurface-line-styles)]
                        [#:alphas alphas alphas/c (isosurface-alphas)]
                        [#:label label (or/c string? #f) #f]
                        ) renderer3d?
  (define g (3d-function->sampler f))
  (renderer3d (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) #f default-ticks-fun
              (isosurfaces3d-render-proc g d-min d-max levels samples colors
                                         line-colors line-widths line-styles alphas
                                         label)))

;; ===================================================================================================

(define ((polar3d-render-proc f g samples color line-color line-width line-style alpha label) area)
  (define-values (x-min x-max y-min y-max z-min z-max) (send area get-bounds))
  (match-define (list xs ys zs dsss)
    (g x-min x-max (animated-samples samples)
       y-min y-max (animated-samples samples)
       z-min z-max (animated-samples samples)))
  
  (send area put-alpha alpha)
  (send area put-brush color 'solid)
  (send area put-pen line-color line-width line-style)
  (for ([za  (in-list zs)]
        [zb  (in-list (rest zs))]
        [dss0  (in-vector dsss)]
        [dss1  (in-vector dsss 1)]
        #:when #t
        [ya  (in-list ys)]
        [yb  (in-list (rest ys))]
        [ds00  (in-vector dss0)]
        [ds01  (in-vector dss0 1)]
        [ds10  (in-vector dss1)]
        [ds11  (in-vector dss1 1)]
        #:when #t
        [xa  (in-list xs)]
        [xb  (in-list (rest xs))]
        [d1  (in-vector ds00)]
        [d2  (in-vector ds00 1)]
        [d3  (in-vector ds01 1)]
        [d4  (in-vector ds01)]
        [d5  (in-vector ds10)]
        [d6  (in-vector ds10 1)]
        [d7  (in-vector ds11 1)]
        [d8  (in-vector ds11)])
    (define (draw-cube xa xb ya yb za zb d1 d2 d3 d4 d5 d6 d7 d8)
      (define polys
        (heights->cube-polys
         0.0
         (exact->inexact d1) (exact->inexact d2) (exact->inexact d3) (exact->inexact d4)
         (exact->inexact d5) (exact->inexact d6) (exact->inexact d7) (exact->inexact d8)))
      (when (not (empty? polys))
        (send area put-polygons
              (scale-normalized-polys polys xa xb ya yb za zb)
              (center-coord (list (vector xa ya za)
                                  (vector xb yb zb))))))
    (cond [(and (xb . > . 0) (ya . < . 0) (yb . > . 0))
           (let* ([yb  -0.00001]
                  [d3  (f xb yb za)]
                  [d4  (f xa yb za)]
                  [d7  (f xb yb zb)]
                  [d8  (f xa yb zb)])
             (draw-cube xa xb ya yb za zb d1 d2 d3 d4 d5 d6 d7 d8))
           (let* ([ya  0.00001]
                  [d1  (f xa ya za)]
                  [d2  (f xb ya za)]
                  [d5  (f xa ya zb)]
                  [d6  (f xb ya zb)])
             (draw-cube xa xb ya yb za zb d1 d2 d3 d4 d5 d6 d7 d8))]
          [else
           (draw-cube xa xb ya yb za zb d1 d2 d3 d4 d5 d6 d7 d8)]))
  
  (cond [label  (rectangle-legend-entry
                 label color 'solid line-color line-width line-style)]
        [else  empty]))

(defproc (polar3d [f (real? real? . -> . real?)]
                  [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                  [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                  [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
                  [#:samples samples (and/c exact-integer? (>=/c 2)) (plot3d-samples)]
                  [#:color color plot-color/c (surface-color)]
                  [#:line-color line-color plot-color/c (surface-line-color)]
                  [#:line-width line-width (>=/c 0) (surface-line-width)]
                  [#:line-style line-style plot-pen-style/c (surface-line-style)]
                  [#:alpha alpha (real-in 0 1) (surface-alpha)]
                  [#:label label (or/c string? #f) #f]
                  ) renderer3d?
  (define rvs (filter vregular? (sample-2d-polar f 0 2pi (* 2 samples) -1/2pi 1/2pi samples)))
  (cond [(empty? rvs)  (renderer3d #f #f #f #f)]
        [else
         (match-define (list (vector rxs rys rzs) ...) rvs)
         (let ([x-min  (if x-min x-min (apply min* rxs))]
               [x-max  (if x-max x-max (apply max* rxs))]
               [y-min  (if y-min y-min (apply min* rys))]
               [y-max  (if y-max y-max (apply max* rys))]
               [z-min  (if z-min z-min (apply min* rzs))]
               [z-max  (if z-max z-max (apply max* rzs))])
           (define new-f (2d-polar->3d-function f))
           (define g (3d-function->sampler new-f))
           (renderer3d (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) #f
                       default-ticks-fun
                       (polar3d-render-proc new-f g samples color
                                            line-color line-width line-style alpha label)))]))
