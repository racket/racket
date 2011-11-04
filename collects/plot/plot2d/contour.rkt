#lang racket/base

;; Renderers for contour lines and contour intervals

(require racket/contract racket/class racket/match racket/list racket/flonum racket/vector racket/math
         plot/utils
         "../common/contract-doc.rkt")

(provide (all-defined-out)
         (rename-out [contours isolines]
                     [contour-intervals isoline-intervals]))

;; ===================================================================================================
;; One contour line

(define ((isoline-render-proc g z samples color width style alpha label) area)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max)) (send area get-bounds-rect))
  (match-define (2d-sample xs ys zss z-min z-max)
    (g x-min x-max samples y-min y-max samples))
  
  (when (<= z-min z z-max)
    (send area put-alpha alpha)
    (send area put-pen color width style)
    (for ([ya  (in-list ys)]
          [yb  (in-list (rest ys))]
          [zs0  (in-vector zss)]
          [zs1  (in-vector zss 1)]
          #:when #t
          [xa  (in-list xs)]
          [xb  (in-list (rest xs))]
          [z1  (in-vector zs0)]
          [z2  (in-vector zs0 1)]
          [z3  (in-vector zs1 1)]
          [z4  (in-vector zs1)])
      (for/list ([line  (in-list (heights->lines xa xb ya yb z z1 z2 z3 z4))])
        (send/apply area put-line (map (λ (v) (vector-take v 2)) line)))))
  
  (cond [label  (line-legend-entry label color width style)]
        [else   empty]))

(defproc (isoline
          [f (real? real? . -> . real?)] [z real?]
          [x-min (or/c regular-real? #f) #f]
          [x-max (or/c regular-real? #f) #f]
          [y-min (or/c regular-real? #f) #f]
          [y-max (or/c regular-real? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (contour-samples)]
          [#:color color plot-color/c (line-color)]
          [#:width width (>=/c 0) (line-width)]
          [#:style style plot-pen-style/c (line-style)]
          [#:alpha alpha (real-in 0 1) (line-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (define g (2d-function->sampler f))
  (renderer2d (vector (ivl x-min x-max) (ivl y-min y-max)) #f default-ticks-fun
              (isoline-render-proc g z samples color width style alpha label)))

;; ===================================================================================================
;; Contour lines

(define ((contours-render-proc g levels samples colors widths styles alphas label) area)
  (let/ec return
    (match-define (vector (ivl x-min x-max) (ivl y-min y-max)) (send area get-bounds-rect))
    (match-define (2d-sample xs ys zss z-min z-max)
      (g x-min x-max samples y-min y-max samples))
    
    (match-define (list (tick zs _ labels) ...) (contour-ticks z-min z-max levels #f))
    
    (define cs (maybe-apply/list colors zs))
    (define ws (maybe-apply/list widths zs))
    (define ss (maybe-apply/list styles zs))
    (define as (maybe-apply/list alphas zs))
    
    (for ([z      (in-list zs)]
          [color  (in-cycle cs)]
          [width  (in-cycle ws)]
          [style  (in-cycle ss)]
          [alpha  (in-cycle as)])
      (send area put-alpha alpha)
      (send area put-pen color width style)
      (for ([ya  (in-list ys)]
            [yb  (in-list (rest ys))]
            [zs0  (in-vector zss)]
            [zs1  (in-vector zss 1)]
            #:when #t
            [xa  (in-list xs)]
            [xb  (in-list (rest xs))]
            [z1  (in-vector zs0)]
            [z2  (in-vector zs0 1)]
            [z3  (in-vector zs1 1)]
            [z4  (in-vector zs1)])
        (for/list ([line  (in-list (heights->lines xa xb ya yb z z1 z2 z3 z4))])
          (match-define (list v1 v2) (map (λ (v) (vector-take v 2)) line))
          (send area put-line v1 v2))))
    
    (cond [label  (line-legend-entries label zs labels colors widths styles)]
          [else   empty])))

(defproc (contours
          [f (real? real? . -> . real?)]
          [x-min (or/c regular-real? #f) #f]
          [x-max (or/c regular-real? #f) #f]
          [y-min (or/c regular-real? #f) #f]
          [y-max (or/c regular-real? #f) #f]
          [#:levels levels (or/c 'auto exact-positive-integer? (listof real?)) (contour-levels)]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (contour-samples)]
          [#:colors colors plot-colors/c (contour-colors)]
          [#:widths widths pen-widths/c (contour-widths)]
          [#:styles styles plot-pen-styles/c (contour-styles)]
          [#:alphas alphas alphas/c (contour-alphas)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (define g (2d-function->sampler f))
  (renderer2d (vector (ivl x-min x-max) (ivl y-min y-max)) #f default-ticks-fun
              (contours-render-proc g levels samples colors widths styles alphas label)))

;; ===================================================================================================
;; Contour intervals

(define ((contour-intervals-render-proc
          g levels samples colors styles contour-colors contour-widths contour-styles alphas label)
         area)
  (let/ec return
    (match-define (vector (ivl x-min x-max) (ivl y-min y-max)) (send area get-bounds-rect))
    (match-define (2d-sample xs ys zss z-min z-max)
      (g x-min x-max samples y-min y-max samples))
    
    (match-define (list (tick zs _ labels) ...) (contour-ticks z-min z-max levels #t))
    
    (define cs (map ->brush-color (maybe-apply/list colors zs)))
    (define fss (map ->brush-style (maybe-apply/list styles zs)))
    (define pss (map (λ (fill-style) (if (eq? fill-style 'solid) 'solid 'transparent)) fss))
    (define as (maybe-apply/list alphas zs))
    
    (for ([za     (in-list zs)]
          [zb     (in-list (rest zs))]
          [color  (in-cycle cs)]
          [fill-style       (in-cycle fss)]
          [poly-line-style  (in-cycle pss)]
          [alpha  (in-cycle as)])
      (define polys
        (append*
         (for/list ([ya  (in-list ys)]
                    [yb  (in-list (rest ys))]
                    [zs0  (in-vector zss)]
                    [zs1  (in-vector zss 1)]
                    #:when #t
                    [xa  (in-list xs)]
                    [xb  (in-list (rest xs))]
                    [z1  (in-vector zs0)]
                    [z2  (in-vector zs0 1)]
                    [z3  (in-vector zs1 1)]
                    [z4  (in-vector zs1)])
           (for/list ([poly  (in-list (heights->polys xa xb ya yb za zb z1 z2 z3 z4))])
             (map (λ (v) (vector-take v 2)) poly)))))
      
      (define (draw-polys)
        (for ([poly  (in-list polys)])
          (send area put-polygon poly)))
      
      (cond [(= alpha 1)
             (send area put-pen color 1 poly-line-style)
             (send area put-brush color fill-style)
             (send area put-alpha 1)
             (draw-polys)]
            [else
             ;; draw the outlines with reduced alpha first
             (send area put-pen color 1 poly-line-style)
             (send area put-brush color 'transparent)
             (send area put-alpha (alpha-expt alpha 1/8))
             (draw-polys)
             ;; now draw the centers
             (send area put-pen color 1 'transparent)
             (send area put-brush color fill-style)
             (send area put-alpha alpha)
             (draw-polys)]))
    
    ((contours-render-proc g levels samples contour-colors contour-widths contour-styles alphas #f)
     area)
    
    (cond [label  (contour-intervals-legend-entries
                   label zs labels cs fss cs '(1) pss contour-colors contour-widths contour-styles)]
          [else   empty])))

(defproc (contour-intervals
          [f (real? real? . -> . real?)]
          [x-min (or/c regular-real? #f) #f]
          [x-max (or/c regular-real? #f) #f]
          [y-min (or/c regular-real? #f) #f]
          [y-max (or/c regular-real? #f) #f]
          [#:levels levels (or/c 'auto exact-positive-integer? (listof real?)) (contour-levels)]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (contour-samples)]
          [#:colors colors plot-colors/c (contour-interval-colors)]
          [#:styles styles plot-brush-styles/c (contour-interval-styles)]
          [#:contour-colors contour-colors plot-colors/c (contour-colors)]
          [#:contour-widths contour-widths pen-widths/c (contour-widths)]
          [#:contour-styles contour-styles plot-pen-styles/c (contour-styles)]
          [#:alphas alphas alphas/c (contour-interval-alphas)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (define g (2d-function->sampler f))
  (renderer2d (vector (ivl x-min x-max) (ivl y-min y-max)) #f default-ticks-fun
              (contour-intervals-render-proc g levels samples colors styles
                                             contour-colors contour-widths contour-styles
                                             alphas label)))
