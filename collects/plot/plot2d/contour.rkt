#lang racket/base

;; Renderers for contour lines and contour intervals

(require racket/contract racket/class racket/match racket/list racket/flonum racket/vector
         "../common/math.rkt"
         "../common/draw.rkt"
         "../common/marching-squares.rkt"
         "../common/contract.rkt"
         "../common/contract-doc.rkt"
         "../common/legend.rkt"
         "../common/sample.rkt"
         "../common/parameters.rkt"
         "../common/ticks.rkt"
         "../common/vector.rkt"
         "../common/format.rkt"
         "../common/renderer.rkt")

(provide contours contour-intervals)

;; ===================================================================================================
;; Contour lines

(define ((contours-render-proc f levels samples colors widths styles alphas label) area)
  (let/ec return
    (define-values (x-min x-max y-min y-max) (send area get-bounds))
    (match-define (list xs ys zss) (f x-min x-max samples y-min y-max samples))
    
    (define-values (z-min z-max)
      (let ([zs  (filter regular? (2d-sample->list zss))])
        (when (empty? zs) (return empty))
        (values (apply min* zs) (apply max* zs))))
    
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
      (send area set-alpha alpha)
      (send area set-pen color width style)
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
        (for/list ([line  (in-list (heights->lines (exact->inexact z)
                                                   (exact->inexact z1) (exact->inexact z2)
                                                   (exact->inexact z3) (exact->inexact z4)))])
          (match-define (vector x1 y1 x2 y2) (scale-normalized-line line xa xb ya yb))
          (send area put-line (vector x1 y1) (vector x2 y2)))))
    
    (cond [label  (line-legend-entries label zs labels colors widths styles)]
          [else   empty])))

(defproc (contours
          [f (real? real? . -> . real?)]
          [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
          [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
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
          f levels samples colors styles contour-colors contour-widths contour-styles alphas label)
         area)
  (let/ec return
    (define-values (x-min x-max y-min y-max) (send area get-bounds))
    (match-define (list xs ys zss) (f x-min x-max samples y-min y-max samples))
    
    (define-values (z-min z-max)
      (let ([flat-zs  (filter regular? (2d-sample->list zss))])
        (when (empty? flat-zs) (return empty))
        (values (apply min* flat-zs) (apply max* flat-zs))))
    
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
           (for/list ([poly  (in-list (heights->mid-polys (exact->inexact za) (exact->inexact zb)
                                                          (exact->inexact z1) (exact->inexact z2)
                                                          (exact->inexact z3) (exact->inexact z4)))])
             (cond [(equal? poly 'full)  (list (vector xa ya) (vector xa yb)
                                               (vector xb yb) (vector xb ya))]
                   [else  (map (λ (v) (vector-take v 2))
                               (scale-normalized-poly poly xa xb ya yb))])))))
      
      (define (draw-polys)
        (for ([poly  (in-list polys)])
          (send area put-polygon poly)))
      
      (cond [(= alpha 1)
             (send area set-pen color 1 poly-line-style)
             (send area set-brush color fill-style)
             (send area set-alpha 1)
             (draw-polys)]
            [else
             ;; draw the outlines with reduced alpha first
             (send area set-pen color 1 poly-line-style)
             (send area set-brush color 'transparent)
             (send area set-alpha (alpha-expt alpha 1/8))
             (draw-polys)
             ;; now draw the centers
             (send area set-pen color 1 'transparent)
             (send area set-brush color fill-style)
             (send area set-alpha alpha)
             (draw-polys)]))
    
    ((contours-render-proc f levels samples contour-colors contour-widths contour-styles alphas #f)
     area)
    
    (cond [label  (contour-intervals-legend-entries
                   label zs labels cs fss cs '(1) pss contour-colors contour-widths contour-styles)]
          [else   empty])))

(defproc (contour-intervals
          [f (real? real? . -> . real?)]
          [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
          [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
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
