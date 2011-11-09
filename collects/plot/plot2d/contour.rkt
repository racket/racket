#lang racket/base

;; Renderers for contour lines and contour intervals

(require racket/contract racket/class racket/match racket/list racket/flonum racket/vector racket/math
         plot/utils
         "../common/contract-doc.rkt"
         "../common/utils.rkt")

(provide (all-defined-out))

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
    
    (let ([colors  (maybe-apply colors zs)]
          [widths  (maybe-apply widths zs)]
          [styles  (maybe-apply styles zs)]
          [alphas  (maybe-apply alphas zs)])
      (for ([z      (in-list zs)]
            [color  (in-cycle colors)]
            [width  (in-cycle widths)]
            [style  (in-cycle styles)]
            [alpha  (in-cycle alphas)])
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
            [else   empty]))))

(defproc (contours
          [f (real? real? . -> . real?)]
          [x-min (or/c regular-real? #f) #f]
          [x-max (or/c regular-real? #f) #f]
          [y-min (or/c regular-real? #f) #f]
          [y-max (or/c regular-real? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (contour-samples)]
          [#:levels levels (or/c 'auto exact-positive-integer? (listof real?)) (contour-levels)]
          [#:colors colors (plot-colors/c (listof real?)) (contour-colors)]
          [#:widths widths (pen-widths/c (listof real?)) (contour-widths)]
          [#:styles styles (plot-pen-styles/c (listof real?)) (contour-styles)]
          [#:alphas alphas (alphas/c (listof real?)) (contour-alphas)]
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
    
    (define-values (z-ivls ivl-labels)
      (for/lists (z-ivls ivl-labels) ([za  (in-list zs)]
                                      [zb  (in-list (rest zs))]
                                      [la  (in-list labels)]
                                      [lb  (in-list (rest labels))])
        (values (ivl za zb) (format "[~a,~a]" la lb))))
    
    (let ([colors  (map ->brush-color (maybe-apply colors z-ivls))]
          [styles  (map ->brush-style (maybe-apply styles z-ivls))]
          [alphas  (maybe-apply alphas z-ivls)])
      (for ([za     (in-list zs)]
            [zb     (in-list (rest zs))]
            [color  (in-cycle colors)]
            [style  (in-cycle styles)]
            [alpha  (in-cycle alphas)])
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
        
        (send area put-pen color 1 'transparent)
        (send area put-brush color style)
        (send area put-alpha alpha)
        (for ([poly  (in-list polys)])
            (send area put-polygon poly)))
      
      ((contours-render-proc g levels samples contour-colors contour-widths contour-styles alphas #f)
       area)
      
      (define n (- (length zs) 2))
      (define contour-colors*
        (append (list 0) (sequence-take (in-cycle (maybe-apply contour-colors zs)) 0 n) (list 0)))
      (define contour-widths*
        (append (list 0) (sequence-take (in-cycle (maybe-apply contour-widths zs)) 0 n) (list 0)))
      (define contour-styles*
        (append '(transparent) (sequence-take (in-cycle (maybe-apply contour-styles zs)) 0 n)
                '(transparent)))
      
      (cond [label  (interval-legend-entries
                     label z-ivls ivl-labels
                     colors styles colors '(1) '(transparent)
                     contour-colors* contour-widths* contour-styles*
                     (rest contour-colors*) (rest contour-widths*) (rest contour-styles*))]
            [else   empty]))))

(defproc (contour-intervals
          [f (real? real? . -> . real?)]
          [x-min (or/c regular-real? #f) #f]
          [x-max (or/c regular-real? #f) #f]
          [y-min (or/c regular-real? #f) #f]
          [y-max (or/c regular-real? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (contour-samples)]
          [#:levels levels (or/c 'auto exact-positive-integer? (listof real?)) (contour-levels)]
          [#:colors colors (plot-colors/c (listof ivl?)) (contour-interval-colors)]
          [#:styles styles (plot-brush-styles/c (listof ivl?)) (contour-interval-styles)]
          [#:contour-colors contour-colors (plot-colors/c (listof real?)) (contour-colors)]
          [#:contour-widths contour-widths (pen-widths/c (listof real?)) (contour-widths)]
          [#:contour-styles contour-styles (plot-pen-styles/c (listof real?)) (contour-styles)]
          [#:alphas alphas (alphas/c (listof ivl?)) (contour-interval-alphas)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (define g (2d-function->sampler f))
  (renderer2d (vector (ivl x-min x-max) (ivl y-min y-max)) #f default-ticks-fun
              (contour-intervals-render-proc g levels samples colors styles
                                             contour-colors contour-widths contour-styles
                                             alphas label)))
