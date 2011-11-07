#lang racket/base

(require racket/class racket/match racket/list racket/flonum racket/contract
         plot/utils
         "../common/contract-doc.rkt"
         "../common/utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; One contour line in 3D (using marching squares)

(define ((isoline3d-render-proc f z samples color width style alpha label) area)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max))
    (send area get-bounds-rect))
  (match-define (2d-sample xs ys zss fz-min fz-max)
    (f x-min x-max (animated-samples samples) y-min y-max (animated-samples samples)))
  
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
      (for ([line  (in-list (heights->lines xa xb ya yb z z1 z2 z3 z4))])
        (match-define (list v1 v2) line)
        (define center (vector (* 1/2 (+ xa xb)) (* 1/2 (+ ya yb))
                               (* 1/2 (+ (min z1 z2 z3 z4) (max z1 z2 z3 z4)))))
        (send area put-line v1 v2 center))))
  
  (cond [label  (line-legend-entry label color width style)]
        [else   empty]))

(defproc (isoline3d
          [f (real? real? . -> . real?)] [z real?]
          [x-min (or/c regular-real? #f) #f]
          [x-max (or/c regular-real? #f) #f]
          [y-min (or/c regular-real? #f) #f]
          [y-max (or/c regular-real? #f) #f]
          [#:z-min z-min (or/c regular-real? #f) #f]
          [#:z-max z-max (or/c regular-real? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (plot3d-samples)]
          [#:color color plot-color/c (line-color)]
          [#:width width (>=/c 0) (line-width)]
          [#:style style plot-pen-style/c (line-style)]
          [#:alpha alpha (real-in 0 1) (line-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (define g (2d-function->sampler f))
  (let ([z-min  (if z-min z-min z)]
        [z-max  (if z-max z-max z)])
    (renderer3d (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max))
                #f default-ticks-fun
                (isoline3d-render-proc g z samples color width style alpha label))))

;; ===================================================================================================
;; Contour lines in 3D (using marching squares)

(define ((contours3d-render-proc f levels samples colors widths styles alphas label) area)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max))
    (send area get-bounds-rect))
  (match-define (2d-sample xs ys zss fz-min fz-max)
    (f x-min x-max (animated-samples samples) y-min y-max (animated-samples samples)))
  
  (match-define (list (tick zs _ labels) ...) (contour-ticks z-min z-max levels #f))
  
  (let ([colors  (maybe-apply colors zs)]
        [widths  (maybe-apply widths zs)]
        [styles  (maybe-apply styles zs)]
        [alphas  (maybe-apply alphas zs)])
    (for ([z  (in-list zs)]
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
        (for ([line  (in-list (heights->lines xa xb ya yb z z1 z2 z3 z4))])
          (match-define (list v1 v2) line)
          (define center (vector (* 1/2 (+ xa xb)) (* 1/2 (+ ya yb))
                                 (* 1/2 (+ (min z1 z2 z3 z4) (max z1 z2 z3 z4)))))
          (send area put-line v1 v2 center))))
    
    (cond [label  (line-legend-entries label zs labels colors widths styles)]
          [else   empty])))

(defproc (contours3d
          [f (real? real? . -> . real?)]
          [x-min (or/c regular-real? #f) #f]
          [x-max (or/c regular-real? #f) #f]
          [y-min (or/c regular-real? #f) #f]
          [y-max (or/c regular-real? #f) #f]
          [#:z-min z-min (or/c regular-real? #f) #f]
          [#:z-max z-max (or/c regular-real? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (plot3d-samples)]
          [#:levels levels (or/c 'auto pos/c (listof real?)) (contour-levels)]
          [#:colors colors (plot-colors/c (listof real?)) (contour-colors)]
          [#:widths widths (pen-widths/c (listof real?)) (contour-widths)]
          [#:styles styles (plot-pen-styles/c (listof real?)) (contour-styles)]
          [#:alphas alphas (alphas/c (listof real?)) (contour-alphas)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (define g (2d-function->sampler f))
  (renderer3d (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max))
              (surface3d-bounds-fun g samples)
              default-ticks-fun
              (contours3d-render-proc g levels samples colors widths styles alphas label)))

;; ===================================================================================================
;; Contour intervals in 3D (using marching squares)

(define ((contour-intervals3d-render-proc
          f levels samples colors styles line-colors line-widths line-styles
          contour-colors contour-widths contour-styles alphas label)
         area)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max))
    (send area get-bounds-rect))
  (match-define (2d-sample xs ys zss fz-min fz-max)
    (f x-min x-max (animated-samples samples) y-min y-max (animated-samples samples)))
  
  (match-define (list (tick zs _ labels) ...) (contour-ticks z-min z-max levels #t))
  
  (define-values (z-ivls ivl-labels)
    (for/lists (z-ivls ivl-labels) ([za  (in-list zs)]
                                    [zb  (in-list (rest zs))]
                                    [la  (in-list labels)]
                                    [lb  (in-list (rest labels))])
      (values (ivl za zb) (format "[~a,~a]" la lb))))
  
  (let ([colors  (maybe-apply colors z-ivls)]
        [styles  (maybe-apply styles z-ivls)]
        [alphas  (maybe-apply alphas z-ivls)]
        [line-colors  (maybe-apply line-colors z-ivls)]
        [line-widths  (maybe-apply line-widths z-ivls)]
        [line-styles  (maybe-apply line-styles z-ivls)])
    (for ([za  (in-list zs)]
          [zb  (in-list (rest zs))]
          [color  (in-cycle colors)]
          [style  (in-cycle styles)]
          [alpha  (in-cycle alphas)]
          [line-color  (in-cycle line-colors)]
          [line-width  (in-cycle line-widths)]
          [line-style  (in-cycle line-styles)])
      (send area put-alpha alpha)
      (send area put-pen line-color line-width line-style)
      (send area put-brush color style)
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
        (for ([poly  (in-list (heights->polys xa xb ya yb za zb z1 z2 z3 z4))])
          (define center (vector (* 1/2 (+ xa xb))
                                 (* 1/2 (+ ya yb))
                                 (* 1/2 (+ (min z1 z2 z3 z4) (max z1 z2 z3 z4)))))
          (send area put-polygon poly center))))
    
    ((contours3d-render-proc f levels samples contour-colors contour-widths contour-styles alphas #f)
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
                   colors styles line-colors line-widths line-styles
                   contour-colors* contour-widths* contour-styles*
                   (rest contour-colors*) (rest contour-widths*) (rest contour-styles*))]
          [else  empty])))

(defproc (contour-intervals3d
          [f (real? real? . -> . real?)]
          [x-min (or/c regular-real? #f) #f]
          [x-max (or/c regular-real? #f) #f]
          [y-min (or/c regular-real? #f) #f]
          [y-max (or/c regular-real? #f) #f]
          [#:z-min z-min (or/c regular-real? #f) #f]
          [#:z-max z-max (or/c regular-real? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (plot3d-samples)]
          [#:levels levels (or/c 'auto pos/c (listof real?)) (contour-levels)]
          [#:colors colors (plot-colors/c (listof ivl?)) (contour-interval-colors)]
          [#:styles styles (plot-brush-styles/c (listof ivl?)) (contour-interval-styles)]
          [#:line-colors line-colors (plot-colors/c (listof ivl?)) (contour-interval-line-colors)]
          [#:line-widths line-widths (pen-widths/c (listof ivl?)) (contour-interval-line-widths)]
          [#:line-styles line-styles (plot-pen-styles/c (listof ivl?)) (contour-interval-line-styles)]
          [#:contour-colors contour-colors (plot-colors/c (listof real?)) (contour-colors)]
          [#:contour-widths contour-widths (pen-widths/c (listof real?)) (contour-widths)]
          [#:contour-styles contour-styles (plot-pen-styles/c (listof real?)) (contour-styles)]
          [#:alphas alphas (alphas/c (listof ivl?)) (contour-interval-alphas)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (define g (2d-function->sampler f))
  (renderer3d (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max))
              (surface3d-bounds-fun g samples)
              default-ticks-fun
              (contour-intervals3d-render-proc g levels samples colors styles
                                               line-colors line-widths line-styles
                                               contour-colors contour-widths contour-styles
                                               alphas label)))
