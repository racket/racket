#lang racket/base

(require racket/class racket/match racket/math racket/list racket/flonum
         (only-in math/flonum fl)
         "../common/plot-device.rkt"
         "../common/ticks.rkt"
         "../common/math.rkt"
         "../common/draw.rkt"
         "../common/axis-transform.rkt"
         "../common/parameters.rkt"
         "../common/utils.rkt"
         "clip.rkt"
         "vector.rkt")

(provide (all-defined-out))

(define plot2d-subdivisions (make-parameter 0))

(define 2d-plot-area%
  (class object%
    (init-field bounds-rect rx-ticks rx-far-ticks ry-ticks ry-far-ticks)
    (init dc dc-x-min dc-y-min dc-x-size dc-y-size)
    (super-new)
    
    (define pd (make-object plot-device% dc dc-x-min dc-y-min dc-x-size dc-y-size))
    (send pd reset-drawing-params)
    
    (define char-height (send pd get-char-height))    
    (define half-char-height (* 1/2 char-height))
    
    (match-define (vector (ivl x-min x-max) (ivl y-min y-max)) bounds-rect)
    (define x-size (- x-max x-min))
    (define y-size (- y-max y-min))
    (define x-mid (* 1/2 (+ x-min x-max)))
    (define y-mid (* 1/2 (+ y-min y-max)))
    
    (define clipping? #f)
    (define clip-x-min x-min)
    (define clip-x-max x-max)
    (define clip-y-min y-min)
    (define clip-y-max y-max)
    
    (define/public (put-clip-rect rect)
      (match-define (vector (ivl rx-min rx-max) (ivl ry-min ry-max)) rect)
      (define cx-min (if rx-min (max* x-min rx-min) x-min))
      (define cx-max (if rx-max (min* x-max rx-max) x-max))
      (define cy-min (if ry-min (max* y-min ry-min) y-min))
      (define cy-max (if ry-max (min* y-max ry-max) y-max))
      (let ([cx-min  (min* cx-min cx-max)]
            [cx-max  (max* cx-min cx-max)]
            [cy-min  (min* cy-min cy-max)]
            [cy-max  (max* cy-min cy-max)])
        (set! clip-x-min cx-min)
        (set! clip-x-max cx-max)
        (set! clip-y-min cy-min)
        (set! clip-y-max cy-max))
      (set! clipping? #t))
    
    (define/public (clear-clip-rect) (set! clipping? #f))
    
    (define (in-bounds? v)
      (or (not clipping?) (point-in-bounds? v clip-x-min clip-x-max clip-y-min clip-y-max)))
    
    (define/public (get-x-ticks) x-ticks)
    (define/public (get-x-far-ticks) x-far-ticks)
    (define/public (get-y-ticks) y-ticks)
    (define/public (get-y-far-ticks) y-far-ticks)
    
    (define/public (get-bounds-rect) bounds-rect)
    
    (define/public (get-clip-rect)
      (cond [clipping?  (vector (ivl clip-x-min clip-x-max) (ivl clip-y-min clip-y-max))]
            [else       bounds-rect]))
    
    ;; There are three coordinate systems:
    ;;  1. Plot coordinates (original, user-facing coordinate system)
    ;;  2. View coordinates (from plot coordinates: transform for each axis, then translate and scale
    ;;     to the interval [0,1])
    ;;  3. Device context coordinates (from view coordinates: scale to plot area)
    
    (match-define (invertible-function fx gx) (apply-axis-transform (plot-x-transform) x-min x-max))
    (match-define (invertible-function fy gy) (apply-axis-transform (plot-y-transform) y-min y-max))
    
    (define identity-transforms?
      (and (equal? (plot-x-transform) id-transform)
           (equal? (plot-y-transform) id-transform)))
    
    (define plot->view
      (if identity-transforms?
          (match-lambda
            [(vector (? rational? x) (? rational? y))
             (vector (fl (/ (- x x-min) x-size))
                     (fl (/ (- y y-min) y-size)))]
            [(vector x y)
             (vector +nan.0 +nan.0)])
          (match-lambda
            [(vector (? rational? x) (? rational? y))
             (let ([x  (fx x)] [y  (fy y)])
               (vector (if (rational? x) (fl (/ (- (inexact->exact x) x-min) x-size)) +nan.0)
                       (if (rational? y) (fl (/ (- (inexact->exact y) y-min) y-size)) +nan.0)))]
            [(vector x y)
             (vector +nan.0 +nan.0)])))
    
    (define view->dc #f)
    (define (plot->dc v) (view->dc (plot->view v)))
    
    (define-values (view-x-size view-y-size)
      (match-let ([(vector view-x-ivl view-y-ivl)
                   (bounding-rect (map plot->view (list (vector x-min y-min) (vector x-min y-max)
                                                        (vector x-max y-min) (vector x-max y-max))))])
        (values (ivl-length view-x-ivl) (ivl-length view-y-ivl))))
    
    (define (make-view->dc left right top bottom)
      (define area-x-min left)
      (define area-x-max (- dc-x-size right))
      (define area-y-min top)
      (define area-y-max (- dc-y-size bottom))
      (define area-per-view-x (/ (- area-x-max area-x-min) view-x-size))
      (define area-per-view-y (/ (- area-y-max area-y-min) view-y-size))
      (let-map
       (area-x-min area-y-max area-per-view-x area-per-view-y) fl
       (λ (v)
         (match-define (vector x y) v)
         (vector (fl+ area-x-min (fl* x area-per-view-x))
                 (fl- area-y-max (fl* y area-per-view-y))))))
    
    (define init-top-margin
      (cond [(and (plot-decorations?) (plot-title))  (* 3/2 char-height)]
            [else  0]))
    
    ;; Initial view->dc (draws labels and half of every tick off the allotted space on the dc)
    (set! view->dc (make-view->dc 0 0 init-top-margin 0))
    
    ;; ===============================================================================================
    ;; Tick and label constants
    
    (define tick-radius (* 1/2 (plot-tick-size)))
    (define half-tick-radius (* 1/2 tick-radius))
    
    (define near-dist^2 (sqr (* 3 (plot-line-width))))
    (define (vnear? v1 v2)
      ((vmag^2 (v- (plot->dc v1) (plot->dc v2))) . <= . near-dist^2))
    
    (define ((x-tick-near? y) t1 t2)
      (vnear? (vector (pre-tick-value t1) y)
              (vector (pre-tick-value t2) y)))
    
    (define ((y-tick-near? x) t1 t2)
      (vnear? (vector x (pre-tick-value t1))
              (vector x (pre-tick-value t2))))
    
    (define x-ticks
      (collapse-ticks (filter (λ (t) (<= x-min (pre-tick-value t) x-max))
                              (map tick-inexact->exact rx-ticks))
                      (x-tick-near? y-min)))
    (define x-far-ticks
      (collapse-ticks (filter (λ (t) (<= x-min (pre-tick-value t) x-max))
                              (map tick-inexact->exact rx-far-ticks))
                      (x-tick-near? y-max)))
    (define y-ticks
      (collapse-ticks (filter (λ (t) (<= y-min (pre-tick-value t) y-max))
                              (map tick-inexact->exact ry-ticks))
                      (y-tick-near? x-min)))
    (define y-far-ticks
      (collapse-ticks (filter (λ (t) (<= y-min (pre-tick-value t) y-max))
                              (map tick-inexact->exact ry-far-ticks))
                      (y-tick-near? x-max)))
    
    ;; ===============================================================================================
    ;; Tick and label parameters, and fixpoint margin computation
    
    ;; From here through "All parameters" are functions that compute *just the parameters* of ticks
    ;; and labels that will be drawn on the plot. We have to separate computing parameters from
    ;; actually drawing the ticks and labels so we can solve for the plot margins using a fixpoint
    ;; computation. See ../common/draw.rkt for more explanation. (Search for 'margin-fixpoint'.)
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Tick parameters
    
    (define (x-tick-value->dc x) (plot->dc (vector x y-min)))
    (define (y-tick-value->dc y) (plot->dc (vector x-min y)))
    
    (define (x-far-tick-value->dc x) (plot->dc (vector x y-max)))
    (define (y-far-tick-value->dc y) (plot->dc (vector x-max y)))
    
    (define (get-tick-params ticks tick-value->dc angle)
      (for/list ([t  (in-list ticks)])
        (match-define (tick x major? _) t)
        (list major? (tick-value->dc x) (if major? tick-radius half-tick-radius) angle)))
    
    (define (get-x-tick-params)
      (if (plot-x-axis?) (get-tick-params x-ticks x-tick-value->dc (* 1/2 pi)) empty))
    
    (define (get-y-tick-params)
      (if (plot-y-axis?) (get-tick-params y-ticks y-tick-value->dc 0) empty))
    
    (define (get-x-far-tick-params)
      (if (plot-x-far-axis?) (get-tick-params x-far-ticks x-far-tick-value->dc (* 1/2 pi)) empty))
    
    (define (get-y-far-tick-params)
      (if (plot-y-far-axis?) (get-tick-params y-far-ticks y-far-tick-value->dc 0) empty))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Tick label parameters
    
    (define draw-x-far-tick-labels? (not (and (plot-x-axis?) (equal? x-ticks x-far-ticks))))
    (define draw-y-far-tick-labels? (not (and (plot-y-axis?) (equal? y-ticks y-far-ticks))))
    
    (define x-tick-label-offset (vector 0 (+ (pen-gap) tick-radius)))
    (define y-tick-label-offset (vector (- (+ (pen-gap) tick-radius)) 0))
    
    (define x-far-tick-label-offset (vneg x-tick-label-offset))
    (define y-far-tick-label-offset (vneg y-tick-label-offset))
    
    (define (get-tick-label-params ticks tick-label-offset tick-value->dc anchor angle)
      (for/list ([t  (in-list ticks)] #:when (pre-tick-major? t))
        (match-define (tick p _ label) t)
        (list label (v+ (tick-value->dc p) tick-label-offset) anchor (degrees->radians angle))))
    
    (define (get-x-tick-label-params)
      (if (plot-x-axis?)
          (get-tick-label-params x-ticks x-tick-label-offset x-tick-value->dc
                                 (plot-x-tick-label-anchor)
                                 (plot-x-tick-label-angle))
          empty))
    
    (define (get-y-tick-label-params)
      (if (plot-y-axis?)
          (get-tick-label-params y-ticks y-tick-label-offset y-tick-value->dc
                                 (plot-y-tick-label-anchor)
                                 (plot-y-tick-label-angle))
          empty))
    
    (define (get-x-far-tick-label-params)
      (if (and (plot-x-far-axis?) draw-x-far-tick-labels?)
          (get-tick-label-params x-far-ticks x-far-tick-label-offset x-far-tick-value->dc
                                 (plot-x-far-tick-label-anchor)
                                 (plot-x-far-tick-label-angle))
          empty))
    
    (define (get-y-far-tick-label-params)
      (if (and (plot-y-far-axis?) draw-y-far-tick-labels?)
          (get-tick-label-params y-far-ticks y-far-tick-label-offset y-far-tick-value->dc
                                 (plot-y-far-tick-label-anchor)
                                 (plot-y-far-tick-label-angle))
          empty))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Axis label parameters
    
    (define (max-tick-offset ts)
      (cond [(empty? ts)  0]
            [(ormap pre-tick-major? ts)  (+ (pen-gap) tick-radius)]
            [else  (+ (pen-gap) (* 1/4 (plot-tick-size)))]))
    
    (define max-x-tick-offset (if (plot-x-axis?) (max-tick-offset x-ticks) 0))
    (define max-y-tick-offset (if (plot-y-axis?) (max-tick-offset y-ticks) 0))
    
    (define max-x-far-tick-offset (if (plot-x-far-axis?) (max-tick-offset x-far-ticks) 0))
    (define max-y-far-tick-offset (if (plot-y-far-axis?) (max-tick-offset y-far-ticks) 0))
    
    (define (get-relative-corners params)
      (append* (map (match-lambda
                      [(list label _ anchor angle)
                       (send pd get-text-corners label #(0 0) anchor angle)])
                    params)))
    
    (define max-x-tick-label-height
      (if (plot-x-axis?)
          (apply max 0 (map (λ (corner) (vector-ref corner 1))
                            (get-relative-corners (get-x-tick-label-params))))
          0))
    
    (define max-y-tick-label-width
      (if (plot-y-axis?)
          (- (apply min 0 (map (λ (corner) (vector-ref corner 0))
                               (get-relative-corners (get-y-tick-label-params)))))
          0))
    
    (define max-x-far-tick-label-height
      (if (and (plot-x-far-axis?) draw-x-far-tick-labels?)
          (- (apply min 0 (map (λ (corner) (vector-ref corner 1))
                               (get-relative-corners (get-x-far-tick-label-params)))))
          0))
    
    (define max-y-far-tick-label-width
      (if (and (plot-y-far-axis?) draw-y-far-tick-labels?)
          (apply max 0 (map (λ (corner) (vector-ref corner 0))
                            (get-relative-corners (get-y-far-tick-label-params))))
          0))
    
    (define (get-x-label-params)
      (define offset (vector 0 (+ max-x-tick-offset max-x-tick-label-height half-char-height)))
      (list (plot-x-label) (v+ (view->dc (vector 0.5 0.0)) offset) 'top))
    
    (define (get-y-label-params)
      (define offset (vector (+ max-y-tick-offset max-y-tick-label-width half-char-height) 0))
      (list (plot-y-label) (v- (view->dc (vector 0.0 0.5)) offset) 'bottom (/ pi 2)))
    
    (define (get-x-far-label-params)
      (define offset (vector 0 (+ max-x-far-tick-offset max-x-far-tick-label-height
                                  half-char-height)))
      (list (plot-x-far-label) (v- (view->dc (vector 0.5 1.0)) offset) 'bottom))
    
    (define (get-y-far-label-params)
      (define offset (vector (+ max-y-far-tick-offset max-y-far-tick-label-width half-char-height) 0))
      (list (plot-y-far-label) (v+ (view->dc (vector 1.0 0.5)) offset) 'top (/ pi 2)))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; All parameters
    
    (define (get-all-label-params)
      (if (plot-decorations?)
          (append (if (plot-x-label) (list (get-x-label-params)) empty)
                  (if (plot-y-label) (list (get-y-label-params)) empty)
                  (if (plot-x-far-label) (list (get-x-far-label-params)) empty)
                  (if (plot-y-far-label) (list (get-y-far-label-params)) empty)
                  (get-x-tick-label-params)
                  (get-y-tick-label-params)
                  (get-x-far-tick-label-params)
                  (get-y-far-tick-label-params))
          empty))
    
    (define (get-all-tick-params)
      (if (plot-decorations?)
          (append (get-x-tick-params) (get-y-tick-params)
                  (get-x-far-tick-params) (get-y-far-tick-params))
          empty))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Fixpoint margin computation
    
    (define (get-param-vs/set-view->dc! left right top bottom)
      ;(printf "margins: ~v ~v ~v ~v~n~n" left right top bottom)
      (set! view->dc (make-view->dc left right top bottom))
      ;(printf "params: ~v~n~n" (get-x-label-params))
      (define res
        (append (append* (map (λ (params) (send/apply pd get-text-corners params))
                              (get-all-label-params)))
                (append* (map (λ (params) (send/apply pd get-tick-endpoints (rest params)))
                              (get-all-tick-params)))))
      ;(printf "res = ~v~n~n" res)
      res)
    
    (define-values (left right top bottom)
      (margin-fixpoint 0 dc-x-size init-top-margin dc-y-size 0 0 init-top-margin 0
                       get-param-vs/set-view->dc!))
    
    (define area-x-min left)
    (define area-x-max (- dc-x-size right))
    (define area-y-min top)
    (define area-y-max (- dc-y-size bottom))
    
    (define/public (get-area-bounds-rect)
      (vector (ivl area-x-min area-x-max) (ivl area-y-min area-y-max)))
    
    (define view->plot
      (let-map
       (x-min y-min x-size y-size) inexact->exact
       (cond [identity-transforms?
              (match-lambda
                [(vector x y)
                 (vector (+ (* (inexact->exact x) x-size) x-min)
                         (+ (* (inexact->exact y) y-size) y-min))])]
             [else
              (match-lambda
                [(vector x y)
                 (vector (gx (+ (* (inexact->exact x) x-size) x-min))
                         (gy (+ (* (inexact->exact y) y-size) y-min)))])])))
    
    (define dc->view
      (let ([area-per-view-x  (/ (- area-x-max area-x-min) view-x-size)]
            [area-per-view-y  (/ (- area-y-max area-y-min) view-y-size)])
        (match-lambda
          [(vector x y)
           (vector (/ (- x area-x-min) area-per-view-x)
                   (/ (- area-y-max y) area-per-view-y))])))
    
    (define/public (dc->plot v)
      (view->plot (dc->view v)))
    
    ;; ===============================================================================================
    ;; Plot decoration
    
    (define (draw-title)
      (when (and (plot-decorations?) (plot-title))
        (send pd draw-text (plot-title) (vector (* 1/2 dc-x-size) 0) 'top)))
    
    (define (draw-axes)
      (when (plot-decorations?)
        (send pd set-minor-pen)
        (when (plot-x-axis?)
          (send pd draw-line
                (vector area-x-min area-y-max)
                (vector area-x-max area-y-max)))
        (when (plot-x-far-axis?)
          (send pd draw-line
                (vector area-x-min area-y-min)
                (vector area-x-max area-y-min)))
        (when (plot-y-axis?)
          (send pd draw-line
                (vector area-x-min area-y-min)
                (vector area-x-min area-y-max)))
        (when (plot-y-far-axis?)
          (send pd draw-line
                (vector area-x-max area-y-min)
                (vector area-x-max area-y-max)))))
    
    (define (draw-ticks)
      (for ([params  (in-list (get-all-tick-params))])
        (match-define (list major? v r angle) params)
        (if major? (send pd set-major-pen) (send pd set-minor-pen))
        (send pd draw-tick v r angle)))
    
    (define (draw-labels)
      (for ([params  (in-list (get-all-label-params))])
        (send/apply pd draw-text params #:outline? #t)))
    
    ;; ===============================================================================================
    ;; Public drawing control (used by plot/dc)
    
    (define/public (start-plot)
      (send pd reset-drawing-params)
      (send pd clear)
      (draw-title)
      (draw-axes)
      (draw-ticks)
      (draw-labels)
      (define lw (plot-line-width))
      (send pd set-clipping-rect
            (vector (ivl (+ 1/2 (- area-x-min lw)) (+ area-x-max lw))
                    (ivl (+ 1/2 (- area-y-min lw)) (+ area-y-max lw)))))
    
    (define/public (start-renderer rend-bounds-rect)
      (reset-drawing-params)
      (put-clip-rect rend-bounds-rect))
    
    (define/public (end-renderers)
      (clear-clip-rect)
      (send pd reset-drawing-params))
    
    (define/public (draw-legend legend-entries)
      (define gap-size (+ (pen-gap) tick-radius))
      (send pd draw-legend legend-entries
            (vector (ivl (+ area-x-min gap-size) (- area-x-max gap-size))
                    (ivl (+ area-y-min gap-size) (- area-y-max gap-size)))))
    
    (define/public (end-plot)
      (send pd restore-drawing-params))
    
    ;; ===============================================================================================
    ;; Public drawing interface (used by renderers)
    
    (define/public (put-alpha alpha) (send pd set-alpha alpha))
    
    (define/public (put-pen color width style) (send pd set-pen color width style))
    (define/public (put-major-pen [style 'solid]) (send pd set-major-pen style))
    (define/public (put-minor-pen [style 'solid]) (send pd set-minor-pen style))
    
    (define/public (put-brush color style) (send pd set-brush color style))
    
    (define/public (put-background color) (send pd set-background color))
    
    (define/public (put-font-size size) (send pd set-font-size size))
    (define/public (put-font-family family) (send pd set-font-family family))
    (define/public put-font
      (case-lambda 
        [(size family) (send pd set-font size #f family)]
        [(size face family) (send pd set-font size face family)]))
    (define/public (put-text-foreground color) (send pd set-text-foreground color))
    
    (define/public (reset-drawing-params)
      (put-alpha (plot-foreground-alpha))
      (put-pen (plot-foreground) (plot-line-width) 'solid)
      (put-brush (plot-background) 'solid)
      (put-background (plot-background))
      (put-font (plot-font-size) (plot-font-face) (plot-font-family))
      (put-text-foreground (plot-foreground)))
    
    ;; Shapes
    
    (define/public (put-lines vs)
      (for ([vs  (in-list (exact-vector2d-sublists vs))])
        (let ([vss  (if clipping?
                        (clip-lines/bounds vs
                                           clip-x-min clip-x-max
                                           clip-y-min clip-y-max)
                        (list vs))])
          (for ([vs  (in-list vss)])
            (unless (empty? vs)
              (let* ([vs  (if identity-transforms? vs (subdivide-lines plot->dc vs))]
                     [vs  (map plot->dc vs)])
                (send pd draw-lines vs)))))))
    
    (define/public (put-line v1 v2)
      (let ([v1  (exact-vector2d v1)]
            [v2  (exact-vector2d v2)])
        (when (and v1 v2)
          (let-values ([(v1 v2)  (if clipping?
                                     (clip-line/bounds v1 v2
                                                       clip-x-min clip-x-max
                                                       clip-y-min clip-y-max)
                                     (values v1 v2))])
            (when (and v1 v2)
              (if identity-transforms?
                  (send pd draw-line (plot->dc v1) (plot->dc v2))
                  (send pd draw-lines (map plot->dc (subdivide-line plot->dc v1 v2)))))))))
    
    (define/public (put-polygon vs)
      (define ls (make-list (length vs) #t))
      (let-values ([(vs ls)  (exact-polygon2d vs ls)])
        (unless (empty? vs)
          (let-values ([(vs ls)  (if clipping?
                                     (clip-polygon/bounds vs ls
                                                          clip-x-min clip-x-max
                                                          clip-y-min clip-y-max)
                                     (values vs ls))])
            (unless (empty? vs)
              (if identity-transforms?
                  (send pd draw-polygon (map plot->dc vs))
                  (send pd draw-polygon (map plot->dc (subdivide-polygon plot->dc vs)))))))))
    
    (define/public (put-rect r)
      (when (rect-rational? r)
        (match-define (vector (ivl x1 x2) (ivl y1 y2)) r)
        (put-polygon (list (vector x1 y1) (vector x2 y1) (vector x2 y2) (vector x1 y2)))))
    
    (define/public (put-text str v [anchor 'top-left] [angle 0] [dist 0] #:outline? [outline? #f])
      (let ([v  (exact-vector2d v)])
        (when (and v (in-bounds? v))
          (send pd draw-text str (plot->dc v) anchor angle dist #:outline? outline?))))
    
    (define/public (put-glyphs vs symbol size)
      (let ([vs  (filter (λ (v) (and v (in-bounds? v))) (map exact-vector2d vs))])
        (unless (empty? vs)
          (send pd draw-glyphs (map plot->dc vs) symbol size))))
      
    (define/public (put-arrow v1 v2)
      (let ([v1  (exact-vector2d v1)]
            [v2  (exact-vector2d v2)])
        (when (and v1 v2 (in-bounds? v1))
          (send pd draw-arrow (plot->dc v1) (plot->dc v2)))))
    
    (define/public (put-tick v r angle)
      (let ([v  (exact-vector2d v)])
        (when (and v (in-bounds? v))
          (send pd draw-tick (plot->dc v) r angle))))
    ))
