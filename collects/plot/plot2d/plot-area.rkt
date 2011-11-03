#lang racket/base

(require racket/draw racket/class racket/contract racket/match racket/math racket/list racket/string
         "../common/plot-device.rkt"
         "../common/ticks.rkt"
         "../common/contract.rkt"
         "../common/math.rkt"
         "../common/draw.rkt"
         "../common/axis-transform.rkt"
         "../common/sample.rkt"
         "../common/legend.rkt"
         "../common/parameters.rkt"
         "../common/utils.rkt"
         "clip.rkt")

(provide (all-defined-out))

(define plot2d-subdivisions (make-parameter 0))

(define 2d-plot-area%
  (class object%
    (init-field rx-ticks rx-far-ticks ry-ticks ry-far-ticks
                x-min x-max y-min y-max)
    (init dc dc-x-min dc-y-min dc-x-size dc-y-size)
    (super-new)
    
    (define pd (make-object plot-device% dc dc-x-min dc-y-min dc-x-size dc-y-size))
    (send pd reset-drawing-params)
    
    (define char-height (send pd get-char-height))    
    (define half-char-height (* 1/2 char-height))
    
    (define dc-x-max (+ dc-x-min dc-x-size))
    (define dc-y-max (+ dc-y-min dc-y-size))
    (define title-y-min
      (cond [(and (plot-decorations?) (plot-title))  (+ dc-y-min (* 3/2 char-height))]
            [else  dc-y-min]))
    
    (define x-size (- x-max x-min))
    (define y-size (- y-max y-min))
    
    (define x-mid (* 1/2 (+ x-min x-max)))
    (define y-mid (* 1/2 (+ y-min y-max)))
    
    (define clipping? #f)
    (define clip-x-min x-min)
    (define clip-x-max x-max)
    (define clip-y-min y-min)
    (define clip-y-max y-max)
    
    (define/public (clip-to-bounds rx-min rx-max ry-min ry-max)
      (set! clipping? #t)
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
        (set! clip-y-max cy-max)))
    
    (define/public (clip-to-none)
      (set! clipping? #f))
    
    (define (in-bounds? v)
      (or (not clipping?)
          (point-in-bounds? v clip-x-min clip-x-max
                            clip-y-min clip-y-max)))
    
    (define/public (get-x-ticks) x-ticks)
    (define/public (get-x-far-ticks) x-far-ticks)
    (define/public (get-y-ticks) y-ticks)
    (define/public (get-y-far-ticks) y-far-ticks)
    
    (define/public (get-x-min) x-min)
    (define/public (get-x-max) x-max)
    (define/public (get-y-min) y-min)
    (define/public (get-y-max) y-max)
    (define/public (get-bounds) (values x-min x-max y-min y-max))
    
    (define/public (get-clip-bounds)
      (cond [clipping?  (values clip-x-min clip-x-max clip-y-min clip-y-max)]
            [else       (values x-min x-max y-min y-max)]))
    
    (define identity-transforms?
      (and (equal? (plot-x-transform) id-transform)
           (equal? (plot-y-transform) id-transform)))
    
    (match-define (invertible-function fx _) (apply-axis-transform (plot-x-transform) x-min x-max))
    (match-define (invertible-function fy _) (apply-axis-transform (plot-y-transform) y-min y-max))
    
    (define plot->view
      (cond [identity-transforms?  (λ (v) v)]
            [else  (λ (v) (match-let ([(vector x y)  v])
                            (vector (fx x) (fy y))))]))
    
    (define view->dc #f)
    (define (plot->dc* v) (view->dc (plot->view v)))
    (define/public (plot->dc v) (plot->dc* v))
    
    (define/public (plot-line->dc-angle v1 v2)
      (match-define (vector dx dy) (v- (plot->dc* v1) (plot->dc* v2)))
      (- (atan2 (- dy) dx)))
    
    (define (make-view->dc left right top bottom)
      (define corners (list (vector x-min y-min) (vector x-min y-max)
                            (vector x-max y-min) (vector x-max y-max)))
      (match-define (list (vector xs ys) ...) (map plot->view corners))
      (define view-x-min (apply min xs))
      (define view-x-max (apply max xs))
      (define view-y-min (apply min ys))
      (define view-y-max (apply max ys))
      
      (define area-x-min (+ dc-x-min left))
      (define area-x-max (- dc-x-max right))
      (define area-y-min (+ dc-y-min top))
      (define area-y-max (- dc-y-max bottom))
      (define area-x-size (- area-x-max area-x-min))
      (define area-y-size (- area-y-max area-y-min))
      
      (define area-per-view-x (/ area-x-size (- view-x-max view-x-min)))
      (define area-per-view-y (/ area-y-size (- view-y-max view-y-min)))
      (λ (v)
        (match-define (vector x y) v)
        (vector (+ area-x-min (* (- x x-min) area-per-view-x))
                (- area-y-max (* (- y y-min) area-per-view-y)))))
    
    ;; Initial view->dc (draws labels and half of every tick off the allotted space on the dc)
    (define init-top-margin (- title-y-min dc-y-min))
    (set! view->dc (make-view->dc 0 0 init-top-margin 0))
    
    ;; ===============================================================================================
    ;; Tick and label constants
    
    (define tick-radius (* 1/2 (plot-tick-size)))
    (define half-tick-radius (* 1/2 tick-radius))
    
    (define near-dist^2 (sqr(* 3 (plot-line-width))))
    (define (vnear? v1 v2)
      ((vmag^2 (v- (plot->dc* v1) (plot->dc* v2))) . <= . near-dist^2))
    
    (define ((x-tick-near? y) t1 t2)
      (vnear? (vector (pre-tick-value t1) y)
              (vector (pre-tick-value t2) y)))
    
    (define ((y-tick-near? x) t1 t2)
      (vnear? (vector x (pre-tick-value t1))
              (vector x (pre-tick-value t2))))
    
    (define x-ticks
      (collapse-nearby-ticks (filter (λ (t) (<= x-min (pre-tick-value t) x-max)) rx-ticks)
                             (x-tick-near? y-min)))
    (define x-far-ticks
      (collapse-nearby-ticks (filter (λ (t) (<= x-min (pre-tick-value t) x-max)) rx-far-ticks)
                             (x-tick-near? y-max)))
    (define y-ticks
      (collapse-nearby-ticks (filter (λ (t) (<= y-min (pre-tick-value t) y-max)) ry-ticks)
                             (y-tick-near? x-min)))
    (define y-far-ticks
      (collapse-nearby-ticks (filter (λ (t) (<= y-min (pre-tick-value t) y-max)) ry-far-ticks)
                             (y-tick-near? x-max)))
    
    (define draw-x-far-tick-labels? (not (and (plot-x-axis?) (equal? x-ticks x-far-ticks))))
    (define draw-y-far-tick-labels? (not (and (plot-y-axis?) (equal? y-ticks y-far-ticks))))
    
    ;; ===============================================================================================
    ;; Tick and tick label parameters
    
    (define (x-tick-value->dc x) (plot->dc* (vector x y-min)))
    (define (y-tick-value->dc y) (plot->dc* (vector x-min y)))
    (define (x-far-tick-value->dc x) (plot->dc* (vector x y-max)))
    (define (y-far-tick-value->dc y) (plot->dc* (vector x-max y)))
    
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

    (define x-tick-label-offset (vector 0 (+ (pen-gap) tick-radius)))
    (define y-tick-label-offset (vector (- (+ (pen-gap) tick-radius)) 0))
    (define x-far-tick-label-offset (vneg x-tick-label-offset))
    (define y-far-tick-label-offset (vneg y-tick-label-offset))
    
    (define (get-tick-label-params ticks tick-label-offset tick-value->dc anchor)
      (for/list ([t  (in-list ticks)] #:when (pre-tick-major? t))
        (match-define (tick p _ label) t)
        (list label (v+ (tick-value->dc p) tick-label-offset) anchor)))
    
    (define (get-x-tick-label-params)
      (if (plot-x-axis?)
          (get-tick-label-params x-ticks x-tick-label-offset x-tick-value->dc 'top)
          empty))
    
    (define (get-y-tick-label-params)
      (if (plot-y-axis?)
          (get-tick-label-params y-ticks y-tick-label-offset y-tick-value->dc 'right)
          empty))
    
    (define (get-x-far-tick-label-params)
      (if (plot-x-far-axis?)
          (get-tick-label-params x-far-ticks x-far-tick-label-offset x-far-tick-value->dc 'bottom)
          empty))
    
    (define (get-y-far-tick-label-params)
      (if (plot-y-far-axis?)
          (get-tick-label-params y-far-ticks y-far-tick-label-offset y-far-tick-value->dc 'left)
          empty))
    
    ;; ===============================================================================================
    ;; Axis label parameters
    
    (define (max-tick-offset ts)
      (cond [(empty? ts)  0]
            [(ormap pre-tick-major? ts)  (+ (pen-gap) tick-radius)]
            [else  (+ (pen-gap) (* 1/4 (plot-tick-size)))]))
    
    (define max-x-tick-offset (if (plot-x-axis?) (max-tick-offset x-ticks) 0))
    (define max-y-tick-offset (if (plot-y-axis?) (max-tick-offset y-ticks) 0))
    (define max-x-far-tick-offset (if (plot-x-far-axis?) (max-tick-offset x-far-ticks) 0))
    (define max-y-far-tick-offset (if (plot-y-far-axis?) (max-tick-offset y-far-ticks) 0))
    
    (define (max-tick-label-height ts)
      (if (ormap pre-tick-major? ts) char-height 0))
    
    (define (max-tick-label-width ts)
      (apply max 0 (for/list ([t  (in-list ts)] #:when (pre-tick-major? t))
                     (send pd get-text-width (tick-label t)))))
    
    (define max-x-tick-label-height (if (plot-x-axis?) (max-tick-label-height x-ticks) 0))
    (define max-y-tick-label-width (if (plot-y-axis?) (max-tick-label-width y-ticks) 0))
    (define max-x-far-tick-label-height (if (and (plot-x-far-axis?) draw-x-far-tick-labels?)
                                            (max-tick-label-height x-far-ticks)
                                            0))
    (define max-y-far-tick-label-width (if (and (plot-y-far-axis?) draw-y-far-tick-labels?)
                                           (max-tick-label-width y-far-ticks)
                                           0))
    
    (define (get-x-label-params)
      (define offset (vector 0 (+ max-x-tick-offset max-x-tick-label-height half-char-height)))
      (list (plot-x-label) (v+ (view->dc (vector x-mid y-min)) offset) 'top))
    
    (define (get-y-label-params)
      (define offset (vector (+ max-y-tick-offset max-y-tick-label-width half-char-height) 0))
      (list (plot-y-label) (v- (view->dc (vector x-min y-mid)) offset) 'bottom (/ pi 2)))
    
    (define (get-x-far-label-params)
      (define offset (vector 0 (+ max-x-far-tick-offset max-x-far-tick-label-height half-char-height)))
      (list (plot-x-far-label) (v- (view->dc (vector x-mid y-max)) offset) 'bottom))
    
    (define (get-y-far-label-params)
      (define offset (vector (+ max-y-far-tick-offset max-y-far-tick-label-width half-char-height) 0))
      (list (plot-y-far-label) (v+ (view->dc (vector x-max y-mid)) offset) 'top (/ pi 2)))
    
    ;; ===============================================================================================
    ;; Fixpoint margin computation
    
    (define (get-all-tick-label-params)
      (append (get-x-tick-label-params) (get-y-tick-label-params)
              (if draw-x-far-tick-labels? (get-x-far-tick-label-params) empty)
              (if draw-y-far-tick-labels? (get-y-far-tick-label-params) empty)))
    
    (define (get-all-axis-label-params)
      (append (if (plot-x-label) (list (get-x-label-params)) empty)
              (if (plot-y-label) (list (get-y-label-params)) empty)
              (if (plot-x-far-label) (list (get-x-far-label-params)) empty)
              (if (plot-y-far-label) (list (get-y-far-label-params)) empty)))
    
    (define (get-all-label-params)
      (cond [(plot-decorations?)  (append (get-all-axis-label-params) (get-all-tick-label-params))]
            [else  empty]))
    
    (define (get-all-tick-params)
      (cond [(plot-decorations?)  (append (get-x-tick-params) (get-y-tick-params)
                                          (get-x-far-tick-params) (get-y-far-tick-params))]
            [else  empty]))
    
    (define (new-margins left right top bottom label-params tick-params)
      (match-define (list (vector label-xs label-ys) ...)
        (append* (map (λ (params) (send/apply pd get-text-corners params)) label-params)))
      (match-define (list (vector tick-xs tick-ys) ...)
        (append* (map (λ (params) (send/apply pd get-tick-endpoints (rest params))) tick-params)))
      (define xs (append label-xs tick-xs))
      (define ys (append label-ys tick-ys))
      
      (define param-x-min (apply min dc-x-min xs))
      (define param-x-max (apply max (sub1 dc-x-max) xs))
      (define param-y-min (apply min title-y-min ys))
      (define param-y-max (apply max (sub1 dc-y-max) ys))
      
      (values (+ left (- dc-x-min param-x-min))
              (- right (- (sub1 dc-x-max) param-x-max))
              (+ top (- title-y-min param-y-min))
              (- bottom (- (sub1 dc-y-max) param-y-max))))
    
    (define-values (area-x-min right area-y-min bottom)
      (for/fold ([left 0] [right 0] [top init-top-margin] [bottom 0]) ([i  (in-range 5)])
        (define-values (new-left new-right new-top new-bottom)
          (new-margins left right top bottom (get-all-label-params) (get-all-tick-params)))
        (set! view->dc (make-view->dc new-left new-right new-top new-bottom))
        ;(printf "margins: ~v ~v ~v ~v~n" new-left new-right new-top new-bottom)
        (values new-left new-right new-top new-bottom)))
    
    (define area-x-max (- dc-x-max right))
    (define area-y-max (- dc-y-max bottom))
    
    ;; ===============================================================================================
    ;; Plot decoration
    
    (define (draw-labels)
      (for ([params  (in-list (get-all-label-params))])
        (send/apply pd draw-text params)))
    
    (define (draw-ticks)
      (for ([params  (in-list (get-all-tick-params))])
        (match-define (list major? v r angle) params)
        (if major? (send pd set-major-pen) (send pd set-minor-pen))
        (send pd draw-tick v r angle)))
    
    (define (draw-title)
      (when (and (plot-decorations?) (plot-title))
        (send pd draw-text (plot-title) (vector (* 1/2 (+ dc-x-min dc-x-max)) dc-y-min) 'top)))
    
    (define (draw-borders)
      (when (plot-decorations?)
        (put-minor-pen)
        (when (plot-x-axis?) (send pd draw-line
                                   (vector area-x-min area-y-max)
                                   (vector area-x-max area-y-max)))
        (when (plot-x-far-axis?) (send pd draw-line
                                       (vector area-x-min area-y-min)
                                       (vector area-x-max area-y-min)))
        (when (plot-y-axis?) (send pd draw-line
                                   (vector area-x-min area-y-min)
                                   (vector area-x-min area-y-max)))
        (when (plot-y-far-axis?) (send pd draw-line
                                       (vector area-x-max area-y-min)
                                       (vector area-x-max area-y-max)))))
    
    ;; ===============================================================================================
    ;; Public drawing control (used by plot/dc)
    
    (define/public (start-plot)
      (send pd reset-drawing-params)
      (send pd clear)
      (draw-borders)
      (draw-ticks))
    
    (define/public (start-renderer rx-min rx-max ry-min ry-max)
      (send pd reset-drawing-params)
      (send pd set-clipping-rect (vector (+ 1/2 (- area-x-min (plot-line-width)))
                                         (+ 1/2 (- area-y-min (plot-line-width))))
            (vector (+ area-x-max (plot-line-width))
                    (+ area-y-max (plot-line-width))))
      (clip-to-bounds rx-min rx-max ry-min ry-max))
    
    (define/public (end-renderers)
      (send pd clear-clipping-rect)
      (clip-to-none)
      (send pd reset-drawing-params)
      (draw-title)
      (draw-labels))
    
    (define/public (draw-legend legend-entries)
      (define gap-size (+ (pen-gap) tick-radius))
      (send pd draw-legend
            legend-entries
            (+ area-x-min gap-size) (- area-x-max gap-size)
            (+ area-y-min gap-size) (- area-y-max gap-size)))
    
    (define/public (end-plot)
      (send pd restore-drawing-params))
    
    ;; ===============================================================================================
    ;; Public drawing interface (used by renderers)
    
    (define/public (get-plot-device) pd)
    
    (define/public (put-alpha alpha) (send pd set-alpha alpha))
    
    (define/public (put-pen color width style) (send pd set-pen color width style))
    (define/public (put-major-pen [style 'solid]) (send pd set-major-pen style))
    (define/public (put-minor-pen [style 'solid]) (send pd set-minor-pen style))
    
    (define/public (put-brush color style) (send pd set-brush color style))
    
    (define/public (put-background color) (send pd set-background color))
    
    (define/public (put-font-size size) (send pd set-font-size size))
    (define/public (put-font-family family) (send pd set-font-family family))
    (define/public (put-font size family) (send pd set-font size family))
    (define/public (put-text-foreground color) (send pd set-text-foreground color))
    
    ;; Shapes
    
    (define/public (put-lines vs)
      (for ([vs  (vregular-sublists vs)])
        (for ([vs  (if clipping?
                       (in-list (clip-lines vs clip-x-min clip-x-max
                                            clip-y-min clip-y-max))
                       (in-value vs))])
          (when (not (empty? vs))
            (let ([vs  (if identity-transforms? vs (subdivide-lines plot->dc* vs))])
              (send pd draw-lines (map (λ (v) (plot->dc* v)) vs)))))))
    
    (define/public (put-line v1 v2)
      (when (and (vregular? v1) (vregular? v2))
        (let-values ([(v1 v2)  (if clipping?
                                   (clip-line v1 v2 clip-x-min clip-x-max
                                              clip-y-min clip-y-max)
                                   (values v1 v2))])
          (when (and v1 v2)
            (if identity-transforms?
                (send pd draw-line (plot->dc* v1) (plot->dc* v2))
                (send pd draw-lines (map (λ (v) (plot->dc* v))
                                         (subdivide-line plot->dc* v1 v2))))))))
    
    (define/public (put-polygon vs)
      (when (andmap vregular? vs)
        (let* ([vs  (if clipping?
                        (clip-polygon vs clip-x-min clip-x-max
                                      clip-y-min clip-y-max)
                        vs)])
          (when (not (empty? vs))
            (if identity-transforms?
                (send pd draw-polygon (map (λ (v) (plot->dc* v)) vs))
                (send pd draw-polygon (map (λ (v) (plot->dc* v))
                                           (subdivide-polygon plot->dc* vs))))))))
    
    (define/public (put-rectangle v1 v2)
      (when (and (vregular? v1) (vregular? v2))
        (let-values ([(v1 v2)  (if clipping?
                                   (clip-rectangle v1 v2 clip-x-min clip-x-max
                                                   clip-y-min clip-y-max)
                                   (values v1 v2))])
          (when (and v1 v2)
            (send pd draw-rectangle (plot->dc* v1) (plot->dc* v2))))))
    
    (define/public (put-text str v [anchor 'top-left] [angle 0]
                             #:outline? [outline? #f])
      (when (and (vregular? v) (in-bounds? v))
        (send pd draw-text str (plot->dc* v) anchor angle #:outline? outline?)))
    
    (define/public (put-glyphs vs symbol size)
      (send pd draw-glyphs (map (λ (v) (plot->dc* v))
                                (filter (λ (v) (and (vregular? v) (in-bounds? v)))
                                        vs))
            symbol size))
    
    (define/public (put-arrow v1 v2)
      (when (and (vregular? v1) (vregular? v2) (in-bounds? v1))
        (send pd draw-arrow (plot->dc* v1) (plot->dc* v2))))
    
    (define/public (put-tick v r angle)
      (when (and (vregular? v) (in-bounds? v))
        (send pd draw-tick (plot->dc* v) r angle)))
    ))
