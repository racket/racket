#lang racket/base

(require racket/draw racket/class racket/contract racket/match racket/math racket/list racket/string
         "../common/area.rkt"
         "../common/ticks.rkt"
         "../common/contract.rkt"
         "../common/math.rkt"
         "../common/axis-transform.rkt"
         "../common/sample.rkt"
         "../common/legend.rkt"
         "../common/parameters.rkt"
         "../common/utils.rkt"
         "clip.rkt")

(provide (all-defined-out))

(define plot2d-subdivisions (make-parameter 0))

(define 2d-plot-area%
  (class plot-area%
    (init-field rx-ticks rx-far-ticks ry-ticks ry-far-ticks
                x-min x-max y-min y-max)
    (init dc dc-x-min dc-y-min dc-x-size dc-y-size)
    (inherit
      set-alpha set-pen set-major-pen set-minor-pen set-brush set-background set-text-foreground
      set-font restore-drawing-params reset-drawing-params
      get-text-width get-text-extent get-char-height get-char-baseline
      set-clipping-rect clear-clipping-rect
      clear draw-polygon draw-rectangle draw-line draw-lines draw-text draw-glyphs draw-arrow
      draw-tick draw-legend-box)
    
    (super-make-object dc dc-x-min dc-y-min dc-x-size dc-y-size)
    
    (reset-drawing-params)
    
    (define char-height (get-char-height))    
    
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
    
    (match-define (invertible-function fx _) (apply-transform (plot-x-transform) x-min x-max))
    (match-define (invertible-function fy _) (apply-transform (plot-y-transform) y-min y-max))
    
    (define plot->view
      (cond [identity-transforms?  (λ (v) v)]
            [else  (λ (v) (match-let ([(vector x y)  v])
                            (vector (fx x) (fy y))))]))
    
    (define view->dc #f)
    (define/public (plot->dc v) (view->dc (plot->view v)))
    
    (define/public (plot-line->dc-angle v1 v2)
      (match-define (vector dx dy) (v- (plot->dc v1) (plot->dc v2)))
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
    
    (define ((x-tick-near? y) t1 t2)
      ((vmag (v- (plot->dc (vector (pre-tick-value t1) y))
                 (plot->dc (vector (pre-tick-value t2) y))))
       . <= . (* 3 (plot-line-width))))
    
    (define ((y-tick-near? x) t1 t2)
      ((vmag (v- (plot->dc (vector x (pre-tick-value t1)))
                 (plot->dc (vector x (pre-tick-value t2)))))
       . <= . (* 3 (plot-line-width))))
    
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
    
    (define (max-tick-offset ts)
      (cond [(empty? ts)  0]
            [(ormap pre-tick-major? ts)  (+ (pen-gap) (* 1/2 (plot-tick-size)))]
            [else  (+ (pen-gap) (* 1/4 (plot-tick-size)))]))
    
    (define max-x-tick-offset (if (plot-x-axis?) (max-tick-offset x-ticks) 0))
    (define max-y-tick-offset (if (plot-y-axis?) (max-tick-offset y-ticks) 0))
    (define max-x-far-tick-offset (if (plot-x-far-axis?) (max-tick-offset x-far-ticks) 0))
    (define max-y-far-tick-offset (if (plot-y-far-axis?) (max-tick-offset y-far-ticks) 0))
    
    (define (max-tick-label-height ts)
      (if (ormap pre-tick-major? ts) char-height 0))
    
    (define (max-tick-label-width ts)
      (apply max 0 (for/list ([t  (in-list ts)] #:when (pre-tick-major? t))
                     (get-text-width (tick-label t)))))
    
    (define max-x-tick-label-height (if (plot-x-axis?) (max-tick-label-height x-ticks) 0))
    (define max-y-tick-label-width (if (plot-y-axis?) (max-tick-label-width y-ticks) 0))
    (define max-x-far-tick-label-height (if (and (plot-x-far-axis?) draw-x-far-tick-labels?)
                                            (max-tick-label-height x-far-ticks)
                                            0))
    (define max-y-far-tick-label-width (if (and (plot-y-far-axis?) draw-y-far-tick-labels?)
                                           (max-tick-label-width y-far-ticks)
                                           0))
    
    ;; ===============================================================================================
    ;; Axis label parameters
    
    (define (get-x-label-params)
      (define offset (vector 0 (+ max-x-tick-offset
                                  max-x-tick-label-height
                                  (* 1/2 char-height))))
      (list (plot-x-label) (v+ (view->dc (vector (* 1/2 (+ x-min x-max)) y-min)) offset)
            'top))
    
    (define (get-y-label-params)
      (define offset (vector (+ max-y-tick-offset
                                max-y-tick-label-width
                                (* 1/2 char-height))
                             0))
      (list (plot-y-label) (v- (view->dc (vector x-min (* 1/2 (+ y-min y-max)))) offset)
            'bottom (/ pi 2)))
    
    (define (get-x-far-label-params)
      (define offset (vector 0 (+ max-x-far-tick-offset
                                  max-x-far-tick-label-height
                                  (* 1/2 char-height))))
      (list (plot-x-far-label) (v- (view->dc (vector (* 1/2 (+ x-min x-max)) y-max)) offset)
            'bottom))
    
    (define (get-y-far-label-params)
      (define offset (vector (+ max-y-far-tick-offset
                                max-y-far-tick-label-width
                                (* 1/2 char-height))
                             0))
      (list (plot-y-far-label) (v+ (view->dc (vector x-max (* 1/2 (+ y-min y-max)))) offset)
            'top (/ pi 2)))
    
    ;; ===============================================================================================
    ;; Tick label parameters
    
    (define (get-x-tick-label-params)
      (define offset (vector 0 (+ (pen-gap) (* 1/2 (plot-tick-size)))))
      (for/list ([t  (in-list x-ticks)] #:when (pre-tick-major? t))
        (match-define (tick x _ label) t)
        (list label (v+ (plot->dc (vector x y-min)) offset) 'top)))
    
    (define (get-y-tick-label-params)
      (define offset (vector (+ (pen-gap) (* 1/2 (plot-tick-size))) 0))
      (for/list ([t  (in-list y-ticks)] #:when (pre-tick-major? t))
        (match-define (tick y _ label) t)
        (list label (v- (plot->dc (vector x-min y)) offset) 'right)))
    
    (define (get-x-far-tick-label-params)
      (define offset (vector 0 (+ (pen-gap) (* 1/2 (plot-tick-size)))))
      (for/list ([t  (in-list x-far-ticks)] #:when (pre-tick-major? t))
        (match-define (tick x _ label) t)
        (list label (v- (plot->dc (vector x y-max)) offset) 'bottom)))
    
    (define (get-y-far-tick-label-params)
      (define offset (vector (+ (pen-gap) (* 1/2 (plot-tick-size))) 0))
      (for/list ([t  (in-list y-far-ticks)] #:when (pre-tick-major? t))
        (match-define (tick y _ label) t)
        (list label (v+ (plot->dc (vector x-max y)) offset) 'left)))
    
    ;; ===============================================================================================
    ;; Tick parameters
    
    (define (get-tick-params)
      (cond [(plot-decorations?)
             (define radius (* 1/2 (plot-tick-size)))
             (define 1/2radius (* 1/2 radius))
             (append
              (for/list ([t  (in-list (if (plot-x-axis?) x-ticks empty))])
                (match-define (tick x major? _) t)
                (list major? (plot->dc (vector x y-min)) (if major? radius 1/2radius) (* 1/2 pi)))
              (for/list ([t  (in-list (if (plot-y-axis?) y-ticks empty))])
                (match-define (tick y major? _) t)
                (list major? (plot->dc (vector x-min y)) (if major? radius 1/2radius) 0))
              (for/list ([t  (in-list (if (plot-x-far-axis?) x-far-ticks empty))])
                (match-define (tick x major? _) t)
                (list major? (plot->dc (vector x y-max)) (if major? radius 1/2radius) (* 1/2 pi)))
              (for/list ([t  (in-list (if (plot-y-far-axis?) y-far-ticks empty))])
                (match-define (tick y major? _) t)
                (list major? (plot->dc (vector x-max y)) (if major? radius 1/2radius) 0)))]
            [else  empty]))
    
    ;; ===============================================================================================
    ;; Fixpoint margin computation
    
    (define (get-label-params)
      (cond [(plot-decorations?)
             (append (if (plot-x-label) (list (get-x-label-params)) empty)
                     (if (plot-y-label) (list (get-y-label-params)) empty)
                     (if (plot-x-far-label) (list (get-x-far-label-params)) empty)
                     (if (plot-y-far-label) (list (get-y-far-label-params)) empty)
                     (if (plot-x-axis?) (get-x-tick-label-params) empty)
                     (if (plot-y-axis?) (get-y-tick-label-params) empty)
                     (if (and (plot-x-far-axis?) draw-x-far-tick-labels?)
                         (get-x-far-tick-label-params)
                         empty)
                     (if (and (plot-y-far-axis?) draw-y-far-tick-labels?)
                         (get-y-far-tick-label-params)
                         empty))]
            [else  empty]))
    
    (define (new-margins left right top bottom label-params tick-params)
      (match-define (list (vector label-xs label-ys) ...)
        (append* (map (λ (params) (send/apply this get-text-corners params)) label-params)))
      (match-define (list (vector tick-xs tick-ys) ...)
        (append* (map (λ (params) (send/apply this get-tick-endpoints (rest params))) tick-params)))
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
          (new-margins left right top bottom (get-label-params) (get-tick-params)))
        (set! view->dc (make-view->dc new-left new-right new-top new-bottom))
        ;(printf "margins: ~v ~v ~v ~v~n" new-left new-right new-top new-bottom)
        (values new-left new-right new-top new-bottom)))
    
    (define area-x-max (- dc-x-max right))
    (define area-y-max (- dc-y-max bottom))
    
    ;; ===============================================================================================
    ;; Plot decoration
    
    (define (draw-labels)
      (for ([params  (in-list (get-label-params))])
        (send/apply this draw-text params)))
    
    (define (draw-ticks)
      (for ([params  (in-list (get-tick-params))])
        (match-define (list major? v r angle) params)
        (if major? (set-major-pen) (set-minor-pen))
        (send this draw-tick v r angle)))
    
    (define (draw-title)
      (when (and (plot-decorations?) (plot-title))
        (draw-text (plot-title) (vector (* 1/2 (+ dc-x-min dc-x-max)) dc-y-min) 'top)))
    
    (define (draw-borders)
      (when (plot-decorations?)
        (set-minor-pen)
        (when (plot-x-axis?) (draw-line (vector area-x-min area-y-max)
                                        (vector area-x-max area-y-max)))
        (when (plot-x-far-axis?) (draw-line (vector area-x-min area-y-min)
                                            (vector area-x-max area-y-min)))
        (when (plot-y-axis?) (draw-line (vector area-x-min area-y-min)
                                        (vector area-x-min area-y-max)))
        (when (plot-y-far-axis?) (draw-line (vector area-x-max area-y-min)
                                            (vector area-x-max area-y-max)))))
    
    ;; ===============================================================================================
    ;; Drawing
    
    (define/public (start-plot)
      (reset-drawing-params)
      (clear)
      (draw-borders)
      (draw-ticks))
    
    (define/public (start-renderer rx-min rx-max ry-min ry-max)
      (reset-drawing-params)
      (set-clipping-rect (vector (+ 1/2 (- area-x-min (plot-line-width)))
                                 (+ 1/2 (- area-y-min (plot-line-width))))
                         (vector (+ area-x-max (plot-line-width))
                                 (+ area-y-max (plot-line-width))))
      (clip-to-bounds rx-min rx-max ry-min ry-max))
    
    (define/public (end-plot)
      (clear-clipping-rect)
      (clip-to-none)
      (reset-drawing-params)
      (draw-title)
      (draw-labels))
    
    (define/public (draw-legend legend-entries)
      (define gap-size (+ (pen-gap) (* 1/2 (plot-tick-size))))
      (draw-legend-box legend-entries
                       (+ area-x-min gap-size) (- area-x-max gap-size)
                       (+ area-y-min gap-size) (- area-y-max gap-size)))
    
    (define subdivide-fracs '(3/7 4/7 2/7 5/7 1/7 6/7))
    
    (define (subdivide-line v1 v2 [depth 10])
      (let/ec return
        (when (zero? depth) (return (list v1 v2)))
        
        (define dc-v1 (plot->dc v1))
        (define dc-v2 (plot->dc v2))
        (define dc-dv (v- dc-v2 dc-v1))
        (when ((vmag dc-dv) . <= . 3)
          (return (list v1 v2)))
        
        (define dv (v- v2 v1))
        (define-values (max-area vc)
          (for/fold ([max-area 0] [vc v1]) ([frac  (in-list subdivide-fracs)])
            (define test-vc (v+ (v* dv frac) v1))
            (define test-area (abs (vcross2 dc-dv (v- (plot->dc test-vc) dc-v1))))
            (cond [(test-area . > . max-area)  (values test-area test-vc)]
                  [else  (values max-area vc)])))
        (when (max-area . <= . 3) (return (list v1 v2)))
        
        ;(plot2d-subdivisions (+ (plot2d-subdivisions) 1))
        (append (subdivide-line v1 vc (- depth 1))
                (rest (subdivide-line vc v2 (- depth 1))))))
    
    (define (subdivide-lines vs)
      (append
       (append*
        (for/list ([v1  (in-list vs)] [v2  (in-list (rest vs))])
          (define line-vs (subdivide-line v1 v2))
          (take line-vs (sub1 (length line-vs)))))
       (list (last vs))))
    
    (define (subdivide-polygon vs)
      (subdivide-lines (append vs (list (first vs)))))
    
    (define/public (put-lines vs)
      (for ([vs  (vregular-sublists vs)])
        (for ([vs  (if clipping?
                       (in-list (clip-lines vs clip-x-min clip-x-max
                                            clip-y-min clip-y-max))
                       (in-value vs))])
          (when (not (empty? vs))
            (let ([vs  (if identity-transforms? vs (subdivide-lines vs))])
              (draw-lines (map (λ (v) (plot->dc v)) vs)))))))
    
    (define/public (put-line v1 v2)
      (when (and (vregular? v1) (vregular? v2))
        (let-values ([(v1 v2)  (if clipping?
                                   (clip-line v1 v2 clip-x-min clip-x-max
                                              clip-y-min clip-y-max)
                                   (values v1 v2))])
          (when (and v1 v2)
            (if identity-transforms?
                (draw-line (plot->dc v1) (plot->dc v2))
                (draw-lines (map (λ (v) (plot->dc v))
                                 (subdivide-line v1 v2))))))))
    
    (define/public (put-polygon vs)
      (when (andmap vregular? vs)
        (let* ([vs  (if clipping?
                        (clip-polygon vs clip-x-min clip-x-max
                                      clip-y-min clip-y-max)
                        vs)])
          (when (not (empty? vs))
            (if identity-transforms?
                (draw-polygon (map (λ (v) (plot->dc v)) vs))
                (draw-polygon (map (λ (v) (plot->dc v))
                                   (subdivide-polygon vs))))))))
    
    (define/public (put-rectangle v1 v2)
      (when (and (vregular? v1) (vregular? v2))
        (let-values ([(v1 v2)  (if clipping?
                                   (clip-rectangle v1 v2 clip-x-min clip-x-max
                                                   clip-y-min clip-y-max)
                                   (values v1 v2))])
          (when (and v1 v2)
            (draw-rectangle (plot->dc v1) (plot->dc v2))))))
    
    (define (in-bounds? v)
      (or (not clipping?)
          (point-in-bounds? v clip-x-min clip-x-max
                            clip-y-min clip-y-max)))
    
    (define/public (put-text str v [anchor 'top-left] [angle 0]
                             #:outline? [outline? #f])
      (when (and (vregular? v) (in-bounds? v))
        (draw-text str (plot->dc v) anchor angle #:outline? outline?)))
    
    (define/public (put-glyphs vs symbol size)
      (draw-glyphs (map (λ (v) (plot->dc v))
                        (filter (λ (v) (and (vregular? v) (in-bounds? v)))
                                vs))
                   symbol size))
    
    (define/public (put-arrow v1 v2)
      (when (and (vregular? v1) (vregular? v2) (in-bounds? v1))
        (draw-arrow (plot->dc v1) (plot->dc v2))))
    
    (define/public (put-tick v r angle)
      (when (and (vregular? v) (in-bounds? v))
        (draw-tick (plot->dc v) r angle)))
    ))
