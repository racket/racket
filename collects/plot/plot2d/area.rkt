#lang racket/base

(require racket/draw racket/class racket/contract racket/match racket/math racket/list
         "../common/area.rkt"
         "../common/ticks.rkt"
         "../common/vector.rkt"
         "../common/contract.rkt"
         "../common/math.rkt"
         "../common/axis-transform.rkt"
         "../common/sample.rkt"
         "../common/legend.rkt"
         "../common/parameters.rkt"
         "clip.rkt"
         "sample.rkt")

(provide 2d-plot-area%)

(define 2d-plot-area%
  (class plot-area%
    (init-field x-ticks y-ticks x-min x-max y-min y-max)
    (init dc dc-x-min dc-y-min dc-x-size dc-y-size)
    (inherit
      set-alpha set-pen set-major-pen set-minor-pen set-brush set-background set-text-foreground
      set-font restore-drawing-params reset-drawing-params
      get-text-width get-text-extent get-char-height get-char-baseline
      set-clipping-rect clear-clipping-rect
      clear draw-polygon draw-rectangle draw-line draw-lines draw-text draw-glyphs draw-arrow
      draw-tick draw-legend)
    
    (super-make-object dc dc-x-min dc-y-min dc-x-size dc-y-size)
    
    (reset-drawing-params)
    
    (define max-y-tick-label-width
      (for/fold ([max-w 0]) ([t  (in-list y-ticks)])
        (cond [(tick-major? t)  (define-values (w h _1 _2)
                                  (get-text-extent (tick-label t)))
                                (max max-w w)]
              [else  max-w])))
    
    (define char-height (get-char-height))
    
    
    (define last-x-tick-label-width
      (cond [(empty? x-ticks)  0]
            [else
             (define last-x-tick (argmax tick-p x-ticks))
             (cond [(tick-major? last-x-tick)  (define-values (w _1 _2 _3)
                                                 (get-text-extent
                                                  (tick-label last-x-tick)))
                                               w]
                   [else  0])]))
    
    (define dc-x-max (+ dc-x-min dc-x-size))
    (define dc-y-max (+ dc-y-min dc-y-size))
    
    (define area-x-min
      (+ dc-x-min
         (* 1/2 (plot-tick-size))                   ; y ticks
         (pen-gap) max-y-tick-label-width           ; y tick labels
         (if (plot-y-label) (* 3/2 char-height) 0)  ; y label
         ))
    
    (define area-x-max
      (- dc-x-max
         (max (* 1/2 last-x-tick-label-width)  ; protruding x tick label
              (* 1/2 (plot-tick-size)))        ; y ticks
         ))
    
    (define area-y-min
      (+ dc-y-min
         (if (plot-title) (* 3/2 char-height) 0)  ; title
         (max (* 1/2 char-height)                 ; protruding y tick label
              (* 1/2 (plot-tick-size)))           ; x ticks
         ))
    
    (define area-y-max
      (- dc-y-max
         (* 1/2 (plot-tick-size))                   ; x ticks
         (pen-gap) char-height                      ; x tick labels
         (if (plot-x-label) (* 3/2 char-height) 0)  ; x label
         ))
    
    (define area-x-size (- area-x-max area-x-min))
    (define area-y-size (- area-y-max area-y-min))
    (define area-x-mid (* 1/2 (+ area-x-min area-x-max)))
    (define area-y-mid (* 1/2 (+ area-y-min area-y-max)))
    
    (define x-size (- x-max x-min))
    (define y-size (- y-max y-min))
    
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
    (define/public (get-y-ticks) y-ticks)
    
    (define/public (get-x-min) x-min)
    (define/public (get-x-max) x-max)
    (define/public (get-y-min) y-min)
    (define/public (get-y-max) y-max)
    (define/public (get-bounds) (values x-min x-max y-min y-max))
    
    (define/public (get-clip-bounds)
      (cond [clipping?  (values clip-x-min clip-x-max clip-y-min clip-y-max)]
            [else       (values x-min x-max y-min y-max)]))
    
    (define/public (get-area-x-min) area-x-min)
    (define/public (get-area-x-max) area-x-max)
    (define/public (get-area-y-min) area-y-min)
    (define/public (get-area-y-max) area-y-max)
    
    (define/public (view->dc/x-size x)
      (* x (/ area-x-size x-size)))
    
    (define/public (view->dc/y-size y)
      (* y (/ area-y-size y-size)))
    
    (define/public (view->dc/angle a)
      (- (atan (view->dc/y-size (sin a))
               (view->dc/x-size (cos a)))))
    
    (define/public (view->dc/angle+mag a m)
      (define dx (view->dc/x-size (* m (cos a))))
      (define dy (view->dc/y-size (* m (sin a))))
      (values (atan (- dy) dx) (sqrt (+ (sqr dx) (sqr dy)))))
    
    (define/public (dc->view/x-size x)
      (* x (/ x-size area-x-size)))
    
    (define/public (dc->view/y-size y)
      (* y (/ y-size area-y-size)))
    
    (define/public (dc->view/angle a)
      (- (atan (dc->view/y-size (sin a))
               (dc->view/x-size (cos a)))))
    
    (define x-zero (view->dc/x-size (- x-min)))
    (define y-zero (view->dc/y-size (- y-min)))
    
    (define/public (view->dc xy)
      (match-define (vector x y) xy)
      (vector (+ area-x-min (+ x-zero (view->dc/x-size x)))
              (- area-y-max (+ y-zero (view->dc/y-size y)))))
    
    (define identity-transforms?
      (and (equal? (plot-x-transform) id-transform)
           (equal? (plot-y-transform) id-transform)))
    
    (define plot->view
      (cond [identity-transforms?  (λ (v) v)]
            [else
             (match-define (invertible-function fx _) ((plot-x-transform) x-min x-max))
             (match-define (invertible-function fy _) ((plot-y-transform) y-min y-max))
             (λ (v)
               (match-define (vector x y) v)
               (vector (fx x) (fy y)))]))
    
    (define/public (plot->dc v)
      (view->dc (plot->view v)))
    
    ;; -------------------------------------------------------------------------
    ;; Plot decoration
    
    (define (draw-borders)
      (set-minor-pen)
      (draw-rectangle (vector area-x-min area-y-min)
                      (vector area-x-max area-y-max)))
    
    (define (draw-x-ticks)
      (define half (* 1/2 (plot-tick-size)))
      (for ([t  (in-list x-ticks)])
        (match-define (tick x x-str major?) t)
        (if major? (set-major-pen) (set-minor-pen))
        (put-tick (vector x y-min) half 1/2pi)
        (put-tick (vector x y-max) half 1/2pi)))
    
    (define (draw-y-ticks)
      (define half (* 1/2 (plot-tick-size)))
      (for ([t  (in-list y-ticks)])
        (match-define (tick y y-str major?) t)
        (if major? (set-major-pen) (set-minor-pen))
        (put-tick (vector x-min y) half 0)
        (put-tick (vector x-max y) half 0)))
    
    (define (draw-x-tick-labels)
      (define offset (vector 0 (+ (pen-gap) (* 1/2 (plot-tick-size)))))
      (for ([t  (in-list (filter tick-major? x-ticks))])
        (match-define (tick x x-str major?) t)
        (draw-text x-str (v+ (plot->dc (vector x y-min)) offset) 'top)))
    
    (define (draw-y-tick-labels)
      (define offset (vector (+ (pen-gap) (* 1/2 (plot-tick-size))) 0))
      (for ([t  (in-list (filter tick-major? y-ticks))])
        (match-define (tick y y-str major?) t)
        (draw-text y-str (v- (plot->dc (vector x-min y)) offset) 'right)))
    
    (define (draw-title)
      (define-values (title-x-size _1 _2 _3)
        (get-text-extent (plot-title)))
      (draw-text (plot-title) (vector (* 1/2 (+ dc-x-min dc-x-max)) dc-y-min) 'top))
    
    (define (draw-x-label)
      (match-define (vector x _)
        (view->dc (vector (* 1/2 (+ x-min x-max)) 0)))
      (draw-text (plot-x-label) (vector x dc-y-max) 'bottom))
    
    (define (draw-y-label)
      (match-define (vector _ y)
        (view->dc (vector 0 (* 1/2 (+ y-min y-max)))))
      (draw-text (plot-y-label) (vector dc-x-min y) 'bottom (/ pi -2)))
    
    ;; -------------------------------------------------------------------------
    ;; Drawing
    
    (define/public (start-plot)
      (reset-drawing-params)
      (clear)
      (draw-borders)
      (draw-x-ticks)
      (draw-y-ticks)
      (draw-x-tick-labels)
      (draw-y-tick-labels))
    
    (define/public (start-renderer rx-min rx-max ry-min ry-max)
      (reset-drawing-params)
      (set-clipping-rect (vector (- area-x-min (plot-line-width))
                                 (- area-y-min (plot-line-width)))
                         (vector (+ area-x-max (plot-line-width))
                                 (+ area-y-max (plot-line-width))))
      (clip-to-bounds rx-min rx-max ry-min ry-max))
    
    (define/public (end-plot)
      (clear-clipping-rect)
      (clip-to-none)
      (reset-drawing-params)
      (when (plot-title) (draw-title))
      (when (plot-x-label) (draw-x-label))
      (when (plot-y-label) (draw-y-label)))
    
    (define/public (put-legend legend-entries)
      (define gap-size (+ (pen-gap) (* 1/2 (plot-tick-size))))
      (draw-legend legend-entries
                   (+ area-x-min gap-size) (- area-x-max gap-size)
                   (+ area-y-min gap-size) (- area-y-max gap-size)))
    
    (define (subdivide-line v1 v2)
      (let/ec return
        (match-define (vector dc-x1 dc-y1) (plot->dc v1))
        (match-define (vector dc-x2 dc-y2) (plot->dc v2))
        (define dc-dx (- dc-x2 dc-x1))
        (define dc-dy (- dc-y2 dc-y1))
        (when (or (zero? dc-dx) (zero? dc-dy)) (return (list v1 v2)))
        
        (match-define (vector x1 y1) v1)
        (match-define (vector x2 y2) v2)
        (cond [((abs dc-dx) . > . (abs dc-dy))
               (define num (+ 1 (inexact->exact (ceiling (* 1/3 (abs dc-dx))))))
               (define xs (nonlinear-seq x1 x2 num (plot-x-transform)))
               (define m (/ (- y2 y1) (- x2 x1)))
               (define b (- y1 (* m x1)))
               (define ys (map (λ (x) (+ (* m x) b)) xs))
               (map vector xs ys)]
              [else
               (define num (+ 1 (inexact->exact (ceiling (* 1/3 (abs dc-dy))))))
               (define ys (nonlinear-seq y1 y2 num (plot-y-transform)))
               (define m (/ (- x2 x1) (- y2 y1)))
               (define b (- x1 (* m y1)))
               (define xs (map (λ (y) (+ (* m y) b)) ys))
               (map vector xs ys)])))
    
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
