#lang racket/base

(require racket/class racket/match racket/list racket/math racket/contract
         "../common/math.rkt"
         "../common/area.rkt"
         "../common/ticks.rkt"
         "../common/draw.rkt"
         "../common/contract.rkt"
         "../common/axis-transform.rkt"
         "../common/parameters.rkt"
         "matrix.rkt"
         "shape.rkt"
         "clip.rkt")

(provide 3d-plot-area%)

(define 3d-plot-area%
  (class plot-area%
    (init-field rx-ticks rx-far-ticks ry-ticks ry-far-ticks rz-ticks rz-far-ticks
                x-min x-max y-min y-max z-min z-max)
    (init dc dc-x-min dc-y-min dc-x-size dc-y-size)
    (inherit
      set-alpha set-pen set-major-pen set-minor-pen set-brush set-background set-text-foreground
      set-font reset-drawing-params
      get-text-width get-text-extent get-char-height get-char-baseline
      set-clipping-rect clear-clipping-rect 
      clear draw-polygon draw-rectangle draw-line draw-lines draw-text draw-glyphs draw-arrow-glyph
      draw-tick draw-legend)
    
    (super-make-object dc dc-x-min dc-y-min dc-x-size dc-y-size)
    
    (reset-drawing-params)
    
    (define char-height (get-char-height))
    
    (define clipping? #f)
    (define clip-x-min x-min)
    (define clip-x-max x-max)
    (define clip-y-min y-min)
    (define clip-y-max y-max)
    (define clip-z-min z-min)
    (define clip-z-max z-max)
    
    (define/public (clip-to-bounds rx-min rx-max ry-min ry-max rz-min rz-max)
      (set! clipping? #t)
      (define cx-min (if rx-min (max* x-min rx-min) x-min))
      (define cx-max (if rx-max (min* x-max rx-max) x-max))
      (define cy-min (if ry-min (max* y-min ry-min) y-min))
      (define cy-max (if ry-max (min* y-max ry-max) y-max))
      (define cz-min (if rz-min (max* z-min rz-min) z-min))
      (define cz-max (if rz-max (min* z-max rz-max) z-max))
      (let ([cx-min  (min* cx-min cx-max)]
            [cx-max  (max* cx-min cx-max)]
            [cy-min  (min* cy-min cy-max)]
            [cy-max  (max* cy-min cy-max)]
            [cz-min  (min* cz-min cz-max)]
            [cz-max  (max* cz-min cz-max)])
        (set! clip-x-min cx-min)
        (set! clip-x-max cx-max)
        (set! clip-y-min cy-min)
        (set! clip-y-max cy-max)
        (set! clip-z-min cz-min)
        (set! clip-z-max cz-max)))
    
    (define (clip-to-none) (set! clipping? #f))
    
    (define (in-bounds? v)
      (or (not clipping?)
          (point-in-bounds? v clip-x-min clip-x-max clip-y-min clip-y-max clip-z-min clip-z-max)))
    
    (define/public (get-x-min) x-min)
    (define/public (get-x-max) x-max)
    (define/public (get-y-min) y-min)
    (define/public (get-y-max) y-max)
    (define/public (get-z-min) z-min)
    (define/public (get-z-max) z-max)
    (define/public (get-bounds) (values x-min x-max y-min y-max z-min z-max))
    
    (define/public (get-clip-bounds)
      (cond [clipping?  (values clip-x-min clip-x-max clip-y-min clip-y-max clip-z-min clip-z-max)]
            [else       (values x-min x-max y-min y-max z-min z-max)]))
    
    (define x-size (- x-max x-min))
    (define y-size (- y-max y-min))
    (define z-size (- z-max z-min))
    
    (define x-mid (* 1/2 (+ x-min x-max)))
    (define y-mid (* 1/2 (+ y-min y-max)))
    (define z-mid (* 1/2 (+ z-min z-max)))
    
    (define angle (plot3d-angle))
    (define altitude (plot3d-altitude))
    ;; FLOATING-POINT HACK: Adding an epsilon to the angle ensures that, when it is 90, 180
    ;; or 270, the x/y/z tick labels are drawn on the left side.
    (define theta (+ (degrees->radians angle) 0.00001))
    (define rho (degrees->radians altitude))
    
    (define do-axis-transforms? #f)
    
    (define identity-axis-transforms?
      (and (equal? (plot-x-transform) id-transform)
           (equal? (plot-y-transform) id-transform)
           (equal? (plot-z-transform) id-transform)))
    
    (match-define (invertible-function fx _) (apply-transform (plot-x-transform) x-min x-max))
    (match-define (invertible-function fy _) (apply-transform (plot-y-transform) y-min y-max))
    (match-define (invertible-function fz _) (apply-transform (plot-z-transform) z-min z-max))
    
    (define center
      (cond
        [identity-axis-transforms?
         (λ (v)
           (match-define (vector x y z) v)
           (vector (- x x-mid) (- y y-mid) (- z z-mid)))]
        [else
         (λ (v)
           (match-define (vector x y z) v)
           (if do-axis-transforms?
               (vector (- (fx x) x-mid) (- (fy y) y-mid) (- (fz z) z-mid))
               (vector (- x x-mid) (- y y-mid) (- z z-mid))))]))
    
    (define transform-matrix/no-rho
      (m3* (m3-rotate-z theta) (m3-scale (/ x-size) (/ y-size) (/ z-size))))
    (define transform-matrix (m3* (m3-rotate-x rho) transform-matrix/no-rho))
    
    (define (plot->view v) (m3-apply transform-matrix (center v)))
    (define (plot->view/no-rho v) (m3-apply transform-matrix/no-rho (center v)))
    (define (rotate/rho v) (m3-apply (m3-rotate-x rho) v))
    
    (define view->dc #f)
    (define (plot->dc v) (view->dc (plot->view v)))
    
    (define dc-x-max (+ dc-x-min dc-x-size))
    (define dc-y-max (+ dc-y-min dc-y-size))
    
    (define (make-view->dc left right top bottom)
      (define corners (list (vector x-min y-min z-min) (vector x-min y-min z-max)
                            (vector x-min y-max z-min) (vector x-min y-max z-max)
                            (vector x-max y-min z-min) (vector x-max y-min z-max)
                            (vector x-max y-max z-min) (vector x-max y-max z-max)))
      (match-define (list (vector xs ys zs) ...) (map plot->view corners))
      (define view-x-min (apply min xs))
      (define view-x-max (apply max xs))
      (define view-y-min (apply min ys))
      (define view-y-max (apply max ys))
      (define view-z-min (apply min zs))
      (define view-z-max (apply max zs))
      
      (define area-x-min (+ dc-x-min left))
      (define area-x-max (- dc-x-max right))
      (define area-y-min (+ dc-y-min top))
      (define area-y-max (- dc-y-max bottom))
      (define area-x-mid (* 1/2 (+ area-x-min area-x-max)))
      (define area-x-size (- area-x-max area-x-min))
      (define area-y-mid (* 1/2 (+ area-y-min area-y-max)))
      (define area-y-size (- area-y-max area-y-min))
      
      (define area-per-view-x (/ area-x-size (- view-x-max view-x-min)))
      (define area-per-view-z (/ area-y-size (- view-z-max view-z-min)))
      (λ (v)
        (match-define (vector x y z) v)
        (let ([x  (* x area-per-view-x)] [z  (* z area-per-view-z)])
          (vector (+ area-x-mid x) (- area-y-mid z)))))
    
    ;; Initial view->dc
    (define init-top-margin (if (and (plot-decorations?) (plot-title)) (* 3/2 (get-char-height)) 0))
    (set! view->dc (make-view->dc 0 0 init-top-margin 0))
    
    ;; ===============================================================================================
    ;; Tick and label constants
    
    (define x-labels-y-min? ((cos theta) . >= . 0))
    (define y-labels-x-min? ((sin theta) . >= . 0))
    
    (define x-ticks (filter (λ (t) (<= x-min (pre-tick-value t) x-max)) rx-ticks))
    (define y-ticks (filter (λ (t) (<= y-min (pre-tick-value t) y-max)) ry-ticks))
    (define z-ticks (filter (λ (t) (<= z-min (pre-tick-value t) z-max)) rz-ticks))
    
    (define x-far-ticks (filter (λ (t) (<= x-min (pre-tick-value t) x-max)) rx-far-ticks))
    (define y-far-ticks (filter (λ (t) (<= y-min (pre-tick-value t) y-max)) ry-far-ticks))
    (define z-far-ticks (filter (λ (t) (<= z-min (pre-tick-value t) z-max)) rz-far-ticks))
    
    (define draw-x-far-tick-labels? (not (and (plot-x-axis?) (equal? x-ticks x-far-ticks))))
    (define draw-y-far-tick-labels? (not (and (plot-y-axis?) (equal? y-ticks y-far-ticks))))
    (define draw-z-far-tick-labels? (not (and (plot-z-axis?) (equal? z-ticks z-far-ticks))))
    
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
    
    (define (max-x-tick-label-diag y-axis-angle)
      (if (plot-x-axis?)
          (+ (* (abs (cos y-axis-angle)) (max-tick-label-width x-ticks)) 
             (* (abs (sin y-axis-angle)) (max-tick-label-height x-ticks)))
          0))
    
    (define (max-y-tick-label-diag x-axis-angle)
      (if (plot-y-axis?)
          (+ (* (abs (cos x-axis-angle)) (max-tick-label-width y-ticks))
             (* (abs (sin x-axis-angle)) (max-tick-label-height y-ticks)))
          0))
    
    (define (max-x-far-tick-label-diag y-axis-angle)
      (if (and (plot-x-far-axis?) draw-x-far-tick-labels?)
          (+ (* (abs (cos y-axis-angle)) (max-tick-label-width x-far-ticks)) 
             (* (abs (sin y-axis-angle)) (max-tick-label-height x-far-ticks)))
          0))
    
    (define (max-y-far-tick-label-diag x-axis-angle)
      (if (and (plot-y-far-axis?) draw-y-far-tick-labels?)
          (+ (* (abs (cos x-axis-angle)) (max-tick-label-width y-far-ticks))
             (* (abs (sin x-axis-angle)) (max-tick-label-height y-far-ticks)))
          0))
    
    (define (plot-dir->dc-angle v)
      (match-define (vector dx dy)
        (v- (plot->dc (v+ v (vector x-mid y-mid z-mid)))
            (plot->dc (vector x-mid y-mid z-mid))))
      (- (atan2 (- dy) dx)))
    
    (define (axis-dc-angles)
      (values (plot-dir->dc-angle (vector 1 0 0))
              (plot-dir->dc-angle (vector 0 1 0))))
    
    ;; ===============================================================================================
    ;; Axis label parameters
    
    (define (get-x-label-params)
      (define-values (x-axis-angle y-axis-angle) (axis-dc-angles))
      (define v0 (plot->dc (vector x-mid (if x-labels-y-min? y-min y-max) z-min)))
      (define dist (+ max-x-tick-offset
                      (max-x-tick-label-diag y-axis-angle)
                      (* 1/2 char-height)))
      (list #t (plot-x-label) (v+ v0 (v* (vector (cos y-axis-angle) (sin y-axis-angle))
                                         (if x-labels-y-min? (- dist) dist)))
            'top (- (if x-labels-y-min? 0 pi) x-axis-angle)))
    
    (define (get-y-label-params)
      (define-values (x-axis-angle y-axis-angle) (axis-dc-angles))
      (define v0 (plot->dc (vector (if y-labels-x-min? x-min x-max) y-mid z-min)))
      (define dist (+ max-y-tick-offset
                      (max-y-tick-label-diag x-axis-angle)
                      (* 1/2 char-height)))
      (list #t (plot-y-label) (v+ v0 (v* (vector (cos x-axis-angle) (sin x-axis-angle))
                                         (if y-labels-x-min? (- dist) dist)))
            'top (- (if y-labels-x-min? pi 0) y-axis-angle)))
    
    (define (get-z-label-params)
      (define x (if x-labels-y-min? x-min x-max))
      (define y (if y-labels-x-min? y-max y-min))
      (list #t (plot-z-label) (v+ (plot->dc (vector x y z-max))
                                  (vector 0 (* -1/2 char-height)))
            'bottom-left 0))
    
    (define (get-x-far-label-params)
      (define-values (x-axis-angle y-axis-angle) (axis-dc-angles))
      (define v0 (plot->dc (vector x-mid (if x-labels-y-min? y-max y-min) z-min)))
      (define dist (+ max-x-far-tick-offset
                      (max-x-far-tick-label-diag y-axis-angle)
                      (* 1/2 char-height)))
      (list #f (plot-x-far-label) (v+ v0 (v* (vector (cos y-axis-angle) (sin y-axis-angle))
                                             (if x-labels-y-min? dist (- dist))))
            'bottom (- (if x-labels-y-min? 0 pi) x-axis-angle)))
    
    (define (get-y-far-label-params)
      (define-values (x-axis-angle y-axis-angle) (axis-dc-angles))
      (define v0 (plot->dc (vector (if y-labels-x-min? x-max x-min) y-mid z-min)))
      (define dist (+ max-y-far-tick-offset
                      (max-y-far-tick-label-diag x-axis-angle)
                      (* 1/2 char-height)))
      (list #f (plot-y-far-label) (v+ v0 (v* (vector (cos x-axis-angle) (sin x-axis-angle))
                                             (if y-labels-x-min? dist (- dist))))
            'bottom (- (if y-labels-x-min? pi 0) y-axis-angle)))
    
    (define (get-z-far-label-params)
      (define x (if x-labels-y-min? x-max x-min))
      (define y (if y-labels-x-min? y-min y-max))
      (list #t (plot-z-far-label) (v+ (plot->dc (vector x y z-max))
                                      (vector 0 (* -1/2 char-height)))
            'bottom-right 0))
    
    ;; ===============================================================================================
    ;; Tick label parameters
    
    (define (opposite-anchor a)
      (case a
        [(top-left)  'bottom-right] [(top)  'bottom] [(top-right)  'bottom-left] [(right)  'left]
        [(bottom-right)  'top-left] [(bottom)  'top] [(bottom-left)  'top-right] [(left)  'right]))
    
    (define x-tick-label-anchor
      (let ([s  (sin theta)])
        (cond [(s . < . (sin (degrees->radians -67.5)))  (if x-labels-y-min? 'top-right 'top-left)]
              [(s . < . (sin (degrees->radians -22.5)))  (if x-labels-y-min? 'top-right 'top-left)]
              [(s . < . (sin (degrees->radians 22.5)))   'top]
              [(s . < . (sin (degrees->radians 67.5)))   (if x-labels-y-min? 'top-left 'top-right)]
              [else                                      (if x-labels-y-min? 'top-left 'top-right)])))
    
    (define y-tick-label-anchor
      (let ([c  (cos theta)])
        (cond [(c . > . (cos (degrees->radians 22.5)))   (if y-labels-x-min? 'top-right 'top-left)]
              [(c . > . (cos (degrees->radians 67.5)))   (if y-labels-x-min? 'top-right 'top-left)]
              [(c . > . (cos (degrees->radians 112.5)))  'top]
              [(c . > . (cos (degrees->radians 157.5)))  (if y-labels-x-min? 'top-left 'top-right)]
              [else                                      (if y-labels-x-min? 'top-left 'top-right)])))
    
    (define x-far-tick-label-anchor (opposite-anchor x-tick-label-anchor))
    (define y-far-tick-label-anchor (opposite-anchor y-tick-label-anchor))
    
    (define (get-x-tick-label-params)
      (define y-axis-angle (plot-dir->dc-angle (vector 0 1 0)))
      (define dist (+ (pen-gap) (* 1/2 (plot-tick-size))))
      (define offset (v* (vector (cos y-axis-angle) (sin y-axis-angle))
                         (if x-labels-y-min? (- dist) dist)))
      (define y (if x-labels-y-min? y-min y-max))
      (for/list ([t  (in-list x-ticks)] #:when (pre-tick-major? t))
        (match-define (tick x _ label) t)
        (list #f label (v+ (plot->dc (vector (fx x) y z-min)) offset) x-tick-label-anchor 0)))
    
    (define (get-y-tick-label-params)
      (define x-axis-angle (plot-dir->dc-angle (vector 1 0 0)))
      (define dist (+ (pen-gap) (* 1/2 (plot-tick-size))))
      (define offset (v* (vector (cos x-axis-angle) (sin x-axis-angle))
                         (if y-labels-x-min? (- dist) dist)))
      (define x (if y-labels-x-min? x-min x-max))
      (for/list ([t  (in-list y-ticks)] #:when (pre-tick-major? t))
        (match-define (tick y _ label) t)
        (list #f label (v+ (plot->dc (vector x (fy y) z-min)) offset) y-tick-label-anchor 0)))
    
    (define (get-z-tick-label-params)
      (define dist (+ (pen-gap) (* 1/2 (plot-tick-size))))
      (define offset (vector (- dist) (* 2 (get-char-baseline))))
      (define x (if x-labels-y-min? x-min x-max))
      (define y (if y-labels-x-min? y-max y-min))
      (for/list ([t  (in-list z-ticks)] #:when (pre-tick-major? t))
        (match-define (tick z _ label) t)
        (list #t label (v+ (plot->dc (vector x y (fz z))) offset) 'bottom-right 0)))
    
    (define (get-x-far-tick-label-params)
      (define y-axis-angle (plot-dir->dc-angle (vector 0 1 0)))
      (define dist (+ (pen-gap) (* 1/2 (plot-tick-size))))
      (define offset (v* (vector (cos y-axis-angle) (sin y-axis-angle))
                         (if x-labels-y-min? dist (- dist))))
      (define y (if x-labels-y-min? y-max y-min))
      (for/list ([t  (in-list x-far-ticks)] #:when (pre-tick-major? t))
        (match-define (tick x _ label) t)
        (list #f label (v+ (plot->dc (vector (fx x) y z-min)) offset) x-far-tick-label-anchor 0)))
    
    (define (get-y-far-tick-label-params)
      (define x-axis-angle (plot-dir->dc-angle (vector 1 0 0)))
      (define dist (+ (pen-gap) (* 1/2 (plot-tick-size))))
      (define offset (v* (vector (cos x-axis-angle) (sin x-axis-angle))
                         (if y-labels-x-min? dist (- dist))))
      (define x (if y-labels-x-min? x-max x-min))
      (for/list ([t  (in-list y-far-ticks)] #:when (pre-tick-major? t))
        (match-define (tick y _ label) t)
        (list #f label (v+ (plot->dc (vector x (fy y) z-min)) offset) y-far-tick-label-anchor 0)))
    
    (define (get-z-far-tick-label-params)
      (define dist (+ (pen-gap) (* 1/2 (plot-tick-size))))
      (define offset (vector dist (* 2 (get-char-baseline))))
      (define x (if x-labels-y-min? x-max x-min))
      (define y (if y-labels-x-min? y-min y-max))
      (for/list ([t  (in-list z-far-ticks)] #:when (pre-tick-major? t))
        (match-define (tick z _ label) t)
        (list #t label (v+ (plot->dc (vector x y (fz z))) offset) 'bottom-left 0)))
    
    ;; ===============================================================================================
    ;; Tick parameters
    
    (define (get-x-tick-params)
      (define radius (* 1/2 (plot-tick-size)))
      (define angle (plot-dir->dc-angle (vector 0 1 0)))
      (define y (if x-labels-y-min? y-min y-max))
      (for/list ([t  (in-list x-ticks)])
        (match-define (tick x major? _) t)
        (list major? (plot->dc (vector (fx x) y z-min)) (if major? radius (* 1/2 radius)) angle)))
    
    (define (get-y-tick-params)
      (define radius (* 1/2 (plot-tick-size)))
      (define angle (plot-dir->dc-angle (vector 1 0 0)))
      (define x (if y-labels-x-min? x-min x-max))
      (for/list ([t  (in-list y-ticks)])
        (match-define (tick y major? _) t)
        (list major? (plot->dc (vector x (fy y) z-min)) (if major? radius (* 1/2 radius)) angle)))
    
    (define (get-z-tick-params)
      (define radius (* 1/2 (plot-tick-size)))
      (define x (if x-labels-y-min? x-min x-max))
      (define y (if y-labels-x-min? y-max y-min))
      (for/list ([t  (in-list z-ticks)])
        (match-define (tick z major? _) t)
        (list major? (plot->dc (vector x y (fz z))) (if major? radius (* 1/2 radius)) 0)))
    
    (define (get-x-far-tick-params)
      (define radius (* 1/2 (plot-tick-size)))
      (define angle (plot-dir->dc-angle (vector 0 1 0)))
      (define y (if x-labels-y-min? y-max y-min))
      (for/list ([t  (in-list x-ticks)])
        (match-define (tick x major? _) t)
        (list major? (plot->dc (vector (fx x) y z-min)) (if major? radius (* 1/2 radius)) angle)))
    
    (define (get-y-far-tick-params)
      (define radius (* 1/2 (plot-tick-size)))
      (define angle (plot-dir->dc-angle (vector 1 0 0)))
      (define x (if y-labels-x-min? x-max x-min))
      (for/list ([t  (in-list y-ticks)])
        (match-define (tick y major? _) t)
        (list major? (plot->dc (vector x (fy y) z-min)) (if major? radius (* 1/2 radius)) angle)))
    
    (define (get-z-far-tick-params)
      (define radius (* 1/2 (plot-tick-size)))
      (define x (if x-labels-y-min? x-max x-min))
      (define y (if y-labels-x-min? y-min y-max))
      (for/list ([t  (in-list z-ticks)])
        (match-define (tick z major? _) t)
        (list major? (plot->dc (vector x y (fz z))) (if major? radius (* 1/2 radius)) 0)))
    
    ;; ===============================================================================================
    ;; Fixpoint margin computation
    
    (define (get-near-label-params)
      (cond [(plot-decorations?)
             (append (if (plot-z-label) (list (get-z-label-params)) empty)
                     (if (plot-z-far-label) (list (get-z-far-label-params)) empty)
                     (if (plot-z-axis?) (get-z-tick-label-params) empty)
                     (if (and (plot-z-far-axis?) draw-z-far-tick-labels?)
                         (get-z-far-tick-label-params)
                         empty))]
            [else  empty]))
    
    (define (get-far-label-params)
      (cond [(plot-decorations?)
             ;; Ordered back-to-front
             (append (if (plot-x-far-label) (list (get-x-far-label-params)) empty)
                     (if (plot-y-far-label) (list (get-y-far-label-params)) empty)
                     (if (and (plot-x-far-axis?) draw-x-far-tick-labels?)
                         (get-x-far-tick-label-params)
                         empty)
                     (if (and (plot-y-far-axis?) draw-y-far-tick-labels?)
                         (get-y-far-tick-label-params)
                         empty)
                     (if (plot-x-axis?) (get-x-tick-label-params) empty)
                     (if (plot-y-axis?) (get-y-tick-label-params) empty)
                     (if (plot-x-label) (list (get-x-label-params)) empty)
                     (if (plot-y-label) (list (get-y-label-params)) empty))]
            [else  empty]))
    
    (define (get-near-tick-params)
      (cond [(plot-decorations?)
             (append (if (plot-z-axis?) (get-z-tick-params) empty)
                     (if (plot-z-far-axis?) (get-z-far-tick-params) empty))]
            [else  empty]))
    
    (define (get-far-tick-params)
      (cond [(plot-decorations?)
             (append (if (plot-x-axis?) (get-x-tick-params) empty)
                     (if (plot-y-axis?) (get-y-tick-params) empty)
                     (if (plot-x-far-axis?) (get-x-far-tick-params) empty)
                     (if (plot-y-far-axis?) (get-y-far-tick-params) empty))]
            [else  empty]))
    
    ;; We have a mutual dependence problem:
    ;; 1. We can't set the margins without knowing where the axis labels will be
    ;; 2. We can't determine the axis label angles (and thus their positions) without knowing the
    ;;    margins
    
    ;; The margins could be solved exactly using algebra and trigonometry, but the solutions wouldn't
    ;; be robust, as small changes to the layout algorithms would invalidate them.
    
    ;; So we use a fixpoint solution:
    ;; 1. Define 'new-margins', which takes the current margins and info about the current labels,
    ;;    and returns margins large enough that the current axis labels would be drawn completely
    ;;    on the dc (although at slightly wrong angles)
    ;; 2. Iterate 'new-margins', recalculating the labels every iteration
    
    ;; Because 'new-margins' is monotone and bounded, the amount of axis label drawn off the dc is
    ;; zero in the limit. In practice, only a few iterations achieves less than 1 drawing unit.
    
    (define (new-margins left right top bottom label-params tick-params)
      (match-define (list (vector label-xs label-ys) ...)
        (append* (map (λ (params) (send/apply this get-text-corners (rest params))) label-params)))
      (match-define (list (vector tick-xs tick-ys) ...)
        (append* (map (λ (params) (send/apply this get-tick-endpoints (rest params))) tick-params)))
      (define xs (append label-xs tick-xs))
      (define ys (append label-ys tick-ys))
      
      (define param-x-min (apply min dc-x-min xs))
      (define param-x-max (apply max (sub1 dc-x-max) xs))
      (define param-y-min (apply min dc-y-min ys))
      (define param-y-max (apply max (sub1 dc-y-max) ys))
      
      (values (+ left (- dc-x-min param-x-min))
              (- right (- (sub1 dc-x-max) param-x-max))
              (+ top (- dc-y-min param-y-min))
              (- bottom (- (sub1 dc-y-max) param-y-max))))
    
    (define-values (area-x-min right area-y-min bottom)
      (for/fold ([left 0] [right 0] [top init-top-margin] [bottom 0]) ([i  (in-range 3)])
        (define-values (new-left new-right new-top new-bottom)
          (new-margins left right top bottom
                       (append (get-near-label-params) (get-far-label-params))
                       (append (get-near-tick-params) (get-far-tick-params))))
        (set! view->dc (make-view->dc new-left new-right new-top new-bottom))
        ;(printf "margins: ~v ~v ~v ~v~n" new-left new-right new-top new-bottom)
        (values new-left new-right new-top new-bottom)))
    
    ;; ===============================================================================================
    
    (define (draw-ticks tick-params)
      (for ([params  (in-list tick-params)])
        (match-define (list major? v r angle) params)
        (if major? (set-major-pen) (set-minor-pen))
        (send this draw-tick v r angle)))
    
    (define (draw-labels label-params)
      (for ([params  (in-list label-params)])
        (send/apply this draw-text (rest params) #:outline? (first params))))
    
    (define (draw-far-borders)
      (when (plot-decorations?)
        (set-minor-pen)
        (define near-x (if y-labels-x-min? x-min x-max))
        (define near-y (if x-labels-y-min? y-min y-max))
        (define far-x (if y-labels-x-min? x-max x-min))
        (define far-y (if x-labels-y-min? y-max y-min))
        (when (plot-x-axis?)
          (draw-line (plot->dc (vector x-min near-y z-min))
                     (plot->dc (vector x-max near-y z-min))))
        (when (plot-x-far-axis?)
          (draw-line (plot->dc (vector x-min far-y z-min))
                     (plot->dc (vector x-max far-y z-min))))
        (when (plot-y-axis?)
          (draw-line (plot->dc (vector near-x y-min z-min))
                     (plot->dc (vector near-x y-max z-min))))
        (when (plot-y-far-axis?)
          (draw-line (plot->dc (vector far-x y-min z-min))
                     (plot->dc (vector far-x y-max z-min))))))
    
    (define (draw-near-borders)
      (when (plot-decorations?)
        (set-minor-pen)
        (define near-x (if x-labels-y-min? x-min x-max))
        (define near-y (if y-labels-x-min? y-max y-min))
        (define far-x (if x-labels-y-min? x-max x-min))
        (define far-y (if y-labels-x-min? y-min y-max))
        (when (plot-z-axis?)
          (draw-line (plot->dc (vector near-x near-y z-min))
                     (plot->dc (vector near-x near-y z-max))))
        (when (plot-z-far-axis?)
          (draw-line (plot->dc (vector far-x far-y z-min))
                     (plot->dc (vector far-x far-y z-max))))))
    
    (define (draw-title)
      (when (and (plot-decorations?) (plot-title))
        (draw-text (plot-title) (vector (* 1/2 (+ dc-x-min dc-x-max)) dc-y-min) 'top)))
    
    (define/public (start-plot)
      (reset-drawing-params)
      (clear)
      (set! render-list empty)
      (draw-labels (get-far-label-params))
      (draw-ticks (get-far-tick-params))
      (draw-far-borders)
      (set! do-axis-transforms? #t))
    
    (define/public (start-renderer rx-min rx-max ry-min ry-max rz-min rz-max)
      (reset-drawing-params)
      (clip-to-bounds rx-min rx-max ry-min ry-max rz-min rz-max))
    
    (define/public (end-plot)
      (set! do-axis-transforms? #f)
      (draw-render-list)
      (clip-to-none)
      (reset-drawing-params)
      (draw-title)
      (draw-near-borders)
      (draw-ticks (get-near-tick-params))
      (draw-labels (get-near-label-params)))
    
    (define (put-major-pen) (put-pen (plot-foreground) (plot-line-width) 'solid))
    (define (put-minor-pen) (put-pen (plot-foreground) (* 1/2 (plot-line-width)) 'solid))
    
    (define (put-angles*)
      (define angle-str (format " angle = ~a " (number->string (round angle))))
      (define alt-str (format " altitude = ~a " (number->string (round altitude))))
      (define-values (angle-width angle-height baseline _angle2) (get-text-extent angle-str))
      (define-values (alt-width alt-height _alt1 _alt2) (get-text-extent alt-str))
      
      (define box-x-size (max angle-width alt-width))
      (define box-y-size (+ angle-height alt-height (* 3 baseline)))
      (define box-x-min (+ dc-x-min (* 1/2 (- dc-x-size box-x-size))))
      (define box-y-min (+ dc-y-min (* 1/2 (- dc-y-size box-y-size))))
      (define box-x-max (+ box-x-min box-x-size))
      (define box-y-max (+ box-y-min box-y-size))
      
      (set-alpha 1/2)
      (set-minor-pen)
      (set-brush (plot-background) 'solid)
      (draw-rectangle (vector box-x-min box-y-min) (vector box-x-max box-y-max))
      
      (set-alpha 1)
      (draw-text angle-str (vector box-x-min (+ box-y-min baseline))
                 'top-left #:outline? #t)
      (draw-text alt-str (vector box-x-min (+ box-y-min baseline char-height))
                 'top-left #:outline? #t))
    
    (define/public (put-angles) (put-angles*))
    
    (define (put-legend* legend-entries)
      (define gap (plot-line-width))
      (draw-legend legend-entries
                   (+ dc-x-min gap) (- dc-x-max gap)
                   (+ area-y-min gap) (- dc-y-max gap)))
    
    (define/public (put-legend legend-entries) (put-legend* legend-entries))
    
    (define light (plot->view (vector x-mid y-mid (+ z-max (* 5 z-size)))))
    (define view-dir (vector 0 -50 0))
    
    (define diffuse-light? (plot3d-diffuse-light?))
    (define specular-light? (plot3d-specular-light?))
    (define ambient-light (plot3d-ambient-light))
    
    (define get-light-values
      (cond
        [(not (or diffuse-light? specular-light?))  (λ (s) (values 1.0 0.0))]
        [else
         (λ (s)
           ; common lighting values
           (define light-dir (vnormalize (v- light (rotate/rho (shape-center s)))))
           (define norm (shape-normal s))
           ; diffuse lighting: typical Lambertian surface model
           (define diff (if diffuse-light? (abs (vdot norm light-dir)) 1.0))
           ; specular highlighting: Blinn-Phong model
           (define spec (cond [specular-light?
                               (define lv (v* (v+ light-dir view-dir) 1/2))
                               (define cos-angle (/ (abs (vdot norm lv)) (vmag lv)))
                               (* 32 (expt cos-angle 10))]
                              [else  0.0]))
           ; put it all together
           (values (+ ambient-light (* (- 1 ambient-light) diff)) spec))]))
    
    (define (draw-shapes lst)
      (for ([s  (in-list (depth-sort lst))])
        (set-alpha (shape-alpha s))
        (match s
          ; shapes
          [(shapes alpha center ss)  (draw-shapes ss)]
          ; polygon
          [(polygon alpha center vs pen-color pen-width pen-style brush-color brush-style)
           (define-values (diff spec) (get-light-values s))
           (let ([pen-color  (map (λ (v) (+ (* v diff) spec)) pen-color)]
                 [brush-color  (map (λ (v) (+ (* v diff) spec)) brush-color)])
             (set-pen pen-color pen-width pen-style)
             (set-brush brush-color brush-style)
             (draw-polygon (map (λ (v) (view->dc v)) vs)))]
          ; line
          [(line alpha center v1 v2 pen-color pen-width pen-style)
           (set-pen pen-color pen-width pen-style)
           (draw-line (view->dc v1) (view->dc v2))]
          ; text
          [(text alpha center anchor angle str font-size font-family color)
           (set-font font-size font-family)
           (set-text-foreground color)
           (draw-text str (view->dc (rotate/rho center)) anchor angle)]
          ; glyph
          [(glyph alpha center symbol size pen-color pen-width pen-style brush-color brush-style)
           (set-pen pen-color pen-width pen-style)
           (set-brush brush-color brush-style)
           (draw-glyphs (list (view->dc (rotate/rho center))) symbol size)]
          ; tick glyph
          [(tick-glyph alpha center radius angle pen-color pen-width pen-style)
           (set-pen pen-color pen-width pen-style)
           (draw-tick (view->dc (rotate/rho center)) radius angle)]
          [_  (error 'end-plot "shape not implemented: ~e" s)])))
    
    ;; ===============================================================================================
    ;; Delayed drawing
    
    (define render-list empty)
    (define (add-shape! shape) (set! render-list (cons shape render-list)))
    (define (draw-render-list) (draw-shapes render-list))
    
    ; drawing parameters
    
    (define alpha 1)
    
    (define pen-color '(0 0 0))
    (define pen-width 1)
    (define pen-style 'solid)
    
    (define brush-color '(255 255 255))
    (define brush-style 'solid)
    
    (define background-color '(255 255 255))
    
    (define font-size 11)
    (define font-family 'roman)
    (define text-foreground '(0 0 0))
    
    ;; drawing parameter accessors
    
    ; alpha
    
    (define/public (put-alpha a) (set! alpha a))
    (define (get-alpha) alpha)
    
    ; pen
    
    (define/public (put-pen color width style)
      (set! pen-color (->pen-color color))
      (set! pen-width width)
      (set! pen-style (->pen-style style)))
    
    (define (get-pen-color) pen-color)
    (define (get-pen-width) pen-width)
    (define (get-pen-style) pen-style)
    
    ; brush
    
    (define/public (put-brush color style)
      (set! brush-color (->brush-color color))
      (set! brush-style (->brush-style style)))
    
    (define (get-brush-color) brush-color)
    (define (get-brush-style) brush-style)
    
    ; background color
    
    (define/public (put-background color)
      (set! background-color (->brush-color color)))
    (define (get-background) background-color)
    
    ; font
    
    (define/public (put-font-size size) (set! font-size size))
    (define/public (put-font-family family) (set! font-family family))
    
    (define/public (put-font size family)
      (put-font-size size)
      (put-font-family family))
    
    (define/public (put-text-foreground c)
      (set! text-foreground (->pen-color c)))
    
    (define (get-font-size) font-size)
    (define (get-font-family) font-family)
    (define (get-text-foreground) text-foreground)
    
    ; shapes
    
    (define/public (put-line v1 v2 [c (vcenter (list v1 v2))])
      (when (and (vregular? v1) (vregular? v2))
        (let-values ([(v1 v2)  (if clipping?
                                   (clip-line v1 v2 clip-x-min clip-x-max
                                              clip-y-min clip-y-max
                                              clip-z-min clip-z-max)
                                   (values v1 v2))])
          (when (and v1 v2)
            (add-shape!
             (line (get-alpha) (plot->view/no-rho c) (plot->view v1) (plot->view v2)
                   (get-pen-color) (get-pen-width) (get-pen-style)))))))
    
    (define/public (put-lines vs)
      (for ([vs  (vregular-sublists vs)])
        (when (not (empty? vs))
          (for ([v1  (in-list vs)] [v2  (in-list (rest vs))])
            (put-line v1 v2)))))
    
    (define (add-polygon lst vs c)
      (let/ec return
        (when (or (empty? vs) (not (and (andmap vregular? vs) (vregular? c))))
          (return lst))
        
        (let* ([vs  (if clipping?
                        (clip-polygon vs clip-x-min clip-x-max
                                      clip-y-min clip-y-max
                                      clip-z-min clip-z-max)
                        vs)]
               [vs  (map plot->view vs)])
          (when (empty? vs) (return lst))
          
          (cons (polygon (get-alpha) (plot->view/no-rho c) vs
                         (get-pen-color) (get-pen-width) (get-pen-style)
                         (get-brush-color) (get-brush-style))
                lst))))
    
    (define/public (put-polygon vs [c (vcenter vs)])
      (set! render-list (add-polygon render-list vs c)))
    
    (define/public (put-polygons vss [c (vcenter (flatten vss))])
      (define lst (for/fold ([lst empty]) ([vs  (in-list vss)]
                                           #:when (not (empty? vs)))
                    (add-polygon lst vs (vcenter vs))))
      (when (not (empty? lst))
        (set! render-list (cons (shapes (get-alpha) (plot->view/no-rho c) lst)
                                render-list))))
    
    (define/public (put-text str v [anchor 'center] [angle 0])
      (when (and (vregular? v) (in-bounds? v))
        (add-shape!
         (text (get-alpha) (plot->view/no-rho v) anchor angle str
               (get-font-size) (get-font-family) (get-text-foreground)))))
    
    (define/public (put-box v1 v2 [c (vcenter (list v1 v2))])
      (when (and (vregular? v1) (vregular? v2))
        (match-define (vector x1 y1 z1) v1)
        (match-define (vector x2 y2 z2) v2)
        (put-polygons
         (list 
          ;; Top
          (list (vector x1 y1 z2) (vector x2 y1 z2) (vector x2 y2 z2) (vector x1 y2 z2))
          ;; Front
          (if ((cos theta) . > . 0)
              (list (vector x1 y1 z1) (vector x2 y1 z1) (vector x2 y1 z2) (vector x1 y1 z2))
              empty)
          ;; Back
          (if ((cos theta) . < . 0)
              (list (vector x1 y2 z1) (vector x2 y2 z1) (vector x2 y2 z2) (vector x1 y2 z2))
              empty)
          ;; Left
          (if ((sin theta) . > . 0)
              (list (vector x1 y1 z1) (vector x1 y2 z1) (vector x1 y2 z2) (vector x1 y1 z2))
              empty)
          ;; Right
          (if ((sin theta) . < . 0)
              (list (vector x2 y1 z1) (vector x2 y2 z1) (vector x2 y2 z2) (vector x2 y1 z2))
              empty))
         c)))
    
    (define/public (put-glyphs vs symbol size)
      (for ([v  (in-list vs)])
        (when (and (vregular? v) (in-bounds? v))
          (add-shape!
           (glyph (get-alpha) (plot->view/no-rho v) symbol size
                  (get-pen-color) (get-pen-width) (get-pen-style)
                  (get-brush-color) (get-brush-style))))))
    
    (define/public (put-tick v radius angle)
      (when (and (vregular? v) (in-bounds? v))
        (add-shape!
         (tick-glyph (get-alpha) (plot->view/no-rho v) radius angle
                     (get-pen-color) (get-pen-width) (get-pen-style)))))
    )) ; end class
