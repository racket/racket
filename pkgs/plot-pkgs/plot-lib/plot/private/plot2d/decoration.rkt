#lang racket/base

;; Renderers for plot decorations: axes, grids, labeled points, etc.

(require racket/contract racket/class racket/match racket/math racket/list
         unstable/latent-contract/defthing
         unstable/contract
         plot/utils
         "../common/utils.rkt"
         "clip.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; X and Y axes

(define ((x-axis-render-proc y ticks? labels? far? alpha) area)
  (match-define (vector (ivl x-min x-max) y-ivl) (send area get-bounds-rect))
  (define x-ticks (if far? (send area get-x-far-ticks) (send area get-x-ticks)))
  (define radius (if ticks? (* 1/2 (plot-tick-size)) 0))
  
  (send area put-alpha alpha)
  (send area put-minor-pen)
  (send area put-line (vector x-min y) (vector x-max y))
  
  (when ticks?
    (for ([t  (in-list x-ticks)])
      (match-define (tick x major? _) t)
      (if major? (send area put-major-pen) (send area put-minor-pen))
      (send area put-tick (vector x y) (if major? radius (* 1/2 radius)) (* 1/2 pi))))
  
  (when labels?
    (define dist (+ radius (pen-gap)))
    (for ([t  (in-list x-ticks)] #:when (pre-tick-major? t))
      (match-define (tick x _ label) t)
      (send area put-text label (vector x y) (if far? 'bottom 'top) 0 dist)))
  
  empty)

(defproc (x-axis [y real? 0]
                 [#:ticks? ticks? boolean? (x-axis-ticks?)]
                 [#:labels? labels? boolean? (x-axis-labels?)]
                 [#:far? far? boolean? (x-axis-far?)]
                 [#:alpha alpha (real-in 0 1) (x-axis-alpha)]) renderer2d?
  (renderer2d #f #f #f (x-axis-render-proc y ticks? labels? far? alpha)))

(define ((y-axis-render-proc x ticks? labels? far? alpha) area)
  (match-define (vector x-ivl (ivl y-min y-max)) (send area get-bounds-rect))
  (define y-ticks (if far? (send area get-y-far-ticks) (send area get-y-ticks)))
  (define radius (if ticks? (* 1/2 (plot-tick-size)) 0))
  
  (send area put-alpha alpha)
  (send area put-minor-pen)
  (send area put-line (vector x y-min) (vector x y-max))
  
  (when ticks?
    (for ([t  (in-list y-ticks)])
      (match-define (tick y major? _) t)
      (if major? (send area put-major-pen) (send area put-minor-pen))
      (send area put-tick (vector x y) (if major? radius (* 1/2 radius)) 0)))
  
  (when labels?
    (define dist (+ radius (pen-gap)))
    (for ([t  (in-list y-ticks)] #:when (pre-tick-major? t))
      (match-define (tick y _ label) t)
      (send area put-text label (vector x y) (if far? 'left 'right) 0 dist)))
  
  empty)

(defproc (y-axis [x real? 0]
                 [#:ticks? ticks? boolean? (y-axis-ticks?)]
                 [#:labels? labels? boolean? (y-axis-labels?)]
                 [#:far? far? boolean? (y-axis-far?)]
                 [#:alpha alpha (real-in 0 1) (y-axis-alpha)]) renderer2d?
  (renderer2d #f #f #f (y-axis-render-proc x ticks? labels? far? alpha)))

(defproc (axes [x real? 0] [y real? 0]
               [#:x-ticks? x-ticks? boolean? (x-axis-ticks?)]
               [#:y-ticks? y-ticks? boolean? (y-axis-ticks?)]
               [#:x-labels? x-labels? boolean? (x-axis-labels?)]
               [#:y-labels? y-labels? boolean? (y-axis-labels?)]
               [#:x-alpha x-alpha (real-in 0 1) (x-axis-alpha)]
               [#:y-alpha y-alpha (real-in 0 1) (y-axis-alpha)]) (listof renderer2d?)
  (list (x-axis y #:ticks? x-ticks? #:labels? x-labels? #:alpha x-alpha)
        (y-axis x #:ticks? y-ticks? #:labels? y-labels? #:alpha y-alpha)))

;; ===================================================================================================
;; Polar axes

(define (build-polar-axes num x-min x-max y-min y-max [start-θ 0])
  (define step (/ (* 2 pi) num))
  (define θs (build-list num (λ (n) (+ start-θ (* n step)))))
  (define max-r (max (vmag (vector x-min y-min)) (vmag (vector x-min y-max))
                     (vmag (vector x-max y-max)) (vmag (vector x-max y-min))))
  (define-values (r-mins r-maxs)
    (for/lists (r-mins r-maxs) ([θ  (in-list θs)])
      (define-values (v1 v2)
        (clip-line/bounds (vector 0 0) (vector (* max-r (cos θ)) (* max-r (sin θ)))
                          x-min x-max y-min y-max))
      (values (if v1 (vmag v1) #f)
              (if v2 (vmag v2) #f))))
  (for/lists (θs r-mins r-maxs) ([θ  (in-list θs)] [r-min  (in-list r-mins)] [r-max  (in-list r-maxs)]
                                                   #:when (and r-min r-max (not (= r-min r-max))))
    (values θ r-min r-max)))

(define (draw-polar-axis-ticks num labels? area)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max)) (send area get-bounds-rect))
  (define-values (θs r-mins r-maxs) (build-polar-axes num x-min x-max y-min y-max
                                                      (* 1/2 (/ (* 2 pi) num))))
  (define corner-rs
    (list (vmag (vector x-min y-min)) (vmag (vector x-min y-max))
          (vmag (vector x-max y-max)) (vmag (vector x-max y-min))))
  (define r-min (if (and (<= x-min 0 x-max) (<= y-min 0 y-max)) 0 (apply min corner-rs)))
  (define r-max (apply max corner-rs))
  (define ts (filter (λ (t) (not (zero? (pre-tick-value t))))
                     ((plot-r-ticks) r-min r-max)))
  ;; Draw the tick lines
  (for ([t  (in-list ts)])
    (match-define (tick r major? label) t)
    (if major? (send area put-minor-pen) (send area put-minor-pen 'long-dash))
    (define pts (for/list ([θ  (in-list (linear-seq 0 (* 2 pi) 500))])
                  (vector (* r (cos θ)) (* r (sin θ)))))
    (send area put-lines pts))
  ;; Draw the labels
  (when (and labels? (not (empty? θs)))
    ;; Find the longest half-axis, rounded to drown out floating-point error
    (define mag (expt 10 (- (digits-for-range r-min r-max))))
    (match-define (list mθ mr-min mr-max)
      (argmax (λ (lst) (* (round (/ (- (third lst) (second lst)) mag)) mag))
              (map list θs r-mins r-maxs)))
    ;; Actually draw the labels
    (for ([t  (in-list ts)])
      (match-define (tick r major? label) t)
      (when (and major? (<= mr-min r mr-max))
        (send area put-text label (vector (* r (cos mθ)) (* r (sin mθ)))
              'center #:outline? #t)))))

(define (draw-polar-axis-lines num area)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max)) (send area get-bounds-rect))
  (define-values (θs r-mins r-maxs) (build-polar-axes num x-min x-max y-min y-max))
  
  (send area put-minor-pen)
  (for ([θ  (in-list θs)] [r-min  (in-list r-mins)] [r-max  (in-list r-maxs)])
    (send area put-line
          (vector (* r-min (cos θ)) (* r-min (sin θ)))
          (vector (* r-max (cos θ)) (* r-max (sin θ))))))

(define ((polar-axes-render-proc num ticks? labels? alpha) area)
  (send area put-alpha alpha)
  (when (num . > . 0) (draw-polar-axis-lines num area))
  (when ticks? (draw-polar-axis-ticks (if (num . > . 0) num 12) labels? area))
  empty)

(defproc (polar-axes [#:number num exact-nonnegative-integer? (polar-axes-number)]
                     [#:ticks? ticks? boolean? (polar-axes-ticks?)]
                     [#:labels? labels? boolean? (polar-axes-labels?)]
                     [#:alpha alpha (real-in 0 1) (polar-axes-alpha)]) renderer2d?
  (renderer2d #f #f #f (polar-axes-render-proc num ticks? labels? alpha)))

;; ===================================================================================================
;; Grid

(define ((x-tick-lines-render-proc) area)
  (match-define (vector x-ivl (ivl y-min y-max)) (send area get-bounds-rect))
  (define x-ticks (send area get-x-ticks))
  
  (send area put-alpha 1/2)
  (for ([t  (in-list x-ticks)])
    (match-define (tick x major? _) t)
    (if major? (send area put-minor-pen) (send area put-minor-pen 'long-dash))
    (send area put-line (vector x y-min) (vector x y-max)))
  
  empty)

(define ((y-tick-lines-render-proc) area)
  (match-define (vector (ivl x-min x-max) y-ivl) (send area get-bounds-rect))
  (define y-ticks (send area get-y-ticks))
  
  (send area put-alpha 1/2)
  (for ([t  (in-list y-ticks)])
    (match-define (tick y major? _) t)
    (if major? (send area put-minor-pen) (send area put-minor-pen 'long-dash))
    (send area put-line (vector x-min y) (vector x-max y)))
  
  empty)

(defproc (x-tick-lines) renderer2d?
  (renderer2d #f #f #f (x-tick-lines-render-proc)))

(defproc (y-tick-lines) renderer2d?
  (renderer2d #f #f #f (y-tick-lines-render-proc)))

(defproc (tick-grid) (listof renderer2d?)
  (list (x-tick-lines) (y-tick-lines)))

;; ===================================================================================================
;; Labeled points

(define (format-coordinate v area)
  (match-define (vector x y) v)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max)) (send area get-bounds-rect))
  (match-define (list x-str) (format-tick-labels (plot-x-ticks) x-min x-max (list x)))
  (match-define (list y-str) (format-tick-labels (plot-y-ticks) y-min y-max (list y)))
  (format "(~a,~a)" x-str y-str))

(define ((label-render-proc label v color size family anchor angle
                            point-color point-fill-color point-size point-line-width point-sym
                            alpha)
         area)
  (let ([label  (if label label (format-coordinate v area))])
    (send area put-alpha alpha)
    ; label
    (send area put-text-foreground color)
    (send area put-font size family)
    (send area put-text (string-append " " label " ") v anchor angle (* 1/2 point-size) #:outline? #t)
    ; point
    (send area put-pen point-color point-line-width 'solid)
    (send area put-brush point-fill-color 'solid)
    (send area put-glyphs (list v) point-sym point-size))
  
  empty)

(defproc (point-label
          [v (sequence/c real?)] [label (or/c string? #f) #f]
          [#:color color plot-color/c (plot-foreground)]
          [#:size size (>=/c 0) (plot-font-size)]
          [#:family family font-family/c (plot-font-family)]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:angle angle real? (label-angle)]
          [#:point-color point-color plot-color/c (point-color)]
          [#:point-fill-color point-fill-color (or/c plot-color/c 'auto) 'auto]
          [#:point-size point-size (>=/c 0) (label-point-size)]
          [#:point-line-width point-line-width (>=/c 0) (point-line-width)]
          [#:point-sym point-sym point-sym/c 'fullcircle]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?
  (let ([v  (sequence-head-vector 'point-label v 2)])
    (match-define (vector x y) v)
    (renderer2d (vector (ivl x x) (ivl y y)) #f #f
                (label-render-proc
                 label v color size family anchor angle
                 point-color (cond [(eq? point-fill-color 'auto)  (->pen-color point-color)]
                                   [else  point-fill-color])
                 point-size point-line-width point-sym
                 alpha))))

(defproc (parametric-label
          [f (real? . -> . (sequence/c real?))] [t real?] [label (or/c string? #f) #f]
          [#:color color plot-color/c (plot-foreground)]
          [#:size size (>=/c 0) (plot-font-size)]
          [#:family family font-family/c (plot-font-family)]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:angle angle real? (label-angle)]
          [#:point-color point-color plot-color/c (point-color)]
          [#:point-fill-color point-fill-color (or/c plot-color/c 'auto) 'auto]
          [#:point-size point-size (>=/c 0) (label-point-size)]
          [#:point-line-width point-line-width (>=/c 0) (point-line-width)]
          [#:point-sym point-sym point-sym/c 'fullcircle]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?
  (point-label (sequence-head-vector 'parametric-label (f t) 2)
               label #:color color #:size size #:family family #:anchor anchor #:angle angle
               #:point-color point-color #:point-fill-color point-fill-color #:point-size point-size
               #:point-line-width point-line-width #:point-sym point-sym
               #:alpha alpha))

(defproc (polar-label
          [f (real? . -> . real?)] [θ real?] [label (or/c string? #f) #f]
          [#:color color plot-color/c (plot-foreground)]
          [#:size size (>=/c 0) (plot-font-size)]
          [#:family family font-family/c (plot-font-family)]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:angle angle real? (label-angle)]
          [#:point-color point-color plot-color/c (point-color)]
          [#:point-fill-color point-fill-color (or/c plot-color/c 'auto) 'auto]
          [#:point-size point-size (>=/c 0) (label-point-size)]
          [#:point-line-width point-line-width (>=/c 0) (point-line-width)]
          [#:point-sym point-sym point-sym/c 'fullcircle]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?
  (point-label (polar->cartesian θ (f θ)) label
               #:color color #:size size #:family family #:anchor anchor #:angle angle
               #:point-color point-color #:point-fill-color point-fill-color #:point-size point-size
               #:point-line-width point-line-width #:point-sym point-sym
               #:alpha alpha))

(defproc (function-label
          [f  (real? . -> . real?)] [x real?] [label (or/c string? #f) #f]
          [#:color color plot-color/c (plot-foreground)]
          [#:size size (>=/c 0) (plot-font-size)]
          [#:family family font-family/c (plot-font-family)]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:angle angle real? (label-angle)]
          [#:point-color point-color plot-color/c (point-color)]
          [#:point-fill-color point-fill-color (or/c plot-color/c 'auto) 'auto]
          [#:point-size point-size (>=/c 0) (label-point-size)]
          [#:point-line-width point-line-width (>=/c 0) (point-line-width)]
          [#:point-sym point-sym point-sym/c 'fullcircle]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?
  (point-label (vector x (f x)) label
               #:color color #:size size #:family family #:anchor anchor #:angle angle
               #:point-color point-color #:point-fill-color point-fill-color #:point-size point-size
               #:point-line-width point-line-width #:point-sym point-sym
               #:alpha alpha))

(defproc (inverse-label
          [f  (real? . -> . real?)] [y real?] [label (or/c string? #f) #f]
          [#:color color plot-color/c (plot-foreground)]
          [#:size size (>=/c 0) (plot-font-size)]
          [#:family family font-family/c (plot-font-family)]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:angle angle real? (label-angle)]
          [#:point-color point-color plot-color/c (point-color)]
          [#:point-fill-color point-fill-color (or/c plot-color/c 'auto) 'auto]
          [#:point-size point-size (>=/c 0) (label-point-size)]
          [#:point-line-width point-line-width (>=/c 0) (point-line-width)]
          [#:point-sym point-sym point-sym/c 'fullcircle]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?
  (point-label (vector (f y) y) label
               #:color color #:size size #:family family #:anchor anchor #:angle angle
               #:point-color point-color #:point-fill-color point-fill-color #:point-size point-size
               #:point-line-width point-line-width #:point-sym point-sym
               #:alpha alpha))
