#lang racket/base

;; Renderers for plot decorations: axes, grids, labeled points, etc.

(require racket/contract racket/class racket/match racket/math racket/list
         "../common/ticks.rkt"
         "../common/math.rkt"
         "../common/format.rkt"
         "../common/contract.rkt"
         "../common/contract-doc.rkt"
         "../common/legend.rkt"
         "../common/vector.rkt"
         "../common/area.rkt"
         "../common/sample.rkt"
         "../common/parameters.rkt"
         "../common/axis-transform.rkt"
         "renderer.rkt"
         "area.rkt"
         "line.rkt"
         "interval.rkt"
         "point.rkt"
         "contour.rkt"
         "clip.rkt")

(provide x-axis y-axis axes
         polar-axes
         x-tick-lines y-tick-lines tick-grid
         point-label
         parametric-label
         polar-label
         function-label
         inverse-label)

;; ===================================================================================================
;; X and Y axes

(define ((x-axis-render-proc y ticks?) area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (define x-ticks (send area get-x-ticks))
  (define half (* 1/2 (plot-tick-size)))
  
  (send area set-alpha 1/2)
  (send area set-major-pen)
  (send area put-line (vector x-min y) (vector x-max y))
  
  (when ticks?
    (for ([t  (in-list x-ticks)])
      (match-define (tick x major? _) t)
      (if major? (send area set-major-pen) (send area set-minor-pen))
      (send area put-tick (vector x y) half 1/2pi)))
  
  empty)

(define ((x-axis-ticks-fun y) x-min x-max y-min y-max)
  (define digits (digits-for-range y-min y-max))
  (values empty (list (tick y #t (real->plot-label y digits)))))

(defproc (x-axis [y real? 0] [add-y-tick? boolean? #f]
                 [#:ticks? ticks? boolean? (x-axis-ticks?)]) renderer2d?
  (renderer2d (x-axis-render-proc y ticks?)
              (if add-y-tick? (x-axis-ticks-fun y) null-2d-ticks-fun)
              null-2d-bounds-fun #f #f #f #f))

(define ((y-axis-render-proc x ticks?) area)
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  (define y-ticks (send area get-y-ticks))
  (define half (* 1/2 (plot-tick-size)))
  
  (send area set-alpha 1/2)
  (send area set-major-pen)
  (send area put-line (vector x y-min) (vector x y-max))
  
  (when ticks?
    (for ([t  (in-list y-ticks)])
      (match-define (tick y major? _) t)
      (if major? (send area set-major-pen) (send area set-minor-pen))
      (send area put-tick (vector x y) half 0)))
  
  empty)

(define ((y-axis-ticks-fun x) x-min x-max y-min y-max)
  (define digits (digits-for-range x-min x-max))
  (values (list (tick x #t (real->plot-label x digits))) empty))

(defproc (y-axis [x real? 0] [add-x-tick? boolean? #f]
                 [#:ticks? ticks? boolean? (y-axis-ticks?)]) renderer2d?
  (renderer2d (y-axis-render-proc x ticks?)
              (if add-x-tick? (y-axis-ticks-fun x) null-2d-ticks-fun)
              null-2d-bounds-fun #f #f #f #f))

(defproc (axes [x real? 0] [y real? 0] [add-x-tick? boolean? #f] [add-y-tick? boolean? #f]
               [#:x-ticks? x-ticks? boolean? (x-axis-ticks?)]
               [#:y-ticks? y-ticks? boolean? (y-axis-ticks?)]
               ) (listof renderer2d?)
  (list (x-axis y add-y-tick? #:ticks? x-ticks?)
        (y-axis x add-x-tick? #:ticks? y-ticks?)))

;; ===================================================================================================
;; Polar axes

(define (build-polar-axes num x-min x-max y-min y-max)
  (define step (/ (* 2 pi) num))
  (define θs (build-list num (λ (n) (* n step))))
  (define max-r (max (vmag (vector x-min y-min)) (vmag (vector x-min y-max))
                     (vmag (vector x-max y-max)) (vmag (vector x-max y-min))))
  (define-values (r-mins r-maxs)
    (for/lists (r-mins r-maxs) ([θ  (in-list θs)])
      (define-values (v1 v2)
        (clip-line (vector 0 0) (vector (* max-r (cos θ)) (* max-r (sin θ)))
                   x-min x-max y-min y-max))
      (values (if v1 (vmag v1) #f)
              (if v2 (vmag v2) #f))))
  (for/lists (θs r-mins r-maxs) ([θ  (in-list θs)] [r-min  (in-list r-mins)] [r-max  (in-list r-maxs)]
                                                   #:when (and r-min r-max (not (= r-min r-max))))
    (values θ r-min r-max)))

(define ((polar-axes-render-proc num ticks?) area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  
  (define-values (θs r-mins r-maxs) (build-polar-axes num x-min x-max y-min y-max))
  
  ;; Draw the axes
  (send area set-alpha 1/2)
  (send area set-major-pen)
  (for ([θ  (in-list θs)] [r-min  (in-list r-mins)] [r-max  (in-list r-maxs)])
    (send area put-line
          (vector (* r-min (cos θ)) (* r-min (sin θ)))
          (vector (* r-max (cos θ)) (* r-max (sin θ)))))
  
  (when ticks?
    (define corner-rs
      (list (vmag (vector x-min y-min)) (vmag (vector x-min y-max))
            (vmag (vector x-max y-max)) (vmag (vector x-max y-min))))
    (define r-min (if (and (<= x-min 0 x-max) (<= y-min 0 y-max)) 0 (apply min corner-rs)))
    (define r-max (apply max corner-rs))
    (define ts ((linear-ticks) r-min r-max (polar-axes-max-ticks) id-transform))
    
    (send area set-alpha 1/2)
    (for ([t  (in-list ts)])
      (match-define (tick r major? label) t)
      (if major? (send area set-major-pen) (send area set-minor-pen 'long-dash))
      (define pts (for/list ([θ  (in-list (linear-seq 0 (* 2 pi) 100))])
                    (vector (* r (cos θ)) (* r (sin θ)))))
      (send area put-lines pts))
    
    (when (not (empty? θs))
      ;; find the longest axis
      (define mag (expt 10 (- (digits-for-range r-min r-max))))
      (match-define (list mθ mr-min mr-max)
        ;; find the longest, rounded to drown out floating-point error
        (argmax (λ (lst) (* (round (/ (- (third lst) (second lst)) mag)) mag))
                (map list θs r-mins r-maxs)))
      
      (send area set-alpha 1)
      (for ([t  (in-list ts)])
        (match-define (tick r major? label) t)
        (when (and major? (<= mr-min r mr-max))
          (send area put-text label (vector (* r (cos mθ)) (* r (sin mθ)))
                'center 0 #:outline? #t)))))
  
  empty)

(defproc (polar-axes [#:number num exact-positive-integer? (polar-axes-number)]
                     [#:ticks? ticks? boolean? (polar-axes-ticks?)]
                     ) renderer2d?
  (renderer2d (polar-axes-render-proc num ticks?)
              null-2d-ticks-fun null-2d-bounds-fun #f #f #f #f))

;; ===================================================================================================
;; Grid

(define ((x-tick-lines-render-proc) area)
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  (define x-ticks (send area get-x-ticks))
  
  (send area set-alpha 1/2)
  (for ([t  (in-list x-ticks)])
    (match-define (tick x major? _) t)
    (if major? (send area set-major-pen) (send area set-minor-pen 'long-dash))
    (send area put-line (vector x y-min) (vector x y-max)))
  
  empty)

(define ((y-tick-lines-render-proc) area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (define y-ticks (send area get-y-ticks))
  
  (send area set-alpha 1/2)
  (for ([t  (in-list y-ticks)])
    (match-define (tick y major? _) t)
    (if major? (send area set-major-pen) (send area set-minor-pen 'long-dash))
    (send area put-line (vector x-min y) (vector x-max y)))
  
  empty)

(defproc (x-tick-lines) renderer2d?
  (renderer2d (x-tick-lines-render-proc) null-2d-ticks-fun null-2d-bounds-fun #f #f #f #f))

(defproc (y-tick-lines) renderer2d?
  (renderer2d (y-tick-lines-render-proc) null-2d-ticks-fun null-2d-bounds-fun #f #f #f #f))

(defproc (tick-grid) (listof renderer2d?)
  (list (x-tick-lines) (y-tick-lines)))

;; ===================================================================================================
;; Labeled points

(define (format-x-coordinate x area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (format "~a" (real->plot-label x (digits-for-range x-min x-max))))

(define (format-y-coordinate y area)
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  (format "~a" (real->plot-label y (digits-for-range y-min y-max))))

(define (format-coordinate v area)
  (match-define (vector x y) v)
  (format "(~a,~a)" (format-x-coordinate x area) (format-y-coordinate y area)))

(define ((label-render-proc label v color size anchor angle point-size alpha) area)
  (let ([label  (if label label (format-coordinate v area))])
    (send area set-alpha alpha)
    ; label
    (send area set-text-foreground color)
    (send area set-font-size size)
    (send area put-text (string-append " " label " ") v anchor angle #:outline? #t)
    ; point
    (send area set-pen color 1 'solid)
    (send area put-glyphs (list v) 'fullcircle point-size))
  
  empty)

(defproc (point-label
          [v (vector/c real? real?)] [label (or/c string? #f) #f]
          [#:color color plot-color/c (plot-foreground)]
          [#:size size (>=/c 0) (plot-font-size)]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:angle angle real? (label-angle)]
          [#:point-size point-size (>=/c 0) (label-point-size)]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?
  (match-define (vector x y) v)
  (renderer2d (label-render-proc label v color size anchor angle point-size alpha)
              null-2d-ticks-fun
              null-2d-bounds-fun
              x x y y))

(defproc (parametric-label
          [f (real? . -> . (vector/c real? real?))]
          [t real?] [label (or/c string? #f) #f]
          [#:color color plot-color/c (plot-foreground)]
          [#:size size (>=/c 0) (plot-font-size)]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:angle angle real? (label-angle)]
          [#:point-size point-size (>=/c 0) (label-point-size)]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?
  (point-label (match f
                 [(vector fx fy)  (vector (fx t) (fy t))]
                 [(? procedure?)  (f t)])
               label #:color color #:size size #:anchor anchor #:angle angle
               #:point-size point-size #:alpha alpha))

(defproc (polar-label
          [f (real? . -> . real?)] [θ real?] [label (or/c string? #f) #f]
          [#:color color plot-color/c (plot-foreground)]
          [#:size size (>=/c 0) (plot-font-size)]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:angle angle real? (label-angle)]
          [#:point-size point-size (>=/c 0) (label-point-size)]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?
  (point-label (polar->cartesian θ (f θ)) label
               #:color color #:size size #:anchor anchor #:angle angle
               #:point-size point-size #:alpha alpha))

(defproc (function-label
          [f  (real? . -> . real?)] [x real?] [label (or/c string? #f) #f]
          [#:color color plot-color/c (plot-foreground)]
          [#:size size (>=/c 0) (plot-font-size)]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:angle angle real? (label-angle)]
          [#:point-size point-size (>=/c 0) (label-point-size)]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?
  (point-label (vector x (f x)) label
               #:color color #:size size #:anchor anchor #:angle angle
               #:point-size point-size #:alpha alpha))

(defproc (inverse-label
          [f  (real? . -> . real?)] [y real?] [label (or/c string? #f) #f]
          [#:color color plot-color/c (plot-foreground)]
          [#:size size (>=/c 0) (plot-font-size)]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:angle angle real? (label-angle)]
          [#:point-size point-size (>=/c 0) (label-point-size)]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?
  (point-label (vector (f y) y) label
               #:color color #:size size #:anchor anchor #:angle angle
               #:point-size point-size #:alpha alpha))
