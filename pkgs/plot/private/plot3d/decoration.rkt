#lang racket/base

;; Renderers for plot decorations: labeled points only so far

(require racket/contract racket/class racket/match racket/list
         unstable/latent-contract/defthing
         unstable/contract
         plot/utils
         "../common/utils.rkt"
         )

(provide (all-defined-out))

;; ===================================================================================================
;; Labeled points

(define (format-coordinate3d v area)
  (match-define (vector x y z) v)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max))
    (send area get-bounds-rect))
  (match-define (list x-str) (format-tick-labels (plot-x-ticks) x-min x-max (list x)))
  (match-define (list y-str) (format-tick-labels (plot-y-ticks) y-min y-max (list y)))
  (match-define (list z-str) (format-tick-labels (plot-z-ticks) z-min z-max (list z)))
  (format "(~a,~a,~a)" x-str y-str z-str))

(define ((label3d-render-proc label v color size family anchor angle
                              point-color point-fill-color point-size point-line-width point-sym
                              alpha)
         area)
  (let ([label  (if label label (format-coordinate3d v area))])
    (send area put-alpha alpha)
    ; label
    (send area put-text-foreground color)
    (send area put-font size family)
    (send area put-text (string-append " " label " ") v anchor angle (* 1/2 point-size)
          #:outline? #t #:layer plot3d-front-layer)
    ; point
    (send area put-pen point-color point-line-width 'solid)
    (send area put-brush point-fill-color 'solid)
    (send area put-glyphs (list v) point-sym point-size #:layer plot3d-front-layer))
  
  empty)

(defproc (point-label3d
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
          ) renderer3d?
  (let ([v  (sequence-head-vector 'point-label3d v 3)])
    (match-define (vector x y z) v)
    (renderer3d (vector (ivl x x) (ivl y y) (ivl z z)) #f #f
                (label3d-render-proc
                 label v color size family anchor angle
                 point-color (cond [(eq? point-fill-color 'auto)  (->pen-color point-color)]
                                   [else  point-fill-color])
                 point-size point-line-width point-sym
                 alpha))))
