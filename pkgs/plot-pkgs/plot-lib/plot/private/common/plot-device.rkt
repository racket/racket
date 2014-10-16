#lang racket/base

;; Instances of this class know how to draw points, polygons, rectangles, lines, text, a bunch of
;; different "glyphs" (used for point symbols and ticks), and legends on their underlying device
;; contexts. Drawing functions accept vectors representing dc coordinates.

;; It is up to callers to transform view or plot coordinates into dc coordinates.

(require racket/draw racket/class racket/match racket/math racket/bool racket/list racket/contract
         racket/vector
         "contract.rkt"
         "draw.rkt"
         "math.rkt"
         "sample.rkt"
         "parameters.rkt"
         "legend.rkt")

(provide plot-device%)

(define (coord->cons v)
  (match-define (vector x y) v)
  (cons x y))

(define (translate-glyph-sym+size sym size)
  (let ([sym  (if (integer? sym) (remainder (abs sym) 128) sym)])
    (case sym
      [(0)   (values 'square size)]
      [(1)   (values 'dot size)]
      [(2)   (values 'plus size)]
      [(3)   (values 'asterisk size)]
      [(4)   (values 'circle size)]
      [(5)   (values 'times size)]
      [(6)   (values 'square size)]
      [(7)   (values 'triangle size)]
      [(8)   (values 'oplus size)]
      [(9)   (values 'odot size)]
      [(10)  (values '4star size)]
      [(11)  (values 'diamond size)]
      [(12)  (values '5star size)]
      [(13)  (values 'square size)]
      [(14)  (values 'circle (* 9/12 size))]
      [(15)  (values '6star size)]
      [(16)  (values 'fullsquare size)]
      [(17)  (values 'fullcircle (* 4/6 size))]
      [(18)  (values 'full5star size)]
      [(19)  (values 'square size)]
      [(20 circle1)  (values 'circle (* 3/6 size))]
      [(21 cirlce2)  (values 'circle (* 4/6 size))]
      [(22 circle3)  (values 'circle (* 5/6 size))]
      [(23 circle4)  (values 'circle size)]
      [(24 circle5)  (values 'circle (* 8/6 size))]
      [(25 circle6)  (values 'circle (* 12/6 size))]
      [(26 circle7)  (values 'circle (* 14/6 size))]
      [(27 circle8)  (values 'circle (* 18/6 size))]
      [(28)  (values 'leftarrow size)]
      [(29)  (values 'rightarrow size)]
      [(30)  (values 'uparrow size)]
      [(31)  (values 'downarrow size)]
      [(fullcircle1)  (values 'fullcircle (* 3/6 size))]
      [(bullet fullcircle2)  (values 'fullcircle (* 4/6 size))]
      [(fullcircle3)  (values 'fullcircle (* 5/6 size))]
      [(fullcircle4)  (values 'fullcircle size)]
      [(fullcircle5)  (values 'fullcircle (* 8/6 size))]
      [(fullcircle6)  (values 'fullcircle (* 12/6 size))]
      [(fullcircle7)  (values 'fullcircle (* 14/6 size))]
      [(fullcircle8)  (values 'fullcircle (* 18/6 size))]
      [else  (cond [(and (integer? sym) (<= 32 sym 127))
                    (values (bytes->string/utf-8 (bytes sym)) size)]
                   [(char? sym)
                    (values (list->string (list sym)) size)]
                   [else
                    (values sym size)])])))

(define full-glyph-hash
  #hash((fullcircle . circle)
        (fullsquare . square)
        (fulldiamond . diamond)
        (fulltriangle . triangle)
        (fulltriangleup . triangleup)
        (fulltriangledown . triangledown)
        (fulltriangleleft . triangleleft)
        (fulltriangleright . triangleright)
        (full4star . 4star)
        (full5star . 5star)
        (full6star . 6star)
        (full7star . 7star)
        (full8star . 8star)))

(define plot-device%
  (class object%
    (init-field dc dc-x-min dc-y-min dc-x-size dc-y-size)
    
    ;(init-field the-dc dc-x-min dc-y-min dc-x-size dc-y-size)
    ;(define dc (make-object null-dc%))
    
    (super-new)
    
    ;; ===============================================================================================
    ;; Drawing parameters
    
    (define-values (old-scale-x old-scale-y) (send dc get-scale))
    (define-values (old-origin-x old-origin-y) (send dc get-origin))
    (define old-smoothing (send dc get-smoothing))
    (define old-text-mode (send dc get-text-mode))
    (define old-clipping-region (send dc get-clipping-region))
    (define old-font (send dc get-font))
    (define old-text-foreground (send dc get-text-foreground))
    (define old-pen (send dc get-pen))
    (define old-brush (send dc get-brush))
    (define old-background (send dc get-background))
    (define old-alpha (send dc get-alpha))
    
    (define/public (restore-drawing-params)
      (send dc set-origin old-origin-x old-origin-y)
      (send dc set-smoothing old-smoothing)
      (send dc set-text-mode old-text-mode)
      (send dc set-clipping-region old-clipping-region)
      (send dc set-font old-font)
      (send dc set-text-foreground old-text-foreground)
      (send dc set-pen old-pen)
      (send dc set-brush old-brush)
      (send dc set-background old-background)
      (send dc set-alpha old-alpha))
    
    (define/public (reset-drawing-params [clipping-rect? #t])
      (send dc set-origin
            (+ old-origin-x (* old-scale-x dc-x-min))
            (+ old-origin-y (* old-scale-y dc-y-min)))
      (send dc set-smoothing 'smoothed)
      (send dc set-text-mode 'transparent)
      (when clipping-rect?
        (send dc set-clipping-rect 0 0 dc-x-size dc-y-size))
      (set-font (plot-font-size) (plot-font-face) (plot-font-family))
      (set-text-foreground (plot-foreground))
      (set-pen (plot-foreground) (plot-line-width) 'solid)
      (set-brush (plot-background) 'solid)
      (set-background (plot-background))
      (set-background-alpha (plot-background-alpha))
      (set-alpha (plot-foreground-alpha)))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Pen, brush, alpha parameters
    
    (define pen-hash (make-hash))
    (define transparent-pen (make-pen% 0 0 0 1 'transparent))
    
    (define pen-color (->pen-color (plot-foreground)))
    (define pen-width (plot-line-width))
    (define pen-style 'solid)
    
    ;; Sets the pen, using a hash table to avoid making duplicate objects. At time of writing (and for
    ;; the forseeable future) this is much faster than using a pen-list%, because it doesn't have to
    ;; synchronize access. It's also not thread-safe.
    (define/public (set-pen color width style)
      (set! pen-style (->pen-style style))
      (cond [(eq? pen-style 'transparent)
             (set! pen-color '(0 0 0))
             (set! pen-width 1)
             (send dc set-pen transparent-pen)]
            [else
             (set! pen-color (->pen-color color))
             (set! pen-width width)
             (match-define (list (app real->color-byte r)
                                 (app real->color-byte g)
                                 (app real->color-byte b))
               pen-color)
             (send dc set-pen (hash-ref! pen-hash (vector r g b width)
                                         (λ () (make-pen% r g b width 'solid))))]))
    
    ;; Sets the pen used to draw major ticks.
    (define/public (set-major-pen [style 'solid])
      (set-pen (plot-foreground) (plot-line-width) style))
    
    ;; Sets the pen used to draw minor ticks.
    (define/public (set-minor-pen [style 'solid])
      (set-pen (plot-foreground) (* 1/2 (plot-line-width)) style))
    
    (define brush-hash (make-hash))
    (define transparent-brush (make-brush% 0 0 0 'transparent))
    
    (define brush-color (->brush-color (plot-background)))
    (define brush-style 'solid)
    
    ;; Sets the brush. Same idea as set-pen.
    (define/public (set-brush color style)
      (set! brush-style (->brush-style style))
      (cond [(eq? brush-style 'transparent)
             (set! brush-color '(0 0 0))
             (send dc set-brush transparent-brush)]
            [else
             (set! brush-color (->brush-color color))
             (match-define (list (app real->color-byte r)
                                 (app real->color-byte g)
                                 (app real->color-byte b))
               brush-color)
             (send dc set-brush (hash-ref! brush-hash (vector r g b brush-style)
                                           (λ () (make-brush% r g b brush-style))))]))
    
    ;; Sets alpha.
    (define/public (set-alpha a)
      (send dc set-alpha a))
    
    ;; Sets the background color.
    (define/public (set-background color)
      (send dc set-background (color->color% (->brush-color color))))
    
    (define background-alpha 1)
    
    ;; Sets the background opacity.
    (define/public (set-background-alpha alpha)
      (set! background-alpha alpha))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Text parameters
    
    ;; Sets the font, using the-font-list to cache fonts.
    (define/public set-font
      (case-lambda
        [(font)  (send dc set-font font)]
        [(size family) (set-font size #f family)]
        [(size face family)
         (send dc set-font
               (if face
                   (send the-font-list find-or-create-font
                         (real->font-size size)
                         face
                         family
                         'normal
                         'normal)
                   (send the-font-list find-or-create-font
                         (real->font-size size)
                         family
                         'normal
                         'normal)))]))
    
    ;; Sets only the font size, not the family.
    (define/public (set-font-size size)
      (set-font size 
                (send (send dc get-font) get-face)
                (send (send dc get-font) get-family)))
    
    ;; Returns the character height, as an exact real.
    (define/public (get-char-height)
      (inexact->exact (send dc get-char-height)))
    
    ;; Returns the character baseline, as an exact real.
    (define/public (get-char-baseline)
      (define-values (_1 _2 b _3) (get-text-extent ""))
      (inexact->exact b))
    
    ;; Returns the extent of a string, as exact reals.
    (define/public (get-text-extent str)
      (define-values (w h b d)
        (send dc get-text-extent str #f #t 0))
      (values (inexact->exact w) (inexact->exact h)
              (inexact->exact b) (inexact->exact d)))
    
    ;; Returns the width of a string, as an exact real.
    (define/public (get-text-width str)
      (define-values (w _1 _2 _3) (get-text-extent str))
      (inexact->exact w))
    
    ;; Sets the text foreground color.
    (define/public (set-text-foreground color)
      (send dc set-text-foreground (color->color% (->pen-color color))))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Clipping
    
    ;; Sets a clipping rectangle
    (define/public (set-clipping-rect r)
      (match-define (vector (ivl x1 x2) (ivl y1 y2)) r)
      (send dc set-clipping-rect x1 y1 (- x2 x1) (- y2 y1)))
    
    ;; Clears the clipping rectangle.
    (define/public (clear-clipping-rect)
      (send dc set-clipping-region #f))
    
    ;; Derived classes both do manual clipping against plot bounds (instead of dc bounds).
    
    ;; ===============================================================================================
    ;; Drawing primitives
    
    (define/public (clear)
      (define old-alpha (send dc get-alpha))
      (send dc set-alpha background-alpha)
      (send dc clear)
      (send dc set-alpha old-alpha))
    
    (define/public (draw-point v)
      (when (vrational? v)
        (match-define (vector x y) v)
        (send dc draw-point x y)))
    
    (define/public (draw-polygon vs)
      (when (andmap vrational? vs)
        (let ([vs  (map coord->cons vs)])
          (cond [(eq? pen-style 'transparent)
                 (send dc set-smoothing 'unsmoothed)
                 (send dc draw-polygon vs 0 0 'winding)
                 (send dc set-smoothing 'smoothed)]
                [else
                 (define old-pen (send dc get-pen))
                 (send dc set-pen transparent-pen)
                 (send dc set-smoothing 'unsmoothed)
                 (send dc draw-polygon vs 0 0 'winding)
                 (send dc set-smoothing 'smoothed)
                 (send dc set-pen old-pen)
                 (draw-lines/pen-style dc (cons (last vs) vs) pen-style)]))))
    
    (define/public (draw-rect r)
      (when (rect-rational? r)
        (match-define (vector (ivl x1 x2) (ivl y1 y2)) r)
        (draw-polygon (list (vector x1 y1) (vector x1 y2) (vector x2 y2) (vector x2 y1)))))
    
    (define/public (draw-lines vs)
      (when (andmap vrational? vs)
        (draw-lines/pen-style dc (map coord->cons vs) pen-style)))
    
    (define/public (draw-line v1 v2)
      (when (and (vrational? v1) (vrational? v2))
        (match-define (vector x1 y1) v1)
        (match-define (vector x2 y2) v2)
        (draw-line/pen-style dc x1 y1 x2 y2 pen-style)))
    
    (define/public (draw-text str v [anchor 'top-left] [angle 0] [dist 0] #:outline? [outline? #f])
      (when (vrational? v)
        (match-define (vector x y) v)
        
        (when outline?
          ;(define alpha (send dc get-alpha))
          (define fg (send dc get-text-foreground))
          
          ;(send dc set-alpha (alpha-expt alpha 1/2))
          (send dc set-text-foreground (send dc get-background))
          (for* ([dx  (list -1 0 1)]
                 [dy  (list -1 0 1)]
                 #:when (not (and (zero? dx) (zero? dy))))
            (draw-text/anchor dc str (+ x dx) (+ y dy) anchor angle dist))
          ;(send dc set-alpha alpha)
          (send dc set-text-foreground fg))
        
        (draw-text/anchor dc str x y anchor angle dist)))
    
    (define/public (get-text-corners str v [anchor 'top-left] [angle 0] [dist 0])
      (cond [(vrational? v)
             (match-define (vector x y) v)
             (map (λ (v) (vector-map inexact->exact v))
                  (get-text-corners/anchor dc str x y anchor angle dist))]
            [else  empty]))
    
    (define/public (draw-arrow v1 v2)
      (when (and (vrational? v1) (vrational? v2))
        (match-define (vector x1 y1) v1)
        (match-define (vector x2 y2) v2)
        (define dx (- x2 x1))
        (define dy (- y2 y1))
        (define angle (if (and (zero? dy) (zero? dx)) 0 (atan dy dx)))
        (define dist (sqrt (+ (sqr dx) (sqr dy))))
        (define head-r (* 2/5 dist))
        (define head-angle (* 1/6 pi))
        (define dx1 (* (cos (+ angle head-angle)) head-r))
        (define dy1 (* (sin (+ angle head-angle)) head-r))
        (define dx2 (* (cos (- angle head-angle)) head-r))
        (define dy2 (* (sin (- angle head-angle)) head-r))
        (send dc draw-line x1 y1 x2 y2)
        (send dc draw-line x2 y2 (- x2 dx1) (- y2 dy1))
        (send dc draw-line x2 y2 (- x2 dx2) (- y2 dy2))))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Glyph (point sym) primitives
    
    (define/public ((make-draw-circle-glyph r) v)
      (when (vrational? v)
        (match-define (vector x y) v)
        (send dc draw-ellipse (- x r -1/2) (- y r -1/2) (* 2 r) (* 2 r))))
    
    (define/public (make-draw-polygon-glyph r sides start-angle)
      (define angles (linear-seq start-angle (+ start-angle (* 2 pi)) (+ 1 sides)))
      (λ (v)
        (when (vrational? v)
          (match-define (vector x y) v)
          (send dc draw-polygon (map (λ (a) (cons (+ x (* (cos a) r)) (+ y (* (sin a) r))))
                                     angles)))))
    
    (define/public (make-draw-star-glyph r sides start-angle)
      (define angles (linear-seq start-angle (+ start-angle (* 2 pi)) (+ 1 (* 2 sides))))
      (λ (v)
        (when (vrational? v)
          (match-define (vector x y) v)
          (define pts
            (for/list ([a  (in-list angles)] [i  (in-naturals)])
              (define r-cos-a (* r (cos a)))
              (define r-sin-a (* r (sin a)))
              (cond [(odd? i)  (cons (+ x r-cos-a) (+ y r-sin-a))]
                    [else      (cons (+ x (* 1/2 r-cos-a)) (+ y (* 1/2 r-sin-a)))])))
          (send dc draw-polygon pts))))
    
    (define/public (make-draw-flare-glyph r sticks start-angle)
      (define step (/ (* 2 pi) sticks))
      (define angles (build-list sticks (λ (n) (+ start-angle (* n step)))))
      (λ (v)
        (when (vrational? v)
          (match-define (vector x y) v)
          (for ([a  (in-list angles)])
            (send dc draw-line x y (+ x (* (cos a) r)) (+ y (* (sin a) r)))))))
    
    (define/public (get-tick-endpoints v r angle)
      (match-define (vector x y) v)
      (define dx (* (inexact->exact (cos angle)) r))
      (define dy (* (inexact->exact (sin angle)) r))
      (list (vector (- x dx) (- y dy)) (vector (+ x dx) (+ y dy))))
    
    (define/public (make-draw-tick r angle)
      (define dx (* (cos angle) r))
      (define dy (* (sin angle) r))
      (λ (v)
        (when (vrational? v)
          (match-define (vector x y) v)
          (send dc draw-line (- x dx) (- y dy) (+ x dx) (+ y dy)))))
    
    (define/public (draw-tick v r angle)
      ((make-draw-tick r angle) v))
    
    (define/public (make-draw-arrow-glyph r angle)
      (define head-r (* 4/5 r))
      (define head-angle (* 1/6 pi))
      (define dx (* (cos angle) r))
      (define dy (* (sin angle) r))
      (define dx1 (* (cos (+ angle head-angle)) head-r))
      (define dy1 (* (sin (+ angle head-angle)) head-r))
      (define dx2 (* (cos (- angle head-angle)) head-r))
      (define dy2 (* (sin (- angle head-angle)) head-r))
      (λ (v)
        (when (vrational? v)
          (match-define (vector x y) v)
          (define head-x (+ x dx))
          (define head-y (+ y dy))
          (define tail-x (- x dx))
          (define tail-y (- y dy))
          (send dc draw-line head-x head-y tail-x tail-y)
          (send dc draw-line head-x head-y (- head-x dx1) (- head-y dy1))
          (send dc draw-line head-x head-y (- head-x dx2) (- head-y dy2)))))
    
    (define/public (draw-arrow-glyph v r angle)
      ((make-draw-arrow-glyph r angle) v))
    
    (define/public (make-draw-text-glyph str)
      (define-values (x-size y-size _1 _2) (get-text-extent str))
      (define dx (* 1/2 x-size))
      (define dy (* 1/2 y-size))
      (λ (v)
        (when (vrational? v)
          (match-define (vector x y) v)
          (send dc draw-text str (- x dx) (- y dy) #t))))
    
    (define ((mix-draw-glyph d1 d2) v)
      (d1 v)
      (d2 v))
    
    (define/public (draw-glyphs vs sym size)
      (let-values ([(real-sym size)  (translate-glyph-sym+size sym size)])
        (define draw-glyph
          (cond
            [(string? real-sym)  (set-font-size (* 2 size))
                                 (set-text-foreground pen-color)
                                 (make-draw-text-glyph real-sym)]
            [(symbol? real-sym)
             (define r (* 1/2 size))
             (define line-sym
               (cond [(hash-has-key? full-glyph-hash real-sym)
                      (when (eq? pen-color brush-color)
                        (set-pen pen-color 1 'transparent)
                        (set-brush brush-color 'solid))
                      (hash-ref full-glyph-hash real-sym)]
                     [else  (set-brush brush-color 'transparent)
                            real-sym]))
             (case line-sym
               ; circles
               [(circle)  (make-draw-circle-glyph r)]
               ; squares
               [(square)   (make-draw-polygon-glyph r 4 (* 1/4 pi))]
               [(diamond)  (make-draw-polygon-glyph r 4 0)]
               ; triangles
               [(triangle
                 triangleup)     (make-draw-polygon-glyph r 3 (* -1/2 pi))]
               [(triangledown)   (make-draw-polygon-glyph r 3 (* 1/2 pi))]
               [(triangleleft)   (make-draw-polygon-glyph r 3 pi)]
               [(triangleright)  (make-draw-polygon-glyph r 3 0)]
               ; dots
               [(point pixel dot)  (set-pen pen-color (* 1/2 r) 'solid)
                                   (λ (v) (draw-point v))]
               [(odot)        (set-pen pen-color 1 'solid)
                              (mix-draw-glyph (make-draw-circle-glyph (+ pen-width r))
                                              (λ (v) (draw-point v)))]
               ; flares
               [(plus)        (make-draw-flare-glyph r 4 0)]
               [(times)       (make-draw-flare-glyph r 4 (* 1/4 pi))]
               [(5asterisk)   (make-draw-flare-glyph r 5 (* -1/2 pi))]
               [(asterisk)    (make-draw-flare-glyph r 6 (* -1/2 pi))]
               [(oplus)       (mix-draw-glyph (make-draw-circle-glyph (+ pen-width r))
                                              (make-draw-flare-glyph r 4 0))]
               [(otimes)      (mix-draw-glyph (make-draw-circle-glyph (+ pen-width r))
                                              (make-draw-flare-glyph r 4 (* 1/4 pi)))]
               [(o5asterisk)  (mix-draw-glyph (make-draw-circle-glyph (+ pen-width r))
                                              (make-draw-flare-glyph r 5 (* -1/2 pi)))]
               [(oasterisk)   (mix-draw-glyph (make-draw-circle-glyph (+ pen-width r))
                                              (make-draw-flare-glyph r 6 (* -1/2 pi)))]
               ; arrows
               [(rightarrow)  (make-draw-arrow-glyph (+ 1 r) 0)]
               [(leftarrow)   (make-draw-arrow-glyph (+ 1 r) pi)]
               [(uparrow)     (make-draw-arrow-glyph (+ 1 r) (* -1/2 pi))]
               [(downarrow)   (make-draw-arrow-glyph (+ 1 r) (* 1/2 pi))]
               ; stars
               [(3star)       (make-draw-star-glyph (+ 1 r) 3 (* 1/2 pi))]
               [(4star)       (make-draw-star-glyph (+ 1 r) 4 (* 1/2 pi))]
               [(5star)       (make-draw-star-glyph (+ 1 r) 5 (* 1/2 pi))]
               [(6star)       (make-draw-star-glyph (+ 1 r) 6 (* 1/2 pi))]
               [(7star)       (make-draw-star-glyph (+ 1 r) 7 (* 1/2 pi))]
               [(8star)       (make-draw-star-glyph (+ 1 r) 8 (* 1/2 pi))]
               [else  (raise-type-error 'draw-glyphs (format "one of ~a" known-point-symbols) sym)])]
            [else  (raise-type-error 'draw-glyphs "integer, character, string or symbol" sym)]))
      
        (for ([v  (in-list vs)])
          (draw-glyph v))))
    
    ;; ===============================================================================================
    ;; Legend
    
    (define/public (draw-legend legend-entries rect)
      (define n (length legend-entries))
      (match-define (list (legend-entry labels draw-procs) ...) legend-entries)
      
      (match-define (vector (ivl x-min x-max) (ivl y-min y-max)) rect)
      
      (define-values (_1 label-y-size baseline _2) (get-text-extent (first labels)))
      (define horiz-gap (get-text-width " "))
      (define top-gap baseline)
      (define bottom-gap (* 1/2 baseline))
      (define baseline-skip (+ label-y-size baseline))
      
      (define max-label-x-size (apply max (map (λ (label) (get-text-width label)) labels)))
      (define labels-x-size (+ max-label-x-size horiz-gap))
      
      (define draw-y-size (- label-y-size baseline))
      (define draw-x-size (* 4 draw-y-size))
      
      (define legend-x-size (+ horiz-gap
                               labels-x-size (* 2 horiz-gap)
                               draw-x-size horiz-gap))
      (define legend-y-size (+ top-gap (* n baseline-skip) bottom-gap))
      
      (define legend-x-min
        (case (plot-legend-anchor)
          [(top-left left bottom-left)     x-min]
          [(top-right right bottom-right)  (- x-max legend-x-size)]
          [(center bottom top)             (- (* 1/2 (+ x-min x-max))
                                              (* 1/2 legend-x-size))]))
      
      (define legend-y-min
        (case (plot-legend-anchor)
          [(top-left top top-right)           y-min]
          [(bottom-left bottom bottom-right)  (- y-max legend-y-size)]
          [(center left right)                (- (* 1/2 (+ y-min y-max))
                                                 (* 1/2 legend-y-size))]))
      
      (define legend-rect (vector (ivl legend-x-min (+ legend-x-min legend-x-size))
                                  (ivl legend-y-min (+ legend-y-min legend-y-size))))
      
      (define label-x-min (+ legend-x-min horiz-gap))
      (define draw-x-min (+ legend-x-min (* 2 horiz-gap) labels-x-size horiz-gap))
      
      ;; legend background
      (set-pen (plot-foreground) 1 'transparent)
      (set-brush (plot-background) 'solid)
      (set-alpha (plot-legend-box-alpha))
      (draw-rect legend-rect)
      
      ;; legend border
      (set-minor-pen)
      (set-brush (plot-background) 'transparent)
      (set-alpha 3/4)
      (draw-rect legend-rect)
      
      (set-alpha (plot-foreground-alpha))
      (set-clipping-rect legend-rect)
      (for ([label  (in-list labels)] [draw-proc  (in-list draw-procs)] [i  (in-naturals)])
        (define label-y-min (+ legend-y-min top-gap (* i baseline-skip)))
        (draw-text label (vector label-x-min label-y-min) #:outline? #t)
        
        (define draw-y-min (+ label-y-min (* 1/2 baseline)))
        
        (define entry-pd (make-object plot-device% dc draw-x-min draw-y-min draw-x-size draw-y-size))
        (send entry-pd reset-drawing-params #f)
        (draw-proc this draw-x-size draw-y-size)
        (send entry-pd restore-drawing-params))
      
      (clear-clipping-rect))
    ))  ; end class
