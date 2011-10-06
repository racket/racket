#lang racket/base

;; Defines the base class for 2d-plot-area% and 3d-plot-area%.

;; Instances of this class know how to draw points, polygons, rectangles, lines, text, and a bunch of
;; different "glyphs" (used for point symbols and ticks), and legends on their underlying device
;; contexts.

;; Drawing functions accept vectors representing dc coordinates. It is up to descendants to provide
;; drawing functions that accept view coordinates and transform them into dc coordinates. By
;; convention, such functions start with "put-" instead of "draw-".

(require racket/draw racket/class racket/match racket/math racket/bool racket/list racket/contract
         "contract.rkt"
         "draw.rkt"
         "math.rkt"
         "parameters.rkt")

(provide plot-area% (struct-out legend-entry))

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

(define plot-area%
  (class object%
    (init-field dc dc-x-min dc-y-min dc-x-size dc-y-size)
    
    (super-new)
    
    ;; ===============================================================================================
    ;; Drawing parameters
    
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
      (send dc set-smoothing old-smoothing)
      (send dc set-text-mode old-text-mode)
      (send dc set-clipping-region old-clipping-region)
      (send dc set-font old-font)
      (send dc set-text-foreground old-text-foreground)
      (send dc set-pen old-pen)
      (send dc set-brush old-brush)
      (send dc set-background old-background)
      (send dc set-alpha old-alpha))
    
    (define/public (reset-drawing-params)
      (send dc set-smoothing 'smoothed)
      (send dc set-text-mode 'transparent)
      (send dc set-clipping-rect dc-x-min dc-y-min dc-x-size dc-y-size)
      (set-font (plot-font-size) (plot-font-family))
      (set-text-foreground (plot-foreground))
      (set-pen (plot-foreground) (plot-line-width) 'solid)
      (set-brush (plot-background) 'solid)
      (set-background (plot-background))
      (set-alpha 1))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Pen, brush, alpha parameters
    
    ;; Sets the pen, if the pen is different. At time of writing, this actually makes setting the pen
    ;; more efficient, because setting the pen is kind of slow.
    (define/public (set-pen color width style)
      (match-define (list r g b) (map real->color-byte (->pen-color color)))
      (let ([style  (->pen-style style)])
        (define pen (send dc get-pen))
        (define c (send pen get-color))
        ; only change the pen if it's different (pen changes are kind of slow)
        (unless (and (= r (send c red)) (= g (send c green)) (= b (send c blue))
                     (= width (send pen get-width))
                     (eq? style (send pen get-style)))
          (send dc set-pen (make-object color% r g b) width style))))
    
    ;; Sets the pen used to draw major ticks.
    (define/public (set-major-pen [style 'solid])
      (set-pen (plot-foreground) (plot-line-width) style))
    
    ;; Sets the pen used to draw minor ticks.
    (define/public (set-minor-pen [style 'solid])
      (set-pen (plot-foreground) (* 1/2 (plot-line-width)) style))
    
    ;; Sets the brush, if the brush is different. Same idea as set-pen; actually makes it faster.
    (define/public (set-brush color style)
      (match-define (list r g b) (map real->color-byte (->brush-color color)))
      (let ([style  (->brush-style style)])
        (define brush (send dc get-brush))
        (define c (send brush get-color))
        (define s (send brush get-style))
        (unless (and (eq? style s)
                     (or (eq? style 'transparent)
                         (and (= r (send c red)) (= g (send c green)) (= b (send c blue)))))
          (send dc set-brush (make-object color% r g b) style))))
    
    ;; Sets alpha.
    (define/public (set-alpha a) (send dc set-alpha a))
    
    ;; Sets the background color.
    (define/public (set-background color)
      (send dc set-background (color->color% (->brush-color color))))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Text parameters
    
    (define font-list (make-object font-list%))
    
    ;; Sets the font, using font-list to cache fonts.
    (define/public set-font
      (case-lambda
        [(font)  (send dc set-font font)]
        [(size family)
         (define font (send font-list find-or-create-font
                            (real->font-size size) family 'normal 'normal #f 'default #t))
         (send dc set-font font)]))
    
    ;; Sets only the font size, not the family.
    (define/public (set-font-size size)
      (set-font size (send (send dc get-font) get-family)))
    
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
    
    ;; Sets a clipping rectangle; deals with swapped mins and maxes.
    (define/public (set-clipping-rect v1 v2)
      (match-define (vector x1 y1) v1)
      (match-define (vector x2 y2) v2)
      (let ([x1  (min x1 x2)]
            [x2  (max x1 x2)]
            [y1  (min y1 y2)]
            [y2  (max y1 y2)])
        (send dc set-clipping-rect x1 y1 (- x2 x1) (- y2 y1))))
    
    ;; Clears the clipping rectangle.
    (define/public (clear-clipping-rect)
      (send dc set-clipping-region #f))
    
    ;; Derived classes both do manual clipping against plot bounds (instead of dc bounds).
    
    ;; ===============================================================================================
    ;; Drawing primitives
    
    (define/public (clear)
      (send dc clear))
    
    (define/public (draw-point v)
      (match-define (vector x y) v)
      (send dc draw-point x y))
    
    (define/public (draw-polygon vs [fill-style 'winding])
      (send dc draw-polygon (map coord->cons vs) 0 0 fill-style))
    
    (define/public (draw-rectangle v1 v2)
      (match-define (vector x1 y1) v1)
      (match-define (vector x2 y2) v2)
      (draw-polygon
       (list (vector x1 y1) (vector x1 y2) (vector x2 y2) (vector x2 y1))))
    
    (define/public (draw-lines vs)
      (send dc draw-lines (map coord->cons vs)))
    
    (define/public (draw-line v1 v2)
      (match-define (vector x1 y1) v1)
      (match-define (vector x2 y2) v2)
      (send dc draw-line x1 y1 x2 y2))
    
    (define/public (draw-text str v [anchor 'top-left] [angle 0] #:outline? [outline? #f])
      (match-define (vector x y) v)
      
      (when outline?
        (define alpha (send dc get-alpha))
        (define fg (send dc get-text-foreground))
        
        (send dc set-alpha (alpha-expt alpha 1/8))
        (send dc set-text-foreground (send dc get-background))
        (for* ([dx  (list -1 0 1)]
               [dy  (list -1 0 1)]
               #:when (not (and (zero? dx) (zero? dy))))
          (draw-text/anchor dc str (+ x dx) (+ y dy) anchor #t 0 angle))
        (send dc set-alpha alpha)
        (send dc set-text-foreground fg))
      
      (draw-text/anchor dc str x y anchor #t 0 angle))
    
    (define/public (get-text-corners str v [anchor 'top-left] [angle 0])
      (match-define (vector x y) v)
      (get-text-corners/anchor dc str x y anchor #t 0 angle))
    
    (define/public (draw-arrow v1 v2)
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
      (send dc draw-line x2 y2 (- x2 dx2) (- y2 dy2)))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Glyph (point sym) primitives
    
    (define/public ((make-draw-circle-glyph r) v)
      (match-define (vector x y) v)
      (send dc draw-ellipse (- x r -1/2) (- y r -1/2) (* 2 r) (* 2 r)))
    
    (define/public (make-draw-polygon-glyph r sides start-angle)
      (define angles (linear-seq start-angle (+ start-angle (* 2 pi)) (+ 1 sides)))
      (λ (v)
        (match-define (vector x y) v)
        (send dc draw-polygon (map (λ (a) (cons (+ x (* (cos a) r)) (+ y (* (sin a) r))))
                                   angles))))
    
    (define/public (make-draw-star-glyph r sides start-angle)
      (define angles (linear-seq start-angle (+ start-angle (* 2 pi)) (+ 1 (* 2 sides))))
      (λ (v)
        (match-define (vector x y) v)
        (define pts
          (for/list ([a  (in-list angles)] [i  (in-naturals)])
            (define r-cos-a (* r (cos a)))
            (define r-sin-a (* r (sin a)))
            (cond [(odd? i)  (cons (+ x r-cos-a) (+ y r-sin-a))]
                  [else      (cons (+ x (* 1/2 r-cos-a)) (+ y (* 1/2 r-sin-a)))])))
        (send dc draw-polygon pts)))
    
    (define/public (make-draw-flare-glyph r sticks start-angle)
      (define step (/ (* 2 pi) sticks))
      (define angles (build-list sticks (λ (n) (+ start-angle (* n step)))))
      (λ (v)
        (match-define (vector x y) v)
        (for ([a  (in-list angles)])
          (send dc draw-line x y (+ x (* (cos a) r)) (+ y (* (sin a) r))))))
    
    (define/public (make-draw-tick r angle)
      (define dx (* (cos angle) r))
      (define dy (* (sin angle) r))
      (λ (v)
        (match-define (vector x y) v)
        (send dc draw-line (- x dx) (- y dy) (+ x dx) (+ y dy))))
    
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
        (match-define (vector x y) v)
        (define head-x (+ x dx))
        (define head-y (+ y dy))
        (define tail-x (- x dx))
        (define tail-y (- y dy))
        (send dc draw-line head-x head-y tail-x tail-y)
        (send dc draw-line head-x head-y (- head-x dx1) (- head-y dy1))
        (send dc draw-line head-x head-y (- head-x dx2) (- head-y dy2))))
    
    (define/public (draw-arrow-glyph v r angle)
      ((make-draw-arrow-glyph r angle) v))
    
    (define/public (make-draw-text-glyph str)
      (define-values (x-size y-size _1 _2) (get-text-extent str))
      (define dx (* 1/2 x-size))
      (define dy (* 1/2 y-size))
      (λ (v)
        (match-define (vector x y) v)
        (send dc draw-text str (- x dx) (- y dy) #t)))
    
    (define ((mix-draw-glyph d1 d2) v)
      (d1 v)
      (d2 v))
    
    (define/public (draw-glyphs vs sym size)
      (let-values ([(real-sym size)  (translate-glyph-sym+size sym size)])
        (define pen (send dc get-pen))
        (define color (send pen get-color))
        (define width (send pen get-width))
        (define style (send pen get-style))
        (define draw-glyph
          (cond
            [(string? real-sym)  (set-font-size (* 2 size))
                                    (set-text-foreground color)
                                    (make-draw-text-glyph real-sym)]
            [(symbol? real-sym)
             (define r (* 1/2 size))
             (define line-sym
               (cond [(hash-has-key? full-glyph-hash real-sym)  (set-pen color width 'transparent)
                                                                (set-brush color 'solid)
                                                                (hash-ref full-glyph-hash real-sym)]
                     [else  (set-pen color width 'solid)
                            (set-brush color 'transparent)
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
               [(point pixel dot)  (set-pen color (* 1/2 r) 'solid)
                                   (λ (v) (draw-point v))]
               [(odot)        (set-pen color 1 'solid)
                              (mix-draw-glyph (make-draw-circle-glyph (+ width r))
                                              (λ (v) (draw-point v)))]
               ; flares
               [(plus)        (make-draw-flare-glyph r 4 0)]
               [(times)       (make-draw-flare-glyph r 4 (* 1/4 pi))]
               [(5asterisk)   (make-draw-flare-glyph r 5 (* -1/2 pi))]
               [(asterisk)    (make-draw-flare-glyph r 6 (* -1/2 pi))]
               [(oplus)       (mix-draw-glyph (make-draw-circle-glyph (+ width r))
                                              (make-draw-flare-glyph r 4 0))]
               [(otimes)      (mix-draw-glyph (make-draw-circle-glyph (+ width r))
                                              (make-draw-flare-glyph r 4 (* 1/4 pi)))]
               [(o5asterisk)  (mix-draw-glyph (make-draw-circle-glyph (+ width r))
                                              (make-draw-flare-glyph r 5 (* -1/2 pi)))]
               [(oasterisk)   (mix-draw-glyph (make-draw-circle-glyph (+ width r))
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
    
    (define/public (draw-legend legend-entries x-min x-max y-min y-max)
      (define n (length legend-entries))
      (match-define (list (legend-entry labels draws) ...) legend-entries)
      
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
      
      (define legend-x-max (+ legend-x-min legend-x-size))
      (define legend-y-max (+ legend-y-min legend-y-size))
      
      (define label-x-min (+ legend-x-min horiz-gap))
      (define draw-x-min (+ legend-x-min (* 2 horiz-gap) labels-x-size horiz-gap))
      (define draw-x-max (+ draw-x-min draw-x-size))
      
      (set-alpha (plot-legend-box-alpha))
      (set-minor-pen)
      (set-brush (plot-background) 'solid)
      (draw-rectangle (vector legend-x-min legend-y-min) (vector legend-x-max legend-y-max))
      
      (set-clipping-rect (vector legend-x-min legend-y-min) (vector legend-x-max legend-y-max))
      (for ([label  (in-list labels)]
            [draw   (in-list draws)]
            [i      (in-naturals)])
        (define label-y-min (+ legend-y-min top-gap (* i baseline-skip)))
        (define draw-y-min (+ label-y-min (* 1/2 baseline)))
        (define draw-y-max (+ draw-y-min draw-y-size))
        
        (reset-drawing-params)
        (draw-text label (vector label-x-min label-y-min) #:outline? #t)
        (draw this draw-x-min draw-x-max draw-y-min draw-y-max))
      
      (clear-clipping-rect))
    ))  ; end class

(define-struct/contract legend-entry
  ([label string?]
   [draw ((is-a?/c plot-area%) real? real? real? real? . -> . void?)])
  #:transparent)
