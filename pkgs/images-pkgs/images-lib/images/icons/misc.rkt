#lang racket/base

(require racket/draw racket/class racket/math racket/sequence
         racket/contract unstable/latent-contract unstable/latent-contract/defthing
         "../private/flomap.rkt"
         "../private/deep-flomap.rkt"
         "../private/utils.rkt"
         "symbol.rkt"
         "style.rkt")

(provide (activate-contract-out
          regular-polygon-icon regular-polygon-flomap
          stop-sign-icon stop-sign-flomap
          stop-signs-icon stop-signs-flomap
          close-icon close-flomap
          foot-icon foot-flomap
          magnifying-glass-icon magnifying-glass-flomap
          left-magnifying-glass-icon left-magnifying-glass-flomap
          bomb-icon bomb-flomap
          left-bomb-icon left-bomb-flomap
          clock-icon clock-flomap
          stopwatch-icon stopwatch-flomap
          stethoscope-icon stethoscope-flomap
          short-stethoscope-icon short-stethoscope-flomap
          lock-icon lock-flomap)
         (only-doc-out (all-defined-out)))

(define (flat-regular-polygon-flomap sides start color size)
  (let ([start  (- start)])
    (draw-icon-flomap
     (λ (dc)
       (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
       (send dc set-brush color 'solid)
       (define dθ (/ (* 2 pi) sides))
       (define θs (sequence->list (in-range start (+ start (* 2 pi)) dθ)))
       (define max-frac (apply max (append (map (compose abs cos) θs)
                                           (map (compose abs sin) θs))))
       (send dc draw-polygon (for/list ([θ  (in-list θs)])
                               (cons (+ 15.5 (/ (* 15.5 (cos θ)) max-frac))
                                     (+ 15.5 (/ (* 15.5 (sin θ)) max-frac))))))
     32 32 (/ size 32))))

(defproc (regular-polygon-flomap
          [sides exact-positive-integer?]
          [start real? (- (/ pi sides) (* 1/2 pi))]
          [#:color color (or/c string? (is-a?/c color%))]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (make-cached-flomap
   [height sides start color material]
   (flomap-render-icon (flat-regular-polygon-flomap sides start color height) material)))

(defproc (stop-sign-flomap
          [#:color color (or/c string? (is-a?/c color%)) halt-icon-color]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (make-cached-flomap
   [height color material]
   (define scale (/ height 32))
   (let* ([indent-fm  (fm* 0.5 (flat-x-flomap "black" (* 22 scale)))]
          [indent-dfm  (deep-flomap-raise (flomap->deep-flomap indent-fm) (* -1 scale))]
          [fm   (flat-regular-polygon-flomap 8 (/ (* 2 pi) 16) color height)]
          [dfm  (flomap->deep-flomap fm)]
          [dfm  (deep-flomap-icon-style dfm)]
          [dfm  (deep-flomap-cc-superimpose 'add dfm indent-dfm)]
          [fm  (deep-flomap-render-icon dfm material)])
     (flomap-cc-superimpose fm (x-flomap #:color light-metal-icon-color
                                         #:height (* 22 scale)
                                         #:material metal-icon-material)))))

(defproc (stop-signs-flomap
          [#:color color (or/c string? (is-a?/c color%)) halt-icon-color]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (define fm (stop-sign-flomap #:color color #:height (* height 2/3) #:material material))
  (flomap-pin* 3/16 1/4 0 0 fm fm fm))

(defproc (close-flomap
          [#:color color (or/c string? (is-a?/c color%)) "black"]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (make-cached-flomap
   [height color material]
   (define scale (/ height 32))
   (let* ([indent-fm  (fm* 0.5 (flat-x-flomap "black" (* 22 scale) #:thickness 6))]
          [indent-dfm  (deep-flomap-raise (flomap->deep-flomap indent-fm) (* -1 scale))]
          [fm   (draw-rendered-icon-flomap
                 (λ (dc)
                   (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
                   (send dc set-brush color 'solid)
                   (send dc draw-ellipse 0 0 31 31))
                 32 32 (/ height 32) material)]
          [dfm  (flomap->deep-flomap fm)]
          [dfm  (deep-flomap-icon-style dfm)]
          [dfm  (deep-flomap-cc-superimpose 'add dfm indent-dfm)]
          [fm  (deep-flomap-render-icon dfm material)])
     (flomap-cc-superimpose fm (x-flomap #:color light-metal-icon-color
                                         #:height (* 22 scale)
                                         #:material metal-icon-material
                                         #:thickness 6)))))

(defproc (foot-flomap
          [#:color color (or/c string? (is-a?/c color%))]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (make-cached-flomap
   [height color material]
   (draw-rendered-icon-flomap
    (λ (dc)
      (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
      (send dc set-brush color 'solid)
      (send dc draw-ellipse 4 8 23 23)
      (send dc draw-ellipse 0 10 4 3.5)
      (send dc draw-ellipse 3 4.5 4.5 4.5)
      (send dc draw-ellipse 8.75 1 5.25 5.25)
      (send dc draw-ellipse 16 0 6 6)
      (send dc draw-ellipse 23.5 1.5 7.5 9))
    32 32 (/ height 32) material)))

;; ---------------------------------------------------------------------------------------------------
;; Magnifying glass

(define magnifying-glass-material
  (deep-flomap-material-value
   'glass 1.0 0.75 1.0
   0.25 0.15 1.0
   0.25 0.25 0.0
   0.0))

(defproc (magnifying-glass-flomap
          [#:frame-color frame-color (or/c string? (is-a?/c color%)) light-metal-icon-color]
          [#:handle-color handle-color (or/c string? (is-a?/c color%)) "brown"]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (make-cached-flomap
   [height frame-color handle-color material]
   (define scale (/ height 32))
   (define glass-fm
     (let* ([fm  (draw-icon-flomap
                  (λ (dc)
                    (set-icon-pen dc (icon-color->outline-color "azure") 1 'solid)
                    (send dc set-brush "azure" 'solid)
                    (send dc draw-ellipse 0 0 17 17))
                  18 18 scale)]
            [dfm  (flomap->deep-flomap fm)]
            [dfm  (deep-flomap-bulge-spheroid dfm (* 4 scale))]
            [dfm  (deep-flomap-raise dfm (* 4 scale))])
       (deep-flomap-render-icon dfm magnifying-glass-material)))
   
   (define circle-fm
     (let* ([fm  (draw-icon-flomap
                  (λ (dc)
                    (define outline-color (icon-color->outline-color frame-color))
                    (send dc set-pen outline-color 3 'solid)
                    (send dc set-brush outline-color 'solid)
                    (send dc draw-ellipse 1 1 25 25)
                    (send dc set-pen frame-color 1 'solid)
                    (send dc set-brush frame-color 'solid)
                    (send dc draw-ellipse 1 1 25 25))
                  28 28 scale)]
            [indent-fm  (draw-icon-flomap
                         (λ (dc)
                           (send dc set-pen frame-color 1 'solid)
                           (send dc set-brush frame-color 'solid)
                           (send dc draw-ellipse 5 5 17 17))
                         28 28 scale)]
            [indent-dfm  (flomap->deep-flomap indent-fm)]
            [indent-dfm  (deep-flomap-raise indent-dfm (* -4 scale))]
            [dfm  (flomap->deep-flomap fm)]
            [dfm  (deep-flomap-raise dfm (* 12 scale))]
            [dfm  (deep-flomap-cc-superimpose 'add dfm indent-dfm)]
            [dfm  (deep-flomap-smooth-z dfm (* 2/3 scale))])
       (deep-flomap-render-icon dfm metal-icon-material)))
   
   (define handle-fm
     (let* ([fm  (draw-icon-flomap
                  (λ (dc)
                    (set-icon-pen dc (icon-color->outline-color handle-color) 1 'solid)
                    (send dc set-brush handle-color 'solid)
                    (define p (new dc-path%))
                    (send p move-to 4 0)
                    (send p line-to 10 5)
                    (send p curve-to 10 8 8 10 5 10)
                    (send p line-to 0 4)
                    (send p move-to 4 0)
                    (send dc draw-path p))
                  11 11 scale)])
       (flomap-render-icon fm material)))
   
   (flomap-pin* 0 0 21/28 21/28
                handle-fm
                (flomap-pin* 1/2 1/2 1/2 1/2 circle-fm glass-fm))))

(defproc (left-magnifying-glass-flomap
          [#:frame-color frame-color (or/c string? (is-a?/c color%)) light-metal-icon-color]
          [#:handle-color handle-color (or/c string? (is-a?/c color%)) "brown"]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (flomap-flip-horizontal
   (magnifying-glass-flomap #:frame-color frame-color #:handle-color handle-color
                            #:height height #:material material)))

;; ---------------------------------------------------------------------------------------------------
;; Bomb

(defproc (left-bomb-flomap
          [#:cap-color cap-color (or/c string? (is-a?/c color%)) light-metal-icon-color]
          [#:bomb-color bomb-color (or/c string? (is-a?/c color%)) dark-metal-icon-color]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (make-cached-flomap
   [height cap-color bomb-color material]
   (define scale (/ height 32))
   (define fuse-fm
     (let* ([fm  (draw-icon-flomap
                  (λ (dc)
                    (send dc set-pen "darkred" 1 'solid)
                    (send dc set-brush "gold" 'solid)
                    (draw-path-commands dc '((m 3.5 0)
                                             (c -5 0 -3.29080284 10.4205 -3 11.5
                                                1.1137011 4.1343 2 6.5 0 8.5
                                                -0.5711131 2.0524 1.5 4 3.5 3.5
                                                2.5711131 -2.5524 3.1327042 -5.5355 2 -9.5
                                                -2 -7 -2 -9 -1.5 -9
                                                0 1 -0.5 2 1 3.5
                                                2 0.5 4 -1.5 3.5 -3.5
                                                -2 -2 -2 -5 -5.5 -5))
                                        0 0))
                  10 25 scale)]
            [dfm  (flomap->deep-flomap fm)]
            [dfm  (deep-flomap-icon-style dfm)]
            [dfm  (deep-flomap-scale-z dfm 1)])
       (deep-flomap-render-icon dfm matte-material)))
   
   (define (bomb-cap-flomap color)
     (draw-icon-flomap
      (λ (dc)
        (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
        (send dc set-brush color 'solid)
        (draw-path-commands dc '((m 1.5 11.5)
                                 (l 10 -10 2.5 2.5)
                                 (c 4 5 -5 14 -10 10)
                                 (l -2.5 -2.5))
                            0 0)
        (draw-path-commands dc '((m 1.5 11.5)
                                 (c -2 -5 5 -12 10 -10
                                    4 5 -5 14 -10 10))
                            0 0))
      20 20 scale))
   
   (define cap-fm
     (let* ([cap-fm  (bomb-cap-flomap cap-color)]
            [cap-dfm  (flomap->deep-flomap cap-fm)]
            [cap-dfm  (deep-flomap-icon-style cap-dfm)])
       (deep-flomap-render-icon cap-dfm material)))
   
   (define sphere-fm
     (let* ([sphere-fm  (draw-icon-flomap
                         (λ (dc)
                           (set-icon-pen dc (icon-color->outline-color bomb-color) 1 'solid)
                           (send dc set-brush bomb-color 'solid)
                           (send dc draw-ellipse 0 0 29 29))
                         30 30 scale)]
            [cap-fm  (bomb-cap-flomap cap-color)]
            [cap-dfm  (flomap->deep-flomap cap-fm)]
            [cap-dfm  (deep-flomap-raise cap-dfm (* -2 scale))]
            [cap-dfm  (deep-flomap-smooth-z cap-dfm (* 1 scale))]
            [sphere-dfm  (flomap->deep-flomap sphere-fm)]
            [sphere-dfm  (deep-flomap-bulge-spheroid sphere-dfm (* 15 scale))]
            [sphere-dfm  (deep-flomap-inset sphere-dfm 2 2 0 0)]
            [sphere-dfm  (deep-flomap-lt-superimpose 'add sphere-dfm cap-dfm)])
       (deep-flomap-render-icon sphere-dfm material)))
   (flomap-lt-superimpose sphere-fm cap-fm fuse-fm)))

(defproc (bomb-flomap
          [#:cap-color cap-color (or/c string? (is-a?/c color%)) light-metal-icon-color]
          [#:bomb-color bomb-color (or/c string? (is-a?/c color%)) dark-metal-icon-color]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (flomap-flip-horizontal
   (left-bomb-flomap #:cap-color cap-color #:bomb-color bomb-color
                     #:height height #:material material)))

;; ---------------------------------------------------------------------------------------------------
;; Clock

(define clock-shell-material
  (deep-flomap-material-value
   'glass 1.5 0.75 0.0
   0.15 0.05 1.0
   0.1 0.1 0.6
   0.0))

(defproc (clock-flomap
          [hours (integer-in 0 11) 1]
          [minutes (real-in 0 60) 47]
          [#:face-color face-color (or/c string? (is-a?/c color%)) light-metal-icon-color]
          [#:hand-color hand-color (or/c string? (is-a?/c color%)) "firebrick"]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          ) flomap?
  (make-cached-flomap
   [height face-color hand-color hours minutes]
   (define R 12.5)
   (define hour-θ (* (+ (- hours 3) (/ minutes 60)) (/ (* 2 pi) 12)))
   (define minute-θ (* (- minutes 15) (/ (* 2 pi) 60)))
   (define scale (/ height 32))
   (define face-fm
     (flomap-cc-superimpose
      ;; face and ticks
      (draw-icon-flomap
       (λ (dc)
         (set-icon-pen dc (icon-color->outline-color (icon-color->outline-color face-color))
                       1 'solid)
         (send dc set-brush face-color 'solid)
         (send dc draw-ellipse 0 0 31 31)
         (set-icon-pen dc "black" 1 'solid)
         (for ([i  (in-range 60)]
               [r  (in-cycle (list 1.5 .5 .5 .5 .5
                                   1.0 .5 .5 .5 .5
                                   1.0 .5 .5 .5 .5))]
               [t  (in-cycle (list 1.0 .25 .25 .25 .25
                                   .75 .25 .25 .25 .25
                                   .75 .25 .25 .25 .25))])
           (define θ (* i (* 1/30 pi)))
           (set-icon-pen dc "black" t 'solid)
           (send dc draw-line
                 (+ 15.5 (* (- R r) (cos θ)))
                 (+ 15.5 (* (- R r) (sin θ)))
                 (+ 15.5 (* R (cos θ)))
                 (+ 15.5 (* R (sin θ))))))
       32 32 scale)
      ;; lambda logo
      (fm* 0.33 (lambda-flomap #:color face-color #:height (* 1/2 height)
                               #:material glass-icon-material))
      ;; minute hand
      (draw-rendered-icon-flomap
       (λ (dc)
         (set-icon-pen dc (icon-color->outline-color hand-color) 1/2 'solid)
         (send dc set-brush hand-color 'solid)
         (send dc draw-polygon
               (list (cons (+ 15.5 (* R (cos minute-θ)))
                           (+ 15.5 (* R (sin minute-θ))))
                     (cons (+ 15.5 (* 1 (cos (+ minute-θ (* 1/2 pi)))))
                           (+ 15.5 (* 1 (sin (+ minute-θ (* 1/2 pi))))))
                     (cons (+ 15.5 (* 1 (cos (+ minute-θ pi))))
                           (+ 15.5 (* 1 (sin (+ minute-θ pi)))))
                     (cons (+ 15.5 (* 1 (cos (+ minute-θ (* 3/2 pi)))))
                           (+ 15.5 (* 1 (sin (+ minute-θ (* 3/2 pi)))))))))
       32 32 scale metal-icon-material)
      ;; hour hand
      (draw-rendered-icon-flomap
       (λ (dc)
         (set-icon-pen dc (icon-color->outline-color hand-color) 1/2 'solid)
         (send dc set-brush hand-color 'solid)
         (send dc draw-polygon
               (list (cons (+ 15.5 (* (- R 5) (cos hour-θ)))
                           (+ 15.5 (* (- R 5) (sin hour-θ))))
                     (cons (+ 15.5 (* 1.25 (cos (+ hour-θ (* 1/2 pi)))))
                           (+ 15.5 (* 1.25 (sin (+ hour-θ (* 1/2 pi))))))
                     (cons (+ 15.5 (* 1.25 (cos (+ hour-θ pi))))
                           (+ 15.5 (* 1.25 (sin (+ hour-θ pi)))))
                     (cons (+ 15.5 (* 1.25 (cos (+ hour-θ (* 3/2 pi)))))
                           (+ 15.5 (* 1.25 (sin (+ hour-θ (* 3/2 pi)))))))))
       32 32 scale metal-icon-material)))
   
   (define shell-fm
     (draw-icon-flomap
      (λ (dc)
        (set-icon-pen dc "white" 1 'solid)
        (send dc set-brush "white" 'solid)
        (send dc draw-ellipse 1 1 29 29))
      32 32 scale))
   
   (let* ([dfm  (flomap->deep-flomap shell-fm)]
          [dfm  (deep-flomap-bulge-spheroid dfm (* 9 scale))]
          [dfm  (deep-flomap-raise dfm (* -2 scale))]
          [dfm  (deep-flomap-smooth-z dfm (* 1/3 scale))])
     (flomap-cc-superimpose
      face-fm
      (deep-flomap-render-icon dfm clock-shell-material face-fm)))))

(defproc (stopwatch-flomap
          [hours (integer-in 0 11) 0]
          [minutes (real-in 0 60) 47]
          [#:face-color face-color (or/c string? (is-a?/c color%)) light-metal-icon-color]
          [#:hand-color hand-color (or/c string? (is-a?/c color%)) "firebrick"]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          ) flomap?
  (make-cached-flomap
   [height face-color hand-color hours minutes]
   (define clock-fm (clock-flomap hours minutes
                                  #:face-color face-color
                                  #:hand-color hand-color
                                  #:height (* 30/32 height)))
   (define buttons-fm
     (draw-rendered-icon-flomap
      (λ (dc)
        (set-icon-pen dc (make-object color% 128 0 0) 1 'solid)
        (send dc set-brush (make-object color% 144 0 0) 'solid)
        (send dc draw-polygon '((2 . 4) (4 . 2) (31 . 31)))
        (send dc draw-polygon '((0 . 5) (5 . 0) (6 . 1) (1 . 6)))
        (set-icon-pen dc "black" 1 'solid)
        (send dc set-brush (make-object color% 16 16 16) 'solid)
        (send dc draw-polygon '((28.5 . 5.5) (25.5 . 2.5) (0 . 31)))
        (send dc draw-polygon '((31 . 5) (26 . 0) (24.5 . 1.5) (29.5 . 6.5))))
      32 8 (/ height 32) metal-icon-material))
   (flomap-pin* 1/2 0 1/2 -2/32 buttons-fm clock-fm)))

;; ---------------------------------------------------------------------------------------------------
;; Stethoscopes

(define rubber-t-commands
  '((m 6 13)
    (c 0 6 3 7.5 9.5 7.5
       6.5 0 9.5 -1.5 9.5 -7.5)))

(define rubber-hose-commands
  '((m 15 21.25)
    (c 0 0 1 3.5 -3 4.5
       -4 1 -7 -8.5 -10.5 -3.5
       -3.5 5 4.0182351 8.2793 11 8
       6.981765 -0.2793 13 -4.5 13 -4.5)))

(define left-metal-commands
  '((m 6 1.5)
    (c -4 2 0 5.5 0 11.5)))

(define right-metal-commands
  '((m 25 1.5)
    (c 4 2 0 5.5 0 11.5)))

(defproc (stethoscope-flomap
          [#:color color (or/c string? (is-a?/c color%)) "black"]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]) flomap?
  (define scale (/ height 32))
  (flomap-ct-superimpose
   (draw-rendered-icon-flomap
    (λ (dc)
      (send dc set-pen (make-object pen% color 2 'solid 'round 'round))
      (send dc set-brush "white" 'transparent)
      (draw-path-commands dc rubber-hose-commands 0 0)
      (draw-path-commands dc rubber-t-commands 0 0)
      (send dc set-pen (make-object pen% "black" 3 'solid 'round 'round))
      (send dc draw-line 23.5 1 25 1.5)
      (send dc draw-line 7.5 1 6 1.5))
    32 32 scale rubber-icon-material)
   (draw-rendered-icon-flomap
    (λ (dc)
      (send dc set-pen (make-object pen% dark-metal-icon-color 2.5 'solid 'round 'round))
      (send dc set-brush "white" 'transparent)
      (draw-path-commands dc left-metal-commands 0 0)
      (draw-path-commands dc right-metal-commands 0 0)
      (send dc set-pen (make-object pen% metal-icon-color 2 'solid 'round 'round))
      (draw-path-commands dc left-metal-commands 0 0)
      (draw-path-commands dc right-metal-commands 0 0)
      (set-icon-pen dc dark-metal-icon-color 0.5 'solid)
      (send dc set-brush metal-icon-color 'solid)
      (send dc draw-ellipse 21.25 21.25 10 10)
      (set-icon-pen dc dark-metal-icon-color 0.25 'solid)
      (send dc set-brush light-metal-icon-color 'solid)
      (send dc draw-ellipse 22.25 22.25 8 8))
    32 32 scale metal-icon-material)))

(define short-rubber-t-commands
  '((m 3 12.5)
    (c 0 6 10 6.5 12.5 6.5
       2.5 0 12.5 -0.5 12.5 -6.5)))

(define short-rubber-hose-commands
  '((m 15 19.25)
    (c 0 0 -3 1 -10 1
       -7 0 -6.5 4.5 6 4
       12.5 -0.5 14.5 -5 14.5 -5)))

(defproc (short-stethoscope-flomap
          [#:color color (or/c string? (is-a?/c color%)) "black"]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          ) flomap?
  (define scale (/ height 32))
  (flomap-ct-superimpose
   (draw-rendered-icon-flomap
    (λ (dc)
      (send dc translate 0 6)
      (send dc set-pen (make-object pen% color 2 'solid 'round 'round))
      (send dc set-brush "white" 'transparent)
      (draw-path-commands dc short-rubber-hose-commands 0 0)
      (draw-path-commands dc short-rubber-t-commands 0 0)
      (send dc set-pen (make-object pen% "black" 3 'solid 'round 'round))
      (send dc draw-line 4.5 1 3 1.5)
      (send dc draw-line 26.5 1 28 1.5))
    32 32 scale rubber-icon-material)
   (draw-rendered-icon-flomap
    (λ (dc)
      (send dc translate 0 6)
      (send dc set-pen (make-object pen% dark-metal-icon-color 2.5 'solid 'round 'round))
      (send dc set-brush "white" 'transparent)
      (draw-path-commands dc left-metal-commands -3 0)
      (draw-path-commands dc right-metal-commands 3 0)
      (send dc set-pen (make-object pen% metal-icon-color 2 'solid 'round 'round))
      (draw-path-commands dc left-metal-commands -3 0)
      (draw-path-commands dc right-metal-commands 3 0)
      (set-icon-pen dc dark-metal-icon-color 0.5 'solid)
      (send dc set-brush metal-icon-color 'solid)
      (send dc draw-ellipse 21.25 15.25 10 10)
      (set-icon-pen dc dark-metal-icon-color 0.25 'solid)
      (send dc set-brush light-metal-icon-color 'solid)
      (send dc draw-ellipse 22.25 16.25 8 8))
    32 32 scale metal-icon-material)))

;; ---------------------------------------------------------------------------------------------------
;; Lock

(define shackle-commands
  '((m 10.5 0)
    (c -6 0 -10 4 -10 10)
    (l 0 5)
    (l 4 0)
    (l 0 -5)
    (c 0 -4 2 -6 6 -6)
    (c 4 0 6 2 6 6)
    (l 0 5)
    (l 4 0)
    (l 0 -5)
    (c 0 -6 -4 -10 -10 -10)))

(defproc (lock-flomap
          [open? boolean? #f]
          [#:body-color body-color (or/c string? (is-a?/c color%)) "orange"]
          [#:shackle-color shackle-color (or/c string? (is-a?/c color%)) light-metal-icon-color]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (make-cached-flomap
   [height open? body-color shackle-color material]
   (define scale (/ height 32))
   
   (define body-fm
     (draw-icon-flomap
      (λ (dc)
        (set-icon-pen dc (icon-color->outline-color body-color) 1 'solid)
        (send dc set-brush body-color 'solid)
        (send dc draw-rounded-rectangle 2 0 27 19 2))
      32 20 scale))
   
   (define face-fm
     (draw-icon-flomap
      (λ (dc)
        (send dc set-pen "black" 1 'transparent)
        (send dc set-brush "black" 'solid)
        (send dc draw-ellipse 13 4.5 5 5)
        (send dc draw-polygon '((14.5 . 8)
                                (16.5 . 8)
                                (17.5 . 15.5)
                                (13.5 . 15.5)))
        (send dc set-alpha 1/8)
        (for ([i  (in-range 4)])
          (send dc draw-rectangle 3.5 (+ 3 (* 4 i)) 24 1)))
      32 20 scale))
   
   (define face-alpha-fm (flomap-ref-component face-fm 0))
   
   (define body-rfm
     (let* ([dfm  (flomap->deep-flomap body-fm)]
            [dfm  (deep-flomap-bulge-horizontal dfm (* scale 6))]
            [dfm  (deep-flomap-emboss dfm (* scale 3) (* scale 2))]
            [dfm  (deep-flomap-raise dfm (* scale 20))]
            [dfm  (deep-flomap-raise dfm (fm* (* scale -1/2) (flomap-blur face-alpha-fm
                                                                          (* 1/2 scale))))])
       (flomap-cc-superimpose (deep-flomap-render-icon dfm material)
                              face-fm)))
   
   (define shackle-fm
     (draw-icon-flomap
      (λ (dc)
        (set-icon-pen dc (icon-color->outline-color shackle-color) 1 'solid)
        (send dc set-brush shackle-color 'solid)
        (draw-path-commands dc shackle-commands 0 0))
      22 16 scale))
   
   (define shackle-rfm
     (let* ([dfm  (flomap->deep-flomap shackle-fm)]
            [dfm  (deep-flomap-emboss dfm (* scale 3) (* scale 10))])
       (deep-flomap-render-icon dfm metal-icon-material)))
   
   (flomap-pin* 1/2 3/4 (if open? 1 1/2) 0 shackle-rfm body-rfm)))

;; ===================================================================================================
;; Bitmaps (icons)

(define-icon-wrappers
  ([sides exact-positive-integer?]
   [start real? (- (/ pi sides) (* 1/2 pi))]
   [#:color color (or/c string? (is-a?/c color%))]
   [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
   [#:material material deep-flomap-material-value? (default-icon-material)])
  (height)
  [regular-polygon-icon regular-polygon-flomap])

(define-icon-wrappers
  ([hours (integer-in 0 11) 0]
   [minutes (real-in 0 60) 47]
   [#:face-color face-color (or/c string? (is-a?/c color%)) light-metal-icon-color]
   [#:hand-color hand-color (or/c string? (is-a?/c color%)) "firebrick"]
   [#:height height (and/c rational? (>=/c 0)) (default-icon-height)])
  (height)
  [clock-icon clock-flomap]
  [stopwatch-icon stopwatch-flomap])

(define-icon-wrappers
  ([#:color color (or/c string? (is-a?/c color%)) halt-icon-color]
   [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
   [#:material material deep-flomap-material-value? (default-icon-material)])
  (height)
  [stop-sign-icon stop-sign-flomap]
  [stop-signs-icon stop-signs-flomap])

(define-icon-wrappers
  ([#:color color (or/c string? (is-a?/c color%)) "black"]
   [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
   [#:material material deep-flomap-material-value? (default-icon-material)])
  (height)
  [close-icon close-flomap])

(define-icon-wrappers
  ([#:color color (or/c string? (is-a?/c color%))]
   [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
   [#:material material deep-flomap-material-value? (default-icon-material)])
  (height)
  [foot-icon foot-flomap])

(define-icon-wrappers
  ([#:frame-color frame-color (or/c string? (is-a?/c color%)) light-metal-icon-color]
   [#:handle-color handle-color (or/c string? (is-a?/c color%)) "brown"]
   [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
   [#:material material deep-flomap-material-value? (default-icon-material)])
  (height)
  [magnifying-glass-icon magnifying-glass-flomap]
  [left-magnifying-glass-icon  left-magnifying-glass-flomap])

(define-icon-wrappers
  ([#:cap-color cap-color (or/c string? (is-a?/c color%)) light-metal-icon-color]
   [#:bomb-color bomb-color (or/c string? (is-a?/c color%)) dark-metal-icon-color]
   [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
   [#:material material deep-flomap-material-value? (default-icon-material)])
  (height)
  [bomb-icon bomb-flomap]
  [left-bomb-icon left-bomb-flomap])

(define-icon-wrappers
  ([open? boolean? #f]
   [#:body-color body-color (or/c string? (is-a?/c color%)) "orange"]
   [#:shackle-color shackle-color (or/c string? (is-a?/c color%)) light-metal-icon-color]
   [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
   [#:material material deep-flomap-material-value? (default-icon-material)])
  (height)
  [lock-icon lock-flomap])

(define-icon-wrappers
  ([#:color color (or/c string? (is-a?/c color%)) "black"]
   [#:height height (and/c rational? (>=/c 0)) (default-icon-height)])
  (height)
  [stethoscope-icon stethoscope-flomap]
  [short-stethoscope-icon short-stethoscope-flomap])
