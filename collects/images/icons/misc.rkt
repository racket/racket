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
          octagon-icon octagon-flomap
          stop-sign-icon stop-sign-flomap
          stop-signs-icon stop-signs-flomap
          foot-icon foot-flomap
          magnifying-glass-icon magnifying-glass-flomap
          left-magnifying-glass-icon  left-magnifying-glass-flomap
          bomb-icon bomb-flomap
          left-bomb-icon left-bomb-flomap
          clock-icon clock-flomap)
         (only-doc-out (all-defined-out)))

(define (flat-regular-polygon-flomap sides start color size)
  (let ([start  (- start)])
    (draw-icon-flomap
     32 32 (λ (dc)
             (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
             (send dc set-brush color 'solid)
             (define dθ (/ (* 2 pi) sides))
             (define θs (sequence->list (in-range start (+ start (* 2 pi)) dθ)))
             (define max-frac (apply max (append (map (compose abs cos) θs)
                                                 (map (compose abs sin) θs))))
             (send dc draw-polygon (for/list ([θ  (in-list θs)])
                                     (cons (+ 15.5 (/ (* 15.5 (cos θ)) max-frac))
                                           (+ 15.5 (/ (* 15.5 (sin θ)) max-frac))))))
     (/ size 32))))

(defproc (regular-polygon-flomap [sides exact-positive-integer?]
                                 [start real?]
                                 [color (or/c string? (is-a?/c color%))]
                                 [height (and/c rational? (>=/c 0)) (default-icon-height)]
                                 [material deep-flomap-material-value? (default-icon-material)]
                                 ) flomap?
  (make-cached-flomap
   [height sides start color material]
   (flomap-render-icon (flat-regular-polygon-flomap sides start color height) material)))

(defproc (octagon-flomap [color (or/c string? (is-a?/c color%))]
                         [height (and/c rational? (>=/c 0)) (default-icon-height)]
                         [material deep-flomap-material-value? (default-icon-material)]) flomap?
  #:document-body
  (regular-polygon-flomap 8 (/ (* 2 pi) 16) color height material))

(defproc (stop-sign-flomap [color (or/c string? (is-a?/c color%))]
                           [height (and/c rational? (>=/c 0)) (default-icon-height)]
                           [material deep-flomap-material-value? (default-icon-material)]) flomap?
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
     (flomap-cc-superimpose fm (x-flomap light-metal-icon-color (* 22 scale) metal-icon-material)))))

(defproc (stop-signs-flomap [color (or/c string? (is-a?/c color%))]
                            [height (and/c rational? (>=/c 0)) (default-icon-height)]
                            [material deep-flomap-material-value? (default-icon-material)]) flomap?
  (define fm (stop-sign-flomap color (* height 2/3) material))
  (flomap-pin* 3/16 1/4 0 0
               fm (flomap-pin* 3/16 1/4 0 0 fm fm)))

(defproc (foot-flomap [color (or/c string? (is-a?/c color%))]
                      [height (and/c rational? (>=/c 0)) (default-icon-height)]
                      [material deep-flomap-material-value? (default-icon-material)]) flomap?
  (make-cached-flomap
   [height color material]
   (draw-rendered-icon-flomap
    32 32 (λ (dc)
            (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
            (send dc set-brush color 'solid)
            (draw-ellipse/smoothed dc 4 8 24 24)
            (draw-ellipse/smoothed dc 0 10 5 4.5)
            (draw-ellipse/smoothed dc 3 4.5 5.5 5.5)
            (draw-ellipse/smoothed dc 8.75 1 6.25 6.25)
            (draw-ellipse/smoothed dc 16 0 7 7)
            (draw-ellipse/smoothed dc 23.5 1.5 8.5 10))
    (/ height 32)
    material)))

;; ---------------------------------------------------------------------------------------------------
;; Magnifying glass

(define magnifying-glass-material
  (deep-flomap-material-value
   'glass 1.0 0.75 1.0
   0.25 0.15 1.0
   0.25 0.25 0.0
   0.0))

(defproc (magnifying-glass-flomap [frame-color (or/c string? (is-a?/c color%))]
                                  [handle-color (or/c string? (is-a?/c color%))]
                                  [height (and/c rational? (>=/c 0)) (default-icon-height)]
                                  [material deep-flomap-material-value? (default-icon-material)]
                                  ) flomap?
  (make-cached-flomap
   [height frame-color handle-color material]
   (define scale (/ height 32))
   (define glass-fm
     (let* ([fm  (draw-icon-flomap
                  18 18 (λ (dc)
                          (set-icon-pen dc (icon-color->outline-color "azure") 1 'solid)
                          (send dc set-brush "azure" 'solid)
                          (draw-ellipse/smoothed dc 0 0 18 18))
                  scale)]
            [dfm  (flomap->deep-flomap fm)]
            [dfm  (deep-flomap-bulge-spheroid dfm (* 4 scale))]
            [dfm  (deep-flomap-raise dfm (* 4 scale))])
       (deep-flomap-render-icon dfm magnifying-glass-material)))
   
   (define circle-fm
     (let* ([fm  (draw-icon-flomap
                  28 28 (λ (dc)
                          (define outline-color (icon-color->outline-color frame-color))
                          (send dc set-pen outline-color 3 'solid)
                          (send dc set-brush outline-color 'solid)
                          (draw-ellipse/smoothed dc 1 1 26 26)
                          (send dc set-pen frame-color 1 'solid)
                          (send dc set-brush frame-color 'solid)
                          (draw-ellipse/smoothed dc 1 1 26 26))
                  scale)]
            [indent-fm  (draw-icon-flomap
                         28 28 (λ (dc)
                                 (send dc set-pen frame-color 1 'solid)
                                 (send dc set-brush frame-color 'solid)
                                 (draw-ellipse/smoothed dc 5 5 18 18))
                         scale)]
            [indent-dfm  (flomap->deep-flomap indent-fm)]
            [indent-dfm  (deep-flomap-raise indent-dfm (* -4 scale))]
            [dfm  (flomap->deep-flomap fm)]
            [dfm  (deep-flomap-raise dfm (* 12 scale))]
            [dfm  (deep-flomap-cc-superimpose 'add dfm indent-dfm)]
            [dfm  (deep-flomap-smooth-z dfm (* 2/3 scale))])
       (deep-flomap-render-icon dfm metal-icon-material)))
   
   (define handle-fm
     (let* ([fm  (draw-icon-flomap
                  11 11 (λ (dc)
                          (set-icon-pen dc (icon-color->outline-color handle-color) 1 'solid)
                          (send dc set-brush handle-color 'solid)
                          (define p (new dc-path%))
                          (send p move-to 4 0)
                          (send p line-to 10 5)
                          (send p curve-to 10 8 8 10 5 10)
                          (send p line-to 0 4)
                          (send p move-to 4 0)
                          (send dc draw-path p))
                  scale)])
       (flomap-render-icon fm material)))
   
   (flomap-pin* 0 0 21/28 21/28
                handle-fm
                (flomap-pin* 1/2 1/2 1/2 1/2 circle-fm glass-fm))))

(defproc (left-magnifying-glass-flomap [frame-color (or/c string? (is-a?/c color%))]
                                       [handle-color (or/c string? (is-a?/c color%))]
                                       [height (and/c rational? (>=/c 0)) (default-icon-height)]
                                       [material deep-flomap-material-value? (default-icon-material)]
                                       ) flomap?
  (flomap-flip-horizontal (magnifying-glass-flomap frame-color handle-color height material)))

;; ---------------------------------------------------------------------------------------------------
;; Bomb

(defproc (left-bomb-flomap [cap-color (or/c string? (is-a?/c color%))]
                           [bomb-color (or/c string? (is-a?/c color%))]
                           [height (and/c rational? (>=/c 0)) (default-icon-height)]
                           [material deep-flomap-material-value? (default-icon-material)]
                           ) flomap?
  (make-cached-flomap
   [height cap-color bomb-color material]
   (define scale (/ height 32))
   (define fuse-fm
     (let* ([fm  (draw-icon-flomap
                  10 25 (λ (dc)
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
                  scale)]
            [dfm  (flomap->deep-flomap fm)]
            [dfm  (deep-flomap-icon-style dfm)]
            [dfm  (deep-flomap-scale-z dfm 1)])
       (deep-flomap-render-icon dfm matte-material)))
   
   (define (bomb-cap-flomap color)
     (draw-icon-flomap
      20 20 (λ (dc)
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
      scale))
   
   (define cap-fm
     (let* ([cap-fm  (bomb-cap-flomap cap-color)]
            [cap-dfm  (flomap->deep-flomap cap-fm)]
            [cap-dfm  (deep-flomap-icon-style cap-dfm)])
       (deep-flomap-render-icon cap-dfm material)))
   
   (define sphere-fm
     (let* ([sphere-fm  (draw-icon-flomap
                         30 30 (λ (dc)
                                 (set-icon-pen dc (icon-color->outline-color bomb-color) 1 'solid)
                                 (send dc set-brush bomb-color 'solid)
                                 (draw-ellipse/smoothed dc 0 0 30 30))
                         scale)]
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

(defproc (bomb-flomap [cap-color (or/c string? (is-a?/c color%))]
                      [bomb-color (or/c string? (is-a?/c color%))]
                      [height (and/c rational? (>=/c 0)) (default-icon-height)]
                      [material deep-flomap-material-value? (default-icon-material)]
                      ) flomap?
  (flomap-flip-horizontal (left-bomb-flomap cap-color bomb-color height material)))

;; ===================================================================================================
;; Clock

(define clock-shell-material
  (deep-flomap-material-value
   'glass 3.0 0.75 0.0
   0.5 0.15 1.0
   0.1 0.1 0.6
   0.0))

(defproc (clock-flomap [height (and/c rational? (>=/c 0)) (default-icon-height)]
                       [face-color (or/c string? (is-a?/c color%)) light-metal-icon-color]
                       [hand-color (or/c string? (is-a?/c color%)) "firebrick"]
                       [hours (integer-in 0 11) 1]
                       [minutes (real-in 0 60) 33]) flomap?
  (make-cached-flomap
   [height face-color hand-color hours minutes]
  (define R 12)
  (define hour-θ (* (+ (- hours 3) (/ minutes 60)) (/ (* 2 pi) 12)))
  (define minute-θ (* (- minutes 15) (/ (* 2 pi) 60)))
  (define 60-degrees (* 60 (/ (* 2 pi) 180)))
  (define scale (/ height 32))
  
  (define face-fm
    (draw-icon-flomap
     32 32 (λ (dc)
             ;; face
             (set-icon-pen dc (icon-color->outline-color face-color) 1 'solid)
             (send dc set-brush face-color 'solid)
             (draw-ellipse/smoothed dc 0 0 32 32)
             ;; ticks
             (set-icon-pen dc "black" 1 'solid)
             (for ([θ  (in-range 0 (* 2 pi) (* 1/6 pi))]
                   [i  (in-cycle (in-range 0 3))])
               (define r (if (= i 0) 2 1))
               (send dc draw-line
                     (+ 15.5 (* (- R r) (cos θ)))
                     (+ 15.5 (* (- R r) (sin θ)))
                     (+ 15.5 (* R (cos θ)))
                     (+ 15.5 (* R (sin θ)))))
             (set-icon-pen dc (icon-color->outline-color hand-color) 1/2 'solid)
             (send dc set-brush hand-color 'solid)
             ;; minute hand
             (send dc draw-polygon
                   (list (cons (+ 15.5 (* R (cos minute-θ)))
                               (+ 15.5 (* R (sin minute-θ))))
                         (cons (+ 15.5 (* 1.5 (cos (+ minute-θ 60-degrees))))
                               (+ 15.5 (* 1.5 (sin (+ minute-θ 60-degrees)))))
                         (cons (+ 15.5 (* 1.5 (cos (- minute-θ 60-degrees))))
                               (+ 15.5 (* 1.5 (sin (- minute-θ 60-degrees)))))))
             ;; hour hand
             (send dc draw-polygon
                   (list (cons (+ 15.5 (* (- R 4) (cos hour-θ)))
                               (+ 15.5 (* (- R 4) (sin hour-θ))))
                         (cons (+ 15.5 (* 1.5 (cos (+ hour-θ 60-degrees))))
                               (+ 15.5 (* 1.5 (sin (+ hour-θ 60-degrees)))))
                         (cons (+ 15.5 (* 1.5 (cos (- hour-θ 60-degrees))))
                               (+ 15.5 (* 1.5 (sin (- hour-θ 60-degrees))))))))
     scale))
  
  (define shell-fm
    (draw-icon-flomap
     32 32 (λ (dc)
             (set-icon-pen dc (icon-color->outline-color "white") 1 'solid)
             (send dc set-brush "white" 'solid)
             (draw-ellipse/smoothed dc 0 0 32 32))
     scale))
  
  (let* ([dfm  (flomap->deep-flomap shell-fm)]
         [dfm  (deep-flomap-bulge-spheroid dfm (* 8 scale))])
    (deep-flomap-render-icon dfm clock-shell-material face-fm))))

;; ===================================================================================================
;; Bitmaps (icons)

(defproc (regular-polygon-icon [sides exact-positive-integer?]
                               [start real?]
                               [color (or/c string? (is-a?/c color%))]
                               [height (and/c rational? (>=/c 0)) (default-icon-height)]
                               [material deep-flomap-material-value? (default-icon-material)]
                               ) (is-a?/c bitmap%)
  (flomap->bitmap (regular-polygon-flomap sides start color height material)))

(defproc (clock-icon [height (and/c rational? (>=/c 0)) (default-icon-height)]
                     [face-color (or/c string? (is-a?/c color%)) light-metal-icon-color]
                     [hand-color (or/c string? (is-a?/c color%)) "firebrick"]
                     [hours (integer-in 0 11) 1]
                     [minutes (real-in 0 60) 33]) (is-a?/c bitmap%)
  (flomap->bitmap (clock-flomap height face-color hand-color hours minutes)))

(define-icon-wrappers
  ([color (or/c string? (is-a?/c color%))]
   [height (and/c rational? (>=/c 0)) (default-icon-height)]
   [material deep-flomap-material-value? (default-icon-material)])
  [octagon-icon octagon-flomap]
  [stop-sign-icon stop-sign-flomap]
  [stop-signs-icon stop-signs-flomap]
  [foot-icon foot-flomap])

(define-icon-wrappers
  ([frame-color (or/c string? (is-a?/c color%))]
   [handle-color (or/c string? (is-a?/c color%))]
   [height (and/c rational? (>=/c 0)) (default-icon-height)]
   [material deep-flomap-material-value? (default-icon-material)])
  [magnifying-glass-icon magnifying-glass-flomap]
  [left-magnifying-glass-icon  left-magnifying-glass-flomap])

(define-icon-wrappers
  ([cap-color (or/c string? (is-a?/c color%))]
   [bomb-color (or/c string? (is-a?/c color%))]
   [height (and/c rational? (>=/c 0)) (default-icon-height)]
   [material deep-flomap-material-value? (default-icon-material)])
  [bomb-icon bomb-flomap]
  [left-bomb-icon left-bomb-flomap])
