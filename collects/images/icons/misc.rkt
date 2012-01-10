#lang racket/base

(require racket/draw racket/class racket/math racket/sequence
         "../private/flomap.rkt"
         "../private/deep-flomap.rkt"
         "../private/utils.rkt"
         "style.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Unrendered flomaps

(define (flat-x-flomap color height)
  (define mn 7.5)
  (define mx 23.5)
  (draw-icon-flomap
   32 32 (λ (dc)
           (send dc set-pen (make-object pen% "black" 12 'solid 'projecting 'miter))
           (send dc draw-line mn mn mx mx)
           (send dc draw-line mn mx mx mn)
           (send dc set-pen (make-object pen% color 10 'solid 'projecting  'miter))
           (send dc draw-line mn mn mx mx)
           (send dc draw-line mn mx mx mn))
   (/ height 32)))

(define (flat-check-flomap color height)
  (draw-icon-flomap
   32 32 (λ (dc)
           (send dc set-brush color 'solid)
           (draw-path-commands
            dc 0 0 '((m 0 19)
                     (c 0 0 7 4 14 12 5.5 -13.5 17 -23 17 -23)
                     (l -9 -8)
                     (c 0 0 -6.5 7.5 -9.5 16 -2.5 -4 -6 -6.5 -6 -6.5)
                     (l -6 9))))
   (/ height 32)))

(define (flat-regular-polygon-flomap sides start color size)
  (draw-icon-flomap
   32 32 (λ (dc)
           (send dc set-brush color 'solid)
           (define dθ (/ (* 2 pi) sides))
           (define θs (sequence->list (in-range start (+ start (* 2 pi)) dθ)))
           (define max-frac (apply max (append (map (compose abs cos) θs)
                                               (map (compose abs sin) θs))))
           (send dc draw-polygon (for/list ([θ  (in-list θs)])
                                   (cons (+ 15.5 (/ (* 15.5 (cos θ)) max-frac))
                                         (+ 15.5 (/ (* 15.5 (sin θ)) max-frac))))))
   (/ size 32)))

;; ===================================================================================================
;; Rendered flomaps

(define (text-flomap str font color trim? outline?
                     [height (default-icon-height)]
                     [material (default-icon-material)])
  (define family (send font get-family))
  (define style (send font get-style))
  (define weight (send font get-weight))
  (define underline? (send font get-underlined))
  (define smoothing (send font get-smoothing))
  (make-cached-flomap
   [height str family style weight underline? smoothing color trim? outline? material]
   (let ([font  (make-object font% (min 255 (inexact->exact (ceiling height)))
                  family style weight underline? smoothing #t)])
     (define-values (w h) (get-text-size str font))
     (define outline-amt (if outline? (/ height 32) 0))
     (define ceiling-amt (inexact->exact (ceiling outline-amt)))
     (define fm
       (let* ([fm  (draw-flomap
                    w h (λ (dc)
                          (send dc set-font font)
                          (send dc set-text-foreground color)
                          (send dc draw-text str 0 0 #t)))]
              [fm  (if trim? (flomap-trim fm) fm)]
              [fm  (flomap-resize fm #f (- height (* 2 ceiling-amt)))]
              [fm  (flomap-inset fm ceiling-amt)]
              [fm  (if outline? (flomap-outlined fm outline-amt) fm)])
         fm))
     (flomap-render-icon fm material))))

(define (x-flomap color [height (default-icon-height)] [material (default-icon-material)])
  (make-cached-flomap
   [height color material]
   (define scale (/ height 32))
   (let* ([fm   (flat-x-flomap color height)]
          [dfm  (flomap->deep-flomap fm)]
          [dfm  (deep-flomap-icon-style dfm)]
          [dfm  (deep-flomap-raise dfm (* -8 scale))])
     (deep-flomap-render-icon dfm material))))

(define (check-flomap color [height (default-icon-height)] [material (default-icon-material)])
  (make-cached-flomap
   [height color material]
   (define scale (/ height 32))
   (let* ([fm   (flat-check-flomap color height)]
          [dfm  (flomap->deep-flomap fm)]
          [dfm  (deep-flomap-icon-style dfm)]
          [dfm  (deep-flomap-raise dfm (* -12 scale))])
     (deep-flomap-render-icon dfm material))))

(define (regular-polygon-flomap sides start color
                                [height (default-icon-height)]
                                [material (default-icon-material)])
  (make-cached-flomap
   [height sides start color material]
   (flomap-render-icon (flat-regular-polygon-flomap sides start color height) material)))

(define (octagon-flomap color [height (default-icon-height)] [material (default-icon-material)])
  (regular-polygon-flomap 8 (/ (* 2 pi) 16) color height material))

(define (stop-sign-flomap color [height (default-icon-height)] [material (default-icon-material)])
  (make-cached-flomap
   [height color material]
   (define scale (/ height 32))
   (let* ([indent-fm  (fm* 0.5 (x-flomap "black" (* 22 scale)))]
          [indent-dfm  (deep-flomap-raise (flomap->deep-flomap indent-fm) (* -2 scale))]
          [fm   (regular-polygon-flomap 8 (/ (* 2 pi) 16) color height)]
          [dfm  (flomap->deep-flomap fm)]
          [dfm  (deep-flomap-cc-superimpose 'add dfm indent-dfm)]
          [dfm  (deep-flomap-icon-style dfm)]
          [fm  (deep-flomap-render-icon dfm material)])
     (flomap-cc-superimpose fm (x-flomap "azure" (* 22 scale) metal-material)))))

;; ---------------------------------------------------------------------------------------------------
;; Magnifying glass

(define magnifying-glass-material
  (deep-flomap-material-value
   'glass 1.0 0.75 1.0
   0.25 0.15 1.0
   0.25 0.25 0.0
   0.0))

(define magnifying-glass-metal-material
  (deep-flomap-material-value
   3.0 0.5 0.0 1.0
   0.8 0.1 0.2
   0.2 0.8 0.0
   0.0))

(define (magnifying-glass-flomap metal-color handle-color
                                 [height (default-icon-height)]
                                 [material (default-icon-material)])
  (make-cached-flomap
   [height metal-color handle-color material]
   (define scale (/ height 32))
   (define glass-fm
     (let* ([fm  (draw-icon-flomap
                  18 18 (λ (dc)
                          (send dc set-pen handle-color 1 'solid)
                          (send dc set-brush "azure" 'solid)
                          (draw-ellipse/smoothed dc 0 0 18 18)
                          (send dc set-alpha 0.75)
                          (send dc set-pen "black" 1 'solid)
                          (send dc set-brush "white" 'transparent)
                          (draw-ellipse/smoothed dc 0 0 18 18))
                  scale)]
            [dfm  (flomap->deep-flomap fm)]
            [dfm  (deep-flomap-bulge-spheroid dfm (* 4 scale))]
            [dfm  (deep-flomap-raise dfm (* 4 scale))])
       (deep-flomap-render-icon dfm magnifying-glass-material)))
   
   (define circle-fm
     (let* ([fm  (draw-icon-flomap
                  28 28 (λ (dc)
                          (send dc set-pen "black" 3 'solid)
                          (send dc set-brush "black" 'solid)
                          (draw-ellipse/smoothed dc 1 1 26 26)
                          (send dc set-pen metal-color 1 'solid)
                          (send dc set-brush metal-color 'solid)
                          (draw-ellipse/smoothed dc 1 1 26 26))
                  scale)]
            [indent-fm  (draw-icon-flomap
                         28 28 (λ (dc)
                                 (send dc set-pen metal-color 1 'solid)
                                 (send dc set-brush metal-color 'solid)
                                 (draw-ellipse/smoothed dc 5 5 18 18))
                         scale)]
            [indent-dfm  (flomap->deep-flomap indent-fm)]
            [indent-dfm  (deep-flomap-raise indent-dfm (* -3 scale))]
            ;[indent-dfm  (deep-flomap-smooth-z indent-dfm (* 2 scale))]
            [dfm  (flomap->deep-flomap fm)]
            ;[dfm  (deep-flomap-icon-style dfm)]
            [dfm  (deep-flomap-raise dfm (* 4 scale))]
            [dfm  (deep-flomap-cc-superimpose 'add dfm indent-dfm)]
            [dfm  (deep-flomap-smooth-z dfm (* 1 scale))]
            )
       (deep-flomap-render-icon dfm magnifying-glass-metal-material)))
   
   (define handle-fm
     (let* ([fm  (draw-icon-flomap
                  11 11 (λ (dc)
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

;; ---------------------------------------------------------------------------------------------------
;; Bomb

(define (left-bomb-flomap cap-color bomb-color
                          [height (default-icon-height)]
                          [material (default-icon-material)])
  (make-cached-flomap
   [height cap-color bomb-color material]
   (define scale (/ height 32))
   (define fuse-fm
     (let* ([fm  (draw-icon-flomap
                  16 16 (λ (dc)
                          (send dc set-pen "black" 1/2 'solid)
                          (send dc set-brush "gold" 'solid)
                          (draw-path-commands
                           dc 0 0
                           '((m 0.5 5.5)
                             (c -1.5 -2 -0.5 -5 2 -5.5
                                3 0.5 5 2.5 6 5
                                0.5 2.5 -1.5 4.5 -4 4
                                -1 -2 -1.5 -3.5 -4 -3.5))))
                  scale)]
            [dfm  (flomap->deep-flomap fm)]
            [dfm  (deep-flomap-icon-style dfm)]
            [dfm  (deep-flomap-scale-z dfm 1)])
       (deep-flomap-render-icon dfm matte-material)))
   
   (define (bomb-cap-flomap color)
     (draw-icon-flomap
      20 20 (λ (dc)
              (send dc set-pen "black" 1 'solid)
              (send dc set-brush color 'solid)
              (draw-path-commands dc 0 0 '((m 1.5 11.5)
                                           (l 10 -10 2.5 2.5)
                                           (c 4 5 -5 14 -10 10)
                                           (l -2.5 -2.5)))
              (draw-path-commands dc 0 0 '((m 1.5 11.5)
                                           (c -2 -5 5 -12 10 -10
                                              4 5 -5 14 -10 10))))
      scale))
   
   (define cap-fm
     (let* ([cap-fm  (bomb-cap-flomap cap-color)]
            [cap-dfm  (flomap->deep-flomap cap-fm)]
            [cap-dfm  (deep-flomap-icon-style cap-dfm)])
       (deep-flomap-render-icon cap-dfm material)))
   
   (define sphere-fm
     (let* ([sphere-fm  (draw-icon-flomap
                         30 30 (λ (dc)
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

(define (stop-signs-flomap color [height (default-icon-height)] [material (default-icon-material)])
  (define fm (stop-sign-flomap color (* height 2/3) material))
  (flomap-pin* 3/16 1/4 0 0
               fm (flomap-pin* 3/16 1/4 0 0 fm fm)))

(define left-magnifying-glass-flomap (compose flomap-flip-horizontal magnifying-glass-flomap))
(define bomb-flomap (compose flomap-flip-horizontal left-bomb-flomap))

;; ===================================================================================================
;; Bitmaps (icons)

(define text-icon (compose flomap->bitmap text-flomap))
(define regular-polygon-icon (compose flomap->bitmap regular-polygon-flomap))
(define octagon-icon (compose flomap->bitmap octagon-flomap))
(define x-icon (compose flomap->bitmap x-flomap))
(define stop-sign-icon (compose flomap->bitmap stop-sign-flomap))
(define stop-signs-icon (compose flomap->bitmap stop-signs-flomap))
(define check-icon (compose flomap->bitmap check-flomap))
(define magnifying-glass-icon (compose flomap->bitmap magnifying-glass-flomap))
(define left-magnifying-glass-icon (compose flomap->bitmap left-magnifying-glass-flomap))
(define bomb-icon (compose flomap->bitmap bomb-flomap))
(define left-bomb-icon (compose flomap->bitmap left-bomb-flomap))
