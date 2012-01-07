#lang racket/base

(require racket/draw racket/class racket/math racket/sequence
         "../private/flomap.rkt"
         "../private/deep-flomap.rkt"
         "../private/renderfx.rkt"
         "../private/utils.rkt"
         "style.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Unrendered flomaps

(define (x-flomap color height)
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

(define (check-flomap color height)
  (draw-icon-flomap
   32 32 (λ (dc)
           (send dc set-brush color 'solid)
           (draw-path-commands
            dc 0 19 '((c (0 . 0) (7 . 4) (14 . 12) (5.5 . -13.5) (17 . -23) (17 . -23))
                      (l (-9 . -8))
                      (c (0 . 0) (-6.5 . 7.5) (-9.5 . 16) (-2.5 . -4) (-6 . -6.5) (-6 . -6.5))
                      (l (-6 . 9)))))
   (/ height 32)))

(define (regular-polygon-flomap sides start color size)
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

(define (text-icon-flomap* str font color trim? outline? height material)
  (define family (send font get-family))
  (define style (send font get-style))
  (define weight (send font get-weight))
  (define underline? (send font get-underlined))
  (define smoothing (send font get-smoothing))
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
  (flomap-render-icon fm material)))

(define (x-icon-flomap* color height material)
  (define scale (/ height 32))
  (let* ([fm   (x-flomap color height)]
         [dfm  (flomap->deep-flomap fm)]
         [dfm  (deep-flomap-icon-style dfm)]
         [dfm  (deep-flomap-raise dfm (* -8 scale))])
    (deep-flomap-render-icon dfm material)))

(define (check-icon-flomap* color height material)
  (define scale (/ height 32))
  (let* ([fm   (check-flomap color height)]
         [dfm  (flomap->deep-flomap fm)]
         [dfm  (deep-flomap-icon-style dfm)]
         [dfm  (deep-flomap-raise dfm (* -12 scale))])
    (deep-flomap-render-icon dfm material)))

(define (regular-polygon-icon-flomap* sides start color height material)
  (flomap-render-icon (regular-polygon-flomap sides start color height) material))

(define (octagon-icon-flomap* color height material)
  (regular-polygon-icon-flomap* 8 (/ (* 2 pi) 16) color height material))

(define (stop-sign-icon-flomap* color height material)
  (define scale (/ height 32))
  (let* ([indent-fm  (fm* 0.5 (x-flomap "black" (* 22 scale)))]
         [indent-dfm  (deep-flomap-raise (flomap->deep-flomap indent-fm) (* -2 scale))]
         [fm   (regular-polygon-flomap 8 (/ (* 2 pi) 16) color height)]
         [dfm  (flomap->deep-flomap fm)]
         [dfm  (deep-flomap-cc-superimpose dfm indent-dfm #:z-mode 'add)]
         [dfm  (deep-flomap-icon-style dfm)]
         [fm  (deep-flomap-render-icon dfm material)])
    (flomap-cc-superimpose
     fm
     (x-icon-flomap* "azure" (* 22 scale) metal-material))))

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
(define (magnifying-glass-icon-flomap* metal-color handle-color height material)
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
           [dfm  (deep-flomap-cc-superimpose dfm indent-dfm #:z-mode 'add)]
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
               (flomap-pin* 1/2 1/2 1/2 1/2 circle-fm glass-fm)))

(define (left-bomb-icon-flomap* cap-color bomb-color height material)
  (define scale (/ height 32))
  (define fuse-fm
    (let* ([fm  (draw-icon-flomap
                 16 16 (λ (dc)
                         (send dc set-pen "black" 5 'solid)
                         (draw-path-commands dc 5.5 5.5 '((c (0 . -1) (-2.5 . -4) (-3 . -2.5))))
                         (send dc set-pen "orange" 4 'solid)
                         (draw-path-commands dc 5.5 5.5 '((c (0 . -1) (-2.5 . -4) (-3 . -2.5)))))
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
             (draw-path-commands
              dc 1 11 '((l (10 . -10) (3 . 3))
                        (c (4 . 5) (-5 . 14) (-10 . 10))
                        (l (-3 . -3))))
             (draw-path-commands
              dc 1 11 '((c (-2 . -5) (5 . -12) (10 . -10)
                           (4 . 5) (-5 . 14) (-10 . 10)))))
     scale))
  
  (define cap-fm
    (let* ([cap-fm  (bomb-cap-flomap cap-color)]
           [cap-dfm  (flomap->deep-flomap cap-fm)]
           [cap-dfm  (deep-flomap-icon-style cap-dfm)])
      (deep-flomap-render-icon cap-dfm material)))
  
  (define sphere-fm
    (let* ([sphere-fm  (draw-icon-flomap
                 32 32 (λ (dc)
                         (send dc set-brush bomb-color 'solid)
                         (draw-ellipse/smoothed dc 0 0 32 32))
                 scale)]
           [cap-fm  (bomb-cap-flomap cap-color)]
           [cap-dfm  (flomap->deep-flomap cap-fm)]
           [cap-dfm  (deep-flomap-raise cap-dfm (* -2 scale))]
           [cap-dfm  (deep-flomap-smooth-z cap-dfm (* 1 scale))]
           [sphere-dfm  (flomap->deep-flomap sphere-fm)]
           [sphere-dfm  (deep-flomap-bulge-spheroid sphere-dfm (* 16 scale))]
           [sphere-dfm  (deep-flomap-lt-superimpose sphere-dfm cap-dfm #:z-mode 'add)]
           )
      (deep-flomap-render-icon sphere-dfm material)))
  (flomap-lt-superimpose sphere-fm cap-fm fuse-fm))

(define-icon-flomap-proc text-icon-flomap text-icon-flomap* 32 str font color trim? outline?)
(define-icon-flomap-proc regular-polygon-icon-flomap regular-polygon-icon-flomap* 32 color)
(define-icon-flomap-proc octagon-icon-flomap octagon-icon-flomap* 32 color)
(define-icon-flomap-proc x-icon-flomap x-icon-flomap* 24 color)
(define-icon-flomap-proc stop-sign-icon-flomap stop-sign-icon-flomap* 32 color)
(define-icon-flomap-proc check-icon-flomap check-icon-flomap* 32 color)
(define-icon-flomap-proc magnifying-glass-icon-flomap
  magnifying-glass-icon-flomap* 32 color metal-color)
(define-icon-flomap-proc left-bomb-icon-flomap left-bomb-icon-flomap* 32 cap-color bomb-color)

(define (stop-signs-icon-flomap color [height  (default-icon-height)]
                                 [icon-material  (default-icon-material)])
  (define fm (stop-sign-icon-flomap color (* height 2/3) icon-material))
  (flomap-pin* 3/16 1/4 0 0
               fm (flomap-pin* 3/16 1/4 0 0 fm fm)))

(define left-magnifying-glass-icon-flomap
  (compose flomap-flip-horizontal magnifying-glass-icon-flomap))

(define bomb-icon-flomap (compose flomap-flip-horizontal left-bomb-icon-flomap))

;; ===================================================================================================
;; Bitmaps (icons)

(define text-icon (compose flomap->bitmap text-icon-flomap))
(define regular-polygon-icon (compose flomap->bitmap regular-polygon-icon-flomap))
(define octagon-icon (compose flomap->bitmap octagon-icon-flomap))
(define x-icon (compose flomap->bitmap x-icon-flomap))
(define stop-sign-icon (compose flomap->bitmap stop-sign-icon-flomap))
(define stop-signs-icon (compose flomap->bitmap stop-signs-icon-flomap))
(define check-icon (compose flomap->bitmap check-icon-flomap))
(define magnifying-glass-icon (compose flomap->bitmap magnifying-glass-icon-flomap))
(define left-magnifying-glass-icon (compose flomap->bitmap left-magnifying-glass-icon-flomap))
(define bomb-icon (compose flomap->bitmap bomb-icon-flomap))
(define left-bomb-icon (compose flomap->bitmap left-bomb-icon-flomap))
