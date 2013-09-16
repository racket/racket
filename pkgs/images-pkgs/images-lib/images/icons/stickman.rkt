#lang racket/base

(require racket/class racket/draw racket/vector racket/match racket/math
         racket/contract unstable/latent-contract unstable/latent-contract/defthing
         "../private/flomap.rkt"
         "../private/deep-flomap.rkt"
         "../private/utils.rkt"
         "style.rkt")

(provide (activate-contract-out
          standing-stickman-icon standing-stickman-flomap
          running-stickman-icon running-stickman-flomap)
         (only-doc-out (all-defined-out)))

;; ===================================================================================================
;; Common

(define (cons+ p1 p2)
  (match-define (cons x1 y1) p1)
  (match-define (cons x2 y2) p2)
  (cons (+ x1 x2) (+ y1 y2)))

(define (polar->cartesian angle mag)
  (cons (* mag (cos (* (/ pi 180) angle)))
        (* mag (sin (* (/ pi 180) angle)))))

(define line-width 1)
(define body-width 4.5)
(define leg-width 4)
(define arm-width 3.5)

(define neck-length 6)
(define torso-length 6)
(define upper-arm-length 5)
(define lower-arm-length 5.5)
(define thigh-length 6.5)
(define shin-length 6.5)
(define shoulder-breadth 7)

;; ===================================================================================================
;; Standing

(define standing-torso-angle -90)
(define standing-neck-angle 5)
(define standing-left-knee-angle 200)
(define standing-left-foot-angle 0)
(define standing-right-knee-angle 150)
(define standing-right-foot-angle 20)
(define standing-left-elbow-angle 230)
(define standing-left-hand-angle -90)
(define standing-right-elbow-angle 140)
(define standing-right-hand-angle 90)

(define standing-hip-point (cons 14 (- 29 (+ thigh-length shin-length))))

(define standing-neck-point
  (cons+ standing-hip-point
         (polar->cartesian standing-torso-angle torso-length)))

(define standing-head-point
  (cons+ standing-neck-point
         (polar->cartesian (+ standing-neck-angle standing-torso-angle)
                           neck-length)))

(define standing-left-knee-point
  (cons+ standing-hip-point
         (polar->cartesian (+ standing-left-knee-angle standing-torso-angle)
                           thigh-length)))

(define standing-left-foot-point
  (cons+ standing-left-knee-point
         (polar->cartesian (+ standing-left-knee-angle standing-torso-angle standing-left-foot-angle)
                           shin-length)))

(define standing-left-shoulder-point
  (cons+ standing-neck-point (cons (* -1/2 shoulder-breadth) 0)))

(define standing-left-elbow-point
  (cons+ standing-left-shoulder-point
         (polar->cartesian (+ standing-left-elbow-angle standing-torso-angle)
                           upper-arm-length)))

(define standing-left-hand-point
  (cons+ standing-left-elbow-point
         (polar->cartesian (+ standing-left-elbow-angle standing-torso-angle standing-left-hand-angle)
                           lower-arm-length)))

(define standing-right-knee-point
  (cons+ standing-hip-point
         (polar->cartesian (+ standing-right-knee-angle standing-torso-angle)
                           thigh-length)))

(define standing-right-foot-point
  (cons+ standing-right-knee-point
         (polar->cartesian (+ standing-right-knee-angle standing-torso-angle standing-right-foot-angle)
                           shin-length)))

(define standing-right-shoulder-point
  (cons+ standing-neck-point (cons (* 1/2 shoulder-breadth) 0)))
  

(define standing-right-elbow-point
  (cons+ standing-right-shoulder-point
         (polar->cartesian (+ standing-right-elbow-angle standing-torso-angle)
                           upper-arm-length)))

(define standing-right-hand-point
  (cons+ standing-right-elbow-point
         (polar->cartesian
          (+ standing-right-elbow-angle standing-torso-angle standing-right-hand-angle)
          lower-arm-length)))

(defproc (standing-stickman-flomap
          [#:body-color body-color (or/c string? (is-a?/c color%)) run-icon-color]
          [#:arm-color arm-color (or/c string? (is-a?/c color%)) "white"]
          [#:head-color head-color (or/c string? (is-a?/c color%)) run-icon-color]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (make-cached-flomap
   [height body-color arm-color head-color material]
   (flomap-lt-superimpose
    (draw-short-rendered-icon-flomap 
     (λ (dc)
       (send dc set-pen (icon-color->outline-color arm-color)
             (+ arm-width (* 2 line-width)) 'solid)
       (send dc draw-lines (list standing-right-shoulder-point
                                 standing-right-elbow-point
                                 standing-right-hand-point))
       (send dc set-pen arm-color arm-width 'solid)
       (send dc draw-lines (list standing-right-shoulder-point
                                 standing-right-elbow-point
                                 standing-right-hand-point)))
     26 32 (/ height 32) material)
    (draw-short-rendered-icon-flomap 
     (λ (dc)
       (send dc set-pen (icon-color->outline-color body-color)
             (+ body-width (* 2 line-width)) 'solid)
       (send dc draw-lines (list standing-neck-point standing-hip-point))
       
       (send dc set-pen (icon-color->outline-color body-color)
             (+ leg-width (* 2 line-width)) 'solid)
       (send dc draw-lines (list standing-hip-point
                                 standing-left-knee-point
                                 standing-left-foot-point))
       (send dc draw-lines (list standing-hip-point
                                 standing-right-knee-point
                                 standing-right-foot-point))
       
       (send dc set-pen body-color body-width 'solid)
       (send dc draw-lines (list standing-neck-point standing-hip-point))
       
       (send dc set-pen body-color leg-width 'solid)
       (send dc draw-lines (list standing-hip-point
                                 standing-left-knee-point
                                 standing-left-foot-point))
       (send dc draw-lines (list standing-hip-point
                                 standing-right-knee-point
                                 standing-right-foot-point)))
     26 32 (/ height 32) material)
    (draw-short-rendered-icon-flomap 
     (λ (dc)
       (send dc set-pen (icon-color->outline-color arm-color)
             (+ arm-width (* 2 line-width)) 'solid)
       (send dc draw-lines (list standing-left-shoulder-point
                                 standing-left-elbow-point
                                 standing-left-hand-point))
       (send dc set-pen arm-color arm-width 'solid)
       (send dc draw-lines (list standing-left-shoulder-point
                                 standing-left-elbow-point
                                 standing-left-hand-point)))
     26 32 (/ height 32) material)
    (draw-short-rendered-icon-flomap 
     (λ (dc)
       (send dc set-pen (icon-color->outline-color head-color) line-width 'solid)
       (send dc set-brush head-color 'solid)
       (match-define (cons x y) standing-head-point)
       (send dc draw-ellipse (- x 3.5) (- y 3.5) 7 7))
     26 32 (/ height 32) material))))

;; ===================================================================================================
;; Running

(define running-neck-angle 20)
(define running-torso-angle -70)

(define (squash* x scale)
  (/ 1 (+ 1 (exp (- (* scale (- x 1/2)))))))

(define (squash x scale)
  (define y-min (squash* 0 scale))
  (define y-max (squash* 1 scale))
  (let* ([x  (max 0.0 (min 1.0 x))]
         [y  (squash* x scale)]
         [y  (/ (- y y-min) (- y-max y-min))])
    (max 0.0 (min 1.0 y))))

(define (cycle t)
  (* 0.5 (+ 1.0 (cos (* 2.0 pi (+ 0.5 t))))))

(define (scale-angle θ θ-min θ-max)
  (+ θ-min (* θ (- θ-max θ-min))))

(define (running-foot-angle t)
  (scale-angle (squash (cycle (+ -0.3333 t)) 3) 0 125))

(define (running-knee-angle t)
  (scale-angle (squash (cycle t) 2) 90 210))

(define (running-hip-height t)
  (+ (- 27 (+ thigh-length shin-length))
     (* 2 (squash (cycle (+ 0.125 (* 2 t))) 5))))

(define (running-elbow-angle t)
  (scale-angle (squash (cycle (+ 0.5 t)) 4) 150 240))

(define (running-hand-angle t)
  (scale-angle (squash (cycle (+ 0.6666 t)) 6) -115 -30))

(define (running-hip-point t) (cons 14 (running-hip-height t)))

(define (running-shoulder-point t)
  (cons+ (running-hip-point t)
         (polar->cartesian running-torso-angle torso-length)))

(define (running-head-point t)
  (cons+ (running-shoulder-point t)
         (polar->cartesian (+ running-neck-angle running-torso-angle)
                           neck-length)))

(define (running-knee-point t)
  (cons+ (running-hip-point t)
         (polar->cartesian (+ (running-knee-angle t) running-torso-angle)
                           thigh-length)))

(define (running-foot-point t)
  (cons+ (running-knee-point t)
         (polar->cartesian (+ (running-knee-angle t) running-torso-angle (running-foot-angle t))
                           shin-length)))

(define (running-elbow-point t)
  (cons+ (running-shoulder-point t)
         (polar->cartesian (+ (running-elbow-angle t) running-torso-angle)
                           upper-arm-length)))

(define (running-hand-point t)
  (cons+ (running-elbow-point t)
         (polar->cartesian (+ (running-elbow-angle t) running-torso-angle (running-hand-angle t))
                           lower-arm-length)))

(define (draw-running-body dc t color width)
  (send dc set-pen color width 'solid)
  (send dc draw-lines (list (running-hip-point t) (running-shoulder-point t))))

(define (draw-running-leg dc t color width)
  (send dc set-pen color width 'solid)
  (send dc draw-lines
        (list (running-hip-point t) (running-knee-point t) (running-foot-point t))))

(define (draw-running-arm dc t color width)
  (send dc set-pen color width 'solid)
  (send dc draw-lines
        (list (running-shoulder-point t) (running-elbow-point t) (running-hand-point t))))

(define (running-head-flomap t color height material)
  (make-cached-flomap
   [height t color material]
   (draw-rendered-icon-flomap 
    (λ (dc)
      (send dc set-pen (icon-color->outline-color color) line-width 'solid)
      (send dc set-brush color 'solid)
      (match-define (cons x y) (running-head-point t))
      (send dc draw-ellipse (- x 3.5) (- y 3.5) 7 7))
    26 32 (/ height 32) material)))

(define (running-leg-flomap t body? color height material)
  (make-cached-flomap
   [height t body? color material]
   (draw-rendered-icon-flomap 
    (λ (dc)
      (draw-running-leg dc t (icon-color->outline-color color) (+ leg-width (* 2 line-width)))
      (when body?
        (draw-running-body dc t (icon-color->outline-color color)
                           (+ body-width (* 2 line-width)))
        (draw-running-body dc t color body-width))
      (draw-running-leg dc t color leg-width))
    26 32 (/ height 32) material)))

(define (running-arm-flomap t color height material)
  (make-cached-flomap
   [height t color material]
   (draw-rendered-icon-flomap 
    (λ (dc)
      (draw-running-arm dc t (icon-color->outline-color color) (+ arm-width (* 2 line-width)))
      (draw-running-arm dc t color arm-width))
    26 32 (/ height 32) material)))

(defproc (running-stickman-flomap
          [t rational?]
          [#:body-color body-color (or/c string? (is-a?/c color%))]
          [#:arm-color arm-color (or/c string? (is-a?/c color%))]
          [#:head-color head-color (or/c string? (is-a?/c color%))]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (make-cached-flomap
   [height t body-color arm-color head-color material]
   (flomap-lt-superimpose (running-arm-flomap (+ t 0.5) arm-color height material)
                          (running-leg-flomap (+ t 0.5) #f body-color height material)
                          (running-leg-flomap t #t body-color height material)
                          (running-head-flomap t head-color height material)
                          (running-arm-flomap t arm-color height material))))

;; ---------------------------------------------------------------------------------------------------

(define-icon-wrappers
  ([#:body-color body-color (or/c string? (is-a?/c color%)) run-icon-color]
   [#:arm-color arm-color (or/c string? (is-a?/c color%)) "white"]
   [#:head-color head-color (or/c string? (is-a?/c color%)) run-icon-color]
   [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
   [#:material material deep-flomap-material-value? (default-icon-material)])
  [standing-stickman-icon standing-stickman-flomap])

(define-icon-wrappers
  ([t rational?]
   [#:body-color body-color (or/c string? (is-a?/c color%)) run-icon-color]
   [#:arm-color arm-color (or/c string? (is-a?/c color%)) "white"]
   [#:head-color head-color (or/c string? (is-a?/c color%)) run-icon-color]
   [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
   [#:material material deep-flomap-material-value? (default-icon-material)])
  [running-stickman-icon running-stickman-flomap])

#;; FOR TESTING ONLY: Do not let this find its way into the repo uncommented!
(begin
  (require racket/gui (planet "animated-canvas.rkt" ("williams" "animated-canvas.plt" 2 4)))
  
  (define size 64)
  
  (standing-stickman-icon halt-icon-color "white" halt-icon-color size)
  
  (define framerate 12)
 
  (default-icon-material glass-icon-material)
  (define frame-bitmaps
    (for/vector ([t  (in-range 0 1 (/ 1 framerate))])
      (time (running-stickman-icon t run-icon-color "white" run-icon-color size))))
  (printf "~v~n" frame-bitmaps)
  
  (define frame (make-object frame% "Canvas"))
  (define canvas (new animated-canvas%
                      [parent frame] [style '(border)]
                      [min-width (+ size 4)] [min-height (+ size 4)]))
  (send frame show #t)
  
  (define i 0)
  (define timer (make-object timer% (λ ()
                                      (define bm (vector-ref frame-bitmaps i))
                                      (set! i (modulo (+ i 1) framerate))
                                      (define dc (send canvas get-dc))
                                      (send dc draw-bitmap bm 0 0)
                                      (send canvas swap-bitmaps)
                                      (yield))
                  (round (* 1000 (/ 1 framerate)))))
  )
