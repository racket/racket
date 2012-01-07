#lang racket/base

(require racket/draw racket/class racket/match racket/math racket/flonum
         "private/flomap.rkt"
         "private/deep-flomap.rkt"
         "private/renderfx.rkt"
         "icons/style.rkt"
         "private/unsafe.rkt")

(provide plt-logo)

(define glass-logo-material
  (deep-flomap-material-value
   'cubic-zirconia 0.7 0.6 0.4
   0.2 0.1 1.0
   0.2 0.1 0.1
   0.0))

(define lambda-start-point (cons 235.0 38.0))
(define lambda-control-points
  (list (list (cons -27.07492 0.489079) (cons -52.83237 9.901645) (cons -78.13681 18.608898))
        (list (cons 11.0396 11.823329) (cons 9.37418 15.558039) (cons 14.19246 14.659919))
        (list (cons 18.43869 -4.46584) (cons 45.7868 -14.85883) (cons 57.97111 4.83448))
        (list (cons 26.56443 33.55767) (cons 37.83026 76.50393) (cons 41.85449 118.37596))
        (list (cons 5.15871 25.44003) (cons -47.30403 116.52589) (cons -63.42303 152.88265))
        (list (cons -26.20045 46.22879) (cons -49.47611 94.20521) (cons -78.99673 138.48542))
        (list (cons 7.0596 9.34303) (cons 17.25993 5.68676) (cons 26.86192 4.2502))
        (list (cons 8.19842 -1.22826) (cons 16.39686 -2.4565) (cons 24.59528 -3.68475))
        (list (cons 26.44013 -62.68827) (cons 54.98797 -120.2314) (cons 79.79859 -183.59412))
        (list (cons 11.30581 -26.11293) (cons 16.82865 -40.47628) (cons 30.26123 -57.57618))
        (list (cons 15.92423 9.74246) (cons 20.66525 33.77224) (cons 29.3527 50.35199))
        (list (cons 25.60238 65.87977) (cons 51.09413 131.80228) (cons 75.25809 198.22074))
        (list (cons 6.32468 2.20244) (cons 12.81613 8.78314) (cons 18.81535 2.44056))
        (list (cons 15.78086 -9.73038) (cons 34.15342 -15.82488) (cons 47.2925 -29.27438))
        (list (cons -3.74907 -18.17899) (cons -15.79452 -35.18254) (cons -23.13261 -52.66524))
        (list (cons -46.51473 -92.95952) (cons -91.3634 -191.5622) (cons -120.47873 -291.65949))
        (list (cons -10.72309 -31.50493) (cons -23.92724 -69.469699) (cons -58.05359 -81.906439))
        (list (cons -7.7741 -2.308013) (cons -15.96612 -2.751575) (cons -24.03222 -2.750218))))

(define (lambda-path x y x-scale y-scale)
  (define (scale-x x) (* x x-scale))
  (define (scale-y y) (* y y-scale))
  (define p (new dc-path%))
  (match-define (cons (app scale-x sx) (app scale-y sy)) lambda-start-point)
  (send p move-to sx sy)
  (for/fold ([lx sx] [ly sy]) ([pt  (in-list lambda-control-points)])
    (match-define (list (cons (app scale-x x1) (app scale-y y1))
                        (cons (app scale-x x2) (app scale-y y2))
                        (cons (app scale-x x3) (app scale-y y3))) pt)
    (send p curve-to (+ lx x1) (+ ly y1) (+ lx x2) (+ ly y2) (+ lx x3) (+ ly y3))
    (values (+ lx x3) (+ ly y3)))
  (send p close)
  p)

(define (draw-lambda dc x y w h)
  (send dc draw-path (lambda-path x y (/ w 565) (/ h 565))))

(define blue-θ-start (* -45 (/ pi 180)))
(define blue-θ-end (* 110 (/ pi 180)))

(define logo-red-color (make-object color% 255 36 32))
(define logo-blue-color (make-object color% 32 36 255))
(define lambda-outline-color (make-object color% 16 16 64))
(define (lambda-pen color width) (make-object pen% color width 'solid 'projecting 'miter))

(define (make-arc-path x y w h start end [ccw? #t])
  (define p (new dc-path%))
  (send p arc x y w h start end ccw?)
  (send p close)
  p)

(define (flomap-add-sparkles! fm)
  (match-define (flomap vs c w h) fm)
  (for ([_  (in-range 2000)])
    (define x (random w))
    (define y (random h))
    (define i (unsafe-fx* c (unsafe-fx+ x (unsafe-fx* w y))))
    (define a (flvector-ref vs i))
    (when (a . > . 0)
      (define l (unsafe-fl+ 0.5 (unsafe-fl* 1.5 (random))))
      (define-values (r g b) (unsafe-flvector-3ref vs (unsafe-fx+ 1 i)))
      (unsafe-flvector-3set! vs (unsafe-fx+ 1 i)
                             (unsafe-fl* r l)
                             (unsafe-fl* g l)
                             (unsafe-fl* b l)))))

(define (flomap-rough fm z-amt)
  (match-define (flomap vs c w h) fm)
  (unsafe-build-flomap
   c w h
   (λ (k x y)
     (define i (unsafe-fx+ k (unsafe-fx* c (unsafe-fx+ x (unsafe-fx* w y)))))
     (unsafe-fl+ (unsafe-fl* z-amt (exact->inexact (random)))
                 (unsafe-flvector-ref vs i)))))

(define (plt-logo height)
  (define scale (/ height 256))
  (define bulge-fm
    (draw-flomap
     height height
     (λ (dc)
       (send dc set-scale scale scale)
       (send dc set-pen logo-red-color 2 'solid)
       (send dc set-brush logo-red-color 'solid)
       (send dc draw-path (make-arc-path 7 7 242 242 blue-θ-end blue-θ-start))
       (send dc set-pen logo-blue-color 2 'solid)
       (send dc set-brush logo-blue-color 'solid)
       (send dc draw-path (make-arc-path 7 7 242 242 blue-θ-start blue-θ-end))
       (send dc set-pen (lambda-pen lambda-outline-color 12))
       (send dc set-brush lambda-outline-color 'solid)
       (draw-lambda dc 0 0 256 256))))
  
  ;(flomap-add-sparkles! bulge-fm)
  
  (define (lambda-flomap color pen-width)
    (draw-flomap
     height height
     (λ (dc)
       (send dc set-scale scale scale)
       (send dc set-pen (lambda-pen color pen-width))
       (send dc set-brush color 'solid)
       (draw-lambda dc 0 0 256 256))))
  
  (let* ([bulge-dfm  (flomap->deep-flomap bulge-fm)]
         [bulge-dfm  (deep-flomap-bulge-spheroid bulge-dfm (* 116 scale))]
         ;[bulge-dfm  (deep-flomap-raise bulge-dfm (* 8 scale))]
         ;[bulge-dfm  (deep-flomap-smooth-z bulge-dfm (* 1/2 scale))]
         #;[bulge-dfm  (deep-flomap (deep-flomap-argb bulge-dfm)
                                  (flomap-rough (deep-flomap-z bulge-dfm) 0.5))]
         [lambda-dfm  (flomap->deep-flomap (lambda-flomap "azure" 4))]
         [lambda-dfm  (deep-flomap-bulge-spheroid lambda-dfm (* 116 scale))]
         [lambda-dfm  (deep-flomap-smooth-z lambda-dfm (* 3 scale))]
         [lambda-fm  (deep-flomap-render-icon lambda-dfm metal-material)]
         [fm  (deep-flomap-render-icon bulge-dfm glass-logo-material)]
         [fm  (flomap-cc-superimpose
               fm
               (lambda-flomap lambda-outline-color 12)
               lambda-fm)]
         [fm  (flomap-inset fm 16)]
         [fm  (flomap-cc-superimpose
               fm
               (draw-flomap
                (inexact->exact (ceiling (* 1.015625 height)))
                (inexact->exact (ceiling (* 1.015625 height)))
                (λ (dc)
                  (send dc set-scale scale scale)
                  (send dc set-origin (* 2.5 scale) (* 2.5 scale))
                  (send dc set-pen lambda-outline-color 4 'solid)
                  (send dc set-brush lambda-outline-color 'transparent)
                  (send dc draw-ellipse 0 0 256 256))))]
         [fm  (flomap-cc-superimpose (fm* 0.5 (flomap-shadow fm (* 4 scale))) fm)]
         )
    (flomap->bitmap fm)))
