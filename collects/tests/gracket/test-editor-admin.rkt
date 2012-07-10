#lang scheme/base
(require scheme/class
         scheme/gui/base)

(provide test-editor-admin%)

(define the-dc
  (new (class* bitmap-dc% ()
         (super-new)
         (define/override (get-text-extent s [font #f] [combine? #f] [offset 0])
           (values (* 10.0 (string-length s)) 10.0 1.0 1.0))
         (define/override (set-pen . p) (void))
         (define/override (get-pen . p) #f)
         (define/override (set-brush . b) (void))
         (define/override (get-brush . b) #f)
         (define/override (set-clipping-rect . b) (void))
         (define/override (get-clipping-region . b) #f)
         (define/override (draw-text s x y [combine? #f] [offset 0] [angle 0.0]) (void))
         (define/override (cache-font-metrics-key) 100))))
         

(define test-editor-admin%
  (class editor-admin% 
    (super-new)

    (define/override (get-dc [x #f] [y #f])
      (when x (set-box! x 1.0))
      (when y (set-box! y 1.0))
      the-dc)

    (define/private (do-get-view x y w h)
      (when x (set-box! x 0.0))
      (when y (set-box! y 0.0))
      (when w (set-box! w 100.0))
      (when h (set-box! h 100.0)))

    (define/override (get-view x y w h [full? #f])
      (do-get-view x y w h))

    (define/override (get-max-view x y w h [full? #f])
      (do-get-view x y w h))

    (define/override (scroll-to x y w h refresh? bias)
      (void))))
