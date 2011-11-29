#lang racket

(require icons slideshow/pict)

(define icon-heights '(8 12 16 24 32 48 64))

(define (tape-deck color height style)
  (list
   (rewind-icon-pict color height style)
   (continue-back-icon-pict color height style)
   (step-back-icon-pict color height style)
   (pause-icon-pict color height style)
   (stop-icon-pict color height style)
   (go-icon-pict color height style)
   (step-icon-pict color height style)
   (continue-icon-pict color height style)
   (fast-forward-icon-pict color height style)))

(for*/list ([height  icon-heights]
            [color   icon-colors]
            [style   icon-styles])
  (tape-deck color height style))

(for/list ([height  icon-heights])
  (for*/list ([color   icon-colors]
              [style   icon-styles])
    (stop-sign-icon-pict color height style)))

(for/list ([height  icon-heights])
  (for/list ([style   icon-styles])
    (stop-signs-icon-pict height style)))

(for/list ([height  icon-heights])
  (for*/list ([color   icon-colors]
              [style   icon-styles])
    (check-icon-pict color height style)))

(for/list ([height  icon-heights])
  (for*/list ([color   icon-colors]
              [style   icon-styles])
    (hc-append (magnifying-glass-icon-pict color height style)
               (check-icon-pict color height style))))

(for/list ([height  icon-heights])
  (for*/list ([color   icon-colors]
              [style   icon-styles])
    (lt-superimpose (check-icon-pict color height style)
                    (magnifying-glass-icon-pict #f height style))))

(for*/list ([height  icon-heights]
            [style   icon-styles])
  (macro-stepper-icon-pict height style))

(ht-append (go-icon-pict 'red 24 'diffuse)
           (back-icon-pict 'blue 32 'shiny))

(hc-append (go-icon-pict 'red 24 'diffuse)
           (back-icon-pict 'blue 32 'shiny))

(hc-append (back-icon-pict 'blue 32 'shiny)
           (go-icon-pict 'red 24 'diffuse))

(hb-append (go-icon-pict 'red 24 'diffuse)
           (back-icon-pict 'blue 32 'shiny))

(hb-append (back-icon-pict 'blue 32 'shiny)
           (go-icon-pict 'red 24 'diffuse))

(lt-superimpose (back-icon-pict 'blue 32 'shiny)
                (go-icon-pict 'red 24 'diffuse))

(rb-superimpose (check-icon-pict 'green (toolbar-icon-height) 'diffuse)
                (scale (magnifying-glass-icon-pict
                        #f (toolbar-icon-height) (default-icon-style))
                       3/4))

(define not-blurry
  (let* ([1x   (compose icon->pict pict->icon)]
         [2x   (compose 1x 1x)]
         [4x   (compose 2x 2x)]
         [8x   (compose 4x 4x)]
         [16x  (compose 8x 8x)]
         [32x  (compose 16x 16x)]
         [64x  (compose 32x 32x)])
    (64x (magnifying-glass-icon-pict 'green 31.5 'shiny))))
(list not-blurry (pict-width not-blurry) (pict-height not-blurry))
