#lang racket

(require icons slideshow/pict)

(define icon-heights '(8 12 16 24 32 48 64))

(define (tape-deck color height style)
  (list
   (rewind-icon color height style)
   (continue-back-icon color height style)
   (step-back-icon color height style)
   (pause-icon color height style)
   (stop-icon color height style)
   (go-icon color height style)
   (step-icon color height style)
   (continue-icon color height style)
   (fast-forward-icon color height style)))

(for*/list ([height  icon-heights]
            [color   icon-colors]
            [style   icon-styles])
  (tape-deck color height style))

(for*/list ([height  icon-heights]
            [style   icon-styles])
  (stop-sign-icon height style))

(for*/list ([height  icon-heights]
            [style   icon-styles])
  (stop-signs-icon height style))

(for/list ([height  icon-heights])
  (for*/list ([color   icon-colors]
              [style   icon-styles])
    (check-icon color height style)))

(for/list ([height  icon-heights])
  (for*/list ([color   icon-colors]
              [style   icon-styles])
    (disk-icon color height style)))

(for/list ([height  icon-heights])
  (for*/list ([color   icon-colors]
              [style   icon-styles])
    (for/list ([make-icon  (list up-arrow-icon down-arrow-icon
                                 left-arrow-icon right-arrow-icon)])
      (make-icon color height style))))

(for/list ([height  icon-heights])
  (for*/list ([color   icon-colors]
              [style   icon-styles])
    (hc-append (magnifying-glass-icon-pict height style)
               (check-icon-pict color height style))))

(for/list ([height  icon-heights])
  (for*/list ([color   icon-colors]
              [style   icon-styles])
    (lt-superimpose (check-icon-pict color height style)
                    (magnifying-glass-icon-pict height style))))

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
                (scale (magnifying-glass-icon-pict (toolbar-icon-height)) 3/4))

(define not-blurry
  (let* ([1x   (compose bitmap pict->bitmap)]
         [2x   (compose 1x 1x)]
         [4x   (compose 2x 2x)]
         [8x   (compose 4x 4x)]
         [16x  (compose 8x 8x)]
         [32x  (compose 16x 16x)]
         [64x  (compose 32x 32x)])
    (64x (magnifying-glass-icon-pict 31.5 'shiny))))
(list not-blurry (pict-width not-blurry) (pict-height not-blurry))

(plt-logo 48 'shiny)
(planet-logo 48 'shiny)

(list (earth-icon 48 'shiny)
      (earth-icon 24 'diffuse)
      (earth-icon 16 'diffuse))

(list (moon-icon 48 'shiny)
      (moon-icon 24 'diffuse)
      (moon-icon 16 'diffuse))

(for/list ([height  icon-heights])
  (for/list ([pose    '("running" "standing")])
    (for*/list ([color   icon-colors]
                [style   icon-styles])
      (load-icon "misc" pose color height style))))
