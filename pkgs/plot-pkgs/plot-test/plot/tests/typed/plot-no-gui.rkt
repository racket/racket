#lang typed/racket

(require typed/rackunit
         plot/typed/no-gui)

(require/typed
 pict
 [#:opaque Pict pict?])

(check-true (pict? (plot-pict (function sin -4 4))))
(check-true (pict? (plot3d-pict (contour-intervals3d * -4 4 -4 4))))

(plot-bitmap (function sin -4 4))
(plot3d-bitmap (contour-intervals3d * -4 4 -4 4))

(define sin.png (make-temporary-file "plot-bitmap-sin-test~a.png"))
(check-true (void? (plot-file (function sin -4 4) sin.png)))
(delete-file sin.png)

(define times.png (make-temporary-file "plot-bitmap-times-test~a.png"))
(check-true (void? (plot3d-file (contour-intervals3d * -4 4 -4 4) times.png)))
(delete-file times.png)
