#lang racket

(require rackunit
         plot/no-gui
         pict
         racket/draw)

(check-true (pict? (plot-pict (function sin -4 4))))
(check-true (pict? (plot3d-pict (contour-intervals3d * -4 4 -4 4))))

(check-true (is-a? (plot-bitmap (function sin -4 4)) bitmap%))
(check-true (is-a? (plot3d-bitmap (contour-intervals3d * -4 4 -4 4)) bitmap%))

(define sin.png (make-temporary-file "plot-bitmap-sin-test~a.png"))
(check-true (void? (plot-file (function sin -4 4) sin.png)))
(check-true (is-a? (read-bitmap sin.png) bitmap%))
(delete-file sin.png)

(define times.png (make-temporary-file "plot-bitmap-times-test~a.png"))
(check-true (void? (plot3d-file (contour-intervals3d * -4 4 -4 4) times.png)))
(check-true (is-a? (read-bitmap times.png) bitmap%))
(delete-file times.png)
